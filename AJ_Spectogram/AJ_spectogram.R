## install and load packages ##
libraries = c("data.table", "xts", "quantmod",  "ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
if (!("qmao" %in% installed.packages())) {install.packages("qmao", repos="http://R-Forge.R-project.org")} 
library("qmao")
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

##
ts_theme <- theme(panel.border = element_blank(), panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey90"),
                  panel.grid.major = element_line(colour = "grey90"),
                  axis.text = element_text(size = 14, face = "bold"),
                  axis.title = element_text(size = 24, face = "bold"),
                  strip.text = element_text(size = 14, face = "bold"),
                  legend.position="top",
                  legend.box = "horizontal",
                  legend.text = element_text(colour="black", size=18, face="bold"),
                  legend.title = element_text(colour="black", size=14, face="bold"),
                  plot.title = element_text(size = 24, face = "bold", hjust = .5),
                  plot.subtitle = element_text(size = 20, face = "bold", hjust = .5)
)
##

#
##
DT_ts_p <- fread("DT_sample.txt")

### AJ CODE ###
year <- "19"

stocknamevec <- c("btcusd")

qtrvec <- 1

# will loop to compute the values of B(p,u,delta) for all those values below
pvec <- seq(from = 0, to = 6, by = 0.25)

# atrunc is expressed in terms of numbers of stdev of the continuous part;  use 10^10 for no truncation
atruncvec <- c(2:20, 25, 30, 40, 50, 60, 75, 100, 10^10)

# specify possible gammas
gammavec <- seq(from = 1, to = 3, by = 0.25)

# specify possible deltas; for simplicity of coding, make sure those are multiples of timeinterval = 5 in the dataset
deltavec <- c(5, 10, 15, 30, 45, 60, 120, 300, 600, 1800) 

# specify possible ks
kvec <- 1:3

Nstock <- length(stocknamevec)
Nqtr <- length(qtrvec)
Np <- length(pvec)
Natrunc <- length(atruncvec)
Ngamma <- length(gammavec)
Ndelta <- length(deltavec)
Nk <- length(kvec)

B <- array(data = 0, dim = c(Np,Natrunc,Ngamma,Ndelta,Nk)) # list?

# loop on stocks
for (stockindex in 1:Nstock){
  
  stockname <- stocknamevec[stockindex]
  
  for (qtrindex in 1:Nqtr){
    
    qtr <- qtrvec[qtrindex]
    
    # read data
    datafilename <- DT_ts_p
    
    dX <- datafilename[, `LOG-RETURN`]# both X and dX have length n
    x0 <- log(datafilename[, PRICE][1]) # initial value
    
    n <- length(dX) 
    T <- 1/365.25 # length of time in years -- this is fixed, the sample size n is T/delta; 
    # 1/252 for one day, 21/252 for one month, 1/4 for one quarter, 1 for one year
    
    # % loop on values of sampling interval
    for (dindex in 1:Ndelta){ 
      
      print(paste("dindex = ", dindex))
      
      nblagj <- deltavec[dindex]/deltavec[1]
      deltaj <- deltavec[dindex]/(6.5*60*60*365.25) # measured in years, this is the value of delta corresponding to that jindex
      
      # nj <- n / nblagj # this is the sample size of the vector Xobsj
      
      # MATLAB: To generate a series of numbers from 10 to 50, incrementing by 5, use A = 10:5:50;
      # A <- seq(from = 10, to = 50, by = 5)
      X <- x0 + cumsum(dX) # do this instead of X=log(price) to avoid including the large overnight returns
      dXobsj <- X[seq(from = (nblagj+1), to = n, by = nblagj)] - X[seq(from = 1, to = (n-nblagj), by = nblagj)] # length(dXobsj) is equal to nj-1
      print(paste('dindex = ', dindex,'; nb obs = ', (length(dXobsj))))
      
      # adapt==1
      # use the small increments in dXobsj to estimate sigma (= volatility of the continuous part of the semimartingale)
      sigmahat <- sqrt( (1/T) * sum( (abs(dXobsj)^2) * ( abs(dXobsj) <= 3 * 0.30 * deltaj^(1/2) )) )
      # use the increments that are smaller than the finite cutoff 3 * 0.30
      # the factor (1/T) is there in the variance to annualize the estimator sigmahat
      print(paste('sigmahat = ', sigmahat))
      
      # loop on values of k
      for (kindex in 1:Nk){
        
        # for all values of kindex, do the truncation based on the value deltaj
        # so that we are using the same cutoffs u_n for delta and 2*delta
        k <- kvec[kindex]
        nblagjk <- nblagj * k
        dXobsjk <- X[seq(from = (nblagjk+1), to = n, by = nblagjk)] - X[seq(from = 1, to = (n-nblagjk), by = nblagjk)]
        
        #  loop on values of atrunc set as number of standard deviations
        for (aindex in 1:Natrunc){
          
          # the cutoff will be  u_n = atrunc * sigmahat * deltaj^(1/2)
          # yes, it's based on deltaj, not deltajk
          atrunc <- atruncvec[aindex]
          
          # loop on values of gamma set as multiple of atrunc
          for (gindex in 1:Ngamma){
           
            g <- gammavec[gindex]
            
            for (pindex in 1:Np){
             
              p <- pvec[pindex]
              B[[pindex,aindex,gindex,dindex,kindex]]  <- sum( (abs(dXobsjk)^p) * ( abs(dXobsjk) < g * atrunc * sigmahat * deltaj^(1/2) ))
            }
          }
        }
      }
    }
    
    # loopstotal <- Nstock*Nqtr 
    # loopsdone <- qtrindex + (stockindex - 1)*Nqtr
    # print(paste("data file #", loopsdone, " out of ", loopstotal))
    
  }
}

#

year <- '19'

stocknamevec <- c("btcusd")
qtrvec <- 1
Nstock <- length(stocknamevec)
Nqtr <- length(qtrvec);

# Initialize vectors of results
SJ <- 0
SFA <- 0
SW <- 0 
QVSplit <- 0
Beta <- 0

# loop on stocks
for (stockindex in 1:Nstock){
  
  stockname <- stocknamevec[stockindex]
  
  for (qtrindex in 1:Nqtr){
    
    qtr <- qtrvec[qtrindex]
    
    # from here the values from the sourced file with the variables B(pindex,aindex,gindex,dindex,kindex)
    # as well as pvec,atruncvec,gammavec,deltavec,kvec are needed
    # Nstock <- length(stocknamevec)
    # Nqtr <- length(qtrvec)
    # Np <- length(pvec)
    # Natrunc <- length(atruncvec)
    # Ngamma <- length(gammavec)
    # Ndelta <- length(deltavec)
    # Nk <- length(kvec)

    # initialize arrays based on length of pvec, etc., read in the first file
    if (stockindex==1 && qtrindex==1){
      U <- array(data = 0, dim = c(Nstock,Nqtr,Natrunc,Ngamma,Ndelta), dimnames = NULL)
    }
    
    # assign in array the U's for use in Beta at p=0 only
    # recall that U = B(without truncation) - B(with truncation)
    
    for (aindex in 1:Natrunc){
      for (gindex in 1:Ngamma){
        for(dindex in 1:Ndelta){
          U[[stockindex,qtrindex,aindex,gindex,dindex]] = B[[1,Natrunc,gindex,dindex,1]] - B[[1,aindex,gindex,dindex,1]]
        }
      }
    }
    
    # compute SJ
    pvec_SJ <- which( (pvec >= 2.5 & pvec <= 6) )
    deltavec_SJ <- which(deltavec <= 120)
    kvec_SJ <- which(kvec >= 2)
    SJtemp <- array(data = 0, dim = c(length(pvec_SJ),length(deltavec_SJ),length(kvec_SJ)))
    DT_SJtemp <- data.table()
    
    for (pindex in 1:length(pvec_SJ)){
      for (dindex in 1:length(deltavec_SJ)){
        for (kindex in 1:length(kvec_SJ)){
          SJtemp[[pindex,dindex,kindex]] <- B[[pvec_SJ[pindex],Natrunc,1,deltavec_SJ[dindex],kvec_SJ[kindex]]] / B[[pvec_SJ[pindex],Natrunc,1,deltavec_SJ[dindex],1]]
          x <- data.table("test" = "SJ",
                      "test_value" =  B[[pvec_SJ[pindex],Natrunc,1,deltavec_SJ[dindex],kvec_SJ[kindex]]] / B[[pvec_SJ[pindex],Natrunc,1,deltavec_SJ[dindex],1]],
                      "p" = pvec[pvec_SJ[[pindex]]],
                      "d" = deltavec[deltavec_SJ[[dindex]]],
                      "k" = kvec[kvec_SJ[[kindex]]])

          DT_SJtemp <- rbind(x, DT_SJtemp)
        }
      }
     }
    
    SJtemp <- matrix(data = SJtemp, nrow = prod(dim(SJtemp)), ncol = 1, byrow=T) # make the array into a column vector
    SJ <- rbind(SJ,SJtemp) # append the new results to those for the other stocks/qtr
    
    # compute SFA
    pvec_SFA <- which( (pvec >= 2.5 & pvec <= 6) )
    atruncvec_SFA <- which( (atruncvec > 5 & atruncvec < 10) )
    deltavec_SFA <- which(deltavec <= 120)
    kvec_SFA <- which(kvec >= 2)
    SFAtemp <- array(data = 0, dim = c(length(pvec_SFA),length(atruncvec_SFA),length(deltavec_SFA),length(kvec_SFA)))
    DT_SFAtemp <- data.table()
    
    for (pindex in 1:length(pvec_SFA)){
      for (aindex in 1:length(atruncvec_SFA)){
        for (dindex in 1:length(deltavec_SFA)){
          for (kindex in 1:length(kvec_SFA)){
            SFAtemp[[pindex,aindex,dindex,kindex]] <- B[[pvec_SFA[pindex],atruncvec_SFA[aindex],1,deltavec_SFA[dindex],kvec_SFA[kindex]]] / B[[pvec_SFA[pindex],atruncvec_SFA[aindex],1,deltavec_SFA[dindex],1]]
          
            x <- data.table("test" = "SFA",
                            "test_value" =  B[[pvec_SFA[pindex],atruncvec_SFA[aindex],1,deltavec_SFA[dindex],kvec_SFA[kindex]]] / B[[pvec_SFA[pindex],atruncvec_SFA[aindex],1,deltavec_SFA[dindex],1]],
                            "p" = pvec[pvec_SFA[[pindex]]],
                            "a" = atruncvec[atruncvec_SFA[[aindex]]],
                            "d" = deltavec[deltavec_SFA[[dindex]]],
                            "k" = kvec[kvec_SFA[[kindex]]])
            
            DT_SFAtemp <- rbind(x, DT_SFAtemp)  
          }
        }
      }
    }
    SFAtemp <- matrix(data = SFAtemp, nrow = prod(dim(SFAtemp)), ncol = 1, byrow=T) # make the array into a column vector
    SFA <- rbind(SFA,SFAtemp) # append the new results to those for the other stocks/qtr
    
    # compute SW
    pvec_SW <- which( (pvec < 2.5 & pvec > 1) )
    atruncvec_SW <- which( (atruncvec > 5 & atruncvec < 10) )
    deltavec_SW <- which(deltavec <= 120)
    kvec_SW <- which(kvec >= 2)
    SWtemp <- array(data = 0, dim = c(length(pvec_SW),length(atruncvec_SW),length(deltavec_SW),length(kvec_SW)))
    DT_SWtemp <- data.table()
    
    for (pindex in 1:length(pvec_SW)){
      for (aindex in 1:length(atruncvec_SW)){
        for (dindex in 1:length(deltavec_SW)){
          for (kindex in 1:length(kvec_SW)){
            SWtemp[[pindex,aindex,dindex,kindex]] <- B[[pvec_SW[pindex],atruncvec_SW[aindex],1,deltavec_SW[dindex],1]] / B[[pvec_SW[pindex],atruncvec_SW[aindex],1,deltavec_SW[dindex],kvec_SW[kindex]]]
            
            x <- data.table("test" = "SW",
                            "test_value" =  B[[pvec_SW[pindex],atruncvec_SW[aindex],1,deltavec_SW[dindex],1]] / B[[pvec_SW[pindex],atruncvec_SW[aindex],1,deltavec_SW[dindex],kvec_SW[kindex]]],
                            "p" = pvec[pvec_SW[[pindex]]],
                            "a" = atruncvec[atruncvec_SW[[aindex]]],
                            "d" = deltavec[deltavec_SW[[dindex]]],
                            "k" = kvec[kvec_SW[[kindex]]])
            
            DT_SWtemp <- rbind(x, DT_SWtemp)  
            
          }
        }
      }
    }
    SWtemp <- matrix(data = SWtemp, nrow = prod(dim(SWtemp)), ncol = 1, byrow=T) # make the array into a column vector
    SW <- rbind(SW,SWtemp) # append the new results to those for the other stocks/qtr
    
    
    # compute QVSplit
    pvec_QVSplit <- which(pvec == 2)
    atruncvec_QVSplit <- which( (atruncvec > 2 & atruncvec < 5) )
    deltavec_QVSplit <- which(deltavec <= 1800)
    QVSplittemp <- array(data = 0, dim = c(length(atruncvec_QVSplit),length(deltavec_QVSplit)))
    DT_QVSplittemp <- data.table()
    
    for (aindex in 1:length(atruncvec_QVSplit)){
      for (dindex in 1:length(deltavec_QVSplit)){
        QVSplittemp[[aindex,dindex]] <- B[[pvec_QVSplit[1],atruncvec_QVSplit[aindex],1,deltavec_QVSplit[dindex],1]] / B[[pvec_QVSplit[1],Natrunc,1,deltavec_QVSplit[dindex],1]]
        
        x <- data.table("test" = "QVSplit",
                        "test_value" =   B[[pvec_QVSplit[1],atruncvec_QVSplit[aindex],1,deltavec_QVSplit[dindex],1]] / B[[pvec_QVSplit[1],Natrunc,1,deltavec_QVSplit[dindex],1]],
                        "a" = atruncvec[atruncvec_QVSplit[[aindex]]],
                        "d" = deltavec[deltavec_QVSplit[[dindex]]])

        DT_QVSplittemp <- rbind(x, DT_QVSplittemp) 
      }
    }
    
    QVSplittemp <- matrix(data = QVSplittemp, nrow = prod(dim(QVSplittemp)), ncol = 1, byrow=T) # make the array into a column vector
    QVSplit <- rbind(QVSplit,QVSplittemp) # append the new results to those for the other stocks/qtr
    
    
  }
  
  # compute Beta
  # for Beta, aggregate all the year's data, otherwise not enough in one quarter
  
  # initialize Usum the first time around only
  if (stockindex == 1){
    Usum <- array(data = 0, dim = c(Nstock,length(atruncvec),length(gammavec),length(deltavec)), dimnames = NULL)
  }
  
  #  sum the number of large increments over the 4 quarters
  for (aindex in 1:length(atruncvec)){
    for (gindex in 1:length(gammavec)){
      for (dindex in 1:length(deltavec)){
        Usum[[stockindex,aindex,gindex,dindex]] <- sum(U[stockindex,,aindex,gindex,dindex])
      }
    }
  }
  
  # calculate Beta
  pvec_Beta <- which(pvec == 0)
  atruncvec_Beta <- which( (atruncvec > 5 & atruncvec < 8) )
  gammavec_Beta <- which( (gammavec >= 1.5 & gammavec < 2.5) )
  deltavec_Beta <- which(deltavec <= 10)
  Betatemp <- array(data = 0, dim = c(length(atruncvec_Beta),length(gammavec_Beta),length(deltavec_Beta)))
  DT_Betatemp <- data.table()
  
  print(length(atruncvec_Beta))
  print(length(gammavec_Beta))
  print(length(deltavec_Beta))
  
  for (aindex in 1:length(atruncvec_Beta)){
    for (gindex in 1:length(gammavec_Beta)){
      for (dindex in 1:length(deltavec_Beta)){
        Betatemp[[aindex,gindex,dindex]] <- (1/log(gammavec[[gammavec_Beta[gindex]]])) * log(Usum[[stockindex,atruncvec_Beta[[aindex]],1,deltavec_Beta[[dindex]]]]/Usum[[stockindex,atruncvec_Beta[[aindex]],gammavec_Beta[[gindex]],deltavec_Beta[[dindex]]]])
        
        x <- data.table("test" = "Beta",
                        "test_value" =   (1/log(gammavec[[gammavec_Beta[gindex]]])) * log(Usum[[stockindex,atruncvec_Beta[[aindex]],1,deltavec_Beta[[dindex]]]]/Usum[[stockindex,atruncvec_Beta[[aindex]],gammavec_Beta[[gindex]],deltavec_Beta[[dindex]]]]),
                        "a" = atruncvec[atruncvec_Beta[[aindex]]],
                        "d" = deltavec[deltavec_Beta[[dindex]]])
        
        DT_Betatemp <- rbind(x, DT_Betatemp) 
      }
    }
  }
  Betatemp <- matrix(data = Betatemp, nrow = prod(dim(Betatemp)), ncol = 1, byrow=T) # make the array into a column vector
  Beta <- rbind(Beta,Betatemp) # append the new results to those for the other stocks/qtr
  
} 

## Aggregate test values ##

DT_test_values <- rbindlist(list(DT_SJtemp, DT_SFAtemp, DT_SWtemp, DT_QVSplittemp, DT_Betatemp), idcol = F, fill = TRUE)
DT_test_values[test %in% "SJ", limit_jumps := 1]
DT_test_values[test %in% "SFA", limit_finite := k^(p/2-1)]
DT_test_values[test %in% "SW", limit := k^(1-p/2)]
DT_test_values <- DT_test_values[!test_value == Inf]

#
test_names <- unique(DT_test_values$test)

## Limits for null hypotheses ##
test_description <- array(data = NA, dim = c(2,1,5), dimnames = NULL)
test_description[[1,1,1]] <- 'SJ: test for jumps'
test_description[[2,1,1]] <- 'limit=1 if jumps, k^{p/2-1} if no jumps'
test_description[[1,1,2]] <- 'SFA: test for finite jump activity'
test_description[[2,1,2]] <- 'limit=1 if infinite activity, k^{p/2-1} if finite'
test_description[[1,1,3]] <- 'SW: test for presence of Brownian motion'
test_description[[2,1,3]] <- 'limit=1 if no Brownian, k^{1-p/2} if Brownian'
test_description[[1,1,4]] <- 'QVSplit: % of QV due to Brownian'
test_description[[2,1,4]] <- " "
test_description[[1,1,5]] <- 'Beta: Estimate of degree of jump activity'
test_description[[2,1,5]] <- " "
## ##

### Plots ###
lapply(1:length(test_names), function(x){
  
  dataset <- DT_test_values[test %in% test_names[[x]]]
  
  bins <- min(500, grDevices::nclass.FD(na.exclude(dataset$test_value)))
  binwidth <- (max(dataset$test_value, na.rm = TRUE) - min(dataset$test_value, na.rm = TRUE))/bins
  
  ggplot(dataset, aes(x=test_value)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=binwidth,
                   colour="skyblue2", fill="skyblue2") +
    ts_theme +
    theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent")) + # get rid of legend panel bg
    labs(x = "Test values", y = "Density", title = paste(test_description[[1,1,x]]), subtitle = paste(test_description[[2,1,x]]))
})




