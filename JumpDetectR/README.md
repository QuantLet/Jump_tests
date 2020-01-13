[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **JumpDetectR** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : JumpDetectR

Published in : 'To be published in METIS'

Description : 'Scalable implementation of Lee / Mykland (2012) and Ait-Sahalia / Jacod (2012) Jump tests for noisy high frequency data'

Keywords : Jumps, jump test, high frequency, time series, Ait-Sahalia, Jacod, Lee, Mykland, stochastic processes, cryptocurrencies, crypto, spectogram

See also : 'Lee, S.S. and Mykland, P.A. (2012) Jumps in Equilibrium Prices and Market Microstructure Noise; Ait-Sahalia, Y. and Jacod, J. (2012) Analyzing the Spectrum of Asset Returns: Jump and Volatility Components in High Frequency Data'

Author : Danial Florian Saef

Submitted : January 12 2020 by Danial Saef
```

### R Code
```r

## install and load packages ##
libraries = c("data.table", "xts", "quantmod", "fasttime")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
if (!("qmao" %in% installed.packages())) {install.packages("qmao", repos="http://R-Forge.R-project.org")} 
library("qmao")
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

## functions ##

make_return_file <- function(DATA, FREQ){
  S <- DATA
  
  #
  start <- paste(as.Date(S[, t][1]), "00:00:00")
  end <- paste(as.Date(S[, t][1]), "23:59:59")
  #
  
  #
  ts_p <- xts(S$p, order.by=S$t, unique = FALSE)
  ts_p <- align.time(ts_p, n=FREQ)
  ts_p <- to.period(ts_p, period = "seconds", k = FREQ)
  
  #
  DT_ts_p <- data.table("index" = seq(from = as.POSIXct(start, tz="UTC"), to = as.POSIXct(end, tz="UTC"), by = paste(FREQ, " sec", sep = "")),
                        "date" = unique(S$date),
                        "id" = unique(S$id),
                        "s" = unique(S$s)
  ) 
  DT_ts_p[, index := as.POSIXct(index, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")]
  DT_ts_p[as.data.table(ts_p), "p" := i.ts_p.Open, on = "index"]
  DT_ts_p[, p := na.locf(p, na.rm = F)]
  DT_ts_p[, p := na.locf(p, na.rm = F, fromLast = T)]
  DT_ts_p[, log_ret := Delt(p, type = "log")]
  DT_ts_p[1, log_ret := 0]
  names(DT_ts_p)[1] <- "t"
  return(DT_ts_p)
}

LM_JumpTest <-function(DATA){
  DT_ts_p <- DATA
  DT_ts_p[, h := hour(t)] # add hour indicator
  DT_ts_p[, count := 1:nrow(DT_ts_p)] # add index
  # DT_split <- split(DT_ts_p, by = "h") # split by hours if jump analysis for subsets
  # names(DT_split) <- unique(DT_ts_p$h)
  
  # P tilde
  P_tilde <- log(DT_ts_p$p)
  
  # acf #
  #acf(DT_ts_p$log_ret)
  bacf <- acf(DT_ts_p$log_ret, plot = FALSE)
  bacfdf <- with(bacf, data.frame(lag, acf))
  
  # CI # https://stackoverflow.com/questions/42753017/adding-confidence-intervals-to-plotted-acf-in-ggplot2
  alpha <- 0.95
  conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(bacf$n.used)
  ##
  lag_outside_conf.lim <- sort(c(which(bacfdf$acf < conf.lims[1]), which(bacfdf$acf > conf.lims[2])))
  
  # Specify k = maximum lag value + 1 
  k <- max(lag_outside_conf.lim) + 1
  
  # Get n
  n <- length(P_tilde)
  
  # Get n-k
  n_diff <- n - k
  
  # Vector with P_tilde_m+k values
  P_tilde_shift <- shift(P_tilde, n = k, type = "lead")
  
  # Calculate q hat
  q_hat <- sqrt(1/(2 * n_diff) * sum((P_tilde[1:n_diff] - P_tilde_shift[1:n_diff])^2)) # *100 for percentage value (but this will cause trouble in further calculations)
  
  # block size #
  C <- NaN
  if (q_hat * 100 > 1) {C <- 1/8; print(paste(DT_ts_p[, "id"][1],DT_ts_p[, "s"][1],DT_ts_p[, "date"][1],"Q_Hat is larger than 1. C defaulted to 1/8. Choose better value.", sep = " - "))}
  if (q_hat * 100 <= 1) C <- 1/8
  if (q_hat * 100 <= (0.9 + 0.8)/2) C <- 1/9
  if (q_hat * 100 <= (0.3 + 0.4)/2) C <- 1/16
  if (q_hat * 100 <= (0.1 + 0.2)/2) C <- 1/18
  if (q_hat * 100 <= (0.05 + 0.07)/2) C <- 1/19
  if (q_hat * 100 < 0.01) print(paste(DT_ts_p[, "id"][1],DT_ts_p[, "s"][1],DT_ts_p[, "date"][1],"Q_Hat is smaller than 0.01. C defaulted to 1/19. Choose better value.", sep = " - "))
  
  if (floor(C*sqrt(n/k)) == 0) {M <- 1
  } else  {
    M <- floor(C*sqrt(n/k))}
  
  
  # if testing different values for C
  C_vector <- c(1/8, 1/9, 1/16, 1/18, 1/19) 
  M_vector <- floor(C_vector*sqrt(n/k))
  M_vector <- unique(M_vector)
  
  # Calculate P_hat_tj (first iteration)
  G_n_k <- seq(from = 0, to = n, by = k) # Grid for first subsampling
  G_n_k[1] <- 1
  P_tilde_t_ik <- P_tilde[G_n_k]
  P_hat_tj <- array(dim = (length(G_n_k) - 1)) # preallocate
  
  # Calculate P_hat_tj (second iteration)
  for (i in G_n_k) P_hat_tj[[i]] <- mean(P_tilde_t_ik[(floor(i/k)):(floor(i/k)+M-1)]) 
  G_n_kM <- seq(from = 0, to = n, by = k*M) # Grid for second subsampling
  G_n_kM[1] <- 1
  P_hat_tj <- P_hat_tj[G_n_kM] # keep only those times t_j where they lie in the grid G_n_kM
  
  # Calculate L_tj
  L_tj <- c(0,diff(P_hat_tj))
  shift(P_hat_tj, 1, type = "lead") - P_hat_tj
  
  # Calculate limit
  V_n <- var(sqrt(M)*L_tj)
  
  ## Calculate sigma_hat ## modulated bipower variation (MBV) & sigma_hat ##
  
  ## Ait-Sahalia sigma_hat ##
  # initialize dX
  dX <- DT_ts_p$log_ret # both X and dX have length n
  x0 <- log(DT_ts_p[, p][1]) # initial value
  
  # specify parameter
  delta <- 1
  T <- 1/365.25 # 1/365.25 = one calendar day, 1/252 = one exchange trading day, 21/252 = one month, 1/4 = one quarter, 1 = one year
  nblagj <- delta
  deltaj <- delta/(6.5*60*60*365.25) # measured in years, this is the value of delta corresponding to that jindex
  
  # initialize X
  X <- x0 + cumsum(dX) # do this instead of X=log(price) to avoid including the large overnight returns
  dXobsj <- X[seq(from = (nblagj+1), to = n, by = nblagj)] - X[seq(from = 1, to = (n-nblagj), by = nblagj)] # length(dXobsj) is equal to nj-1
  
  # calculate sigma_hat (realized bipower variation)
  sigma_hat_as <- sqrt( (1/T) * sum( (abs(dXobsj)^2) * ( abs(dXobsj) <= 3 * 0.30 * deltaj^(1/2) )) )
  sigmahat <- sigma_hat_as
  ## ##
  
  ## Jacod et al. (2010) pre-averaging & truncated realized multipower variation ##
  
  # specify parameters 
  k_jac <- 2 # choose from: 1:3
  p <- 3  # choose from: seq(from = 0, to = 6, by = 0.25)
  gamma <- 2 # choose from: seq(from = 1, to = 3, by = 0.25)
  atrunc <- 20 # choose from: c(2:20, 25, 30, 40, 50, 60, 75, 100, 10^10)
  
  nblagjk <- nblagj * k_jac
  dXobsjk <- X[seq(from = (nblagjk+1), to = n, by = nblagjk)] - X[seq(from = 1, to = (n-nblagjk), by = nblagjk)]
  
  sigma_hat_trunc <- sum( (abs(dXobsjk)^p) * ( abs(dXobsjk) < gamma * atrunc * sigmahat * deltaj^(1/2) ))
  ## ##
  
  sigma_hat <- sigma_hat_as
  
  plim_Vn <- 2/3 * sigma_hat^2 * C^2 * T + 2 * q_hat^2
  
  # Calculte Chi_tj
  Chi_tj <- sqrt(M) / sqrt(plim_Vn) * L_tj
  
  # Define A_n & B_n
  A_n <- sqrt(2 * log(floor(n/k*M))) - (log(pi) + log ( log(floor(n/k*M)) ) / 2 * sqrt(2 * log(floor(n/k*M))))
  B_n <- 1 / (sqrt(2 * log(floor(n/k*M))))
  
  # Define Xi_hat
  Xi_hat <- B_n^-1 * (abs(Chi_tj) - A_n)
  Xi_hat_max <- B_n^-1 * (max(abs(Chi_tj)) - A_n)
  
  # Jump threshold
  significance_level <- 0.01
  beta_star   <-  -log(-log(1-significance_level)) # Jump threshold
  
  J   <-  as.numeric(Xi_hat > beta_star) # Test threshold
  J   <-  J*sign(Chi_tj) # Add direction
  
  # aggregate results in data.table
  res <- data.table("t" = DT_ts_p[,t][G_n_kM],
                    "date" =  DT_ts_p[,date][1],
                    "id" =  DT_ts_p[,id][1],
                    "s" =  DT_ts_p[,s][1],
                    "count" = DT_ts_p[,count][G_n_kM], 
                    "P_hat_tj" = P_hat_tj,
                    "Exp(P_hat_tj)" = exp(P_hat_tj))
  res[, L_t_j := L_tj]
  res[,'Chi_t_j' := sqrt(M) / sqrt(plim_Vn) * L_t_j]
  res[,'Xihat' := B_n^-1 * (abs(Chi_t_j) - A_n)/10]
  res[,'betastar' := -log(-log(1-significance_level))]
  res[,'Jump_indicator' := as.numeric(Xihat > betastar)]
  res[,'Jump_indicator' := Jump_indicator*sign(Chi_t_j)]
  
  return(res)
}

AJ_JumpTest <- function(DATA){
 
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
  
  # read data
  tmp_DT <- DATA
  
  if (nrow(tmp_DT) == 86400){
    subs <- seq(0,nrow(tmp_DT), by = 5)
    subs[1] <- 1
    tmp_DT <- tmp_DT[subs]
  }
  
dX <- tmp_DT[, log_ret]# both X and dX have length n
  x0 <- log(tmp_DT[,p][1]) # initial value
  
  # set T (if cryptocurrency exchange data, set T = 1/365.25 for 1 day, if on normal exchange data set T = 1/252 for one day, T = 1 for one year
  T <- 1/365.25
  n <- length(dX)
  
  # If frequency is less than 5 / 10 seconds, only consider according deltas, if less than 15 seconds -> data is not "high frequency" (although this is an arbitrary threshold)
  if(n < 86400/5) {deltavec <- deltavec[2:length(deltavec)]}
  if(n < 86400/10) {deltavec <- deltavec[3:length(deltavec)]}
  if(n < 86400/15) {return(data.table(date = character(),
                                      id = character(),
                                      s = character(),
                                      p = numeric(),
                                      a = numeric(),    
                                      gamma = numeric(),                                  
                                      delta = numeric(),
                                      k = integer(),
                                      nblag_j = integer(),
                                      delta_j = numeric(),
                                      sigmahat = numeric(),
                                      nblag_jk = numeric(),
                                      thresh = integer(),
                                      B = numeric(),
                                      SJ = numeric()
                                      ))}
  
  # preallocate grid with all possible parameter combinations
  par_grid <- setDT(expand.grid("p" = pvec,
                                "a" = atruncvec,
                                "gamma" = gammavec,
                                "delta" = deltavec,
                                "k" = kvec))
  # compute nblag_j & delta_j
  par_grid[, nblag_j := delta/sort(unique(par_grid$delta)[1])]
  par_grid[, delta_j := delta/(6.5*60*60*365.25)]
  
  # preallocate all possible nblagj to consider on X
  N_nblagj <- sort(unique(par_grid$nblag_j))
  
  # for all nblagj, calculate sigma_hat and store it in par_grid
  for (i in seq_along(N_nblagj)){
    X <- x0 + cumsum(dX) # do this instead of X=log(price) to avoid including the large overnight returns
    nblagj <- unique(par_grid[nblag_j %in% sort(unique(par_grid$nblag_j))[i]]$nblag_j)
    deltaj <-  unique(par_grid[nblag_j %in% sort(unique(par_grid$nblag_j))[i]]$delta_j)
    dXobsj <- sort(abs(X[seq(from = (nblagj+1), to = n, by = nblagj)] - X[seq(from = 1, to = (n-nblagj), by = nblagj)])) # length(dXobsj) is equal to nj-1
    sigma_hat <- sqrt( (1/T) * sum( (abs(dXobsj)^2) * ( abs(dXobsj) <= 3 * 0.30 * deltaj^(1/2) )) )
    par_grid[nblag_j %in% nblagj, sigmahat := sigma_hat]
  }
  
  # calculate nblag_jk and the according threshold for each parameter combination
  par_grid[, nblag_jk := nblag_j*k]
  par_grid[, thresh := gamma * a * sigmahat * sqrt(delta_j)]
  
  # preallocate all possible nblagjk to consider on X
  N_nblagjk <- sort(unique(par_grid$nblag_jk))
  
  # compute dXobsjk for all nblagjk
  list_dXobsjk <- lapply(seq_along(N_nblagjk), function(i){
    nblagjk <- unique(par_grid[nblag_jk %in% sort(unique(par_grid$nblag_jk))[i]]$nblag_jk)
    deltaj <-  unique(par_grid[nblag_jk %in% sort(unique(par_grid$nblag_jk))[i]]$delta_j)
    
    sort(abs(X[seq(from = (nblagjk+1), to = n, by = nblagjk)] - X[seq(from = 1, to = (n-nblagjk), by = nblagjk)]))
  })
  
  # preallocate result list
  result_list <- vector(mode="list", length = length(N_nblagjk))
  
  # calculate B values quickly:  https://stackoverflow.com/questions/59617633/most-efficient-way-to-calculate-function-with-large-number-of-parameter-combinat
  for (i in seq_along(list_dXobsjk)){
    sa_dX_i <- list_dXobsjk[[i]] # temp dXobsjk
    
    # subset par_grid to only include parameter combinations for nblagjk
    par_grid_tmp <- par_grid[nblag_jk %in% N_nblagjk[i]]
    
    test1 <- rep(NA, nrow(par_grid_tmp)) # prellocate result vector
    loc <- findInterval(par_grid_tmp$thresh, sa_dX_i) # find location of last value smaller than threshold 'thresh'
    loc[loc == 0] <- NA  # Handle threshold smaller than everything in dX_i
    
    # calculate B for every unique 'p'
    for (pval in unique(par_grid_tmp$p)) {
      this.p <- par_grid_tmp$p == pval
      cs_dX_i_p <- cumsum(sa_dX_i^pval)
      test1[this.p] <- cs_dX_i_p[loc[this.p]]
    }
    test1[is.na(test1)] <- 0 # Handle threshold smaller than everything in dX_i
    
    par_grid_tmp[,B := test1]
    result_list[[i]] <- par_grid_tmp
  }
  
  # calculate test statistic SJ / other test statistics SFA / SW / QV can be obtained by calculating according B_upper / B_lower values (See AJ_Spectrogram)
  pvec_SJ <- pvec[which( (pvec >= 2.5 & pvec <= 6) )]
  deltavec_SJ <- deltavec[which(deltavec <= 120)]
  kvec_SJ <- kvec[which(kvec >= 2)]
  
  SJ <- rbindlist(result_list)[gamma == gammavec[1] & a == max(par_grid$a) & p %in% pvec_SJ & k %in%  kvec_SJ & delta %in% deltavec_SJ]
  B_lower <- rbindlist(result_list)[gamma == gammavec[1] & a == max(par_grid$a)& p %in% pvec_SJ & k == kvec[1] & delta %in% deltavec_SJ]
  
  for (i in unique(kvec_SJ)){
    SJ[k == i, SJ := B / B_lower$B]
  }
DT_SJ <- data.table("date" =  tmp_DT[,date][1],
                          "id" =  tmp_DT[,id][1],
                          "s" =  tmp_DT[,s][1],
                          SJ)
  return(SJ)
}


##
files <- list.files(path = "./DT_agg_sub", pattern = "*.csv")
##

##
setDTthreads(percent = 75)
DT_agg_sub <- rbindlist(lapply(files, function(x) {
  print(Sys.time())
  print(paste("Reading file ", x))
  cbind(fread(paste("./DT_agg_sub/", x, sep = "")))
}),
idcol = FALSE)
DT_agg_sub[, t := fastPOSIXct(t, tz = "UTC")]
##

##
DT_split <- split(DT_agg_sub, by = c("date", "id", "s"))
DT_split <- DT_split[which(lapply(DT_split,function(x) nrow(x) >= (0.75*86400/300)) == TRUE)] # only keep time series that can be aggregated to at least 5 mins intervals
##

##
progress <- round(quantile(1:length(DT_split), probs = seq(0,1,0.05)))
##


## transform tick data files to regularly spaced return series w.r.t. number of observations##
#
delts <- c(1, 5, 10, 15, 20, 45, 60, 120, 300) # frequencies for testing 
#

DT_split <- lapply(1:length(DT_split), function(x) {
  if (x %in% progress) {print(Sys.time()); print(progress[which(progress == x)])}
  DT_tmp <- DT_split[[x]]
  N <- nrow(DT_tmp)
  DT_ret <- data.table()
  
  for (i in 1:length(delts)){
    d <- delts[i]
        if (N >= 0.75*86400*d) { DT_ret <-	make_return_file(DT_tmp, d)}  # if N is greater or equal to 75% of a c(1, 5, 10, 15, 20, 45, 60, 120, 300) interval convert the file to that frequency
  }
  return(DT_ret)
})

rm(delts)
##


##
DT_split <- DT_split[which(lapply(DT_split,function(x) nrow(x) > 0) == TRUE)] # only keep non-empty list entries (yes, this discards all time series that are not "high frequency")
##

##
progress <- round(quantile(1:length(DT_split), probs = seq(0,1,0.05)))
##

## Lee / Mykland Jump Test ##
LM_result <- rbindlist(lapply(1:length(DT_split), function(x) {
  if (x %in% progress) {print(Sys.time()); print(progress[which(progress == x)])}
  LM_JumpTest(DT_split[[x]])
})
)
## 

## Ait-Sahalia / Jacod Jump Test ##
AJ_result <- rbindlist(lapply(1:length(DT_split), function(x) {
  if (x %in% progress) {print(Sys.time()); print(progress[which(progress == x)])}
  AJ_JumpTest(DT_split[[x]])
})
)
##

```

automatically created on 2020-01-13