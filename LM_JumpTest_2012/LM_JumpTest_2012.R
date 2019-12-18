## install and load packages ##
libraries = c("data.table", "xts", "quantmod", "fitdistrplus", "ggplot2", "scales")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
if (!("qmao" %in% installed.packages())) {install.packages("qmao", repos="http://R-Forge.R-project.org")} 
library("qmao")
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

#
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
DT_ts_p <- fread("DT_sample.csv")
DT_ts_p[, h := hour(index)] # add hour indicator
DT_ts_p[, count := 1:n] # add index
DT_split <- split(DT_ts_p, by = "h") # split by hours if jump analysis for subsets
names(DT_split) <- unique(DT_ts_p$h)

# P tilde
P_tilde <- log(DT_ts_p$p)

# acf
acf(DT_ts_p$log_ret)

# Get n
n <- length(P_tilde)

# Specify k
k <- 50

# Get n-k
n_diff <- n - k

# Vector with P_tilde_m+k values
P_tilde_shift <- shift(P_tilde, n = k, type = "lead")

# Calculate q hat
q_hat <- sqrt(1/(2 * n_diff) * sum((P_tilde[1:n_diff] - P_tilde_shift[1:n_diff])^2)) # *100 for percentage value (but this will cause trouble in further calculations)

# block size #
C <- NaN
if (q_hat * 100 > 1) C <- 1/8; print("Q_Hat is larger than 1. C defaulted to 1/8. Choose better value.")
if (q_hat * 100 <= 1) C <- 1/8
if (q_hat * 100 <= (0.9 + 0.8)/2) C <- 1/9
if (q_hat * 100 <= (0.3 + 0.4)/2) C <- 1/16
if (q_hat * 100 <= (0.1 + 0.2)/2) C <- 1/18
if (q_hat * 100 <= (0.05 + 0.07)/2) C <- 1/19
if (q_hat * 100 < 0.01) print("Q_Hat is smaller than 0.01. C defaulted to 1/19. Choose better value.")

M <- floor(C*sqrt(n/k))

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

## ##

## Jacod et al. (2010) pre-averaging & truncated realized multipower variation ##

# specify parameters 
k_jac <- 2 # choose from: 1:3
p <- 3  # choose from: seq(from = 0, to = 6, by = 0.25)
gamma <- 2 # choose from: seq(from = 1, to = 3, by = 0.25)
atrunc <- 20 # choose from: c(2:20, 25, 30, 40, 50, 60, 75, 100, 10^10)

nblagjk <- nblagj * k_jac
dXobsjk <- X[seq(from = (nblagjk+1), to = n, by = nblagjk)] - X[seq(from = 1, to = (n-nblagjk), by = nblagjk)]

sigma_hat_trunc <- sum( (abs(dXobsjk)^p) * ( abs(dXobsjk) < g * atrunc * sigmahat * deltaj^(1/2) ))
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
res <- data.table("t" = DT_ts_p[,index][G_n_kM],
                  "count" = DT_ts_p[,count][G_n_kM], 
                  "P_hat_tj" = P_hat_tj,
                  "Exp(P_hat_tj)" = exp(P_hat_tj))
res[, L_t_j := L_tj]
res[,'Chi_t_j' := sqrt(M) / sqrt(plim_Vn) * L_t_j]
res[,'Xihat' := B_n^-1 * (abs(Chi_t_j) - A_n)/10]
res[,'betastar' := -log(-log(1-significance_level))]
res[,'Jump_indicator' := as.numeric(Xihat > betastar)]
res[,'Jump_indicator' := Jump_indicator*sign(Chi_t_j)]

## plot ##
# ggplot(data = res[count >= 12000 & count <= 45000], aes(x = t, y = `Exp(P_hat_tj)`)) +
ggplot(data = res, aes(x = t, y = `Exp(P_hat_tj)`)) +
  geom_point(col = "#00BFC4") + 
  geom_vline(xintercept = res[Jump_indicator<0]$t, col = "red") +
  geom_vline(xintercept = res[Jump_indicator>0]$t, col = "green") +
  ts_theme +
  theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"))+
   theme(legend.position  = "none") +
  labs(x = "Timestamp", y = "BTC/USD", title = "BTC / USD on May 17, 2019")

## ##


# Plot fit of observed distribution vs sample #
dgumbel <- function(x,mu,s){ # PDF
  exp((mu - x)/s - exp((mu - x)/s))/s
}

pgumbel <- function(q,mu,s){ # CDF
  exp(-exp(-((q - mu)/s)))
}

qgumbel <- function(p, mu, s){ # quantile function
  mu-s*log(-log(p))
} 

gumbel.fit <- fitdist(L_tj, "gumbel", start=list(mu=0.577, s=pi/sqrt(6)), method="mle", lower = c(0,0))

summary(gumbel.fit)

par(cex=1.2, bg="white")
plot(gumbel.fit, lwd=2, col="steelblue") 

