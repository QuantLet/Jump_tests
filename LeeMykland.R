# October 03, 2019
# By: Danial Saef
# R Translation of python code as found on
# https://gist.github.com/linuskohl/690da335a34ebf1cfc5ab27973e16ee5

if (!require(data.table)) install.packages("data.table")

movmean <- function(v, kb, kf){
    # Computes the mean with a window of length kb+kf+1 that includes the element 
    # in the current position, kb elements backward, and kf elements forward.
    # Nonexisting elements at the edges get substituted with NaN.
    # 
    # Args:
    #     v (float) <- vector of values.
    #     kb (int) <- Number of elements to include before current position
    #     kf (int) <- Number of elements to include after current position
    #     
    # Returns:
    #     m <- vector of the same size as v containing the mean values
  
  
  m <- rep(NaN, length(v))
  
  for(i in kb:(length(v) - kf)) {
    m[i] <- mean(v[(i-kb):(i+kf+1)])
  }
  
return(m)
}

LeeMykland <- function(S, sampling, significance_level=0.01){
  
    # "Jumps in Equilibrium Prices and Market Microstructure Noise"
    # - by Suzanne S. Lee and Per A. Mykland
    # 
    # "https://galton.uchicago.edu/~mykland/paperlinks/LeeMykland-2535.pdf"
    # 
    # Args:
    #     S <- An array containing prices, where each entry corresponds to the price sampled every "sampling" minutes.
    #     sampling (int) <- Minutes between entries in S
    #     significance_level (float) <- Defaults to 1% (0.001)
    #     
    # Returns:
    #     A data.table containing a row covering the interval 
    #     [t_i, t_i+sampling] containing the following values:
    #     J:   Binary value is jump with direction (sign)
    #     L:   L statistics
    #     T:   Test statistics
    #     sig: Volatility estimate
   
  tm <-  24*60 # Trading minutes
  k   <-  ceiling(sqrt(tm/sampling)) # block size
  
  r <-  c(NaN, diff(log(S)))
  
  bpv <-  abs(r) * abs(c(NaN, r[1:(length(r)-1)]))
  bpv <-  c(NaN, bpv[1:(length(r)-1)]) # Realized bipower variation
  sig <-  sqrt(movmean(bpv, k-3, 0)) # Volatility estimate
  L   <-  r/sig
  n   <-  length(S) # Length of S
  c   <-  (2/pi)**0.5
  Sn  <-  c*(2*log(n))**0.5
  Cn  <-  (2*log(n))**0.5/c - log(pi*log(n))/(2*c*(2*log(n))**0.5)
  beta_star   <-  -log(-log(1-significance_level)) # Jump threshold
  T   <-  (abs(L)-Cn)*Sn
  J   <-  as.numeric(T > beta_star)
  J   <-  J*sign(r) # Add direction
  # First k rows are NaN involved in bipower variation estimation are set to NaN.
  J[0:k] <-  NaN
  # Build and return result dataframe
  return (data.table('L' = L,'sig' = sig, 'T' = T,'J' = J))
  
}

