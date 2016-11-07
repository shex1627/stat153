##########################################################
####Q1
ar1_sim <- function(phi, n){
  #takes in @phi and sample size @n, returns a vector with n simulated ar1 model
  #with parameter @phi
  return(arima.sim(model=list(ar=phi), n = n))
}

estimate_phi <- function(data) {
  #take in a vector @data and returns the yule-walker estimation of the data's ar1 model
  estimated_acf <- acf(data, type = "covariance", plot = FALSE)[[1]] #indexing 1 to get a vector
  return(estimated_acf[2]/estimated_acf[1]) #based on the formula
}

sim_phi <- function(phi, n){
  #create 10000 estimated phi based on simulated ar1 sample of parameter @phi, size @n
  allphis <- replicate(10000, estimate_phi(ar1_sim(phi, n)))
  se_estimated <- round(var(allphis), 4)
  se_theoretical <- (1-phi^2)/n
  cat("Theoretical Sample Distribution\n")
  cat("size:", n, "phi:", phi, "se:", se_theoretical, "\n")
  cat("Estimated Sample Distribution:\n")
  cat("phi:", round(mean(allphis), 3), "se:", se_estimated, "\n\n")
}
##########################################################
####Q2
ma2_sim <- function(phi1, phi2, n) {
  # simulate a 0-mean MA(2) process with paramters @phi1, @phi2, and size @n
  # returns the cofficients as a 2-tuple 
  sim_data <- arima.sim(model=list(ma = c(phi1, phi2)), n = n)
  ma2mod <- arima(sim_data, order = c(0,0,2), method = "ML", include.mean = FALSE, optim.control = list(maxit = 1000))
  return(ma2mod$coef)
}

sim_theta <- function(ta1, ta2, n) {
  # transpose the matrix so c1 is all estimated theta1, c2 is all estimated theta2
  theta_pairs <- t(replicate(1000, ma2_sim(ta1, ta2, n))) 
  cov_matrix <- cov(theta_pairs)
  theta_estimated <- round(apply(theta_pairs, 2, mean), 3)
  cov_estimated <- round(apply(theta_pairs, 2, var), 4)
  cov_theoretical <- 1/n *c(1 - ta2^2, ta1 * (1 + ta2))
  cat("sample size:", n, "theta1,theta2:", c(ta1, ta2), "\n")
  cat("The estimated means of theta1 and theta2 are", theta_estimated, "\n")
  cat("The theoretical covariance between theta1 and theta2 are", cov_theoretical[2], "\n")
  cat("The estimated covariance between theta1 and theta2 are", cov_estimated[2], "\n\n")
}

