# Dependencies ------------------------------------------------------------
# For sampling from Multivariate Normal distribution
require(mvtnorm)
# For sampling from Truncated Normal distribution
require(truncnorm)
# To calculate ESS
require(coda)
# For hyper parameter tuning
require(RPEnsemble)
# Polya-gamma data augmentation for Logistic regression
require(BayesLogit)

# helper functions written in rcpp
source("Probit_HH_cpp.R")

# Function: Data generation (n<p) : Independent, AR1 design -------------------------------------------------------
DataGeneration <- function(n, p, s0, design = "Independent", rho, seed){
set.seed(seed)
if(design == "Independent"){
  # Design matrix
  x = matrix(rnorm(n*p), nrow = n, ncol = p); 
  X_true <- as.matrix(cbind(rep(1, n), x))
  # True values of regression coeffiecients theta
  true_theta <- c(1, rep(1, s0), rep(0, p - s0))
  
  # Obtain the vector with probabilities of success p using the probit link
  prob <- pnorm(X_true %*% true_theta)
  
  # Generate binary observation data y
  y <- rbinom(n, 1, prob)
  
  # Variables that we will need later
  N1  <- sum(y)  # Number of successes
  N0  <- n - N1  # Number of failures
}
if(design == "AR1"){
  x = matrix(NA, nrow = n, ncol = p);
  for(i in 1:n){
    x[i, ] = as.vector(arima.sim(model = list(ar = rho), p))
  }
  X_true <- as.matrix(cbind(rep(1, n), x))
  # True values of regression coeffiecients theta
  true_theta <- c(1, rep(1, s0), rep(0, p - s0))
  
  # Obtain the vector with probabilities of success p using the probit link
  prob <- pnorm(X_true %*% true_theta)
  
  # Generate binary observation data y
  y <- rbinom(n, 1, prob)
  
  # Variables that we will need later
  N1  <- sum(y)  # Number of successes
  N0  <- n - N1  # Number of failures
}
  return(list("X" = X_true, "y" = y))
}  

# Function: Random Compression ------------------------------------------------------

phi_matrix <- function(m, p, sparsity){
phi_matrix = matrix(0, m, p)
for(i in 1:m){
  for(j in 1:p){
    a = runif(1)
    if(a <1/(2*sparsity)){phi_matrix[i, j] = -sqrt(sparsity)}
    if(a >(1 - 1/(2*sparsity))){phi_matrix[i, j] = sqrt(sparsity)}
  }
}
normalizer = rowSums(phi_matrix^2)
for(i in 1:m){
  phi_matrix[i, ] = phi_matrix[i, ]/normalizer[i]
}
return(phi_matrix)
}

# Function: Probit Regression (Albert & Chib)--------------------------------------------------------------------
probitBasic <- function(y, X, 
                        prior_variance, 
                        N_sim = 10000, burn_in_prop = 0.5)
{
D = ncol(X); N1 = sum(y); N0 = length(y) - N1
# Conjugate prior on the coefficients \theta ~ N(theta_0, Q_0)
theta_0 <- rep(0, D)
Q_0 <- diag(prior_variance, D)

# Initialize parameters
theta <- rep(0, D)
z <- rep(0, nrow(X))

# Number of simulations for Gibbs sampler
#N_sim <- 10000 
# Burn in period
burn_in <- floor(N_sim*burn_in_prop)
# Matrix storing samples of the \theta parameter
theta_chain <- matrix(0, nrow = N_sim, ncol = D)


# ---------------------------------
# Gibbs sampling algorithm
# ---------------------------------

# Compute posterior variance of theta
prec_0 <- solve(Q_0)
V <- solve(prec_0 + crossprod(X, X))

for (t in 2:N_sim) {
  # Update Mean of z
  mu_z <- X %*% theta
  # Draw latent variable z from its full conditional: z | \theta, y, X
  z[y == 0] <- rtruncnorm(N0, mean = mu_z[y == 0], sd = 1, a = -Inf, b = 0)
  z[y == 1] <- rtruncnorm(N1, mean = mu_z[y == 1], sd = 1, a = 0, b = Inf)
  
  # Compute posterior mean of theta
  M <- V %*% (prec_0 %*% theta_0 + crossprod(X, z))
  # Draw variable \theta from its full conditional: \theta | z, X
  theta <- c(rmvnorm(1, M, V))
  
  # Store the \theta draws
  theta_chain[t, ] <- theta
}
post_mean = colMeans(theta_chain[((burn_in+1):N_sim), ])
return(list("theta_chain" = theta_chain, "post_mean" = post_mean))
}


# Function: Probit Regression (Homes and Held) --------------------------------------------------------------------
probitHH <- function(y, X, 
                     prior_variance, 
                     N_sim = 10000, burn_in_prop = 0.5)
{
  D = ncol(X);
  n = length(y); N1 = sum(y); N0 = n - N1
  # Conjugate prior on the coefficients \theta ~ N(theta_0, Q_0)
  theta_0 <- rep(0, D)
  Q_0 <- diag(prior_variance, D)
  
  # Initialize parameters
  theta <- rep(0, D)
  z <- rep(0, nrow(X))
  
  # Number of simulations for Gibbs sampler
  #N_sim <- 10000 
  # Burn in period
  burn_in <- floor(N_sim*burn_in_prop)
  # Matrix storing samples of the \theta parameter
  theta_chain <- matrix(0, nrow = N_sim, ncol = D)
  
  
  # ---------------------------------
  # Gibbs sampling algorithm
  # ---------------------------------
  
  # Compute posterior variance of theta
  prec_0 <- solve(Q_0)
  V <- solve(prec_0 + crossprod(X, X))
  V <- 0.5*(V + t(V)) # guard against numerical issue
  
  # Compute the martix S = VX'
  S <- tcrossprod(V, X)
  
  h <- vector(mode = "numeric", length = n)
  w <- vector(mode = "numeric", length = n)
  u <- vector(mode = "numeric", length = n)
  for (j in 1:n){
    # h stores the diagonal elements of the hat matrix (XS = XVX')
    h[j] <- X[j, ] %*% S[, j]
    w[j] <- h[j] / (1 - h[j])
    u[j] <- w[j] + 1
  }
  
  # Initialize latent variable Z, from truncated normal
  z[y == 0] <- rtruncnorm(N0, mean = 0, sd = 1, a = -Inf, b = 0)
  z[y == 1] <- rtruncnorm(N1, mean = 0, sd = 1, a = 0, b = Inf)
  
  # Matrix storing samples of the \theta parameter
  theta_chain_holmes <- matrix(0, nrow = N_sim, ncol = D)
  
  # ---------------------------------
  # Gibbs sampling algorithm
  # ---------------------------------
  
  # Compute the conditional mean of \theta
  M <- as.vector(S %*% z)
  
  for (t in 2:N_sim) {
    for (j in 1:n){
      # Store the old value of z
      z_old <- z[j]
      
      # Update mean of latent variable z_i
      m <- X[j, ] %*% M
      m <- m - w[j] * (z[j] - m)
      
      # Draw latent variable z from full conditional: z_j | z_-j, y, X
      if (y[j] == 0)
        z[j] <- rtruncnorm(1, mean = m, sd = u[j], a = -Inf, b = 0)
      else
        z[j] <- rtruncnorm(1, mean = m, sd = u[j], a = 0, b = Inf)
      
      # Update the posterior mean M
      M <- as.vector(M + (z[j] - z_old) %*% S[ ,j])
    }
    
    # Posterior of M | Z
    theta_chain_holmes[t, ] <- c(rmvnorm(1, M, V))
  }
  
  post_mean = colMeans(theta_chain_holmes[((burn_in+1):N_sim), ])
  return(list("theta_chain" = theta_chain_holmes, "post_mean" = post_mean))
}

# Function: Logit Regression (Polya & Gamma)--------------------------------------------------------------------

logit.R <- function(y, X, n=rep(1, length(y)),
                    m0=rep(0, ncol(X)), P0=matrix(0, nrow=ncol(X), ncol=ncol(X)),
                    samp=1000, burn=500, verbose=500)
{
  ## X: n by p matrix
  ## y: n by 1 vector, avg response
  ## n: n by 1 vector, # of obs at distinct x
  
  ## Combine data.
  ## new.data = logit.combine(y, X, n);
  ## y = new.data$y;
  ## X = new.data$X;
  ## n = new.data$n;
  ## n.prior = 0.0;
  
  X = as.matrix(X);
  y = as.numeric(y)
  
  p = ncol(X)
  N = nrow(X)
  
  alpha = (y-1/2)*n
  
  Z = colSums(X*alpha) + P0 %*% m0;
  ## PsiToBeta = solve(t(X) %*% X) %*% t(X);
  
  w = rep(0,N)
  ## w = w.known;
  beta = rep(0.0, p)
  
  output <- list(w = matrix(nrow=samp, ncol=N),
                 beta = matrix(nrow=samp, ncol=p)
  )
  
  ## c_k = (1:200-1/2)^2 * pi^2 * 4;
  
  ## Timing
  start.time = proc.time()
  
  ## Sample
  for ( j in 1:(samp+burn) )
  {
    if (j==burn+1) start.ess = proc.time();
    
    ## draw w
    psi = drop(X%*%beta)
    ## Sum of gamma: poor approximation when psi is large!  Causes crash.
    ## w = rpg.gamma(N, n, psi)
    ## Devroye is faster anyway.
    w = rpg.devroye(N, n, psi);
    
    ## draw beta - Joint Sample.
    PP = t(X) %*% (X * w) + P0;
    ## U = chol(PP);
    ## m = backsolve(U, Z, transpose=TRUE);
    ## m = backsolve(U, m);
    ## beta = m + backsolve(U, rnorm(p))
    S = chol2inv(chol(PP));
    m = S %*% as.vector(Z);
    beta = m + t(chol(S)) %*% rnorm(p);
    
    # Record if we are past burn-in.
    if (j>burn) {
      output$w[j-burn,] <- w
      output$beta[j-burn,] <- beta
    }
    
    if (j %% verbose == 0) { print(paste("LogitPG: Iteration", j)); }
  }
  
  end.time = proc.time()
  output$total.time = end.time - start.time
  output$ess.time   = end.time - start.ess
  
  ## Add new data to output.
  output$"y" = y;
  output$"X" = X;
  output$"n" = n;
  
  output
} ## logit.gibbs.R

# logit.R(y, X, n=rep(1, length(y)),
#         m0=rep(0, ncol(X)), P0=matrix(0, nrow=ncol(X), ncol=ncol(X)),
#         samp=1000, burn=500, verbose=500)




# Function: Repeated Compressed Probit Regression (Albert & Chib) -------------------------------------------------------
Repeated_Probit<- function(n = 100, p = 1000, s0 = 5, design = "Independent", rho = 0.9, seed = 1, # Data generation
                           prior_variance = 1, # Prior hyper-parameter specification,
                           s = 100 , m = 40, sparsity = 10, # Projection
                           N_sim = 10000, burn_in_prop = 0.5, 
                           Sampler = "Albert_Chib", # Sampler specification
                           alpha = 0.5){  # Cut-off
start_time <- Sys.time()
# Data generation
Data = DataGeneration(n, p, s0, design, rho, seed)
X_true = Data$X; y = Data$y
# Fit
y_pred_mat = matrix(NA, nrow = length(y), ncol = s)
ESS = rep(NA, s)
for(k in 1:s){
  phi_mat = phi_matrix(m , p + 1, sparsity)
  X = t(phi_mat%*%t(X_true))
  if(Sampler == "Albert_Chib"){
    out = probitBasic(y, X, 
                      prior_variance, 
                      N_sim, burn_in_prop) 
  }
  if(Sampler == "Holmes_Held"){
    out = out = probitHH(y, X, 
                         prior_variance, 
                         N_sim, burn_in_prop)
  }
  # Fit
  q = X%*%(out$post_mean); mu = mean(q); sigma = sd(q)
  prob_pred <- pnorm(q, mean = mu, sd = sigma)
  for(i in 1:n){y_pred_mat[i, k] = rbinom(1, 1, prob_pred[i])}
  # Effective sample size
  ESS[k] <- mean(effectiveSize(mcmc(out$theta_chain[-(1:floor(N_sim*burn_in_prop)), ])))
  print(paste0(k, " of s=", s, " finished!"))
}
# Miss-clasification rate
error_vanilla = sum(y!=as.numeric(rowSums(y_pred_mat)>=alpha*s))/length(y) 
# Choice of alpha
alpha_computed = RPalpha(RP.out = y_pred_mat + 1, Y = y + 1, p1 = mean(y)) - 1 # +1 for using the 1/2 coding
error_opt  = sum(y!=as.numeric(rowSums(y_pred_mat)>=alpha_computed*s))/length(y) 
# Effective sample size 
avg_ESS = mean(ESS)
# Time taken
end_time <- Sys.time()
# time_taken <- round(as.numeric(end_time - start_time), 2)
time_taken <- end_time - start_time

return(list("error_vanilla"=error_vanilla,
            "error_opt" = error_opt,
            "ESS" = avg_ESS,
            "time_taken" = time_taken ))
}

# Function: Repeated Compressed Logit Regression -------------------------------------------------------
Repeated_Logit<- function(n = 100, p = 1000, s0 = 5, design = "AR1", rho = 0.5, seed = 1, # Data generation
                           #prior_variance = 1, # Prior hyper-parameter specification,
                           s = 100 , m = 40, sparsity = 10, # Projection
                           #N_sim = 10000, burn_in_prop = 0.5, 
                           burn = 5000, samp = 5000, verbose = 100,
                           #Sampler = "Albert_Chib", # Sampler specification
                           alpha = 0.5){  # Cut-off
  start_time <- Sys.time()
  # Data generation
  Data = DataGeneration(n, p, s0, design, rho, seed)
  X_true = Data$X; y = Data$y
  # Fit
  y_pred_mat = matrix(NA, nrow = length(y), ncol = s)
  ESS = rep(NA, s)
  for(k in 1:s){
    phi_mat = phi_matrix(m , p + 1, sparsity)
    X = t(phi_mat%*%t(X_true))
    
    out = logit.R(y, X, n=rep(1, length(y)),
                  m0=rep(0, ncol(X)), P0=matrix(0, nrow=ncol(X), ncol=ncol(X)),
                  samp, burn, verbose)
    
    # Fit
    q = X%*%(colMeans(out$beta)); exp(-q)/(1 + exp(-q))
    mu = mean(q); sigma = sd(q)
    prob_pred <- pnorm(q, mean = mu, sd = sigma)
    for(i in 1:n){y_pred_mat[i, k] = rbinom(1, 1, prob_pred[i])}
    # Effective sample size
    ESS[k] <- mean(effectiveSize(mcmc(out$beta)))
    print(paste0(k, " of s=", s, " finished!"))
  }
  # Miss-clasification rate
  error_vanilla = sum(y!=as.numeric(rowSums(y_pred_mat)>=alpha*s))/length(y) 
  # Choice of alpha
  alpha_computed = RPalpha(RP.out = y_pred_mat + 1, Y = y + 1, p1 = mean(y)) - 1 # +1 for using the 1/2 coding
  error_opt  = sum(y!=as.numeric(rowSums(y_pred_mat)>=alpha_computed*s))/length(y) 
  # Effective sample size 
  avg_ESS = mean(ESS)
  # Time taken
  end_time <- Sys.time()
  # time_taken <- round(as.numeric(end_time - start_time), 2)
  time_taken <- end_time - start_time
  
  return(list("error_vanilla"=error_vanilla,
              "error_opt" = error_opt,
              "ESS" = avg_ESS,
              "time_taken" = time_taken ))
}


# Functions: Compressed Probit for real data analysis-------------------------------------------------------
Data_Probit<- function(X, y, 
                       seed = 1, prop_train = 1, 
                       prior_variance = 1, # Prior hyper-parameter specification,
                       s = 25 , m = 40, sparsity = 10, # Projection
                       N_sim = 10000, burn_in_prop = 0.5, 
                       Sampler = "Albert_Chib", # Sampler specification
                       alpha = 0.5){  # Cut-off
  start_time <- Sys.time()
  # Data 
  n = nrow(X)
  set.seed(seed)
  subsample = sample(1:n, floor(n*prop_train), replace = FALSE)
  # Subset of data
  print(subsample); print(y)
  X_true = X[subsample, ]; y = y[subsample]
  print(y)
  n = nrow(X_true); p = ncol(X_true)
  # Fit
  y_pred_mat = matrix(NA, nrow = length(y), ncol = s)
  ESS = rep(NA, s)
  for(k in 1:s){
    phi_mat = phi_matrix(m , p , sparsity)
    X = t(phi_mat%*%t(X_true))
    if(Sampler == "Albert_Chib"){
      out = probitBasic(y, X, 
                        prior_variance, 
                        N_sim, burn_in_prop) 
    }
    if(Sampler == "Holmes_Held"){
      out  =  probitHH(y, X, 
                           prior_variance, 
                           N_sim, burn_in_prop)
    }
    # Fit
    q = X%*%(out$post_mean); mu = mean(q); sigma = sd(q)
    prob_pred <- pnorm(q, mean = mu, sd = sigma)
    for(i in 1:n){y_pred_mat[i, k] = rbinom(1, 1, prob_pred[i])}
    # Effective sample size
    ESS[k] <- mean(effectiveSize(mcmc(out$theta_chain[-(1:floor(N_sim*burn_in_prop)), ])))
    print(paste0(k, " of s=", s, " finished!"))
  }
  # Miss-clasification rate
  error_vanilla = sum(y!=as.numeric(rowSums(y_pred_mat)>=alpha*s))/length(y) 
  # Choice of alpha
  alpha_computed = RPalpha(RP.out = y_pred_mat + 1, Y = y + 1, p1 = mean(y)) - 1 # +1 for using the 1/2 coding
  error_opt  = sum(y!=as.numeric(rowSums(y_pred_mat)>=alpha_computed*s))/length(y) 
  # Effective sample size 
  avg_ESS = mean(ESS)
  # Time taken
  end_time <- Sys.time()
  # time_taken <- round(as.numeric(end_time - start_time), 2)
  time_taken <- end_time - start_time
  
  return(list("error_vanilla"=error_vanilla,
              "error_opt" = error_opt,
              "ESS" = avg_ESS,
              "time_taken" = time_taken ))
}



# Function: Time comparison in Compressed Probit Regression  -------------------------------------------------------
Repeated_Probit_timecomparison<- function(n = 100, p = 1000, s0 = 5, design = "Independent", rho = 0.9, seed = 1, # Data generation
                                          prior_variance = 1, # Prior hyper-parameter specification,
                                          s = 100 , m = 40, sparsity = 10, # Projection
                                          N_sim = 10000, burn_in_prop = 0.5, 
                                          Sampler = "Albert_Chib", # Sampler specification
                                          alpha = 0.5){  # Cut-off
start_time <- Sys.time()
# Data generation
Data = DataGeneration(n, p, s0, design, rho, seed)
X_true = Data$X; y = Data$y
# Fit
y_pred_mat = matrix(NA, nrow = length(y), ncol = s)
ESS = rep(NA, s)
for(k in 1:s){
  phi_mat = phi_matrix(m , p + 1, sparsity)
  X = t(phi_mat%*%t(X_true))
  if(Sampler == "Albert_Chib"){
    out = probitBasic(y, X, 
                      prior_variance, 
                      N_sim, burn_in_prop) 
  }
  if(Sampler == "Holmes_Held"){
    out = out = probitHH(y, X, 
                         prior_variance, 
                         N_sim, burn_in_prop)
  }
  # Fit
  q = X%*%(out$post_mean); mu = mean(q); sigma = sd(q)
  prob_pred <- pnorm(q, mean = mu, sd = sigma)
  for(i in 1:n){y_pred_mat[i, k] = rbinom(1, 1, prob_pred[i])}
  # Effective sample size
  ESS[k] <- mean(effectiveSize(mcmc(out$theta_chain[-(1:floor(N_sim*burn_in_prop)), ])))
  print(paste0(k, " of s=", s, " finished!"))
}
end_time <- Sys.time()
time_taken <- as.numeric((end_time - start_time))/s

start_time <- Sys.time()
alpha_computed = RPalpha(RP.out = y_pred_mat + 1, Y = y + 1, p1 = mean(y)) - 1 # +1 for using the 1/2 coding
end_time <- Sys.time()
alpha_time <- as.numeric(end_time - start_time)

return(list("time_taken" = time_taken, "alpha_time" = alpha_time, "ratio" = time_taken/(time_taken + alpha_time) ))
}

# Function: Time comparison in Compressed Probit Regression, Version 2 -------------------------------------------------------
Repeated_Probit_timecomparison_v2<- function(n = 100, p = 1000, s0 = 5, design = "Independent", rho = 0.9, seed = 1, # Data generation
                                          prior_variance = 1, # Prior hyper-parameter specification,
                                          s = 100 , m = 40, sparsity = 10, # Projection
                                          N_sim = 10000, burn_in_prop = 0.5, 
                                          Sampler = "Albert_Chib", # Sampler specification
                                          alpha = 0.5){  # Cut-off
start_time <- Sys.time()
# Data generation
Data = DataGeneration(n, p, s0, design, rho, seed)
X_true = Data$X; y = Data$y
# Fit
y_pred_mat = matrix(NA, nrow = length(y), ncol = s)
ESS = rep(NA, s)
for(k in 1:s){
  phi_mat = phi_matrix(m , p + 1, sparsity)
  X = t(phi_mat%*%t(X_true))
  if(Sampler == "Albert_Chib"){
    out = probitBasic(y, X, 
                      prior_variance, 
                      N_sim, burn_in_prop) 
  }
  if(Sampler == "Holmes_Held"){
    out = out = probitHH_fast(y, X, 
                         prior_variance, 
                         N_sim, burn_in_prop)
  }
  # Fit
  q = X%*%(out$post_mean); mu = mean(q); sigma = sd(q)
  prob_pred <- pnorm(q, mean = mu, sd = sigma)
  for(i in 1:n){y_pred_mat[i, k] = rbinom(1, 1, prob_pred[i])}
  # Effective sample size
  ESS[k] <- mean(effectiveSize(mcmc(out$theta_chain[-(1:floor(N_sim*burn_in_prop)), ])))
  print(paste0(k, " of s=", s, " finished!"))
}
end_time <- Sys.time()
time_taken <- as.numeric((end_time - start_time))/s

start_time <- Sys.time()
alpha_computed = RPalpha(RP.out = y_pred_mat + 1, Y = y + 1, p1 = mean(y)) - 1 # +1 for using the 1/2 coding
end_time <- Sys.time()
alpha_time <- as.numeric(end_time - start_time)

return(list("time_taken" = time_taken, "alpha_time" = alpha_time, "ratio" = time_taken/(time_taken + alpha_time) ))
}










                  

