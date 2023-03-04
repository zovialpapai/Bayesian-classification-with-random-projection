# Dependencies ----------------------------------------------------------
library(Rcpp)
library(truncnorm)
library(mvtnorm)

# Rcpp -------------------------------------------------------------------
sourceCpp(code=
'
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <omp.h>

using namespace Rcpp;
using namespace arma;

// norm_rs(a, b)
// generates a sample from a N(0,1) RV restricted to be in the interval
// (a,b) via rejection sampling.
// ======================================================================

// [[Rcpp::export]]

double norm_rs(double a, double b)
{
   double  x;
   x = Rf_rnorm(0.0, 1.0);
   while( (x < a) || (x > b) ) x = norm_rand();
   return x;
}

// half_norm_rs(a, b)
// generates a sample from a N(0,1) RV restricted to the interval
// (a,b) (with a > 0) using half normal rejection sampling.
// ======================================================================

// [[Rcpp::export]]

double half_norm_rs(double a, double b)
{
   double   x;
   x = fabs(norm_rand());
   while( (x<a) || (x>b) ) x = fabs(norm_rand());
   return x;
}

// unif_rs(a, b)
// generates a sample from a N(0,1) RV restricted to the interval
// (a,b) using uniform rejection sampling. 
// ======================================================================

// [[Rcpp::export]]

double unif_rs(double a, double b)
{
   double xstar, logphixstar, x, logu;

   // Find the argmax (b is always >= 0)
   // This works because we want to sample from N(0,1)
   if(a <= 0.0) xstar = 0.0;
   else xstar = a;
   logphixstar = R::dnorm(xstar, 0.0, 1.0, 1.0);

   x = R::runif(a, b);
   logu = log(R::runif(0.0, 1.0));
   while( logu > (R::dnorm(x, 0.0, 1.0,1.0) - logphixstar))
   {
      x = R::runif(a, b);
      logu = log(R::runif(0.0, 1.0));
   }
   return x;
}

// exp_rs(a, b)
// generates a sample from a N(0,1) RV restricted to the interval
// (a,b) using exponential rejection sampling.
// ======================================================================

// [[Rcpp::export]]

double exp_rs(double a, double b)
{
  double  z, u, rate;

//  Rprintf("in exp_rs");
  rate = 1/a;
//1/a

   // Generate a proposal on (0, b-a)
   z = R::rexp(rate);
   while(z > (b-a)) z = R::rexp(rate);
   u = R::runif(0.0, 1.0);

   while( log(u) > (-0.5*z*z))
   {
      z = R::rexp(rate);
      while(z > (b-a)) z = R::rexp(rate);
      u = R::runif(0.0,1.0);
   }
   return(z+a);
}




// rnorm_trunc( mu, sigma, lower, upper)
//
// generates one random normal RVs with mean mu and standard
// deviation sigma, truncated to the interval (lower,upper), where
// lower can be -Inf and upper can be Inf.
//======================================================================

// [[Rcpp::export]]
double rnorm_trunc (double mu, double sigma, double lower, double upper)
{
int change;
 double a, b;
 double logt1 = log(0.150), logt2 = log(2.18), t3 = 0.725;
 double z, tmp, lograt;

 change = 0;
 a = (lower - mu)/sigma;
 b = (upper - mu)/sigma;

 // First scenario
 if( (a == R_NegInf) || (b == R_PosInf))
   {
     if(a == R_NegInf)
       {
     change = 1;
     a = -b;
     b = R_PosInf;
       }

     // The two possibilities for this scenario
     if(a <= 0.45) z = norm_rs(a, b);
     else z = exp_rs(a, b);
     if(change) z = -z;
   }
 // Second scenario
 else if((a * b) <= 0.0)
   {
     // The two possibilities for this scenario
     if((R::dnorm(a, 0.0, 1.0,1.0) <= logt1) || (R::dnorm(b, 0.0, 1.0, 1.0) <= logt1))
       {
     z = norm_rs(a, b);
       }
     else z = unif_rs(a,b);
   }
 // Third scenario
 else
   {
     if(b < 0)
       {
     tmp = b; b = -a; a = -tmp; change = 1;
       }

     lograt = R::dnorm(a, 0.0, 1.0, 1.0) - R::dnorm(b, 0.0, 1.0, 1.0);
     if(lograt <= logt2) z = unif_rs(a,b);
     else if((lograt > logt1) && (a < t3)) z = half_norm_rs(a,b);
     else z = exp_rs(a,b);
     if(change) z = -z;
   }
   double output;
   output = sigma*z + mu;
 return (output);
}


// rtnm( mu, sigma, lower, upper, cores)
//
// generates one random normal RVs with mean mu and standard
// deviation sigma, truncated to the interval (lower,upper), where
// lower can be -Inf and upper can be Inf.
// mu, sigma, lower, upper are vectors, and vectorized calls of this function
// speed up computation
// cores is an intege, representing the number of cores to be used in parallel
//======================================================================


// [[Rcpp::export]]

Rcpp::NumericVector rtnm(Rcpp::NumericVector mus, Rcpp::NumericVector sigmas, Rcpp::NumericVector lower, Rcpp::NumericVector upper, int cores){
  omp_set_num_threads(cores);
  int nobs = mus.size();
  Rcpp::NumericVector out(nobs);
  double logt1 = log(0.150), logt2 = log(2.18), t3 = 0.725;
    double a,b, z, tmp, lograt;

     int  change;

  #pragma omp parallel for schedule(dynamic)   
  for(int i=0;i<nobs;i++) {  

     a = (lower(i) - mus(i))/sigmas(i);
     b = (upper(i) - mus(i))/sigmas(i);
     change=0;
     // First scenario
     if( (a == R_NegInf) || (b == R_PosInf))
       {
         if(a == R_NegInf)
           {
              change = 1;
              a = -b;
              b = R_PosInf;
           }

         // The two possibilities for this scenario
         if(a <= 0.45) z = norm_rs(a, b);
         else z = exp_rs(a, b);
         if(change) z = -z;
       }
     // Second scenario
     else if((a * b) <= 0.0)
       {
         // The two possibilities for this scenario
         if((R::dnorm(a, 0.0, 1.0,1.0) <= logt1) || (R::dnorm(b, 0.0, 1.0, 1.0) <= logt1))
           {
                z = norm_rs(a, b);
           }
         else z = unif_rs(a,b);
       }

     // Third scenario
     else
       {
         if(b < 0)
           {
                tmp = b; b = -a; a = -tmp; change = 1;
           }

         lograt = R::dnorm(a, 0.0, 1.0, 1.0) - R::dnorm(b, 0.0, 1.0, 1.0);
         if(lograt <= logt2) z = unif_rs(a,b);
         else if((lograt > logt1) && (a < t3)) z = half_norm_rs(a,b);
         else z = exp_rs(a,b);
         if(change) z = -z;
       }
    out(i)=sigmas(i)*z + mus(i);          
  }

return(out);
}

// [[Rcpp::export]]
arma::vec part_cpp(int  n, int p, arma::vec y, arma::mat  X, arma::vec  z, arma::vec  w , arma::vec  u, arma::vec  M, arma::mat  S) {
  
  for(int j = 0; j < n; j++) {
  double z_old = z(j);
  
  double m =  (X.row(j) * M).eval()(0,0);
  //mat M =  X * M;
  m = m - w(j) * (z(j) - m);
  
  if (y(j) == 0){
   z(j) =  rnorm_trunc(m, u(j), -10^10, 0);
  } else {
    z(j) =  rnorm_trunc(m, u(j), 0, 10^10);
  }
  
  M = M + (z(j) - z_old) * S.col(j);
  }
  
  return(M);
}'
)

# Function: Probit Regression (Homes and Held) fast-------------------------------------------------------------------
probitHH_fast <- function(y, X, 
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
  
  h <- diag(X%*%S)
  w <- h/(1-h)
  u <- w + 1
  
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
    M = part_cpp(n, D, y, X, z, w, u, M, S)
    
    # Posterior of M | Z
    theta_chain_holmes[t, ] <- c(rmvnorm(1, M, V))
    print(t)
  }
  
  post_mean = colMeans(theta_chain_holmes[((burn_in+1):N_sim), ])
  return(list("theta_chain" = theta_chain_holmes, "post_mean" = post_mean))
}
