# Dependencies ------------------------------------------------------------
library(foreach)
library(doParallel)
# Library for sampling from Multivariate Normal distribution
require(mvtnorm)
# Library for sampling from Truncated Normal distribution
require(truncnorm)
# to calculate ESS
require(coda)
# choice of alpha
require(RPEnsemble)
# main functions
source("BCC_Functions.R")


################### Sparse cases
n = 10^2
p_grid = c(10^3, 10^4)
rho_grid = c(0.0, 0.5, 0.7, 0.9)
s0_grid = c(5, 10)
Algorithms = 1 # Change it 2 for Holmes and Held
s_grid = c(2, 5, 10, 20, 30, 40, 50)

result_sparse = matrix(NA, nrow = 128, ncol = 8); counter = 1
for(p in p_grid){
for(s0 in s0_grid){
for(rho in rho_grid){
for(Algorithm in Algorithms){
for(s in s_grid){
if(Algorithm==1){
out = 
  Repeated_Probit(n , p , s0 , design = "AR1", rho , seed = 1, # Data generation
                  prior_variance = 1, # Prior hyper-parameter specification,
                  s , m = 40, sparsity = 10, # Projection
                  N_sim = 10000, burn_in_prop = 0.5,
                  Sampler = "Albert_Chib", # Sampler
                  alpha = 0.5) # Cut-off
}else{
out = 
  Repeated_Probit(n , p , s0 , design = "AR1", rho, seed = 1, # Data generation
                  prior_variance = 1, # Prior hyper-parameter specification,
                  s , m = 40, sparsity = 10, # Projection
                  N_sim = 10000, burn_in_prop = 0.5,
                  Sampler = "Holmes_Held", # Sampler
                  alpha = 0.5) # Cut-off
}
result_sparse[counter, ] = c(n, p, s0, rho, Algorithm, s,  out$error_vanilla, out$error_opt)
print(paste0(counter, " of 128 cases fisnished!"))
counter = counter + 1
}
}
}
}
}
saveRDS(result_sparse, "weaklearner_alg1_sparse.rds")
################### Dense cases
n = 10^2
p_grid = 10^3
rho_grid = c(0.0, 0.5, 0.7, 0.9)
s0_grid = 10^3
Algorithms = 1 # Change it 2 for Holmes and Held
s_grid = c(2, 5, 10, 20, 30, 40, 50)

result_dense = matrix(NA, nrow = 32, ncol = 8); counter = 1
for(p in p_grid){
for(s0 in s0_grid){
for(rho in rho_grid){
for(Algorithm in Algorithms){
for(s in s_grid){
if(Algorithm==1){
out = 
  Repeated_Probit(n , p , s0 , design = "AR1", rho , seed = 1, # Data generation
                  prior_variance = 1, # Prior hyper-parameter specification,
                  s , m = 40, sparsity = 10, # Projection
                  N_sim = 10000, burn_in_prop = 0.5,
                  Sampler = "Albert_Chib", # Sampler
                  alpha = 0.5) # Cut-off
}else{
out = 
  Repeated_Probit(n , p , s0 , design = "AR1", rho, seed = 1, # Data generation
                  prior_variance = 1, # Prior hyper-parameter specification,
                  s , m = 40, sparsity = 10, # Projection
                  N_sim = 10000, burn_in_prop = 0.5,
                  Sampler = "Holmes_Held", # Sampler
                  alpha = 0.5) # Cut-off
}
result_dense[counter, ] = c(n, p, s0, rho, Algorithm, s,  out$error_vanilla, out$error_opt)
print(paste0(counter, " of 128 cases fisnished!"))
counter = counter + 1
}
}
}
}
}
saveRDS(result_dense, "weaklearner_alg1_dense.rds")

