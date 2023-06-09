#################### Dependencies
#source("BCC_Functions.R")

library(foreach)
library(doParallel)


################### Repeated simulations
# Simulation grid
sampler_grid = c("Albert_Chib","Holmes_Held")
p_grid = c(10^3, 10^4)
rho_grid = c(0.0, 0.5, 0.7,  0.9)
s0_grid = c(5, 10, 10^3)

# Repeatation
nrep = 50
# Run simulation
#case_count = 0
complete_results = matrix(NA, nrow = 48, ncol = 10)
for(sampler_choice %in% sampler_grid){
for(p in p_grid){
for(rho in rho_grid){
for(s0 in s0_grid){
################### Set up parallel processing environment
no_cores <- detectCores()
cl <- makeCluster(no_cores)
registerDoParallel(cl)
result <- foreach(i = 1:nrep, .combine = rbind)%dopar%{
  source("BCC_Functions.R")
  tryCatch({
  out = 
  Repeated_Probit(n = 100, p , s0 , design = "AR1", rho, seed = i, # Data generation
                  prior_variance = 1, # Prior hyper-parameter specification,
                  s = 50 , m = 40, sparsity = s0, # Projection
                  N_sim = 10000, burn_in_prop = 0.5,
                  Sampler = sampler_choice, # Sampler
                  alpha = 0.5) # Cut-off 
  #out = 
  #Repeated_Logit(n = 100, p = 1000, s0 = 1000, design = "AR1", rho = 0.9, seed = i, # Data generation
  #               s = 50 , m = 40, sparsity = 10, # Projection
  #               burn = 5000, samp = 5000, verbose = 5000,
  #               alpha = 0.5)
  
  c(out$error_vanilla, out$error_opt, out$ESS)
  }, error = function(e) return(rep(NA, 3)))
} 
#stopCluster(cl)
Sys.sleep(10)

out1 = quantile(result[ , 1], prob = c(0.025, 0.5, 0.975), na.rm = T)
out2 = quantile(result[ , 2], prob = c(0.025, 0.5, 0.975), na.rm = T)
out3 = quantile(result[ , 3], prob = c(0.025, 0.5, 0.975), na.rm = T)

case_count = case_count + 1
complete_results[case_count, ] = c(n, p, rho, s0, out1[2], (out1[3]-out1[1])/2 ,out2[2], (out2[3]-out2[1])/2, out3[2], (out3[3]-out3[1])/2)
print(paste0("Case ", case_count, " of", nrow(complete_results) ," completed!"))
}
}
}
}
complete_results <- cbind(c(rep("AC", 24), rep("HH", 24)), complete_results)
colnames(complete_results) <- c("method","n", "p", "rho", "Sparsity", "mean_vanilla", "SD_vanilla", "mean_optimised(+)", "SD_optimised(+)", "mean_ESS", "SD_ESS")
