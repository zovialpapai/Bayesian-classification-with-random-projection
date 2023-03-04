#################### Dependencies
#source("BCC_Functions.R")

library(foreach)
library(doParallel)


################### Repeated simulations
# Simulation grid
#p_grid = c(100, 10^3, 10^4)
#rho_grid = c(0.0, 0.5, 0.7,  0.9)
#s0_grid = c(5, 10)
# Repeatation
nrep = 50


# Run simulation
#case_count = 0
#complete_results = matrix(NA, nrow = 24, ncol = 12)
#for(p in p_grid){
#for(rho in rho_grid){
#for(s0 in s0_grid){
################### Set up parallel processing environment
no_cores <- detectCores()
cl <- makeCluster(no_cores)
registerDoParallel(cl)
result <- foreach(i = 1:nrep, .combine = rbind)%dopar%{
  source("BCC_Functions.R")
  tryCatch({
  out = 
  Repeated_Probit(n = 100, p = 10000, s0 = 10, design = "AR1", rho = 0.9, seed = i, # Data generation
                  prior_variance = 1, # Prior hyper-parameter specification,
                  s = 50 , m = 40, sparsity = 10, # Projection
                  N_sim = 10000, burn_in_prop = 0.5,
                  Sampler = "Holmes_Held", # Sampler
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

out1
out2
out3

n = 100; p = 10000; s0 = 10; rho = 0.9
saveRDS(result, paste0("results/","n=", n, "p=", p , "s0=", s0 , "rho", 100*rho, ".rds") )

#case_count = case_count + 1
#complete_results[case_count, ] = c(p, rho, s0, out1, out2, out3)
#print(paste0("Case ", case_count, " of", nrow(complete_results) ," completed!"))
#}
#}
#}

#colnames(complete_results) <- c("p", "rho", "s0",  
#                                "lb_error_vanilla", "avg_error_vanilla", "ub_error_vanilla",
#                                "lb_error_opt", "avg_error_opt", "ub_error_opt",
#                                "lb_ESS", "avg_ESS", "ub_ESS")
                                
                                
#saveRDS(complete_results, "results/complete_results_AC.rds")
#complete_results