# Dependencies ------------------------------------------------------------
library(latex2exp)
# Choice of R (Sparse) -------------------------------------------------------------
S_alg1 = readRDS("E:/Projects/BCC_plots/R1_plots/weaklearner_alg1_sparse.rds")[1:112, ]
#S_alg1 = readRDS("E:/Projects/BCC_plots/R1_plots/weaklearner_alg1_dense.rds")
colnames(S_alg1) <- c("n", "p", "s0", "rho", "Algorithm", "s",  "error_vanilla", "error_opt")

# n = 10^2
# p_grid = c(1000, 10000)
# rho_grid = c(0.0, 0.5, 0.7, 0.9)
# s0_grid = c(5, 10)
# Algorithms = 1
# s_grid = c(2, 5, 10, 20, 30, 40, 50)
par(mfrow= c(1, 4))
grid = 0:3# grid = 8:11 for sparse, 0:3 for dense
for(k in grid){
  if(k==grid[1]){
    plot(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 7], 
         xlab = "R", ylab = "Error", ylim = c(0, 0.6),
         main = TeX(sprintf(r'($\rho = 0.0$)' )),
         type = "l", lwd = 2, lty = 1)
    lines(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 8], 
          lwd = 2, lty = 2)  
  }
  
  if(k==grid[2]){
    plot(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 7], 
         xlab = "R", ylab = "Error", ylim = c(0, 0.6),
         main = TeX(sprintf(r'($\rho = 0.5$)' )),
         type = "l", lwd = 2, lty = 1)
    lines(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 8], 
          lwd = 2, lty = 2)  
  }
  
  if(k==grid[3]){
    plot(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 7], 
         xlab = "R", ylab = "Error", ylim = c(0, 0.6),
         main = TeX(sprintf(r'($\rho = 0.7$)' )),
         type = "l", lwd = 2, lty = 1)
    lines(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 8], 
          lwd = 2, lty = 2)  
  }
  
  if(k==grid[4]){
    plot(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 7], 
         xlab = "R", ylab = "Error", ylim = c(0, 0.6),
         main = TeX(sprintf(r'($\rho = 0.9$)' )),
         type = "l", lwd = 2, lty = 1)
    lines(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 8], 
          lwd = 2, lty = 2)  
  }
  legend("topright", legend=c("AC", "AC+"),
         cex=1, lwd = 2, lty = 1:2)
   
}


# time comparison (Sparse)---------------------------------------------------------
par(mfrow= c(1, 4))
grid = 0:3
# par(mfrow= c(1, 1)) # par(mfrow= c(1, 4)) for sparse
# grid = 0 # 0:3 for sparse
for(k in grid){
    # Algorithm 1
    time_comp = as.data.frame(readRDS("E:/Projects/BCC_plots/R1_plots/result_sparse.rds"))
    #time_comp = as.data.frame(readRDS("E:/Projects/BCC_plots/R1_plots/result_dense.rds"))
    colnames(time_comp) <- c("n", "p", "s0", "rho", "Algorithm", 
                             "time_vanilla", "time_opt", "time_ratio")
    time_comp$time_opt = time_comp$time_vanilla + time_comp$time_opt
    algorithm = 1
    time_comp = time_comp[time_comp$Algorithm==algorithm, ]
    plot(time_comp[(4*k + 1):(4*(k+1)), 4], time_comp[(4*k + 1):(4*(k+1)), 6], 
         xlab = expression(rho), ylab = "time (in seconds)", ylim = c(0.09, 0.15),
         type = "l", lwd = 2, col = 1)
    lines(time_comp[(4*k + 1):(4*(k+1)), 4], time_comp[(4*k + 1):(4*(k+1)), 7], 
          lwd = 2, col = 2) 
    
    # Algorithm 2
    time_comp = as.data.frame(readRDS("E:/Projects/BCC_plots/R1_plots/result_sparse.rds"))
    #time_comp = as.data.frame(readRDS("E:/Projects/BCC_plots/R1_plots/result_dense.rds"))
    colnames(time_comp) <- c("n", "p", "s0", "rho", "Algorithm", 
                             "time_vanilla", "time_opt", "time_ratio")
    time_comp$time_opt = time_comp$time_vanilla + time_comp$time_opt
    algorithm = 2
    time_comp = time_comp[time_comp$Algorithm==algorithm, ]
    lines(time_comp[(4*k + 1):(4*(k+1)), 4], time_comp[(4*k + 1):(4*(k+1)), 6], 
         lwd = 2, col = 3)
    lines(time_comp[(4*k + 1):(4*(k+1)), 4], time_comp[(4*k + 1):(4*(k+1)), 7], 
          lwd = 2, col = 4) 
    
    legend("bottomright", legend=c("AC", "AC+","HH", "HH+"),
           cex=0.6, lwd = 2, col = 1:4)
}


# Choice of R (Dense) -------------------------------------------------------------
#S_alg1 = readRDS("E:/Projects/BCC_plots/R1_plots/weaklearner_alg1_sparse.rds")[1:112, ]
S_alg1 = readRDS("E:/Projects/BCC_plots/R1_plots/weaklearner_alg1_dense.rds")
colnames(S_alg1) <- c("n", "p", "s0", "rho", "Algorithm", "s",  "error_vanilla", "error_opt")

# n = 10^2
# p_grid = c(1000, 10000)
# rho_grid = c(0.0, 0.5, 0.7, 0.9)
# s0_grid = c(5, 10)
# Algorithms = 1
# s_grid = c(2, 5, 10, 20, 30, 40, 50)
par(mfrow= c(1, 4))
grid = 0:3# grid = 8:11 for sparse, 0:3 for dense
for(k in grid){
  if(k==grid[1]){
    plot(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 7], 
         xlab = "R", ylab = "Error", ylim = c(0, 0.6),
         main = TeX(sprintf(r'($\rho = 0.0$)' )),
         type = "l", lwd = 2, lty = 1)
    lines(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 8], 
          lwd = 2, lty = 2)  
  }
  
  if(k==grid[2]){
    plot(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 7], 
         xlab = "R", ylab = "Error", ylim = c(0, 0.6),
         main = TeX(sprintf(r'($\rho = 0.5$)' )),
         type = "l", lwd = 2, lty = 1)
    lines(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 8], 
          lwd = 2, lty = 2)  
  }
  
  if(k==grid[3]){
    plot(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 7], 
         xlab = "R", ylab = "Error", ylim = c(0, 0.6),
         main = TeX(sprintf(r'($\rho = 0.7$)' )),
         type = "l", lwd = 2, lty = 1)
    lines(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 8], 
          lwd = 2, lty = 2)  
  }
  
  if(k==grid[4]){
    plot(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 7], 
         xlab = "R", ylab = "Error", ylim = c(0, 0.6),
         main = TeX(sprintf(r'($\rho = 0.9$)' )),
         type = "l", lwd = 2, lty = 1)
    lines(S_alg1[(7*k + 1):(7*(k+1)), 6], S_alg1[(7*k + 1):(7*(k+1)), 8], 
          lwd = 2, lty = 2)  
  }
  legend("topright", legend=c("AC", "AC+"),
         cex=1, lwd = 2, lty = 1:2)
  
}


# time comparison (Dense)---------------------------------------------------------
# par(mfrow= c(1, 4))
# grid = 0:3
par(mfrow= c(1, 1)) # par(mfrow= c(1, 4)) for sparse
grid = 0 # 0:3 for sparse
for(k in grid){
  # Algorithm 1
  #time_comp = as.data.frame(readRDS("E:/Projects/BCC_plots/R1_plots/result_sparse.rds"))
  time_comp = as.data.frame(readRDS("E:/Projects/BCC_plots/R1_plots/result_dense.rds"))
  colnames(time_comp) <- c("n", "p", "s0", "rho", "Algorithm", 
                           "time_vanilla", "time_opt", "time_ratio")
  time_comp$time_opt = time_comp$time_vanilla + time_comp$time_opt
  algorithm = 1
  time_comp = time_comp[time_comp$Algorithm==algorithm, ]
  plot(time_comp[(4*k + 1):(4*(k+1)), 4], time_comp[(4*k + 1):(4*(k+1)), 6], 
       xlab = expression(rho), ylab = "time (in seconds)", ylim = c(0.09, 0.15),
       type = "l", lwd = 2, col = 1)
  lines(time_comp[(4*k + 1):(4*(k+1)), 4], time_comp[(4*k + 1):(4*(k+1)), 7], 
        lwd = 2, col = 2) 
  
  # Algorithm 2
  #time_comp = as.data.frame(readRDS("E:/Projects/BCC_plots/R1_plots/result_sparse.rds"))
  time_comp = as.data.frame(readRDS("E:/Projects/BCC_plots/R1_plots/result_dense.rds"))
  colnames(time_comp) <- c("n", "p", "s0", "rho", "Algorithm", 
                           "time_vanilla", "time_opt", "time_ratio")
  time_comp$time_opt = time_comp$time_vanilla + time_comp$time_opt
  algorithm = 2
  time_comp = time_comp[time_comp$Algorithm==algorithm, ]
  lines(time_comp[(4*k + 1):(4*(k+1)), 4], time_comp[(4*k + 1):(4*(k+1)), 6], 
        lwd = 2, col = 3)
  lines(time_comp[(4*k + 1):(4*(k+1)), 4], time_comp[(4*k + 1):(4*(k+1)), 7], 
        lwd = 2, col = 4) 
  
  legend("bottomright", legend=c("AC", "AC+","HH", "HH+"),
         cex=0.6, lwd = 2, col = 1:4)
}

