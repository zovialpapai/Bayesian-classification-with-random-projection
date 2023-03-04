# Dependencies ------------------------------------------------------------
library(stringr)

library(foreach)
library(doParallel)

library("readxl")

#source("C:/Users/zovia/OneDrive/Desktop/BCC_Functions.R")

# Leukemia data: preprocessing-----------------------------------------------------------
## Raw data
X_train = read.csv("Leukomia/data_set_ALL_AML_independent.csv")
X_independent = read.csv("Leukomia/data_set_ALL_AML_train.csv",
                          quote = "")
dim(X_train); dim(X_independent)

y = as.data.frame(read.csv("Leukomia/actual.csv"))

## Pre-processing y and X
y$cancer = as.numeric(y$cancer=="ALL")

X_full = rbind(t(X_train[ , -c(1,2, 2*(2:35))]), t(X_independent[ , -c(1,2, 2*(2:39))]) )
dim(X_full)
X_index = as.data.frame(cbind(as.numeric(substr(rownames(X_full), 2, 4)), X_full))
colnames(X_index)[1] <- "patient"

## Data alignment
aligned_y_X = merge(y, X_index, by = "patient")
dim(aligned_y_X)

## Final data
X = as.matrix(aligned_y_X[ , -c(1:2)])
y = as.vector(aligned_y_X$cancer)


# Leukemia data: Implementation of BCC ---------------------------------------------------
#nrep = 50
################### Set up parallel processing environment
#no_cores <- detectCores()
#cl <- makeCluster(no_cores)
#registerDoParallel(cl)
#result <- foreach(i = 1:nrep, .combine = rbind)%dopar%{
#  source("BCC_Functions.R")
#  tryCatch({
#  out = 
#  Data_Probit(X, y, seed = i, prop_train = 0.6, 
#            prior_variance = 1, # Prior hyper-parameter specification,
#            s = 25 , m = 40, sparsity = 5, # Projection
#            N_sim = 10000, burn_in_prop = 0.5, 
#            Sampler = "Holmes_Held", # Sampler specification
#            alpha = 0.5)
#  c(out$error_vanilla, out$error_opt, out$ESS)
#  }, error = function(e) return(rep(NA, 3)))
#} 
##stopCluster(cl)
#Sampler = "Holmes_Held"; prop_train= 0.6
#saveRDS(result, paste0("results/","Leukomia_Sampler=", Sampler,",prop_train=", prop_train, ".rds") )

#out1 = quantile(result[ , 1], prob = c(0.025, 0.5, 0.975), na.rm = T)
#out2 = quantile(result[ , 2], prob = c(0.025, 0.5, 0.975), na.rm = T)
#out3 = quantile(result[ , 3], prob = c(0.025, 0.5, 0.975), na.rm = T)

#out1
#out2
#out3


###################### Lung cancer data : preprocessing --------------------------------------------------------
Data <- read_excel("Lung_cancer/Gordon-2002_LungCancer.xlsx")
X = as.matrix(t(Data[-1, -1])); dim(X)
X = matrix(as.numeric(X), ncol = ncol(X))
y = as.numeric(as.matrix(Data[1, -1]=="MPM")[1, ]); length(y); table (y)
############################ Lung cancer data: Implementation of BCC
#nrep = 50
################### Set up parallel processing environment
#no_cores <- detectCores()
#cl <- makeCluster(no_cores)
#registerDoParallel(cl)
#result <- foreach(i = 1:nrep, .combine = rbind)%dopar%{
#  source("BCC_Functions.R")
#  tryCatch({
#  out = 
#  Data_Probit(X, y, seed = i, prop_train = 0.5, 
#            prior_variance = 1, # Prior hyper-parameter specification,
#            s = 25 , m = 40, sparsity = 5, # Projection
#            N_sim = 10000, burn_in_prop = 0.5, 
#            Sampler = "Holmes_Held", # Sampler specification
#            alpha = 0.5)
#  c(out$error_vanilla, out$error_opt, out$ESS)
#  }, error = function(e) return(rep(NA, 3)))
#} 
##stopCluster(cl)
#Sampler = "Holmes_Held"; prop_train= 0.5
#saveRDS(result, paste0("results/","LungCancer_Sampler=", Sampler,",prop_train=", prop_train, ".rds") )

#out1 = quantile(result[ , 1], prob = c(0.025, 0.5, 0.975), na.rm = T)
#out2 = quantile(result[ , 2], prob = c(0.025, 0.5, 0.975), na.rm = T)
#out3 = quantile(result[ , 3], prob = c(0.025, 0.5, 0.975), na.rm = T)

#out1
#out2
#out3

###################### Prostate cancer data : preprocessing --------------------------------------------------------
Data <- read_excel("Prostate_cancer/Singh-2002_ProstateCancer.xlsx")
X = as.matrix(t(Data[-1, -1])); dim(X)
X = matrix(as.numeric(X), ncol = ncol(X))
y = as.numeric(as.matrix(Data[1, -1]=="N")[1, ]); length(y); table (y)

############################ Lung cancer data: Implementation of BCC
nrep = 50
################### Set up parallel processing environment
no_cores <- detectCores()
cl <- makeCluster(no_cores)
registerDoParallel(cl)
result <- foreach(i = 1:nrep, .combine = rbind)%dopar%{
  source("BCC_Functions.R")
  tryCatch({
  out = 
  Data_Probit(X, y, seed = i, prop_train = 0.7, 
            prior_variance = 1, # Prior hyper-parameter specification,
            s = 25 , m = 40, sparsity = 5, # Projection
            N_sim = 10000, burn_in_prop = 0.5, 
            Sampler = "Holmes_Held", # Sampler specification
            alpha = 0.5)
  c(out$error_vanilla, out$error_opt, out$ESS)
  }, error = function(e) return(rep(NA, 3)))
} 
##stopCluster(cl)
Sampler = "Holmes_Held"; prop_train= 0.7
saveRDS(result, paste0("results/","ProstateCancer_Sampler=", Sampler,",prop_train=", prop_train, ".rds") )

out1 = quantile(result[ , 1], prob = c(0.025, 0.5, 0.975), na.rm = T)
out2 = quantile(result[ , 2], prob = c(0.025, 0.5, 0.975), na.rm = T)
out3 = quantile(result[ , 3], prob = c(0.025, 0.5, 0.975), na.rm = T)

out1
out2
out3

