# Dependencies ------------------------------------------------------------
library(stringr)

library("ggplot2")
library("ggpubr")

library("readxl")

# source("C:/Users/zovia/OneDrive/Desktop/BCC_Functions.R")

# Data source: https://data.mendeley.com/datasets/ynp2tst2hh/1

# Leukemia data -----------------------------------------------------------
## Raw data
X_train = read.csv("C:/Users/zovia/OneDrive/Desktop/Golub_dataset/data_set_ALL_AML_independent.csv")
X_independent = read.csv("C:/Users/zovia/OneDrive/Desktop/Golub_dataset/data_set_ALL_AML_train.csv",
                          quote = "")
dim(X_train); dim(X_independent)

y = as.data.frame(read.csv("C:/Users/zovia/OneDrive/Desktop/Golub_dataset/actual.csv"))

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


# Plots 


nrep = 50

AC = readRDS("E:/Projects/BCC_plots/output/Leukomia_Sampler=Albert_Chib,prop_train=0.5.rds")
HH = readRDS("E:/Projects/BCC_plots/output/Leukomia_Sampler=Holmes_Held,prop_train=0.5.rds")
AC_HH =data.frame (c( AC[ ,1], AC[,2], HH[,1], HH[,2]),
                   c( rep("AC", nrep), rep("AC+", nrep), rep("HH", nrep), rep("HH+", nrep)))
colnames(AC_HH) <- c("error", "method")
p1 <- ggplot(AC_HH, aes(x=method, y=error)) +
              geom_boxplot(color="red", fill="orange", alpha=0.2) +
              ggtitle("Sampling proportion = 0.5")

AC = readRDS("E:/Projects/BCC_plots/output/Leukomia_Sampler=Albert_Chib,prop_train=0.6.rds")
HH = readRDS("E:/Projects/BCC_plots/output/Leukomia_Sampler=Holmes_Held,prop_train=0.6.rds")
AC_HH =data.frame (c( AC[ ,1], AC[,2], HH[,1], HH[,2]),
                   c( rep("AC", nrep), rep("AC+", nrep), rep("HH", nrep), rep("HH+", nrep)))
colnames(AC_HH) <- c("error", "method")
p2 <- ggplot(AC_HH, aes(x=method, y=error)) +
              geom_boxplot(color="red", fill="orange", alpha=0.2) +
              ggtitle("Sampling proportion = 0.6")

AC = readRDS("E:/Projects/BCC_plots/output/Leukomia_Sampler=Albert_Chib,prop_train=0.7.rds")
HH = readRDS("E:/Projects/BCC_plots/output/Leukomia_Sampler=Holmes_Held,prop_train=0.7.rds")
AC_HH =data.frame (c( AC[ ,1], AC[,2], HH[,1], HH[,2]),
                   c( rep("AC", nrep), rep("AC+", nrep), rep("HH", nrep), rep("HH+", nrep)))
colnames(AC_HH) <- c("error", "method")
p3 <- ggplot(AC_HH, aes(x=method, y=error)) +
       geom_boxplot(color="red", fill="orange", alpha=0.2) +
       ggtitle("Sampling proportion = 0.7")

ggarrange(p1, p2, p3, ncol = 3, nrow = 1)




# Lung cancer data --------------------------------------------------------
Data <- read_excel("E:/Projects/BCC_plots/Gordon-2002_LungCancer.xlsx")
X = as.matrix(t(Data[-1, -1])); dim(X)
X = matrix(as.numeric(X), ncol = ncol(X))
y = as.numeric(as.matrix(Data[1, -1]=="MPM")[1, ]); length(y); table (y)


# Plots 
nrep = 50

AC = readRDS("E:/Projects/BCC_plots/output/LungCancer_Sampler=Albert_Chib,prop_train=0.5.rds")
HH = readRDS("E:/Projects/BCC_plots/output/LungCancer_Sampler=Holmes_Held,prop_train=0.5.rds")
AC_HH =data.frame (c( AC[ ,1], AC[,2], HH[,1], HH[,2]),
                   c( rep("AC", nrep), rep("AC+", nrep), rep("HH", nrep), rep("HH+", nrep)))
colnames(AC_HH) <- c("error", "method")
p1 <- ggplot(AC_HH, aes(x=method, y=error)) +
  geom_boxplot(color="red", fill="orange", alpha=0.2) +
  ggtitle("Sampling proportion = 0.5")

AC = readRDS("E:/Projects/BCC_plots/output/LungCancer_Sampler=Albert_Chib,prop_train=0.6.rds")
HH = readRDS("E:/Projects/BCC_plots/output/LungCancer_Sampler=Holmes_Held,prop_train=0.6.rds")
AC_HH =data.frame (c( AC[ ,1], AC[,2], HH[,1], HH[,2]),
                   c( rep("AC", nrep), rep("AC+", nrep), rep("HH", nrep), rep("HH+", nrep)))
colnames(AC_HH) <- c("error", "method")
p2 <- ggplot(AC_HH, aes(x=method, y=error)) +
  geom_boxplot(color="red", fill="orange", alpha=0.2) +
  ggtitle("Sampling proportion = 0.6")

AC = readRDS("E:/Projects/BCC_plots/output/LungCancer_Sampler=Albert_Chib,prop_train=0.7.rds")
HH = readRDS("E:/Projects/BCC_plots/output/LungCancer_Sampler=Holmes_Held,prop_train=0.7.rds")
AC_HH =data.frame (c( AC[ ,1], AC[,2], HH[,1], HH[,2]),
                   c( rep("AC", nrep), rep("AC+", nrep), rep("HH", nrep), rep("HH+", nrep)))
colnames(AC_HH) <- c("error", "method")
p3 <- ggplot(AC_HH, aes(x=method, y=error)) +
  geom_boxplot(color="red", fill="orange", alpha=0.2) +
  ggtitle("Sampling proportion = 0.7")

ggarrange(p1, p2, p3, ncol = 3, nrow = 1)





# Prostate cancer data ----------------------------------------------------
Data <- read_excel("E:/Projects/BCC_plots/Singh-2002_ProstateCancer.xlsx")
X = as.matrix(t(Data[-1, -1])); dim(X)
X = matrix(as.numeric(X), ncol = ncol(X))
y = as.numeric(as.matrix(Data[1, -1]=="N")[1, ]); length(y); table (y)

# Plots 

AC = readRDS("E:/Projects/BCC_plots/output/ProstateCancer_Sampler=Albert_Chib,prop_train=0.5.rds")
HH = readRDS("E:/Projects/BCC_plots/output/ProstateCancer_Sampler=Holmes_Held,prop_train=0.5.rds")
AC_HH =data.frame (c( AC[ ,1], AC[,2], HH[,1], HH[,2]),
                   c( rep("AC", nrep), rep("AC+", nrep), rep("HH", nrep), rep("HH+", nrep)))
colnames(AC_HH) <- c("error", "method")
p1 <- ggplot(AC_HH, aes(x=method, y=error)) +
  geom_boxplot(color="red", fill="orange", alpha=0.2) +
  ggtitle("Sampling proportion = 0.5")

AC = readRDS("E:/Projects/BCC_plots/output/ProstateCancer_Sampler=Albert_Chib,prop_train=0.6.rds")
HH = readRDS("E:/Projects/BCC_plots/output/ProstateCancer_Sampler=Holmes_Held,prop_train=0.6.rds")
AC_HH =data.frame (c( AC[ ,1], AC[,2], HH[,1], HH[,2]),
                   c( rep("AC", nrep), rep("AC+", nrep), rep("HH", nrep), rep("HH+", nrep)))
colnames(AC_HH) <- c("error", "method")
p2 <- ggplot(AC_HH, aes(x=method, y=error)) +
  geom_boxplot(color="red", fill="orange", alpha=0.2) +
  ggtitle("Sampling proportion = 0.6")

AC = readRDS("E:/Projects/BCC_plots/output/ProstateCancer_Sampler=Albert_Chib,prop_train=0.7.rds")
HH = readRDS("E:/Projects/BCC_plots/output/ProstateCancer_Sampler=Holmes_Held,prop_train=0.7.rds")
AC_HH =data.frame (c( AC[ ,1], AC[,2], HH[,1], HH[,2]),
                   c( rep("AC", nrep), rep("AC+", nrep), rep("HH", nrep), rep("HH+", nrep)))
colnames(AC_HH) <- c("error", "method")
p3 <- ggplot(AC_HH, aes(x=method, y=error)) +
  geom_boxplot(color="red", fill="orange", alpha=0.2) +
  ggtitle("Sampling proportion = 0.7")

ggarrange(p1, p2, p3, ncol = 3, nrow = 1)




