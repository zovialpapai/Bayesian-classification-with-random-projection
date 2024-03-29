Computer code to reproduce the numerical results presented in "Efficient Bayesian high-dimensional classification via random projection with application to gene expression data" (Chakraborty A., 2023+). Here, we present a short decription about the directories in the repository. 

functions:
The directory contains utility functions in two R scripts, that are utilsed in the repeated simulations and real data analysis conducted in the paper. 
(a) BCC_Functions.R contains functions for compression matrix generation; Probit regression via Albert & Chib and Holmes & Held data augmentation schemes; Logit regression via Polya-Gamma data augmentation scheme; hyper-parameter tuning; and associated helper functions.
(b) Probit_HH_cpp.R contains Probit regression via Holmes & Held data augmentation scheme, written in Rcpp.

repeated simulations:
The directory contains three R scripts, named BCC_sims.R, Weakleaners.R, and time_comparison.R. 

(a) BCC_sims.R can be utilsed to carry out the simulations presented in Section 3 on High-dimensional Probit regression, and Section 5 on High-dimensional Logit regression, along with hyper-parameter tuning. BCC_probitsims_reproducible.R computes quantities presented in Tables 1,2,3,4,5. BCC_logitsims_reproducible.R computes quantities presented in Table 6.

(b) Weakleaners.R can be utilized to study the effect of number of replications of compression matrix (or number of weak classifiers)  on the accuracy of classifiers AC, AC+, HH, HH+. The results are presented in Section 3.  Plot_ChoiceR_TimeComparison.R takes the output rds files from  Weakleaners.R, collected in repeated simulations/output folder; and reproduces Figures 2 and 4 in the article.

(c) time_comparison.R can be utilised to study comparative computional time of our classifiers. The results are presented in Section 3. Plot_ChoiceR_TimeComparison.R takes the output rds files from  time_comparison.R, collected in repeated simulations/output folder; and reproduces Figures 1 and 3 in the article.

data: 
Micro-array gene expression cancer data sets utilized in the article is freely available on the website https://data.mendeley.com/. Copies of the data sets are available in the "data" directory in the our repository.

real data analysis: 
The directory contains the a R script named BCC_data.R that can be utilised to carry out the analysis of micro-array gene expression cancer data sets (Leukomia, Lung Cancer, Prostate cancer), presented in Section 4 of the paper. Plot_RealData.R takes the output rds files from BCC_data.R, collected in real data analysis/output folder; and reproduces Figures 5, 6 and 7 in the article.

Please reach out to abhisek_chakraborty@tamu.edu for any queries.



