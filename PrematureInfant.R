############################################################################################
############################################################################################
# This R program can be used to reproduce our real data analysis of the manuscript 
#       "A more powerful goodness-of-fit test for uniform stochastic ordering."                                                                          
############################################################################################
############################################################################################
library(TestUSO)
#############################################
# Read the premature infant data.
rawdata = read.csv("PrematureInfant.csv")
X=unlist(rawdata$No_Caffeine)
Y=unlist(rawdata$Caffeine)
Y=Y[is.na(Y)==FALSE]

#############################################
# Reproduce Figure 2 of the manuscript.
# It plots the empirical ODC and its least star-shaped majorant.
LSM(X,Y,graph=TRUE)

#############################################
# Conduct the fixed-critical-value approach.
TestUSO_fixed(X,Y,alpha=0.05)
# [1] "1: reject USO; 0: do not rejct USO"
#         Test_statistic Fixed_cv Reject_USO
# p=1          0.1696006    0.580          0
# p=2          0.2625633    0.676          0
# p=infty      0.9488259    1.353          0

#############################################
# Conduct the sample-based-critical-value approach.
set.seed(100)
TestUSO_samplebased(X,Y,alpha=0.05)

# [1] "1: reject USO; 0: do not rejct USO"
#         Test_statistic Sample_cv Reject_USO
# p=1          0.1696006 0.3301349          0
# p=2          0.2625633 0.4761526          0
# p=infty      0.9488259 1.1835879          0