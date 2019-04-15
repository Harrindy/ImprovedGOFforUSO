############################################################################################
############################################################################################
# This R program can be used to reproduce the real data analysis in the paper 
#       "More powerful goodness-of-fit tests for uniform stochastic ordering."                                                                          
############################################################################################

############################################################################################
# Please install the TestUSO package:
# library(devtools)
# install_github("Harrindy/TestUSO",force=TRUE) 
############################################################################################
library(TestUSO)
#############################################
# Read the premature infant data.
rawdata = read.csv("https://raw.githubusercontent.com/harrindy/ImprovedGOFforUSO/master/PrematureInfant.csv")
X=unlist(rawdata$No_Caffeine)
Y=unlist(rawdata$Caffeine)
Y=Y[is.na(Y)==FALSE]

#############################################
# Plot the empirical ODC and its least star-shaped majorant.
LSM(X,Y,graph=TRUE)
#############################################
# Conduct the test via four methods.
set.seed(100)
GoF4USO(X,Y,alpha=0.05,graph=TRUE)
# [1] "Fixed_cv: critical values using Tang et al. (2017)"
# [1] "Reject_USO_fix: reject (1) or not (0) using Tang et al. (2017)"
# [1] "lf_cv: critical values using the finie-sample version of Tang et al. (2017)"
# [1] "Reject_USO_lf: reject (1) or not (0) using the finie-sample version of Tang et al. (2017)"
# [1] "iso_cv: critical values using method one of Wang et al. (2019)"
# [1] "Reject_USO_iso: reject (1) or not (0) using method one of Wang et al. (2019)"
# [1] "bs_cv: critical values using method two of Wang et al. (2019)"
# [1] "Reject_USO_bs: reject (1) or not (0) using method two of Wang et al. (2019)"
# 
#            Test_statistic Fixed_cv Reject_USO_fix     lf_cv Reject_USO_lf    iso_cv Reject_USO_iso     bs_cv Reject_USO_bs
# p=1             0.1696006    0.580              0 0.6022493             0 0.2505163              0 0.3822582             0
# p=2             0.2625633    0.676              0 0.6869073             0 0.3801602              0 0.5171492             0
# p=infinity      0.9488259    1.353              0 1.3300630             0 1.0709166              0 1.1902433             0
