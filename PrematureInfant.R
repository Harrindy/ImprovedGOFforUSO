############################################################################################
############################################################################################
# This R program can be used to reproduce the real data analysis in the paper 
#       "A more powerful goodness-of-fit test for uniform stochastic ordering."                                                                          
############################################################################################
############################################################################################
library(TestUSO)
#############################################
# Read the premature infant data.
rawdata = read.csv("https://raw.githubusercontent.com/harrindy/ImprovedGOFforUSO/master/PrematureInfant.csv")
X=unlist(rawdata$No_Caffeine)
Y=unlist(rawdata$Caffeine)
Y=Y[is.na(Y)==FALSE]

#############################################
# Reproduce Figure 2 of the manuscript.
# It plots the empirical ODC and its least star-shaped majorant.
LSM(X,Y,graph=TRUE)

![Optional Text](../master/InfantRmn.png)

#############################################
# Reproduce Figure 3(b) of the manuscript
# Conduct the test via four methods.
set.seed(100)
GoF4USO(X,Y,alpha=0.05,graph=TRUE)

![Optional Text](../master/InfantNew.png)

# [1] "1: reject USO; 0: do not rejct USO"
# [1] "Fixed_cv: critical values based on the limiting distribution at the least favorable configuration"
# [1] "Reject_USO_fix: reject or not based on the limiting distribution at the least favorable configuration"
# [1] "lf_cv: critical values based on the finite distribution at the least favorable configuration"
# [1] "Reject_USO_lf: reject or not based on the finite distribution at the least favorable configuration"
# [1] "iso_cv: critical values based on the isotonic fitting"
# [1] "Reject_USO_iso: reject or not based on the isotonic fitting"
# [1] "bs_cv: critical values based on the resample method"
# [1] "Reject_USO_bs: reject or not based on the resample method"
# 
#            Test_statistic Fixed_cv Reject_USO_fix     lf_cv Reject_USO_lf    iso_cv Reject_USO_iso     bs_cv Reject_USO_bs
# p=1             0.1696006    0.580              0 0.6022493             0 0.2505163              0 0.3822582             0
# p=2             0.2625633    0.676              0 0.6869073             0 0.3801602              0 0.5171492             0
# p=infinity      0.9488259    1.353              0 1.3300630             0 1.0709166              0 1.1902433             0
