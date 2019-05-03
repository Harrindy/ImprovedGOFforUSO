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
