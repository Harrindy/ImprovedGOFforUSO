############################################################################################
############################################################################################
# This R program can be used to repeat the simulation studies in the manuscript 
#       "A more powerful goodness-of-fit test for uniform stochastic ordering."                                                                          
############################################################################################
############################################################################################

############################################################################################
# Please install the TestUSO package:
# library(devtools)
# install_github("Harrindy/TestUSO",force=TRUE) 
############################################################################################

library(TestUSO)
source("https://raw.githubusercontent.com/harrindy/ImprovedGOFforUSO/master/Curves.R")

# In the simulation section (Section 4),
# we considered 9 fixed ODC: R0, R1, ..., R8,
# and four sequences of ODCs indexed by 
#           delta_a, delta_b, delta_c, delta_d.
# After sourcing the Curves.R file using the above command,
# two main functions will be defined:
#   1. R(u, mod) calcualtes the function value of a considered ODC at u;
#   2. Data.Generate(m,n,mod) generates 
#                               m observations from F=picked ODC and 
#                               n observations from Uniform(0,1),
#                           so that the resulting ODC is the picked ODC.
#   Options for mod include
#     "R0", "R1", ..., "R8" (corresponding to the 9 fixed ODCs in Section 4.1)
#     "delta_a_0",..., "delta_a_9" (the sequence of ODCs indexed by delta_a in Section 4.2)
#     "delta_b_0",..., "delta_b_9" (the sequence of ODCs indexed by delta_b in Section 4.2)
#     "delta_c_0",..., "delta_c_9" (the sequence of ODCs indexed by delta_c in Section 4.2)
#     "delta_d_0",..., "delta_d_9" (the sequence of ODCs indexed by delta_d in Section 4.2)

# For example, if considering the ODC R4 in Section 4.1
# This ODC can be plotted by
u=seq(0,1,length=1000)
plot(u,R(u,mod="R4"),type="l")

# To generate m=100, n=100 independent samples for R5
data=Data.Generate(100,100,mod="R4")
X=data$X
Y=data$Y

# Based on the generated X and Y,
# the sample ODC and its least star-shaped majorant can be plotted by
library(TestUSO)
LSM(X,Y,graph=TRUE)
# The tests can be performed by
GoF4USO(X,Y,alpha=0.05,graph=TRUE)


# You can repeat these for any ODC that was considered in Section 4. All you need to change is the value of mod.
# For example, if you want the member delta_c=8 in the third ODC sequence in Section 4.2, simply set mod="delta_c_8":
u=seq(0,1,length=1000)
plot(u,R(u,mod="delta_c_8"),type="l")
data=Data.Generate(100,100,mod="delta_c_8")
X=data$X
Y=data$Y
library(TestUSO)
LSM(X,Y,graph=TRUE)
GoF4USO(X,Y,alpha=0.05,graph=TRUE)
