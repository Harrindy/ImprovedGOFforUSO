# Improved goodness-of-fit test for uniform stochastic ordering
This repository contains R programs for the article, “A more powerful goodness-of-fit test for uniform stochastic ordering,” by Dewei Wang, Chuan-Fa Tang, and Joshua Tebbs. This article has been submitted for publication.

Prior to using R programs on this repository, please install R package "TestUSO" (https://github.com/Harrindy/TestUSO) using the following commands:

    library(devtools)
    install_github("Harrindy/TestUSO",force=TRUE)

To repeat our simulation results: 
    
    1. Open Simulation.R in R (Rstudio). 
    3. Follow the comments to change parameters to the simulation setting that you want to repeat. 
    4. Make sure that "TestUSO" has been installed and R (or Rstudio) can access the R file Curves.R.
    5. Run Simulation.R. 


The premature infant data is provided in PrematureInfant.CSV. Due to a confidentiality issue, we cannot provide the original data. However, because the ODC and both methods considered in the paper are invariant to an increasing transformation on the two samples, we divided each observation by the maximum of the two samples and saved the transformed data in PrematureInfant.csv. 

To reproduce the analysis in the premature infant data example: 

    1. Run PrematureInfant.R. 
