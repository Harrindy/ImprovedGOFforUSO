# A more powerful goodness-of-fit test for uniform stochastic ordering
This repository contains R programs for the article, “A more powerful goodness-of-fit test for uniform stochastic ordering,” by Dewei Wang and others. This article has been submitted for publication.

Prior to using R programs on this repository, please install R package "TestUSO" (https://github.com/Harrindy/TestUSO) using the following commands:

    library(devtools)
    install_github("Harrindy/TestUSO")

To reproduce our simulation results: Download the files Curves.R and Simulation.R to your computer. Open Simulation.R in R (Rstudio). Follow the instructions to change the parameters according to the simulation setting that you want to reproduce. Then run Simulation.R. Make sure that R package "TestUSO" has been installed and R (or Rstudio) can access the R file Curves.R.

The premature infant data is provided in PrematureInfant.CSV. Due to a confidentiality issue, we cannot provide the original data. However, because the ODC and both methods considered in the paper are invariant to an increasing transformation on the two samples, we divided each observation by the maximum of the two samples and saved the transformed data in PrematureInfant.csv. 

To reproduce the analysis in the premature infant data example: Download the data file (PrematureInfant.csv) and the R file (PrematureInfant.R).  Open R (or Rstudio) and run PrematureInfant.R. Make sure that R (or Rstudio) can access the data file PrematureInfant.csv.
