#!/usr/bin/env Rscript

# This is mainly what package entropies needs to run. 
library(devtools)
install.packages(c("tidyverse", "ggtern", "caret", "mlbench","candisc", "vcd"), 
                 dependencies = TRUE)
# test, just ignore this line: remove.packages(c("caret"))
devtools::install_github("FJValverde/entropies", dependencies=TRUE)#Dependencies is passed onto devtools::install

# If you have a problem with ggtern and ggplot you might have to roll ggplot2 back to the ggtern
# version e.g. lookup the installed version of ggtern and then issue
#install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org")

#Next just load.
library(entropies)
vignette(package="entropies")#Check out which vignettes are there in the package. 
