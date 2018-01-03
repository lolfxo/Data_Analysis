# Script to create the "hypothesistests" package

# Clear current list
# rm(list=ls())

# Get working directory
getwd()

# Create your own R package

# Step 0: Packages I will need
install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)

# Step 1: Create your package directory
setwd("C:/Users/hongn/Documents/GitHub/Data_Analysis")

# Create folder in which to include functions. This is also the package name.
# create("hypothesistests")

setwd("./hypothesistests")

# create the document
document()

# install 'hypothesistests' package straight from GitHub
install_github("lolfxo/Data_Analysis",subdir="hypothesistests")