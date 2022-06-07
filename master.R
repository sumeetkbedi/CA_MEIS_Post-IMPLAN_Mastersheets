## MASTER CODE FOR WRANGLING IMPLAN RESULTS TOGETHER ##

## Clear Environment ##
rm(list = ls(all.names = TRUE))

## Set Working Directory ##


## Load Libraries ##
library(openxlsx)
library(readxl) 
library(tidyverse) 

## Load Parameters ##
source("parameters.R")

# Load function scripts ##
source("src/result_loop.R")


## STEP 1: Combine all the IMPLAN results for county and district into 2 respective files ##
source("src/combine_county_results.R")
source("src/combine_district_results.R")