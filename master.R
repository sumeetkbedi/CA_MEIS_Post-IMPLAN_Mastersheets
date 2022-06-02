##MASTER CODE FOR WRANGLING DATA##

## Clear Environment##
rm(list = ls(all.names = TRUE))

##Set Working Directory##


##Load Libraries##
library(dplyr)
library(httr)
library(jsonlite)
library(openxlsx)
library(readxl) 
library(tidyverse) 

##Load Parameters##
source("parameters.R")

##STEP 1: Combine all the IMPLAN results for county and district into 2 respective files##
source("src/combine_county_results.R")
source("src/combine_district_results.R")