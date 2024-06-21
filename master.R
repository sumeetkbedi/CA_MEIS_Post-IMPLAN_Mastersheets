## MASTER CODE FOR WRANGLING IMPLAN RESULTS TOGETHER ##

## Clear Environment ##
rm(list = ls(all.names = TRUE))

## Set Working Directory ##


## Load Libraries ##
library(openxlsx)
library(readxl) 
library(tidyverse)
library(censusapi)

## Load Parameters ##
source("parameters.R")

# Load function scripts ##
source("src/result_loop.R")
source("src/gsub_loop.R")

## Combine all IMPLAN results for county and district into their respective files based on data type (4 for county, 1 for district) ##
source("src/combine_county_results.R")
source("src/combine_district_results.R")

## Combine input and output data for counties and districts into their respective mastersheets
source("src/create_county_mastersheet.R")
source("src/create_district_mastersheet.R")