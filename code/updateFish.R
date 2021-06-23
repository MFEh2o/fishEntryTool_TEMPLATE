# updateFish.R - wrapper for fishEntry.R
# 4-18-2019, SEJ
# Updated June 2021 by KG

# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)

# Show long errors and warnings
options(warning.length = 10000L, error.length = 10000L)

# Set file paths ----------------------------------------------------------
# File paths are relative paths, written with the here` package
dbdir <- here()
db <- "MFEdb_20210528.db" # name of the database file you're using. Try to use one with a specific date to make the workflow clear. For example, "MFEdb_20200530.db"
funcdir <- here("code") # directory where the code/functions is stored
isdir <- here("inSeason") # directory where the in-season db files will be stored
ssdir <- here("sampleSheets")

# Run the code below to update fish data with new datasheets that have been entered into the template and saved as .csv files

# Load the entry function
source(here("code", "fishEntry.R"))

# Run code to update in-season files
## Note about headerRows: 18 is the default number of headerRows present in the normal fish data sheets (i.e. non-minnow trap sheets). The tool takes the headerRows number and subtracts 1 to account for the slightly shorter minnow trap header.
updateFish(headerRows = 18, 
           # the following arguments are defined above:
           dbdir = dbdir, 
           db = db, 
           funcdir = funcdir, 
           isdir = isdir, 
           ssdir = ssdir)
# add any force_* arguments in the console

