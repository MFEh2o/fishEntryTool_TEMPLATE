# Supporting functions for the fish entry tool
# Created by Kaija Gahm on 21 May 2021

# Packages ----------------------------------------------------------------
library(tidyverse)

# getHeaderInfo -----------------------------------------------------------
getHeader <- function(d = cur){
  header <- d[1:17, 1:2] %>%
    setNames(., c("key", "value")) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    as.list()
  
  header$projectID <- as.numeric(header$projectID)
  header$dateTimeSet <- paste0(header$dateTimeSet, ":00")
  header$dateTimeSample <- paste0(header$dateTimeSample, ":00")
  
  
  # Return the header list
  return(header)
}
