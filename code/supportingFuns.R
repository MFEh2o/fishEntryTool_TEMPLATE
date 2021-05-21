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
  
  # Fix the "dataTimeSet" typo if it occurs
  names(header) <- str_replace(names(header), "dataTimeSet", "dateTimeSet")
  
  # Apply some formatting and coercion
  header$projectID <- as.numeric(header$projectID)
  header$dateTimeSet <- paste0(header$dateTimeSet, ":00")
  header$dateTimeSample <- paste0(header$dateTimeSample, ":00")
  header$crew <- str_replace_all(header$crew, ",", ", ") %>%
    str_replace_all(., "\\s\\s", " ")
  # XXX should enforce a constraint on gear here
  header$distanceShocked <- ifelse(header$gear == "BE", header$distanceShocked, NA)
  header$effort <- as.numeric(header$effort)
  # XXX should also enforce constraints on effort units

  # Return the header list
  return(header)
}
