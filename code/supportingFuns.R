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
  names(header) <- str_replace(names(header), "UseCPUE", "useCPUE")
  
  # Apply some formatting and coercion
  header$projectID <- as.numeric(header$projectID)
  
  header$dateTimeSet <- paste0(header$dateTimeSet, ":00")
  header$dateTimeSample <- paste0(header$dateTimeSample, ":00")
  header$dateSet <- lubridate::date(lubridate::mdy_hms(header$dateTimeSet))
  header$dateSample <- lubridate::date(lubridate::mdy_hms(header$dateTimeSample))
  
  header$crew <- str_replace_all(header$crew, ",", ", ") %>%
    str_replace_all(., "\\s\\s", " ")
  # XXX should enforce a constraint on gear here
  header$distanceShocked <- ifelse(header$gear == "BE", header$distanceShocked, NA)
  header$effort <- as.numeric(header$effort)
  # XXX should also enforce constraints on effort units
  
  # Return the header list
  return(header)
}

# getCurData --------------------------------------------------------------
getCurData <- function(d = cur){
  curData <- d[19:nrow(d),] %>% 
    setNames(., d[18,]) %>%
    mutate(across(c("fishNum", "fishLength", "fishWeight"), as.numeric))
  
  curData <- curData[-which(is.na(curData$fishLength) & is.na(curData$otu)),]
  
  # Change name from 'species' to 'otu' if needed
  if("species" %in% names(curData)){
    curData <- curData %>%
      rename("otu" = species)
  }
  
  return(curData)
}

# convertSpeciesAbbreviations ---------------------------------------------
convertSpeciesAbbreviations <- function(x, fn = fishNames){
  assertDataFrame(x)
  assertChoice("otu", names(x))
  
  # It's confusing that x$otu contains abbreviations. Let's rename it.
  x <- x %>%
    rename("abbreviation" = "otu")
  
  # Get all abbreviations from fishNames
  abbrsDB <- fn$abbreviation
  
  # Check whether any abbreviations from the data sheet don't show up in OTU (fishNames)
  if(any(!x$abbreviation %in% abbrsDB)){
    if(force_species == FALSE){
      stop(paste0("Some species abbreviations (",
                  paste(unique(x$abbreviation[!x$abbreviation %in% abbrsDB]), collapse = ", "),
                  ") are not in the OTU table. If you are certain this is the correct abbreviation, use the force_species argument to add it to this season's working database."))
    } # XXX need to provide an option to enter names instead of abbreviations, or maybe a layered check, where first it checks abbrs and then it checks names if the abbrs fail.
  }
  
  # Join the species names from fn (fishNames) and replace the abbreviations with those names.
  x <- x %>%
    # XXX DEFINITELY need to write a check here to make sure the number of rows doesn't change. Should only be one row per abbreviation in OTU
    left_join(fn %>% 
                select(commonName, abbreviation),
              by = "abbreviation") %>%
    rename("otu" = "commonName") %>%
    mutate(otu = case_when(is.na(otu) & abbreviation == "RHS" ~ "redhorse",
                           is.na(otu) & abbreviation == "unidentifiable" ~
                             "fish_unidentifiable",
                           is.na(otu) & abbreviation == "PKL" ~ "grass_pickerel",
                           is.na(otu) & abbreviation == "BFN" ~ "bowfin",
                           TRUE ~ otu)) %>%
    select(-abbreviation)
  
  return(x)
}

# tochar ------------------------------------------------------------------
# shortcut for converting all columns in a data frame to character
tochar <- function(df){
  assertDataFrame(df)
  
  df2 <- df %>% 
    mutate(across(everything(), 
                  as.character))
  return(df2)
}
