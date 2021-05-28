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
  header$dateSet = strftime(strptime(header$dateTimeSet, format = "%m/%d/%Y %H:%M:%S"), format = "%Y-%m-%d")
  header$dateSample = strftime(strptime(header$dateTimeSample, format = "%m/%d/%Y %H:%M:%S"), format = "%Y-%m-%d")
  
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

  return(curData)
}

# convertSpeciesAbbreviations ---------------------------------------------
convertSpeciesAbbreviations <- function(x, fn = fishNames){
  assertDataFrame(x)
  assertChoice("species", names(x))
  
  # It's confusing that x$species contains abbreviations. Let's rename it.
  x <- x %>%
    rename("abbreviation" = "species")
  
  # Get all abbreviations from fishNames
  abbrs <- fn$abbreviation
  
  # Check whether any abbreviations from the data sheet don't show up in OTU (fishNames)
  if(any(!x$abbreviation %in% abbrs)){
    if(force_species == FALSE){
      stop(paste0("Some species (",
                 paste(unique(x$abbreviation[!x$abbreviation %in% abbrs]), collapse = ", "),
                 ") are not in the MFE database nor in this season's working database. If you are certain this is the correct species name, use the force_species argument to add it to this season's working database."))
    } # XXX this doesn't include the working database because it hasn't been bound on yet. Come back to this--did I misinterpret?
  }
  
  # Join the species names from fn (fishNames) and replace the abbreviations with those names.
  x <- x %>%
    # XXX DEFINITELY need to write a check here to make sure the number of rows doesn't change. Should only be one row per abbreviation in OTU
    left_join(fn %>% 
                select(commonName, abbreviation),
              by = "abbreviation") %>%
    rename("species" = "commonName") %>%
    mutate(species = case_when(is.na(species) & abbreviation == "RHS" ~ "redhorse",
                               is.na(species) & abbreviation == "unidentifiable" ~ "fish_unidentifiable",
                               is.na(species) & abbreviation == "PKL" ~ "grass_pickerel",
                               is.na(species) & abbreviation == "BFN" ~ "bowfin",
                               TRUE ~ species)) %>%
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
