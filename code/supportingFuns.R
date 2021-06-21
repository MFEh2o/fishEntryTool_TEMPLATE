# Supporting functions for the fish entry tool
# Created by Kaija Gahm on 21 May 2021

# Packages ----------------------------------------------------------------
library(tidyverse)

# List of retired projectIDs ----------------------------------------------
retiredProjectIDs <- c(1:2, 4:5, 7:12, 14, 16, 18:25, 27:33, 39:40)

# tochar ------------------------------------------------------------------
# shortcut for converting all columns in a data frame to character
tochar <- function(df){
  assertDataFrame(df)
  
  suppressWarnings(df2 <- df %>% mutate(across(everything(), as.character)))
  return(df2)
}

# getHeader ---------------------------------------------------------------
getHeader <- function(d, hr){
  # Check inputs
  assertDataFrame(d)
  assertNumeric(hr, len = 1)
  
  # Pull header
  header <- d[1:(hr-1), 1:2] %>%
    setNames(., c("key", "value")) %>%
    mutate(key = str_remove_all(key, "\\s"))
  
  # Check whether any of the header elements are not named
  if(any(header$key == "")){
    w <- which(header$key == "")
    stop(paste0("The following elements of your header don't have names: ",
                paste(w, collapse = ", "),
                ". Make sure each header element has a name, e.g. 'projectID', 'dateTimeSample', etc."))
  }
  
  # assuming the header has all its names, proceed
  header <- header %>%
    pivot_wider(names_from = key, values_from = value) %>%
    as.list()
  
  # Fix the "dataTimeSet" typo if it occurs
  names(header) <- str_replace(names(header), "dataTimeSet", "dateTimeSet")
  names(header) <- str_replace(names(header), "UseCPUE", "useCPUE")
  
  # Check date formats (this will allow the date to parse correctly in the next step)
  pat <- "^[0-9]{1,2}\\/[0-9]{1,2}\\/[0-9]{2,4}\\s[0-9]{1,2}:[0-9]{2}$"
  assertCharacter(header$dateTimeSet, pattern = pat)
  assertCharacter(header$dateTimeSample, pattern = pat)
  
  # Apply some formatting and coercion
  header$projectID <- as.numeric(header$projectID)
  header$dateTimeSet <- format(lubridate::mdy_hm(header$dateTimeSet), 
                               "%Y-%m-%d %H:%M:%S")
  header$dateTimeSample <- format(lubridate::mdy_hm(header$dateTimeSample),
                                  "%Y-%m-%d %H:%M:%S")
  header$effort <- as.numeric(header$effort)
  # add dateSet and dateSample
  header$dateSet <- lubridate::date(header$dateTimeSet)
  header$dateSample <- lubridate::date(header$dateTimeSample)
  # separate crew members with commas and spaces
  header$crew <- str_replace_all(header$crew, ",", ", ") %>%
    str_replace_all(., "\\s\\s", " ")
  # remove spaces from gear and metadataID
  header$gear <- str_remove_all(header$gear, "\\s")
  header$metadataID <- str_remove_all(header$metadataID, "\\s")
  # set distanceShocked to NA if gear is not BE
  header$distanceShocked <- ifelse(header$gear == "BE", header$distanceShocked, NA)

  # Fix some common siteName capitalization errors
  if("siteName" %in% names(header)){
    if(header$siteName %in% c("wholeShoreline", "wholeshoreline", "wholeShore", 
                              "wholeshore", "WholeShore")){
      message(paste0("Correcting siteName '", header$siteName, "' to 'WholeShoreline'."))
      header$siteName <- "WholeShoreline"
    }
    if(header$siteName %in% c("Deephole", "deephole", "DH", "deepHole")){
      message(paste0("Correcting siteName '", header$siteName, "' to 'DeepHole'."))
      header$siteName <- "DeepHole"
    }
  }
  
  # Fix common gear problem: adding fyke net number to gear, when it should just be FN
  if(grepl("FN[[:alnum:]]+", header$gear)){
    message(paste0("Correcting gear '", header$gear, "' to 'FN'."))
    header$gear <- "FN"
  }
  
  # Return the header list
  return(header)
}

# getCurData --------------------------------------------------------------
getCurData <- function(d, hr){
  # Check inputs
  assertDataFrame(d)
  assertNumeric(hr, len = 1)
  assertSubset(c("fishNum", "fishLength", "fishWeight"), 
               choices = as.character(slice(d, hr)))
  
  # Pull the data portion of the sample sheet
  curData <- d[(hr + 1):nrow(d),] %>% 
    setNames(., d[hr,]) %>%
    mutate(across(c("fishNum", "fishLength", "fishWeight"), as.numeric))
  
  curData <- curData %>%
    filter(!(is.na(fishLength) & is.na(species)))
  
  # Change name from 'species' to 'otu' if needed
  if("species" %in% names(curData)){
    curData <- curData %>% rename("otu" = species)
  }
  
  # Change name from 'clipRecap' to 'clipRecapture' if needed
  if("clipRecap" %in% names(curData)){
    curData <- curData %>% rename("clipRecapture" = clipRecap)
  }
  
  # Change "sampled" to "sample" for spine and scale. For some reason, otolith and diet stay as "sampled"
  if("spineSampled" %in% names(curData)){
    curData <- curData %>% rename("spineSample" = spineSampled)
  }
  
  if("scaleSampled" %in% names(curData)){
    curData <- curData %>% rename("scaleSample" = scaleSampled)
  }
  
  return(curData)
}

# convertSpeciesAbbreviations ---------------------------------------------
convertSpeciesAbbreviations <- function(x, fn = fishNames, f){
  assertDataFrame(x, col.names = "unique")
  assertDataFrame(fn, col.names = "unique")
  assertChoice("otu", names(x))
  assertChoice("abbreviation", names(fn))
  assertFlag(f)
  
  # It's confusing that x$otu contains abbreviations. Let's rename it.
  x <- x %>%
    rename("abbreviation" = "otu") %>%
    mutate(abbreviation = case_when(abbreviation == "minnow" ~ "MNW",
                                    TRUE ~ abbreviation)) %>%
    filter(abbreviation != "NFC" | is.na(abbreviation)) # remove any NFC observations
  
  # Get all abbreviations from fishNames
  abbrsDB <- fn$abbreviation
  
  # Check whether any are NA, and throw an error
  NAabbr <- x %>% filter(is.na(abbreviation))
  if(nrow(NAabbr) > 0){
    stop(paste0("The following fish have no species abbreviation recorded:\n\n",
                paste(capture.output(NAabbr), collapse = "\n"),
                "\n\nPlease fill in a species abbreviation for these rows. If the fish could not be identified, use 'fish_unidentifiable'."))
  }
  
  
  # Check whether any abbreviations from the data sheet don't show up in OTU (fishNames)
  if(any(!x$abbreviation %in% abbrsDB)){
    if(f == FALSE){
      stop(paste0("Some species abbreviations (",
                  paste(unique(x$abbreviation[!x$abbreviation %in% abbrsDB]), 
                        collapse = ", "),
                  ") are not in the OTU table. If you are certain this is the correct abbreviation, use the force_species argument to add it to this season's working database."))
    } # XXX need to provide an option to enter names instead of abbreviations, or maybe a layered check, where first it checks abbrs and then it checks names if the abbrs fail.
  }
  
  # Join the species names from fn (fishNames) and replace the abbreviations with those names.
  i <- nrow(x)
  x <- x %>%
    left_join(fn %>% 
                select(otu, abbreviation) %>%
                # juuuuust in case there are multiple entries for one abbreviation, take the first one. This should not happen, but we need to be careful.
                group_by(abbreviation) %>%
                slice(1),
              by = "abbreviation") %>%
    # If there's no match (because you've already forced a new species name), just fill in otu with the value in `abbreviation.`
    mutate(otu = case_when(is.na(otu) ~ abbreviation,
                           TRUE ~ otu)) %>%
    select(-abbreviation)
  j <- nrow(x)
  
  # Throw an error if the number of rows changes
  if(i != j){
    stop("Something went wrong with joining species names/abbreviations!")
  }
  
  return(x)
}

# makeFishInfoNEW ---------------------------------------------------------
makeFishInfoNEW <- function(d = curData, h = header, dss = dateSampleString, 
                            tss = timeSampleString, f = file, m = isMinnow){
  # Check inputs
  assertDataFrame(d, col.names = "unique")
  assertList(h)
  assertSubset(c("projectID", "metadataID", "lakeID", "gear"), 
               choices = names(h))
  if(!m){
    assertChoice("siteName", choices = names(h))
    assertSubset(c("pitApply", "pitRecapture", "floyApply", "floyRecapture"),
                 choices = names(d))
  }
  assertChoice("fishNum", names(d))
  assertCharacter(dss, len = 1)
  assertCharacter(tss, len = 1)
  assertCharacter(f, len = 1)
  
  # If isMinnow, we expect the trapNumbers to be in one of a few formats
  if(m){
    d$trapNumber <- as.character(d$trapNumber)
    assertCharacter(d$trapNumber, 
                    pattern = "MT\\.[0-9]{0,3}|MT[0-9]{0,3}|[0-9]{0,3}")
  }
  
  # Make fishInfoNEW
  fishInfoNEW <- d %>%
    {if(m){
      filter(., otu != "NFC" | is.na(otu)) %>%
        rename("siteName" = trapNumber) %>%
        mutate(siteName = 
                 case_when(grepl("MT", siteName) & !grepl("\\.", siteName) ~
                             str_replace(siteName, "MT", "MT."),
                           !grepl("MT", siteName) ~ 
                             paste0("MT.", str_pad(siteName, 
                                                   side = "left", 
                                                   pad = "0", 
                                                   width = 3))
                 )
        )}else{
          mutate(., 
                 siteName = h$siteName,
                 across(c("pitApply", "pitRecapture", "floyApply", "floyRecapture"), 
                        as.character))
        }} %>%
    mutate(projectID = h$projectID,
           metadataID = h$metadataID,
           sampleID = paste(h$lakeID, siteName, dss,
                            tss, h$gear, metadataID, 
                            sep = "_"),
           fishNum = as.numeric(fishNum),
           fishID = paste(sampleID, fishNum, sep = "_"),
           entryFile = f)
  
  assertNumeric(fishInfoNEW$fishNum, any.missing = F)
  return(fishInfoNEW)
}

# makeFishSamplesNEW ------------------------------------------------------
makeFishSamplesNEW <- function(h = header, dss = dateSampleString, 
                               tss = timeSampleString, f = file, m = isMinnow,
                               dat = curData){
  # Check inputs
  assertList(h)
  assertCharacter(dss, len = 1)
  assertCharacter(tss, len = 1)
  assertCharacter(f, len = 1)
  assertSubset(c("lakeID", "gear", "metadataID", "projectID", "lakeID", "dataRecorder", "dataEnteredBy", "effortUnits", "crew", "useCPUE"), choices = names(h)) # XXX updateID?
  if(!m){
    assertChoice("siteName", choices = names(h))
  }
  h$dateSet <- as.character(h$dateSet)
  h$dateSample <- as.character(h$dateSample)
  h$dateTimeSet <- as.character(h$dateTimeSet)
  h$dateTimeSample <- as.character(h$dateTimeSample)
  assertCharacter(h$dateSet, len = 1, 
                  pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}")
  assertCharacter(h$dateSample, len = 1, 
                  pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}")
  assertCharacter(h$dateTimeSet, len = 1, 
                  pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}\\s[0-9]{2}:[0-9]{2}:[0-9]{2}")
  assertCharacter(h$dateTimeSample, len = 1, 
                  pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}\\s[0-9]{2}:[0-9]{2}:[0-9]{2}")
  assertFlag(m)
  assertDataFrame(dat)
  
  # If the data sheet is a minnow trap sheet, get the unique trapNumbers
  if(m){
    assertChoice("trapNumber", choices = names(dat))
    trapNumbers <- unique(dat$trapNumber)
    # insert a period between MT and the number
    assertCharacter(trapNumbers, pattern = "MT[0-9]{3}")
    trapNumbers <- str_replace(trapNumbers, "MT", "MT.")
  }
  
  # Make fishSamplesNEW
  fishSamplesNEW <- data.frame(key = names(h),
                               value = purrr::reduce(h, c)) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    {if(m){slice(., rep(1:n(), length(trapNumbers))) %>%
        mutate(siteName = trapNumbers)} else .} %>% # XXX have to introduce periods into the minnow trap names?
    mutate(siteID = paste(lakeID, siteName, sep = "_"),
           sampleID = paste(siteID, dss, tss,
                            gear, metadataID, sep = "_"),
           dayOfYear = as.numeric(strftime(strptime(dateSample,
                                                    format = "%Y-%m-%d"),
                                           format = "%j")),
           entryFile = f) %>%
    select(-siteName)
  
  # Add nAnglers column based on crew only if effortUnits == angler_hours. 
  # checkHeader function has already validated crew and effortUnits.
  fishSamplesNEW <- fishSamplesNEW %>%
    mutate(nAnglers = case_when(effortUnits == "angler_hours" ~ 
                                  as.character(
                                    length(
                                      unlist(strsplit(crew, split = ", "))
                                    )
                                  ),
                                TRUE ~ NA_character_)) %>%
    relocate(nAnglers, .after = "effortUnits")
  
  # Remove leading and trailing spaces from useCPUE
  fishSamplesNEW$useCPUE <- stringr::str_remove_all(fishSamplesNEW$useCPUE, "\\s$")
  fishSamplesNEW$useCPUE <- stringr::str_remove_all(fishSamplesNEW$useCPUE, "^\\s")
  
  return(fishSamplesNEW)
}

# makeFishOtolithsNEW -----------------------------------------------------
makeFishOtolithsNEW <- function(d = curData, h = header, 
                                dss = dateSampleString, 
                                tss = timeSampleString){
  # Check inputs
  assertDataFrame(d, col.names = "unique")
  assertList(h)
  assertCharacter(dss, len = 1)
  assertCharacter(tss, len = 1)
  assertSubset(c("otolithSample", "fishNum", "fishLength", "fishWeight"),
               choices = names(d))
  assertSubset(c("lakeID", "siteName", "gear", "metadataID"),
               choices = names(h))
  
  # Make fishOtolithsNEW
  fishOtolithsNEW <- d %>%
    filter(otolithSample == 1) %>%
    select(fishNum, fishLength, fishWeight) %>%
    mutate(fishID = paste(h$lakeID, h$siteName, dss,
                          tss, h$gear, h$metadataID,
                          fishNum, sep = "_"),
           otolithWeight = NA) %>%
    rename("lengthAtCapture" = fishLength,
           "weightAtCapture" = fishWeight)
  return(fishOtolithsNEW)
}

# makeFishSpinesNEW -----------------------------------------------------
makeFishSpinesNEW <- function(d = curData, h = header, 
                              dss = dateSampleString, 
                              tss = timeSampleString){
  # Check inputs
  assertDataFrame(d, col.names = "unique")
  assertList(h)
  assertCharacter(dss, len = 1)
  assertCharacter(tss, len = 1)
  assertSubset(c("spineSample", "fishNum", "fishLength", "fishWeight"),
               choices = names(d))
  assertSubset(c("lakeID", "siteName", "gear", "metadataID"),
               choices = names(h))
  
  # Make fishSpinesNEW
  fishSpinesNEW <- d %>%
    filter(spineSample == 1) %>%
    select(fishNum, fishLength, fishWeight) %>%
    mutate(fishID = paste(h$lakeID, h$siteName, dss,
                          tss, h$gear, h$metadataID,
                          fishNum, sep = "_")) %>%
    rename("lengthAtCapture" = fishLength,
           "weightAtCapture" = fishWeight)
  return(fishSpinesNEW)
}

# makeFishScalesNEW -------------------------------------------------------
makeFishScalesNEW <- function(d = curData, h = header, 
                              dss = dateSampleString, 
                              tss = timeSampleString){
  # Check inputs
  assertDataFrame(d, col.names = "unique")
  assertList(h)
  assertCharacter(dss, len = 1)
  assertCharacter(tss, len = 1)
  assertSubset(c("scaleSample", "fishNum", "fishLength", "fishWeight"),
               choices = names(d))
  assertSubset(c("lakeID", "siteName", "gear", "metadataID"),
               choices = names(h))
  
  # Make fishScalesNEW
  fishScalesNEW <- d %>%
    filter(scaleSample == 1) %>%
    select(fishNum, fishLength, fishWeight) %>%
    mutate(fishID = paste(h$lakeID, h$siteName, dss,
                          tss, h$gear, h$metadataID,
                          fishNum, sep = "_")) %>%
    rename("lengthAtCapture" = fishLength,
           "weightAtCapture" = fishWeight)
  return(fishScalesNEW)
}

# makeFishDietsNEW --------------------------------------------------------
makeFishDietsNEW <- function(d = curData, h = header, 
                             dss = dateSampleString, 
                             tss = timeSampleString){
  # Check inputs
  assertDataFrame(d, col.names = "unique")
  assertList(h)
  assertCharacter(dss, len = 1)
  assertCharacter(tss, len = 1)
  assertSubset(c("dietSampled", "fishNum", "otu"),
               choices = names(d))
  assertSubset(c("lakeID", "siteName", "gear", "metadataID", "dateSample"),
               choices = names(h))
  
  # Make fishDietsNEW
  fishDietsNEW <- d %>%
    filter(dietSampled == 1) %>%
    select(fishNum, otu) %>%
    mutate(fishID = paste(h$lakeID, h$siteName, dss,
                          tss, h$gear, h$metadataID,
                          fishNum, sep = "_"),
           lakeID = h$lakeID,
           dateSample = h$dateSample)
  return(fishDietsNEW)
}

# reverseRegex ------------------------------------------------------------
rr <- function(vec, unique = F){
  # Check that `vec` is a character vector that has at least some non-missing entries
  assertCharacter(vec, all.missing = F)
  
  # Classify each character as uppercase (A), lowercase (a), or digit (0). Leave punctuation marks as they are (because for my use case, different punctuation = different format)
  classified <- vec %>%
    str_replace_all(., "[0-9]", "0") %>%
    str_replace_all(., "[A-Z]", "A") %>%
    str_replace_all(., "[a-z]", "a")
  
  # Two possible returns: all unique formats, or all formats
  if(unique == T){
    # Get all unique formats
    unique_formats <- unique(classified)
    
    # Return unique formats
    return(unique_formats)
  }else{
    return(classified)
  }
}