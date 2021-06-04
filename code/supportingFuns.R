# Supporting functions for the fish entry tool
# Created by Kaija Gahm on 21 May 2021

# Packages ----------------------------------------------------------------
library(tidyverse)

# getHeader ---------------------------------------------------------------
getHeader <- function(d = cur){
  header <- d[1:17, 1:2] %>%
    setNames(., c("key", "value")) %>%
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
  header$dateTimeSet <- lubridate::mdy_hm(header$dateTimeSet)
  header$dateTimeSample <- lubridate::mdy_hm(header$dateTimeSample)
  header$dateSet <- lubridate::date(header$dateTimeSet)
  header$dateSample <- lubridate::date(header$dateTimeSample)
  
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
  
  curData <- curData %>%
    filter(!(is.na(fishLength) & is.na(species)))

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

# makeFishInfoNEW ---------------------------------------------------------
makeFishInfoNEW <- function(d = curData, h = header, dss = dateSampleString, tss = timeSampleString, f = file){
  fishInfoNEW <- d %>%
    mutate(projectID = h$projectID,
           metadataID = h$metadataID,
           sampleID = paste(h$lakeID, h$siteName, dss,
                            tss, h$gear, metadataID, 
                            sep = "_"),
           fishNum = as.numeric(fishNum),
           fishID = paste(sampleID, fishNum, sep = "_"),
           entryFile = f)
  assertNumeric(fishInfoNEW$fishNum, any.missing = F)
  return(fishInfoNEW)
}

# convertTagColumns -------------------------------------------------------
convertTagColumns <- function(fin = fishInfoNEW){
  assertDataFrame(fin)
  assertSubset(c("tagApply", "tagRecapture", "tagApplyType", "tagRecaptureType", 
                 "oldTag", "fishID"), names(fin))
  
  # Make sure all tags are either 'pit', 'floy', or NA
  assertSubset(fin$tagApplyType, choices = c("pit", "floy", NA))
  assertSubset(fin$tagRecaptureType, choices = c("pit", "floy", NA))
  
  # Check that all rows that have a tag value also have a tag type
  missingTypeApply <- fin %>%
    filter(is.na(tagApplyType), !is.na(tagApply)) %>%
    select("fishID", tagApplyType, tagApply)
    
  missingTypeRecapture <- fin %>%
    filter(is.na(tagRecaptureType), !is.na(tagRecapture)) %>%
    select("fishID", tagRecaptureType, tagRecapture)
  
  if(nrow(missingTypeApply) > 0){
    stop(paste0("Missing type for the following applied tag numbers:\n\n",
                paste0(capture.output(missingTypeApply), collapse = "\n"),
                "\n\nYou must provide a tag type (pit or floy) in order for the entry tool to run."))
  }
  
  if(nrow(missingTypeRecapture) > 0){
    stop(paste0("Missing type for the following recaptured tag numbers:\n\n",
                paste0(capture.output(missingTypeRecapture), collapse = "\n"),
                "\n\nYou must provide a tag type (pit or floy) in order for the entry tool to run."))
  }
  
  # Check that all rows that have a tag type also have a tag value
  missingTagApply <- fin %>%
    filter(is.na(tagApply), !is.na(tagApplyType)) %>%
    select("fishID", tagApplyType, tagApply)
  
  missingTagRecapture <- fin %>%
    filter(is.na(tagRecapture), !is.na(tagRecaptureType)) %>%
    select("fishID", tagRecaptureType, tagRecapture)
  
  if(nrow(missingTagApply) > 0){
    stop(paste0("Missing tag number for the following applied tags:\n\n",
                paste0(capture.output(missingTagApply), collapse = "\n"),
                "\n\nYou must provide a tag number in order for the entry tool to run. If the tag was unreadable or you didn't record the tag number, please write 'unknown'."))
  }
  
  if(nrow(missingTagRecapture) > 0){
    stop(paste0("Missing tag number for the following recaptured tags:\n\n",
                paste0(capture.output(missingTagRecapture), collapse = "\n"),
                "\n\nYou must provide a tag number in order for the entry tool to run. If the tag was unreadable or you didn't record the tag number, please write 'unknown'."))
  }
  
  # XXX what to do about oldTag?
  # Reformat the tag columns
  tags <- fin %>%
    select(tagApply, tagRecapture, tagApplyType, tagRecaptureType, oldTag, fishID) %>%
    mutate(pitApply = case_when(!is.na(tagApply) & tagApplyType == "pit" ~ tagApply,
                                TRUE ~ NA_character_),
           floyApply = case_when(!is.na(tagApply) & tagApplyType == "floy" ~ tagApply,
                                 TRUE ~ NA_character_),
           pitRecapture = case_when(!is.na(tagRecapture) & tagRecaptureType == "pit" ~
                                      tagRecapture,
                                    TRUE ~ NA_character_),
           floyRecapture = case_when(!is.na(tagRecapture) & tagRecaptureType == "floy" ~
                                       tagRecapture,
                                     TRUE ~ NA_character_)) %>%
    select(-oldTag) %>%
    select(-c(tagApply, tagRecapture, tagApplyType, tagRecaptureType))
  
  # Join the reformatted tag columns back to fishInfoNEW and remove the old format
  fishInfoNEW <- fin %>%
    select(-c(oldTag, tagApply, tagRecapture, tagApplyType, tagRecaptureType)) %>%
    left_join(tags, by = "fishID")
  
  # Spit out the reformatted data
  return(fishInfoNEW)
}

# makeFishSamplesNEW ------------------------------------------------------
makeFishSamplesNEW <- function(h = header, dss = dateSampleString, 
                               tss = timeSampleString, f = file){
  fishSamplesNEW <- data.frame(key = names(h),
                               value = unname(unlist(h))) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    mutate(siteID = paste(lakeID, siteName, sep = "_"),
           sampleID = paste(siteID, dss, tss,
                            gear, metadataID, sep = "_"),
           dayOfYear = as.numeric(strftime(strptime(dateSample,
                                                    format = "%Y-%m-%d"),
                                           format = "%j")),
           entryFile = f,
           projectID = h$projectID,
           lakeID = h$lakeID,
           dataRecorder = h$dataRecorder,
           dataEnteredBy = h$dataEnteredBy,
           updateID = h$updateID) %>%
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
  
  
  return(fishSamplesNEW)
}

# makeFishOtolithsNEW -----------------------------------------------------
makeFishOtolithsNEW <- function(d = curData, h = header, 
                                dss = dateSampleString, 
                                tss = timeSampleString){
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
  fishScalesNEW <- curData %>%
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
  fishDietsNEW <- curData %>%
    filter(dietSampled == 1) %>%
    select(fishNum, otu) %>%
    mutate(fishID = paste(h$lakeID, h$siteName, dss,
                          tss, h$gear, h$metadataID,
                          fishNum, sep = "_"),
           lakeID = h$lakeID,
           dateSample = h$dateSample)
  return(fishDietsNEW)
}


