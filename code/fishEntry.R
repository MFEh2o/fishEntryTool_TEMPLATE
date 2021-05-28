# Function for generating in-season database files from fish datasheets
# Originally developed by Stuart E. Jones
# Last updated by Kaija Gahm, May 2021

# Show full text of errors and warnings
options(warning.length = 3000L, error.length = 3000L)

# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(checkmate)
source(here("code", "supportingFuns.R"))
source(here("code", "checks.R"))

Sys.setenv(tz = "America/Chicago")

# (remove all of this at the end)
dbdir <- here()
db <- "MFEdb_20210528.db"
funcdir <- here("code")
isdir <- here("inSeason")
# (remove the above)

updateFish <- function(headerRows = 18, dbdir, db, funcdir, isdir, 
                       force_lakeID = FALSE, 
                       force_siteID = FALSE, 
                       force_dayOfYear = FALSE, 
                       force_gear = FALSE, 
                       force_sampleGroup = FALSE, 
                       force_effort = FALSE, 
                       force_effortUnits = FALSE, 
                       force_distanceShocked = FALSE, 
                       force_metadataID = FALSE, 
                       force_projectID = FALSE, 
                       force_species = FALSE, 
                       force_fishLength = FALSE, 
                       force_fishWeight = FALSE, 
                       force_clip = FALSE){
  
  source(file.path(funcdir, "dbUtil.R")) # load the dbUtil functions
  
  # Load tables from database
  lakesDB <- suppressWarnings(dbTable("lakes"))
  fishSamplesDB <- suppressWarnings(dbTable("fish_samples"))
  fishInfoDB <- suppressWarnings(dbTable("fish_info"))
  otu <- suppressWarnings(dbTable("otu"))
  fishNames <- otu %>% filter(grouping == "fish")
  
  # Load in-season database files, or initialize them
  if("fishInfoIS.csv" %in% list.files(isdir)){
    fishInfoIS <- read.csv(here(isdir, "fishInfoIS.csv"), 
                           header = T, stringsAsFactors = F)
  }else{
    fishInfoIS <- fishInfoDB[FALSE, ]
  }
  if("fishSamplesIS.csv" %in% list.files(isdir)){
    fishSamplesIS <- read.csv(here(isdir, "fishSamplesIS.csv"), 
                              header = T, stringsAsFactors = F)
  }else{
    fishSamplesIS <- fishSamplesDB[FALSE, ]
  }
  
  # Check which files have been compiled and which have not in the directory
  beenCompiled <- unique(fishInfoIS$entryFile)
  toCompile <- list.files(path = here("sampleSheets"), 
                          pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{4}\\.csv")
  toCompile <- toCompile[!toCompile %in% beenCompiled]
  if(length(toCompile) == 0){
    # No files that have not been compiled into the in-season database
    "The in season database is up to date; no new files to compile"
  }else{
    # There are files to be compiled; generate rows to append
    for(i in 1:length(toCompile)){
      # Save the file name as a variable `file` for future use
      file <- toCompile[i]
      
      # Read in the current file, setting all blank cells to NA
      cur <- read.csv(here("sampleSheets", file), 
                      na.strings = c("", " ", "NA"), header = F)
      
      # Pull header info
      header <- getHeader(cur)
      
      # Check for NA or empty strings in the header, taking into account whether a value for distanceShocked is expected. Regardless of gear, comments are allowed to be NA.
      checkHeader(header)
      
      # Tabular data
      curData <- getCurData(cur)
      
      #need to be sure that the column names in the entry template are the same as in the database 
      #(change datasheet to match this too); then ones that aren't in curData colnames, get NA and
      #others will match
      # XXX come back to this.
      
      # Get date and time strings for sampleID's and fishID's
      dateSampleString <- strftime(strptime(header$dateTimeSample, format = "%m/%d/%Y %H:%M:%S"), format = "%Y%m%d")
      timeSampleString <- strftime(strptime(header$dateTimeSample, format = "%m/%d/%Y %H:%M:%S"), format = "%H%M")
      # XXX will need lots of checks here, and in the getHeader function too.
      
      # Make new rows for FISH_INFO # XXX this can be its own function
      fishInfoNEW <- curData %>%
        mutate(projectID = header$projectID,
               metadataID = header$metadataID,
               sampleID = paste(header$lakeID, header$siteName, dateSampleString,
                                timeSampleString, header$gear, metadataID, 
                                sep = "_"),
               fishNum = as.numeric(fishNum),
               fishID = paste(sampleID, fishNum, sep = "_"),
               entryFile = file)
      
      # Convert species abbreviations to common names
      fishInfoNEW <- convertSpeciesAbbreviations(x = fishInfoNEW, fn = fishNames)
      # Convert the tag columns to match the new format
      ## First, check to make sure all tags are either "pit" or "floy" or force a correction.
      checkTagApply(fishInfoNEW)
      checkTagRecap(fishInfoNEW)
      assertAtomic(fishInfoNEW$fishID, unique = TRUE) # make sure all the fishID's are unique
      
      tags <- fishInfoNEW %>% # XXX for now I'm going to assume that oldTag isn't useful and can be removed from the sample sheet. But I'll definitely have to revisit this if it turns out that oldTag is actually used.
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
      
      fishInfoNEW <- fishInfoNEW %>%
        select(-c(oldTag, tagApply, tagRecapture, tagApplyType, tagRecaptureType)) %>%
        left_join(tags, by = "fishID")
      # XXX will need a check for allowable clips
        
      
      # generate FISH_SAMPLES rows # XXX this can be its own function
      fishSamplesNEW <- data.frame(key = names(header),
                                   value = unname(unlist(header))) %>%
        pivot_wider(names_from = key, values_from = value) %>%
        mutate(siteID = paste(lakeID, siteName, sep = "_"),
               sampleID = paste(siteID, dateSampleString, timeSampleString,
                                gear, metadataID, sep = "_"),
               dayOfYear = as.numeric(strftime(strptime(dateSample,
                                                        format = "%Y-%m-%d"),
                                               format = "%j")),
               entryFile = file,
               projectID = header$projectID,
               lakeID = header$lakeID,
               dataRecorder = header$dataRecorder,
               dataEnteredBy = header$dataEnteredBy,
               updateID = header$updateID) %>%
        select(-siteName)
      
      # Check for otoliths pulled and generate a log of fish otoliths
      if("otolithSample" %in% names(curData)){
        # If any otoliths were taken
        if(any(curData$otolithSample == 1)){
          # Load the log if it exists
          if("fishOtolithsLOG.csv" %in% list.files(isdir)){
            fishOtolithsLOG <- read.csv(here(isdir, "fishOtolithsLOG.csv"), 
                                        header = T, stringsAsFactors = F)
            # Else, create a log
          }else{
            fishOtolithsLOG <- data.frame()
          }
          
          # Make the data for FISH_OTOLITHS # XXX this can be its own function
          fishOtolithsNEW <- curData %>%
            filter(otolithSample == 1) %>%
            select(fishNum, fishLength, fishWeight) %>%
            mutate(fishID = paste(header$lakeID, header$siteName, dateSampleString,
                                  timeSampleString, header$gear, header$metadataID,
                                  fishNum, sep = "_"),
                   otolithWeight = NA) %>%
            rename("lengthAtCapture" = fishLength,
                   "weightAtCapture" = fishWeight)
        }
      }
      
      
      # Check for spines pulled and generate a log of fish spines
      if("spineSample" %in% names(curData)){
        if(any(curData$spineSample == 1)){
          if("fishspinesLOG.csv" %in% list.files(isdir)){
            fishspinesLOG <- read.csv(here(isdir, "fishspinesLOG.csv"),
                                      header = T, stringsAsFactors = F)
          }else{
            fishspinesLOG <- data.frame()
          }
          fishspinesNEW <- curData %>%
            filter(spineSample == 1) %>%
            select(fishNum, fishLength, fishWeight) %>%
            mutate(fishID = paste(header$lakeID, header$siteName, dateSampleString,
                                  timeSampleString, header$gear, header$metadataID,
                                  fishNum, sep = "_")) %>%
            rename("lengthAtCapture" = fishLength,
                   "weightAtCapture" = fishWeight)
        }
      }
      
      # Check for scales pulled and generate a log of fish scales
      if("scaleSample" %in% names(curData)){
        if(any(curData$scaleSample == 1)){
          if("fishscalesLOG.csv" %in% list.files(isdir)){
            fishscalesLOG <- read.csv(here(isdir, "fishscalesLOG.csv"),
                                      header = T, stringsAsFactors = F)
          }else{
            fishscalesLOG <- data.frame()
          }
          fishscalesNEW <- curData %>%
            filter(scaleSample == 1) %>%
            select(fishNum, fishLength, fishWeight) %>%
            mutate(fishID = paste(header$lakeID, header$siteName, dateSampleString,
                                  timeSampleString, header$gear, header$metadataID,
                                  fishNum, sep = "_")) %>%
            rename("lengthAtCapture" = fishLength,
                   "weightAtCapture" = fishWeight)
        }
      }
      
      # Check for diets taken and generate a log of diets
      if("dietSampled" %in% names(curData)){
        if(any(curData$dietSampled == 1)){
          if("fishDietsLOG.csv" %in% list.files(isdir)){
            fishDietsLOG <- read.csv(here(isdir, "fishDietsLOG.csv"),
                                     header = T, stringsAsFactors = F)
          }else{
            fishDietsLOG <- data.frame()
          }
          fishDietsNEW <- curData %>%
            filter(dietSampled == 1) %>%
            select(fishNum, species) %>%
            mutate(fishID = paste(header$lakeID, header$siteName, dateSampleString,
                                  timeSampleString, header$gear, header$metadataID,
                                  fishNum, sep = "_"),
                   lakeID = header$lakeID,
                   dateSample = header$dateSample)
        }
      }
      
      # update tables with new entries
      fishSamplesIS <- bind_rows(tochar(fishSamplesIS), tochar(fishSamplesNEW))
      fishInfoIS <- bind_rows(tochar(fishInfoIS), tochar(fishInfoNEW))
      if(exists("fishDietsNEW")){fishDietsLOG <- bind_rows(tochar(fishDietsLOG), 
                                                           tochar(fishDietsNEW))}
      if(exists("fishOtolithsNEW")){fishOtolithsLOG <- bind_rows(tochar(fishOtolithsLOG), 
                                                                 tochar(fishOtolithsNEW))}
      if(exists("fishspinesNEW")){fishspinesLOG <- bind_rows(tochar(fishspinesLOG), 
                                                             tochar(fishspinesNEW))}
      if(exists("fishscalesNEW")){fishscalesLOG <- bind_rows(tochar(fishscalesLOG), 
                                                             tochar(fishscalesNEW))}
    }
    
    # Run checks --------------------------------------------------------------
    
    # write updates to files
    write.csv(fishInfoIS, here("inSeason", "fishInfoIS.csv"), 
              row.names = FALSE)
    write.csv(fishSamplesIS, here("inSeason", "fishSamplesIS.csv"), 
              row.names = FALSE)
    
    if(exists("fishDietsNEW")){
      write.csv(fishDietsLOG, here("inSeason", "fishDietsLOG.csv"), 
                row.names = FALSE)}
    if(exists("fishOtolithsNEW")){
      write.csv(fishOtolithsLOG, here("inSeason", "fishOtolithsLOG.csv"), 
                row.names = FALSE)}
    if(exists("fishspinesNEW")){
      write.csv(fishspinesLOG, here("inSeason", "fishspinesLOG.csv"), 
                row.names = FALSE)}
    if(exists("fishscalesNEW")){
      write.csv(fishscalesLOG, here("inSeason", "fishscalesLOG.csv"), 
                row.names = FALSE)}
    
  }
}
