# Function for generating in-season database files from fish datasheets
# Originally developed by Stuart E. Jones
# Last updated by Kaija Gahm, May 2021

# Show full text of errors and warnings
options(warning.length = 6000L, error.length = 6000L)

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
                       force_newProjectID = FALSE, 
                       force_retiredProjectID = FALSE,
                       force_species = FALSE, 
                       force_fishLength = FALSE, 
                       force_fishWeight = FALSE, 
                       force_clip = FALSE){
  
  source(file.path(funcdir, "dbUtil.R")) # load the dbUtil functions
  
  # Load tables from database
  lakesDB <- suppressWarnings(dbTable("lakes"))
  sitesDB <- suppressWarnings(dbTable("sites"))
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
  
  # Start saving header information for this round of sheets compiled (so we can run checks at the end)
  headerDF <- data.frame()
    
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
      if("species" %in% names(curData)){
        curData <- curData %>%
          rename("otu" = species)
      }
      
      #need to be sure that the column names in the entry template are the same as in the database 
      #(change datasheet to match this too); then ones that aren't in curData colnames, get NA and
      #others will match
      # XXX come back to this.
      
      # Get date and time strings for sampleID's and fishID's
      dateSampleString <- strftime(strptime(header$dateTimeSample, 
                                            format = "%m/%d/%Y %H:%M:%S"), 
                                   format = "%Y%m%d")
      timeSampleString <- strftime(strptime(header$dateTimeSample, 
                                            format = "%m/%d/%Y %H:%M:%S"), 
                                   format = "%H%M")
      # XXX will need lots of checks here, and in the getHeader function too.
      
      # Add header information to the overall headerDF
      headerRow <- header %>% unlist() %>% t() %>% as.data.frame()
      headerRow$entryFile <- file
      headerRow <- headerRow %>%
        mutate(sampleID = paste(lakeID, siteName, dateSampleString, timeSampleString, gear, metadataID, sep = "_"),
               siteID = paste(lakeID, siteName, sep = "_"),
               doy = as.numeric(strftime(strptime(dateSample,
                                                   format = "%Y-%m-%d"),
                                          format = "%j")))
      headerDF <- bind_rows(headerDF, headerRow)
      
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
      checkClips(fishInfoNEW)
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
      
      # Add nAnglers column based on crew only if effortUnits == angler_hours. 
      # checkHeader function has already validated crew and effortUnits.
      fishSamplesNEW <- fishSamplesNEW %>%
        mutate(nAnglers = case_when(effortUnits == "angler_hours" ~ 
                                      as.character(length(unlist(strsplit(fishSamplesNEW$crew, split = ", ")))),
                                    TRUE ~ NA_character_)) %>%
        relocate(nAnglers, .after = "effortUnits")
      
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
          if("fishSpinesLOG.csv" %in% list.files(isdir)){
            fishSpinesLOG <- read.csv(here(isdir, "fishSpinesLOG.csv"),
                                      header = T, stringsAsFactors = F)
          }else{
            fishSpinesLOG <- data.frame()
          }
          fishSpinesNEW <- curData %>%
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
          if("fishScalesLOG.csv" %in% list.files(isdir)){
            fishScalesLOG <- read.csv(here(isdir, "fishScalesLOG.csv"),
                                      header = T, stringsAsFactors = F)
          }else{
            fishScalesLOG <- data.frame()
          }
          fishScalesNEW <- curData %>%
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
            select(fishNum, otu) %>%
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
      if(exists("fishSpinesNEW")){fishSpinesLOG <- bind_rows(tochar(fishSpinesLOG), 
                                                             tochar(fishSpinesNEW))}
      if(exists("fishScalesNEW")){fishScalesLOG <- bind_rows(tochar(fishScalesLOG), 
                                                             tochar(fishScalesNEW))}
    }
    
    # Run checks --------------------------------------------------------------
    checkForNew(colName = "lakeID", tc = toCompile, db = lakesDB, is = fishSamplesIS, 
                hdf = headerDF, f = force_lakeID)
    checkForNew(colName = "siteID", tc = toCompile, db = sitesDB, is = fishSamplesIS, 
                hdf = headerDF, f = force_siteID) # XXX Could maybe have a 'did you mean' option that uses fuzzy matching or similar to find any siteID's that are similar, and suggest them?
    checkForNew(colName = "gear", tc = toCompile, db = fishSamplesDB, is = fishSamplesIS,
                hdf = headerDF, f = force_gear)
    checkForNew(colName = "sampleGroup", tc = toCompile, db = fishSamplesDB, is = fishSamplesIS,
                hdf = headerDF, f = force_sampleGroup)
    checkForNew(colName = "effortUnits", tc = toCompile, db = fishSamplesDB, is = fishSamplesIS,
                hdf = headerDF, f = force_effortUnits)
    checkForNew(colName = "metadataID", tc = toCompile, db = fishSamplesDB, is = fishSamplesIS,
                hdf = headerDF, f = force_metadataID) # XXX Could maybe have a 'did you mean' option that uses fuzzy matching or similar to find any metadataID's that are similar, and suggest them?
    checkForNew(colName = "projectID", tc = toCompile, db = fishSamplesDB, is = fishSamplesIS,
                hdf = headerDF, f = force_newProjectID)
    checkForNew(colName = "useCPUE", tc = toCompile, db = fishSamplesDB, is = fishSamplesIS,
                hdf = headerDF) # no force option
    checkForNew(colName = "useSampleMarkRecap", tc = toCompile, db = fishSamplesDB, 
                is = fishSamplesIS, hdf = headerDF) # no force option
    checkForNew(colName = "otu", tc = toCompile, db = fishInfoDB,
                is = fishInfoIS, hdf = fishInfoIS, f = force_species)
    checkForRepeats(colName = "sampleID", tc = toCompile, db = fishSamplesDB, 
                    is = fishSamplesIS, hdf = headerDF)
    checkDuplicateFishIDs(is = fishInfoIS, db = fishInfoDB, tc = toCompile)
    checkDateTimes(hdf = headerDF)
    checkRangeLimits(colName = "doy", hdf = headerDF, f = force_dayOfYear,
                     minVal = 91, maxVal = 305, 
                     allowMinEqual = F, allowMaxEqual = F)
    checkRangeLimits(colName = "distanceShocked", hdf = headerDF, 
                     f = force_distanceShocked,
                     minVal = 0, maxVal = 25,
                     allowMinEqual = F, allowMaxEqual = F)
    checkRangeLimits(colName = "effort", hdf = headerDF,
                     f = force_effort,
                     minVal = 0, maxVal = 24,
                     allowMinEqual = T, allowMaxEqual = F)
    checkFishLengthWeight(db = fishInfoDB, tc = toCompile, is = fishInfoIS, 
                          fl = force_fishLength, fw = force_fishWeight)
    
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
    if(exists("fishSpinesNEW")){
      write.csv(fishSpinesLOG, here("inSeason", "fishSpinesLOG.csv"), 
                row.names = FALSE)}
    if(exists("fishScalesNEW")){
      write.csv(fishScalesLOG, here("inSeason", "fishScalesLOG.csv"), 
                row.names = FALSE)}
    
  }
}
