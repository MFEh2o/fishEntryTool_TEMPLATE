# Function for generating in-season database files from fish datasheets
# Originally developed by Stuart E. Jones
# Last updated by Kaija Gahm, May 2021

# Show full text of errors and warnings
options(warning.length = 6000L, error.length = 6000L)

# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(checkmate)
library(janitor)
library(lubridate)
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
                       force_clipApply = FALSE,
                       force_clipRecapture = FALSE,
                       force_clipLake = FALSE,
                       force_pitApply = FALSE,
                       force_floyApply = FALSE,
                       force_pitLake = FALSE,
                       force_floyLake = FALSE){
  
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
    
  if(length(toCompile) == 0){
    # No files that have not been compiled into the in-season database
    "The in season database is up to date; no new files to compile"
  }else{
    # There are files to be compiled; generate rows to append
    for(i in 1:length(toCompile)){
      # Save the file name as a variable `file` for future use
      file <- toCompile[i]
      message(paste0("Compiling file ", i, ": ", file))
      
      # Read in the current file, setting all blank cells to NA
      cur <- read.csv(here("sampleSheets", file), 
                      na.strings = c("", " ", "NA"), header = F)
      
      # Pull header info
      header <- getHeader(cur)
      
      # Check for NA or empty strings in the header, taking into account whether a value for distanceShocked is expected. Regardless of gear, comments are allowed to be NA.
      checkHeader(header)
      
      # Tabular data
      curData <- getCurData(cur)
      
      # Get date and time strings for sampleID's and fishID's
      dateSampleString <- date(header$dateTimeSample) %>% str_remove_all(., "-")
      timeSampleString <- paste0(hour(header$dateTimeSample), minute(header$dateTimeSample))

      # Make new rows for FISH_INFO
      fishInfoNEW <- makeFishInfoNEW(d = curData, h = header, 
                                     dss = dateSampleString, 
                                     tss = timeSampleString, f = file)
      
      # Convert species abbreviations to common names
      fishInfoNEW <- convertSpeciesAbbreviations(x = fishInfoNEW, fn = fishNames)
      
      # Convert the tag columns to match the new format
      ## First, basic checks on tag types and numbers
      #checkTag(fishInfoNEW, type = "apply") # XXX come back to this
      #checkTag(fishInfoNEW, type = "recapture") # XXX come back to this
      #checkTagRecap(fishInfoNEW)
      assertAtomic(fishInfoNEW$fishID, unique = TRUE) # make sure all the fishID's are unique
      
      fishInfoNEW <- convertTagColumns(fin = fishInfoNEW)
        
      # generate FISH_SAMPLES rows
      fishSamplesNEW <- makeFishSamplesNEW(h = header, dss = dateSampleString, 
                                           tss = timeSampleString, f = file)
      
      # Check for otoliths pulled and generate a log of fish otoliths
      if("otolithSample" %in% names(curData)){
        # If any otoliths were taken
        if(any(curData$otolithSample == 1)){
          # Load the log if it exists
          if("fishOtolithsLOG.csv" %in% list.files(isdir)){
            fishOtolithsLOG <- read.csv(here(isdir, "fishOtolithsLOG.csv"), 
                                        header = T, stringsAsFactors = F)
            # Else, create a log
          }else{fishOtolithsLOG <- data.frame()}
          fishOtolithsNEW <- makeFishOtolithsNEW(d = curData, h = header,
                                                 dss = dateSampleString,
                                                 tss = timeSampleString)
        }
      }
      
      # Check for spines pulled and generate a log of fish spines
      if("spineSample" %in% names(curData)){
        if(any(curData$spineSample == 1)){
          if("fishSpinesLOG.csv" %in% list.files(isdir)){
            fishSpinesLOG <- read.csv(here(isdir, "fishSpinesLOG.csv"),
                                      header = T, stringsAsFactors = F)
          }else{fishSpinesLOG <- data.frame()}
          fishSpinesNEW <- makeFishSpinesNEW(d = curData, h = header, 
                                             dss = dateSampleString, 
                                             tss = timeSampleString)
        }
      }
      
      # Check for scales pulled and generate a log of fish scales
      if("scaleSample" %in% names(curData)){
        if(any(curData$scaleSample == 1)){
          if("fishScalesLOG.csv" %in% list.files(isdir)){
            fishScalesLOG <- read.csv(here(isdir, "fishScalesLOG.csv"),
                                      header = T, stringsAsFactors = F)
          }else{fishScalesLOG <- data.frame()}
          fishScalesNEW <- makeFishScalesNEW(d = curData, h = header, 
                                             dss = dateSampleString, 
                                             tss = timeSampleString)
        }
      }
      
      # Check for diets taken and generate a log of diets
      if("dietSampled" %in% names(curData)){
        if(any(curData$dietSampled == 1)){
          if("fishDietsLOG.csv" %in% list.files(isdir)){
            fishDietsLOG <- read.csv(here(isdir, "fishDietsLOG.csv"),
                                     header = T, stringsAsFactors = F)
          }else{fishDietsLOG <- data.frame()}
          fishDietsNEW <- makeFishDietsNEW(d = curData, h = header, 
                                           dss = dateSampleString, 
                                           tss = timeSampleString)
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
    # XXX sort checks by which data frame (fish samples, fish info) they're checking
    checkForNew(colName = "lakeID", tc = toCompile, db = lakesDB, 
                is = fishSamplesIS, 
                f = force_lakeID)
    checkForNew(colName = "siteID", tc = toCompile, db = sitesDB, is = fishSamplesIS, 
                f = force_siteID) # XXX Could maybe have a 'did you mean' option that uses fuzzy matching or similar to find any siteID's that are similar, and suggest them?
    checkForNew(colName = "gear", tc = toCompile, db = fishSamplesDB, is = fishSamplesIS,
                f = force_gear)
    checkForNew(colName = "sampleGroup", tc = toCompile, db = fishSamplesDB, is = fishSamplesIS,
                f = force_sampleGroup)
    checkForNew(colName = "effortUnits", tc = toCompile, db = fishSamplesDB, is = fishSamplesIS,
                f = force_effortUnits)
    checkForNew(colName = "metadataID", tc = toCompile, db = fishSamplesDB, is = fishSamplesIS,
                f = force_metadataID) # XXX Could maybe have a 'did you mean' option that uses fuzzy matching or similar to find any metadataID's that are similar, and suggest them?
    checkForNew(colName = "projectID", tc = toCompile, db = fishSamplesDB, is = fishSamplesIS,
                f = force_newProjectID)
    checkForNew(colName = "useCPUE", tc = toCompile, db = fishSamplesDB, is = fishSamplesIS) # no force option
    checkForNew(colName = "useSampleMarkRecap", tc = toCompile, db = fishSamplesDB, 
                is = fishSamplesIS) # no force option
    checkForNew(colName = "otu", tc = toCompile, db = fishInfoDB,
                is = fishInfoIS, f = force_species)
    checkForRepeats(colName = "sampleID", tc = toCompile, db = fishSamplesDB, 
                    is = fishSamplesIS)
    checkDuplicateFishIDs(is = fishInfoIS, db = fishInfoDB, tc = toCompile)
    checkDateTimes(is = fishSamplesIS, tc = toCompile)
    checkRangeLimits(colName = "doy", is = fishSamplesIS, tc = toCompile,
                     f = force_dayOfYear,
                     minVal = 91, maxVal = 305, 
                     allowMinEqual = F, allowMaxEqual = F)
    checkRangeLimits(colName = "distanceShocked", is = fishSamplesIS, 
                     tc = toCompile, 
                     f = force_distanceShocked,
                     minVal = 0, maxVal = 25,
                     allowMinEqual = F, allowMaxEqual = F)
    checkRangeLimits(colName = "effort", is = fishSamplesIS, 
                     tc = toCompile,
                     f = force_effort,
                     minVal = 0, maxVal = 24,
                     allowMinEqual = T, allowMaxEqual = F)
    checkFishLengthWeight(db = fishInfoDB, tc = toCompile, is = fishInfoIS, 
                          fl = force_fishLength, fw = force_fishWeight)
    checkForNew(colName = "clipApply", tc = toCompile, db = fishInfoDB, 
                is = fishInfoIS, f = force_clipApply)
    checkForNew(colName = "clipRecapture", tc = toCompile, db = fishInfoDB,
                is = fishInfoIS, f = force_clipRecapture)
    checkForRepeats(colName = "pitApply", tc = toCompile, db = fishInfoDB,
                    is = fishInfoIS, na.ok = T, f = force_pitApply)
    checkForRepeats(colName = "floyApply", tc = toCompile, db = fishInfoDB,
                    is = fishInfoIS, na.ok = T, f = force_floyApply)
    clipTagLakeCheck(tc = toCompile, db = fishInfoDB, is = fishInfoIS,
                 f = force_clipLake, type = "clip")
    clipTagLakeCheck(tc = toCompile, db = fishInfoDB, is = fishInfoIS,
                 f = force_pitLake, type = "pit")
    clipTagLakeCheck(tc = toCompile, db = fishInfoDB, is = fishInfoIS,
                 f = force_floyLake, type = "floy")

    
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
