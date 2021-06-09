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
                       force_floyLake = FALSE,
                       force_tagRecap = FALSE){
  
  source(file.path(funcdir, "dbUtil.R")) # load the dbUtil functions
  
  # Load database tables ---------------------------------------------------
  lakesDB <- suppressWarnings(dbTable("lakes"))
  sitesDB <- suppressWarnings(dbTable("sites"))
  fishSamplesDB <- suppressWarnings(dbTable("fish_samples"))
  fishInfoDB <- suppressWarnings(dbTable("fish_info"))
  otu <- suppressWarnings(dbTable("otu"))
  
  # Grab all the 'fish' from OTU, for use in converting abbreviations to species names
  fishNames <- otu %>% filter(grouping == "fish")
  
  # Load in-season db files ------------------------------------------------
  # (or initialize them if they don't already exist)
  ## FISH_INFO
  if("fishInfoIS.csv" %in% list.files(isdir)){
    fishInfoIS <- read.csv(here(isdir, "fishInfoIS.csv"), 
                           header = T, stringsAsFactors = F)
  }else{
    fishInfoIS <- fishInfoDB[FALSE, ]
  }
  ## FISH_SAMPLES
  if("fishSamplesIS.csv" %in% list.files(isdir)){
    fishSamplesIS <- read.csv(here(isdir, "fishSamplesIS.csv"), 
                              header = T, stringsAsFactors = F)
  }else{
    fishSamplesIS <- fishSamplesDB[FALSE, ]
  }
  
  # Prepare to compile files -----------------------------------------------
  # Check which files have been compiled and which have not in the directory
  beenCompiled <- unique(fishInfoIS$entryFile)
  filenames <- list.files(path = here("sampleSheets"), 
                          pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{4}\\.csv")
  toCompile <- filenames[!filenames %in% beenCompiled]
  
  # Initialize data frames to hold the new FISH_INFO and FISH_SAMPLES data
  newFI <- data.frame() # info
  newFS <- data.frame() # samples
  newFO <- data.frame() # otoliths
  newFP <- data.frame() # spines
  newFC <- data.frame() # scales
  newFD <- data.frame() # diets
  
  # Compile sample sheets --------------------------------------------------
  if(length(toCompile) == 0){
    # No files that have not been compiled into the in-season database
    "The in season database is up to date; no new files to compile"
  }else{
    # There are files to be compiled; generate rows to append
    for(i in 1:length(toCompile)){
      message("Compiling sample sheets")
      # Save the file name as a variable `file` for future use
      file <- toCompile[i]
      message(paste0("Compiling file ", i, ": ", file))
      
      # Get file ----------------------------------------------------------
      # Read in the current file, setting all blank cells to NA
      cur <- read.csv(here("sampleSheets", file), 
                      na.strings = c("", " ", "NA"), header = F)
      
      # Get header --------------------------------------------------------
      # Pull header info
      header <- getHeader(cur)
      
      # Check the header values
      checkHeader(header)
      
      # Get data ----------------------------------------------------------
      # Tabular data
      curData <- getCurData(cur)
      
      # Get date and time strings for sampleID's and fishID's
      dateSampleString <- date(header$dateTimeSample) %>% str_remove_all(., "-")
      timeSampleString <- paste0(hour(header$dateTimeSample), minute(header$dateTimeSample))

      # Make FISH_INFO -----------------------------------------------------
      fishInfoNEW <- makeFishInfoNEW(d = curData, h = header, 
                                     dss = dateSampleString, 
                                     tss = timeSampleString, f = file)
      # Convert species abbreviations to common names
      fishInfoNEW <- convertSpeciesAbbreviations(x = fishInfoNEW, fn = fishNames)
      # Convert the tag columns to match the new format
      fishInfoNEW <- convertTagColumns(fin = fishInfoNEW)
        
      # Make FISH_SAMPLES --------------------------------------------------
      fishSamplesNEW <- makeFishSamplesNEW(h = header, dss = dateSampleString, 
                                           tss = timeSampleString, f = file)
      
      # Get otolith data -------------------------------------------------
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
      
      # Get spine data -------------------------------------------------
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
      
      # Get scale data -------------------------------------------------
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
      
      # Get diet data -------------------------------------------------
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
      
      # Update temporary df's with new data -------------------------------
      if(exists("fishInfoNEW")){newFI <- bind_rows(tochar(newFI),
                                                      tochar(fishInfoNEW))}
      if(exists("fishSamplesNEW")){newFS <- bind_rows(tochar(newFS),
                                                      tochar(fishSamplesNEW))}
      if(exists("fishOtolithsNEW")){newFO <- bind_rows(tochar(newFO),
                                                      tochar(fishOtolithsNEW))}
      if(exists("fishSpinesNEW")){newFP <- bind_rows(tochar(newFP),
                                                      tochar(fishSpinesNEW))}
      if(exists("fishScalesNEW")){newFC <- bind_rows(tochar(newFC),
                                                      tochar(fishScalesNEW))}
      if(exists("fishDietsNEW")){newFD <- bind_rows(tochar(newFD),
                                                      tochar(fishDietsNEW))}
    }
    
    # Run checks --------------------------------------------------------------
    # Note that I run these checks *before* adding the new compiled data to the in-season database. Otherwise, I'd have to separate out the new IS data from the old IS data in every check function, which is super tedious.
    # XXX possible fuzzy matching 'did you mean' option for e.g. site names, metadataID's
    # FISH_SAMPLES
    checkForNew(colName = "lakeID", new = newFS, db = lakesDB, 
                is = fishSamplesIS, f = force_lakeID)
    checkForNew(colName = "siteID", new = newFS, db = sitesDB, 
                is = fishSamplesIS, f = force_siteID)
    checkForNew(colName = "gear", new = newFS, db = fishSamplesDB, 
                is = fishSamplesIS, f = force_gear)
    checkForNew(colName = "sampleGroup", new = newFS, db = fishSamplesDB, 
                is = fishSamplesIS, f = force_sampleGroup)
    checkForNew(colName = "effortUnits", new = newFS, db = fishSamplesDB, 
                is = fishSamplesIS, f = force_effortUnits)
    checkForNew(colName = "metadataID", new = newFS, db = fishSamplesDB, 
                is = fishSamplesIS, f = force_metadataID)
    checkForNew(colName = "projectID", new = newFS, db = fishSamplesDB, 
                is = fishSamplesIS, f = force_newProjectID)
    assertSubset(newFS$useCPUE, 
                 choices = unique(fishSamplesDB$useCPUE)) # no force option
    assertSubset(newFS$useSampleMarkRecap, 
                 choices = unique(fishSamplesDB$useSampleMarkRecap)) # no force option
    checkForRepeats(colName = "sampleID", new = newFS, db = fishSamplesDB, 
                    is = fishSamplesIS)
    #checkDateTimes(new = newFS) # XXX come back to this one
    checkRangeLimits(colName = "doy", new = newFS,
                     f = force_dayOfYear, minVal = 91, maxVal = 305, 
                     allowMinEqual = F, allowMaxEqual = F)
    checkRangeLimits(colName = "distanceShocked",  
                     new = newFS, f = force_distanceShocked, minVal = 0, 
                     maxVal = 25, allowMinEqual = F, allowMaxEqual = F)
    checkRangeLimits(colName = "effort", new = newFS,
                     f = force_effort, minVal = 0, maxVal = 24,
                     allowMinEqual = T, allowMaxEqual = F)
    
    # FISH_INFO
    checkForNew(colName = "otu", new = newFI, db = fishInfoDB,
                is = fishInfoIS, f = force_species)
    checkDuplicateFishIDs(new = newFI, is = fishInfoIS, db = fishInfoDB)
    checkFishLengthWeight(db = fishInfoDB, new = newFI, is = fishInfoIS, 
                          fl = force_fishLength, fw = force_fishWeight)
    checkForRepeats(colName = "sampleID", new = newFI, db = fishInfoDB, 
                    is = fishInfoIS)
    checkForNew(colName = "clipApply", new = newFI, db = fishInfoDB, 
                is = fishInfoIS, f = force_clipApply)
    checkClipRecapture(new = newFI, db = fishInfoDB, is = fishInfoIS, 
                       f = force_clipRecapture)
    checkTagRecapture(new = newFI, db = fishInfoDB, is = fishInfoIS, 
                      fd = force_tagRecapType, fn = force_tagRecap, 
                      fs = force_tagRecapSpecies, fl = force_tagRecapLake)
    checkForRepeats(colName = "pitApply", new = newFI, db = fishInfoDB,
                    is = fishInfoIS, na.ok = T, f = force_pitApply)
    checkForRepeats(colName = "floyApply", new = newFI, db = fishInfoDB,
                    is = fishInfoIS, na.ok = T, f = force_floyApply)
    vonBCheck(new = newFI, db = fishInfoDB, is = fishInfoIS, f = force_vonB)
    checkLakeSpecies(new = newFI, db = fishInfoDB, is = fishInfoIS, 
                     f = force_lakeSpecies)

    
    # Update tables with new entries ------------------------------------------
    fishInfoIS <- bind_rows(tochar(fishInfoIS), tochar(newFI))
    fishSamplesIS <- bind_rows(tochar(fishSamplesIS), tochar(newFS))
    fishOtolithsLOG <- bind_rows(tochar(fishOtolithsLOG), tochar(newFO))
    fishSpinesLOG <- bind_rows(tochar(fishSpinesLOG), tochar(newFP))
    fishScalesLOG <- bind_rows(tochar(fishScalesLOG), tochar(newFC))
    fishDietsLOG <- bind_rows(tochar(fishDietsLOG), tochar(newFD))
    
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
