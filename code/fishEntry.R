# Function for generating in-season database files from fish datasheets
# Originally developed by Stuart E. Jones
# Last updated by Kaija Gahm, June 2021

# Show full text of errors and warnings
options(warning.length = 6000L, error.length = 6000L)

# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(checkmate)
library(lubridate)
source(here("code", "supportingFuns.R"))
source(here("code", "checks.R"))
Sys.setenv(tz = "America/Chicago")

updateFish <- function(headerRows = 18, dbdir, db, funcdir, isdir, ssdir,
                       force_species = F,
                       force_lakeID = F,
                       force_siteID = F,
                       force_gear = F,
                       force_sampleGroup = F,
                       force_effortUnits = F,
                       force_metadataID = F,
                       force_newProjectID = F,
                       force_retiredProjectID = F,
                       force_dayOfYear = F,
                       force_distanceShocked = F,
                       force_effort = F,
                       force_fishLength = F,
                       force_fishWeight = F,
                       force_clipApply = F,
                       force_clipRecapture = F,
                       force_tagRecapType = F,
                       force_tagRecap = F,
                       force_tagRecapSpecies = F,
                       force_tagRecapLake = F,
                       force_pitApply = F,
                       force_floyApply = F,
                       force_vonB = F,
                       force_lakeSpecies = F,
                       force_pitFormat = F,
                       force_floyFormat = F){
  
  source(file.path(funcdir, "dbUtil.R")) # load the dbUtil functions
  
  # Load database tables ---------------------------------------------------
  message("Loading database tables...")
  lakesDB <- suppressWarnings(dbTable("lakes"))
  sitesDB <- suppressWarnings(dbTable("sites"))
  fishSamplesDB <- suppressWarnings(dbTable("fish_samples"))
  fishInfoDB <- suppressWarnings(dbTable("fish_info"))
  otu <- suppressWarnings(dbTable("otu"))
  
  # Grab all the 'fish' from OTU, for use in converting abbreviations to species names
  fishNames <- otu %>% filter(grouping == "fish")
  
  # Load in-season db files ------------------------------------------------
  # (or initialize them if they don't already exist)
  message("Loading or initializing in-season db files...")
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
  message("Finding files to compile...")
  beenCompiled <- unique(fishInfoIS$entryFile)
  filenames <- list.files(path = ssdir, 
                          pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{4}\\.csv|minnowtrap")
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
    message("Compiling sample sheets")
    # There are files to be compiled; generate rows to append
    for(i in 1:length(toCompile)){
      # Save the file name as a variable `file` for future use
      file <- toCompile[i]
      message(paste0("Compiling file ", i, ": ", file))
      
      # Set a flag to tell us if this is a minnow trap data sheet
      isMinnow <- ifelse(grepl("minnowtrap", file), TRUE, FALSE)
      
      # Get file ----------------------------------------------------------
      # Read in the current file, setting all blank cells to NA
      cur <- read.csv(here(ssdir, file), 
                      na.strings = c("", " ", "NA"), header = F)
      
      # Get header --------------------------------------------------------
      # Pull header info
      header <- getHeader(d = cur, 
                          # minnow traps have a different number of header rows
                          hr = ifelse(isMinnow, headerRows - 1, headerRows))
      
      # Check the header values
      checkHeader(h = header, f = file, m = isMinnow)
      
      # Get data ----------------------------------------------------------
      # Tabular data
      curData <- getCurData(cur, 
                            # minnow traps have a different number of header rows
                            hr = ifelse(isMinnow, headerRows - 1, headerRows))
      
      # Get date and time strings for sampleID's and fishID's
      dateSampleString <- date(header$dateTimeSample) %>% str_remove_all(., "-")
      timeSampleString <- paste0(hour(header$dateTimeSample), minute(header$dateTimeSample))
      
      # Make FISH_SAMPLES --------------------------------------------------
      fishSamplesNEW <- makeFishSamplesNEW(h = header, dss = dateSampleString, 
                                           tss = timeSampleString, f = file, 
                                           m = isMinnow, dat = curData)

      # Make FISH_INFO -----------------------------------------------------
      fishInfoNEW <- makeFishInfoNEW(d = curData, h = header, 
                                     dss = dateSampleString, 
                                     tss = timeSampleString, f = file,
                                     m = isMinnow)
      
      # Convert species abbreviations to common names
      fishInfoNEW <- convertSpeciesAbbreviations(x = fishInfoNEW, fn = fishNames, 
                                                 f = force_species)
      
      # Get otolith data -------------------------------------------------
      # Check for otoliths pulled and generate a log of fish otoliths
      if("otolithSampled" %in% names(curData)){
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
        if(any(!is.na(curData$spineSample) & curData$spineSample == 1)){
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
        if(any(!is.na(curData$scaleSample) & curData$scaleSample == 1)){
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
        if(any(!is.na(curData$dietSampled) & curData$dietSampled == 1)){
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
    binary <- c(NA, "1", "0", 1, 0)
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
    retiredProjectIDsCheck(new = newFS, f = force_retiredProjectID)
    assertSubset(newFS$useCPUE, 
                 choices = unique(fishSamplesDB$useCPUE)) # no force option
    assertSubset(newFS$useSampleMarkRecap, 
                 choices = unique(fishSamplesDB$useSampleMarkRecap)) # no force option
    checkForRepeats(colName = "sampleID", new = newFS, db = fishSamplesDB, 
                    is = fishSamplesIS)
    checkDateTimes(new = newFS)
    checkRangeLimits(colName = "dayOfYear", new = newFS,
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
    checkFishLengthWeight(db = fishInfoDB, new = newFI,
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
    # checkTagFormats(new = newFI, fp = force_pitFormat, ff = force_floyFormat) # XXX un-comment this once you've written the regular expression for the pit and floy tag formats (see GH 150)

    # Update tables with new entries ------------------------------------------
    fishInfoIS <- bind_rows(tochar(fishInfoIS), tochar(newFI))
    fishSamplesIS <- bind_rows(tochar(fishSamplesIS), tochar(newFS))
    if(exists("fishOtolithsLOG")){
      fishOtolithsLOG <- bind_rows(tochar(fishOtolithsLOG), tochar(newFO))}
    if(exists("fishSpinesLOG")){
      fishSpinesLOG <- bind_rows(tochar(fishSpinesLOG), tochar(newFP))}
    if(exists("fishScalesLOG")){
      fishScalesLOG <- bind_rows(tochar(fishScalesLOG), tochar(newFC))}
    if(exists("fishDietsLOG")){
      fishDietsLOG <- bind_rows(tochar(fishDietsLOG), tochar(newFD))}
    
    # write updates to files
    write.csv(fishInfoIS, here(isdir, "fishInfoIS.csv"), 
              row.names = FALSE)
    write.csv(fishSamplesIS, here(isdir, "fishSamplesIS.csv"), 
              row.names = FALSE)
    
    if(exists("fishDietsNEW")){
      write.csv(fishDietsLOG, here(isdir, "fishDietsLOG.csv"), 
                row.names = FALSE)}
    if(exists("fishOtolithsNEW")){
      write.csv(fishOtolithsLOG, here(isdir, "fishOtolithsLOG.csv"), 
                row.names = FALSE)}
    if(exists("fishSpinesNEW")){
      write.csv(fishSpinesLOG, here(isdir, "fishSpinesLOG.csv"), 
                row.names = FALSE)}
    if(exists("fishScalesNEW")){
      write.csv(fishScalesLOG, here(isdir, "fishScalesLOG.csv"), 
                row.names = FALSE)}
  }
}
