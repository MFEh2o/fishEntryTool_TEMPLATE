# Function for generating in-season database files from fish datasheets
# Originally developed by Stuart E. Jones
# Last updated by Kaija Gahm, June 2021

# Load packages -----------------------------------------------------------
library(tidyverse) # because I use pipes/tidyverse functions a lot
library(here) # for simpler relative file paths
library(checkmate) # for assert* style unit tests, which are scattered throughout to ensure that data inputs have expected formats.
library(lubridate) # for dealing with dates/times
# Supporting functions, to keep fishEntry.R pretty short and understandable
source(here("code", "supportingFuns.R"))
# Check functions, to be called by fishEntry.R
source(here("code", "checks.R"))
Sys.setenv(tz = "America/Chicago")

# Main function to update the log files. The arguments it takes are defined in the function call in updateFish.R
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
  ## I suppress warnings here because dbTable usually returns a lot of lengthy/verbose warnings about NA's, etc.
  message("Loading database tables...")
  lakesDB <- suppressWarnings(dbTable("lakes"))
  sitesDB <- suppressWarnings(dbTable("sites"))
  fishSamplesDB <- suppressWarnings(dbTable("fish_samples"))
  fishInfoDB <- suppressWarnings(dbTable("fish_info"))
  otu <- suppressWarnings(dbTable("otu"))
  
  # Grab all the OTU rows defined as 'fish', for use in converting abbreviations to species names
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
  beenCompiled <- unique(fishSamplesIS$entryFile) # file names that already show up in the in-season fish samples file
  
  # Expected file name formats
  ## Regular fishing (AN, FN, BE, aka *not* minnow traps): *YYYY-MM-DD_hhmm.csv, where * can be anything (doesn't matter what comes before the date and time), Y, M, D, h, and m are all digits between 0 and 9, and the - and _ can actually be any punctuation mark (because there was some variability in how the file names were written).
  ## Minnow traps: must contain 'minnowtrap'.
  filenames <- list.files(path = ssdir, 
                          pattern = "[0-9]{4}[[:punct:]][0-9]{2}[[:punct:]][0-9]{2}[[:punct:]][0-9]{4}\\.csv|minnowtrap")
  toCompile <- filenames[!filenames %in% beenCompiled] # files that haven't yet been compiled
  # If any file names don't match the above patterns, the tool will warn you, because otherwise they would just silently remain unprocessed.
  otherFiles <- filenames[!filenames %in% beenCompiled & !filenames %in% toCompile]
  
  # Initialize data frames to hold the new FISH_INFO and FISH_SAMPLES data
  ## We initialize FISH_INFO and FISH_SAMPLES from the database tables so they'll have the same column names. That saves us from having to manually set a bunch of lesser-used columns to NA. Because I use `bind_rows()` later on in the tool instead of `rbind()`, the new data that we append doesn't have to have all of the same columns as the database tables. Any columns it doesn't have will just be filled in with NA.
  newFI <- fishInfoDB[FALSE, ] # info
  newFS <- fishSamplesDB[FALSE, ] # samples
  newFO <- data.frame() # otoliths
  newFP <- data.frame() # spines
  newFC <- data.frame() # scales
  newFD <- data.frame() # diets
  
  # Compile sample sheets --------------------------------------------------
  if(length(toCompile) == 0){
    # No files that have not been compiled into the in-season database
    "The in season database is up to date; no new files to compile"
    # If there were any file names that didn't match the format, warn the user:
    if(length(otherFiles) > 0){
      warning(paste0("The following sample sheets won't be compiled:\n\n", 
                     paste(otherFiles, collapse = ", "),
                     ". Check that the file names have the correct format."))
    }
  }else{
    message("Compiling sample sheets")
    # There are files to be compiled; generate rows to append
    for(i in 1:length(toCompile)){
      # Save the file name as a variable `file` for future use
      file <- toCompile[i] # the current file name
      message(paste0("Compiling file ", i, ": ", file)) # progress message
      
      # Set a flag to tell us if this is a minnow trap data sheet
      # XXX maybe put in another check? If someone forgets to put minnowtrap in the file name, the tool will assume it's regular fish sampling. Another telltale sign of minnow traps is the presence of a trapNumber column, so maybe check for that as well. If the file name doesn't have 'minnowtrap' in it, but there is a trapNumber column detected, should throw an error and force them to change the file name.
      isMinnow <- ifelse(grepl("minnowtrap", file), TRUE, FALSE)
      
      # Get file ----------------------------------------------------------
      # Read in the current file, setting all blank cells to NA
      cur <- read.csv(here(ssdir, file), 
                      na.strings = c("", " ", "NA"), header = F)
      
      # Get header --------------------------------------------------------
      # Pull header info
      header <- getHeader(d = cur, 
                          # minnow trap data sheets have a different # of header rows
                          hr = ifelse(isMinnow, headerRows - 1, headerRows))
      
      # Check the header values
      ## Check for missing values, wrong formats, etc.
      checkHeader(h = header, f = file, 
                  # different requirements if minnow trap vs. not
                  m = isMinnow)
      
      # Get data ----------------------------------------------------------
      # Read in the tabular data (non-header data)
      curData <- getCurData(cur, 
                            # minnow traps have a different number of header rows
                            hr = ifelse(isMinnow, headerRows - 1, headerRows))
      
      # Check tag column format
      ## For non-minnow-traps, check that the sheet has the new pitApply, pitRecapture, floyApply, floyRecapture format. If not, throw an error and ask for a correction.
      checkTagColsFormat(m = isMinnow, d = curData)
      
      # Get date and time strings for creating sampleID's and fishID's
      dateSampleString <- date(header$dateTimeSample) %>% 
        str_remove_all(., "-")
      # check format of date string
      assertCharacter(dateSampleString, len = 1, 
                      pattern = "20[0-9]{2}[0-2][0-9][0-5][0-9]")
      
      # Have to left-pad both the hour and minute values with zeroes, up to width 2, because lubridate will return single-digit hours/minutes (3) instead of double digits (03), which makes the format check inconsistent.
      timeSampleString <- paste0(str_pad(hour(header$dateTimeSample), width = 2, 
                                         side = "left", pad = "0"), 
                                 str_pad(minute(header$dateTimeSample), width = 2,
                                         side = "left", pad = "0"))
      # check format of time string
      assertCharacter(timeSampleString, len = 1, pattern = "[0-2][0-9][0-5][0-9]")
     
      
      # Make FISH_SAMPLES --------------------------------------------------
      # For regular fishing, take most of the sample information from the header, and then add the date and time strings created above to generate sampleID's. For minnow traps, make one FISH_SAMPLES line for each minnow trap, even if NFC.
      fishSamplesNEW <- makeFishSamplesNEW(h = header, dss = dateSampleString, 
                                           tss = timeSampleString, f = file, 
                                           m = isMinnow, dat = curData)

      # Make FISH_INFO -----------------------------------------------------
      # Get fish data, remove NFC, and enter each fish. Create fishID's and sampleID's from header info and date/time sample strings.
      fishInfoNEW <- makeFishInfoNEW(d = curData, h = header, 
                                     dss = dateSampleString, 
                                     tss = timeSampleString, f = file,
                                     m = isMinnow)
      
      # Convert species abbreviations to common names
      # Fish species are entered as abbreviations. Compare to a list of acceptable abbreviations from fishNames (extracted from the OTU table) and use that to convert the abbreviations to full otu's. If any new species are forced in, just pass the abbreviation value straight through to otu, since we have no way of doing any other conversion.
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
    
    # FISH_SAMPLES
    ## any new lakeID's?
    checkForNew(colName = "lakeID", new = newFS, db = lakesDB, 
                is = fishSamplesIS, f = force_lakeID)
    
    ## any new siteID's?
    checkForNew(colName = "siteID", new = newFS, db = sitesDB, 
                is = fishSamplesIS, f = force_siteID)
    
    ## any new gear types?
    checkForNew(colName = "gear", new = newFS, db = fishSamplesDB, 
                is = fishSamplesIS, f = force_gear)
    
    ## any new sampleGroups?
    checkForNew(colName = "sampleGroup", new = newFS, db = fishSamplesDB, 
                is = fishSamplesIS, f = force_sampleGroup)
    
    ## any new effortUnit types?
    checkForNew(colName = "effortUnits", new = newFS, db = fishSamplesDB, 
                is = fishSamplesIS, f = force_effortUnits)
    
    ## any new metadataID's? (It's pretty common to catch typos here.)
    checkForNew(colName = "metadataID", new = newFS, db = fishSamplesDB, 
                is = fishSamplesIS, f = force_metadataID)
    
    ## any new projectID's?
    checkForNew(colName = "projectID", new = newFS, db = fishSamplesDB, 
                is = fishSamplesIS, f = force_newProjectID)
    
    ## any projectID's that were supposed to be retired (not used anymore)? (see supportingFuns.R for a list of retired projectID's)
    retiredProjectIDsCheck(new = newFS, f = force_retiredProjectID)
    
    ## Controlled vocabulary for useCPUE (no force allowed)
    assertSubset(newFS$useCPUE, 
                 choices = unique(fishSamplesDB$useCPUE))
    
    ## Controlled vocabulary for useSampleMarkRecap (no force allowed)
    assertSubset(newFS$useSampleMarkRecap, 
                 choices = unique(fishSamplesDB$useSampleMarkRecap))
    
    ## Check for repeat sampleID's in FISH_SAMPLES
    #checkForRepeats(colName = "sampleID", new = newFS, db = fishSamplesDB, 
                    # is = fishSamplesIS) # XXX commenting this out so I can test the tool on old data
    
    ## Check that set/sample dates and times make sense
    checkDateTimes(new = newFS)
    
    ## dayOfYear in a reasonable range? 
    checkRangeLimits(colName = "dayOfYear", new = newFS,
                     f = force_dayOfYear, minVal = 91, maxVal = 305, 
                     allowMinEqual = F, allowMaxEqual = F)
    
    ## distanceShocked in a reasonable range?
    checkRangeLimits(colName = "distanceShocked",  
                     new = newFS, f = force_distanceShocked, minVal = 0, 
                     maxVal = 25, allowMinEqual = F, allowMaxEqual = F)
    
    ## effort in a reasonable range?
    checkRangeLimits(colName = "effort", new = newFS,
                     f = force_effort, minVal = 0, maxVal = 24,
                     allowMinEqual = T, allowMaxEqual = F)
    
    # FISH_INFO
    ## any new otu (fish species) values?
    checkForNew(colName = "otu", new = newFI, db = fishInfoDB,
                is = fishInfoIS, f = force_species)
    
    ## any duplicate fishID's?
    # checkDuplicateFishIDs(new = newFI, is = fishInfoIS, db = fishInfoDB) # XXX commenting this out so I can test out the tool on old data
    
    ## check absolute bounds for length and weight, and run a length-weight regression where possible. Are length/weight values reasonable for the species?
    checkFishLengthWeight(db = fishInfoDB, new = newFI,
                          force_fishLength = force_fishLength, 
                          force_fishWeight = force_fishWeight)
    
    ## any repeated sampleID's in FISH_INFO?
    # checkForRepeats(colName = "sampleID", new = newFI, db = fishInfoDB, 
    #                 is = fishInfoIS) # XXX commenting this out so I can test out the tool on old data
    
    ## any new clipApply codes?
    checkForNew(colName = "clipApply", new = newFI, db = fishInfoDB, 
                is = fishInfoIS, f = force_clipApply)
    
    ## have recaptured clips been applied to this fish species in this lake before?
    checkClipRecapture(new = newFI, db = fishInfoDB, is = fishInfoIS, 
                       f = force_clipRecapture)
    
    ## have recaptured tags been previously applied to this fish species in this lake?
    checkTagRecapture(new = newFI, db = fishInfoDB, is = fishInfoIS, 
                      fd = force_tagRecapType, fn = force_tagRecap, 
                      fs = force_tagRecapSpecies, fl = force_tagRecapLake)
    
    ## any repeated pitApply values?
    checkForRepeats(colName = "pitApply", new = newFI, db = fishInfoDB,
                    is = fishInfoIS, na.ok = T, f = force_pitApply)
    
    ## any repeated floyApply values?
    checkForRepeats(colName = "floyApply", new = newFI, db = fishInfoDB,
                    is = fishInfoIS, na.ok = T, f = force_floyApply)
    
    ## where possible, compute growth curves for tagged fish that have been previously recaptured, and see if their weight makes sense.
    # XXX I think there's something wrong with this check--it's flagging way more fish than I would expect.
    vonBCheck(new = newFI, db = fishInfoDB, is = fishInfoIS, f = force_vonB)
    
    ## have these fish species been reported in these lakes before?
    checkLakeSpecies(new = newFI, db = fishInfoDB, is = fishInfoIS, 
                     f = force_lakeSpecies)
    
    ## do the pit and floy tags have reasonable formats?
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
    # XXX should add a column name check here so no new columns get added. Make sure to include entryFile in addition to the database table column names.
    
    message("Writing files...")
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
    
    message("Data entry complete!")
    if(length(otherFiles) > 0){
      warning(paste0("The following sample sheets won't be compiled:\n\n", 
                     paste(otherFiles, collapse = ", "),
                     ". Check that the file names have the correct format."))
    }
  }
}
