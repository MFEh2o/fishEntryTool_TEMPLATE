# Check functions for the fish entry tool
# Created by Kaija Gahm on 21 May 2021

expectedEffortUnits <- c("angler_hours", "electrofishing_hours", 
                         "meters", "trap_hours", NA, "seine_pulls")

# checkHeader function ----------------------------------------------------
checkHeader <- function(h = header, f = file, m = isMinnow){
  # Checks for inputs
  assertList(h, names = "unique")
  assertSubset(c("comments", "distanceShocked", "gear", "effortUnits", "crew", "dateSet", "dateSample", "dateTimeSet", "dateTimeSample"), choices = names(h))
  
  # Define required fields for electrofishing vs. not, and for minnow traps
  requiredElectro <- h[!names(h) == "comments"]
  requiredNonElectro <- h[!names(h) %in% c("comments", "distanceShocked")]
  requiredMinnow <- h[!names(h) %in% c("comments", "distanceShocked", "siteName")]
  
  # Check that all required fields are present (different requirements for electrofishing vs. not)
  # Here, we define missing as either blank ("") or NA (is.na())
  if(h$gear == "BE"){
    if(any(is.na(requiredElectro)|requiredElectro == "")){
      stop(paste0("Required header information is incomplete in ", f, 
                  ". You're missing: ", 
                  paste(names(requiredElectro[requiredElectro == ""|
                                                is.na(requiredElectro)]), 
                        collapse = ", ")))
    }
  }else if(m){
    if(any(is.na(requiredMinnow)|requiredMinnow == "")){
      stop(paste0("Required header information is incomplete in ", f,
                  ". You're missing: ",
                  paste(names(requiredMinnow[requiredMinnow == ""|
                                                   is.na(requiredMinnow)]),
                        collapse = ", ")))
    }
  }else{
    if(any(is.na(requiredNonElectro)|requiredNonElectro == "")){
      stop(paste0("Required header information is incomplete in ", f,
                  ". You're missing: ",
                  paste(names(requiredNonElectro[requiredNonElectro == ""|
                                                   is.na(requiredNonElectro)]),
                        collapse = ", ")))
    }
  }
  
  # Check that effortUnits is in one of the allowable formats
  assertCharacter(expectedEffortUnits)
  if(!h$effortUnits %in% expectedEffortUnits){
    stop(paste0("effortUnits must be one of the following values: ", 
                paste(expectedEffortUnits, collapse = ", ")))
  }
  
  # If effortUnits == 'angler_hours', then we need crew to be in a certain format.
  if(h$effortUnits == "angler_hours"){
    if(!grepl("^([A-Za-z]+, )+([A-Za-z]+)$", h$crew)){
      stop(paste0("Problem with crew information. You entered:\n", 
                  h$crew,
                  "\nIn order to calculate nAnglers, crew must be either blank or a list of names or initials, upper/lowercase letters only, separated by commas and spaces. Examples: 'chris, stuart, randi', or 'CTS, SEJ, RN', or 'chris, SEJ, Randi', etc. Please correct your crew data and try again."))
    }
  }
  
  # Check the date formats
  pat_dateTime <- "^[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}\\s[0-9]{2}:[0-9]{2}:[0-9]{2}$"
  pat_date <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
  assertCharacter(as.character(h$dateTimeSet), pattern = pat_dateTime)
  assertCharacter(as.character(h$dateTimeSample), pattern = pat_dateTime)
  assertCharacter(as.character(h$dateSet), pattern = pat_date)
  assertCharacter(as.character(h$dateSample), pattern = pat_date)

}

# Checks at the end against the in-season database and full database ----------

# checkForNew -------------------------------------------------------------
# Check function that can be used to check if you're introducing any new values
checkForNew <- function(colName, new, db, is, f = NULL){
  # Check inputs
  assertDataFrame(new, col.names = "unique")
  assertDataFrame(db, col.names = "unique")
  assertDataFrame(is, col.names = "unique")
  assertChoice(colName, choices = names(db))
  assertChoice(colName, choices = names(is))
  assertChoice("entryFile", names(new))
  assertFlag(f)
  
  # Get values previously used in the database
  dbVals <- db %>% pull({{colName}}) %>% unique()
  
  # Get values previously used in the in-season table
  isVals <- is %>% pull({{colName}}) %>% unique()
  
  # Put them together
  previousVals <- c(dbVals, isVals)
  
  # Find problem rows in newData (i.e. those that have new vals)
  problemRows <- new %>%
    filter(!.data[[colName]] %in% previousVals) %>%
    select({{colName}}, entryFile) %>%
    distinct()
  
  # If there are new values, throw an error and print the new values
  if(nrow(problemRows) > 0){
    if(is.null(f)){
      stop(paste0("You are attempting to add ", colName, " values that do not exist in either the database or the in-season file. Here are the values, and the sample sheets they come from:\n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\nFor the fish entry tool to work, all your values must be included in the following set:\n\n",
                  paste0(previousVals, sep = ", ")))
    }
    if(!is.null(f) & f == FALSE){
      stop(paste0("You are attempting to add ", colName, " values that do not exist in either the database or the in-season file. Here are the values, and the sample sheets they come from:\n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\nIf you are sure that these values are valid, use the ", 
                  deparse(substitute(f)), " argument."))
    }
  }
}

# checkForRepeats ---------------------------------------------------------
# Check function to make sure you're not introducing any repeat values.
checkForRepeats <- function(colName, new, db, is, na.ok = F, f = NULL){
  # Check inputs
  assertDataFrame(new, col.names = "unique")
  assertDataFrame(db, col.names = "unique")
  assertDataFrame(is, col.names = "unique")
  assertChoice(colName, choices = names(db))
  assertChoice(colName, choices = names(is))
  assertChoice("entryFile", names(new))
  assertFlag(x = na.ok)
  assertFlag(f, null.ok = T)
  
  # Get values previously used in the database
  dbVals <- db %>% pull({{colName}}) %>% unique()
  
  # Get values previously used in the in-season table
  isVals <- is %>% pull({{colName}}) %>% unique()
  
  # Put them together
  previousVals <- c(dbVals, isVals)
  
  # Find problem rows in new (i.e. those that match/repeat a previous value)
  problemRows <- new %>%
    {if(na.ok == T) filter(., !is.na(.data[[colName]])) else .} %>%
    filter(.data[[colName]] %in% previousVals) %>%
    select({{colName}}, entryFile) %>%
    distinct()
  
  # If there are repeat values, throw an error and print the repeat values
  if(nrow(problemRows) > 0){
    if(is.null(f)){
      stop(paste0("You are attempting to add ", colName, " values that are already in either the database or the in-season file:\n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\nFor the fish entry tool to work, all of your ", colName, " values must be unique."))
    }else{
      stop(paste0("You are attempting to add ", colName, " values that are already in either the database or the in-season file:\n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\nIf you are sure you're entering the right values, you can use ", 
                  deparse(substitute(f)), 
                  " to bypass this message and enter the values anyway. But BEWARE! Tell the database manager. The database may not compile correctly if it includes repeat values."))
    }
  }
}

# checkRangeLimits --------------------------------------------------------
# Default is to define problem values as anything <= minVal and >= maxVal. If allowMinEqual = T, then only < minVal is a problem; if allowMaxEqual = T, then only > maxVal is a problem.
checkRangeLimits <- function(colName, new, minVal, maxVal, 
                             allowMinEqual = F, allowMaxEqual = F, f){
  # Check inputs
  assertDataFrame(new, col.names = "unique")
  assertChoice(colName, choices = names(new))
  assertChoice("entryFile", names(new))
  assertFlag(allowMinEqual)
  assertFlag(allowMaxEqual)
  assertFlag(f)
  assertNumeric(minVal, max.len = 1)
  assertNumeric(maxVal, max.len = 1)

  # If the column isn't numeric, coerce it to numeric
  if(!is.numeric(new[,colName])){
    new[,colName] <- as.numeric(new[,colName])
  }
  
  # Isolate problem rows
  problemRows <- new %>%
    select({{colName}}, entryFile) %>%
    {if(allowMinEqual){
      filter(., {{colName}} < minVal)
    }else{
      filter(., {{colName}} <= minVal)}
    } %>%
    {if(allowMaxEqual){
      filter(., {{colName}} > maxVal)
    }else{
      filter(., {{colName}} >= maxVal)}
    }
  
  # Throw error
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      stop(paste("Some ", colName, " values are outside the normal range of ", 
                 minVal, " to ", maxVal, ". The offending values are:\n\n",
                 paste0(capture.output(problemRows), collapse = "\n"),
                 "If you are sure that these values are correct, use the ", 
                 f, " argument."))
    }
  }
}

# checkDateTimes ----------------------------------------------------------
checkDateTimes <- function(new){
  # Check inputs
  assertDataFrame(new, col.names = "unique")
  assertSubset(c("dateSample", "dateSet"), choices = names(new))
  
  # dateSet must be the same or earlier than dateSample
  problemRowsDate <- new %>%
    filter(lubridate::ymd(dateSample) < lubridate::ymd(dateSet))
  
  # Error message for dateSet/dateSample
  if(nrow(problemRowsDate) > 0){
    stop("In the following sample sheets, the dateSample is earlier than the dateSet:\n\n", paste0(capture.output(problemRowsDate), collapse = "\n"))
  }
  
  # dateTimeSet must be the same or earlier than dateTimeSample
  problemRowsDateTime <- new %>% # XXX add a check earlier to make sure the dates are in this format.
    filter(strptime(dateTimeSample, 
                    format = "%m/%d%Y %H:%M:%S") < 
             strptime(dateTimeSet, 
                      format = "%m/%d%Y %H:%M:%S"))
  
  # Error message for dateTimeSet/dateTimeSample
  if(nrow(problemRowsDateTime) > 0){
    stop("In the following sample sheets, the dateTimeSample is earlier than the dateTimeSet:\n\n", paste0(capture.output(problemRowsDateTime), collapse = "\n"))
  }
}

# checkDuplicateFishIDs --------------------------------------------------
# Can't use checkForRepeats on this one because we care about *all* the rows, not just comparing new to old.
checkDuplicateFishIDs <- function(new, is, db){
  # Check inputs
  assertDataFrame(new, col.names = "unique")
  assertDataFrame(is, col.names = "unique")
  assertDataFrame(db, col.names = "unique")
  assertChoice("fishID", choices = names(new))
  assertChoice("fishID", choices = names(db))
  assertChoice("fishID", choices = names(is))
  assertChoice("entryFile", names(new))
  
  # Get fishID's
  dbIDs <- db$fishID # database
  isIDs <- is$fishID # in-season
  newIDs <- new$fishID # new
  
  # Check whether there are internal duplicates in the data you're entering
  if(any(duplicated(newIDs))){
    problemRows <- new %>%
      group_by(fishID) %>%
      filter(n() > 1) %>%
      arrange(fishID) %>%
      select(fishID, entryFile)
    stop(paste0("There are duplicate fishID's in the data you're trying to enter right now. The problem rows are:\n\n",
                paste0(capture.output(problemRows), collapse = "\n")))
  }
  
  # Check whether any of the fishID's that you're trying to enter already appear in the database
  if(any(newIDs %in% dbIDs)){
    problemRows <- new %>%
      filter(fishID %in% dbIDs) %>%
      select(fishID, entryFile)
    stop(paste0("You're trying to enter fishID's that are already present in the FISH_INFO database table. The problem rows are:\n\n",
                paste0(capture.output(problemRows), collapse = "\n")))
  }
  
  # Check whether any of the fishID's that you're trying to enter already appear in the in-season database
  if(any(newIDs %in% isIDs)){
    problemRows <- new %>%
      filter(fishID %in% isIDs) %>%
      select(fishID, entryFile)
    stop(paste0("You're trying to enter fishID's that are already present in the FISH_INFO in-season database. The problem rows are:\n\n",
                paste0(capture.output(problemRows), collapse = "\n")))
  }
}

# checkFishLengthWeight ---------------------------------------------------
checkFishLengthWeight <- function(new, db, force_fishLength, force_fishWeight){
  # Check inputs
  assertDataFrame(new, col.names = "unique")
  assertDataFrame(db, col.names = "unique")
  assertFlag(force_fishLength)
  assertFlag(force_fishWeight)
  assertSubset(c("fishLength", "fishWeight", "entryFile", "otu"), 
               choices = names(new))
  assertSubset(c("fishLength", "fishWeight"), choices = names(db))

  # Coerce fishLength and fishWeight to numeric
  new <- new %>% mutate(across(c("fishLength", "fishWeight"), as.numeric))
  db <- db %>% mutate(across(c("fishLength", "fishWeight"), as.numeric))

  # Throw an error for fish weights or lengths that are 0 or negative.
  problemRows <- new %>%
    filter(fishLength <= 0|fishWeight <= 0) %>%
    select(fishID, fishLength, fishWeight, entryFile)
  
  if(nrow(problemRows) > 0){
    stop(paste0("You are reporting fishLength or fishWeight values that are 0 or negative:\n\n",
                paste0(capture.output(problemRows), collapse = "\n"),
                "\n\nLengths and weights can't be 0 or negative. If these measurements are not available, leave them as NA, NOT 0."))
  }
  
  # Get a list of all the unique species being entered
  species <- unique(new$otu)
  
  # Check each species for length
  purrr::walk(.x = species, .f = function(x, ...){
    # Fish we want to check: any new fish that have length measurements.
    curNEW <- new %>% filter(otu == x, !is.na(fishLength)) %>%
      select(fishID, otu, fishLength, fishWeight)
    
    # Get all fish of this species in the database that have length measurements
    curDB <- db %>% filter(otu == x, !is.na(fishLength), fishLength > 0)
    
    # Check length only if there are at least 15 previous measurements for this species
    if(nrow(curDB) < 15){
      warning(paste0("We have <15 observations for length of species ", x, ", so we won't run an automated fishLength check. Be sure to double-check the lengths you've entered."))
    }else{
      # Compute mean and sd for the species
      mn <- mean(curDB$fishLength, na.rm = T)
      sd <- sd(curDB$fishLength, na.rm = T)
      
      # Find fish that are too short or too long
      tooLong <- curNEW %>% filter(fishLength > mn + 3*sd)
      tooShort <- curNEW %>% filter(fishLength < mn - 3*sd)
      
      # Throw error for too long
      if(nrow(tooLong) > 0){
        if(fl == FALSE){
          stop(paste0("Found some fish that are longer than 3 sd above the mean for their species, ", x, ". They are:\n\n",
                      paste0(capture.output(tooLong), collapse = "\n"),
                      "\n\nIf you're sure you want to enter these values, use ",
                      deparse(substitute(fl)), "."))
        }
      }
      
      # Throw error for too short
      if(nrow(tooShort) > 0){
        if(fl == FALSE){
          stop(paste0("Found some fish that are shorter than 3 sd below the mean for their species, ", x, ". They are:\n\n",
                      paste0(capture.output(tooShort), collapse = "\n"),
                      "\n\nIf you're sure you want to enter these values, use ",
                      deparse(substitute(fl)), "."))
        }
      }
    }
    
    # Now, only fish that have length AND weight measurements
    curDB_weight <- curDB %>%
      filter(!is.na(fishWeight), fishWeight > 0)
    
    # Do a length-weight regression if there are at least 15 fish
    if(nrow(curDB_weight) < 15){
      warning(paste0("We have <15 observations for weight of species ", x, ", so we won't run an automated length-weight regression. Be sure to double-check the weights you've entered."))
    }else{
      # Length-weight regression for this species (log-transformed)
      curDB_weight$logWeight <- log(curDB_weight$fishWeight)
      curDB_weight$logLength <- log(curDB_weight$fishLength)
      lwreg <- lm(logWeight ~ logLength, data = curDB_weight)
      
      # Predict high and low weight bounds for each individual
      preds <- predict(lwreg, newdata = 
                         data.frame(logLength = 
                                      log(curNEW$fishLength)),
                       interval = "prediction",
                       se.fit = TRUE, level = 0.99)
      curNEW$predsLow <- exp(preds$fit[,2])
      curNEW$predsHigh <- exp(preds$fit[,3])
      
      # Are any of the newly-entered fish weights < predsLow or > predsHigh...
      tooHeavy <- curNEW %>% filter(fishWeight > predsHigh)
      tooLight <- curNEW %>% filter(fishWeight < predsLow)
      
      # Throw error for heavy fish
      if(nrow(tooHeavy) > 0){
        if(fw == FALSE){
          stop(paste0("You report fishWeight heavier than the prediction based on a length-weight regression from our database for ", x, ". Here are the problematic rows:\n\n",
                      paste0(capture.output(tooHeavy), collapse = "\n"),
                      "\n\nIf you're sure you want to enter these values, use ",
                      deparse(substitute(fw)), "."))
        }
      }
      
      # Throw error for light fish
      if(nrow(tooLight) > 0){
        if(fw == FALSE){
          stop(paste0("You report fishWeight lighter than the prediction based on a length-weight regression from our database for ", x, ". Here are the problematic rows:\n\n",
                      paste0(capture.output(tooLight), collapse = "\n"),
                      "\n\nIf you're sure you want to enter these values, use ",
                      deparse(substitute(fw)), "."))
        }
      }
    }
  })
  
  # Throw an error for fish weights or lengths that are heavier than expected (this will catch anything where there were <15 fish, or anything that's off the charts for both length and weight but theoretically reasonable according to the regression)
  tooLarge <- new %>%
    filter(fishLength >= 1500|fishWeight >= 12000) %>%
    select(fishID, fishLength, fishWeight, entryFile)
  
  if(nrow(tooLarge) > 0){
    stop(paste0("Some of your length and/or weight measurements seem fishy:\n\n",
                paste0(capture.output(tooLarge), collapse = "\n"),
                "\n\nfishLength should be < 1500 mm, and fishWeight should be < 12000 g. Double-check your lengths and weights for typos or incorrect units."))
  }
}

# checkTagRecapture ------------------------------------------------------------
checkTagRecapture <- function(new, db, is, fd, fn, fs, fl){
  # Check inputs
  assertDataFrame(new, col.names = "unique")
  assertDataFrame(db, col.names = "unique")
  assertDataFrame(is, col.names = "unique")
  assertSubset(c("fishID", "pitRecapture", "floyRecapture", "otu"), 
               choices = names(new))
  assertSubset(c("fishID", "pitApply", "floyApply", "otu"), 
               choices = names(db))
  assertSubset(c("fishID", "pitApply", "floyApply", "otu"), 
               choices = names(is))
  assertFlag(fd)
  assertFlag(fn)
  assertFlag(fs)
  assertFlag(fl)
  
  # This is a long check, so give a progress message
  message("Checking recaptured tags...")
  
  # Reformat the data and get only fish with recaptured tags
  newData <- new %>%
    select(fishID, "pit" = pitRecapture, "floy" = floyRecapture, otu) %>%
    mutate(lakeID = word(fishID, 1, 1, sep = "_")) %>%
    pivot_longer(cols = c("pit", "floy"),
                 names_to = "tagRecaptureType",
                 values_to = "tagRecapture") %>%
    filter(!is.na(tagRecapture)) %>%
    mutate(
      lakeSpeciesTag = paste(lakeID, otu, tagRecapture, sep = "_"),
      lakeTag = paste(lakeID, tagRecapture, sep = "_"),
      speciesTag = paste(otu, tagRecapture, sep = "_")
    )
  
  # Create a minimal data frame of tags, lakeID's, and species for comparison
  oldData <- bind_rows(tochar(db), tochar(is)) %>%
    select(fishID, "pit" = pitApply, "floy" = floyApply, otu) %>%
    mutate(lakeID = word(fishID, 1, 1, sep = "_"),
           dateSample = lubridate::ymd(word(fishID, 3, 3, sep = "_"))) %>%
    pivot_longer(cols = c("pit", "floy"),
                 names_to = "tagApplyType",
                 values_to = "tagApply") %>%
    filter(!is.na(tagApply)) %>%
    mutate(lakeSpeciesTag = paste(lakeID, otu, tagApply, sep = "_"),
           lakeTag = paste(lakeID, tagApply, sep = "_"),
           speciesTag = paste(otu, tagApply, sep = "_")) %>%
    distinct() %>%
    # If there are multiple apply instances for whatever reason, take the first one
    arrange(tagApply, dateSample) %>%
    group_by(tagApply) %>%
    slice(1) %>%
    ungroup()
  
  if(nrow(newData) > 0){
    # Check whether there are any tags where the number matches but not the tag type
    differentTagType <- newData %>%
      select(fishID, tagRecapture, tagRecaptureType) %>%
      left_join(oldData %>%
                  select(tagApply, tagApplyType),
                by = c("tagRecapture" = "tagApply")) %>%
      filter(!is.na(tagRecaptureType), !is.na(tagApplyType), 
             tagRecaptureType != tagApplyType)
    
    if(nrow(differentTagType) > 0){
      if(fd == F){
        stop(paste0("For the following tag(s), the tag recapture type is different than the tag apply type:\n\n",
                    paste0(capture.output(differentTagType), sep = "\n"),
                    "\n\nIf you're sure you want to enter this data, use ",
                    deparse(substitute(fd)), "."))
      }
    }
    
    # Look for problems in the tag numbers themselves
    problems <- newData %>%
      # Annotate each row of the new data
      mutate(problem = case_when(
        # No problem: tag has been applied to the same species in the same lake
        lakeSpeciesTag %in% oldData$lakeSpeciesTag ~ "none",
        
        # Problem: tag has never been applied before, regardless of species/lake
        !tagRecapture %in% oldData$tagApply ~ "never applied--wrong number?",
        
        # Problem: tag has been applied before, but not in this species or lake
        (tagRecapture %in% oldData$tagApply) & (!lakeTag %in% oldData$lakeTag) &
          (!speciesTag %in% oldData$speciesTag) ~ "wrong species, wrong lake",
        
        # Problem: right lake, wrong species
        (lakeTag %in% oldData$lakeTag) &
          (!speciesTag %in% oldData$speciesTag) ~ "right lake, wrong species",
        
        # Problem: right species, wrong lake
        (speciesTag %in% oldData$speciesTag) &
          (!lakeTag %in% oldData$lakeTag) ~ "right species, wrong lake",
        
        # Other: what's going on here?
        TRUE ~ NA_character_)) %>%
      filter(problem != "none")
    
    # Separate out the problem tags so it's easier to show the data in the error messages
    neverApplied <- problems %>% 
      filter(problem == "never applied--wrong number?") %>%
      select(fishID, tagRecapture, tagRecaptureType)
    
    wrongSpeciesWrongLake <- problems %>% 
      filter(problem == "wrong species, wrong lake") %>%
      select(fishID, tagRecapture, tagRecaptureType, otu)
    original_1 <- oldData %>%
      filter(tagApply %in% wrongSpeciesWrongLake$tagRecapture) %>%
      select('applyOTU' = otu, 'applyLake' = lakeID, tagApply)
    wswl <- left_join(wrongSpeciesWrongLake, original_1, by = c("tagRecapture" = "tagApply"))
    
    rightLakeWrongSpecies <- problems %>% 
      filter(problem == "right lake, wrong species") %>%
      select(fishID, tagRecapture, tagRecaptureType, otu)
    original_2 <- oldData %>%
      filter(tagApply %in% rightLakeWrongSpecies$tagRecapture) %>%
      select('applyOTU' = otu, tagApply)
    rlws <- left_join(rightLakeWrongSpecies, original_2, by = c("tagRecapture" = "tagApply"))
    
    rightSpeciesWrongLake <- problems %>% 
      filter(problem == "right species, wrong lake") %>%
      select(fishID, tagRecapture, tagRecaptureType, lakeID)
    original_3 <- oldData %>%
      filter(tagApply %in% rightSpeciesWrongLake$tagRecapture) %>%
      select('applyLake' = lakeID, tagApply)
    rswl <- left_join(rightSpeciesWrongLake, original_3, by = c("tagRecapture" = "tagApply"))
    
    # Throw errors
    if(nrow(neverApplied) > 0){
      if(fn == F){
        stop(paste0("You are reporting tag values that have never been applied before, in any lake or species:\n\n",
                    paste0(capture.output(neverApplied), collapse = "\n"),
                    "\n\nIf you're sure you want to report these values, use ", 
                    deparse(substitute(fn)), "."))
      }
    }
    if(nrow(rlws) > 0){
      if(fs == F){
        stop(paste0("You are reporting ", re, " values that have been applied before in this lake, but in a different species:\n\n",
                    paste0(capture.output(rlws), collapse = "\n"),
                    "\n\nIf you're sure that this is correct, use ",
                    deparse(substitute(fs)), "."))
      }
    }
    if(nrow(rswl) > 0){
      if(fl == F){
        stop(paste0("You are reporting ", re, " values that have been applied before to this species, but in a different lake:\n\n",
                    paste0(capture.output(rswl), collapse = "\n"),
                    "\n\nIf you're sure that this is correct, use ",
                    deparse(substitute(fl)), "."))
      }
    }
    if(nrow(wswl) > 0){
      if(fs == F | fl == F){ # both have to be TRUE to let this proceed
        stop(paste0("You are reporting ", re, " values that have been applied before in a different species and different lake:\n\n",
                    paste0(capture.output(wswl), collapse = "\n"),
                    "\n\nIf you're sure this is correct, use both ",
                    deparse(substitute(fs)), " and ",
                    deparse(substitute(fl)), "."))
      }
    }
  }
  message("Recaptured tags look good!")
}

# checkClipRecapture ------------------------------------------------------
# Checks that clips recaptured have been previously applied to the same species in the same lake (and same type of clip)
checkClipRecapture <- function(new, db, is, f){
  # Check inputs
  assertDataFrame(new, col.names = "unique")
  assertDataFrame(db, col.names = "unique")
  assertDataFrame(is, col.names = "unique")
  assertSubset(c("clipRecapture", "fishID", "otu"), 
               choices = names(new))
  assertSubset(c("clipApply", "fishID", "otu"), 
               choices = names(db))
  assertSubset(c("clipApply", "fishID", "otu"), 
               choices = names(is))
  assertFlag(f)
  
  # Get old clip/species/lake apply combos
  previous <- db %>%
    filter(!is.na(clipApply)) %>%
    mutate(lakeID = word(fishID, 1, 1, sep = "_")) %>%
    select(lakeID, otu, clipApply)%>%
    bind_rows(is %>%
                filter(!is.na(clipApply)) %>%
                mutate(lakeID = word(fishID, 1, 1, sep = "_")) %>%
                select(lakeID, otu, clipApply)) %>%
    distinct() %>%
    mutate(combo = paste(lakeID, otu, clipApply, sep = "_"))
  
  # Get new clip/species recapture combos
  new <- new %>%
    filter(!is.na(clipRecapture)) %>%
    mutate(lakeID = word(fishID, 1, 1, sep = "_")) %>%
    select(fishID, lakeID, otu, clipRecapture) %>%
    distinct() %>%
    mutate(combo = paste(lakeID, otu, clipRecapture, sep = "_"))
  
  # Check whether any of the recaptures have no precedent (same clip type in this lake/species)
  if(nrow(new) > 0){
    problemRows <- new %>%
      filter(!combo %in% previous$combo) %>%
      select(fishID, otu, clipRecapture)
    
    if(nrow(problemRows) > 0){
      if(f == F){
        stop(paste0("You are reporting clipRecapture values that haven't been previously reported in the same lake and fish species:\n\n",
                    paste0(capture.output(problemRows), sep = "\n"),
                    "\n\nDouble-check your clip type, species, and lake information. If you're sure you want to report these clipRecapture values, use the force_clipRecapture argument."))
      }
    }
  }
}

# checkTagFormats ---------------------------------------------------------
checkTagFormats <- function(new, fp = force_pitFormat, ff = force_floyFormat){
  # Check inputs
  assertDataFrame(new, col.names = "unique")
  assertSubset(c("pitApply", "pitRecapture", "floyApply", "floyRecapture"),
               choices = names(new))
  assertFlag(fp)
  assertFlag(ff)
  
  # Check tag formats
  ## Pit
  pitPattern <- "" # XXX NEED TO WRITE THIS! See GH issue #150
  pitProblems <- new %>%
    select(fishID, "apply" = pitApply, "recapture" = pitRecapture) %>%
    pivot_longer(cols = c("apply", "recapture"), names_to = "type", values_to = "tag") %>%
    filter(!is.na(tag),
           !str_detect(tag, pattern = pitPattern))
  
  if(nrow(pitProblems) > 0){
    stop(paste0("Some pit tag numbers don't match any expected pit tag formats:\n\n",
                paste0(capture.output(pitProblems), collapse = "\n"), 
                "\n\nIf you're sure you want to enter these tags, use ", 
                deparse(substitute(fp)), "."))
  }
  
  ## Floy
  floyPattern <- "" # XXX NEED TO WRITE THIS! See GH issue #150
  floyProblems <- new %>%
    select(fishID, "apply" = floyApply, "recapture" = floyRecapture) %>%
    pivot_longer(cols = c("apply", "recapture"), names_to = "type", values_to = "tag") %>%
    filter(!is.na(tag),
           !str_detect(tag, pattern = floyPattern))
  
  if(nrow(floyProblems) > 0){
    stop(paste0("Some floy tag numbers don't match any expected floy tag formats:\n\n",
                paste0(capture.output(floyProblems), collapse = "\n"), 
                "\n\nIf you're sure you want to enter these tags, use ", 
                deparse(substitute(ff)), "."))
  }
}

# vonB --------------------------------------------------------------------
vonB <- function(df){
  # Check inputs
  assertDataFrame(df, col.names = "unique")
  assertSubset(c("fishIDApply", "fishID", "fishLength", "lengthApply", 
                 "Linf", "K", "t0"), choices = names(df))
  
  # Extract the dateApply and dateRecap
  df <- df %>%
    mutate(dateApply = word(fishIDApply, 3, 3, sep = "_"),
           dateRecap = word(fishID, 3, 3, sep = "_")) %>%
    mutate(across(.cols = c("dateApply", "dateRecap"), .fns = lubridate::ymd),
           across(.cols = c("fishLength", "lengthApply"), .fns = as.numeric))
  
  # Compute age and growth time
  df <- df %>%
    mutate(growthTime = as.numeric(dateRecap - dateApply)/365, # convert from days to years, since t0 has units of years
           age1 = log(1 - c(fishLength/Linf))/-K + t0,
           age2 = as.numeric(age1 + growthTime),
           vonB_expectedLength = Linf*(1-exp(-K*(age2-t0)))) %>%
    select(-c("dateApply", "dateRecap", "growthTime", "age1", "age2"))
  
  # Return the whole data frame, with expectedLength added
  return(df)
}

# vonBCheck ---------------------------------------------------------------
vonBCheck <- function(new, db, is, f){
  # Check inputs
  assertDataFrame(new, col.names = "unique")
  assertDataFrame(db, col.names = "unique")
  assertDataFrame(is, col.names = "unique")
  assertSubset(c("fishID", "otu", "fishLength", "pitRecapture", "floyRecapture", "entryFile"), choices = names(new))
  assertSubset(c("fishID", "otu", "fishLength", "pitApply", "floyApply"),
               choices = names(db))
  assertSubset(c("fishID", "otu", "fishLength", "pitApply", "floyApply"), 
               choices = names(is))
  assertFlag(f)
  
  recaptured <- new %>% # XXX go back to the other functions and add the entryFile column
    select(fishID, otu, fishLength, "pit" = pitRecapture, "floy" = floyRecapture, entryFile) %>%
    pivot_longer(cols = c("pit", "floy"), names_to = "tagRecaptureType", 
                 values_to = "tagRecapture") %>%
    filter(!is.na(tagRecapture))
  
  applied <- bind_rows(tochar(db), tochar(is)) %>%
    filter(!is.na(fishLength)) %>%
    select("fishIDApply" = fishID, "otuApply" = otu, # we're not filtering on otu, but we'll let people see it in the error message for extra info.
           "lengthApply" = fishLength, "pit" = pitApply, "floy" = floyApply) %>%
    pivot_longer(cols = c("pit", "floy"), names_to = "tagApplyType",
                 values_to = "tagApply") %>%
    filter(!is.na(tagApply)) %>%
    # If multiple applies, take the most recent one. This is a coarse check.
    arrange(lubridate::ymd(word(fishIDApply, 3, 3, sep = "_"))) %>%
    group_by(tagApply) %>%
    slice_tail() %>% # most recent apply
    ungroup()
  
  # Pair up recaptured tags with applied tags
  paired <- left_join(recaptured, applied, by = c("tagRecapture" = "tagApply")) %>%
    filter(!is.na(fishIDApply))
  
  # Apply vonB equation, with different parameters depending on whether the fish are from Long Lake or not.
  paired <- paired %>%
    mutate(lakeID = word(fishID, 1, 1, sep = "_"),
           Linf = ifelse(lakeID %in% c("WL", "EL", "FE"), 376.909, 550),
           K = ifelse(lakeID %in% c("WL", "EL", "FE"), 0.32, 0.19),
           t0 = ifelse(lakeID %in% c("WL", "EL", "FE"), -0.599, -0.024)) %>%
    vonB() # defined above
  
  # Separate out any fish that deviate too much from the prediction
  problems <- paired %>%
    filter(abs(vonB_expectedLength - fishLength) > (0.1*fishLength)) %>%
    select(fishID, tagRecapture, fishLength, lengthApply, vonB_expectedLength, 
           otu, otuApply) %>%
    as.data.frame()
  
  # Throw an error if there are any problematic fish.
  if(nrow(problems) > 0){
    if(f == F){
      stop(paste0("The following recaptured fish have lengths that would not be expected based on von Bertalanffy growth curves:\n\n",
                  paste0(capture.output(problems), collapse = "\n"),
                  "\n\nIs there a problem with the tag data or length data? If you're sure you want to enter this data, use ", deparse(substitute(f)), "."))
    }
  }
}


# checkLakeSpecies --------------------------------------------------------
# Has this fish species ever been captured in this lake before?
checkLakeSpecies <- function(new, db, is, f){
  # Check inputs
  assertDataFrame(new, col.names = "unique")
  assertDataFrame(db, col.names = "unique")
  assertDataFrame(is, col.names = "unique")
  assertFlag(f)
  assertSubset(c("fishID", "otu", "entryFile"), names(new))
  assertSubset(c("fishID", "otu"), names(db))
  assertSubset(c("fishID", "otu"), names(is))
  
  # new lake/species combos
  newCombos <- new %>%
    mutate(lakeID = word(fishID, 1, 1, sep = "_")) %>%
    select(lakeID, otu, entryFile) %>%
    distinct()
  
  # old lake/species combos
  oldCombos <- bind_rows(tochar(db), tochar(is)) %>%
    mutate(lakeID = word(fishID, 1, 1, sep = "_")) %>%
    select(lakeID, otu) %>%
    distinct()
  
  # lake/species combos that have never appeared before
  problemCombos <- newCombos %>%
    filter(!paste(lakeID, otu, sep = "_") %in% 
             paste(oldCombos$lakeID, oldCombos$otu, sep = "_"))
  
  if(nrow(problemCombos) > 0){
    if(f == FALSE){
      stop(paste0("You're reporting some species in lakes where they have never been reported before:\n\n",
                  paste0(capture.output(problemCombos), collapse = "\n"),
                  "\n\nIf you're sure these species and lakes are correct, use ", deparse(substitute(f)), "."))
    }
  }
}

# retiredProjectIDsCheck --------------------------------------------------
retiredProjectIDsCheck <- function(new, f = force_retiredProjectID){
  assertFlag(f)
  assertChoice("projectID", choices = names(new))
  assertChoice("entryFile", choices = names(new))
  
  # Check whether any of the newly-added projectIDs should have been retired
  problemRows <- new %>%
    filter(projectID %in% retiredProjectIDs) %>%
    select(projectID, entryFile) %>%
    distinct()
  
  # If there are retired projectID's, throw error and print the retired projectIDs
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      stop(paste0("Some of the projectID's you're trying to enter are from old projects and should not be associated with incoming data. Here are the projectID's, and the files they come from: \n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\n If you are sure that you want to use these projectID's, use force_retiredProjectIDs."))
    }
  }
}


