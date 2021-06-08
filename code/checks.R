# Check functions for the fish entry tool
# Created by Kaija Gahm on 21 May 2021

expectedEffortUnits <- c("angler_hours", "electrofishing_hours", "meters", "trap_hours", 
                         NA, "seine_pulls")

# checkHeader function ----------------------------------------------------
checkHeader <- function(h = header){
  # Define required fields for electrofishing vs. not.
  requiredElectro <- h[!names(h) == "comments"]
  requiredNonElectro <- h[!names(h) %in% c("comments", "distanceShocked")]
  
  # Check that all required fields are present (different requirements for electrofishing vs. not)
  # Here, we define missing as either blank ("") or NA (is.na())
  if(h$gear == "BE"){
    if(any(is.na(requiredElectro)|requiredElectro == "")){
      stop(paste0("Required header information is incomplete in ", file, 
                  ". You're missing: ", 
                  paste(names(requiredElectro[requiredElectro == ""|is.na(requiredElectro)]), 
                        collapse = ", ")))
    }
  }else{
    if(any(is.na(requiredNonElectro)|requiredNonElectro == "")){
      stop(paste0("Required header information is incomplete in ", file,
                  ". You're missing: ",
                  paste(names(requiredNonElectro[requiredNonElectro == ""|is.na(requiredNonElectro)]),
                        collapse = ", ")))
    }
  }
  
  # Check that effortUnits is in one of the allowable formats
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
  assertCharacter(as.character(header$dateTimeSet), pattern = pat_dateTime)
  assertCharacter(as.character(header$dateTimeSample), pattern = pat_dateTime)
  assertCharacter(as.character(header$dateSet), pattern = pat_date)
  assertCharacter(as.character(header$dateSample), pattern = pat_date)
}

# vonBertalanffy ----------------------------------------------------------
vonB <- function(Linf, K, t0){
  
}

# Find all recap tags that *do* match an apply
# For each that has a fish length:
# If the fish was originally tagged this year and length was measured when it was originally tagged
# (There seems to be a distinction between fish tagged this year vs. last year?) But I can't see what the difference is.
# If EL, FE, or WL, set vonB parameters to something
# Otherwise, set them to something else (from source)
# Run vonB


# # - tagRecap accidently matches an old apply
# # check lengths when matching recap to apply
# # use vonB for Long Lake and or Wisconsin for this...
# if(length(tagsRecapped)>0){
#   for(j in 1:length(tagsRecapped)){
#     # if we measured a length
#     if(!is.na(curNEW$fishLength[curNEW$tagRecapture == tagsRecapped[j]])){
#       #if tagged this year
#       if(tagsRecapped[j]%in%fishInfoIS$tagApply){
#         #if we measured length when tag was applied
#         if(any(!is.na(fishInfoIS$fishLength[fishInfoIS$tagApply == tagsRecapped[j]]), na.rm = TRUE)){
#           #if we are looking at Long Lake, use our vonB
#           if(lakeID%in%c("EL", "FE", "WL")){
#             Linf = 376.909
#             K = 0.32
#             t0 = -0.599
#             tagLength = fishInfoIS$fishLength[fishInfoIS$tagApply == tagsRecapped[j]]
#             tagDate = fishSamplesIS$dateSample[fishSamplesIS$sampleID == fishInfoIS$sampleID[fishInfoIS$tagApply == tagsRecapped[j]]]
#             growthTime = as.Date(dateSample)-as.Date(tagDate)
#             age1 = log(1-tagLength/Linf)/-K+t0
#             age2 = age1+growthTime
#             expL = Linf*(1-exp(-K*(age2-t0)))
#             obsL = curNEW$fishLength[curNEW$tagRecapture == tagsRecapped[j]]
#             #if the fish we recapped is shorter or longer than fish tagged, based on vonB
#             if(abs(expL-obsL)>(0.1*obsL)){
#               stop(paste("The fish with tagRecapture:", tagsRecapped[j], "is a length that would not be expected. Is there a problem with the tag data or length data?", sep = " "))
#             }
#             # use general vonB from Beamsderfer & North 1995
#           }else{
#             Linf = 550
#             K = 0.19
#             t0 = -0.024
#             tagLength = fishInfoIS$fishLength[fishInfoIS$tagApply == tagsRecapped[j]]
#             tagDate = fishSamplesIS$dateSample[fishSamplesIS$sampleID == fishInfoIS$sampleID[fishInfoIS$tagApply == tagsRecapped[j]]]
#             growthTime = (as.Date(dateSample)-as.Date(tagDate))/365 # difference in days, so convert to years for vonB
#             age1 = log(1-tagLength/Linf)/-K+t0
#             age2 = age1+growthTime
#             expL = Linf*(1-exp(-K*(age2-t0)))
#             obsL = curNEW$fishLength[curNEW$tagRecapture == tagsRecapped[j]]
#             #if the fish we recapped is shorter or longer than fish tagged, based on vonB
#             if(abs(expL-obsL)>(0.1*obsL)){
#               stop(paste("The fish with tagRecapture:", tagsRecapped[j], "is a length that would not be expected. Is there a problem with the tag data or length data?", sep = " "))
#             }
#           }   
#         }
#       }
#       # if tagged in a previous year
#       if(tagsRecapped[j]%in%fishInfoDB$tagApply){
#         #if we measured length when tag was applied
#         if(any(!is.na(fishInfoDB$fishLength[fishInfoDB$tagApply == tagsRecapped[j]]), na.rm = TRUE)){
#           #if we are looking at Long Lake, use our vonB
#           if(lakeID%in%c("EL", "FE", "WL")){
#             Linf = 376.909
#             K = 0.32
#             t0 = -0.599
#             tagLength = fishInfoDB$fishLength[fishInfoDB$tagApply == tagsRecapped[j]]
#             tagDate = fishSamplesDB$dateSample[fishSamplesDB$sampleID == fishInfoDB$sampleID[fishInfoDB$tagApply == tagsRecapped[j]]]
#             growthTime = as.Date(dateSample)-as.Date(tagDate)
#             age1 = log(1-tagLength/Linf)/-K+t0
#             age2 = age1+growthTime
#             expL = Linf*(1-exp(-K*(age2-t0)))
#             obsL = curNEW$fishLength[curNEW$tagRecapture == tagsRecapped[j]]
#             #if the fish we recapped is shorter or longer than fish tagged, based on vonB
#             if(abs(expL-obsL)>(0.1*obsL)){
#               stop(paste("The fish with tagRecapture:", tagsRecapped[j], "is a length that would not be expected. Is there a problem with the tag data or length data?", sep = " "))
#             }
#             # use general vonB from Beamsderfer & North 1995
#           }else{
#             Linf = 550
#             K = 0.19
#             t0 = -0.024
#             tagLength = fishInfoDB$fishLength[fishInfoDB$tagApply == tagsRecapped[j]]
#             tagDate = fishSamplesDB$dateSample[fishSamplesDB$sampleID == fishInfoDB$sampleID[fishInfoDB$tagApply == tagsRecapped[j]]]
#             growthTime = (as.Date(dateSample)-as.Date(tagDate))/365 # difference in days, so convert to years for vonB
#             age1 = log(1-tagLength/Linf)/-K+t0
#             age2 = age1+growthTime
#             expL = Linf*(1-exp(-K*(age2-t0)))
#             obsL = curNEW$fishLength[curNEW$tagRecapture == tagsRecapped[j]]
#             #if the fish we recapped is shorter or longer than fish tagged, based on vonB
#             if(abs(expL-obsL)>(0.1*obsL)){
#               stop(paste("The fish with tagRecapture:", tagsRecapped[j], "is a length that would not be expected. Is there a problem with the tag data or length data?", sep = " "))
#             }
#           }   
#         }
#       }
#     }
#   }
# }

# Checks at the end against the in-season database and full database ----------

# checkForNew -------------------------------------------------------------
# Check function that can be used to check if you're introducing any new values
checkForNew <- function(colName, new, db, is, f = NULL){
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
                  "\n\nIf you are sure you're entering the right values, you can use ", deparse(substitute(f)), " to bypass this message and enter the values anyway. But BEWARE! Tell the database manager. The database may not compile correctly if it includes repeat values."))
    }
  }
}

# checkRangeLimits --------------------------------------------------------
# Default is to define problem values as anything <= minVal and >= maxVal. If allowMinEqual = T, then only < minVal is a problem; if allowMaxEqual = T, then only > maxVal is a problem.
checkRangeLimits <- function(colName, new, f, minVal, maxVal, 
                             allowMinEqual = F, allowMaxEqual = F){
  # Isolate problem rows
  problemRows <- new %>%
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
  
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      stop(paste("Some ", colName, " values are outside the normal range of ", minVal, " to ", maxVal, ". The offending values are:\n\n",
                 paste0(capture.output(problemRows), collapse = "\n"),
                 "If you are sure that these values are correct, use the ", f, " argument."))
    }
  }
}

# checkDateTimes ----------------------------------------------------------
checkDateTimes <- function(new){
  # dateSet must be the same or earlier than dateSample
  problemRowsDate <- new %>%
    filter(as.Date(dateSample) < as.Date(dateSet))
  
  # Error message for dateSet/dateSample
  if(nrow(problemRowsDate) > 0){
    stop("In the following sample sheets, the dateSample is earlier than the dateSet:\n\n", paste0(capture.output(problemRowsDate), collapse = "\n"))
  }
  
  # dateTimeSet must be the same or earlier than dateTimeSample
  problemRowsDateTime <- new %>%
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
checkDuplicateFishIDs <- function(is, db){
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
checkFishLengthWeight <- function(new, db, is, fl, fw){
  new <- new %>% mutate(across(c("fishLength", "fishWeight"), as.numeric))
  db <- db %>% mutate(across(c("fishLength", "fishWeight"), as.numeric))
  is <- is %>% mutate(across(c("fishLength", "fishWeight"), as.numeric))
  
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
                      paste0(capture.output(tooLong), collapse = "\n")))
        }
      }
      
      # Throw error for too short
      if(nrow(tooShort) > 0){
        if(fl == FALSE){
          stop(paste0("Found some fish that are shorter than 3 sd below the mean for their species, ", x, ". They are:\n\n",
                      paste0(capture.output(tooShort), collapse = "\n")))
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
                      paste0(capture.output(tooHeavy), collapse = "\n")))
        }
      }
      
      # Throw error for light fish
      if(nrow(tooLight) > 0){
        if(fw == FALSE){
          stop(paste0("You report fishWeight lighter than the prediction based on a length-weight regression from our database for ", x, ". Here are the problematic rows:\n\n",
                      paste0(capture.output(tooLight), collapse = "\n")))
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
    rlws <- left_join(rightSpeciesWrongLake, original_3, by = c("tagRecapture" = "tagApply"))
    
    # Throw errors
    if(nrow(neverApplied) > 0){
      if(fn == F){
        stop(paste0("You are reporting ", re, " values that have never been applied before, in any lake or species:\n\n",
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
checkClipRecapture <- function(new = newFI, db = fishInfoDB, is = fishInfoIS, 
                               f = force_clipRecapture){
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
checkTagFormats <- function(new, fp, ff){
  # insert code here to check that pit and floy tags match appropriate regex
  # fp is going to be "force_pitFormat", and ff will be "force_floyFormat" (or something like that.)
}



# # Check that any recaptured fish that were previously tagged in the same lake have a reasonable size
# recaptured <- x %>%
#   filter(!is.na(tagRecapture))
# if(nrow(recaptured) > 0){
#   for(i in 1:nrow(recaptured)){
#     # fishID
#     fish <- recaptured[i, "fishID"]
#     # length at recapture
#     length <- recaptured[i, "fishLength"]
#     # tag number recaptured
#     tagNumber <- as.character(recaptured[i, "tagRecapture"])
#     # lake
#     lake <- word(recaptured[i, "fishID"], 1, 1, sep = "_")
#     
#     # If we measured a length at recapture
#     if(!is.na(length)){
#       # Get any instances of the same tag number being applied in the same lake
#       lengthApplied <- previous %>%
#         mutate(lakeID = word(fishID, 1, 1, sep = "_")) %>%
#         filter(lakeID == lake,
#                as.character(pitApply) == tagNumber|
#                  as.character(floyApply) == tagNumber) %>%
#         filter(!is.na(fishLength)) %>%
#         pull(fishLength)
#       if(length(lengthApplied) > 1){
#         warning(paste0("Tag ", tagNumber, " from fish ", fish, " has multiple previous apply records in this lake. Can't run vonB length check. Make a note to investigate the previous records for this fish."))
#       }
#     }
#   }
# }

