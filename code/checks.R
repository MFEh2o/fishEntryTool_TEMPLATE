# Check functions for the fish entry tool
# Created by Kaija Gahm on 21 May 2021

expectedEffortUnits <- c("angler_hours", "electrofishing_hours", "meters", "trap_hours", 
                         NA, "seine_pulls")

# checkHeader function ----------------------------------------------------
checkHeader <- function(h = header){
  requiredElectro <- h[!names(h) == "comments"]
  requiredNonElectro <- h[!names(h) %in% c("comments", "distanceShocked")]
  
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
}

# checkTagApply -----------------------------------------------------------
checkTagApply <- function(x){
  assertDataFrame(x)
  assertSubset(c("tagApply", "tagApplyType"), names(x))
  
  # Check that all values in tagApplyType are either "pit", "floy" or NA
  if(!all(x$tagApplyType %in% c(NA, "pit", "floy"))){
    problemRows <- x %>%
      filter(!tagApplyType %in% c(NA, "pit", "floy")) %>%
      select(fishID, entryFile, tagApplyType) %>%
      distinct()
    stop(paste0("tagApplyType must be 'pit', 'floy', or NA. The offending rows are:\n\n",
                paste0(capture.output(problemRows), collapse = "\n"),
                "\n\nYou must correct the value on the sample sheet in order for the entry tool to run."))
  }
  
  # Check that all rows that have a tag value also have a tag type
  missingType <- x %>%
    filter(is.na(tagApplyType), !is.na(tagApply))
  if(nrow(missingType) > 0){
    stop(paste0("At least one fish has a tag number but no tagApplyType. The offending rows are:\n\n",
                paste0(capture.output(missingType), collapse = "\n"),
                "\n\nYou must provide a tag type in order for the entry tool to run."))
  }
  
  # Check that all rows that have a tag type also have a tag value
  missingTag <- x %>%
    filter(is.na(tagApply), !is.na(tagApplyType))
  if(nrow(missingTag) > 0){
    stop(paste0("At least one fish has a tagApplyType but no tag number. The offending rows are:\n\n",
                paste0(capture.output(missingTag), collapse = "\n"),
                "\n\nYou must provide a tag number in order for the entry tool to run. If the tag was unreadable or you didn't record the tag number, please write 'unknown'."))
  }
}

# checkTagRecap -----------------------------------------------------------
checkTagRecap <- function(x){
  assertDataFrame(x)
  assertSubset(c("tagRecapture", "tagRecaptureType"), names(x))
  
  # Check that all values in tagRecapturetype are either "pit", "floy" or NA
  if(!all(x$tagRecaptureType %in% c(NA, "pit", "floy"))){
    problemRows <- x %>%
      filter(!tagRecaptureType %in% c(NA, "pit", "floy")) %>%
      select(fishID, entryFile, tagRecaptureType) %>%
      distinct()
    stop(paste0("tagRecaptureType must be 'pit', 'floy', or NA. The offending rows are:\n\n",
                paste0(capture.output(problemRows), collapse = "\n"),
                "\n\nYou must correct the value on the sample sheet in order for the entry tool to run."))
  }
  
  # Check that all rows that have a tag value also have a tag type
  missingType <- x %>%
    filter(is.na(tagRecaptureType), !is.na(tagRecapture))
  if(nrow(missingType) > 0){
    stop(paste0("At least one fish has a tag number but no tagRecaptureType. The offending rows are:\n\n",
                paste0(capture.output(missingType), collapse = "\n"),
                "\n\nYou must provide a tag type in order for the entry tool to run."))
  }
  
  # Check that all rows that have a tag type also have a tag value
  missingTag <- x %>%
    filter(is.na(tagRecapture), !is.na(tagRecaptureType))
  if(nrow(missingTag) > 0){
    stop(paste0("At least one fish has a tagRecaptureType but no tag number. The offending rows are:\n\n",
                paste0(capture.output(missingTag), collapse = "\n"),
                "\n\nYou must provide a tag number in order for the entry tool to run. If the tag was unreadable or you didn't record the tag number, please write 'unknown'."))
  }
}

# Checks at the end against the in-season database and full database ----------

# checkForNew -------------------------------------------------------------
# Check function that can be used to check if you're introducing any new values
checkForNew <- function(colName, tc, db, is, f = NULL){
  # Get values previously used in the database
  dbVals <- db %>% pull({{colName}}) %>% unique()
  
  # Separate out the new data
  newData <- is %>%
    filter(entryFile %in% tc)
  
  # Get values previously used in the in-season table (but not including the current entry)
  isVals <- is %>% 
    filter(!entryFile %in% tc) %>%
    pull({{colName}}) %>% unique()
  
  # Put them together
  previousVals <- c(dbVals, isVals)
  
  # Find problem rows in newData (i.e. those that have new vals)
  problemRows <- newData %>%
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
checkForRepeats <- function(colName, tc, db, is, na.ok = F, f = NULL){
  # Get values previously used in the database
  dbVals <- db %>% pull({{colName}}) %>% unique()
  
  # Get values previously used in the in-season table
  isVals <- is %>% 
    filter(!entryFile %in% tc) %>%
    pull({{colName}}) %>% unique()
  
  # Get new data
  newData <- is %>%
    filter(entryFile %in% tc)
  
  # Put them together
  previousVals <- c(dbVals, isVals)
  
  # Find problem rows in newData (i.e. those that match/repeat a previous value)
  problemRows <- newData %>%
    {if(na.ok == T) filter(., !is.na(.data[[colName]])) else .} %>%
    filter(.data[[colName]] %in% previousVals) %>%
    select({{colName}}, entryFile) %>%
    distinct()
  
  # If there are repeat values, throw an error and print the repeat values
  if(nrow(problemRows) > 0){
    if(is.null(f)){
      stop(paste0("You are attempting to add ", colName, " values that are already in either the database or the in-season file. Here are the repeat values, and the sample sheets they come from:\n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\nFor the fish entry tool to work, all of your ", colName, " values must be unique."))
    }else{
      stop(paste0("You are attempting to add ", colName, " values that are already in either the database or the in-season file. Here are the repeat values, and the sample sheets they come from:\n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\nIf you are sure you're entering the right values, you can use ", deparse(substitute(f)), " to bypass this message and enter the values anyway. But BEWARE! Tell the database manager. The database may not compile correctly if it includes repeat values."))
    }
  }
}

# checkRangeLimits --------------------------------------------------------
# Default is to define problem values as anything <= minVal and >= maxVal. If allowMinEqual = T, then only < minVal is a problem; if allowMaxEqual = T, then only > maxVal is a problem.
checkRangeLimits <- function(colName, is, tc, f, minVal, maxVal, 
                             allowMinEqual = F, allowMaxEqual = F){
  # Get new data
  newData <- is %>%
    filter(entryFile %in% tc)
  
  # Isolate problem rows
  problemRows <- newData %>%
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
checkDateTimes <- function(is, tc){
  # Get new data
  newData <- is %>%
    filter(entryFile %in% tc)
  
  # dateSet must be the same or earlier than dateSample
  problemRowsDate <- newData %>%
    filter(as.Date(dateSample) < as.Date(dateSet))
  
  # dateTimeSet must be the same or earlier than dateTimeSample
  problemRowsDateTime <- newData %>%
    filter(strptime(dateTimeSample, 
                    format = "%m/%d%Y %H:%M:%S") < 
             strptime(dateTimeSet, 
                      format = "%m/%d%Y %H:%M:%S"))
  
  # Error message for dateSet/dateSample
  if(nrow(problemRowsDate) > 0){
    stop("In the following sample sheets, the dateSample is earlier than the dateSet:\n\n", paste0(capture.output(problemRowsDate), collapse = "\n"))
  }
  
  # Error message for dateTimeSet/dateTimeSample
  if(nrow(problemRowsDateTime) > 0){
    stop("In the following sample sheets, the dateTimeSample is earlier than the dateTimeSet:\n\n", paste0(capture.output(problemRowsDateTime), collapse = "\n"))
  }
}

# checkDuplicateFishIDs --------------------------------------------------
# Can't use checkForRepeats on this one because we care about *all* the rows, not just comparing new to old.
checkDuplicateFishIDs <- function(is, db, tc){
  # Get db fishIDs
  dbIDs <- db %>%
    pull(fishID)
  
  # Get previously-entered in-season fishIDs
  oldIS <- is %>%
    filter(!entryFile %in% tc) %>%
    pull(fishID)
  
  # Get newly-entered fishIDs
  newIS <- is %>%
    filter(entryFile %in% tc) %>%
    pull(fishID)
  
  # Check whether there are internal duplicates in the data you're entering
  if(any(duplicated(newIS))){
    problemRows <- is %>%
      filter(entryFile %in% tc) %>%
      group_by(fishID) %>%
      filter(n() > 1) %>%
      arrange(fishID) %>%
      select(fishID, entryFile)
    stop(paste0("There are duplicate fishID's in the data you're trying to enter right now. The problem rows are:\n\n",
                paste0(capture.output(problemRows), collapse = "\n")))
  }
  
  # Check whether any of the fishID's that you're trying to enter already appear in the database
  if(any(newIS %in% dbIDs)){
    problemRows <- is %>%
      filter(entryFile %in% tc) %>%
      filter(fishID %in% dbIDs) %>%
      select(fishID, entryFile)
    stop(paste0("You're trying to enter fishID's that are already present in the FISH_INFO database table. The problem rows are:\n\n",
                paste0(capture.output(problemRows), collapse = "\n")))
  }
  
  # Check whether any of the fishID's that you're trying to enter already appear in the in-season database
  if(any(newIS %in% oldIS)){
    problemRows <- is %>%
      filter(entryFile %in% tc) %>%
      filter(fishID %in% oldIS) %>%
      select(fishID, entryFile)
    stop(paste0("You're trying to enter fishID's that are already present in the FISH_INFO in-season database. The problem rows are:\n\n",
                paste0(capture.output(problemRows), collapse = "\n")))
  }
}

# checkFishLengthWeight ---------------------------------------------------
checkFishLengthWeight <- function(db, tc, is, fl, fw){
  # Get only the newly-entered fish
  new <- is %>%
    filter(entryFile %in% tc)
  
  # Throw an error for fish weights or lengths that are 0 or negative.
  problemRows <- new %>%
    filter(fishLength <= 0|fishWeight <= 0) %>%
    select(fishID, fishLength, fishWeight, entryFile)
  if(nrow(problemRows) > 0){
    stop(paste0("You are trying to enter 0 or negative lengths or weights for the following fish:\n\n",
                paste0(capture.output(problemRows), collapse = "\n"),
                "\n\nPlease double-check your lengths and weights. If you don't have length/weight information, leave these values as blank or NA, not 0."))
  }
  
  # Get a list of all the unique species being entered
  species <- new %>%
    pull(otu) %>%
    unique()
  
  # Check each species for length
  purrr::walk(.x = species, .f = function(x, ...){
    # Get all fish of this species in the database that have length measurements
    curDB <- db %>% filter(otu == x, !is.na(fishLength), fishLength > 0)
    curNEW <- new %>% filter(otu == x, !is.na(fishLength)) %>%
      select(fishID, otu, fishLength, fishWeight)
    if(nrow(curDB) < 15){
      warning(paste0("We have <15 observations for length of species ", x, ", so we won't run an automated fishLength check. Be sure to double-check the lengths you've entered."))
    }else{
      mn <- mean(curDB$fishLength, na.rm = T)
      sd <- sd(curDB$fishLength, na.rm = T)
      tooLong <- curNEW %>%
        filter(as.numeric(fishLength) > mn + 3*sd)
      tooShort <- curNEW %>%
        filter(as.numeric(fishLength) < mn - 3*sd)
      if(nrow(tooLong) > 0){
        if(fl == FALSE){
          stop(paste0("Found some fish that are longer than 3 sd above the mean for their species, ", x, ". They are:\n\n",
                      paste0(capture.output(tooLong), collapse = "\n")))
        }
      }
      if(nrow(tooShort) > 0){
        if(fl == FALSE){
          stop(paste0("Found some fish that are shorter than 3 sd below the mean for their species, ", x, ". They are:\n\n",
                      paste0(capture.output(tooShort), collapse = "\n")))
        }
      }
    }
    
    # Now, limit the database subset down to just the ones that also have weight measurements.
    curDB_weight <- curDB %>%
      filter(!is.na(fishWeight), fishWeight > 0)
    if(nrow(curDB_weight) < 15){
      warning(paste0("We have <15 observations for weight of species ", x, ", so we won't run an automated length-weight regression. Be sure to double-check the weights you've entered."))
    }else{
      curDB_weight$logWeight <- log(curDB_weight$fishWeight)
      curDB_weight$logLength <- log(curDB_weight$fishLength)
      lwreg <- lm(logWeight ~ logLength, data = curDB_weight)
      # Predict high and low weight bounds for each individual
      preds <- predict(lwreg, newdata = 
                         data.frame(logLength = 
                                      log(as.numeric(curNEW$fishLength))),
                       interval = "prediction",
                       se.fit = TRUE, level = 0.99)
      curNEW$predsLow <- exp(preds$fit[,2])
      curNEW$predsHigh <- exp(preds$fit[,3])
      # Are any of the newly-entered fish weights < predsLow or > predsHigh...
      tooHeavy <- curNEW %>%
        filter(as.numeric(fishWeight) > predsHigh)
      tooLight <- curNEW %>%
        filter(as.numeric(fishWeight) < predsLow)
      
      if(nrow(tooHeavy) > 0){
        if(fw == FALSE){
          stop(paste0("You report fishWeight heavier than the prediction based on a length-weight regression from our database for ", x, ". Here are the problematic rows:\n\n",
                      paste0(capture.output(tooHeavy), collapse = "\n")))
        }
      }
      if(nrow(tooLight) > 0){
        if(fw == FALSE){
          stop(paste0("You report fishWeight lighter than the prediction based on a length-weight regression from our database for ", x, ". Here are the problematic rows:\n\n",
                      paste0(capture.output(tooLight), collapse = "\n")))
        }
      }
    }
  })
}

# clipTagLakeCheck ------------------------------------------------------------
tagLakeCheck <- function(tc, db, is, f, type){
  # Make sure the 'type' argument is either pit or floy or clip
  assertSubset(type, choices = c("pit", "floy", "clip"))
  
  ap <- paste0(type, "Apply")
  re <- paste0(type, "Recapture")
  
  # Get new data
  newData <- is[is$entryFile %in% tc & !is.na(is[,re]),] %>%
    mutate(lakeID = word(sampleID, 1, 1, sep = "_"))
  
  # Loop through the lakes and check for any *Recapture values that were not previously applied in this lake
  if(nrow(newData) > 0){
    for(i in 1:length(unique(newData$lakeID))){
      l <- unique(newData$lakeID)[i]
      
      # Get all new data from this lake
      lakeSubsetNew <- newData %>%
        filter(lakeID == l)
      
      # Get all old data from this lake (that had tags/clips applied)
      lakeSubsetOld <- db %>% 
        filter(!is.na(.data[[re]])) %>%
        mutate(lakeID = word(sampleID, 1, 1, sep = "_")) %>%
        filter(lakeID == l)
      
      # Find any problems (*Recapture values that didn't previously appear as *Apply values in this lake)
      problemRows <- lakeSubsetNew %>%
        filter(!.data[[re]] %in% lakeSubsetOld[,ap]) %>%
        select(fishID, re)
      
      # If there are problem rows, throw an error.
      if(nrow(problemRows) > 0){
        if(f == F){
          stop(paste0("You are reporting ", re, " values that don't match past ", ap, " values from this lake. The problematic rows are:\n\n",
                      paste0(capture.output(problemRows), collapse = "\n"),
                      "\n\nIf you're sure you want to report these values, use ",
                      deparse(substitute(f)), "."))
        }
      }
    }
  }
}

# # -tagRecap should have an apply in the database from that lake at some point in the past
# # the absence of a previous apply might mean it was entered wrong
# # make this the tag apply -> change data sheet too!?!?!
# tagsRecapped = curNEW$tagRecapture[!is.na(curNEW$tagRecapture)]
# tagsRecapped = curNEW$tagRecapture[curNEW$tagRecapture! = ""]
# if(length(tagsRecapped)>0){
#   if(any(!tagsRecapped%in%c(fishInfoDB$tagApply, fishInfoIS$tagApply))){
#     print(tagsRecapped[!tagsRecapped%in%c(fishInfoDB$tagApply, fishInfoIS$tagApply)])
#     stop("You are attempting to enter a tagRecapture that was not ever recorded as a tagApply in the database or the in-season database.")
#   }
# }
# 
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