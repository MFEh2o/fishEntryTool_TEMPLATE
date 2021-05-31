# Check functions for the fish entry tool
# Created by Kaija Gahm on 21 May 2021

expectedClips <- c(NA, "LV", "LC", "LP", "UC", "RV", "1", "AF", "UNK")
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

# checkClips --------------------------------------------------------------
checkClips <- function(x){
  assertDataFrame(x)
  assertSubset(c("clipApply", "clipRecapture"), names(x))
  
  # Check that all values in clipApply and clipRecapture are included in the vector of expected clip values
  if(any(!c(x$clipApply, x$clipRecapture) %in% expectedClips)){
    problemRows <- x %>%
      filter(!(clipApply %in% expectedClips) | !(clipRecapture %in% expectedClips)) %>%
      select(fishID, entryFile, clipApply, clipRecapture) %>%
      distinct()
    stop(paste0("Found unexpected clipApply and/or clipRecapture values. The offending rows are:\n\n",
                paste0(capture.output(problemRows), collapse = "\n"),
                "\n\nYou must correct these clip values in order for the entry tool to run. Allowed clip values are: ", 
                paste(expectedClips, collapse = ", ")))
  }
}

# Checks at the end against the in-season database and full database ----------

# checkForNew -------------------------------------------------------------
# Check function that can be used to check if you're introducing any new values
checkForNew <- function(colName, db, is, hdf, f = NULL){
  # Get values previously used in the database
  dbVals <- db %>% pull({{colName}}) %>% unique()
  
  # Get values previously used in the in-season table
  isVals <- is %>% pull({{colName}}) %>% unique()
  
  # Put them together
  previousVals <- c(dbVals, isVals)
  
  # Find problem rows in hdf (i.e. those that have new vals)
  problemRows <- hdf %>%
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
                  "\n\nIf you are sure that these values are valid, use the", f, " argument."))
    }
  }
}

# repeatSampleIDsCheck -----------------------------------------------------
# I didn't bother writing a general function here, because sampleID's are the only thing we check for repeats.
repeatSampleIDsCheck <- function(fsdb, is, hdf){
  # Get sampleID's previously in the database (FISH_SAMPLES)
  dbSampleIDs <- fsdb %>% pull(sampleID) %>% unique()
  
  # Get sampleID's previously in the in-season FISH_SAMPLES file
  isSampleIDs <- is %>% pull(sampleID) %>% unique()
  
  # Put them together
  previousSampleIDs <- c(dbSampleIDs, isSampleIDs)
  
  # Find problem rows in hdf (i.e. that have repeat sampleID's)
  problemRows <- hdf %>%
    filter(sampleID %in% previousSampleIDs) %>%
    select(sampleID, entryFile) %>%
    distinct()
  
  # If there are repeat sampleIDs, throw error and print the repeat sampleIDs. No option to force_ these.
  if(nrow(problemRows) > 0){
    stop(paste0("You are attempting to add sampleID's that already exist in either the database FISH_SAMPLES table or the in-season FISH_SAMPLES file. Here are the sampleID's, and the files they come from: \n\n",
                paste0(capture.output(problemRows), collapse = "\n"),
                "\n\n These repeat sampleID's must be corrected before the fish entry tool can run."))
  }
}

# checkRangeLimits --------------------------------------------------------
# Default is to define problem values as anything <= minVal and >= maxVal. If allowMinEqual = T, then only < minVal is a problem; if allowMaxEqual = T, then only > maxVal is a problem.
checkRangeLimits <- function(colName, hdf, f, minVal, maxVal, 
                             allowMinEqual = F, allowMaxEqual = F){
  problemRows <- hdf %>%
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
checkDateTimes <- function(hdf){
  # dateSet must be the same or earlier than dateSample
  problemRowsDate <- hdf %>%
    filter(as.Date(dateSample) < as.Date(dateSet))
  
  # dateTimeSet must be the same or earlier than dateTimeSample
  problemRowsDateTime <- hdf %>%
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


# metadataIDCheck --------------------------------------------------------
metadataIDCheck <- function(fsdb, is, hdf, f = force_metadataID){
  # Get metadataID previously in the database
  dbMetadataID <- fsdb %>% pull(metadataID) %>% unique()
  
  # Get metadataID previously in the in-season FISH_SAMPLES file
  isMetadataID <- is %>% pull(metadataID) %>% unique()
  
  # Put them together
  previousMetadataID <- c(dbMetadataID, isMetadataID)
  
  # Find problem rows in hdf (i.e. that have new metadataID)
  problemRows <- hdf %>%
    filter(!metadataID %in% previousMetadataID) %>%
    select(metadataID, entryFile) %>%
    distinct()
  
  # If there are new metadataID's, throw error and print the new metadataID's
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      stop(paste0("You are attempting to add metadataID's that do not exist in either the database FISH_SAMPLES table or the in-season FISH_SAMPLES file. Here are the metadataID's, and the files they come from: \n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\n If you are sure these metadataID's are valid, use the force_metadataID argument."))
    }
  }
}

# newProjectIDCheck --------------------------------------------------------
newProjectIDCheck <- function(fsdb, is, hdf, f = force_newProjectID){
  # Get projectIDs previously in the database
  dbProjectID <- fsdb %>% pull(projectID) %>% unique()
  
  # Get projectIDs previously in the in-season FISH_SAMPLES file
  isProjectID <- is %>% pull(projectID) %>% unique()
  
  # Put them together
  previousProjectIDs <- c(dbProjectID, isProjectID)
  
  # Find problem rows in hdf (i.e. that have new projectID)
  problemRows <- hdf %>%
    filter(!projectID %in% previousProjectIDs) %>%
    select(projectID, entryFile) %>%
    distinct()
  
  # If there are new projectID's, throw error and print the new projectID's
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      stop(paste0("You are attempting to add projectID's that do not exist in either the database FISH_SAMPLES table or the in-season FISH_SAMPLES file. Here are the projectID's, and the files they come from: \n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\n If you are sure these projectID's are valid, use the force_newProjectID argument."))
    }
  }
}

# # species names
# if(any(!fishInfoNEW$species%in%c(fishInfoIS$species, fishInfoDB$species))){
#   if(force_species == FALSE){
#     stop(paste("the species (", unique(fishInfoNEW$species[!fishInfoNEW$species%in%c(fishInfoIS$species, fishInfoDB$species)]), ") is not in the MFE database nor in this season's working database; if you are certain this is the correct species name, use the argument force_species to add it to this season's working database.", sep = ""))
#   }
# }
# 
# # fish number check for duplicates
# if(any(duplicated(fishInfoNEW$fishNum))){
#   stop("You have duplicate fish numbers and therefore duplicate fishIDs!")
# }
# 
# # size and weight bounds
# uniqSpec = unique(fishInfoNEW$species)
# for(j in 1:length(uniqSpec)){
#   curDB = fishInfoDB[(fishInfoDB$species == uniqSpec[j] & !is.na(fishInfoDB$fishLength)), ]
#   curDB = curDB[curDB$fishLength>0, ]
#   curNEW = fishInfoNEW[fishInfoNEW$species == uniqSpec[j], ]
#   
#   # range check on lengths, if those data were collected, first if() throws out NFC rows for anglers in some samples if they caught nothing but others in the boat did
#   if(any(!is.na(curNEW$fishLength))){
#     curNEW_Lcheck = curNEW[!is.na(curNEW$fishLength), ]
#     if(any(curNEW_Lcheck$fishLength>0)){
#       if(nrow(curDB)>15){
#         if(any(curNEW_Lcheck$fishLength< = 0)){
#           stop(paste("You report negative fishLength in sample ", paste(lakeID, siteName, dateSampleString, timeSampleString, gear, metadataID, sep = "_"), 
#                      "; Check fish:", paste(curNEW_Lcheck$fishID[curNEW_Lcheck$fishLength< = 0], collapse = ", "), sep = ""))
#         }
#         if(any(curNEW_Lcheck$fishLength>(mean(curDB$fishLength)+3*sd(curDB$fishLength)))){
#           if(force_fishLength == FALSE){
#             stop(paste("You report fish longer than 3 standard deviations above the mean ever observed by us for", uniqSpec[j], "in sample", paste(lakeID, siteName, dateSampleString, timeSampleString, gear, metadataID, sep = "_"), "if you are certain you have fishLength correct for all individuals use the argument force_fishLength;", 
#                        "Check fish: ", paste(curNEW_Lcheck$fishID[curNEW_Lcheck$fishLength>(mean(curDB$fishLength)+3*sd(curDB$fishLength))], collapse = ", "), sep = " "))
#           }
#         } 
#       }else{
#         warning(paste("We have less than 15 observations for length of species", uniqSpec[j], " and will not be running an automated fishLength check. Be sure to double check the lengths you've entered.", sep = " "))
#       }
#     }
#   }
#   # check on weights (based on length-weight regression), if those data were collected
#   if(any(curNEW$fishWeight>0 & !is.na(curNEW$fishWeight))){
#     curDB = curDB[!is.na(curDB$fishWeight), ]
#     curDB = curDB[curDB$fishWeight>0, ]
#     if(nrow(curDB)>15){
#       if(any(curNEW$fishWeight< = 0)){
#         stop(paste("You report negative fishWeight in sample ", paste(lakeID, siteName, dateSampleString, timeSampleString, gear, metadataID, sep = "_"), 
#                    ". Check fish: ", paste(curNEW$fishID[curNEW$fishWeight< = 0], collapse = ", "), sep = ""))
#       }
#       #fit length-weight regression for uniqSpec[j]
#       curDB$logWeight = log(curDB$fishWeight)
#       curDB$logLength = log(curDB$fishLength)
#       lwreg = lm(logWeight~logLength, data = curDB)
#       preds = predict(lwreg, newdata = data.frame(logLength = log(as.numeric(curNEW$fishLength))), interval = "prediction", se.fit = TRUE, level = 0.99)
#       predsLow = exp(preds$fit[, 2])
#       predsHigh = exp(preds$fit[, 3])
#       
#       if(any(curNEW$fishWeight<predsLow | curNEW$fishWeight>predsHigh)){
#         if(force_fishWeight == FALSE){
#           stop(paste("You report fishWeight outside the prediction based on a length-weight regression from our database for", uniqSpec[j], "in sample", paste(lakeID, siteName, dateSampleString, timeSampleString, gear, metadataID, sep = "_"), "if you are certain you have fishWeight correct for all individuals use the argument force_fishWeight;", 
#                      "The check fish:", paste(curNEW$fishID[curNEW$fishWeight<predsLow | curNEW$fishWeight>predsHigh], collapse = ", "), sep = " "))
#         }
#       }
#     }else{
#       warning(paste("We have less than 15 observations for weight of species", uniqSpec[j], " and will not be running an automated fishWeight check. Be sure to double check the lengths you've entered.", sep = " "))
#     }
#     
#   }
#   
# }
# 
# ### tag marks and recaps --> figure what the common prefixes are and distinguish errors in prefix vs. individual number?
# 
# # -clipApply & clipRecapture should be in the database
# clips = c(curNEW$clipApply, curNEW$clipRecapture)
# clips = clips[!is.na(clips)]
# if(length(clips)>0){
#   if(any(!clips%in%c(fishInfoDB$clipApply, fishInfoDB$clipRecapture, fishInfoIS$clipApply, fishInfoIS$clipRecapture))){
#     if(force_clip == FALSE){
#       stop("You have indicated a clipApply or clipRecapture that is not in the database or in-season database. If you are certain this is the correct clipApply or clipRecapture then use the argument force_clip.")
#     }
#   }
# }
# 
# #check for clipApply of that type in that lake
# clipRecap = curNEW$clipRecapture[!is.na(curNEW$clipRecapture)]
# if(length(clipRecap)>0){
#   if(any(!unique(clipRecap)%in%c(fishInfoDB$clipApply[grepl(lakeID, fishInfoDB$sampleID)], fishInfoIS$clipApply[grepl(lakeID, fishInfoIS$sampleID)]))){
#     
#     if(force_clip == FALSE){
#       stop("You have indicated a clipApply or clipRecapture that is not in the database or in-season database. If you are certain this is the correct clipApply or clipRecapture then use the argument force_clip.")
#     }
#   }
# }
# 
# #tagApply today conflicts with tagApply from previous
# #* check in this lake and other lakes...
# # print conflicting fishInfo rows
# # attempt to fix based on sequential tags, but probably not
# # most likely remove the previous tagApply -- but are we sure that today's is right and not the previous apply?
# # change data sheet too!?!?!?
# tagsApplied = curNEW$tagApply[!is.na(curNEW$tagApply)]
# tagsApplied = curNEW$tagApply[curNEW$tagApply! = ""]
# if(length(tagsApplied)>0){
#   if(any(tagsApplied%in%c(fishInfoDB$tagApply, fishInfoIS$tagApply), na.rm = TRUE)){
#     for(j in 1:length(tagsApplied)){
#       if(tagsApplied[j]%in%fishInfoDB$tagApply){
#         tagApplyStop = TRUE
#         temp = rbind(fishInfoDB[fishInfoDB$tagApply == tagsApplied[j], c(1:8, 12:19, 38:39)], curNEW[curNEW$tagApply == tagsApplied[j], c(1:8, 12:19, 38, 41)])
#         print(temp)
#       }
#       if(tagsApplied[i]%in%fishInfoIS$tagApply){
#         tagApplyStop = TRUE
#         temp = rbind(fishInfoIS[fishInfoIS$tagApply == tagsApplied[j], c(1:8, 12:19, 38:39)], curNEW[curNEW$tagApply == tagsApplied[j], c(1:8, 12:19, 38, 41)])
#         print(temp)
#       }
#     }
#     stop("You are attempting to enter a tagApply that was already recorded as a tagApply in the database or in-season database.")
#   }
# }
# 
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