# Check functions for the fish entry tool
# Created by Kaija Gahm on 21 May 2021

# checkHeader function ----------------------------------------------------
checkHeader <- function(h = header){
  requiredElectro <- h[!names(h) == "comments"]
  requiredNonElectro <- h[!names(h) %in% c("comments", "distanceShocked")]
  
  if(h$gear == "BE"){
    if(any(is.na(requiredElectro)|requiredElectro == "")){
      paste0("Required header information is incomplete in ", file, 
             ". You're missing: ", 
             paste(names(requiredElectro[requiredElectro == ""|is.na(requiredElectro)]), 
                   collapse = ", "))
    }
  }else{
    if(any(is.na(requiredNonElectro)|requiredNonElectro == "")){
      paste0("Required header information is incomplete in ", file,
             ". You're missing: ",
             paste(names(requiredNonElectro[requiredNonElectro == ""|is.na(requiredNonElectro)]),
                   collapse = ", "))
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

# # lakeID already in database
# lakeIDsIS <- word(fishSamplesIS$siteID, 1, 1, sep = "_") %>%
#   unique()
# if(!header$lakeID %in% c(lakesDB$lakeID, lakeIDsIS)){
#   if(force_lakeID == FALSE){
#     stop(paste("your lakeID (", lakeID, ") is not in the MFE database nor in this season's working database; if you are certain this is the correct lakeID, use the argument force_lakeID to add a new lakeID to this season's working database.", sep = ""))
#   }
# }
# 
# # siteID already in database
# if(!fishSamplesNEW$siteID%in%c(fishSamplesIS$siteID, fishSamplesDB$siteID)){
#   if(force_siteID == FALSE){
#     stop(paste("your siteID (", fishSamplesNEW$siteID, ") is not in the MFE database nor in this season's working database; if you are certain this is the correct siteID, use the argument force_siteID to add a new siteID to this season's working database.", sep = ""))
#   }
# }
# 
# # sampleID NOT already in database
# if(fishSamplesNEW$sampleID%in%c(fishSamplesIS$sampleID, fishSamplesDB$sampleID)){
#   stop(paste("your sampleID (", fishSamplesNEW$sampleID, ") is already in the MFE database or in this season's working database!", sep = ""))
# }
# 
# # dayOfYear in an acceptable range
# if(fishSamplesNEW$dayOfYear<91 | fishSamplesNEW$dayOfYear>305){
#   if(force_dayOfYear == FALSE){
#     stop(paste("your dayOfYear (", fishSamplesNEW$dayOfYear, ") is not in the usual range; if your dayOfYear is correct use the argument force_dayOfYear", sep = ""))
#   }
# }
# 
# #dateSet must be the same or earlier than dateSample?
# if(as.Date(dateSample)<as.Date(dateSet)){
#   stop("Your dateSample is < your dateSet!")
# }
# 
# #dateTimeSet must be the same or earlier than dateTimeSample?
# if(strptime(dateTimeSample, format = "%m/%d/%Y %H:%M:%S")<strptime(dateTimeSet, format = "%m/%d/%Y %H:%M:%S")){
#   stop("Your dateTimeSample is < your dateTimeSet!")
# }
# 
# # gear already in database
# if(!fishSamplesNEW$gear%in%c(fishSamplesIS$gear, fishSamplesDB$gear)){
#   if(force_gear == FALSE){
#     stop(paste("your gear (", fishSamplesNEW$gear, ") is not in the MFE database nor in this season's working database; if you are certain this is the correct gear, use the argument force_gear to add a new gear to this season's working database.", sep = ""))
#   }
# }
# 
# # sampleGroup already in database????
# if(!fishSamplesNEW$sampleGroup%in%c(fishSamplesIS$sampleGroup, fishSamplesDB$sampleGroup)){
#   if(force_sampleGroup == FALSE){
#     stop(paste("your sampleGroup (", fishSamplesNEW$sampleGroup, ") is not in the MFE database nor in this season's working database; if you are certain this is the correct sampleGroup, use the argument force_sampleGroup to add a new sampleGroup to this season's working database.", sep = ""))
#   }
# }
# 
# # effort in an acceptable range
# if(fishSamplesNEW$effort< = 0 | fishSamplesNEW$effort>24){
#   if(force_effort == FALSE){
#     stop(paste("your effort (", fishSamplesNew$effort, ") is outside the normal range; if you are certain this is the correct effort use the argument force_effort."))
#   }
# }
# 
# # effortUnits already in database????
# if(!fishSamplesNEW$effortUnits%in%c(fishSamplesIS$effortUnits, fishSamplesDB$effortUnits)){
#   if(force_effortUnits == FALSE){
#     stop(paste("your effortUnits (", fishSamplesNEW$effortUnits, ") is not in the MFE database nor in this season's working database; if you are certain this is the correct effortUnits, use the argument force_effortUnits to add a new effortUnits to this season's working database.", sep = ""))
#   }
# }
# 
# # distanceShocked in an acceptable range
# if(fishSamplesNEW$distanceShocked<0 | fishSamplesNEW$distanceShocked>25){
#   if(force_distanceShocked == FALSE){
#     stop(paste("your distanceShocked (", fishSamplesNEW$effortUnits, ") is outside the normal range; if you are certain this is the correct distanceShocked use the argument force_distanceShocked."))
#   }
# }
# 
# # useCPUE already in database????
# if(!fishSamplesNEW$useCPUE%in%c(fishSamplesIS$useCPUE, fishSamplesDB$useCPUE)){
#   stop(paste("your useCPUE (", fishSamplesNEW$useCPUE, ") is not an acceptable value", sep = ""))
# }
# 
# # useSampleMarkRecap already in database????
# if(!fishSamplesNEW$useSampleMarkRecap%in%c(fishSamplesIS$useSampleMarkRecap, fishSamplesDB$useSampleMarkRecap)){
#   stop(paste("your useSampleMarkRecap (", fishSamplesNEW$useSampleMarkRecap, ") is not an acceptable value", sep = ""))
# }
# 
# # metadataID already in database
# if(!fishSamplesNEW$metadataID%in%c(fishSamplesIS$metadataID, fishSamplesDB$metadataID)){
#   if(force_metadataID == FALSE){
#     stop(paste("your metadataID (", fishSamplesNEW$metadataID, ") is not in the MFE database nor in this season's working database; if you are certain this is the correct metadataID, use the argument force_metadataID to add a new metadataID to this season's working database.", sep = ""))
#   }
# }
# 
# # projectID already in database
# if(!fishInfoNEW$projectID[1]%in%c(fishInfoIS$projectID, fishInfoDB$projectID)){
#   if(force_projectID == FALSE){
#     stop(paste("your projectID (", fishInfoNEW$projectID[1], ") is not in the MFE database nor in this season's working database; if you are certain this is the correct projectID, use the argument force_projectID to add a new projectID to this season's working database.", sep = ""))
#   }
# }
# 
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