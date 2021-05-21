# Function for generating in-season database files from fish datasheets
# Originally developed by Stuart E. Jones
# Last updated by Kaija Gahm, May 2021

# Show full text of errors and warnings
options(warning.length = 3000L, error.length = 3000L)

# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(checkmate)

Sys.setenv(tz = "America/Chicago")

dbdir <- here()
db <- "MFEdb_20210423.db"
funcdir <- here("code")
isdir <- here("inSeason")

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
      force_projectID = FALSE, 
      force_species = FALSE, 
      force_fishLength = FALSE, 
      force_fishWeight = FALSE, 
      force_clip = FALSE){
 
 source(file.path(funcdir, "dbUtil.R")) # load the dbUtil functions
 
 # Load tables from database
 lakesDB <- suppressWarnings(dbTable("lakes"))
 fishSamplesDB <- suppressWarnings(dbTable("fish_samples"))
 fishInfoDB <- suppressWarnings(dbTable("fish_info"))
 otu <- suppressWarnings(dbTable("otu"))
 fishNames <- otu %>% filter(!is.na(abbreviation)) # #XXX This assumes that all fish in the database have an abbreviation. I'm not sure that's a safe assumption. Would it be better to do all where the category is "fish"?

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
   
   # Read in the current file, setting all blank cells to NA
   cur <- read.csv(here("sampleSheets", file), 
                   na.strings = c("", " ", "NA"), header = F)
   
  # Pull header info
  header <- getHeader(cur)

  # Check for NA or empty strings in the header, taking into account whether a value for distanceShocked is expected. Regardless of gear, comments are allowed to be NA.
  checkHeader(header)
 
  # Tabular data
  curData <- getCurData(cur)
 
  #need to be sure that the column names in the entry template are the same as in the database 
  #(change datasheet to match this too); then ones that aren't in curData colnames, get NA and
  #others will match
  # XXX come back to this.
 
  # Get date and time strings for sampleID's and fishID's
  dateSampleString <- strftime(strptime(header$dateTimeSample, format = "%m/%d/%Y %H:%M:%S"), format = "%Y%m%d")
  timeSampleString <- strftime(strptime(header$dateTimeSample, format = "%m/%d/%Y %H:%M:%S"), format = "%H%M")
  # XXX will need lots of checks here, and in the getHeader function too.
  
  # Make new rows for FISH_INFO # XXX this can be its own function
  fishInfoNew <- curData %>%
    mutate(projectID = header$projectID,
           metadataID = header$metadataID,
           sampleID = paste(header$lakeID, header$siteName, dateSampleString,
                            timeSampleString, header$gear, metadataID, 
                            sep = "_"),
           fishNum = as.numeric(fishNum),
           fishID = paste(sampleID, fishNum, sep = "_"),
           entryFile = file)
  
  # Convert species abbreviations to common names
  fishInfoNew <- convertSpeciesAbbreviations(x = fishInfoNew, fn = fishNames)
  
  # generate FISH_SAMPLES rows # XXX this can be its own function
  fishSamplesNew <- data.frame(key = names(header),
                               value = unname(unlist(header))) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    mutate(siteID = paste(lakeID, siteName, sep = "_"),
           sampleID = paste(siteID, dateSampleString, timeSampleString,
                            gear, metadataID, sep = "_"),
           dayOfYear = as.numeric(strftime(strptime(dateSample,
                                                    format = "%Y-%m-%d"),
                                           format = "%j")),
           entryFile = file,
           updateID = NA) %>%
    select(-siteName)

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
      if("fishspinesLOG.csv" %in% list.files(isdir)){
        fishspinesLOG <- read.csv(here(isdir, "fishspinesLOG.csv"),
                                  header = T, stringsAsFactors = F)
      }else{
        fishSpinesLOG <- data.frame()
      }
      
      # Make new fish spines rows # XXX START HERE: move the below code for fishspinesNEW up to this if statement.
    }
  }
  
  
  fishspinesNEW = data.frame(fishID = paste(lakeID, siteName, dateSampleString, timeSampleString, gear, metadataID, curData$fishNum, sep = "_")[curData$spineSample == 1], 
         lengthAtCapture = curData$length[curData$spineSample == 1], 
         weightAtCapture = curData$weight[curData$spineSample == 1]
  )
  }  
  
  # check for scales pulled and generate a log of fish scales
  if("scaleSample"%in%colnames(curData) & any(curData$scaleSample == 1)){
  if("fishscalesLOG.csv"%in%list.files()){
   fishscalesLOG = read.csv("fishscalesLOG.csv", header = TRUE, stringsAsFactors = FALSE)
  }else{
   fishscalesLOG = data.frame()
  }
  
  
  fishscalesNEW = data.frame(fishID = paste(lakeID, siteName, dateSampleString, timeSampleString, gear, metadataID, curData$fishNum, sep = "_")[curData$scaleSample == 1], 
         lengthAtCapture = curData$length[curData$scaleSample == 1], 
         weightAtCapture = curData$weight[curData$scaleSample == 1]
  )
  }  
  
  # check for diets taken and generate a log of diets
  if("dietSampled"%in%colnames(curData) & any(curData$dietSampled == 1)){
  if("fishDietsLOG.csv"%in%list.files()){
   fishDietsLOG = read.csv("fishDietsLOG.csv", header = TRUE, stringsAsFactors = FALSE)
  }else{
   fishDietsLOG = data.frame()
  }
  
  fishDietsNEW = data.frame(fishID = paste(lakeID, siteName, dateSampleString, timeSampleString, gear, metadataID, curData$fishNum, sep = "_")[curData$dietSample == 1], 
        lakeID = lakeID[curData$dietSampled == 1], 
        dateSample = dateSample[curData$dietSampled == 1], 
        species = species[curData$dietSampled == 1]
        )
  }

  # run checks against in-season database and the full database for...
  
  # lakeID already in database
  lakeIDsIS = substr(fishSamplesIS$siteID, start = 1, stop = 2)
  if(!lakeID%in%c(lakesDB$lakeID, lakeIDsIS)){
  if(force_lakeID == FALSE){
   stop(paste("your lakeID (", lakeID, ") is not in the MFE database nor in this season's working database; if you are certain this is the correct lakeID, use the argument force_lakeID to add a new lakeID to this season's working database.", sep = ""))
  }
  }
  
  # siteID already in database
  if(!fishSamplesNEW$siteID%in%c(fishSamplesIS$siteID, fishSamplesDB$siteID)){
  if(force_siteID == FALSE){
   stop(paste("your siteID (", fishSamplesNEW$siteID, ") is not in the MFE database nor in this season's working database; if you are certain this is the correct siteID, use the argument force_siteID to add a new siteID to this season's working database.", sep = ""))
  }
  }
  
  # sampleID NOT already in database
  if(fishSamplesNEW$sampleID%in%c(fishSamplesIS$sampleID, fishSamplesDB$sampleID)){
  stop(paste("your sampleID (", fishSamplesNEW$sampleID, ") is already in the MFE database or in this season's working database!", sep = ""))
  }
  
  # dayOfYear in an acceptable range
  if(fishSamplesNEW$dayOfYear<91 | fishSamplesNEW$dayOfYear>305){
  if(force_dayOfYear == FALSE){
   stop(paste("your dayOfYear (", fishSamplesNEW$dayOfYear, ") is not in the usual range; if your dayOfYear is correct use the argument force_dayOfYear", sep = ""))
  }
  }
  
  #dateSet must be the same or earlier than dateSample?
  if(as.Date(dateSample)<as.Date(dateSet)){
  stop("Your dateSample is < your dateSet!")
  }
  
  #dateTimeSet must be the same or earlier than dateTimeSample?
  if(strptime(dateTimeSample, format = "%m/%d/%Y %H:%M:%S")<strptime(dateTimeSet, format = "%m/%d/%Y %H:%M:%S")){
  stop("Your dateTimeSample is < your dateTimeSet!")
  }
  
  # gear already in database
  if(!fishSamplesNEW$gear%in%c(fishSamplesIS$gear, fishSamplesDB$gear)){
  if(force_gear == FALSE){
   stop(paste("your gear (", fishSamplesNEW$gear, ") is not in the MFE database nor in this season's working database; if you are certain this is the correct gear, use the argument force_gear to add a new gear to this season's working database.", sep = ""))
  }
  }
  
  # sampleGroup already in database????
  if(!fishSamplesNEW$sampleGroup%in%c(fishSamplesIS$sampleGroup, fishSamplesDB$sampleGroup)){
  if(force_sampleGroup == FALSE){
   stop(paste("your sampleGroup (", fishSamplesNEW$sampleGroup, ") is not in the MFE database nor in this season's working database; if you are certain this is the correct sampleGroup, use the argument force_sampleGroup to add a new sampleGroup to this season's working database.", sep = ""))
  }
  }
  
  # effort in an acceptable range
  if(fishSamplesNEW$effort< = 0 | fishSamplesNEW$effort>24){
  if(force_effort == FALSE){
   stop(paste("your effort (", fishSamplesNew$effort, ") is outside the normal range; if you are certain this is the correct effort use the argument force_effort."))
  }
  }
  
  # effortUnits already in database????
  if(!fishSamplesNEW$effortUnits%in%c(fishSamplesIS$effortUnits, fishSamplesDB$effortUnits)){
  if(force_effortUnits == FALSE){
   stop(paste("your effortUnits (", fishSamplesNEW$effortUnits, ") is not in the MFE database nor in this season's working database; if you are certain this is the correct effortUnits, use the argument force_effortUnits to add a new effortUnits to this season's working database.", sep = ""))
  }
  }
  
  # distanceShocked in an acceptable range
  if(fishSamplesNEW$distanceShocked<0 | fishSamplesNEW$distanceShocked>25){
  if(force_distanceShocked == FALSE){
   stop(paste("your distanceShocked (", fishSamplesNEW$effortUnits, ") is outside the normal range; if you are certain this is the correct distanceShocked use the argument force_distanceShocked."))
  }
  }
  
  # useCPUE already in database????
  if(!fishSamplesNEW$useCPUE%in%c(fishSamplesIS$useCPUE, fishSamplesDB$useCPUE)){
  stop(paste("your useCPUE (", fishSamplesNEW$useCPUE, ") is not an acceptable value", sep = ""))
  }
  
  # useSampleMarkRecap already in database????
  if(!fishSamplesNEW$useSampleMarkRecap%in%c(fishSamplesIS$useSampleMarkRecap, fishSamplesDB$useSampleMarkRecap)){
  stop(paste("your useSampleMarkRecap (", fishSamplesNEW$useSampleMarkRecap, ") is not an acceptable value", sep = ""))
  }
  
  # metadataID already in database
  if(!fishSamplesNEW$metadataID%in%c(fishSamplesIS$metadataID, fishSamplesDB$metadataID)){
  if(force_metadataID == FALSE){
   stop(paste("your metadataID (", fishSamplesNEW$metadataID, ") is not in the MFE database nor in this season's working database; if you are certain this is the correct metadataID, use the argument force_metadataID to add a new metadataID to this season's working database.", sep = ""))
  }
  }
  
  # projectID already in database
  if(!fishInfoNEW$projectID[1]%in%c(fishInfoIS$projectID, fishInfoDB$projectID)){
  if(force_projectID == FALSE){
   stop(paste("your projectID (", fishInfoNEW$projectID[1], ") is not in the MFE database nor in this season's working database; if you are certain this is the correct projectID, use the argument force_projectID to add a new projectID to this season's working database.", sep = ""))
  }
  }
  
  # species names
  if(any(!fishInfoNEW$species%in%c(fishInfoIS$species, fishInfoDB$species))){
  if(force_species == FALSE){
   stop(paste("the species (", unique(fishInfoNEW$species[!fishInfoNEW$species%in%c(fishInfoIS$species, fishInfoDB$species)]), ") is not in the MFE database nor in this season's working database; if you are certain this is the correct species name, use the argument force_species to add it to this season's working database.", sep = ""))
  }
  }
  
  # fish number check for duplicates
  if(any(duplicated(fishInfoNEW$fishNum))){
  stop("You have duplicate fish numbers and therefore duplicate fishIDs!")
  }
  
  # size and weight bounds
  uniqSpec = unique(fishInfoNEW$species)
  for(j in 1:length(uniqSpec)){
  curDB = fishInfoDB[(fishInfoDB$species == uniqSpec[j] & !is.na(fishInfoDB$fishLength)), ]
  curDB = curDB[curDB$fishLength>0, ]
  curNEW = fishInfoNEW[fishInfoNEW$species == uniqSpec[j], ]
  
  # range check on lengths, if those data were collected, first if() throws out NFC rows for anglers in some samples if they caught nothing but others in the boat did
  if(any(!is.na(curNEW$fishLength))){
   curNEW_Lcheck = curNEW[!is.na(curNEW$fishLength), ]
   if(any(curNEW_Lcheck$fishLength>0)){
   if(nrow(curDB)>15){
    if(any(curNEW_Lcheck$fishLength< = 0)){
    stop(paste("You report negative fishLength in sample ", paste(lakeID, siteName, dateSampleString, timeSampleString, gear, metadataID, sep = "_"), 
      "; Check fish:", paste(curNEW_Lcheck$fishID[curNEW_Lcheck$fishLength< = 0], collapse = ", "), sep = ""))
    }
    if(any(curNEW_Lcheck$fishLength>(mean(curDB$fishLength)+3*sd(curDB$fishLength)))){
    if(force_fishLength == FALSE){
     stop(paste("You report fish longer than 3 standard deviations above the mean ever observed by us for", uniqSpec[j], "in sample", paste(lakeID, siteName, dateSampleString, timeSampleString, gear, metadataID, sep = "_"), "if you are certain you have fishLength correct for all individuals use the argument force_fishLength;", 
       "Check fish: ", paste(curNEW_Lcheck$fishID[curNEW_Lcheck$fishLength>(mean(curDB$fishLength)+3*sd(curDB$fishLength))], collapse = ", "), sep = " "))
    }
    } 
   }else{
    warning(paste("We have less than 15 observations for length of species", uniqSpec[j], " and will not be running an automated fishLength check. Be sure to double check the lengths you've entered.", sep = " "))
   }
   }
  }
  # check on weights (based on length-weight regression), if those data were collected
  if(any(curNEW$fishWeight>0 & !is.na(curNEW$fishWeight))){
   curDB = curDB[!is.na(curDB$fishWeight), ]
   curDB = curDB[curDB$fishWeight>0, ]
   if(nrow(curDB)>15){
   if(any(curNEW$fishWeight< = 0)){
    stop(paste("You report negative fishWeight in sample ", paste(lakeID, siteName, dateSampleString, timeSampleString, gear, metadataID, sep = "_"), 
       ". Check fish: ", paste(curNEW$fishID[curNEW$fishWeight< = 0], collapse = ", "), sep = ""))
   }
   #fit length-weight regression for uniqSpec[j]
   curDB$logWeight = log(curDB$fishWeight)
   curDB$logLength = log(curDB$fishLength)
   lwreg = lm(logWeight~logLength, data = curDB)
   preds = predict(lwreg, newdata = data.frame(logLength = log(as.numeric(curNEW$fishLength))), interval = "prediction", se.fit = TRUE, level = 0.99)
   predsLow = exp(preds$fit[, 2])
   predsHigh = exp(preds$fit[, 3])
   
   if(any(curNEW$fishWeight<predsLow | curNEW$fishWeight>predsHigh)){
    if(force_fishWeight == FALSE){
    stop(paste("You report fishWeight outside the prediction based on a length-weight regression from our database for", uniqSpec[j], "in sample", paste(lakeID, siteName, dateSampleString, timeSampleString, gear, metadataID, sep = "_"), "if you are certain you have fishWeight correct for all individuals use the argument force_fishWeight;", 
       "The check fish:", paste(curNEW$fishID[curNEW$fishWeight<predsLow | curNEW$fishWeight>predsHigh], collapse = ", "), sep = " "))
    }
   }
   }else{
   warning(paste("We have less than 15 observations for weight of species", uniqSpec[j], " and will not be running an automated fishWeight check. Be sure to double check the lengths you've entered.", sep = " "))
   }
   
  }
  
  }
  
  ### tag marks and recaps --> figure what the common prefixes are and distinguish errors in prefix vs. individual number?
  
  # -clipApply & clipRecapture should be in the database
  clips = c(curNEW$clipApply, curNEW$clipRecapture)
  clips = clips[!is.na(clips)]
  if(length(clips)>0){
  if(any(!clips%in%c(fishInfoDB$clipApply, fishInfoDB$clipRecapture, fishInfoIS$clipApply, fishInfoIS$clipRecapture))){
   if(force_clip == FALSE){
   stop("You have indicated a clipApply or clipRecapture that is not in the database or in-season database. If you are certain this is the correct clipApply or clipRecapture then use the argument force_clip.")
   }
  }
  }
  
  #check for clipApply of that type in that lake
  clipRecap = curNEW$clipRecapture[!is.na(curNEW$clipRecapture)]
  if(length(clipRecap)>0){
  if(any(!unique(clipRecap)%in%c(fishInfoDB$clipApply[grepl(lakeID, fishInfoDB$sampleID)], fishInfoIS$clipApply[grepl(lakeID, fishInfoIS$sampleID)]))){
   
   if(force_clip == FALSE){
   stop("You have indicated a clipApply or clipRecapture that is not in the database or in-season database. If you are certain this is the correct clipApply or clipRecapture then use the argument force_clip.")
   }
  }
  }
  
  #tagApply today conflicts with tagApply from previous
  #* check in this lake and other lakes...
  # print conflicting fishInfo rows
  # attempt to fix based on sequential tags, but probably not
  # most likely remove the previous tagApply -- but are we sure that today's is right and not the previous apply?
  # change data sheet too!?!?!?
  tagsApplied = curNEW$tagApply[!is.na(curNEW$tagApply)]
  tagsApplied = curNEW$tagApply[curNEW$tagApply! = ""]
  if(length(tagsApplied)>0){
  if(any(tagsApplied%in%c(fishInfoDB$tagApply, fishInfoIS$tagApply), na.rm = TRUE)){
   for(j in 1:length(tagsApplied)){
   if(tagsApplied[j]%in%fishInfoDB$tagApply){
    tagApplyStop = TRUE
    temp = rbind(fishInfoDB[fishInfoDB$tagApply == tagsApplied[j], c(1:8, 12:19, 38:39)], curNEW[curNEW$tagApply == tagsApplied[j], c(1:8, 12:19, 38, 41)])
    print(temp)
   }
   if(tagsApplied[i]%in%fishInfoIS$tagApply){
    tagApplyStop = TRUE
    temp = rbind(fishInfoIS[fishInfoIS$tagApply == tagsApplied[j], c(1:8, 12:19, 38:39)], curNEW[curNEW$tagApply == tagsApplied[j], c(1:8, 12:19, 38, 41)])
    print(temp)
   }
   }
   stop("You are attempting to enter a tagApply that was already recorded as a tagApply in the database or in-season database.")
  }
  }
  
  # -tagRecap should have an apply in the database from that lake at some point in the past
  # the absence of a previous apply might mean it was entered wrong
  # make this the tag apply -> change data sheet too!?!?!
  tagsRecapped = curNEW$tagRecapture[!is.na(curNEW$tagRecapture)]
  tagsRecapped = curNEW$tagRecapture[curNEW$tagRecapture! = ""]
  if(length(tagsRecapped)>0){
  if(any(!tagsRecapped%in%c(fishInfoDB$tagApply, fishInfoIS$tagApply))){
   print(tagsRecapped[!tagsRecapped%in%c(fishInfoDB$tagApply, fishInfoIS$tagApply)])
   stop("You are attempting to enter a tagRecapture that was not ever recorded as a tagApply in the database or the in-season database.")
  }
  }
  
  # - tagRecap accidently matches an old apply
  # check lengths when matching recap to apply
  # use vonB for Long Lake and or Wisconsin for this...
  if(length(tagsRecapped)>0){
  for(j in 1:length(tagsRecapped)){
   # if we measured a length
   if(!is.na(curNEW$fishLength[curNEW$tagRecapture == tagsRecapped[j]])){
   #if tagged this year
   if(tagsRecapped[j]%in%fishInfoIS$tagApply){
    #if we measured length when tag was applied
    if(any(!is.na(fishInfoIS$fishLength[fishInfoIS$tagApply == tagsRecapped[j]]), na.rm = TRUE)){
    #if we are looking at Long Lake, use our vonB
    if(lakeID%in%c("EL", "FE", "WL")){
     Linf = 376.909
     K = 0.32
     t0 = -0.599
     tagLength = fishInfoIS$fishLength[fishInfoIS$tagApply == tagsRecapped[j]]
     tagDate = fishSamplesIS$dateSample[fishSamplesIS$sampleID == fishInfoIS$sampleID[fishInfoIS$tagApply == tagsRecapped[j]]]
     growthTime = as.Date(dateSample)-as.Date(tagDate)
     age1 = log(1-tagLength/Linf)/-K+t0
     age2 = age1+growthTime
     expL = Linf*(1-exp(-K*(age2-t0)))
     obsL = curNEW$fishLength[curNEW$tagRecapture == tagsRecapped[j]]
     #if the fish we recapped is shorter or longer than fish tagged, based on vonB
     if(abs(expL-obsL)>(0.1*obsL)){
     stop(paste("The fish with tagRecapture:", tagsRecapped[j], "is a length that would not be expected. Is there a problem with the tag data or length data?", sep = " "))
     }
    # use general vonB from Beamsderfer & North 1995
    }else{
     Linf = 550
     K = 0.19
     t0 = -0.024
     tagLength = fishInfoIS$fishLength[fishInfoIS$tagApply == tagsRecapped[j]]
     tagDate = fishSamplesIS$dateSample[fishSamplesIS$sampleID == fishInfoIS$sampleID[fishInfoIS$tagApply == tagsRecapped[j]]]
     growthTime = (as.Date(dateSample)-as.Date(tagDate))/365 # difference in days, so convert to years for vonB
     age1 = log(1-tagLength/Linf)/-K+t0
     age2 = age1+growthTime
     expL = Linf*(1-exp(-K*(age2-t0)))
     obsL = curNEW$fishLength[curNEW$tagRecapture == tagsRecapped[j]]
     #if the fish we recapped is shorter or longer than fish tagged, based on vonB
     if(abs(expL-obsL)>(0.1*obsL)){
     stop(paste("The fish with tagRecapture:", tagsRecapped[j], "is a length that would not be expected. Is there a problem with the tag data or length data?", sep = " "))
     }
    }   
    }
   }
   # if tagged in a previous year
   if(tagsRecapped[j]%in%fishInfoDB$tagApply){
    #if we measured length when tag was applied
    if(any(!is.na(fishInfoDB$fishLength[fishInfoDB$tagApply == tagsRecapped[j]]), na.rm = TRUE)){
    #if we are looking at Long Lake, use our vonB
    if(lakeID%in%c("EL", "FE", "WL")){
     Linf = 376.909
     K = 0.32
     t0 = -0.599
     tagLength = fishInfoDB$fishLength[fishInfoDB$tagApply == tagsRecapped[j]]
     tagDate = fishSamplesDB$dateSample[fishSamplesDB$sampleID == fishInfoDB$sampleID[fishInfoDB$tagApply == tagsRecapped[j]]]
     growthTime = as.Date(dateSample)-as.Date(tagDate)
     age1 = log(1-tagLength/Linf)/-K+t0
     age2 = age1+growthTime
     expL = Linf*(1-exp(-K*(age2-t0)))
     obsL = curNEW$fishLength[curNEW$tagRecapture == tagsRecapped[j]]
     #if the fish we recapped is shorter or longer than fish tagged, based on vonB
     if(abs(expL-obsL)>(0.1*obsL)){
     stop(paste("The fish with tagRecapture:", tagsRecapped[j], "is a length that would not be expected. Is there a problem with the tag data or length data?", sep = " "))
     }
     # use general vonB from Beamsderfer & North 1995
    }else{
     Linf = 550
     K = 0.19
     t0 = -0.024
     tagLength = fishInfoDB$fishLength[fishInfoDB$tagApply == tagsRecapped[j]]
     tagDate = fishSamplesDB$dateSample[fishSamplesDB$sampleID == fishInfoDB$sampleID[fishInfoDB$tagApply == tagsRecapped[j]]]
     growthTime = (as.Date(dateSample)-as.Date(tagDate))/365 # difference in days, so convert to years for vonB
     age1 = log(1-tagLength/Linf)/-K+t0
     age2 = age1+growthTime
     expL = Linf*(1-exp(-K*(age2-t0)))
     obsL = curNEW$fishLength[curNEW$tagRecapture == tagsRecapped[j]]
     #if the fish we recapped is shorter or longer than fish tagged, based on vonB
     if(abs(expL-obsL)>(0.1*obsL)){
     stop(paste("The fish with tagRecapture:", tagsRecapped[j], "is a length that would not be expected. Is there a problem with the tag data or length data?", sep = " "))
     }
    }   
    }
   }
   }
  }
  }
  
  # update tables with new entries
  fishSamplesIS = rbind(fishSamplesIS, fishSamplesNEW)
  fishInfoIS = rbind(fishInfoIS, fishInfoNEW)
  if(exists("fishDietsNEW")){fishDietsLOG = rbind(fishDietsLOG, fishDietsNEW)}
  if(exists("fishOtolithsNEW")){fishOtolithsLOG = rbind(fishOtolithsLOG, fishOtolithsNEW)}
  if(exists("fishspinesNEW")){fishspinesLOG = rbind(fishspinesLOG, fishspinesNEW)}
  if(exists("fishscalesNEW")){fishscalesLOG = rbind(fishscalesLOG, fishscalesNEW)}
  
 }
 
 # write updates to files
 write.csv(fishInfoIS, "fishInfoIS.csv", row.names = FALSE)
 write.csv(fishSamplesIS, "fishSamplesIS.csv", row.names = FALSE)
 
 if(exists("fishDietsNEW")){write.csv(fishDietsLOG, "fishDietsLOG.csv", row.names = FALSE)}
 if(exists("fishOtolithsNEW")){write.csv(fishOtolithsLOG, "fishOtolithsLOG.csv", row.names = FALSE)}
 if(exists("fishspinesNEW")){write.csv(fishspinesLOG, "fishspinesLOG.csv", row.names = FALSE)}
 if(exists("fishscalesNEW")){write.csv(fishscalesLOG, "fishscalesLOG.csv", row.names = FALSE)}
 
 }
}
