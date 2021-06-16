# Function for generating in-season database files from minnow trap datasheets
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

updateMinnowTrap <- function(headerRows = 17, dbdir = dbdir, 
                             db = db, funcdir = funcdir,
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
                     force_clip = FALSE
                    ){

  source(file.path(funcdir, "dbUtil.R")) # load the dbUtil functions
  
  # Load database tables ---------------------------------------------------
  message("Loading database tables...")
  lakesDB <- suppressWarnings(dbTable("lakes"))
  sitesDB <- suppressWarnings(dbTable("sites"))
  fishSamplesDB <- suppressWarnings(dbTable("fish_samples"))
  fishInfoDB <- suppressWarnings(dbTable("fish_info"))
  otu <- suppressWarnings(dbTable("otu"))

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

  # check which files have been compiled and which have not in the directory
  beenCompiled=unique(fishInfoIS$entryFile)
  toCompile=list.files(pattern="[0-9]{4}-[0-9]{2}-[0-9]{2}\\.csv")
  toCompile=toCompile[!(toCompile%in%beenCompiled)]
  toCompile=toCompile[grepl("minnowtrap", toCompile)]
  if(length(toCompile)==0){
    #no files that have not been compiled into the in-season database
    print("The in season database is up to date; no new files to compile")
  }else{
    #there are files to be compiled; generate rows to append
    for(i in 1:length(toCompile)){
      cur=scan(toCompile[i],what=character(),sep ="\n")
    
      #pull header info
      projectID=as.numeric(strsplit(cur[1],",")[[1]][2])
      lakeID=strsplit(cur[2],",")[[1]][2]
      dateTimeSet=strsplit(cur[3],",")[[1]][2]
      dateTimeSet=paste(dateTimeSet,":00",sep="")
      dateTimeSample=strsplit(cur[4],",")[[1]][2]
      dateTimeSample=paste(dateTimeSample,":00",sep="")
      crew=strsplit(cur[5],",")[[1]][2]
      gear=strsplit(cur[6],",")[[1]][2]
      distanceShocked=strsplit(cur[7],",")[[1]][2]
      effort=as.numeric(strsplit(cur[8],",")[[1]][2])
      effortUnits=strsplit(cur[9],",")[[1]][2]
      comments=strsplit(cur[10],",")[[1]][2]
      useCPUE=strsplit(cur[11],",")[[1]][2]
      dataRecorder=strsplit(cur[12],",")[[1]][2]
      dataEnteredBy=strsplit(cur[13],",")[[1]][2]
      metadataID=strsplit(cur[14],",")[[1]][2]
      useSampleMarkRecap=strsplit(cur[15],",")[[1]][2]
      sampleGroup=strsplit(cur[16],",")[[1]][2]
    
      # add a check that these are non-empty springs or stop and tell user the header info is incomplete
      headerVals=c(projectID,lakeID,dateTimeSet,dateTimeSample,crew,gear,distanceShocked,effort,effortUnits,comments,useCPUE,dataRecorder,dataEnteredBy,metadataID,useSampleMarkRecap,sampleGroup)
      if(any(headerVals=="",na.rm=TRUE)){
        headerText=c("projectID","lakeID","dateTimeSet","dateTimeSample","crew","gear","distanceShocked","effort","effortUnits","comments","useCPUE","dataRecorder","dataEnteredBy","metadataID","useSampleMarkRecap","sampleGroup")
        stop(paste("required header information is incomplete; you're missing: ",headerText[headerVals==""],sep=","))
      }
    
      #tabular data
      # need to deal with empty cells at end of string for each line, thus the gsub() usage
      toTabular=cur[(headerRows+1):length(cur)]
      toTabular=gsub(",$",", ",toTabular)
    
      curData=data.frame(matrix(unlist(strsplit(toTabular,",")),nrow=length(cur)-headerRows,byrow=TRUE),stringsAsFactors=FALSE)
      colnames(curData)=strsplit(cur[headerRows],",")[[1]]
      curData$fishNum=as.numeric(curData$fishNum)
      curData$fishLength=as.numeric(curData$fishLength)
      curData$fishWeight=as.numeric(curData$fishWeight)
      curData$trapNumber=as.numeric(curData$trapNumber)
      
      # generate site id based on trapNumber
      curData$siteID=lakeID
      curData$siteID[curData$trapNumber<10]=paste(lakeID,".00",curData$trapNumber[curData$trapNumber<10],sep="")
      curData$siteID[curData$trapNumber>9 & curData$trapNumber<100]=paste(lakeID,".0",curData$trapNumber[curData$trapNumber>9 & curData$trapNumber<100],sep="")
      curData$siteID[curData$trapNumber>99]=paste(lakeID,".",curData$trapNumber[curData$trapNumber>99],sep="")
      
      #need to be sure that the column names in the entry template are the same as in the database 
      #(change datasheet to match this too); then ones that aren't in curData colnames, get NA and
      #others will match
    
      #generate date info and date strings for sample and fish IDs
      dateSet=strftime(strptime(dateTimeSet,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
      dateSample=strftime(strptime(dateTimeSample,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")

      dateSampleString=strftime(strptime(dateTimeSample,format="%Y-%m-%d %H:%M:%S"),format="%Y%m%d")
      timeSampleString=strftime(strptime(dateTimeSample,format="%Y-%m-%d %H:%M:%S"),format="%H%M")
    
      # generate FISH_INFO rows
      # pull only traps with fish in them from curData
      toFI=curData[curData$species!="NFC",]
      if(sum(curData$species!="NFC")==0){
        fishInfoNEW=data.frame(projectID=rep(projectID,nrow(toFI)),
                            sampleID=paste(toFI$siteID,dateSampleString,timeSampleString,gear,metadataID,sep="_"),
                            fishID=paste(toFI$siteID,dateSampleString,timeSampleString,gear,metadataID,toFI$fishNum,sep="_"),
                            fishNum=if("fishNum"%in%colnames(toFI)) as.numeric(toFI$fishNum) else NA,  
                            species=if("species"%in%colnames(toFI)) toFI$species else NA,
                            fishLength=if("fishLength"%in%colnames(toFI)) toFI$fishLength else NA,
                            standardLength=if("standardLength"%in%colnames(toFI)) toFI$standardLength else NA,
                            fishWeight=if("fishWeight"%in%colnames(toFI)) toFI$fishWeight else NA,
                            caughtBy=if("caughtBy"%in%colnames(toFI)) toFI$caughtBy else NA,
                            jumperDescription=if("jumperDescription"%in%colnames(toFI)) toFI$jumperDescription else NA,
                            useTagMarkRecap=if("useTagMarkRecap"%in%colnames(toFI)) toFI$useTagMarkRecap else NA,
                            tagID=if("tagID"%in%colnames(toFI)) toFI$tagID else NA,
                            oldTag=if("oldTag"%in%colnames(toFI)) toFI$oldATag else NA,
                            tagApply=if("tagApply"%in%colnames(toFI)) toFI$tagApply else NA,
                            tagRecapture=if("tagRecapture"%in%colnames(toFI)) toFI$tagRecapture else NA,
                            tagApplyType=if("tagApplyType"%in%colnames(toFI)) toFI$tagApplyType else NA,
                            tagRecaptureType=if("tagRecaptureType"%in%colnames(toFI)) toFI$tagRecaptureType else NA,
                            clipApply=if("clipApply"%in%colnames(toFI)) toFI$clipApply else NA,
                            clipRecapture=if("clipRecapture"%in%colnames(toFI)) toFI$clipRecapture else NA,
                            sex=if("sex"%in%colnames(toFI)) toFI$sex else NA,
                            mortality=if("mortality"%in%colnames(toFI)) toFI$mortality else NA,
                            removed=if("removed"%in%colnames(toFI)) toFI$removed else NA,
                            otolithSample=if("otolithSample"%in%colnames(toFI)) toFI$otolithSample else NA,
                            tissueSampled=if("tissueSampled"%in%colnames(toFI)) toFI$tissueSampled else NA,
                            dietSampled=if("dietSampled"%in%colnames(toFI)) toFI$dietSampled else NA,
                            stomachRemoved=if("stomachRemoved"%in%colnames(toFI)) toFI$stomachRemoved else NA,
                            gillArchRemoved=if("gillArchRemoved"%in%colnames(toFI)) toFI$gillArchRemoved else NA,
                            pectoralFinRemoved=if("pectoralFinRemoved"%in%colnames(toFI)) toFI$pectoralFinRemoved else NA,
                            gonadRemoved=if("gonadRemoved"%in%colnames(toFI)) toFI$gonadRemoved else NA,
                            leftEyeRemoved=if("leftEyeRemoved"%in%colnames(toFI)) toFI$leftEyeRemoved else NA,
                            finClipCollected=if("finClipCollected"%in%colnames(toFI)) toFI$finClipCollected else NA,
                            photo=if("photo"%in%colnames(toFI)) toFI$photo else NA,
                            gonadWeight=if("gonadWeight"%in%colnames(toFI)) toFI$gonadWeight else NA,
                            rectalTemp=if("rectalTemp"%in%colnames(toFI)) toFI$rectalTemp else NA,
                            gonadSqueze=if("gonadSqueze"%in%colnames(toFI)) toFI$gonadSqueze else NA,
                            sexualStage_MaierScale=if("sexualStage_MaierScale"%in%colnames(toFI)) toFI$sexualStage_MaierScale else NA,
                            gonadWeight=if("gonadWeight"%in%colnames(toFI)) toFI$gonadWeight else NA,
                            gpsWaypoint=if("gpsWaypoint"%in%colnames(toFI)) toFI$gpsWaypoint else NA,
                            finClipBox=if("finClipBox"%in%colnames(toFI)) toFI$finClipBox else NA,
                            spineSample=if("spineSampled"%in%colnames(curData)) toFI$spineSampled else NA,
                            scaleSample=if("scaleSampled"%in%colnames(curData)) toFI$scaleSampled else NA,
                            comments=if("comments"%in%colnames(toFI)) toFI$comments else NA,
                            entryFile=toCompile[i],
                            stringsAsFactors=FALSE)
      
        # convert species abbreviations to common names
        if(any(!fishInfoNEW$species%in%fishNames$abbreviation)){
          if(force_species==FALSE){
            stop(paste("the species (",unique(fishInfoNEW$species[!fishInfoNEW$species%in%fishNames$abbreviation]),") is not in the MFE database nor in this season's working database; if you are certain this is the correct species name, use the argument force_species to add it to this season's working database.",sep=""))
          }
        }
        abbrevs=unique(fishInfoNEW$species)
        for(j in 1:length(abbrevs)){
          fishInfoNEW$species[fishInfoNEW$species==abbrevs[j]]=fishNames$commonName[fishNames$abbreviation==abbrevs[j]]
        }
      }
    
      # generate FISH_SAMPLES rows
      # remove duplicate rows from same trap
      toFS=curData[!duplicated(curData$siteID),]
      fishSamplesNEW=data.frame(siteID=toFS$siteID,
                                sampleID=paste(toFS$siteID,dateSampleString,timeSampleString,gear,metadataID,sep="_"),
                                dayOfYear=as.numeric(strftime(strptime(dateSample,format="%Y-%m-%d"),format="%j")),
                                dateSet=dateSet,
                                dateSample=dateSample,
                                dateTimeSet=dateTimeSet,
                                dateTimeSample=dateTimeSample,
                                crew=crew,
                                gear=gear,
                                sampleGroup=sampleGroup,
                                effort=effort,
                                effortUnits=effortUnits,
                                distanceShocked=distanceShocked,
                                useCPUE=useCPUE,
                                useSampleMarkRecap=useSampleMarkRecap,
                                comments=comments,
                                metadataID=metadataID,
                                entryFile=toCompile[i],
                                stringsAsFactors=FALSE)

      # run checks against in-season database and the full database for...
      
      # lakeID already in database
      lakeIDsIS=substr(fishSamplesIS$siteID,start=1,stop=2)
      if(!lakeID%in%c(lakesDB$lakeID,lakeIDsIS)){
        if(force_lakeID==FALSE){
          stop(paste("your lakeID (",lakeID,") is not in the MFE database nor in this season's working database; if you are certain this is the correct lakeID, use the argument force_lakeID to add a new lakeID to this season's working database.",sep=""))
        }
      }
      
      # siteID already in database
      if(any(!fishSamplesNEW$siteID%in%c(fishSamplesIS$siteID,fishSamplesDB$siteID))){
        if(force_siteID==FALSE){
          stop(paste("your siteID (",fishSamplesNEW$siteID[!fishSamplesNEW$siteID%in%c(fishSamplesIS$siteID,fishSamplesDB$siteID)],") is not in the MFE database nor in this season's working database; if you are certain this is the correct siteID, use the argument force_siteID to add a new siteID to this season's working database.",sep=""))
        }
      }
      
      # sampleID NOT already in database
      if(any(fishSamplesNEW$sampleID%in%c(fishSamplesIS$sampleID,fishSamplesDB$sampleID))){
        stop(paste("your sampleID (",fishSamplesNEW$sampleID[fishSamplesNEW$sampleID%in%c(fishSamplesIS$sampleID,fishSamplesDB$sampleID)],") is already in the MFE database or in this season's working database!",sep=""))
      }
      
      # dayOfYear in an acceptable range
      if(any(fishSamplesNEW$dayOfYear<91 | fishSamplesNEW$dayOfYear>305)){
        if(force_dayOfYear==FALSE){
          stop(paste("your dayOfYear (",fishSamplesNEW$dayOfYear[fishSamplesNEW$dayOfYear<91 | fishSamplesNEW$dayOfYear>305],") is not in the usual range; if your dayOfYear is correct use the argument force_dayOfYear",sep=""))
        }
      }
      
      #dateSet must be the same or earlier than dateSample?
      if(as.Date(dateSample)<as.Date(dateSet)){
        stop("Your dateSample is < your dateSet!")
      }
      
      #dateTimeSet must be the same or earlier than dateTimeSample?
      if(strptime(dateTimeSample,format="%Y-%m-%d %H:%M:%S")<strptime(dateTimeSet,format="%Y-%m-%d %H:%M:%S")){
        stop("Your dateTimeSample is < your dateTimeSet!")
      }
      
      # gear already in database
      if(any(!fishSamplesNEW$gear%in%c(fishSamplesIS$gear,fishSamplesDB$gear))){
        if(force_gear==FALSE){
          stop(paste("your gear (",unique(fishSamplesNEW$gear),") is not in the MFE database nor in this season's working database; if you are certain this is the correct gear, use the argument force_gear to add a new gear to this season's working database.",sep=""))
        }
      }
      
      # sampleGroup already in database????
      if(any(!fishSamplesNEW$sampleGroup%in%c(fishSamplesIS$sampleGroup,fishSamplesDB$sampleGroup))){
        if(force_sampleGroup==FALSE){
          stop(paste("your sampleGroup (",unique(fishSamplesNEW$sampleGroup),") is not in the MFE database nor in this season's working database; if you are certain this is the correct sampleGroup, use the argument force_sampleGroup to add a new sampleGroup to this season's working database.",sep=""))
        }
      }
      
      # effort in an acceptable range
      if(any(fishSamplesNEW$effort<0 | fishSamplesNEW$effort>72)){
        if(force_effort==FALSE){
          stop(paste("your effort (",fishSamplesNew$effort[fishSamplesNEW$effort<0 | fishSamplesNEW$effort>72],") is outside the normal range; if you are certain this is the correct effort use the argument force_effort."))
        }
      }
      
      # effortUnits already in database????
      if(any(!fishSamplesNEW$effortUnits%in%c(fishSamplesIS$effortUnits,fishSamplesDB$effortUnits))){
        if(force_effortUnits==FALSE){
          stop(paste("your effortUnits (",fishSamplesNEW$effortUnits[!fishSamplesNEW$effortUnits%in%c(fishSamplesIS$effortUnits,fishSamplesDB$effortUnits)],") is not in the MFE database nor in this season's working database; if you are certain this is the correct effortUnits, use the argument force_effortUnits to add a new effortUnits to this season's working database.",sep=""))
        }
      }
      
      # useCPUE already in database????
      if(any(!fishSamplesNEW$useCPUE%in%c(fishSamplesIS$useCPUE,fishSamplesDB$useCPUE))){
        stop(paste("your useCPUE (",fishSamplesNEW$useCPUE[!fishSamplesNEW$useCPUE%in%c(fishSamplesIS$useCPUE,fishSamplesDB$useCPUE)],") is not an acceptable value",sep=""))
      }
      
      # useSampleMarkRecap already in database????
      if(any(!fishSamplesNEW$useSampleMarkRecap%in%c(fishSamplesIS$useSampleMarkRecap,fishSamplesDB$useSampleMarkRecap))){
        stop(paste("your useSampleMarkRecap (",fishSamplesNEW$useSampleMarkRecap[!fishSamplesNEW$useSampleMarkRecap%in%c(fishSamplesIS$useSampleMarkRecap,fishSamplesDB$useSampleMarkRecap)],") is not an acceptable value",sep=""))
      }
      
      # metadataID already in database
      if(any(!fishSamplesNEW$metadataID%in%c(fishSamplesIS$metadataID,fishSamplesDB$metadataID))){
        if(force_metadataID==FALSE){
          stop(paste("your metadataID (",fishSamplesNEW$metadataID[!fishSamplesNEW$metadataID%in%c(fishSamplesIS$metadataID,fishSamplesDB$metadataID)],") is not in the MFE database nor in this season's working database; if you are certain this is the correct metadataID, use the argument force_metadataID to add a new metadataID to this season's working database.",sep=""))
        }
      }
      
      # projectID already in database
      if(any(!fishInfoNEW$projectID[1]%in%c(fishInfoIS$projectID,fishInfoDB$projectID))){
        if(force_projectID==FALSE){
          stop(paste("your projectID (",fishInfoNEW$projectID[!fishInfoNEW$projectID[1]%in%c(fishInfoIS$projectID,fishInfoDB$projectID)],") is not in the MFE database nor in this season's working database; if you are certain this is the correct projectID, use the argument force_projectID to add a new projectID to this season's working database.",sep=""))
        }
      }
      
      # species names
      if(any(!fishInfoNEW$species%in%c(fishInfoIS$species,fishInfoDB$species))){
        if(force_species==FALSE){
          stop(paste("the species (",unique(fishInfoNEW$species[!fishInfoNEW$species%in%c(fishInfoIS$species,fishInfoDB$species)]),") is not in the MFE database nor in this season's working database; if you are certain this is the correct species name, use the argument force_species to add it to this season's working database.",sep=""))
        }
      }
      
      # size and weight bounds
      uniqSpec=unique(fishInfoNEW$species)
      for(j in 1:length(uniqSpec)){
        curDB=fishInfoDB[(fishInfoDB$species==uniqSpec[j] & !is.na(fishInfoDB$fishLength)),]
        curDB=curDB[curDB$fishLength>0,]
        curNEW=fishInfoNEW[fishInfoNEW$species==uniqSpec[j],]
        
        # range check on lengths, if those data were collected
        if(any(curNEW$fishLength>0 | !is.na(curNEW$fishLength))){
          if(nrow(curDB)>15){
            if(any(curNEW$fishLength<=0)){
              stop(paste("You report negative fishLength in sample ",paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,sep="_"),
                        "; Check fish:",paste(curNEW$fishID[curNEW$fishLength<=0],collapse=", "),sep=""))
            }
            if(any(curNEW$fishLength>(mean(curDB$fishLength)+3*sd(curDB$fishLength)))){
              if(force_fishLength==FALSE){
                stop(paste("You report fish longer than 3 standard deviations above the mean ever observed by us for",uniqSpec[j],"in sample",paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,sep="_"),"if you are certain you have fishLength correct for all individuals use the argument force_fishLength;",
                           "Check fish: ",paste(curNEW$fishID[curNEW$fishLength>(mean(curDB$fishLength)+3*sd(curDB$fishLength))],collapse=", "),sep=" "))
              }
            }    
          }else{
            warning(paste("We have less than 15 observations for length of species",uniqSpec[j]," and will not be running an automated fishLength check. Be sure to double check the lengths you've entered.",sep=" "))
          }
        }
      
        # check on weights (based on length-weight regression), if those data were collected
        if(any(curNEW$fishWeight>0 | !is.na(curNEW$fishWeight))){
          curDB=curDB[!is.na(curDB$fishWeight),]
          curDB=curDB[curDB$fishWeight>0,]
          if(nrow(curDB)>15){
            if(any(curNEW$fishWeight<=0)){
              stop(paste("You report negative fishWeight in sample ",paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,sep="_"),
                         ". Check fish: ",paste(curNEW$fishID[curNEW$fishWeight<=0],collapse=", "),sep=""))
            }
            #fit length-weight regression for uniqSpec[j]
            curDB$logWeight=log(curDB$fishWeight)
            curDB$logLength=log(curDB$fishLength)
            lwreg=lm(logWeight~logLength,data=curDB)
            preds=predict(lwreg,newdata=data.frame(logLength=log(as.numeric(curNEW$fishLength))),interval="prediction",se.fit=TRUE,level=0.99)
            predsLow=exp(preds$fit[,2])
            predsHigh=exp(preds$fit[,3])
            
            if(any(curNEW$fishWeight<predsLow | curNEW$fishWeight>predsHigh)){
              if(force_fishWeight==FALSE){
                stop(paste("You report fishWeight outside the prediction based on a length-weight regression from our database for",uniqSpec[j],"in sample",paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,sep="_"),"if you are certain you have fishWeight correct for all individuals use the argument force_fishWeight;",
                           "The check fish:",paste(curNEW$fishID[curNEW$fishWeight<predsLow | curNEW$fishWeight>predsHigh],collapse=", "),sep=" "))
              }
            }
          }else{
            warning(paste("We have less than 15 observations for weight of species",uniqSpec[j]," and will not be running an automated fishWeight check. Be sure to double check the lengths you've entered.",sep=" "))
          }
          
        }
        
      }
      
      # -clipApply & clipRecapture should be in the database
      clips=c(curNEW$clipApply,curNEW$clipRecapture)
      clips=clips[!is.na(clips)]
      if(length(clips)>0){
        if(any(!clips%in%c(fishInfoDB$clipApply,fishInfoDB$clipRecapture,fishInfoIS$clipApply,fishInfoIS$clipRecapture))){
          if(force_clip==FALSE){
            stop("You have indicated a clipApply or clipRecapture that is not in the database or in-season database. If you are certain this is the correct clipApply or clipRecapture then use the argument force_clip.")
          }
        }
      }
        
      #check for clipApply of that type in that lake
      clipRecap=curNEW$clipRecapture[!is.na(curNEW$clipRecapture)]
      if(length(clipRecap)>0){
        if(any(!unique(clipRecap)%in%c(fishInfoDB$clipApply[grepl(lakeID,fishInfoDB$sampleID)],fishInfoIS$clipApply[grepl(lakeID,fishInfoIS$sampleID)]))){
          stop("You have indicated a clipRecapture that has never been applied in the lake you sampled.")
        }
      }
      
    
      # update tables with new entries
      fishSamplesIS=rbind(fishSamplesIS,fishSamplesNEW)
      fishInfoIS=rbind(fishInfoIS,fishInfoNEW)
    }
    
    # write updates to files
    write.csv(fishInfoIS,"fishInfoIS.csv",row.names=FALSE)
    write.csv(fishSamplesIS,"fishSamplesIS.csv",row.names=FALSE)
  }
}