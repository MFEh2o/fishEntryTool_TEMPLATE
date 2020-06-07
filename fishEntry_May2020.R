# function for generating in-season database files from fish datasheets
# 2019-03-20 - updated for LongLake by AKA


Sys.setenv(tz="America/Chicago")

updateFish<-function(headerRows=18,dbdir="C:/Users/Amary/Documents/MFEDB/database/",db="MFEdb.db",funcdir="C:/Users/Amary/Documents/MFEDB/fishEntryTool/", # set these to the locations on your computer
                     force_lakeID=FALSE,
                     force_siteID=FALSE,
                     force_dayOfYear=FALSE,
                     force_gear=FALSE,
                     force_sampleGroup=FALSE,
                     force_effort=FALSE,
                     force_effortUnits=FALSE,
                     force_distanceShocked=FALSE,
                     force_metadataID=FALSE,
                     force_projectID=FALSE,
                     force_species=FALSE,
                     force_fishLength=FALSE,
                     force_fishWeight=FALSE,
                     force_clip=FALSE
                    ){

  source(paste(funcdir,"dbUtil.R",sep=""))
  
  # load tables from database
  lakesDB=dbTable("LAKES")
  fishSamplesDB=dbTable("FISH_SAMPLES")
  fishInfoDB=dbTable("FISH_INFO")
  otu=dbTable("OTU")
  fishNames=otu[otu$abbreviation!="NA",]

  # load in-season database files
  if("fishInfoIS.csv"%in%list.files()){
    fishInfoIS=read.csv("fishInfoIS.csv",header=TRUE,stringsAsFactors=FALSE)
  }else{
    fishInfoIS=setNames(data.frame(matrix(ncol=ncol(fishInfoDB),nrow=0)),colnames(fishInfoDB))
  }
  if("fishSamplesIS.csv"%in%list.files()){
    fishSamplesIS=read.csv("fishSamplesIS.csv",header=TRUE,stringsAsFactors=FALSE)
  }else{
    fishSamplesIS=setNames(data.frame(matrix(ncol=ncol(fishSamplesDB),nrow=0)),colnames(fishSamplesDB))
  }

  # check which files have been compiled and which have not in the directory
  beenCompiled=unique(fishInfoIS$entryFile)
  toCompile=list.files(pattern="[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{4}\\.csv")
  toCompile=toCompile[!(toCompile%in%beenCompiled)]
  toCompile=toCompile[grepl("angling",toCompile)]
  
  if(length(toCompile)==0){
    #no files that have not been compiled into the in-season database
    "The in season database is up to date; no new files to compile"
  }else{
    #there are files to be compiled; generate rows to append
    for(i in 1:length(toCompile)){
      cur=scan(toCompile[i],what=character(),sep ="\n")
    
      #pull header info
      projectID=as.numeric(strsplit(cur[1],",")[[1]][2])
      lakeID=strsplit(cur[2],",")[[1]][2]
      siteName=strsplit(cur[3],",")[[1]][2]
      dateTimeSet=strsplit(cur[4],",")[[1]][2]
      dateTimeSet=paste(dateTimeSet,":00",sep="")
      dateTimeSample=strsplit(cur[5],",")[[1]][2]
      dateTimeSample=paste(dateTimeSample,":00",sep="")
      crew=gsub("[\"]", "", cur[6])
      crew=strsplit(crew,",")[[1]]
      crew=crew[-1];crew=crew[crew!=""]
      crew=paste(crew, collapse = ",")
      gear=strsplit(cur[7],",")[[1]][2]
      distanceShocked=ifelse(gear=="BE",strsplit(cur[8],",")[[1]][2], 0)
      effort=as.numeric(strsplit(cur[9],",")[[1]][2])
      effortUnits=strsplit(cur[10],",")[[1]][2]
      comments=strsplit(cur[11],",")[[1]][2]
      comments=ifelse(comments=="",NA,comments)
      useCPUE=strsplit(cur[12],",")[[1]][2]
      dataRecorder=strsplit(cur[13],",")[[1]][2]
      dataEnteredBy=strsplit(cur[14],",")[[1]][2]
      metadataID=strsplit(cur[15],",")[[1]][2]
      useSampleMarkRecap=strsplit(cur[16],",")[[1]][2]
      sampleGroup=strsplit(cur[17],",")[[1]][2]
    
      # add a check that these are non-empty springs or stop and tell user the header info is incomplete
      headerVals=c(projectID,lakeID,siteName,dateTimeSet,dateTimeSample,crew,gear,distanceShocked,effort,effortUnits,comments,useCPUE,dataRecorder,dataEnteredBy,metadataID,useSampleMarkRecap,sampleGroup)
      if(any(headerVals=="",na.rm=TRUE)){
        headerText=c("projectID","lakeID","siteName","dateTimeSet","dateTimeSample","crew","gear","distanceShocked","effort","effortUnits","comments","useCPUE","dataRecorder","dataEnteredBy","metadataID","useSampleMarkRecap","sampleGroup")
        stop(paste("required header information is incomplete; you're missing: ",headerText[headerVals==""],sep=","))
      }
    
      #tabular data
      curData=data.frame(matrix(unlist(strsplit(cur[(headerRows+1):length(cur)],",")),nrow=length(cur)-headerRows,byrow=TRUE),stringsAsFactors=FALSE)
      colnames(curData)=strsplit(cur[headerRows],",")[[1]]
      curData$fishNum=as.numeric(curData$fishNum)
      curData$fishLength=as.numeric(curData$fishLength)
      curData$fishWeight=as.numeric(curData$fishWeight)
    
      #need to be sure that the column names in the entry template are the same as in the database 
      #(change datasheet to match this too); then ones that aren't in curData colnames, get NA and
      #others will match
    
      #generate date info and date strings for sample and fish IDs
      dateSet=strftime(strptime(dateTimeSet,format="%m/%d/%Y %H:%M:%S"),format="%Y-%m-%d")
      dateSample=strftime(strptime(dateTimeSample,format="%m/%d/%Y %H:%M:%S"),format="%Y-%m-%d")

      dateSampleString=strftime(strptime(dateTimeSample,format="%m/%d/%Y %H:%M:%S"),format="%Y%m%d")
      timeSampleString=strftime(strptime(dateTimeSample,format="%m/%d/%Y %H:%M:%S"),format="%H%M")
    
      # generate FISH_INFO rows
      fishInfoNEW=data.frame(projectID=rep(projectID,nrow(curData)),
                          sampleID=rep(paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,sep="_"),nrow(curData)),
                          fishID=paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,curData$fishNum,sep="_"),
                          fishNum=if("fishNum"%in%colnames(curData)) as.numeric(curData$fishNum) else NA,  
                          species=if("species"%in%colnames(curData)) curData$species else NA,
                          fishLength=if("fishLength"%in%colnames(curData)) curData$fishLength else NA,
                          standardLength=if("standardLength"%in%colnames(curData)) curData$standardLength else NA,
                          fishWeight=if("fishWeight"%in%colnames(curData)) curData$fishWeight else NA,
                          caughtBy=if("caughtBy"%in%colnames(curData)) curData$caughtBy else NA,
                          jumperDescription=if("jumperDescription"%in%colnames(curData)) curData$jumperDescription else NA,
                          useTagMarkRecap=if("useTagMarkRecap"%in%colnames(curData)) curData$useTagMarkRecap else NA,
                          tagID=if("tagID"%in%colnames(curData)) curData$tagID else NA,
                          oldTag=if("oldTag"%in%colnames(curData)) curData$oldTag else NA,
                          tagApply=if("tagApply"%in%colnames(curData)) curData$tagApply else NA,
                          tagRecapture=if("tagRecapture"%in%colnames(curData)) curData$tagRecapture else NA,
                          tagApplyType=if("tagApplyType"%in%colnames(curData)) curData$tagApplyType else NA,
                          tagRecaptureType=if("tagRecaptureType"%in%colnames(curData)) curData$tagRecaptureType else NA,
                          clipApply=if("clipApply"%in%colnames(curData)) curData$clipApply else NA,
                          clipRecapture=if("clipRecapture"%in%colnames(curData)) curData$clipRecapture else NA,
                          sex=if("sex"%in%colnames(curData)) curData$sex else NA,
                          mortality=if("mortality"%in%colnames(curData)) curData$mortality else NA,
                          removed=if("removed"%in%colnames(curData)) curData$removed else NA,
                          otolithSampled=if("otolithSampled"%in%colnames(curData)) curData$otolithSampled else NA,
                          tissueSampled=if("tissueSampled"%in%colnames(curData)) curData$tissueSampled else NA,
                          dietSampled=if("dietSampled"%in%colnames(curData)) curData$dietSampled else NA,
                          stomachRemoved=if("stomachRemoved"%in%colnames(curData)) curData$stomachRemoved else NA,
                          gillArchRemoved=if("gillArchRemoved"%in%colnames(curData)) curData$gillArchRemoved else NA,
                          pectoralFinRemoved=if("pectoralFinRemoved"%in%colnames(curData)) curData$pectoralFinRemoved else NA,
                          gonadRemoved=if("gonadRemoved"%in%colnames(curData)) curData$gonadRemoved else NA,
                          leftEyeRemoved=if("leftEyeRemoved"%in%colnames(curData)) curData$leftEyeRemoved else NA,
                          finClipCollected=if("finClipCollected"%in%colnames(curData)) curData$finClipCollected else NA,
                          photo=if("photo"%in%colnames(curData)) curData$photo else NA,
                          gonadWeight=if("gonadWeight"%in%colnames(curData)) curData$gonadWeight else NA,
                          rectalTemp=if("rectalTemp"%in%colnames(curData)) curData$rectalTemp else NA,
                          gonadSqueeze=if("gonadSqueeze"%in%colnames(curData)) curData$gonadSqueeze else NA,
                          sexualStage_MaierScale=if("sexualStage_MaierScale"%in%colnames(curData)) curData$sexualStage_MaierScale else NA,
                          gpsWaypoint=if("gpsWaypoint"%in%colnames(curData)) curData$gpsWaypoint else NA,
                          finClipBox=if("finClipBox"%in%colnames(curData)) curData$finClipBox else NA,
                          spineSample=if("spineSampled"%in%colnames(curData)) curData$spineSampled else NA,
                          scaleSample=if("scaleSampled"%in%colnames(curData)) curData$scaleSampled else NA,
                          comments=if("comments"%in%colnames(curData)) curData$comments else NA,
                        
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
        if(abbrevs[j]%in%c("minnow", "BFN", "RHS", "unidentifiable", "PKL")==T){
          if(abbrevs[j]=="RHS"){
            fishInfoNEW$species[fishInfoNEW$species=="RHS"]="redhorse"
          }else{
            if(abbrevs[j]=="unidentifiable"){
              fishInfoNEW$species[fishInfoNEW$species=="unidentifiable"]="fish_unidentifiable"
            }else{
              if(abbrevs[j]=="PKL"){
                fishInfoNEW$species[fishInfoNEW$species=="PKL"]="grass_pickerel"
              }else{
                fishInfoNEW$species[fishInfoNEW$species%in%c("minnow", "BFN")==T]=ifelse(fishInfoNEW$species[fishInfoNEW$species%in%c("minnow", "BFN")]=="minnow", "minnow", "bowfin")
              }
            }
          }
        }else{
        fishInfoNEW$species[fishInfoNEW$species==abbrevs[j]]=fishNames$commonName[fishNames$abbreviation==abbrevs[j]]
        }
      }
        
    
      # generate FISH_SAMPLES rows
      fishSamplesNEW=data.frame(siteID=paste(lakeID,siteName,sep="_"),
                                sampleID=paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,sep="_"),
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

      # check for otoliths pulled and generate a log of fish otoliths
      if("otolithSample"%in%colnames(curData) & any(curData$otolithSample==1)){
        if("fishOtolithsLOG.csv"%in%list.files()){
          fishOtolithsLOG=read.csv("fishOtolithsLOG.csv",header=TRUE,stringsAsFactors=FALSE)
        }else{
          fishOtolithsLOG=data.frame()
        }
      
      
        fishOtolithsNEW=data.frame(fishID=paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,curData$fishNum,sep="_")[curData$otolithSample==1],
                                   lengthAtCapture=curData$length[curData$otolithSample==1],
                                   weightAtCapture=curData$weight[curData$otolithSample==1],
                                   otolithWeight=""
                                  )
      }  
      # check for spines pulled and generate a log of fish spines
      if("spineSample"%in%colnames(curData) & any(curData$spineSample==1)){
        if("fishspinesLOG.csv"%in%list.files()){
          fishspinesLOG=read.csv("fishspinesLOG.csv",header=TRUE,stringsAsFactors=FALSE)
        }else{
          fishspinesLOG=data.frame()
        }
        
        
        fishspinesNEW=data.frame(fishID=paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,curData$fishNum,sep="_")[curData$spineSample==1],
                                   lengthAtCapture=curData$length[curData$spineSample==1],
                                   weightAtCapture=curData$weight[curData$spineSample==1]
        )
      }      
      
      # check for scales pulled and generate a log of fish scales
      if("scaleSample"%in%colnames(curData) & any(curData$scaleSample==1)){
        if("fishscalesLOG.csv"%in%list.files()){
          fishscalesLOG=read.csv("fishscalesLOG.csv",header=TRUE,stringsAsFactors=FALSE)
        }else{
          fishscalesLOG=data.frame()
        }
        
        
        fishscalesNEW=data.frame(fishID=paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,curData$fishNum,sep="_")[curData$scaleSample==1],
                                 lengthAtCapture=curData$length[curData$scaleSample==1],
                                 weightAtCapture=curData$weight[curData$scaleSample==1]
        )
      }      
      
      # check for diets taken and generate a log of diets
      if("dietSampled"%in%colnames(curData) & any(curData$dietSampled==1)){
        if("fishDietsLOG.csv"%in%list.files()){
          fishDietsLOG=read.csv("fishDietsLOG.csv",header=TRUE,stringsAsFactors=FALSE)
        }else{
          fishDietsLOG=data.frame()
        }
      
        fishDietsNEW=data.frame(fishID=paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,curData$fishNum,sep="_")[curData$dietSample==1],
                                lakeID=rep(lakeID, sum(curData$dietSampled==1)),
                                dateSample=rep(dateSample, sum(curData$dietSampled==1)),
                                species=curData$species[curData$dietSampled==1]
                                )
      }

      # run checks against in-season database and the full database for...
      
      # lakeID already in database
      lakeIDsIS=substr(fishSamplesIS$siteID,start=1,stop=2)
      if(!lakeID%in%c(lakesDB$lakeID,lakeIDsIS)){
        if(force_lakeID==FALSE){
          stop(paste("your lakeID (",lakeID,") is not in the MFE database nor in this season's working database; if you are certain this is the correct lakeID, use the argument force_lakeID to add a new lakeID to this season's working database.",sep=""))
        }
      }
      
      # siteID already in database
      if(!fishSamplesNEW$siteID%in%c(fishSamplesIS$siteID,fishSamplesDB$siteID)){
        if(force_siteID==FALSE){
          stop(paste("your siteID (",fishSamplesNEW$siteID,") is not in the MFE database nor in this season's working database; if you are certain this is the correct siteID, use the argument force_siteID to add a new siteID to this season's working database.",sep=""))
        }
      }
      
      # sampleID NOT already in database
      if(fishSamplesNEW$sampleID%in%c(fishSamplesIS$sampleID,fishSamplesDB$sampleID)){
        stop(paste("your sampleID (",fishSamplesNEW$sampleID,") is already in the MFE database or in this season's working database!",sep=""))
      }
      
      # dayOfYear in an acceptable range
      if(fishSamplesNEW$dayOfYear<91 | fishSamplesNEW$dayOfYear>305){
        if(force_dayOfYear==FALSE){
          stop(paste("your dayOfYear (",fishSamplesNEW$dayOfYear,") is not in the usual range; if your dayOfYear is correct use the argument force_dayOfYear",sep=""))
        }
      }
      
      #dateSet must be the same or earlier than dateSample?
      if(as.Date(dateSample)<as.Date(dateSet)){
        stop("Your dateSample is < your dateSet!")
      }
      
      #dateTimeSet must be the same or earlier than dateTimeSample?
      if(strptime(dateTimeSample,format="%m/%d/%Y %H:%M:%S")<strptime(dateTimeSet,format="%m/%d/%Y %H:%M:%S")){
        stop("Your dateTimeSample is < your dateTimeSet!")
      }
      
      # gear already in database
      if(!fishSamplesNEW$gear%in%c(fishSamplesIS$gear,fishSamplesDB$gear)){
        if(force_gear==FALSE){
          stop(paste("your gear (",fishSamplesNEW$gear,") is not in the MFE database nor in this season's working database; if you are certain this is the correct gear, use the argument force_gear to add a new gear to this season's working database.",sep=""))
        }
      }
      
      # sampleGroup already in database????
      if(!fishSamplesNEW$sampleGroup%in%c(fishSamplesIS$sampleGroup,fishSamplesDB$sampleGroup)){
        if(force_sampleGroup==FALSE){
          stop(paste("your sampleGroup (",fishSamplesNEW$sampleGroup,") is not in the MFE database nor in this season's working database; if you are certain this is the correct sampleGroup, use the argument force_sampleGroup to add a new sampleGroup to this season's working database.",sep=""))
        }
      }
      
      # effort in an acceptable range
      if(fishSamplesNEW$effort<=0 | fishSamplesNEW$effort>24){
        if(force_effort==FALSE){
          stop(paste("your effort (",fishSamplesNew$effort,") is outside the normal range; if you are certain this is the correct effort use the argument force_effort."))
        }
      }
      
      # effortUnits already in database????
      if(!fishSamplesNEW$effortUnits%in%c(fishSamplesIS$effortUnits,fishSamplesDB$effortUnits)){
        if(force_effortUnits==FALSE){
          stop(paste("your effortUnits (",fishSamplesNEW$effortUnits,") is not in the MFE database nor in this season's working database; if you are certain this is the correct effortUnits, use the argument force_effortUnits to add a new effortUnits to this season's working database.",sep=""))
        }
      }
      
      # distanceShocked in an acceptable range
      if(fishSamplesNEW$distanceShocked<0 | fishSamplesNEW$distanceShocked>25){
        if(force_distanceShocked==FALSE){
          stop(paste("your distanceShocked (",fishSamplesNEW$distanceShocked,") is outside the normal range; if you are certain this is the correct distanceShocked use the argument force_distanceShocked."))
        }
      }
      
      # useCPUE already in database????
      if(!fishSamplesNEW$useCPUE%in%c(fishSamplesIS$useCPUE,fishSamplesDB$useCPUE)){
        stop(paste("your useCPUE (",fishSamplesNEW$useCPUE,") is not an acceptable value",sep=""))
      }
      
      # useSampleMarkRecap already in database????
      if(!fishSamplesNEW$useSampleMarkRecap%in%c(fishSamplesIS$useSampleMarkRecap,fishSamplesDB$useSampleMarkRecap)){
        stop(paste("your useSampleMarkRecap (",fishSamplesNEW$useSampleMarkRecap,") is not an acceptable value",sep=""))
      }
      
      # metadataID already in database
      if(!fishSamplesNEW$metadataID%in%c(fishSamplesIS$metadataID,fishSamplesDB$metadataID)){
        if(force_metadataID==FALSE){
          stop(paste("your metadataID (",fishSamplesNEW$metadataID,") is not in the MFE database nor in this season's working database; if you are certain this is the correct metadataID, use the argument force_metadataID to add a new metadataID to this season's working database.",sep=""))
        }
      }
      
      # projectID already in database
      if(!fishInfoNEW$projectID[1]%in%c(fishInfoIS$projectID,fishInfoDB$projectID)){
        if(force_projectID==FALSE){
          stop(paste("your projectID (",fishInfoNEW$projectID[1],") is not in the MFE database nor in this season's working database; if you are certain this is the correct projectID, use the argument force_projectID to add a new projectID to this season's working database.",sep=""))
        }
      }
      
      # species names
      if(any(!fishInfoNEW$species%in%c(fishInfoIS$species,fishInfoDB$species))){
        if(force_species==FALSE){
          stop(paste("the species (",unique(fishInfoNEW$species[!fishInfoNEW$species%in%c(fishInfoIS$species,fishInfoDB$species)]),") is not in the MFE database nor in this season's working database; if you are certain this is the correct species name, use the argument force_species to add it to this season's working database.",sep=""))
        }
      }
      
      # fish number check for duplicates
      if(any(duplicated(fishInfoNEW$fishNum))){
        stop("You have duplicate fish numbers and therefore duplicate fishIDs!")
      }
      
      # size and weight bounds
      uniqSpec=unique(fishInfoNEW$species)
      for(j in 1:length(uniqSpec)){
        curDB=fishInfoDB[(fishInfoDB$species==uniqSpec[j] & !is.na(fishInfoDB$fishLength)),]
        curDB=curDB[curDB$fishLength>0,]
        curNEW=fishInfoNEW[fishInfoNEW$species==uniqSpec[j],]
        
        # range check on lengths, if those data were collected, first if() throws out NFC rows for anglers in some samples if they caught nothing but others in the boat did
        if(any(!is.na(curNEW$fishLength))){
          curNEW_Lcheck=curNEW[!is.na(curNEW$fishLength),]
          if(any(curNEW_Lcheck$fishLength>0)){
            if(nrow(curDB)>15){
              if(any(curNEW_Lcheck$fishLength<=0)){
                stop(paste("You report negative fishLength in sample ",paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,sep="_"),
                        "; Check fish:",paste(curNEW_Lcheck$fishID[curNEW_Lcheck$fishLength<=0],collapse=", "),sep=""))
              }
              if(any(curNEW_Lcheck$fishLength>(mean(curDB$fishLength)+3*sd(curDB$fishLength)))){
                if(force_fishLength==FALSE){
                  stop(paste("You report fish longer than 3 standard deviations above the mean ever observed by us for",uniqSpec[j],"in sample",paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,sep="_"),"if you are certain you have fishLength correct for all individuals use the argument force_fishLength;",
                           "Check fish: ",paste(curNEW_Lcheck$fishID[curNEW_Lcheck$fishLength>(mean(curDB$fishLength)+3*sd(curDB$fishLength))],collapse=", "),sep=" "))
                }
              }    
            }else{
              warning(paste("We have less than 15 observations for length of species",uniqSpec[j]," and will not be running an automated fishLength check. Be sure to double check the lengths you've entered.",sep=" "))
            }
          }
        }
        # check on weights (based on length-weight regression), if those data were collected
        if(any(!is.na(curNEW$fishWeight))){
          curDB=curDB[!is.na(curDB$fishWeight),]
          curDB=curDB[curDB$fishWeight>0,]
          if(nrow(curDB)>15){
            curNEWa<-curNEW[!is.na(curNEW$fishWeight),]
            if(any(curNEWa$fishWeight<=0)){
              stop(paste("You report negative fishWeight in sample ",paste(lakeID,siteName,dateSampleString,timeSampleString,gear,metadataID,sep="_"),
                          ". Check fish: ",paste(curNEW$fishID[curNEW$fishWeight<=0],collapse=", "),sep=""))
             }
             #fit length-weight regression for uniqSpec[j]
             curDB$logWeight=log(curDB$fishWeight)
             curDB$logLength=log(curDB$fishLength)
             lwreg=lm(logWeight~logLength,data=curDB)
             preds=predict(lwreg,newdata=data.frame(logLength=log(as.numeric(curNEWa$fishLength))),interval="prediction",se.fit=TRUE,level=0.99)
             predsLow=exp(preds$fit[,2])
             predsHigh=exp(preds$fit[,3])
             
             if(any(curNEWa$fishWeight<predsLow | curNEWa$fishWeight>predsHigh)){
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
      
      ### tag marks and recaps --> figure what the common prefixes are and distinguish errors in prefix vs. individual number?
      
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
          
          if(force_clip==FALSE){
            stop("You have indicated a clipApply or clipRecapture that is not in the database or in-season database. If you are certain this is the correct clipApply or clipRecapture then use the argument force_clip.")
          }
        }
      }
      
      #tagApply today conflicts with tagApply from previous
      tagsApplied=curNEW$tagApply[!is.na(curNEW$tagApply)]
      tagsApplied=curNEW$tagApply[curNEW$tagApply!=""]
      if(length(tagsApplied)>0){
        if(any(tagsApplied%in%c(fishInfoDB$tagApply,fishInfoIS$tagApply),na.rm=TRUE)){
          for(j in 1:length(tagsApplied)){
            if(tagsApplied[j]%in%fishInfoDB$tagApply){
              tagApplyStop=TRUE
              temp=rbind(fishInfoDB[fishInfoDB$tagApply==tagsApplied[j],c(1:8,12:19,38:39)],curNEW[curNEW$tagApply==tagsApplied[j],c(1:8,12:19,38,41)])
              print(temp)
            }
            if(tagsApplied[i]%in%fishInfoIS$tagApply){
              tagApplyStop=TRUE
              temp=rbind(fishInfoIS[fishInfoIS$tagApply==tagsApplied[j],c(1:8,12:19,38:39)],curNEW[curNEW$tagApply==tagsApplied[j],c(1:8,12:19,38,41)])
              print(temp)
            }
          }
          stop("You are attempting to enter a tagApply that was already recorded as a tagApply in the database or in-season database.")
        }
      }
      
      # -tagRecap should have an apply in the database from that lake at some point in the past -- a bunch of the tags that are recaps on Long don't appear as a tag apply in the db
        # the absence of a previous apply might mean it was entered wrong
      tagsRecapped=curNEW$tagRecapture[!is.na(curNEW$tagRecapture)]
      tagsRecapped=curNEW$tagRecapture[curNEW$tagRecapture!=""]
      if(length(tagsRecapped)>0){
        if(any(!tagsRecapped%in%c(fishInfoDB$tagApply,fishInfoIS$tagApply))){
          print(tagsRecapped[!tagsRecapped%in%c(fishInfoDB$tagApply,fishInfoIS$tagApply)])
          stop("You are attempting to enter a tagRecapture that was not ever recorded as a tagApply in the database or the in-season database.")
        }
      }
      
      # - tagRecap accidently matches an old apply
      # check lengths when matching recap to apply
      # use vonB for Long Lake and or Wisconsin for this...
#     if(length(tagsRecapped)>0){
        for(j in 1:length(tagsRecapped)){
          # if we measured a length
          if(!is.na(curNEW$fishLength[curNEW$tagRecapture==tagsRecapped[j]])){
            #if tagged this year
            if(tagsRecapped[j]%in%fishInfoIS$tagApply){
              #if we measured length when tag was applied
              if(any(!is.na(fishInfoIS$fishLength[fishInfoIS$tagApply==tagsRecapped[j]]),na.rm=TRUE)){
                #if we are looking at Long Lake, use our vonB
                if(lakeID%in%c("EL","FE","WL")){
                  Linf=376.909
                  K=0.32
                  t0=-0.599
                  tagLength=fishInfoIS$fishLength[fishInfoIS$tagApply==tagsRecapped[j]]
                  tagDate=fishSamplesIS$dateSample[fishSamplesIS$sampleID==fishInfoIS$sampleID[fishInfoIS$tagApply==tagsRecapped[j]]]
                  growthTime=as.Date(dateSample)-as.Date(tagDate)
                  age1=log(1-tagLength/Linf)/-K+t0
                  age2=age1+growthTime
                  expL=Linf*(1-exp(-K*(age2-t0)))
                  obsL=curNEW$fishLength[curNEW$tagRecapture==tagsRecapped[j]]
                  #if the fish we recapped is shorter or longer than fish tagged, based on vonB
                  if(abs(expL-obsL)>(0.1*obsL)){
                    stop(paste("The fish with tagRecapture:",tagsRecapped[j],"is a length that would not be expected. Is there a problem with the tag data or length data?",sep=" "))
                  }
                # use general vonB from Beamsderfer & North 1995
                }else{
                  Linf=550
                  K=0.19
                  t0=-0.024
                  tagLength=fishInfoIS$fishLength[fishInfoIS$tagApply==tagsRecapped[j]]
                  tagDate=fishSamplesIS$dateSample[fishSamplesIS$sampleID==fishInfoIS$sampleID[fishInfoIS$tagApply==tagsRecapped[j]]]
                  growthTime=(as.Date(dateSample)-as.Date(tagDate))/365  # difference in days, so convert to years for vonB
                  age1=log(1-tagLength/Linf)/-K+t0
                  age2=age1+growthTime
                  expL=Linf*(1-exp(-K*(age2-t0)))
                  obsL=curNEW$fishLength[curNEW$tagRecapture==tagsRecapped[j]]
                  #if the fish we recapped is shorter or longer than fish tagged, based on vonB
                  if(abs(expL-obsL)>(0.1*obsL)){
                    stop(paste("The fish with tagRecapture:",tagsRecapped[j],"is a length that would not be expected. Is there a problem with the tag data or length data?",sep=" "))
                  }
                }            
              }
            }
          # if tagged in a previous year
            if(tagsRecapped[j]%in%fishInfoDB$tagApply){
              #if we measured length when tag was applied
              if(any(!is.na(fishInfoDB$fishLength[fishInfoDB$tagApply==tagsRecapped[j]]),na.rm=TRUE)){
                #if we are looking at Long Lake, use our vonB
                if(lakeID%in%c("EL","FE","WL")){
                  Linf=376.909
                  K=0.32
                  t0=-0.599
                  tagLength=fishInfoDB$fishLength[fishInfoDB$tagApply==tagsRecapped[j]]
                  tagDate=fishSamplesDB$dateSample[fishSamplesDB$sampleID==fishInfoDB$sampleID[fishInfoDB$tagApply==tagsRecapped[j]]]
                  growthTime=as.Date(dateSample)-as.Date(tagDate)
                  age1=log(1-tagLength/Linf)/-K+t0
                  age2=as.numeric(age1+growthTime)
                  expL=Linf*(1-exp(-K*(age2-t0)))
                  obsL=curNEW$fishLength[curNEW$tagRecapture==tagsRecapped[j]]
                  #if the fish we recapped is shorter or longer than fish tagged, based on vonB
                 if(abs(expL-obsL)>(0.1*obsL)){
                    stop(paste("The fish with tagRecapture:",tagsRecapped[j],"is a length that would not be expected. Is there a problem with the tag data or length data?",sep=" "))
                  }
                  # use general vonB from Beamsderfer & North 1995
                }else{
                  Linf=550
                  K=0.19
                  t0=-0.024
                  tagLength=fishInfoDB$fishLength[fishInfoDB$tagApply==tagsRecapped[j]]
                  tagDate=fishSamplesDB$dateSample[fishSamplesDB$sampleID==fishInfoDB$sampleID[fishInfoDB$tagApply==tagsRecapped[j]]]
                  growthTime=(as.Date(dateSample)-as.Date(tagDate))/365  # difference in days, so convert to years for vonB
                  age1=log(1-tagLength/Linf)/-K+t0
                  age2=age1+growthTime
                  expL=Linf*(1-exp(-K*(age2-t0)))
                  obsL=curNEW$fishLength[curNEW$tagRecapture==tagsRecapped[j]]
                  #if the fish we recapped is shorter or longer than fish tagged, based on vonB
                  if(abs(expL-obsL)>(0.1*obsL)){
                    stop(paste("The fish with tagRecapture:",tagsRecapped[j],"is a length that would not be expected. Is there a problem with the tag data or length data?",sep=" "))
                  }
                }            
              }
            }
          }
        }
      }
      
      # update tables with new entries
      fishSamplesIS=rbind(fishSamplesIS,fishSamplesNEW)
      fishInfoIS=rbind(fishInfoIS,fishInfoNEW)
      fishDietsLOG=rbind(fishDietsLOG,fishDietsNEW)
      # fishOtolithsLOG=rbind(fishOtolithsLOG,fishOtolithsNEW) no otolith info here
      # fishspinesLOG=rbind(fishspinesLOG,fishspinesNEW) no fish spines info here
      # fishscalesLOG=rbind(fishscalesLOG,fishscalesNEW) no fish scale info here
      
      if(exists("fishDietsNEW")){fishDietsLOG=rbind(fishDietsLOG,fishDietsNEW)}
      if(exists("fishOtolithsNEW")){fishOtolithsLOG=rbind(fishOtolithsLOG,fishOtolithsNEW)}
      if(exists("fishspinesNEW")){fishspinesLOG=rbind(fishspinesLOG,fishspinesNEW)}
      if(exists("fishscalesNEW")){fishscalesLOG=rbind(fishscalesLOG,fishscalesNEW)}
      
    }
    
    # write updates to files
    write.csv(fishInfoIS,"fishInfoIS.csv",row.names=FALSE)
    write.csv(fishSamplesIS,"fishSamplesIS.csv",row.names=FALSE)
    
    if(exists("fishDietsNEW")){write.csv(fishDietsLOG,"fishDietsLOG.csv",row.names=FALSE)}
    if(exists("fishOtolithsNEW")){write.csv(fishOtolithsLOG,"fishOtolithsLOG.csv",row.names=FALSE)}
    if(exists("fishspinesNEW")){write.csv(fishspinesLOG,"fishspinesLOG.csv",row.names=FALSE)}
    if(exists("fishscalesNEW")){write.csv(fishscalesLOG,"fishscalesLOG.csv",row.names=FALSE)}
    
  }
}
