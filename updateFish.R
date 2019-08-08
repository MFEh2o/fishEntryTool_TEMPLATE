# updateFish.R - wrapper for fishEntry.R
# 4-18-2019
# SEJ

# run the code below to update fish data with new datasheets that have been entered into the template
# and saved as .csv files

#setwd("C:/Users/jones/OneDrive/temp_fishscapesCSVentries") #UNDERC lab computer wd
#setwd("C:/Users/jones/OneDrive/temp_fishscapesCSVentries") #Colin's PC wd

# Load the entry function
source('C:/Users/jones/Documents/Summer2019/Fish/fishEntryTool/fishEntry.R') #lab computer
#source('C:/Users/jones/Box Sync/NDstuff/ND_R/fishEntryTool/fishEntry.R') #colin 's computer

# # set variables - Colin's computer
# dbdir="C:/Users/jones/Box Sync/NDstuff/ND_R/"
# db="MFEdb.db"
# funcdir="C:/Users/jones/Box Sync/NDstuff/ND_R/fishEntryTool/"

# set variables - UNDERC lab computer
dbdir="C:/Users/jones/Documents/Summer2019/Database/current/"
db="MFEdb.db"
funcdir="C:/Users/jones/Documents/Summer2019/Database/dbFunctions/"

#update
updateFish(dbdir=dbdir,db=db,funcdir=funcdir, force_siteID = T, force_clip = T, force_species = T, force_sampleGroup = T, force_metadataID = T, force_fishLength = T, force_distanceShocked = T)

#manually fixing some random issues that won't go away like silver lake 6/3/19 20:40 won't read in right 
fishS=read.csv("fishSamplesIS.csv", stringsAsFactors = F)
fishI=read.csv("fishInfoIS.csv", stringsAsFactors = F)

#fixing sample "SV_wholeShoreline_00190603_2045_AN_Fishscapes.Angling.20180625"
fixS=fishS[fishS$sampleID=="SV_wholeShoreline_00190603_2045_AN_Fishscapes.Angling.20180625",] # see what needs to be fixed
fishS$sampleID[fishS$sampleID=="SV_wholeShoreline_00190603_2045_AN_Fishscapes.Angling.20180625"]="SV_wholeShoreline_20190603_2045_AN_Fishscapes.Angling.20180625"
fishS$dateSample[fishS$sampleID=="SV_wholeShoreline_00190603_2045_AN_Fishscapes.Angling.20180625"]="6/3/2019"
fishS$dateSet[fishS$sampleID=="SV_wholeShoreline_00190603_2045_AN_Fishscapes.Angling.20180625"]="6/3/2019"


#fixing fishIDs from that same sample "SV_wholeShoreline_00190603_2045_AN_Fishscapes.Angling.20180625"
fixI=fishI[fishI$sampleID=="SV_wholeShoreline_00190603_2045_AN_Fishscapes.Angling.20180625",] # see wh at needs to be fixed
fishI$sampleID[fishI$sampleID=="SV_wholeShoreline_00190603_2045_AN_Fishscapes.Angling.20180625"]="SV_wholeShoreline_20190603_2045_AN_Fishscapes.Angling.20180625"
temp=character()
for(i in 1:nrow(fixI)){
  temp[i]=paste("SV_wholeShoreline_20190603_2045_AN_Fishscapes.Angling.20180625", fixI$fishNum[i], sep = "_")
}
fishI$fishID[fishI$sampleID=="SV_wholeShoreline_00190603_2045_AN_Fishscapes.Angling.20180625"]=temp

#writing the new, fixed files
write.csv(fishS, "fishSamplesIS.csv", row.names = F)
write.csv(fishI, "fishInfoIS.csv", row.names = F)
