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

