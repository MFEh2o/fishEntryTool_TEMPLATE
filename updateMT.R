# updateMT.R - wrapper for minnowtrapEntry.R
# 12-04-2019
# SEJ

# run the code below to update minnow trap data with new datasheets that have been entered into the template
# and saved as .csv files

#setwd("C:/Users/jones/OneDrive/temp_fishscapesCSVentries") #UNDERC lab computer wd
#setwd("C:/Users/jones/Box Sync/NDstuff/ND_R/fishEntryTool") #Colin's PC wd
#setwd("~/Documents/Research/MFE/database/fishEntryTool/") #Stuart's laptop

# Load the entry function
source('C:/Users/jones/Documents/Summer2019/Fish/fishEntryTool/fishEntry.R') #lab computer
#source('C:/Users/jones/Box Sync/NDstuff/ND_R/fishEntryTool/fishEntry.R') #colin 's computer
source("minnowtrapEntry.R")

# # set variables - Colin's computer
# dbdir="C:/Users/jones/Box Sync/NDstuff/ND_R/"
# db="MFEdb.db"
# funcdir="C:/Users/jones/Box Sync/NDstuff/ND_R/fishEntryTool/"

# set variables - UNDERC lab computer
dbdir="C:/Users/jones/Documents/Summer2019/Database/current/"
db="MFEdb.db"
funcdir="C:/Users/jones/Documents/Summer2019/Database/dbFunctions/"

# set variables -Stuart's laptop
dbdir="~/Documents/Research/MFE/database/"
db="MFEdb.db"
funcdir="~/Documents/Research/MFE/database/db"

#update
updateMinnowTrap(dbdir=dbdir,db=db,funcdir=funcdir)


#writing the new files
write.csv(fishS, "MTfishSamplesIS.csv", row.names = F)
write.csv(fishI, "MTfishInfoIS.csv", row.names = F)
