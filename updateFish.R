# updateFish.R - wrapper for fishEntry.R
# 4-18-2019
# SEJ

# run the code below to update fish data with new datasheets that have been entered into the template
# and saved as .csv files

# Load the entry function
source('fishEntry.R')

# set variables
dbdir="C:/Users/jones/Box Sync/NDstuff/ND_R/"
db="MFEdb.db"
funcdir="C:/Users/jones/Box Sync/NDstuff/ND_R/fishEntryTool/"

#update
updateFish(dbdir=dbdir,db=db,funcdir=funcdir)