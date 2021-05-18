# updateFish.R - wrapper for fishEntry.R
# 4-18-2019
# SEJ

# run the code below to update fish data with new datasheets that have been entered into the template
# and saved as .csv files

#setwd("C:/Users/jones/OneDrive/temp_fishscapesCSVentries") #UNDERC lab computer wd
#setwd("C:/Users/jones/OneDrive/temp_fishscapesCSVentries") #Colin's PC wd

# Load the entry function
source('C:/Users/mosle/Documents/JonesLakeExperiment_Datasheets_2020/fishEntry.R') #camille's pc

#set variables - Camille's pc
dbdir="C:/Users/mosle/Documents/JonesLakeExperiment_Datasheets_2020/"
#database name
db="MFEdb.db"
funcdir="C:/Users/mosle/Documents/JonesLakeExperiment_Datasheets_2020/"


#update
updateFish(dbdir=dbdir,db=db,funcdir=funcdir, force_sampleGroup = T, force_metadataID = T, force_projectID = T, force_clip = T, force_siteID = T)

