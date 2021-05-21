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
