# Function that reads the current year preliminary catches 
# zip file downloaded from ICES 



read.prelC <- function(file){
  
  fname <- as.character(unzip(file, list = TRUE)$Name)
  fname <- unique(fname[str_detect(fname, "csv")])
  prelC <- read.table(unz(file, fname), header=T,  sep=",")
  prelC$Area <- tolower(gsub( "_", ".", prelC$Area))
  prelC <- rename(prelC, 
                  Lan = AMS.Catch.TLW., 
                  BMS = BMS.Catch.TLW.)
  prelC$Year <- as.character(prelC$Year)
  
  return(prelC)
}

