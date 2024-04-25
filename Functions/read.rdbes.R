# Function that read the RDBES CL and CE files ----------------------------
# For now only reads the zip files that are exported from RDBES with the ID's
# If we allow the import of the files prior the import
# we will not have the RDBES field checks
# TODO 
# 1. you can download multiple years in a single zip file from RDBES?
# need to check if the code below works when I upload the data
# It should all be in one CommercialXXX file? 
# 2. Check for consistency for CL years and CE years. If the years are not the same throw error; it's not possible to 
# match the two tables for further checks

read.rdbes <- function(file){
  
  CLfiles <- unique(file[str_detect(file, "HCL")])
  CEfiles <- unique(file[str_detect(file, "HCE")])
  
  if(length(CLfiles) > 1){
    listCL <- lapply(CLfiles, function(x){read.table(unz(x, "CommercialLanding.csv"), 
                                                     header=T,  sep=",", quote = "")})
    CL <- do.call(rbind, listCL)
  }else{
    CL <- read.table(unz(CLfiles, "CommercialLanding.csv"), 
                     header=T,  sep=",", quote = "")
  }
  
  if(length(CEfiles) > 1){
    listCE <- lapply(CEfiles, function(x){read.table(unz(x, "CommercialEffort.csv"), 
                                                     header=T,  sep=",", quote = "")})
    CE <- do.call(rbind, listCE)
  }else{
    CE <- read.table(unz(CEfiles, "CommercialEffort.csv"), 
                     header=T,  sep=",", quote = "")
  }
  
  # Character 
  CL$CLyear <- as.character(CL$CLyear)
  CE$CEyear <- as.character(CE$CEyear)
  # Create factors of variables for plots 
  # Month 
  CL$CLmonth <- factor(CL$CLmonth, levels = as.character(c(1:12)))
  CE$CEmonth <- factor(CE$CEmonth, levels = as.character(c(1:12)))
  # Quarter 
  CL$CLquarter <- factor(CL$CLquarter, levels = as.character(c(1:4)))
  CE$CEquarter <- factor(CE$CEquarter, levels = as.character(c(1:4)))
  # Vessel length category 
  CL$CLvesselLengthCategory <- factor(CL$CLvesselLengthCategory, levels = c("VL0006", "VL0608", "VL0810", "VL1012", "VL1215", "VL1518", "VL1824", "VL2440", "VL40XX"))
  CE$CEvesselLengthCategory <- factor(CE$CEvesselLengthCategory, levels = c("VL0006", "VL0608", "VL0810", "VL1012", "VL1215", "VL1518", "VL1824", "VL2440", "VL40XX"))
  
  
  return(list(CL,CE))
  
}
