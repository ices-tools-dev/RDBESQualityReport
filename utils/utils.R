#' Function that reads the RDBES exported zip files with the column names 
#'
#' @param file Path to the zip files 
#'
#' @return List of two dataframes
#' @export
#'
#' @examples



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
  
  # R names 
  
  names(CL) <- c("CLid", "CLrecType", "CLdTypSciWeig", "CLdSouSciWeig", "CLsampScheme", "CLdSouLanVal", "CLlanCou", "CLvesFlagCou", "CLyear", "CLquar", "CLmonth", "CLarea", "CLstatRect", "CLdSoucstatRect", "CLfishManUnit", "CLgsaSubarea", "CLjurisdArea", "CLfishAreaCat", "CLfreshWatNam", "CLeconZone", "CLeconZoneIndi", "CLspecCode", "CLspecFAO", "CLlandCat", "CLcatchCat", "CLregDisCat", "CLsizeCatScale", "CLsizeCat", "CLnatFishAct", "CLmetier6", "CLIBmitiDev", "CLloc", "CLvesLenCat", "CLfishTech", "CLmesSizRan", "CLsupReg", "CLgeoInd", "CLspeConTech", "CLdeepSeaReg", "CLFDIconCod", "CLoffWeight", "CLsciWeight", "CLexpDiff", "CLtotOffLanVal", "CLtotNumFish", "CLnumUniqVes", "CLsciWeightErrMeaValTyp", "CLsciWeightErrMeaValFirst", "CLsciWeightErrMeaValSecond", "CLvalErrMeaValTyp", "CLvalErrMeaValFirst", "CLvalErrMeaValSecond", "CLnumFishInCatchErrMeaValTyp", "CLnumFishInCatchErrMeaValFirst", "CLnumFishInCatchErrMeaValSecond", "CLcom", "CLsciWeightQualBias", "CLconfiFlag", "CLencrypVesIds")
  
  # CE month and area to lower case  
  names(CE) <- c("CEid", "CErecType", "CEdTypSciEff", "CEdSouSciEff", "CEsampScheme", "CEvesFlagCou", "CEyear", "CEquar", "CEmonth", "CEarea", "CEstatRect", "CEsoucStatRect", "CEfishManUnit", "CEgsaSubarea", "CEjurisdArea", "CEfishAreaCat", "CEfreshWatNam", "CEeconZone", "CLeconZoneIndi", "CEnatFishAct", "CEmetier6", "CEIBmitiDev", "CEloc", "CEvesLenCat", "CEfishTech", "CEmesSizRan", "CEsupReg", "CEgeoInd", "CEspeConTech", "CEdeepSeaReg", "CEoffVesHoursAtSea", "CEnumFracTrips", "CEnumDomTrip", "CEoffDaySea", "CESciDaySea", "CEoffFishDay", "CEsciFishDay", "CEoffNumHaulSet", "CEsciNumHaulSet", "CEoffVesFishHour", "CEsciVesFishHour", "CEoffSoakMeterHour", "CEsciSoakMeterHour", "CEoffkWDaySea", "CEscikWDaySea", "CEoffkWFishDay", "CEscikWFishDay", "CEoffkWFishHour", "CEscikWFishHour", "CEgTDaySea", "CEgTFishDay", "CEgTFishHour", "CEnumUniqVes", "CEgearDim", "CEnumFAD", "CEnumSupVes", "CEfishDaysErrMeaValTyp", "CEfishDaysErrMeaValFirst", "CEfishDaysErrMeaValSecond", "CEscientificFishingDaysQualBias", "CEconfiFlag", "CEencrypVesIds")
  
  
  # Character 
  CL$CLyear <- as.character(CL$CLyear)
  CE$CEyear <- as.character(CE$CEyear)
  # Create factors of variables for plots 
  # Month 
  CL$CLmonth <- factor(CL$CLmonth, levels = as.character(c(1:12)))
  CE$CEmonth <- factor(CE$CEmonth, levels = as.character(c(1:12)))
  # Quarter 
  CL$CLquar <- factor(CL$CLquar, levels = as.character(c(1:4)))
  CE$CEquar <- factor(CE$CEquar, levels = as.character(c(1:4)))
  # Vessel length category 
  CL$CLvesLenCat <- factor(CL$CLvesLenCat, levels = c("VL0006", "VL0608", "VL0810", "VL1012", "VL1215", "VL1518", "VL1824", "VL2440", "VL40XX"))
  CE$CEvesLenCat<- factor(CE$CEvesLenCat, levels = c("VL0006", "VL0608", "VL0810", "VL1012", "VL1215", "VL1518", "VL1824", "VL2440", "VL40XX"))
  
  
  return(list(CL,CE))
  
}



#' Function that reads the ICES preliminary catches zip file 
#'
#' @param file Path to the ICES preliminary catches zip file
#'
#' @return A dataframe 
#' @export
#'
#' @examples



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

 

#' Function that reads the zip file downloaded from the EU fleet register
#'
#' @param file Path to the fleet register zip folder
#'
#' @return
#' @export
#'
#' @examples


read.fleet <- function(file){
  fleet <- read.table(unz(file, "vesselRegistryListResults.csv"), 
                      header=T,  sep=";", quote = "")
  fleet <- mutate(fleet, 
                  StartYear = substr(Event.Start.Date, 1, 4),
                  EndYear = substr(Event.End.Date, 1, 4))
  return(fleet)
}



#' Function that changes the class of some fields in the CL and CE tables for plotting
#'
#' @param x The CL or CE dataframe 
#'
#' @return
#' @export
#'
#' @examples



rdbes.fc <- function(x){
  
  
  year <- paste0(deparse(substitute(x)) , "year")
  month <- paste0(deparse(substitute(x)) , "month")
  quar <- paste0(deparse(substitute(x)) , "quar")
  vesLenCat <- paste0(deparse(substitute(x)) , "vesLenCat")
  
  # Character 
  x[[year]] <- as.character(x[[year]])
  # Create factors for plots 
  # Month 
  x[[month]] <- factor(x[[month]], levels = as.character(c(1:12)))
  # Quarter 
  x[[quar]] <- factor(x[[quar]] , levels = as.character(c(1:4)))
  # Vessel length category 
  x[[vesLenCat]] <- factor(x[[vesLenCat]], levels = c("VL0006", "VL0608", "VL0810", "VL1012", "VL1215", "VL1518", "VL1824", "VL2440", "VL40XX"))
  
  return(x)
}


#' Title
#'
#' @param text 
#' @param level 
#'
#' @return
#' @export
#'
#' @examples



catHeader <- function(text = "", level = 3) {
  cat(paste0("\n\n", 
             paste(rep("#", level), collapse = ""), 
             " ", text, "\n"))
}
