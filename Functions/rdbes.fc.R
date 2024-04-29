# Function that transform the CL variables to factors/characters 
# for plotting 

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