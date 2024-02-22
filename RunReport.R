# RDBES national quality report -------------------------------------------
# The following code runs the Rmarkdown html document 
# You need to define the parameters
# Year: single or multiple years
# Region: all regions or specific areas 
# Printcode: If you want the code to be printed along the document, default = FALSE
# File: Paths to the files you want to import 




rmarkdown::render("RDBESNationalQualityReport.Rmd", params = list(
  
  year = c(2017, 2018),
  
  region = "Asia", # Choices correspond to RCG - NAtl, Baltic, NSea, 
  
  printcode = FALSE,
  
  file = c("./HCL_2024_02_20_110233.zip", "./HCL_2024_02_20_110234.zip","./HCE_2024_02_20_110348.zip", "./HCE_2024_02_20_110349.zip")
  
))



# c("./HCL_2024_02_20_110233.zip", "./HCL_2024_02_20_110234.zip","./HCE_2024_02_20_110348.zip", "./HCE_2024_02_20_110348.zip")
# c("./HCL_2024_02_20_110233.zip","./HCE_2024_02_20_110348.zip")

# list.files(path = "./", pattern = "*.zip", full.names = TRUE)

