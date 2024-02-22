# RDBES national quality report -------------------------------------------
# The following code runs the Rmarkdown html document 
# You need to define the parameters
# Year: single or multiple years
# Region: all regions or specific areas 
# File: Paths to the files you want to import 
# eurostat: if TRUE it fetches the catches in all fishing regions by country to compare the total weight




rmarkdown::render("RDBESNationalQualityReport.Rmd", params = list(
  
  year = c(2017, 2018), # Keep this or just use the years present in the dataset? 
  
  region = "", # Choices will correspond to RCG codes, ignore for now needs further development
  
  file = c("./HCL_2024_02_20_110233.zip", "./HCL_2024_02_20_110234.zip","./HCE_2024_02_20_110348.zip", "./HCE_2024_02_20_110349.zip"),
  
  eurostat = TRUE,
  
  prelcatchstat = FALSE, # TRUE is not working need to give file 
  
  fleetRegister = FALSE # TRUE is not working need to give file
  
))



# c("./HCL_2024_02_20_110233.zip", "./HCL_2024_02_20_110234.zip","./HCE_2024_02_20_110348.zip", "./HCE_2024_02_20_110348.zip")
# c("./HCL_2024_02_20_110233.zip","./HCE_2024_02_20_110348.zip")

# list.files(path = "./", pattern = "*.zip", full.names = TRUE)

