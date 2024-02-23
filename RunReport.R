# RDBES national quality report -------------------------------------------
# The following code runs the Rmarkdown html document 
# You need to define the parameters
# year: single or multiple years
# region: all regions or specific areas 
# RDBESfile: Paths to the files you want to import 
# eurostat: if TRUE it fetches the catches in all fishing regions by country to compare the total weight
# prelcatchstat: if TRUE you need to supply the file path where the file downloaded from ICES is stored under the prelcatchFile param
# fleetRegister: if TRUE you need to supply the file path where the file downloaded from Eurostat is stored under the fleetRegisterFile param



rmarkdown::render("./RmdCLCE/RDBESNationalQualityReport.Rmd", 
                  
                  params = list(
                    
                    year = c(2017, 2018), # Keep this or just use the years present in the dataset? 
                    
                    region = "", # Choices will correspond to RCG codes, ignore for now needs further development and only if we have time
                    
                    RDBESfile = c("../HCL_2024_02_20_110233.zip", "../HCL_2024_02_20_110234.zip","../HCE_2024_02_20_110348.zip", "../HCE_2024_02_20_110349.zip"),
                    
                    eurostat = TRUE,
                    
                    prelcatchstat = FALSE,
                    
                    prelcatchFile = "", # TRUE is not working not yet implemented 
                    
                    fleetRegister = FALSE,   # TRUE is not working not yet implemented 
                    
                    fleetRegisterFile = ""
                    
                  ),
                  
                  # The path to where the report is saved 
                  output_dir = "../RDBESQualityReport/ReportOutput")



# c("./HCL_2024_02_20_110233.zip", "./HCL_2024_02_20_110234.zip","./HCE_2024_02_20_110348.zip", "./HCE_2024_02_20_110348.zip")
# c("./HCL_2024_02_20_110233.zip","./HCE_2024_02_20_110348.zip")



