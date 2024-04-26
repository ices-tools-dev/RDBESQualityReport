# RDBES national quality report -------------------------------------------
# The following code runs the Rmarkdown html document 
# You need to define the parameters
# region: all regions or specific areas 
# RDBESfile: Paths to the files you want to import 
# eurostat: if TRUE it fetches the catches in all fishing regions by country to compare the total weight
# prelcatchstat: if TRUE you need to supply the file path where the file downloaded from ICES is stored under the prelcatchFile param
# fleetRegister: if TRUE you need to supply the file path where the file downloaded from Eurostat is stored under the fleetRegisterFile param



rmarkdown::render("./RmdCLCE/RDBESNationalQualityReport.Rmd", 
                  
                  params = list(
                  
                    region = "", # Choices will correspond to RCG codes, ignore for now needs further development and only if we have time
                    
                    RDBESfile = c("../Data/HCE_2024_04_24_075857.zip", "../Data/HCL_2024_04_24_075836.zip"),
                    
                    eurostat = TRUE,
                    
                    prelcatchstat = FALSE,
                    
                    prelcatchFile = "../Data/Rec12_05sgy0mmzfee0byudat1u2pw133584160602355104.zip", 
                    
                    fleetRegister = FALSE,   # TRUE is not working not yet implemented 
                    
                    fleetRegisterFile = ""
                    
                  ),
                  
                  # The path to where the report is saved 
                  output_dir = "../RDBESQualityReport/ReportOutput")







