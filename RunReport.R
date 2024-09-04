# RDBES national quality report -------------------------------------------
# The following code runs the Rmarkdown html document 
# You need to define the parameters
# RDBESfile: Paths to the files you want to import 
# eurostat: if TRUE it fetches the catches in all fishing regions by country to compare the total weight
# prelcatchstat: if TRUE you need to supply the file path where the file downloaded from ICES is stored under the prelcatchFile param
# fleetRegister: if TRUE you need to supply the file path where the file downloaded from Eurostat is stored under the fleetRegisterFile param



rmarkdown::render("./rmd/RDBESNationalQualityReport.Rmd", 
                  
                  params = list(
                    
                    RDBESfile = c("../Data/HCE_2024_04_24_075857.zip", "../Data/HCL_2024_04_24_075836.zip"),
                    
                    eurostat = FALSE,
                    
                    prelcatchstat = FALSE,
                    
                    prelcatchFile = "../Data/Rec12_05sgy0mmzfee0byudat1u2pw133584160602355104.zip", 
                    
                    fleetRegister = FALSE,   
                    
                    fleetRegisterFile = "../Data/vesselRegistryListResults.zip"
                    
                  ),
                  
                  # The path to where the report is saved 
                  output_dir = "../ReportOutput")



