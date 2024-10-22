# RDBES national quality report -------------------------------------------
# The following code runs the Rmarkdown html document 
# You need to define the parameters
# RDBESfile: Paths to the files you want to import 
# eurostat: if TRUE it fetches the catches in all fishing regions by country to compare the total weight
# prelcatchstat: if TRUE you need to supply the file path where the file downloaded from ICES is stored under the prelcatchFile param
# fleetRegister: if TRUE you need to supply the file path where the file downloaded from Eurostat is stored under the fleetRegisterFile param



rmarkdown::render("path the the Rmd document folder/rmd/RDBESNationalQualityReport.Rmd", 
                  
                  params = list(
                    
                    RDBESfile = c("path to the commercial effort", "path to the commercial landings"),
                    
                    eurostat = TRUE,
                    
                    prelcatchstat = TRUE,
                    
                    prelcatchFile = "path to the ICES preliminary catch statistics", 
                    
                    fleetRegister = FALSE,   
                    
                    fleetRegisterFile = ""
                    
                  ),
                  
                  # The path to where the report is saved 
                  output_dir = "path to the report output folder/ReportOutput")



