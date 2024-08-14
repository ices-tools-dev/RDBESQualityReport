# RDBESQualityReport

This Rmarkdown report script has been developed under the WGRBDES-GOV ISSG Data Quality. Its primary purpose is to serve as a resource for data submitters, helping them identify and correct potential errors on the Commercial Landings and Commercial Effort tables before the submission deadline.

## Input data

### Primary data

The script uses the Commercial Landings and Effort tables exported from the [RDBES database](https://rdbes.ices.dk/#/). It is important to run the script with the exported data to ensure the database checks have been already performed and there are no format errors.

### Auxiliary data

Comparisons with other data sources require to download the most recent [ICES preliminary catch statistics](https://data.ices.dk/rec12/login.aspx), and set to TRUE the option to load the [Eurostat](https://ec.europa.eu/eurostat/databrowser/view/tag00076/default/table?lang=en&category=t_fish) catch statistics.

> [!WARNING]\
> The Fleet register is not yet implemented.

## Usage

You can clone the repository from the main branch locally or you can download the code in a zip folder and store it in a dedicated folder.

You then need to define the local path to where the report and data are stored in the respective fields. If you want the report to produce the comparison with the auxiliary data you need to set the field values of eurostat and prelcatchstat to TRUE.

``` r

rmarkdown::render("/RmdCLCE/RDBESNationalQualityReport.Rmd", 
                  
                  params = list(
                    
                    RDBESfile = c("path to where the commercial effort file is", "path to where the commercial landings file is"),
                    
                    eurostat = TRUE,
                    
                    prelcatchstat = TRUE,
                    
                    prelcatchFile = "path the where the ICES preliminary catch statistics file is", 
                    
                    fleetRegister = FALSE,   
                    
                    fleetRegisterFile = ""
                    
                  ),
                  
                  # The path to where the report is saved 
                  output_dir = "/ReportOutput")
```

## Feedback
