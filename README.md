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

You can clone the repository from the main branch to your local enviroment or you can download the code in a zip folder and store it in a dedicated folder. The script to run the report is named:

> RunReport.R

In this script you have to define the local path to where the report Rmarkdown and data are stored in the respective fields. If you want the report to produce the comparison with the auxiliary data you need to set the field values of eurostat and prelcatchstat to TRUE.

> [!IMPORTANT]\
> No preprocessing is required for the data. The script works with the exported zip files.

``` r

rmarkdown::render("path the the report folder/RDBESNationalQualityReport.Rmd", 
                  
                  params = list(
                    
                    RDBESfile = c("path to the commercial effort file", "path to the commercial landings"),
                    
                    eurostat = TRUE,
                    
                    prelcatchstat = TRUE,
                    
                    prelcatchFile = "path to the ICES preliminary catch statistics", 
                    
                    fleetRegister = FALSE,   
                    
                    fleetRegisterFile = ""
                    
                  ),
                  
                  # The path to where the report is saved 
                  output_dir = "path to the report output folder/ReportOutput")
```

## Feedback

If you encounter any issues or would like to propose new features please open an issue under this repository.
