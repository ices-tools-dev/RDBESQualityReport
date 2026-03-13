# RDBESQualityReport

This Rmarkdown report script has been developed under the WGRBDES-GOV ISSG Data Quality. Its primary purpose is to serve as a resource for data submitters, helping them identify and correct potential errors on the Commercial Landings and Commercial Effort tables before the submission deadline.

## Input data

### Primary data

The script uses the Commercial Landings and Effort tables exported from the [RDBES database](https://rdbes.ices.dk/#/). It is important to run the script with the exported data to ensure the database checks have been already performed and there are no format errors.

### Auxiliary data

Comparisons with other data sources require to download the most recent [ICES preliminary catch statistics](https://data.ices.dk/rec12/downloaddata), and set to TRUE the option to load the [Eurostat](https://ec.europa.eu/eurostat/databrowser/view/tag00076/default/table?lang=en&category=t_fish) catch statistics.

> [!WARNING]\
> The Fleet register is not yet implemented.

## Usage

You can clone the repository from the main branch to your local environment or you can download the code in a zip folder and store it in a dedicated folder. The script to run the report is named:

> RunReport.R

### How to run

The script is divided into two sections:

1. **Section 1 — Your settings** (edit this section): Define file paths and toggle optional comparisons.
2. **Section 2 — Run the report** (no changes needed): Handles path resolution, sanity checks, and rendering automatically.

To generate the report:
1. Open `RunReport.R` and fill in your settings in **Section 1**.
2. Run the entire script (e.g. `Ctrl+A` then `Ctrl+Enter` in RStudio, or use **Source**).
3. The finished HTML report will be saved in a `ReportOutput` folder created next to the script.

### File path notes

- Use forward slashes `/` in all paths, even on Windows.
- You can use **absolute paths** (e.g. `C:/Users/Alice/Data/myfile.zip`) or **relative paths** from the script's location (e.g. `Data/myfile.zip`).
- Point directly to the zip files — no need to unzip first.

### Settings reference

```r
# -- Required: paths to your two RDBES export zip files (HCL and HCE) --------
#    Supply them in any order; the script detects which is which automatically.

RDBESfile <- c(
  "path/to/your/HCE_file.zip",
  "path/to/your/HCL_file.zip"
)

# -- Optional: Eurostat comparison -------------------------------------------
#    Set to TRUE to download and compare against Eurostat landings data.
#    Requires an internet connection.

eurostat <- FALSE

# -- Optional: ICES Preliminary Catches comparison ---------------------------
#    Set to TRUE and supply the path to the zip downloaded from ICES.

prelcatchstat  <- FALSE
prelcatchFile  <- ""   # e.g. "path/to/your/PrelimCatches.zip"

# -- Optional: EU Fleet Register comparison ----------------------------------
#    Set to TRUE and supply the path to the zip downloaded from Eurostat.

fleetRegister     <- FALSE
fleetRegisterFile <- ""   # e.g. "path/to/your/vesselRegistryListResults.zip"
```

> [!IMPORTANT]\
> No pre-processing is required for the data. The script works with the exported zip files. However, the zip files need to be of the same download format (either all Table with id's format, or all Upload format).

## Feedback

If you encounter any issues or would like to propose new features please open an issue under this repository.
