# =============================================================================
# RDBES National Quality Report — Configuration
# =============================================================================
# HOW TO USE:
#   1. Fill in the file paths and settings in SECTION 1 below.
#   2. Run this entire script (Ctrl+A then Ctrl+Enter, or use "Source").
#   3. The finished HTML report will be saved in a "ReportOutput" folder
#      next to this script.
#
# NOTES ON FILE PATHS:
#   - Use forward slashes "/" in paths, even on Windows.
#   - You can use absolute paths  e.g. "C:/Users/Alice/Data/myfile.zip"
#     or paths relative to where this script lives  e.g. "Data/myfile.zip"
#   - If your files are zips, just point to the zip — no need to unzip first.
# =============================================================================


# -----------------------------------------------------------------------------
# SECTION 1 — YOUR SETTINGS (edit these)
# -----------------------------------------------------------------------------

# -- Required: paths to your two RDBES export zip files (HCL and HCE) --------
#    Supply them in any order; the script detects which is which automatically.
#    Example:
#      RDBESfile <- c("C:/Data/HCL_2023.zip", "C:/Data/HCE_2023.zip")

RDBESfile <- c(
  "Data/HCE_2026_03_11_170242047.zip",
  "Data/HCL_2026_03_11_170145372.zip"
)

# -- Optional: Eurostat comparison -------------------------------------------
#    Set to TRUE to download and compare against Eurostat landings data.
#    Requires an internet connection.

eurostat <- FALSE

# -- Optional: ICES Preliminary Catches comparison ---------------------------
#    Set to TRUE and supply the path to the zip downloaded from ICES.

prelcatchstat  <- FALSE
prelcatchFile  <- ""   # e.g. "C:/Data/PrelimCatches_2023.zip"

# -- Optional: EU Fleet Register comparison ----------------------------------
#    Set to TRUE and supply the path to the zip downloaded from Eurostat.

fleetRegister     <- FALSE
fleetRegisterFile <- ""   # e.g. "C:/Data/vesselRegistryListResults.zip"


# =============================================================================
# SECTION 2 — RUN THE REPORT  (no changes needed below this line)
# =============================================================================

# -- Locate this script so all relative paths work regardless of working dir --
script_dir <- if (requireNamespace("rstudioapi", quietly = TRUE) &&
                    rstudioapi::isAvailable()) {
  dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  getwd()   # fallback when sourced from the command line / non-RStudio session
}

# -- Helper: resolve a path relative to the script directory -----------------
.rel <- function(p) {
  if (nchar(trimws(p)) == 0) return(p)           # empty string -> leave as-is
  if (grepl("^([A-Za-z]:|/|\\\\)", p)) return(p) # already absolute -> leave as-is
  file.path(script_dir, p)
}

# -- Resolve user-supplied paths (handles relative *and* absolute) -----------
RDBESfile         <- sapply(RDBESfile,         .rel, USE.NAMES = FALSE)
prelcatchFile     <- .rel(prelcatchFile)
fleetRegisterFile <- .rel(fleetRegisterFile)

# -- Basic sanity checks before we even start rendering ----------------------
if (all(grepl("^path/to/your", RDBESfile))) {
  stop(
    "\nPlease edit RunReport.R and set RDBESfile to the actual paths of your HCL and HCE zip files.",
    call. = FALSE
  )
}

missing_rdbes <- RDBESfile[!file.exists(RDBESfile)]
if (length(missing_rdbes) > 0) {
  stop(
    "\nCannot find the following RDBES file(s):\n",
    paste(" -", missing_rdbes, collapse = "\n"),
    "\nCheck that the paths in RDBESfile are correct.",
    call. = FALSE
  )
}

if (prelcatchstat && !file.exists(prelcatchFile)) {
  stop(
    "\nprelcatchstat is TRUE but the file was not found:\n  ", prelcatchFile,
    "\nSet the correct path in prelcatchFile, or set prelcatchstat <- FALSE.",
    call. = FALSE
  )
}

if (fleetRegister && !file.exists(fleetRegisterFile)) {
  stop(
    "\nfleetRegister is TRUE but the file was not found:\n  ", fleetRegisterFile,
    "\nSet the correct path in fleetRegisterFile, or set fleetRegister <- FALSE.",
    call. = FALSE
  )
}

# -- Install rmarkdown if needed ---------------------------------------------
if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  message("Installing 'rmarkdown' package (one-time setup)...")
  install.packages("rmarkdown")
}

# -- Output folder (created next to this script if it does not exist) --------
output_dir <- file.path(script_dir, "ReportOutput")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -- Render -------------------------------------------------------------------
rmd_file <- file.path(script_dir, "rmd", "RDBESNationalQualityReport.Rmd")

if (!file.exists(rmd_file)) {
  stop(
    "\nCould not find the report template at:\n  ", rmd_file,
    "\nMake sure this script sits in the same folder as the 'rmd' directory.",
    call. = FALSE
  )
}

message("Rendering report — this may take a few minutes...")

rmarkdown::render(
  input      = rmd_file,
  params     = list(
    RDBESfile         = RDBESfile,
    eurostat          = eurostat,
    prelcatchstat     = prelcatchstat,
    prelcatchFile     = prelcatchFile,
    fleetRegister     = fleetRegister,
    fleetRegisterFile = fleetRegisterFile
  ),
  output_dir = output_dir
)

message("Done! Report saved to: ", output_dir)
