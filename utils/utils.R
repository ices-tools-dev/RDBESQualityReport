#' Read RDBES exported zip files (CL and CE tables)
#'
#' Supports two export formats from the RDBES web interface:
#'
#' \strong{"Table with IDs"} — zip contains \code{CommercialLanding.csv} /
#' \code{CommercialEffort.csv} with a header row and an auto-generated integer
#' ID column as the first field.
#'
#' \strong{"Upload" (Table without IDs or column names)} — zip contains
#' \code{HCL.csv} / \code{HCE.csv} with no header row and no ID column.
#' The format is detected automatically from the CSV filename inside the zip.
#'
#' @param file Character vector of length 2: paths to the zip files containing
#'   the CL and CE data. File names must contain "HCL" and "HCE" respectively
#'   (case-insensitive) so the function can tell them apart.
#'
#' @return Named list with elements \code{CL} and \code{CE} (data.frames),
#'   both using canonical R column names from the RDBES data model.
#' @export
read.rdbes <- function(file) {

  # ---- Input validation -------------------------------------------------------
  if (!is.character(file) || length(file) < 2) {
    stop("'file' must be a character vector with paths to the CE and CL zip files.")
  }
  if (!all(grepl("\\.zip$", file, ignore.case = TRUE))) {
    stop("All files must be zip archives (.zip). Check your file paths.")
  }
  missing_files <- file[!file.exists(file)]
  if (length(missing_files) > 0) {
    stop(paste("File(s) not found:", paste(missing_files, collapse = ", ")))
  }

  # ---- Identify CL and CE files by name ---------------------------------------
  CLfiles <- unique(file[grepl("HCL", file, ignore.case = TRUE)])
  CEfiles <- unique(file[grepl("HCE", file, ignore.case = TRUE)])

  if (length(CLfiles) == 0) stop("No CL file found. File name must contain 'HCL'.")
  if (length(CEfiles) == 0) stop("No CE file found. File name must contain 'HCE'.")

  # ---- Detect format from CSV name inside the zip ----------------------------
  .csv_stem <- function(zip) {
    nms <- unzip(zip, list = TRUE)$Name
    stems <- sub("\\.csv$", "", nms[grepl("^(CommercialLanding|CommercialEffort|HCL|HCE)\\.csv$",
                                          nms, ignore.case = TRUE)])
    if (length(stems) == 0)
      stop(paste0(
        "Cannot detect format from zip: ", zip, "\n",
        "Expected one of: CommercialLanding.csv, CommercialEffort.csv, HCL.csv, HCE.csv"
      ))
    unique(stems)[1]
  }

  fmt_CL <- .csv_stem(CLfiles[1])
  fmt_CE <- .csv_stem(CEfiles[1])

  # Resolve to canonical format label
  is_table_ids <- fmt_CL == "CommercialLanding" && fmt_CE == "CommercialEffort"
  is_upload    <- fmt_CL == "HCL"               && fmt_CE == "HCE"

  if (!is_table_ids && !is_upload) {
    stop(paste0(
      "Mixed or unrecognised RDBES formats.\n",
      "  CL format detected: '", fmt_CL, "'\n",
      "  CE format detected: '", fmt_CE, "'\n",
      "Both files must use the same format ('Table with IDs' or 'Upload')."
    ))
  }

  format_label <- if (is_table_ids) "Table with IDs" else "Upload (no header/IDs)"
  message("RDBES format detected: ", format_label)

  # ---- Helper: read and row-bind one or many zips -----------------------------
  .read_zips <- function(zip_files, csv_stem, has_header) {
    read_one <- function(z) {
      read.table(unz(z, paste0(csv_stem, ".csv")),
                 header = has_header, sep = ",", quote = "",
                 stringsAsFactors = FALSE, fill = TRUE)
    }
    if (length(zip_files) > 1) {
      do.call(rbind, lapply(zip_files, read_one))
    } else {
      read_one(zip_files)
    }
  }

  # ---- Canonical column name vectors -----------------------------------------
  # Source of truth: RDBES_Data_Model_CL_CE.xlsx, "R Name" column.
  #
  # CL_NAMES_DATA / CE_NAMES_DATA — the 59 / 62 data fields (no ID column).
  # These are used for the Upload format, which has no header and no ID.
  #
  # CL_NAMES / CE_NAMES — prepend the ID column for the "Table with IDs"
  # format (60 / 63 columns total).

  CL_NAMES_DATA <- c(
    "CLrecType",                    # 1
    "CLdBasSciWeig",                # 2
    "CLdSouSciWeig",                # 3
    "CLsampScheme",                 # 4
    "CLdSouLanVal",                 # 5
    "CLlanCou",                     # 6
    "CLvesFlagCou",                 # 7
    "CLyear",                       # 8
    "CLquar",                       # 9
    "CLmonth",                      # 10
    "CLarea",                       # 11
    "CLstatRect",                   # 12
    "CLdatBasStatRect",             # 13
    "CLdSoucstatRect",              # 14
    "CLfishManUnit",                # 15
    "CLgsaSubarea",                 # 16
    "CLjurisdArea",                 # 17
    "CLfishAreaCat",                # 18
    "CLfreshWatNam",                # 19
    "CLeconZone",                   # 20
    "CLeconZoneIndi",               # 21
    "CLspecCode",                   # 22
    "CLspecFAO",                    # 23
    "CLlandCat",                    # 24
    "CLcatchCat",                   # 25
    "CLregDisCat",                  # 26
    "CLsizeCatScale",               # 27
    "CLsizeCat",                    # 28
    "CLnatFishAct",                 # 29
    "CLmetier6",                    # 30
    "CLIBmitiDev",                  # 31
    "CLloc",                        # 32
    "CLvesLenCat",                  # 33
    "CLfishTech",                   # 34
    "CLmesSizRan",                  # 35
    "CLsupReg",                     # 36
    "CLgeoInd",                     # 37
    "CLspeConTech",                 # 38
    "CLdeepSeaReg",                 # 39
    "CLFDIconCod",                  # 40
    "CLoffWeight",                  # 41
    "CLsciWeight",                  # 42
    "CLexpDiff",                    # 43
    "CLlanVal",                     # 44
    "CLtotNumFish",                 # 45
    "CLnumUniqVes",                 # 46
    "CLsciWeightErrMeaValTyp",      # 47
    "CLsciWeightErrMeaValFirst",    # 48
    "CLsciWeightErrMeaValSecond",   # 49
    "CLvalErrMeaValTyp",            # 50
    "CLvalErrMeaValFirst",          # 51
    "CLvalErrMeaValSecond",         # 52
    "CLnumFishInCatchErrMeaValTyp",    # 53
    "CLnumFishInCatchErrMeaValFirst",  # 54
    "CLnumFishInCatchErrMeaValSecond", # 55
    "CLcom",                        # 56
    "CLsciWeightQualBias",          # 57
    "CLconfiFlag",                  # 58
    "CLencrypVesIds"                # 59
  )

  CE_NAMES_DATA <- c(
    "CErecType",                    # 1
    "CEdBasSciEff",                 # 2
    "CEdSouSciEff",                 # 3
    "CEsampScheme",                 # 4
    "CEvesFlagCou",                 # 5
    "CEyear",                       # 6
    "CEquar",                       # 7
    "CEmonth",                      # 8
    "CEarea",                       # 9
    "CEstatRect",                   # 10
    "CEdatBasStatRect",             # 11
    "CEsoucStatRect",               # 12
    "CEfishManUnit",                # 13
    "CEgsaSubarea",                 # 14
    "CEjurisdArea",                 # 15
    "CEfishAreaCat",                # 16
    "CEfreshWatNam",                # 17
    "CEeconZone",                   # 18
    "CEeconZoneIndi",               # 19
    "CEnatFishAct",                 # 20
    "CEmetier6",                    # 21
    "CEIBmitiDev",                  # 22
    "CEloc",                        # 23
    "CEvesLenCat",                  # 24
    "CEfishTech",                   # 25
    "CEmesSizRan",                  # 26
    "CEsupReg",                     # 27
    "CEgeoInd",                     # 28
    "CEspeConTech",                 # 29
    "CEdeepSeaReg",                 # 30
    "CEoffVesHoursAtSea",           # 31
    "CEnumFracTrips",               # 32
    "CEnumDomTrip",                 # 33
    "CEoffDaySea",                  # 34
    "CEsciDaySea",                  # 35
    "CEoffFishDay",                 # 36
    "CEsciFishDay",                 # 37
    "CEoffNumHaulSet",              # 38
    "CEsciNumHaulSet",              # 39
    "CEoffVesFishHour",             # 40
    "CEsciVesFishHour",             # 41
    "CEoffSoakMeterHour",           # 42
    "CEsciSoakMeterHour",           # 43
    "CEoffkWDaySea",                # 44
    "CEscikWDaySea",                # 45
    "CEoffkWFishDay",               # 46
    "CEscikWFishDay",               # 47
    "CEoffkWFishHour",              # 48
    "CEscikWFishHour",              # 49
    "CEgTDaySea",                   # 50
    "CEgTFishDay",                  # 51
    "CEgTFishHour",                 # 52
    "CEnumUniqVes",                 # 53
    "CEgearDim",                    # 54
    "CEnumFAD",                     # 55
    "CEnumSupVes",                  # 56
    "CEfishDaysErrMeaValTyp",       # 57
    "CEfishDaysErrMeaValFirst",     # 58
    "CEfishDaysErrMeaValSecond",    # 59
    "CEscientificFishingDaysQualBias", # 60
    "CEconfiFlag",                  # 61
    "CEencrypVesIds"                # 62
  )

  # Full name vectors including the ID column (Table with IDs format)
  CL_NAMES <- c("CLid", CL_NAMES_DATA)   # 60 columns
  CE_NAMES <- c("CEid", CE_NAMES_DATA)   # 63 columns

  # ---- Read data ---------------------------------------------------------------
  if (is_table_ids) {
    # Header present, first column is the integer PK
    CL <- .read_zips(CLfiles, "CommercialLanding", has_header = TRUE)
    CE <- .read_zips(CEfiles, "CommercialEffort",  has_header = TRUE)
    expected_CL <- length(CL_NAMES)       # 60
    expected_CE <- length(CE_NAMES)       # 63

  } else {
    # No header, no ID column — assign canonical names directly
    CL <- .read_zips(CLfiles, "HCL", has_header = FALSE)
    CE <- .read_zips(CEfiles, "HCE", has_header = FALSE)
    # Add a synthetic integer ID so downstream code that uses CLid / CEid works
    CL <- cbind(CLid = seq_len(nrow(CL)), CL)
    CE <- cbind(CEid = seq_len(nrow(CE)), CE)
    expected_CL <- length(CL_NAMES)       # 60 (59 data + 1 synthetic ID)
    expected_CE <- length(CE_NAMES)       # 63 (62 data + 1 synthetic ID)
  }

  # ---- Assign canonical column names ------------------------------------------
  .rename_safe <- function(df, target_names, label) {
    n_got      <- ncol(df)
    n_expected <- length(target_names)
    if (n_got == n_expected) {
      names(df) <- target_names
    } else {
      warning(sprintf(
        paste0(
          "%s has %d columns but %d are expected for the '%s' format.\n",
          "  Columns will NOT be renamed — check the RDBES export version.\n",
          "  Expected %d, got %d."
        ),
        label, n_got, n_expected, format_label, n_expected, n_got
      ))
    }
    df
  }

  CL <- .rename_safe(CL, CL_NAMES, "CL")
  CE <- .rename_safe(CE, CE_NAMES, "CE")

  # ---- Fix issue #12: coerce all-logical columns to character -----------------
  # Empty CSV columns are read as logical NA by R; convert them to character.
  CL <- as.data.frame(lapply(CL, function(x) if (is.logical(x)) as.character(x) else x),
                      stringsAsFactors = FALSE)
  CE <- as.data.frame(lapply(CE, function(x) if (is.logical(x)) as.character(x) else x),
                      stringsAsFactors = FALSE)

  # Trim any whitespace from column names (guards against trailing spaces in
  # "Table with IDs" exports from some RDBES versions)
  names(CL) <- trimws(names(CL))
  names(CE) <- trimws(names(CE))

  # ---- Type coercions ---------------------------------------------------------
  CL$CLyear  <- as.character(CL$CLyear)
  CE$CEyear  <- as.character(CE$CEyear)

  CL$CLoffWeight <- suppressWarnings(as.numeric(CL$CLoffWeight))
  CL$CLsciWeight <- suppressWarnings(as.numeric(CL$CLsciWeight))
  CL$CLlanVal    <- suppressWarnings(as.numeric(CL$CLlanVal))
  CE$CEsciFishDay <- suppressWarnings(as.numeric(CE$CEsciFishDay))
  CE$CEsciDaySea  <- suppressWarnings(as.numeric(CE$CEsciDaySea))

  # ---- Factors for plotting ---------------------------------------------------
  VES_LEN_LEVELS <- c("VL0006", "VL0608", "VL0810", "VL1012",
                       "VL1215", "VL1518", "VL1824", "VL2440", "VL40XX")

  CL$CLmonth     <- factor(CL$CLmonth,     levels = as.character(1:12))
  CL$CLquar      <- factor(CL$CLquar,      levels = as.character(1:4))
  CL$CLvesLenCat <- factor(CL$CLvesLenCat, levels = VES_LEN_LEVELS)

  CE$CEmonth     <- factor(CE$CEmonth,     levels = as.character(1:12))
  CE$CEquar      <- factor(CE$CEquar,      levels = as.character(1:4))
  CE$CEvesLenCat <- factor(CE$CEvesLenCat, levels = VES_LEN_LEVELS)

  # ---- Warn if years differ between CL and CE ---------------------------------
  cl_years <- sort(unique(CL$CLyear))
  ce_years <- sort(unique(CE$CEyear))
  missing_in_ce <- setdiff(cl_years, ce_years)
  missing_in_cl <- setdiff(ce_years, cl_years)
  if (length(missing_in_ce) > 0)
    warning(paste("Years present in CL but not CE:", paste(missing_in_ce, collapse = ", ")))
  if (length(missing_in_cl) > 0)
    warning(paste("Years present in CE but not CL:", paste(missing_in_cl, collapse = ", ")))

  return(list(CL = CL, CE = CE))
}



#' Read the ICES preliminary catches zip file
#'
#' @param file Path to the ICES preliminary catches zip file.
#'
#' @return A data.frame.
#' @export
read.prelC <- function(file) {

  if (!file.exists(file)) stop(paste("Preliminary catches file not found:", file))

  fname <- as.character(unzip(file, list = TRUE)$Name)
  fname <- unique(fname[grepl("csv", fname, ignore.case = TRUE)])

  if (length(fname) == 0) stop("No CSV found inside the preliminary catches zip file.")
  if (length(fname) > 1)  fname <- fname[1]   # take the first if multiple

  prelC <- read.table(unz(file, fname), header = TRUE, sep = ",",
                      stringsAsFactors = FALSE)

  prelC$Area <- tolower(gsub("_", ".", prelC$Area))
  prelC <- dplyr::rename(prelC,
                         Lan = AMS.Catch.TLW.,
                         BMS = BMS.Catch.TLW.)
  prelC$Year <- as.character(prelC$Year)

  return(prelC)
}


#' Read the EU fleet register zip file
#'
#' @param file Path to the fleet register zip folder.
#'
#' @return A data.frame.
#' @export
read.fleet <- function(file) {

  if (!file.exists(file)) stop(paste("Fleet register file not found:", file))

  fleet <- read.table(unz(file, "vesselRegistryListResults.csv"),
                      header = TRUE, sep = ";", quote = "",
                      stringsAsFactors = FALSE)
  fleet <- dplyr::mutate(fleet,
                         StartYear = substr(Event.Start.Date, 1, 4),
                         EndYear   = substr(Event.End.Date,   1, 4))
  return(fleet)
}


#' Dynamic Rmarkdown section header (for tabset generation)
#'
#' @param text Header text.
#' @param level Heading level (number of \code{#}).
#'
#' @return Invisibly; called for its side-effect of printing markdown.
#' @export
#'
#' @references https://stackoverflow.com/questions/53444333
catHeader <- function(text = "", level = 3) {
  cat(paste0("\n\n", strrep("#", level), " ", text, "\n"))
}



#' Read RDBES CS ("Table with IDs") zip files and merge hierarchy tables
#'
#' The RDBES CS export zip contains one sub-folder per sampling hierarchy
#' (e.g. \code{H1/}, \code{HSL/}, \code{HVD/}).  Each folder holds CSV files
#' whose names use the full RDBES table names as exported:
#'
#' \itemize{
#'   \item \code{Design.csv}
#'   \item \code{SamplingDetails.csv}
#'   \item \code{VesselSelection.csv}
#'   \item \code{FishingTrip.csv}
#'   \item \code{FishingOperation.csv}
#'   \item \code{TemporalEvent.csv}
#'   \item \code{OnshoreEvent.csv}
#'   \item \code{Location.csv}
#'   \item \code{LandingEvent.csv}
#'   \item \code{SpeciesSelection.csv}
#'   \item \code{Sample.csv}
#'   \item \code{FrequencyMeasure.csv}
#'   \item \code{BiologicalVariable.csv}
#'   \item \code{SpeciesList.csv}                   (auxiliary)
#'   \item \code{IndividualSpeciesInSpeciesList.csv} (auxiliary)
#' }
#'
#' The tables present in each folder depend on the hierarchy (H1-H13).
#' This function reads every folder, applies the correct hierarchy-specific
#' join chain per the RDBES data model documentation, then row-binds all
#' results into a single flat data frame.
#'
#' Join chains by upper hierarchy:
#' \preformatted{
#'  H1:  SD->DE; VS->FT->FO->SS->SA
#'  H2:  SD->DE; FT->FO->SS->SA
#'  H3:  SD->DE; TE->VS->FT->FO->SS->SA
#'  H4:  SD->DE; OS->FT->LE->SS->SA
#'  H5:  SD->DE; OS->LE->SS->SA  (FT optional)
#'  H6:  SD->DE; OS->FT->FO->SS->SA  (LE optional)
#'  H7:  SD->DE; OS->SS->SA  (LE auxiliary)
#'  H8:  SD->DE; TE->VS->LE->SS->SA  (FT optional)
#'  H9:  SD->DE; LO->TE->SS->SA  (LE auxiliary)
#'  H10: SD->DE; VS->TE->FT->FO->SS->SA
#'  H11: SD->DE; LO->TE->FT->SS->SA  (LE auxiliary)
#'  H12: SD->DE; LO->TE->LE->SS->SA  (FT optional)
#'  H13: SD->DE; FO->SS->SA  (FT optional)
#'  Lower A/B: SA->FM; Lower A/C: SA->FM->BV or SA->BV
#' }
#'
#' \strong{Optional:} returns \code{NULL} silently when \code{file} is
#' \code{""} or the file does not exist, so the report skips the CS section.
#'
#' @param file Path to the CS zip (with hierarchy sub-folders) exported from
#'   RDBES in "Table with IDs" format, or \code{""} to skip.
#'
#' @return A single flat data.frame combining all hierarchies, or \code{NULL}.
#' @export
read.cs <- function(file) {

  # ---- Optional: return NULL silently when no file is configured --------------
  if (!nzchar(trimws(file))) return(NULL)
  if (!file.exists(file)) {
    warning(paste("CS file not found - CS section will be skipped:", file))
    return(NULL)
  }
  if (!grepl("\\.zip$", file, ignore.case = TRUE))
    stop("'file' must be a zip archive (.zip).")

  # ---------------------------------------------------------------------------
  # Full CSV filename stems as exported by RDBES "Table with IDs" format
  # ---------------------------------------------------------------------------
  CS_TABLE_NAMES <- c(
    DE = "Design",
    SD = "SamplingDetails",
    VS = "VesselSelection",
    FT = "FishingTrip",
    FO = "FishingOperation",
    TE = "TemporalEvent",
    OS = "OnshoreEvent",
    LO = "Location",
    LE = "LandingEvent",
    SS = "SpeciesSelection",
    SA = "Sample",
    FM = "FrequencyMeasure",
    BV = "BiologicalVariable",
    SL = "SpeciesList",
    IS = "IndividualSpeciesInSpeciesList"
  )

  # ---------------------------------------------------------------------------
  # Per-hierarchy join chains (ordered table abbreviations, SD to SA).
  # SD->DE join is always applied first, separately.
  # Tables not in the chain but present in the folder are silently ignored
  # (e.g. optional auxiliary tables like FT in H5).
  # ---------------------------------------------------------------------------
  CHAINS <- list(
    H1  = c("VS", "FT", "FO", "SS", "SA"),
    H2  = c("FT", "FO", "SS", "SA"),
    H3  = c("TE", "VS", "FT", "FO", "SS", "SA"),
    H4  = c("OS", "FT", "LE", "SS", "SA"),
    H5  = c("OS", "LE", "SS", "SA"),
    H6  = c("OS", "FT", "FO", "SS", "SA"),
    H7  = c("OS", "SS", "SA"),
    H8  = c("TE", "VS", "LE", "SS", "SA"),
    H9  = c("LO", "TE", "SS", "SA"),
    H10 = c("VS", "TE", "FT", "FO", "SS", "SA"),
    H11 = c("LO", "TE", "FT", "SS", "SA"),
    H12 = c("LO", "TE", "LE", "SS", "SA"),
    H13 = c("FO", "SS", "SA")
  )

  # FK candidates for each table (tried in order; first mutual match wins)
  TABLE_FK <- list(
    VS = c("SDid"),
    FT = c("VSid", "SDid", "OSid", "TEid"),
    FO = c("FTid", "TEid", "SDid"),
    TE = c("SDid", "LOid"),
    OS = c("SDid"),
    LO = c("SDid"),
    LE = c("OSid", "FTid", "VSid", "TEid", "LOid", "SDid"),
    SS = c("LEid", "FOid", "FTid", "OSid", "TEid", "LOid", "SDid"),
    SA = c("SSid"),
    FM = c("SAid"),
    BV = c("SAid", "FMid")
  )

  # ---- Inventory the zip -------------------------------------------------------
  zip_contents <- unzip(file, list = TRUE)$Name
  all_csvs     <- zip_contents[grepl("\\.csv$", zip_contents, ignore.case = TRUE)]

  # Sub-folder detection (paths like "H1/Design.csv")
  has_subfolders <- any(grepl("/", all_csvs))
  all_folders <- if (has_subfolders) {
    unique(sub("/.*", "", all_csvs[grepl("/", all_csvs)]))
  } else {
    ""  # flat layout: one anonymous folder
  }

  # Known auxiliary folders that never contain hierarchy tables — skip them.
  # HSL = Species List, HVD = Vessel Details, HIS = Individual Species.
  # We also skip any folder whose name does not look like a hierarchy (H + digits
  # or H + digits + letter), unless it is the only folder present.
  AUX_FOLDERS <- c("HSL", "HVD", "HIS")
  hierarchy_folders <- all_folders[!toupper(all_folders) %in% AUX_FOLDERS]
  if (length(hierarchy_folders) == 0) hierarchy_folders <- all_folders  # fallback

  message(sprintf("CS zip: all folders: %s",  paste(all_folders,       collapse = ", ")))
  message(sprintf("CS zip: processing:   %s",  paste(hierarchy_folders, collapse = ", ")))

  # ---- Helpers ----------------------------------------------------------------

  .read_one <- function(path_in_zip) {
    if (!path_in_zip %in% zip_contents) return(NULL)
    tryCatch(
      read.table(unz(file, path_in_zip),
                 header = TRUE, sep = ",", quote = "",
                 stringsAsFactors = FALSE, fill = TRUE),
      error = function(e) {
        message(sprintf("    Failed to read '%s': %s", path_in_zip, conditionMessage(e)))
        NULL
      }
    )
  }

  # Resolve a table abbreviation to an actual path in the zip.
  # Tries, in order:
  #   1. Exact canonical name  (e.g. "H1/Design.csv")
  #   2. Case-insensitive match of the canonical stem
  #   3. Case-insensitive match of any CSV whose basename contains the stem
  #      or the abbreviation (catches "Sampling_Details.csv", "SD.csv", etc.)
  .find_path <- function(abbr, folder) {
    stem        <- CS_TABLE_NAMES[[abbr]]
    folder_pfx  <- if (nzchar(folder)) paste0(folder, "/") else ""

    # CSVs in this folder only
    in_folder <- all_csvs[startsWith(all_csvs, folder_pfx)]
    basenames  <- tolower(sub(".*/", "", in_folder))           # basename, lower
    stem_lc    <- tolower(stem)
    abbr_lc    <- tolower(abbr)

    # 1. Exact
    exact <- paste0(folder_pfx, stem, ".csv")
    if (exact %in% zip_contents) return(exact)

    # 2. Case-insensitive exact stem
    m <- which(basenames == paste0(stem_lc, ".csv"))
    if (length(m)) return(in_folder[m[1]])

    # 3. Basename contains stem or abbreviation (removes underscores/spaces)
    clean_bases <- gsub("[_ ]", "", basenames)
    m <- which(clean_bases == paste0(gsub("[_ ]", "", stem_lc), ".csv") |
               clean_bases == paste0(abbr_lc, ".csv"))
    if (length(m)) return(in_folder[m[1]])

    NULL  # not found
  }

  .read_abbr <- function(abbr, folder) {
    path <- .find_path(abbr, folder)
    if (is.null(path)) return(NULL)
    df <- .read_one(path)
    if (!is.null(df))
      message(sprintf("    Read %s ('%s'): %d rows, %d cols",
                      abbr, path, nrow(df), ncol(df)))
    df
  }

  # Left-join 'right' onto 'left', finding the best FK match automatically
  .safe_join <- function(left, right, fk_candidates, suffix_label) {
    if (is.null(right) || nrow(right) == 0) return(left)
    join_keys <- intersect(
      intersect(fk_candidates, names(left)),
      intersect(fk_candidates, names(right))
    )
    if (length(join_keys) == 0) return(left)
    dplyr::left_join(left, right, by = join_keys,
                     suffix = c("", paste0(".", suffix_label)))
  }

  # ---- Process each hierarchy folder independently ----------------------------
  folder_results <- lapply(hierarchy_folders, function(fld) {

    message(sprintf("Processing folder: '%s'", if (nzchar(fld)) fld else "(root)"))

    tbls <- lapply(setNames(names(CS_TABLE_NAMES), names(CS_TABLE_NAMES)),
                   function(abbr) .read_abbr(abbr, fld))

    present <- names(tbls)[!sapply(tbls, is.null)]
    message(sprintf("  Tables found: %s", paste(present, collapse = ", ")))

    DE <- tbls[["DE"]];  SD <- tbls[["SD"]]
    if (is.null(DE) || is.null(SD)) {
      message(sprintf("  Folder '%s': Design.csv or SamplingDetails.csv missing - skipped.",
                      if (nzchar(fld)) fld else "(root)"))
      return(NULL)
    }

    # Base join: SD -> DE on DEid
    out <- dplyr::left_join(SD, DE, by = "DEid", suffix = c("", ".DE"))

    # Determine which hierarchy chain to apply.
    # 1) Try exact folder name match (case-insensitive, e.g. "H1" -> CHAINS$H1)
    # 2) Fall back to inferring from which tables are present.
    folder_upper <- toupper(fld)
    if (folder_upper %in% names(CHAINS)) {
      chain <- CHAINS[[folder_upper]]
    } else {
      present <- names(tbls)[!sapply(tbls, is.null)]
      scores  <- sapply(CHAINS, function(ch) sum(ch %in% present))
      best    <- which.max(scores)
      chain   <- CHAINS[[best]]
      message(sprintf("  Folder '%s': hierarchy inferred as %s.",
                      fld, names(CHAINS)[best]))
    }

    # Apply chain step by step
    for (abbr in chain) {
      fk  <- TABLE_FK[[abbr]]
      if (is.null(fk)) next
      out <- .safe_join(out, tbls[[abbr]], fk, abbr)
    }

    # Lower hierarchies: attach FM and BV after SA is in 'out'
    if ("SAid" %in% names(out)) {
      out <- .safe_join(out, tbls[["FM"]], "SAid",          "FM")
      bv_keys <- intersect(c("SAid", "FMid"), names(out))
      out <- .safe_join(out, tbls[["BV"]], bv_keys,         "BV")
    }

    out[[".cs_folder"]] <- fld
    out
  })

  folder_results <- Filter(Negate(is.null), folder_results)
  if (length(folder_results) == 0) {
    warning("CS zip contained no readable hierarchy data.")
    return(NULL)
  }

  out <- dplyr::bind_rows(folder_results)
  out[[".cs_folder"]] <- NULL

  # Fix all-logical columns (empty CSV fields read as logical NA)
  out <- as.data.frame(
    lapply(out, function(x) if (is.logical(x)) as.character(x) else x),
    stringsAsFactors = FALSE
  )
  names(out) <- trimws(names(out))

  message(sprintf("CS data loaded: %d rows, %d columns across %d hierarchy folder(s).",
                  nrow(out), ncol(out), length(hierarchy_folders)))
  out
}
