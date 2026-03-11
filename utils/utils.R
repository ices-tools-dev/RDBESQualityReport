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
