#' Read RDBES exported zip files (CL and CE tables)
#'
#' Supports both "Table with IDs" format (CommercialLanding/CommercialEffort)
#' and "Upload" format (HCL/HCE). Detects the format automatically.
#'
#' @param file Character vector of length 2: paths to the HCE and HCL zip files.
#'
#' @return Named list with elements \code{CL} and \code{CE} (data.frames).
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

  # ---- Identify CL and CE files -----------------------------------------------
  CLfiles <- unique(file[grepl("HCL", file, ignore.case = TRUE)])
  CEfiles <- unique(file[grepl("HCE", file, ignore.case = TRUE)])

  if (length(CLfiles) == 0) stop("No CL (HCL) file found. Check file names.")
  if (length(CEfiles) == 0) stop("No CE (HCE) file found. Check file names.")

  # ---- Detect format -----------------------------------------------------------
  filenamCL <- unzip(CLfiles[1], list = TRUE)$Name
  filenamCE <- unzip(CEfiles[1], list = TRUE)$Name

  csvnamCL <- sub("\\.csv$", "", filenamCL[grepl("^(CommercialLanding|CommercialEffort|HCE|HCL).*\\.csv$", filenamCL)])
  csvnamCE <- sub("\\.csv$", "", filenamCE[grepl("^(CommercialLanding|CommercialEffort|HCE|HCL).*\\.csv$", filenamCE)])

  if (length(csvnamCL) == 0 || length(csvnamCE) == 0) {
    stop("Could not detect RDBES file format. Expected 'CommercialLanding.csv', 'CommercialEffort.csv', 'HCL.csv', or 'HCE.csv' inside the zip files.")
  }

  format_CL <- unique(csvnamCL)
  format_CE <- unique(csvnamCE)

  # ---- Helper: read one or many zips ------------------------------------------
  .read_zips <- function(zip_files, csv_name, has_header) {
    read_one <- function(z) {
      read.table(unz(z, paste0(csv_name, ".csv")),
                 header = has_header, sep = ",", quote = "",
                 stringsAsFactors = FALSE)
    }
    if (length(zip_files) > 1) {
      do.call(rbind, lapply(zip_files, read_one))
    } else {
      read_one(zip_files)
    }
  }

  # ---- Read data ---------------------------------------------------------------
  if (format_CL == "CommercialLanding" && format_CE == "CommercialEffort") {
    # Table with IDs format
    CL <- .read_zips(CLfiles, "CommercialLanding", has_header = TRUE)
    CE <- .read_zips(CEfiles, "CommercialEffort",  has_header = TRUE)

  } else if (format_CL == "HCL" && format_CE == "HCE") {
    # Upload format
    CL <- .read_zips(CLfiles, "HCL", has_header = FALSE)
    CL <- cbind(CLid = seq_len(nrow(CL)), CL)

    CE <- .read_zips(CEfiles, "HCE", has_header = FALSE)
    CE <- cbind(CEid = seq_len(nrow(CE)), CE)

  } else {
    stop(paste0(
      "Mixed or unrecognised RDBES formats.\n",
      "  CL format detected: '", format_CL, "'\n",
      "  CE format detected: '", format_CE, "'\n",
      "Both files must use the same format (either 'Table with IDs' or 'Upload')."
    ))
  }

  # ---- Assign canonical column names ------------------------------------------
  # Source of truth: RDBES_Data_Model_CL_CE.xlsx, "R Name" column.
  # CL has 60 fields (including CLid); CE has 63 fields (including CEid).

  CL_NAMES <- c(
    "CLid",                        # PK — not in upload format, added synthetically
    "CLrecType",                   # 1
    "CLdBasSciWeig",               # 2
    "CLdSouSciWeig",               # 3
    "CLsampScheme",                # 4
    "CLdSouLanVal",                # 5  (data source of landing value)
    "CLlanCou",                    # 6
    "CLvesFlagCou",                # 7
    "CLyear",                      # 8
    "CLquar",                      # 9
    "CLmonth",                     # 10
    "CLarea",                      # 11
    "CLstatRect",                  # 12
    "CLdatBasStatRect",            # 13
    "CLdSoucstatRect",             # 14
    "CLfishManUnit",               # 15
    "CLgsaSubarea",                # 16
    "CLjurisdArea",                # 17
    "CLfishAreaCat",               # 18
    "CLfreshWatNam",               # 19
    "CLeconZone",                  # 20
    "CLeconZoneIndi",              # 21
    "CLspecCode",                  # 22
    "CLspecFAO",                   # 23
    "CLlandCat",                   # 24
    "CLcatchCat",                  # 25
    "CLregDisCat",                 # 26
    "CLsizeCatScale",              # 27
    "CLsizeCat",                   # 28
    "CLnatFishAct",                # 29
    "CLmetier6",                   # 30
    "CLIBmitiDev",                 # 31
    "CLloc",                       # 32
    "CLvesLenCat",                 # 33
    "CLfishTech",                  # 34
    "CLmesSizRan",                 # 35
    "CLsupReg",                    # 36
    "CLgeoInd",                    # 37
    "CLspeConTech",                # 38
    "CLdeepSeaReg",                # 39
    "CLFDIconCod",                 # 40
    "CLoffWeight",                 # 41
    "CLsciWeight",                 # 42
    "CLexpDiff",                   # 43
    "CLlanVal",                    # 44  NOTE: was wrongly "CLtotOffLanVal" — corrected from data model
    "CLtotNumFish",                # 45
    "CLnumUniqVes",                # 46
    "CLsciWeightErrMeaValTyp",     # 47
    "CLsciWeightErrMeaValFirst",   # 48
    "CLsciWeightErrMeaValSecond",  # 49
    "CLvalErrMeaValTyp",           # 50
    "CLvalErrMeaValFirst",         # 51
    "CLvalErrMeaValSecond",        # 52
    "CLnumFishInCatchErrMeaValTyp",   # 53
    "CLnumFishInCatchErrMeaValFirst", # 54
    "CLnumFishInCatchErrMeaValSecond",# 55
    "CLcom",                       # 56
    "CLsciWeightQualBias",         # 57
    "CLconfiFlag",                 # 58
    "CLencrypVesIds"               # 59  (trimws handles trailing space in xlsx)
  )

  CE_NAMES <- c(
    "CEid",                        # PK — not in upload format, added synthetically
    "CErecType",                   # 1
    "CEdBasSciEff",                # 2
    "CEdSouSciEff",                # 3
    "CEsampScheme",                # 4
    "CEvesFlagCou",                # 5
    "CEyear",                      # 6
    "CEquar",                      # 7
    "CEmonth",                     # 8
    "CEarea",                      # 9
    "CEstatRect",                  # 10
    "CEdatBasStatRect",            # 11
    "CEsoucStatRect",              # 12
    "CEfishManUnit",               # 13
    "CEgsaSubarea",                # 14
    "CEjurisdArea",                # 15
    "CEfishAreaCat",               # 16
    "CEfreshWatNam",               # 17
    "CEeconZone",                  # 18
    "CEeconZoneIndi",              # 19  NOTE: was wrongly "CLeconZoneIndi" — corrected from data model
    "CEnatFishAct",                # 20
    "CEmetier6",                   # 21
    "CEIBmitiDev",                 # 22
    "CEloc",                       # 23
    "CEvesLenCat",                 # 24
    "CEfishTech",                  # 25
    "CEmesSizRan",                 # 26
    "CEsupReg",                    # 27
    "CEgeoInd",                    # 28
    "CEspeConTech",                # 29
    "CEdeepSeaReg",                # 30
    "CEoffVesHoursAtSea",          # 31
    "CEnumFracTrips",              # 32
    "CEnumDomTrip",                # 33
    "CEoffDaySea",                 # 34
    "CEsciDaySea",                 # 35  NOTE: was wrongly "CESciDaySea" — corrected from data model
    "CEoffFishDay",                # 36
    "CEsciFishDay",                # 37
    "CEoffNumHaulSet",             # 38
    "CEsciNumHaulSet",             # 39
    "CEoffVesFishHour",            # 40
    "CEsciVesFishHour",            # 41
    "CEoffSoakMeterHour",          # 42
    "CEsciSoakMeterHour",          # 43
    "CEoffkWDaySea",               # 44
    "CEscikWDaySea",               # 45
    "CEoffkWFishDay",              # 46
    "CEscikWFishDay",              # 47
    "CEoffkWFishHour",             # 48
    "CEscikWFishHour",             # 49
    "CEgTDaySea",                  # 50
    "CEgTFishDay",                 # 51
    "CEgTFishHour",                # 52
    "CEnumUniqVes",                # 53
    "CEgearDim",                   # 54
    "CEnumFAD",                    # 55
    "CEnumSupVes",                 # 56
    "CEfishDaysErrMeaValTyp",      # 57
    "CEfishDaysErrMeaValFirst",    # 58
    "CEfishDaysErrMeaValSecond",   # 59
    "CEscientificFishingDaysQualBias", # 60
    "CEconfiFlag",                 # 61
    "CEencrypVesIds"               # 62
  )

  # Validate column counts before renaming (helps diagnose format mismatches)
  if (ncol(CL) != length(CL_NAMES)) {
    warning(sprintf(
      "CL has %d columns but %d names are defined. Columns will not be renamed. Check the RDBES export version.",
      ncol(CL), length(CL_NAMES)
    ))
  } else {
    names(CL) <- CL_NAMES
  }

  if (ncol(CE) != length(CE_NAMES)) {
    warning(sprintf(
      "CE has %d columns but %d names are defined. Columns will not be renamed. Check the RDBES export version.",
      ncol(CE), length(CE_NAMES)
    ))
  } else {
    names(CE) <- CE_NAMES
  }

  # ---- Fix issue #12: coerce all-logical columns to character -----------------
  # Empty CSV columns are read as logical NA by R; convert them to character.
  CL <- as.data.frame(lapply(CL, function(x) if (is.logical(x)) as.character(x) else x),
                      stringsAsFactors = FALSE)
  CE <- as.data.frame(lapply(CE, function(x) if (is.logical(x)) as.character(x) else x),
                      stringsAsFactors = FALSE)

  # ---- Fix issue #16: normalise column name casing ----------------------------
  # Already handled above via canonical name vectors, but guard against
  # any residual case variation from the "Table with IDs" format.
  names(CL) <- trimws(names(CL))
  names(CE) <- trimws(names(CE))

  # ---- Type coercions ---------------------------------------------------------
  CL$CLyear <- as.character(CL$CLyear)
  CE$CEyear  <- as.character(CE$CEyear)

  CL$CLoffWeight <- suppressWarnings(as.numeric(CL$CLoffWeight))
  CL$CLsciWeight <- suppressWarnings(as.numeric(CL$CLsciWeight))
  CL$CLlanVal    <- suppressWarnings(as.numeric(CL$CLlanVal))
  CE$CEsciFishDay <- suppressWarnings(as.numeric(CE$CEsciFishDay))
  CE$CEsciDaySea  <- suppressWarnings(as.numeric(CE$CEsciDaySea))

  # ---- Factors for plotting ---------------------------------------------------
  VES_LEN_LEVELS <- c("VL0006", "VL0608", "VL0810", "VL1012",
                       "VL1215", "VL1518", "VL1824", "VL2440", "VL40XX")

  CL$CLmonth    <- factor(CL$CLmonth,    levels = as.character(1:12))
  CL$CLquar     <- factor(CL$CLquar,     levels = as.character(1:4))
  CL$CLvesLenCat <- factor(CL$CLvesLenCat, levels = VES_LEN_LEVELS)

  CE$CEmonth    <- factor(CE$CEmonth,    levels = as.character(1:12))
  CE$CEquar     <- factor(CE$CEquar,     levels = as.character(1:4))
  CE$CEvesLenCat <- factor(CE$CEvesLenCat, levels = VES_LEN_LEVELS)

  # ---- Issue #13: warn if key fields differ between CL and CE -----------------
  cl_years <- sort(unique(CL$CLyear))
  ce_years <- sort(unique(CE$CEyear))
  missing_in_ce <- setdiff(cl_years, ce_years)
  missing_in_cl <- setdiff(ce_years, cl_years)
  if (length(missing_in_ce) > 0) {
    warning(paste("Years present in CL but not CE:", paste(missing_in_ce, collapse = ", ")))
  }
  if (length(missing_in_cl) > 0) {
    warning(paste("Years present in CE but not CL:", paste(missing_in_cl, collapse = ", ")))
  }

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
