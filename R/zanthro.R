#' Calculate Z-scores for anthropometric measures in children and adolescents
#'
#' This function calculates z-scores for anthropometric measurements using the LMS method
#' and reference data from various growth charts: 2000 CDC Growth Reference, British 1990
#' Growth Reference, WHO Child Growth Standards, WHO Reference 2007, UK-WHO Preterm Growth
#' Reference, and UK-WHO Term Growth Reference.
#'
#' @param measure Numeric vector of anthropometric measurements (e.g., height, weight, BMI)
#' @param xvar Numeric vector of the x-variable (usually age, but can be length/height for
#'   weight-for-length/height charts). Units specified by ageunit parameter.
#' @param chart Character string specifying the growth chart code. Valid options:
#'   \itemize{
#'     \item \strong{Length/Height charts:} "la" (length-for-age), "ha" (height-for-age)
#'     \item \strong{Weight charts:} "wa" (weight-for-age), "wl" (weight-for-length),
#'       "wh" (weight-for-height)
#'     \item \strong{BMI charts:} "ba" (BMI-for-age)
#'     \item \strong{Head circumference:} "hca" (head circumference-for-age)
#'     \item \strong{UK-specific charts:} "sha" (sitting height-for-age), "lla" (leg length-for-age),
#'       "wsa" (waist-for-age), "bfa" (body fat-for-age)
#'     \item \strong{WHO skinfolds:} "aca" (arm circumference-for-age), "ssa" (subscapular skinfold-for-age),
#'       "tsa" (triceps skinfold-for-age)
#'   }
#' @param version Character string specifying the reference population. Valid options:
#'   \itemize{
#'     \item "US" - 2000 CDC Growth Reference
#'     \item "UK" - British 1990 Growth Reference
#'     \item "WHO" - WHO Child Growth Standards (0-5y) + WHO Reference 2007 (5-19y)
#'     \item "UKWHOpreterm" - UK-WHO composite for preterm births
#'     \item "UKWHOterm" - UK-WHO composite for term births (37-42 weeks gestation)
#'   }
#' @param gender Vector (string or numeric) indicating gender for each observation
#' @param male_code The code used for males in the gender variable (e.g., "M", "Male", 1, "m")
#' @param female_code The code used for females in the gender variable (e.g., "F", "Female", 2, "f")
#' @param gencode Character string specifying gender codes in format "male=M female=F" or
#'   "female=F male=M" (deprecated - use male_code and female_code instead). Can use any codes
#'   (e.g., "male=1 female=2", "m=Male f=Female"). Comma between codes is optional.
#' @param ageunit Character string specifying age units when xvar represents age.
#'   Options: "day", "week", "month", "year" (default). Not used for weight-for-length/height charts.
#'   Conversions: 1 year = 12 months = 365.25/7 weeks = 365.25 days
#' @param gestage Numeric vector of gestational ages in weeks (optional). Enables age adjustment
#'   for gestational age. Default assumption is 40 weeks. Ages are corrected by (gestage-40) weeks.
#'   Warning given if any gestational age > 42 weeks. Only valid for age-based charts.
#' @param nocutoff Logical. If FALSE (default), z-scores with absolute values ≥5 are set to NA
#'   to flag potential data entry errors. If TRUE, all z-scores are returned regardless of magnitude.
#'
#' @return Numeric vector of z-scores with the same length as the input measure vector.
#'   NA values indicate: age out of range, missing gender, gestational age placing corrected
#'   age out of range, non-positive measurements, or |z-score| ≥ 5 (unless nocutoff=TRUE).
#'
#' @details
#' \strong{Chart-Version Compatibility:}
#' \itemize{
#'   \item UK charts only: "sha", "lla", "wsa", "bfa"
#'   \item US/WHO charts only: "wl", "wh"
#'   \item WHO charts only: "aca", "ssa", "tsa"
#'   \item US charts only: "la"
#'   \item Other charts available in multiple versions
#' }
#'
#' \strong{Age Ranges by Chart and Version:}
#' \itemize{
#'   \item \strong{US CDC 2000:} la (0-35.5 months), ha (2-20 years), wa (0-20 years),
#'     ba (2-20 years), hca (0-36 months), wl (45-103.5 cm), wh (77-121.5 cm)
#'   \item \strong{UK 1990:} ha (0-23 years), wa (0-23 years), ba (0-23 years),
#'     hca (0-18y males, 0-17y females), sha (0-23 years), lla (0-23 years),
#'     wsa (3-17 years), bfa (4.75-19.83 years)
#'   \item \strong{WHO:} ha (0-19 years), wa (0-10 years), ba (0-19 years),
#'     hca (0-5 years), aca/ssa/tsa (0.25-5 years), wl (45-110 cm), wh (65-120 cm)
#'   \item \strong{UK-WHO:} ha/wa (0-20 years), ba (0.038-20 years),
#'     hca (0-18y males, 0-17y females)
#' }
#'
#' \strong{Measurement Units:}
#' Length/height: cm, Weight: kg, BMI: kg/m², Head circumference: cm,
#' Sitting height: cm, Leg length: cm, Waist: cm, Body fat: %,
#' Arm circumference: cm, Skinfolds: mm
#'
#' @examples
#' # Load reference data (assumes CSV is in working directory)
#' # Basic weight-for-age z-scores using WHO charts
#' z_wa <- zanthro(measure = c(10.5, 12.2, 14.8),
#'                xvar = c(1, 2, 3),
#'                chart = "wa",
#'                version = "WHO",
#'                gender = c("M", "F", "M"),
#'                male_code = "M",
#'                female_code = "F")
#'
#' # Height-for-age with numeric gender codes and age in months
#' z_ha <- zanthro(measure = c(75, 85, 95),
#'                xvar = c(12, 24, 36),
#'                chart = "ha",
#'                version = "WHO",
#'                gender = c(1, 2, 1),
#'                male_code = 1,
#'                female_code = 2,
#'                ageunit = "month")
#'
#' # BMI-for-age with gestational age correction
#' z_bmi <- zanthro(measure = c(15.2, 16.8, 17.5),
#'                 xvar = c(2.5, 3.0, 3.5),
#'                 chart = "ba",
#'                 version = "UK",
#'                 gender = c("Male", "Female", "Male"),
#'                 male_code = "Male",
#'                 female_code = "Female",
#'                 gestage = c(38, 35, 42))
#'
#' # Weight-for-height (xvar is height, not age)
#' z_wh <- zanthro(measure = c(12, 15, 18),
#'                xvar = c(80, 90, 100),
#'                chart = "wh",
#'                version = "WHO",
#'                gender = c("M", "F", "M"),
#'                male_code = "M",
#'                female_code = "F")
#'
#' # Including extreme values (no cutoff)
#' z_wa_all <- zanthro(measure = c(8, 25, 35),
#'                    xvar = c(2, 3, 4),
#'                    chart = "wa",
#'                    version = "WHO",
#'                    gender = c("M", "F", "M"),
#'                    male_code = "M",
#'                    female_code = "F",
#'                    nocutoff = TRUE)
#'
#' @export
zanthro <- function(measure, xvar, chart, version, gender, male_code = NULL, female_code = NULL, gencode = NULL,
                    ageunit = "year", gestage = NULL, nocutoff = FALSE) {

  # Use internal package data (zanthro_ref_data is available via sysdata.rda)
  # No need to load - it's automatically available in package environment

  # Input validation
  valid_versions <- c("US", "UK", "WHO", "UKWHOpreterm", "UKWHOterm")
  if (!version %in% valid_versions) {
    stop(paste(version, "is an invalid version. Valid choices are:", paste(valid_versions, collapse = ", ")))
  }

  valid_charts <- c("la", "ha", "wa", "ba", "hca", "wh", "wl", "sha", "lla", "wsa", "aca", "ssa", "tsa", "bfa")
  if (!chart %in% valid_charts) {
    stop(paste(chart, "is an invalid chart code. Valid chart codes are:", paste(valid_charts, collapse = ", ")))
  }

  # Chart-version compatibility checks (based on Stata help file)
  chart_version_rules <- list(
    # UK-only charts
    sha = "UK", lla = "UK", wsa = "UK", bfa = "UK",
    # US/WHO charts
    wl = c("US", "WHO"), wh = c("US", "WHO"),
    # WHO-only charts
    aca = "WHO", ssa = "WHO", tsa = "WHO",
    # US-only charts
    la = "US"
    # Other charts (ha, wa, ba, hca) available in multiple versions
  )

  if (chart %in% names(chart_version_rules)) {
    if (!version %in% chart_version_rules[[chart]]) {
      valid_versions_for_chart <- chart_version_rules[[chart]]
      stop(paste("For chart code", chart, ", valid versions are:",
                 paste(valid_versions_for_chart, collapse = " and ")))
    }
  }

  # Handle gender codes - flexible approach with backwards compatibility
  if (!is.null(male_code) && !is.null(female_code)) {
    # New flexible approach - convert to character for consistency
    male_code <- as.character(male_code)
    female_code <- as.character(female_code)

  } else if (!is.null(gencode)) {
    # Legacy gencode approach - parse the string
    warning("The 'gencode' parameter is deprecated. Use 'male_code' and 'female_code' instead.")

    # Parse gender codes - handle flexible format
    # Remove commas, split on spaces, remove empty elements
    gencode_clean <- gsub(",", " ", gencode)
    gencode_parts <- unlist(strsplit(gencode_clean, "\\s+"))
    gencode_parts <- gencode_parts[gencode_parts != ""]

    if (length(gencode_parts) != 6 || gencode_parts[2] != "=" || gencode_parts[5] != "=") {
      stop("gencode() option invalid. Format should be 'male=M female=F' or 'female=F male=M'")
    }

    # Determine which code is for male/female based on first term
    first_term <- tolower(gencode_parts[1])
    if (startsWith(first_term, "m")) {  # "male" or "m"
      male_code <- gencode_parts[3]
      fourth_term <- tolower(gencode_parts[4])
      if (!startsWith(fourth_term, "f")) {  # Should be "female" or "f"
        stop("gencode() option invalid - expected female specification after male")
      }
      female_code <- gencode_parts[6]
    } else if (startsWith(first_term, "f")) {  # "female" or "f"
      female_code <- gencode_parts[3]
      fourth_term <- tolower(gencode_parts[4])
      if (!startsWith(fourth_term, "m")) {  # Should be "male" or "m"
        stop("gencode() option invalid - expected male specification after female")
      }
      male_code <- gencode_parts[6]
    } else {
      stop("gencode() option invalid - first term should start with 'male', 'm', 'female', or 'f'")
    }

  } else {
    # Try to auto-detect common gender codes
    unique_genders <- unique(gender[!is.na(gender)])
    unique_genders <- as.character(unique_genders)  # Convert to character

    # Common patterns for auto-detection
    male_patterns <- c("M", "Male", "MALE", "male", "m", "1", "Boy", "boy")
    female_patterns <- c("F", "Female", "FEMALE", "female", "f", "2", "Girl", "girl")

    detected_male <- intersect(unique_genders, male_patterns)
    detected_female <- intersect(unique_genders, female_patterns)

    if (length(detected_male) == 1 && length(detected_female) == 1) {
      male_code <- detected_male
      female_code <- detected_female
      cat("Auto-detected gender codes: male =", male_code, ", female =", female_code, "\n")
    } else {
      stop("Gender codes not specified and could not be auto-detected. ",
           "Please specify 'male_code' and 'female_code' parameters. ",
           "Found gender values: ", paste(unique_genders, collapse = ", "))
    }
  }

  # Convert gender variable to character for comparison
  gender_char <- as.character(gender)

  # Validate gender variable values
  unique_genders <- unique(gender_char[!is.na(gender_char)])
  valid_codes <- c(male_code, female_code)
  if (!all(unique_genders %in% valid_codes)) {
    invalid_genders <- setdiff(unique_genders, valid_codes)
    stop(paste("The gender variable contains invalid values:", paste(invalid_genders, collapse = ", "),
               ". Valid codes are:", paste(valid_codes, collapse = ", ")))
  }

  # Validate age unit for age-based charts
  age_based_charts <- c("la", "ha", "wa", "ba", "hca", "sha", "lla", "wsa", "aca", "ssa", "tsa", "bfa")
  if (chart %in% age_based_charts) {
    if (!ageunit %in% c("day", "week", "month", "year")) {
      stop("The ageunit option must be day, week, month or year")
    }
  } else {
    # For wl and wh charts, xvar is length/height, not age
    if (!missing(ageunit) && ageunit != "year") {
      warning("ageunit option ignored for weight-for-length/height charts")
    }
    if (!is.null(gestage)) {
      warning("gestage option ignored for weight-for-length/height charts")
    }
  }

  # Map chart and version combination to reference filename
  filename_map <- list(
    # UK charts
    "lla_UK" = "zllageuk.dta", "sha_UK" = "zshtageuk.dta", "wsa_UK" = "zwsageuk.dta", "bfa_UK" = "zbfageuk.dta",
    "hca_UK" = "zhcageuk.dta", "ba_UK" = "zbmiageuk.dta", "wa_UK" = "zwtageuk.dta", "ha_UK" = "zhtageuk.dta",

    # US charts
    "la_US" = "zlenageius.dta", "hca_US" = "zhcageius.dta", "ba_US" = "zbmiageus.dta",
    "wa_US" = "zwtagecomus.dta", "ha_US" = "zhtageus.dta", "wh_US" = "zwthtus.dta", "wl_US" = "zwtlenius.dta",

    # WHO charts
    "aca_WHO" = "zacagewho.dta", "ssa_WHO" = "zssagewho.dta", "tsa_WHO" = "ztsagewho.dta",
    "hca_WHO" = "zhcagewho.dta", "ba_WHO" = "zbmiagewho.dta", "wa_WHO" = "zwtagewho.dta",
    "ha_WHO" = "zlhagewho.dta", "wh_WHO" = "zwthtwho.dta", "wl_WHO" = "zwtlenwho.dta",

    # UK-WHO preterm charts
    "hca_UKWHOpreterm" = "zhcageukwhopreterm.dta", "ba_UKWHOpreterm" = "zbmiageukwhopreterm.dta",
    "wa_UKWHOpreterm" = "zwtageukwhopreterm.dta", "ha_UKWHOpreterm" = "zlhtageukwhopreterm.dta",

    # UK-WHO term charts
    "hca_UKWHOterm" = "zhcageukwhoterm.dta", "ba_UKWHOterm" = "zbmiageukwhoterm.dta",
    "wa_UKWHOterm" = "zwtageukwhoterm.dta", "ha_UKWHOterm" = "zlhtageukwhoterm.dta"
  )

  chart_key <- paste(chart, version, sep = "_")
  if (!chart_key %in% names(filename_map)) {
    stop(paste("No reference data available for chart", chart, "version", version))
  }

  target_filename <- filename_map[[chart_key]]

  # Filter reference data for this specific chart
  ref_data <- zanthro_ref_data[zanthro_ref_data$filename == target_filename, ]
  if (nrow(ref_data) == 0) {
    stop(paste("Reference data not found for", target_filename))
  }

  # Handle column name variations (R may add X prefix to names starting with special chars)
  col_names <- colnames(ref_data)

  # Create mapping for column names (with or without X prefix)
  get_col_name <- function(base_name) {
    if (base_name %in% col_names) {
      return(base_name)
    } else if (paste0("X", base_name) %in% col_names) {
      return(paste0("X", base_name))
    } else {
      stop(paste("Column", base_name, "not found in reference data"))
    }
  }

  # Get the correct column names
  sex_col <- get_col_name("__SVJCKHsex")
  xmrg_col <- get_col_name("__SVJCKHxmrg")
  xvar_col <- get_col_name("__SVJCKHxvar")
  xvar_nx_col <- get_col_name("__SVJCKHxvar_nx")
  l_col <- get_col_name("__SVJCKHl")
  l_nx_col <- get_col_name("__SVJCKHl_nx")
  m_col <- get_col_name("__SVJCKHm")
  m_nx_col <- get_col_name("__SVJCKHm_nx")
  s_col <- get_col_name("__SVJCKHs")
  s_nx_col <- get_col_name("__SVJCKHs_nx")
  agegp_col <- get_col_name("__SVJCKHagegp")

  # Convert age to years and calculate tday (age in days * 10000) for age-based charts
  if (chart %in% age_based_charts) {
    # Age conversion using exact factors from Stata help file
    t_years <- switch(ageunit,
                      "year" = xvar,
                      "month" = xvar / 12,
                      "week" = xvar / (365.25/7),  # 1 year = 365.25/7 weeks
                      "day" = xvar / 365.25
    )

    tday <- switch(ageunit,
                   "year" = xvar * 365.25 * 10000,
                   "month" = xvar * (365.25/12) * 10000,  # 1 month = 365.25/12 days
                   "week" = xvar * 7 * 10000,
                   "day" = xvar * 10000
    )

    # Adjust for gestational age if provided (only for age-based charts)
    if (!is.null(gestage)) {
      max_gestage <- max(gestage, na.rm = TRUE)
      if (max_gestage > 42) {
        warning(paste("Maximum value in gestational age variable is", max_gestage, "weeks"))
      }
      # Adjust age by (gestational_age - 40) weeks
      t_years <- t_years + (gestage - 40) * 7 / 365.25
      tday <- tday + (gestage - 40) * 7 * 10000
    }
  } else {
    # For weight-for-length/height charts, xvar is the length/height measurement
    # Don't convert to age - use the measurement directly but scale for lookup
    t_years <- xvar  # This is actually length/height in cm, not years
    tday <- xvar * 10000  # Scale to match reference data format
  }

  # Convert gender to numeric codes (1=male, 2=female as per Stata convention)
  sex_numeric <- ifelse(gender_char == male_code, 1,
                        ifelse(gender_char == female_code, 2, NA))

  # Calculate age groups for special chart types (based on Stata code logic)
  agegp <- NULL
  if ((chart %in% c("ha", "ba") && version == "WHO")) {
    # WHO height and BMI charts have 0-2 and 2-5 year sections
    agegp <- ifelse(t_years < 2, 1, 2)
  } else if (version %in% c("UKWHOpreterm", "UKWHOterm")) {
    # UK-WHO charts have 4 age groups based on tday thresholds
    agegp <- ifelse(tday < 140000, 1,
                    ifelse(tday >= 140000 & t_years < 2, 2,
                           ifelse(t_years >= 2 & t_years < 4, 3, 4)))
  }

  # Initialize output vector
  z_scores <- rep(NA, length(measure))

  # Process each observation individually
  for (i in seq_along(measure)) {
    # Skip if essential data is missing or invalid
    if (is.na(measure[i]) || is.na(sex_numeric[i]) || is.na(tday[i]) || measure[i] <= 0) {
      next
    }

    # Find matching reference data subset
    if (!is.null(agegp)) {
      # Charts with age groups (WHO ha/ba, UK-WHO charts)
      ref_subset <- ref_data[ref_data[[sex_col]] == sex_numeric[i] &
                               !is.na(ref_data[[agegp_col]]) &
                               ref_data[[agegp_col]] == agegp[i], ]
    } else {
      # Charts without age groups
      ref_subset <- ref_data[ref_data[[sex_col]] == sex_numeric[i], ]
    }

    if (nrow(ref_subset) == 0) next

    # Find the appropriate reference values for interpolation
    xmrg_values <- ref_subset[[xmrg_col]]

    # Check for exact match first
    exact_match <- which(ref_subset[[xmrg_col]] == tday[i])
    if (length(exact_match) > 0) {
      # Exact age match found
      row_idx <- exact_match[1]
      l_val <- ref_subset[[l_col]][row_idx]
      m_val <- ref_subset[[m_col]][row_idx]
      s_val <- ref_subset[[s_col]][row_idx]
    } else {
      # Need interpolation between reference points
      below_indices <- which(xmrg_values <= tday[i])
      above_indices <- which(xmrg_values > tday[i])

      if (length(below_indices) == 0 || length(above_indices) == 0) {
        next  # Age/measurement out of range for this chart
      }

      # Get the bracketing reference points
      lower_idx <- max(below_indices)
      upper_idx <- min(above_indices)

      # Linear interpolation (simplified from Stata's more complex interpolation)
      x1 <- ref_subset[[xvar_col]][lower_idx]
      x2 <- ref_subset[[xvar_nx_col]][lower_idx]

      if (is.na(x1) || is.na(x2) || x2 == x1) {
        next  # Cannot interpolate
      }

      # Calculate interpolation fraction
      frac <- (t_years[i] - x1) / (x2 - x1)

      # Interpolate L, M, S parameters
      l1 <- ref_subset[[l_col]][lower_idx]
      l2 <- ref_subset[[l_nx_col]][lower_idx]
      m1 <- ref_subset[[m_col]][lower_idx]
      m2 <- ref_subset[[m_nx_col]][lower_idx]
      s1 <- ref_subset[[s_col]][lower_idx]
      s2 <- ref_subset[[s_nx_col]][lower_idx]

      if (any(is.na(c(l1, l2, m1, m2, s1, s2)))) {
        next  # Missing reference parameters
      }

      l_val <- l1 + frac * (l2 - l1)
      m_val <- m1 + frac * (m2 - m1)
      s_val <- s1 + frac * (s2 - s1)
    }

    # Calculate z-score using LMS method (Box-Cox transformation)
    if (!is.na(l_val) && !is.na(m_val) && !is.na(s_val) && m_val > 0) {
      if (l_val == 0) {
        # When L=0, use logarithmic transformation
        z_scores[i] <- log(measure[i] / m_val) / s_val
      } else {
        # Standard LMS formula
        z_scores[i] <- ((measure[i] / m_val)^l_val - 1) / (l_val * s_val)
      }

      # Apply cutoff if enabled (default behavior)
      if (!nocutoff && !is.na(z_scores[i]) && abs(z_scores[i]) >= 5) {
        z_scores[i] <- NA
      }
    }
  }

  # Generate summary statistics and messages (matching Stata output format)
  valid_count <- sum(!is.na(z_scores))
  missing_count <- sum(is.na(z_scores))

  # Report results
  plural_s <- ifelse(valid_count != 1, "s", "")
  cat("Z value", plural_s, " generated for ", valid_count, " case", plural_s, "\n", sep = "")
  cat("(gender was assumed to be coded male=", male_code, ", female=", female_code, ")\n", sep = "")

  if (chart %in% age_based_charts) {
    cat("(age was assumed to be in ", ageunit, "s)\n", sep = "")
  }

  if (missing_count > 0) {
    cat("(Z values can be missing because ")
    reasons <- c()
    if (chart %in% age_based_charts) {
      reasons <- c(reasons, "age is out of range for the chart")
    } else {
      reasons <- c(reasons, "measurement is out of range for the chart")
    }
    reasons <- c(reasons, "the gender variable is missing")
    if (!is.null(gestage)) {
      reasons <- c(reasons, "gestation age is missing or places corrected age out of range")
    }
    if (!nocutoff) {
      reasons <- c(reasons, "the Z value has an absolute value >=5")
    }
    cat(paste(reasons, collapse = ", "), ")\n", sep = "")
  }

  return(z_scores)
}
