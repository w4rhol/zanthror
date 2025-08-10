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
#' # Load the test dataset
#' data(zanthror_testdata)
#'
#' # Basic weight-for-age z-scores using WHO charts
#' z_wa <- zanthro(measure = zanthror_testdata$weight_kg,
#'                xvar = zanthror_testdata$age_years,
#'                chart = "wa",
#'                version = "WHO",
#'                gender = zanthror_testdata$gender_label,
#'                male_code = "Male",
#'                female_code = "Female")
#'
#' # Height-for-age with numeric gender codes
#' z_ha <- zanthro(measure = zanthror_testdata$height_cm,
#'                xvar = zanthror_testdata$age_years,
#'                chart = "ha",
#'                version = "WHO",
#'                gender = zanthror_testdata$gender,
#'                male_code = 1,
#'                female_code = 2)
#'
#' # BMI-for-age using UK charts with auto-detection of gender codes
#' z_bmi <- zanthro(measure = zanthror_testdata$bmi,
#'                 xvar = zanthror_testdata$age_years,
#'                 chart = "ba",
#'                 version = "UK",
#'                 gender = zanthror_testdata$gender_label)  # Auto-detects Male/Female
#'
#' # Head circumference-for-age using US charts, subset of young children
#' young_children <- zanthror_testdata$age_years <= 3
#' z_hc <- zanthro(measure = zanthror_testdata$head_circumference_cm[young_children],
#'                xvar = zanthror_testdata$age_years[young_children],
#'                chart = "hca",
#'                version = "US",
#'                gender = zanthror_testdata$gender_label[young_children],
#'                male_code = "Male",
#'                female_code = "Female")
#'
#' # Weight-for-height (xvar is height, not age)
#' z_wh <- zanthro(measure = zanthror_testdata$weight_kg,
#'                xvar = zanthror_testdata$height_cm,
#'                chart = "wh",
#'                version = "WHO",
#'                gender = zanthror_testdata$gender_label,
#'                male_code = "Male",
#'                female_code = "Female")
#'
#' # WHO skinfold measurements for young children
#' young_subset <- zanthror_testdata$age_years <= 5
#' z_triceps <- zanthro(measure = zanthror_testdata$triceps_skinfold_mm[young_subset],
#'                     xvar = zanthror_testdata$age_years[young_subset],
#'                     chart = "tsa",
#'                     version = "WHO",
#'                     gender = zanthror_testdata$gender_label[young_subset],
#'                     male_code = "Male",
#'                     female_code = "Female")
#'
#' # UK-specific measurements (sitting height)
#' # Note: Only available for UK participants in the dataset
#' uk_participants <- !is.na(zanthror_testdata$sitting_height_cm)
#' z_sitting <- zanthro(measure = zanthror_testdata$sitting_height_cm[uk_participants],
#'                     xvar = zanthror_testdata$age_years[uk_participants],
#'                     chart = "sha",
#'                     version = "UK",
#'                     gender = zanthror_testdata$gender_label[uk_participants],
#'                     male_code = "Male",
#'                     female_code = "Female")
#'
#' # Including extreme values (no cutoff)
#' z_wa_nocutoff <- zanthro(measure = zanthror_testdata$weight_kg,
#'                         xvar = zanthror_testdata$age_years,
#'                         chart = "wa",
#'                         version = "WHO",
#'                         gender = zanthror_testdata$gender_label,
#'                         male_code = "Male",
#'                         female_code = "Female",
#'                         nocutoff = TRUE)
#'
#' # Add z-scores directly to the dataset
#' zanthror_testdata$z_weight_who <- zanthro(
#'   measure = zanthror_testdata$weight_kg,
#'   xvar = zanthror_testdata$age_years,
#'   chart = "wa",
#'   version = "WHO",
#'   gender = zanthror_testdata$gender_label,
#'   male_code = "Male",
#'   female_code = "Female"
#' )
#'
#' @export
zanthro <- function(measure, xvar, chart, version, gender, male_code = NULL, female_code = NULL,
                    ageunit = "year", gestage = NULL, nocutoff = FALSE) {

  # Use internal package data (zanthro_ref_data is available via sysdata.rda)

  # Input validation (same as before, but grouped for efficiency)
  valid_versions <- c("US", "UK", "WHO", "UKWHOpreterm", "UKWHOterm")
  valid_charts <- c("la", "ha", "wa", "ba", "hca", "wh", "wl", "sha", "lla", "wsa", "aca", "ssa", "tsa", "bfa")

  if (!version %in% valid_versions) {
    stop(paste(version, "is an invalid version. Valid choices are:", paste(valid_versions, collapse = ", ")))
  }
  if (!chart %in% valid_charts) {
    stop(paste(chart, "is an invalid chart code. Valid chart codes are:", paste(valid_charts, collapse = ", ")))
  }

  # Chart-version compatibility checks
  chart_version_rules <- list(
    sha = "UK", lla = "UK", wsa = "UK", bfa = "UK",
    wl = c("US", "WHO"), wh = c("US", "WHO"),
    aca = "WHO", ssa = "WHO", tsa = "WHO", la = "US"
  )

  if (chart %in% names(chart_version_rules)) {
    if (!version %in% chart_version_rules[[chart]]) {
      valid_versions_for_chart <- chart_version_rules[[chart]]
      stop(paste("For chart code", chart, ", valid versions are:",
                 paste(valid_versions_for_chart, collapse = " and ")))
    }
  }

  # Handle gender codes
  if (!is.null(male_code) && !is.null(female_code)) {
    male_code <- as.character(male_code)
    female_code <- as.character(female_code)
  } else {
    # Auto-detection
    unique_genders <- unique(gender[!is.na(gender)])
    unique_genders <- as.character(unique_genders)

    male_patterns <- c("M", "Male", "MALE", "male", "m", "1", "Boy", "boy")
    female_patterns <- c("F", "Female", "FEMALE", "female", "f", "2", "Girl", "girl")

    detected_male <- intersect(unique_genders, male_patterns)
    detected_female <- intersect(unique_genders, female_patterns)

    if (length(detected_male) == 1 && length(detected_female) == 1) {
      male_code <- detected_male
      female_code <- detected_female
    } else {
      stop("Gender codes not specified and could not be auto-detected. ",
           "Please specify 'male_code' and 'female_code' parameters.")
    }
  }

  # Validate gender variable values
  gender_char <- as.character(gender)
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

  # Map chart and version to filename
  filename_map <- list(
    "la_US" = "zlenageius.dta", "ha_US" = "zhtageus.dta", "wa_US" = "zwtagecomus.dta",
    "ba_US" = "zbmiageus.dta", "hca_US" = "zhcageius.dta", "wh_US" = "zwthtus.dta", "wl_US" = "zwtlenius.dta",
    "ha_UK" = "zhtageuk.dta", "wa_UK" = "zwtageuk.dta", "ba_UK" = "zbmiageuk.dta", "hca_UK" = "zhcageuk.dta",
    "sha_UK" = "zshtageuk.dta", "lla_UK" = "zllageuk.dta", "wsa_UK" = "zwsageuk.dta", "bfa_UK" = "zbfageuk.dta",
    "ha_WHO" = "zlhagewho.dta", "wa_WHO" = "zwtagewho.dta", "ba_WHO" = "zbmiagewho.dta", "hca_WHO" = "zhcagewho.dta",
    "aca_WHO" = "zacagewho.dta", "ssa_WHO" = "zssagewho.dta", "tsa_WHO" = "ztsagewho.dta",
    "wh_WHO" = "zwthtwho.dta", "wl_WHO" = "zwtlenwho.dta",
    "ha_UKWHOpreterm" = "zlhtageukwhopreterm.dta", "wa_UKWHOpreterm" = "zwtageukwhopreterm.dta",
    "ba_UKWHOpreterm" = "zbmiageukwhopreterm.dta", "hca_UKWHOpreterm" = "zhcageukwhopreterm.dta",
    "ha_UKWHOterm" = "zlhtageukwhoterm.dta", "wa_UKWHOterm" = "zwtageukwhoterm.dta",
    "ba_UKWHOterm" = "zbmiageukwhoterm.dta", "hca_UKWHOterm" = "zhcageukwhoterm.dta"
  )

  chart_key <- paste(chart, version, sep = "_")
  target_filename <- filename_map[[chart_key]]
  if (is.null(target_filename)) {
    stop(paste("No reference data available for chart", chart, "version", version))
  }

  # Filter reference data for this specific chart
  ref_data <- zanthro_ref_data[zanthro_ref_data$filename == target_filename, ]
  if (nrow(ref_data) == 0) {
    stop(paste("Reference data not found for", target_filename))
  }

  # Handle column name variations (X prefix)
  col_names <- colnames(ref_data)
  get_col_name <- function(base_name) {
    if (base_name %in% col_names) {
      return(base_name)
    } else if (paste0("X", base_name) %in% col_names) {
      return(paste0("X", base_name))
    } else {
      stop(paste("Column", base_name, "not found in reference data"))
    }
  }

  # Get correct column names
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

  # Vectorized data preparation
  n_obs <- length(measure)

  # Convert age/measurement and calculate tday (vectorized)
  if (chart %in% age_based_charts) {
    t_years <- switch(ageunit,
                      "year" = xvar,
                      "month" = xvar / 12,
                      "week" = xvar / (365.25/7),
                      "day" = xvar / 365.25
    )

    tday <- switch(ageunit,
                   "year" = xvar * 365.25 * 10000,
                   "month" = xvar * (365.25/12) * 10000,
                   "week" = xvar * 7 * 10000,
                   "day" = xvar * 10000
    )

    # Adjust for gestational age if provided (vectorized)
    if (!is.null(gestage)) {
      if (max(gestage, na.rm = TRUE) > 42) {
        warning(paste("Maximum value in gestational age variable is", max(gestage, na.rm = TRUE), "weeks"))
      }
      t_years <- t_years + (gestage - 40) * 7 / 365.25
      tday <- tday + (gestage - 40) * 7 * 10000
    }
  } else {
    t_years <- xvar
    tday <- xvar * 10000
  }

  # Convert gender to numeric (vectorized)
  sex_numeric <- ifelse(gender_char == male_code, 1,
                        ifelse(gender_char == female_code, 2, NA))

  # Calculate age groups for special charts (vectorized)
  if ((chart %in% c("ha", "ba") && version == "WHO")) {
    agegp <- ifelse(t_years < 2, 1, 2)
  } else if (version %in% c("UKWHOpreterm", "UKWHOterm")) {
    agegp <- ifelse(tday < 140000, 1,
                    ifelse(tday >= 140000 & t_years < 2, 2,
                           ifelse(t_years >= 2 & t_years < 4, 3, 4)))
  } else {
    agegp <- NULL
  }

  # VECTORIZED Z-SCORE CALCULATION
  z_scores <- rep(NA_real_, n_obs)

  # Create valid observation mask
  valid_mask <- !is.na(measure) & !is.na(sex_numeric) & !is.na(tday) & measure > 0

  if (sum(valid_mask) == 0) {
    # No valid observations
    return(z_scores)
  }

  # Process each sex-agegp combination separately for efficiency
  unique_combinations <- if (!is.null(agegp)) {
    unique(data.frame(sex = sex_numeric[valid_mask], agegp = agegp[valid_mask]))
  } else {
    unique(data.frame(sex = sex_numeric[valid_mask], agegp = NA))
  }

  for (i in seq_len(nrow(unique_combinations))) {
    sex_val <- unique_combinations$sex[i]
    agegp_val <- unique_combinations$agegp[i]

    # Find observations for this combination
    if (!is.null(agegp)) {
      obs_mask <- valid_mask & sex_numeric == sex_val & agegp == agegp_val
      ref_subset <- ref_data[ref_data[[sex_col]] == sex_val &
                               !is.na(ref_data[[agegp_col]]) &
                               ref_data[[agegp_col]] == agegp_val, ]
    } else {
      obs_mask <- valid_mask & sex_numeric == sex_val
      ref_subset <- ref_data[ref_data[[sex_col]] == sex_val, ]
    }

    if (nrow(ref_subset) == 0 || sum(obs_mask) == 0) next

    # Get data for this subset
    obs_tday <- tday[obs_mask]
    obs_tyears <- t_years[obs_mask]
    obs_measure <- measure[obs_mask]

    # Vectorized interpolation using approx()
    ref_xmrg <- ref_subset[[xmrg_col]]
    ref_xvar <- ref_subset[[xvar_col]]
    ref_xvar_nx <- ref_subset[[xvar_nx_col]]
    ref_l <- ref_subset[[l_col]]
    ref_l_nx <- ref_subset[[l_nx_col]]
    ref_m <- ref_subset[[m_col]]
    ref_m_nx <- ref_subset[[m_nx_col]]
    ref_s <- ref_subset[[s_col]]
    ref_s_nx <- ref_subset[[s_nx_col]]

    # Find exact matches and interpolation needs
    exact_matches <- match(obs_tday, ref_xmrg)
    needs_interp <- is.na(exact_matches)

    # Initialize LMS values
    l_vals <- rep(NA_real_, length(obs_tday))
    m_vals <- rep(NA_real_, length(obs_tday))
    s_vals <- rep(NA_real_, length(obs_tday))

    # Handle exact matches (vectorized)
    if (any(!needs_interp)) {
      exact_indices <- exact_matches[!needs_interp]
      l_vals[!needs_interp] <- ref_l[exact_indices]
      m_vals[!needs_interp] <- ref_m[exact_indices]
      s_vals[!needs_interp] <- ref_s[exact_indices]
    }

    # Handle interpolation (vectorized)
    if (any(needs_interp)) {
      interp_tday <- obs_tday[needs_interp]
      interp_tyears <- obs_tyears[needs_interp]

      # Check if observations are within range
      in_range <- interp_tday >= min(ref_xmrg, na.rm = TRUE) &
        interp_tday <= max(ref_xmrg, na.rm = TRUE)

      if (any(in_range)) {
        interp_tday_valid <- interp_tday[in_range]
        interp_tyears_valid <- interp_tyears[in_range]

        # Find bracketing points (vectorized)
        ref_order <- order(ref_xmrg)
        ref_xmrg_sorted <- ref_xmrg[ref_order]

        # Use findInterval for efficient lookup
        intervals <- findInterval(interp_tday_valid, ref_xmrg_sorted)
        intervals <- pmax(1, pmin(intervals, length(ref_xmrg_sorted) - 1))

        lower_indices <- ref_order[intervals]
        upper_indices <- ref_order[intervals + 1]

        # Linear interpolation (vectorized)
        x1 <- ref_xvar[lower_indices]
        x2 <- ref_xvar_nx[lower_indices]

        # Check for valid interpolation
        valid_interp <- !is.na(x1) & !is.na(x2) & x2 != x1

        if (any(valid_interp)) {
          frac <- (interp_tyears_valid[valid_interp] - x1[valid_interp]) /
            (x2[valid_interp] - x1[valid_interp])

          # Interpolate L, M, S (vectorized)
          l_interp <- ref_l[lower_indices[valid_interp]] +
            frac * (ref_l_nx[lower_indices[valid_interp]] - ref_l[lower_indices[valid_interp]])
          m_interp <- ref_m[lower_indices[valid_interp]] +
            frac * (ref_m_nx[lower_indices[valid_interp]] - ref_m[lower_indices[valid_interp]])
          s_interp <- ref_s[lower_indices[valid_interp]] +
            frac * (ref_s_nx[lower_indices[valid_interp]] - ref_s[lower_indices[valid_interp]])

          # Map back to original positions
          needs_interp_indices <- which(needs_interp)
          in_range_indices <- needs_interp_indices[in_range]
          valid_interp_indices <- in_range_indices[valid_interp]

          l_vals[valid_interp_indices] <- l_interp
          m_vals[valid_interp_indices] <- m_interp
          s_vals[valid_interp_indices] <- s_interp
        }
      }
    }

    # Calculate z-scores (vectorized LMS transformation)
    valid_lms <- !is.na(l_vals) & !is.na(m_vals) & !is.na(s_vals) & m_vals > 0

    if (any(valid_lms)) {
      obs_subset_indices <- which(obs_mask)
      valid_subset_indices <- obs_subset_indices[valid_lms]

      l_valid <- l_vals[valid_lms]
      m_valid <- m_vals[valid_lms]
      s_valid <- s_vals[valid_lms]
      measure_valid <- obs_measure[valid_lms]

      # LMS formula (vectorized)
      z_subset <- ifelse(l_valid == 0,
                         log(measure_valid / m_valid) / s_valid,
                         ((measure_valid / m_valid)^l_valid - 1) / (l_valid * s_valid))

      z_scores[valid_subset_indices] <- z_subset
    }
  }

  # Apply cutoff if enabled (vectorized)
  if (!nocutoff) {
    z_scores[abs(z_scores) >= 5] <- NA
  }

  # Generate summary statistics and messages
  valid_count <- sum(!is.na(z_scores))
  missing_count <- sum(is.na(z_scores))

  # Report results (same as before)
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
