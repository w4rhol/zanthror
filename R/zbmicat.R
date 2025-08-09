#' Classify BMI using IOTF cutoffs (Stata zbmicat replication)
#'
#' This function replicates the functionality of Stata's zbmicat command,
#' classifying BMI values into weight categories using International Obesity
#' Task Force (IOTF) age and sex-specific cutoffs with interpolation.
#'
#' @param bmi Numeric vector. Body Mass Index values (kg/m²)
#' @param age Numeric vector. Age values (same length as bmi)
#' @param gender Numeric or character vector. Gender codes (same length as bmi)
#' @param male_code Scalar. Code used for males in the gender variable (default: 1)
#' @param female_code Scalar. Code used for females in the gender variable (default: 2)
#' @param ageunit Character. Units for age values. One of "year", "month", "week", "day" (default: "year")
#' @param wtabbr Logical. If TRUE, returns "Normal wt"; if FALSE, returns "Normal weight" (default: FALSE)
#' @param return Character. Output format: "string", "factor", "labelled", "haven", or "numeric" (default: "string")
#'
#' @return Vector of BMI classifications. Format depends on \code{return} parameter:
#' \itemize{
#'   \item "string": Character vector with category names
#'   \item "factor": Ordered factor with proper level ordering
#'   \item "labelled": Labelled numeric vector with values -3 to 2 (requires labelled package)
#'   \item "haven": Haven-labelled numeric vector with values -3 to 2 (requires haven package)
#'   \item "numeric": Plain numeric vector with values -3 to 2 (no labels)
#' }
#'
#' @details
#' The function uses IOTF reference data (Cole & Lobstein, 2012) to classify BMI values into:
#' \itemize{
#'   \item Grade 3 thinness (BMI < 16 equivalent at age 18)
#'   \item Grade 2 thinness (BMI 16-17 equivalent at age 18)
#'   \item Grade 1 thinness (BMI 17-18.5 equivalent at age 18)
#'   \item Normal weight (BMI 18.5-25 equivalent at age 18)
#'   \item Overweight (BMI 25-30 equivalent at age 18)
#'   \item Obese (BMI ≥30 equivalent at age 18)
#' }
#'
#' Age and sex-specific cutoffs are calculated using the same interpolation methods
#' as Stata's zbmicat command:
#' \itemize{
#'   \item Linear interpolation for ages 2-2.5 and 17.5-18 years
#'   \item Cubic interpolation for ages 2.5-17.5 years
#'   \item No interpolation when age matches reference grid exactly
#' }
#'
#' Missing values (NA) are returned for:
#' \itemize{
#'   \item Missing BMI, age, or gender values
#'   \item BMI ≤ 0
#'   \item Age < 2 or > 18 years
#'   \item Invalid gender codes
#' }
#'
#' @references
#' Cole, T. J., & Lobstein, T. (2012). Extended international (IOTF) body mass index
#' cut-offs for thinness, overweight and obesity. \emph{Pediatric Obesity}, 7(4), 284-294.
#'
#' Cole, T. J., Bellizzi, M. C., Flegal, K. M., & Dietz, W. H. (2000). Establishing a
#' standard definition for child overweight and obesity worldwide: international survey.
#' \emph{BMJ}, 320(7244), 1240-1243.
#'
#' @examples
#' # Load test data
#' data(zanthror_testdata)
#'
#' # Basic usage - string output
#' zanthror_testdata$bmi_category <- zbmicat(
#'   bmi = zanthror_testdata$bmi,
#'   age = zanthror_testdata$age_years,
#'   gender = zanthror_testdata$gender
#' )
#' table(zanthror_testdata$bmi_category, useNA = "ifany")
#'
#' # Factor output for statistical analysis
#' bmi_factor <- zbmicat(
#'   bmi = zanthror_testdata$bmi,
#'   age = zanthror_testdata$age_years,
#'   gender = zanthror_testdata$gender,
#'   return = "factor"
#' )
#' levels(bmi_factor)
#'
#' # Labelled output
#' bmi_labelled <- zbmicat(
#'   bmi = zanthror_testdata$bmi,
#'   age = zanthror_testdata$age_years,
#'   gender = zanthror_testdata$gender,
#'   return = "labelled"
#' )
#' labelled::val_labels(bmi_labelled)
#'
#' # Numeric output (values -3 to 2, no labels)
#' bmi_numeric <- zbmicat(
#'   bmi = zanthror_testdata$bmi,
#'   age = zanthror_testdata$age_years,
#'   gender = zanthror_testdata$gender,
#'   return = "numeric"
#' )
#' table(bmi_numeric)
#'
#' # Haven-labelled output
#' bmi_haven <- zbmicat(
#'   bmi = zanthror_testdata$bmi,
#'   age = zanthror_testdata$age_years,
#'   gender = zanthror_testdata$gender,
#'   return = "haven"
#' )
#' attr(bmi_haven, "labels")
#'
#' # With character gender codes
#' zanthror_testdata$gender_char <- ifelse(zanthror_testdata$gender == 1, "M", "F")
#' result <- zbmicat(
#'   bmi = zanthror_testdata$bmi,
#'   age = zanthror_testdata$age_years,
#'   gender = zanthror_testdata$gender_char,
#'   male_code = "M",
#'   female_code = "F"
#' )
#' table(result)
#'
#' # Age in months
#' age_months <- zanthror_testdata$age_years * 12
#' result_months <- zbmicat(
#'   bmi = zanthror_testdata$bmi,
#'   age = age_months,
#'   gender = zanthror_testdata$gender,
#'   ageunit = "month"
#' )
#' table(result_months)
#'
#' @export
#' @importFrom stats approx complete.cases
#' @importFrom labelled labelled
#' @importFrom haven labelled as_factor

# version 57

zbmicat <- function(bmi, age, gender, male_code = 1, female_code = 2, ageunit = "year", wtabbr = FALSE, return = "string") {

  # Validate inputs
  if (length(bmi) != length(age) || length(bmi) != length(gender)) {
    stop("bmi, age, and gender must have the same length")
  }

  if (!ageunit %in% c("day", "week", "month", "year")) {
    stop("ageunit must be one of: 'day', 'week', 'month', 'year'")
  }

  if (!return %in% c("string", "factor", "labelled", "haven", "numeric")) {
    stop("return must be one of: 'string', 'factor', 'labelled', 'haven', or 'numeric'")
  }

  # Load the IOTF reference data from package internal data
  if (!exists("iotf_data")) {
    stop("IOTF reference data not found. This function requires the package internal data.")
  }
  # Note: In a package, iotf_data would be automatically available from R/sysdata.rda

  # Convert age to years if needed
  age_years <- switch(ageunit,
                      "year" = age,
                      "month" = age / 12,
                      "week" = age / (365.25 / 7),
                      "day" = age / 365.25
  )

  # Convert gender to numeric vector
  gender_num <- rep(NA_integer_, length(gender))
  if (is.character(gender) || is.factor(gender)) {
    gender_num[gender == as.character(male_code)] <- 1L
    gender_num[gender == as.character(female_code)] <- 2L
  } else {
    gender_num[gender == male_code] <- 1L
    gender_num[gender == female_code] <- 2L
  }

  # Create validity mask - IOTF only valid for ages 2-18
  valid_mask <- !is.na(bmi) & !is.na(age_years) & !is.na(gender_num) &
    bmi > 0 & age_years >= 2 & age_years <= 18

  # Initialize result vector
  result <- rep(NA_character_, length(bmi))

  # Process only valid cases
  if (!any(valid_mask)) {
    return(convert_output(result, return, wtabbr))
  }

  # Subset to valid cases for vectorized operations
  valid_bmi <- bmi[valid_mask]
  valid_age <- age_years[valid_mask]
  valid_gender <- gender_num[valid_mask]

  # Vectorized age rounding (matching Stata logic)
  age_int <- floor(valid_age)
  age_diff <- valid_age - age_int
  age_rounded <- ifelse(age_diff >= 0.5, age_int + 0.5, age_int)

  # Create age-sex combinations for merging
  age_sex_combo <- paste(valid_gender, age_rounded, sep = "_")
  iotf_combo <- paste(iotf_data$X__IOTFsex, iotf_data$X__IOTFage, sep = "_")

  # Vectorized merge with IOTF data
  match_indices <- match(age_sex_combo, iotf_combo)
  valid_matches <- !is.na(match_indices)

  if (!any(valid_matches)) {
    return(convert_output(result, return, wtabbr))
  }

  # Subset to cases with IOTF matches
  final_valid <- which(valid_mask)[valid_matches]
  matched_bmi <- valid_bmi[valid_matches]
  matched_age <- valid_age[valid_matches]
  matched_rounded_age <- age_rounded[valid_matches]
  matched_iotf_rows <- iotf_data[match_indices[valid_matches], ]

  # Vectorized age fraction calculation
  age_frac <- ifelse(matched_age > 2 & matched_age < 18,
                     (matched_age - matched_rounded_age) / 0.5,
                     0)
  age_frac2 <- age_frac * age_frac

  # Vectorized interpolation for all 5 cutoffs
  cutoffs_matrix <- matrix(NA, nrow = length(matched_bmi), ncol = 5)
  colnames(cutoffs_matrix) <- c("16", "17", "18_5", "25", "30")

  for (i in 1:5) {
    cutoff_name <- colnames(cutoffs_matrix)[i]

    # Column indices for this cutoff
    col_pre <- paste0("X__IOTF", cutoff_name, "_pre")
    col_current <- paste0("X__IOTF", cutoff_name)
    col_nx <- paste0("X__IOTF", cutoff_name, "_nx")
    col_nx2 <- paste0("X__IOTF", cutoff_name, "_nx2")

    # Safety check: ensure we have data to process
    if (length(matched_bmi) == 0) {
      next  # Skip this cutoff if no data
    }

    # Extract values for vectorized computation
    pre_vals <- matched_iotf_rows[[col_pre]]
    curr_vals <- matched_iotf_rows[[col_current]]
    nx_vals <- matched_iotf_rows[[col_nx]]
    nx2_vals <- matched_iotf_rows[[col_nx2]]

    # Safety check: ensure all vectors have data
    if (length(pre_vals) == 0 || length(curr_vals) == 0 ||
        length(nx_vals) == 0 || length(nx2_vals) == 0) {
      next  # Skip this cutoff if missing reference data
    }

    # Vectorized interpolation logic
    # No interpolation when age equals reference age
    exact_age <- matched_age == matched_rounded_age

    # Linear interpolation for edge segments
    linear_seg <- (matched_age > 2 & matched_age < 2.5) |
      (matched_age > 17.5 & matched_age < 18)

    # Cubic interpolation for middle segment
    cubic_seg <- matched_age > 2.5 & matched_age < 17.5

    # Calculate interpolated values with safety checks
    cutoff_vals <- rep(NA_real_, length(matched_bmi))

    # Safety check: ensure all logical vectors have the right length
    if (length(exact_age) != length(matched_bmi) ||
        length(linear_seg) != length(matched_bmi) ||
        length(cubic_seg) != length(matched_bmi)) {
      stop("Length mismatch in interpolation vectors")
    }

    # Exact age - no interpolation
    exact_indices <- which(exact_age)
    if (length(exact_indices) > 0 && length(curr_vals[exact_indices]) > 0) {
      cutoff_vals[exact_indices] <- curr_vals[exact_indices]
    }

    # Linear interpolation
    linear_indices <- which(linear_seg)
    if (length(linear_indices) > 0 && length(curr_vals[linear_indices]) > 0) {
      cutoff_vals[linear_indices] <- curr_vals[linear_indices] +
        age_frac[linear_indices] * (nx_vals[linear_indices] - curr_vals[linear_indices])
    }

    # Cubic interpolation (vectorized)
    cubic_indices <- which(cubic_seg)
    if (length(cubic_indices) > 0 && length(curr_vals[cubic_indices]) > 0) {
      a0 <- -pre_vals/6 + curr_vals/2 - nx_vals/2 + nx2_vals/6
      a1 <- pre_vals/2 - curr_vals + nx_vals/2
      a2 <- -pre_vals/3 - curr_vals/2 + nx_vals - nx2_vals/6
      a3 <- curr_vals

      cutoff_vals[cubic_indices] <- a0[cubic_indices] * age_frac[cubic_indices] * age_frac2[cubic_indices] +
        a1[cubic_indices] * age_frac2[cubic_indices] +
        a2[cubic_indices] * age_frac[cubic_indices] +
        a3[cubic_indices]
    }

    cutoffs_matrix[, i] <- cutoff_vals
  }

  # Vectorized classification
  valid_cutoffs <- complete.cases(cutoffs_matrix)
  if (any(valid_cutoffs)) {
    final_indices <- final_valid[valid_cutoffs]
    final_bmi <- matched_bmi[valid_cutoffs]
    final_cutoffs <- cutoffs_matrix[valid_cutoffs, ]

    # Vectorized BMI classification with wtabbr option
    normal_weight_label <- if (wtabbr) "Normal wt" else "Normal weight"

    classifications <- character(length(final_bmi))
    classifications[final_bmi < final_cutoffs[, 1]] <- "Grade 3 thinness"
    classifications[final_bmi >= final_cutoffs[, 1] & final_bmi < final_cutoffs[, 2]] <- "Grade 2 thinness"
    classifications[final_bmi >= final_cutoffs[, 2] & final_bmi < final_cutoffs[, 3]] <- "Grade 1 thinness"
    classifications[final_bmi >= final_cutoffs[, 3] & final_bmi < final_cutoffs[, 4]] <- normal_weight_label
    classifications[final_bmi >= final_cutoffs[, 4] & final_bmi < final_cutoffs[, 5]] <- "Overweight"
    classifications[final_bmi >= final_cutoffs[, 5]] <- "Obese"

    # Assign results back to original positions
    result[final_indices] <- classifications
  }

  # Convert output based on return parameter
  return(convert_output(result, return, wtabbr))
}

# Helper function to convert output format
convert_output <- function(result, return, wtabbr) {

  if (return == "string") {
    return(result)
  }

  # Define normal weight label based on wtabbr
  normal_weight_label <- if (wtabbr) "Normal wt" else "Normal weight"

  if (return == "factor") {
    factor_levels <- c("Grade 3 thinness", "Grade 2 thinness", "Grade 1 thinness",
                       normal_weight_label, "Overweight", "Obese")
    result_factor <- factor(result, levels = factor_levels, ordered = TRUE)
    return(result_factor)
  }

  # For numeric, labelled, and haven - convert strings to numeric values
  numeric_result <- rep(NA_integer_, length(result))
  numeric_result[result == "Grade 3 thinness"] <- -3L
  numeric_result[result == "Grade 2 thinness"] <- -2L
  numeric_result[result == "Grade 1 thinness"] <- -1L
  numeric_result[result == normal_weight_label] <- 0L
  numeric_result[result == "Overweight"] <- 1L
  numeric_result[result == "Obese"] <- 2L

  if (return == "numeric") {
    return(numeric_result)
  }

  # Create labels for labelled and haven formats
  labels <- c("Grade 3 thinness" = -3, "Grade 2 thinness" = -2, "Grade 1 thinness" = -1,
              "Overweight" = 1, "Obese" = 2)
  # Add the normal weight label with value 0
  labels <- c(labels[1:3], setNames(0, normal_weight_label), labels[4:5])

  if (return == "labelled") {
    return(labelled::labelled(numeric_result, labels = labels))
  }

  if (return == "haven") {
    return(haven::labelled(numeric_result, labels = labels))
  }

  return(result)
}
