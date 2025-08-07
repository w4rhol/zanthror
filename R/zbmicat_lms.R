#' Classify BMI using IOTF cutoffs (LMS method)
#'
#' This function classifies BMI values into weight categories using International Obesity
#' Task Force (IOTF) age and sex-specific cutoffs calculated via the LMS method from the
#' sitar package, providing a more accurate alternative to interpolation-based approaches.
#'
#' @param bmi Numeric vector. Body Mass Index values (kg/m²)
#' @param age Numeric vector. Age values (same length as bmi)
#' @param gender Numeric or character vector. Gender codes (same length as bmi)
#' @param male_code Scalar. Code used for males in the gender variable (default: 1)
#' @param female_code Scalar. Code used for females in the gender variable (default: 2)
#' @param age_unit Character. Units for age values. One of "year", "month", "week", "day" (default: "year")
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
#' The function uses the LMS method via the sitar package to calculate IOTF cutoffs,
#' classifying BMI values into:
#' \itemize{
#'   \item Grade 3 thinness (BMI < 16 equivalent at age 18)
#'   \item Grade 2 thinness (BMI 16-17 equivalent at age 18)
#'   \item Grade 1 thinness (BMI 17-18.5 equivalent at age 18)
#'   \item Normal weight (BMI 18.5-25 equivalent at age 18)
#'   \item Overweight (BMI 25-30 equivalent at age 18)
#'   \item Obese (BMI ≥30 equivalent at age 18)
#' }
#'
#' This function uses the LMS method implemented in the sitar package to convert
#' between BMI values and z-scores, providing more accurate age-sex-specific cutoffs
#' than interpolation methods.
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
#' zanthror_testdata$bmi_category <- zbmicat_lms(
#'   bmi = zanthror_testdata$bmi,
#'   age = zanthror_testdata$age_years,
#'   gender = zanthror_testdata$gender
#' )
#' table(zanthror_testdata$bmi_category, useNA = "ifany")
#'
#' # Factor output for statistical analysis
#' bmi_factor <- zbmicat_lms(
#'   bmi = zanthror_testdata$bmi,
#'   age = zanthror_testdata$age_years,
#'   gender = zanthror_testdata$gender,
#'   return = "factor"
#' )
#' levels(bmi_factor)
#'
#' # Labelled output
#' bmi_labelled <- zbmicat_lms(
#'   bmi = zanthror_testdata$bmi,
#'   age = zanthror_testdata$age_years,
#'   gender = zanthror_testdata$gender,
#'   return = "labelled"
#' )
#' labelled::val_labels(bmi_labelled)
#'
#' # Numeric output (values -3 to 2, no labels)
#' bmi_numeric <- zbmicat_lms(
#'   bmi = zanthror_testdata$bmi,
#'   age = zanthror_testdata$age_years,
#'   gender = zanthror_testdata$gender,
#'   return = "numeric"
#' )
#' table(bmi_numeric)
#'
#' # Haven-labelled output
#' bmi_haven <- zbmicat_lms(
#'   bmi = zanthror_testdata$bmi,
#'   age = zanthror_testdata$age_years,
#'   gender = zanthror_testdata$gender,
#'   return = "haven"
#' )
#' attr(bmi_haven, "labels")
#'
#' # With character gender codes
#' zanthror_testdata$gender_char <- ifelse(zanthror_testdata$gender == 1, "M", "F")
#' result <- zbmicat_lms(
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
#' result_months <- zbmicat_lms(
#'   bmi = zanthror_testdata$bmi,
#'   age = age_months,
#'   gender = zanthror_testdata$gender,
#'   age_unit = "month"
#' )
#' table(result_months)
#'
#' @export
#' @importFrom sitar LMS2z
#' @importFrom stats complete.cases setNames
#' @importFrom labelled labelled
#' @importFrom haven labelled as_factor

zbmicat_lms <- function(bmi, age, gender, male_code = 1, female_code = 2,
                        age_unit = "year", wtabbr = FALSE, return = "string") {

  # Input validation
  if (length(bmi) != length(age) || length(bmi) != length(gender)) {
    stop("bmi, age, and gender must have the same length")
  }

  if (!age_unit %in% c("year", "month", "week", "day")) {
    stop("age_unit must be one of: 'year', 'month', 'week', 'day'")
  }

  if (!return %in% c("string", "factor", "labelled", "haven", "numeric")) {
    stop("return must be one of: 'string', 'factor', 'labelled', 'haven', or 'numeric'")
  }

  # Check if sitar package is available
  if (!requireNamespace("sitar", quietly = TRUE)) {
    stop("The sitar package is required for zbmicat_lms. Please install it with: install.packages('sitar')")
  }

  # Convert age to years if necessary
  age_years <- switch(age_unit,
                      "year" = age,
                      "month" = age / 12,
                      "week" = age / (365.25/7),
                      "day" = age / 365.25)

  # Convert gender to numeric vector
  gender_num <- rep(NA_integer_, length(gender))
  if (is.character(gender) || is.factor(gender)) {
    gender_num[gender == as.character(male_code)] <- 1L
    gender_num[gender == as.character(female_code)] <- 2L
  } else {
    gender_num[gender == male_code] <- 1L
    gender_num[gender == female_code] <- 2L
  }

  # Create validity mask
  valid_mask <- !is.na(bmi) & !is.na(age_years) & !is.na(gender_num) &
    age_years >= 2 & age_years <= 18 & bmi > 0

  # Initialize result vector
  result <- rep(NA_character_, length(bmi))

  # Process only valid cases
  if (!any(valid_mask)) {
    return(convert_output_lms(result, return, wtabbr))
  }

  # Extract valid data
  valid_bmi <- bmi[valid_mask]
  valid_age <- age_years[valid_mask]
  valid_gender <- gender_num[valid_mask]

  # Convert gender codes to sitar format
  gender_char <- character(length(valid_gender))
  gender_char[valid_gender == 1] <- "boys"
  gender_char[valid_gender == 2] <- "girls"

  # Remove cases with invalid gender coding
  gender_valid_mask <- gender_char %in% c("boys", "girls")
  if (!any(gender_valid_mask)) {
    return(convert_output_lms(result, return, wtabbr))
  }

  # Further filter for valid gender
  valid_bmi <- valid_bmi[gender_valid_mask]
  valid_age <- valid_age[gender_valid_mask]
  gender_char <- gender_char[gender_valid_mask]

  # Define IOTF cutoff values (BMI values at age 18)
  cutoffs <- c(16, 17, 18.5, 25, 30)

  # Pre-calculate z-scores for cutoffs at age 18 for both sexes
  z_scores_18 <- data.frame(
    cutoff = cutoffs,
    boys = sapply(cutoffs, function(x) {
      tryCatch(sitar::LMS2z(x = 18, y = x, sex = "boys", measure = "bmi", ref = "iotf", toz = TRUE),
               error = function(e) NA)
    }),
    girls = sapply(cutoffs, function(x) {
      tryCatch(sitar::LMS2z(x = 18, y = x, sex = "girls", measure = "bmi", ref = "iotf", toz = TRUE),
               error = function(e) NA)
    })
  )

  # Check if z-score calculation failed
  if (any(is.na(z_scores_18$boys)) || any(is.na(z_scores_18$girls))) {
    warning("Some IOTF z-score calculations failed. Results may be incomplete.")
  }

  # Create data frame for processing
  df <- data.frame(
    bmi = valid_bmi,
    age = valid_age,
    gender = gender_char,
    stringsAsFactors = FALSE
  )

  # Split by gender for vectorized LMS calculations
  boys_idx <- df$gender == "boys"
  girls_idx <- df$gender == "girls"

  # Initialize cutoff matrix
  n_valid <- nrow(df)
  bmi_cutoffs <- matrix(NA, nrow = n_valid, ncol = 5)

  # Vectorized calculation for boys
  if (any(boys_idx) && !any(is.na(z_scores_18$boys))) {
    boys_ages <- df$age[boys_idx]
    for (j in 1:5) {
      tryCatch({
        bmi_cutoffs[boys_idx, j] <- sitar::LMS2z(x = boys_ages, y = z_scores_18$boys[j],
                                                 sex = "boys", measure = "bmi", ref = "iotf", toz = FALSE)
      }, error = function(e) {
        # Handle errors gracefully
      })
    }
  }

  # Vectorized calculation for girls
  if (any(girls_idx) && !any(is.na(z_scores_18$girls))) {
    girls_ages <- df$age[girls_idx]
    for (j in 1:5) {
      tryCatch({
        bmi_cutoffs[girls_idx, j] <- sitar::LMS2z(x = girls_ages, y = z_scores_18$girls[j],
                                                  sex = "girls", measure = "bmi", ref = "iotf", toz = FALSE)
      }, error = function(e) {
        # Handle errors gracefully
      })
    }
  }

  # Vectorized classification
  valid_categories <- rep(NA_character_, n_valid)

  # Check for complete cutoff data
  complete_cases <- complete.cases(bmi_cutoffs)

  if (any(complete_cases)) {
    bmi_vals <- df$bmi[complete_cases]
    cutoffs_complete <- bmi_cutoffs[complete_cases, , drop = FALSE]

    # Define normal weight label based on wtabbr
    normal_weight_label <- if (wtabbr) "Normal wt" else "Normal weight"

    # Vectorized classification
    classifications <- character(length(bmi_vals))
    classifications[bmi_vals < cutoffs_complete[, 1]] <- "Grade 3 thinness"
    classifications[bmi_vals >= cutoffs_complete[, 1] & bmi_vals < cutoffs_complete[, 2]] <- "Grade 2 thinness"
    classifications[bmi_vals >= cutoffs_complete[, 2] & bmi_vals < cutoffs_complete[, 3]] <- "Grade 1 thinness"
    classifications[bmi_vals >= cutoffs_complete[, 3] & bmi_vals < cutoffs_complete[, 4]] <- normal_weight_label
    classifications[bmi_vals >= cutoffs_complete[, 4] & bmi_vals < cutoffs_complete[, 5]] <- "Overweight"
    classifications[bmi_vals >= cutoffs_complete[, 5]] <- "Obese"

    valid_categories[complete_cases] <- classifications
  }

  # Map back to original positions
  original_positions <- which(valid_mask)[gender_valid_mask]
  result[original_positions] <- valid_categories

  # Convert output based on return parameter
  return(convert_output_lms(result, return, wtabbr))
}

# Helper function to convert output format for LMS version
convert_output_lms <- function(result, return, wtabbr) {

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
  labels <- c(labels[1:3], stats::setNames(0, normal_weight_label), labels[4:5])

  if (return == "labelled") {
    if (!requireNamespace("labelled", quietly = TRUE)) {
      stop("The labelled package is required for labelled output. Please install it with: install.packages('labelled')")
    }
    return(labelled::labelled(numeric_result, labels = labels))
  }

  if (return == "haven") {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("The haven package is required for haven output. Please install it with: install.packages('haven')")
    }
    return(haven::labelled(numeric_result, labels = labels))
  }

  return(result)
}
