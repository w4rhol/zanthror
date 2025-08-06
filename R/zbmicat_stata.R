# zbmicat_stata: Vectorized R replication of Stata's zbmicat function
# Uses the actual IOTF reference data from zbmicat.dta with optimized performance

zbmicat_stata <- function(bmi, age, gender, male_code = 1, female_code = 2, age_unit = "year", wtabbr = FALSE, return = "string") {

  # Validate inputs
  if (length(bmi) != length(age) || length(bmi) != length(gender)) {
    stop("bmi, age, and gender must have the same length")
  }

  if (!age_unit %in% c("day", "week", "month", "year")) {
    stop("age_unit must be one of: 'day', 'week', 'month', 'year'")
  }

  if (!return %in% c("string", "factor", "labelled")) {
    stop("return must be one of: 'string', 'factor', or 'labelled'")
  }

  # Load the IOTF reference data from package internal data
  if (!exists("iotf_data")) {
    stop("IOTF reference data not found. This function requires the package internal data.")
  }
  # Note: In a package, iotf_data would be automatically available from R/sysdata.rda

  # Convert age to years if needed
  age_years <- switch(age_unit,
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

  # Create validity mask
  valid_mask <- !is.na(bmi) & !is.na(age_years) & !is.na(gender_num) &
    bmi > 0 & age_years >= 2 & age_years <= 18

  # Initialize result vector
  result <- rep(NA_character_, length(bmi))

  # Process only valid cases
  if (!any(valid_mask)) {
    return(result)
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
    return(result)
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

    # Extract values for vectorized computation
    pre_vals <- matched_iotf_rows[[col_pre]]
    curr_vals <- matched_iotf_rows[[col_current]]
    nx_vals <- matched_iotf_rows[[col_nx]]
    nx2_vals <- matched_iotf_rows[[col_nx2]]

    # Vectorized interpolation logic
    # No interpolation when age equals reference age
    exact_age <- matched_age == matched_rounded_age

    # Linear interpolation for edge segments
    linear_seg <- (matched_age > 2 & matched_age < 2.5) |
      (matched_age > 17.5 & matched_age < 18)

    # Cubic interpolation for middle segment
    cubic_seg <- matched_age > 2.5 & matched_age < 17.5

    # Calculate interpolated values
    cutoff_vals <- rep(NA_real_, length(matched_bmi))

    # Exact age - no interpolation
    cutoff_vals[exact_age] <- curr_vals[exact_age]

    # Linear interpolation
    if (any(linear_seg)) {
      cutoff_vals[linear_seg] <- curr_vals[linear_seg] +
        age_frac[linear_seg] * (nx_vals[linear_seg] - curr_vals[linear_seg])
    }

    # Cubic interpolation (vectorized)
    if (any(cubic_seg)) {
      a0 <- -pre_vals/6 + curr_vals/2 - nx_vals/2 + nx2_vals/6
      a1 <- pre_vals/2 - curr_vals + nx_vals/2
      a2 <- -pre_vals/3 - curr_vals/2 + nx_vals - nx2_vals/6
      a3 <- curr_vals

      cutoff_vals[cubic_seg] <- a0[cubic_seg] * age_frac[cubic_seg] * age_frac2[cubic_seg] +
        a1[cubic_seg] * age_frac2[cubic_seg] +
        a2[cubic_seg] * age_frac[cubic_seg] +
        a3[cubic_seg]
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

  # Convert result based on return type
  if (return == "string") {
    return(result)
  } else if (return == "factor") {
    # Create ordered factor with proper labels
    normal_weight_label <- if (wtabbr) "Normal wt" else "Normal weight"
    factor_levels <- c("Grade 3 thinness", "Grade 2 thinness", "Grade 1 thinness",
                       normal_weight_label, "Overweight", "Obese")

    result_factor <- factor(result, levels = factor_levels, ordered = TRUE)
    return(result_factor)
  } else if (return == "labelled") {
    # Check if labelled package is available
    if (!requireNamespace("labelled", quietly = TRUE)) {
      stop("The 'labelled' package is required for return='labelled'. Install it with: install.packages('labelled')")
    }

    # Create numeric values (-3, -2, -1, 0, 1, 2)
    normal_weight_label <- if (wtabbr) "Normal wt" else "Normal weight"

    numeric_result <- rep(NA_integer_, length(result))
    numeric_result[result == "Grade 3 thinness"] <- -3L
    numeric_result[result == "Grade 2 thinness"] <- -2L
    numeric_result[result == "Grade 1 thinness"] <- -1L
    numeric_result[result == normal_weight_label] <- 0L
    numeric_result[result == "Overweight"] <- 1L
    numeric_result[result == "Obese"] <- 2L

    # Create labelled variable
    labels <- c("Grade 3 thinness" = -3, "Grade 2 thinness" = -2, "Grade 1 thinness" = -1,
                "Overweight" = 1, "Obese" = 2)
    # Add the normal weight label with value 0
    labels <- c(labels[1:3], setNames(0, normal_weight_label), labels[4:5])

    result_labelled <- labelled::labelled(numeric_result, labels = labels)
    return(result_labelled)
  }

  return(result)
}

# Optimized helper function to load IOTF data
load_iotf_data <- function(file_path = "iotf.csv") {
  if (!file.exists(file_path)) {
    stop(paste("IOTF reference file", file_path, "not found."))
  }

  # Read CSV efficiently
  iotf_data <- read.csv(file_path, stringsAsFactors = FALSE)

  # Standardize column names (handle different CSV export formats)
  expected_patterns <- c("IOTFsex", "IOTFage",
                         "IOTF16_pre", "IOTF16", "IOTF16_nx", "IOTF16_nx2",
                         "IOTF17_pre", "IOTF17", "IOTF17_nx", "IOTF17_nx2",
                         "IOTF18_5_pre", "IOTF18_5", "IOTF18_5_nx", "IOTF18_5_nx2",
                         "IOTF25_pre", "IOTF25", "IOTF25_nx", "IOTF25_nx2",
                         "IOTF30_pre", "IOTF30", "IOTF30_nx", "IOTF30_nx2")

  # Rename columns to standard format if needed
  col_names <- names(iotf_data)
  for (pattern in expected_patterns) {
    # Find columns that contain the pattern
    matching_cols <- grep(pattern, col_names, value = TRUE)
    if (length(matching_cols) == 1) {
      standard_name <- paste0("X__", gsub("^.*__", "", matching_cols))
      names(iotf_data)[names(iotf_data) == matching_cols] <- standard_name
    }
  }

  # Convert sex and age to appropriate types
  if ("X__IOTFsex" %in% names(iotf_data)) {
    iotf_data$X__IOTFsex <- as.integer(iotf_data$X__IOTFsex)
  }
  if ("X__IOTFage" %in% names(iotf_data)) {
    iotf_data$X__IOTFage <- as.numeric(iotf_data$X__IOTFage)
  }

  return(iotf_data)
}

# Performance testing function
benchmark_zbmicat <- function(n = 10000) {
  # Generate test data
  set.seed(123)
  test_bmi <- rnorm(n, mean = 18, sd = 3)
  test_age <- runif(n, min = 2, max = 18)
  test_gender <- sample(c(1, 2), n, replace = TRUE)

  cat("Testing with", n, "observations...\n")

  # Time the function
  start_time <- Sys.time()
  results <- zbmicat_stata(test_bmi, test_age, test_gender)
  end_time <- Sys.time()

  elapsed <- as.numeric(end_time - start_time, units = "secs")
  cat("Time elapsed:", round(elapsed, 3), "seconds\n")
  cat("Rate:", round(n / elapsed), "observations per second\n")

  # Show result distribution
  cat("\nResult distribution:\n")
  print(table(results, useNA = "ifany"))

  return(results)
}

# Example usage:
#
# # String output (default)
# bmi$zbmicat_string <- zbmicat_stata(bmi = bmi$BMI,
#                                    age = bmi$AgeYrs,
#                                    gender = bmi$Sex,
#                                    return = "string")
#
# # Ordered factor output
# bmi$zbmicat_factor <- zbmicat_stata(bmi = bmi$BMI,
#                                    age = bmi$AgeYrs,
#                                    gender = bmi$Sex,
#                                    return = "factor")
# levels(bmi$zbmicat_factor)  # Shows the ordered levels
#
# # Labelled output (requires 'labelled' package)
# # install.packages("labelled")
# bmi$zbmicat_labelled <- zbmicat_stata(bmi = bmi$BMI,
#                                      age = bmi$AgeYrs,
#                                      gender = bmi$Sex,
#                                      return = "labelled")
# labelled::val_labels(bmi$zbmicat_labelled)  # Shows value-label mapping
#
# # Combined options
# bmi$zbmicat_custom <- zbmicat_stata(bmi = bmi$BMI,
#                                    age = bmi$AgeYrs,
#                                    gender = bmi$Sex,
#                                    wtabbr = TRUE,      # Abbreviated labels
#                                    return = "factor")  # As ordered factor
#
# # Test performance:
# benchmark_zbmicat(10000)  # Test with 10k observations
#
# # Clear cache if needed (e.g., if you update iotf.csv):
# rm(.iotf_data_cache, envir = .GlobalEnv)
