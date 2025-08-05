### THIS IS WORKING ALBEIT WITH UNDERCLASSIFICATION!

# IOTF BMI Classification Function for R
# Replicates the functionality of Stata's zbmicat command
# Author: Based on Tim Cole's original work
# Requires: sitar package

library(sitar)

# Main classification function (vectorized for speed)
classify_bmi_iotf <- function(bmi, age, sex, age_unit = "years") {
  
  # Input validation
  if(length(bmi) != length(age) || length(bmi) != length(sex)) {
    stop("bmi, age, and sex must have the same length")
  }
  
  if(!age_unit %in% c("years", "months", "weeks", "days")) {
    stop("age_unit must be one of: 'years', 'months', 'weeks', 'days'")
  }
  
  # Convert age to years if necessary
  age_years <- switch(age_unit,
                      "years" = age,
                      "months" = age / 12,
                      "weeks" = age / (365.25/7),
                      "days" = age / 365.25)
  
  # Initialize result vector
  n <- length(bmi)
  categories <- rep(NA_integer_, n)
  
  # Create validity mask
  valid_mask <- !is.na(bmi) & !is.na(age_years) & !is.na(sex) & 
    age_years >= 2 & age_years <= 18 & bmi > 0
  
  if(!any(valid_mask)) {
    # No valid cases, return early
    categories_factor <- factor(categories,
                                levels = c(-3, -2, -1, 0, 1, 2),
                                labels = c("Grade 3 thinness", "Grade 2 thinness", 
                                           "Grade 1 thinness", "Normal weight", 
                                           "Overweight", "Obese"))
    cat("0 cases could not be classified\n")
    cat(sprintf("%d cases could not be classified\n", n))
    return(categories_factor)
  }
  
  # Extract valid data
  valid_bmi <- bmi[valid_mask]
  valid_age <- age_years[valid_mask]
  valid_sex <- sex[valid_mask]
  
  # Standardize sex coding
  sex_char <- character(length(valid_sex))
  if(is.numeric(valid_sex)) {
    sex_char[valid_sex == 1] <- "boys"
    sex_char[valid_sex == 2] <- "girls"
  } else {
    sex_lower <- tolower(as.character(valid_sex))
    sex_char[sex_lower %in% c("male", "m", "boy", "boys")] <- "boys"
    sex_char[sex_lower %in% c("female", "f", "girl", "girls")] <- "girls"
  }
  
  # Remove cases with invalid sex coding
  sex_valid_mask <- sex_char %in% c("boys", "girls")
  if(!any(sex_valid_mask)) {
    # No valid sex codes
    categories_factor <- factor(categories,
                                levels = c(-3, -2, -1, 0, 1, 2),
                                labels = c("Grade 3 thinness", "Grade 2 thinness", 
                                           "Grade 1 thinness", "Normal weight", 
                                           "Overweight", "Obese"))
    cat("0 cases classified successfully\n")
    cat(sprintf("%d cases could not be classified\n", n))
    return(categories_factor)
  }
  
  # Further filter for valid sex
  valid_bmi <- valid_bmi[sex_valid_mask]
  valid_age <- valid_age[sex_valid_mask]
  sex_char <- sex_char[sex_valid_mask]
  
  # Define IOTF cutoff values (BMI values at age 18)
  cutoffs <- c(16, 17, 18.5, 25, 30)
  
  # Pre-calculate z-scores for cutoffs at age 18 for both sexes
  z_scores_18 <- data.frame(
    cutoff = cutoffs,
    boys = sapply(cutoffs, function(x) LMS2z(x = 18, y = x, sex = "boys", measure = "bmi", ref = "iotf", toz = TRUE)),
    girls = sapply(cutoffs, function(x) LMS2z(x = 18, y = x, sex = "girls", measure = "bmi", ref = "iotf", toz = TRUE))
  )
  
  # Create data frame for vectorized processing
  df <- data.frame(
    bmi = valid_bmi,
    age = valid_age,
    sex = sex_char,
    stringsAsFactors = FALSE
  )
  
  # Split by sex for vectorized LMS calculations
  boys_idx <- df$sex == "boys"
  girls_idx <- df$sex == "girls"
  
  # Initialize cutoff matrices
  n_valid <- nrow(df)
  bmi_cutoffs <- matrix(NA, nrow = n_valid, ncol = 5)
  
  # Vectorized calculation for boys
  if(any(boys_idx)) {
    boys_ages <- df$age[boys_idx]
    for(j in 1:5) {
      tryCatch({
        bmi_cutoffs[boys_idx, j] <- LMS2z(x = boys_ages, y = z_scores_18$boys[j], 
                                          sex = "boys", measure = "bmi", ref = "iotf", toz = FALSE)
      }, error = function(e) {
        # Handle errors gracefully
      })
    }
  }
  
  # Vectorized calculation for girls  
  if(any(girls_idx)) {
    girls_ages <- df$age[girls_idx]
    for(j in 1:5) {
      tryCatch({
        bmi_cutoffs[girls_idx, j] <- LMS2z(x = girls_ages, y = z_scores_18$girls[j], 
                                           sex = "girls", measure = "bmi", ref = "iotf", toz = FALSE)
      }, error = function(e) {
        # Handle errors gracefully
      })
    }
  }
  
  # Vectorized classification
  valid_categories <- rep(NA_integer_, n_valid)
  
  # Check for complete cutoff data
  complete_cases <- complete.cases(bmi_cutoffs)
  
  if(any(complete_cases)) {
    bmi_vals <- df$bmi[complete_cases]
    cutoffs_complete <- bmi_cutoffs[complete_cases, , drop = FALSE]
    
    # Vectorized classification using nested ifelse
    temp_categories <- ifelse(bmi_vals < cutoffs_complete[,1], -3,
                              ifelse(bmi_vals < cutoffs_complete[,2], -2,
                                     ifelse(bmi_vals < cutoffs_complete[,3], -1,
                                            ifelse(bmi_vals < cutoffs_complete[,4], 0,
                                                   ifelse(bmi_vals < cutoffs_complete[,5], 1, 2)))))
    
    valid_categories[complete_cases] <- temp_categories
  }
  
  # Map back to original positions
  original_positions <- which(valid_mask)[sex_valid_mask]
  categories[original_positions] <- valid_categories
  
  # Create factor with proper labels
  categories_factor <- factor(categories,
                              levels = c(-3, -2, -1, 0, 1, 2),
                              labels = c("Grade 3 thinness", "Grade 2 thinness", 
                                         "Grade 1 thinness", "Normal weight", 
                                         "Overweight", "Obese"))
  
  # Print summary information
  valid_n <- sum(!is.na(categories))
  invalid_n <- sum(is.na(categories))
  
  if(valid_n > 0) {
    cat(sprintf("BMI categories generated for %d case%s\n", 
                valid_n, ifelse(valid_n == 1, "", "s")))
    cat(sprintf("Age was assumed to be in %s\n", age_unit))
  }
  
  if(invalid_n > 0) {
    cat(sprintf("%d case%s could not be classified\n", 
                invalid_n, ifelse(invalid_n == 1, "", "s")))
    cat("(Cases may be missing due to: age < 2 or > 18 years, missing values, or BMI ≤ 0)\n")
  }
  
  return(categories_factor)
}

# Convenience wrapper function that mimics the Stata syntax more closely
zbmicat <- function(data, bmi_var, age_var, sex_var, 
                    male_code = 1, female_code = 2, age_unit = "years") {
  
  # Extract variables from data frame
  bmi <- data[[bmi_var]]
  age <- data[[age_var]]
  sex <- data[[sex_var]]
  
  # Recode sex if it's not already in the expected format
  if(is.character(sex)) {
    # If sex is character, convert to numeric for consistency
    sex_numeric <- ifelse(tolower(sex) %in% c("male", "m", "boy", "boys"), 1,
                          ifelse(tolower(sex) %in% c("female", "f", "girl", "girls"), 2, NA))
    sex <- sex_numeric
  }
  
  # Call the main classification function
  classify_bmi_iotf(bmi = bmi, age = age, sex = sex, age_unit = age_unit)
}

# Example usage and testing
if(FALSE) {
  # Example 1: Basic usage
  # Create sample data
  sample_data <- data.frame(
    id = 1:10,
    age_years = c(5, 7, 9, 12, 15, 6, 8, 10, 14, 16),
    sex = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2),  # 1 = male, 2 = female
    weight = c(18, 25, 32, 45, 65, 20, 28, 35, 55, 60),
    height = c(1.1, 1.25, 1.35, 1.5, 1.7, 1.15, 1.3, 1.4, 1.65, 1.68)
  )
  
  # Calculate BMI
  sample_data$bmi <- sample_data$weight / (sample_data$height^2)
  
  # Classify using the convenience function
  sample_data$bmi_category <- zbmicat(sample_data, 
                                      bmi_var = "bmi",
                                      age_var = "age_years", 
                                      sex_var = "sex",
                                      male_code = 1,
                                      female_code = 2,
                                      age_unit = "years")
  
  print(sample_data)
  
  # Example 2: Direct function call
  bmi_cats <- classify_bmi_iotf(
    bmi = c(15.2, 18.5, 22.1, 26.8, 32.1),
    age = c(8, 10, 12, 14, 16),
    sex = c("boys", "girls", "boys", "girls", "boys"),
    age_unit = "years"
  )
  
  print(bmi_cats)
  
  # Example 3: With age in months
  bmi_cats_months <- classify_bmi_iotf(
    bmi = c(16.1, 19.2, 24.5),
    age = c(96, 120, 180),  # 8, 10, 15 years in months
    sex = c(1, 2, 1),
    age_unit = "months"
  )
  
  print(bmi_cats_months)
}