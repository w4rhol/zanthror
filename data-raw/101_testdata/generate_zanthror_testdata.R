# Create realistic test dataset for zanthror package
# This script generates a plausible dataset with anthropometric measurements

set.seed(42)  # For reproducibility

# Generate test data
n <- 500  # Default to 500 observations

# Create realistic age distribution (more children at younger ages)
# Scale the proportions based on n - extended to ages 0-21
infants <- round(n * 0.08)           # 8% ages 0-2 (infants/toddlers)
early_childhood <- round(n * 0.25)   # 25% ages 2-6
middle_childhood <- round(n * 0.22)  # 22% ages 6-10
early_adolescence <- round(n * 0.20) # 20% ages 10-14
late_adolescence <- round(n * 0.15)  # 15% ages 14-18
young_adults <- n - (infants + early_childhood + middle_childhood + early_adolescence + late_adolescence)  # 10% ages 18-21

age_years <- c(
  runif(infants, min = 0, max = 2),
  runif(early_childhood, min = 2, max = 6),
  runif(middle_childhood, min = 6, max = 10),
  runif(early_adolescence, min = 10, max = 14),
  runif(late_adolescence, min = 14, max = 18),
  runif(young_adults, min = 18, max = 21)
)

# Generate gender (slightly more boys in sample, as often seen in growth studies)
gender <- sample(c(1, 2), n, replace = TRUE, prob = c(0.52, 0.48))
gender_labels <- ifelse(gender == 1, "Male", "Female")

# Generate realistic heights based on WHO growth curves approximations
# Heights vary by age and gender, extended to ages 0-21
height_cm <- numeric(n)
for (i in 1:n) {
  if (gender[i] == 1) {  # Boys
    # Approximate height-for-age from birth to adulthood
    if (age_years[i] < 2) {
      # Infant growth (birth ~50cm, rapid growth)
      mean_height <- 50 + age_years[i] * 25
    } else if (age_years[i] <= 18) {
      # Childhood/adolescent growth
      mean_height <- 78 + (age_years[i] - 2) * 6.2 + (age_years[i] - 2)^1.5 * 0.8
    } else {
      # Young adult (plateau around 175-180cm)
      mean_height <- 175 + (age_years[i] - 18) * 1.5
    }
    height_cm[i] <- rnorm(1, mean = mean_height, sd = 5)
  } else {  # Girls
    if (age_years[i] < 2) {
      # Infant growth (slightly smaller than boys)
      mean_height <- 49 + age_years[i] * 24
    } else if (age_years[i] <= 18) {
      # Childhood/adolescent growth
      mean_height <- 77 + (age_years[i] - 2) * 5.9 + (age_years[i] - 2)^1.5 * 0.7
    } else {
      # Young adult (plateau around 162-167cm)
      mean_height <- 162 + (age_years[i] - 18) * 1.0
    }
    height_cm[i] <- rnorm(1, mean = mean_height, sd = 4.8)
  }
}

# Generate realistic weights based on height and introducing BMI variation
# This creates a mix of weight categories, extended for all ages
weight_kg <- numeric(n)
for (i in 1:n) {
  if (age_years[i] < 2) {
    # Infant weights (birth ~3.5kg, rapid growth)
    base_weight <- 3.5 + age_years[i] * 8 + rnorm(1, 0, 1.0)
  } else {
    # Base BMI varies by age (younger children have lower BMI typically)
    if (age_years[i] <= 18) {
      base_bmi <- 15.5 + age_years[i] * 0.3 + rnorm(1, 0, 1.5)
    } else {
      # Young adults - more adult-like BMI distribution
      base_bmi <- 22 + rnorm(1, 0, 2.5)
    }

    # Add some systematic variation to create different weight categories
    bmi_modifier <- sample(c(-2.5, -1.5, -0.5, 0, 0.5, 1.5, 3.0), 1,
                           prob = c(0.03, 0.08, 0.15, 0.48, 0.15, 0.08, 0.03))

    target_bmi <- base_bmi + bmi_modifier
    base_weight <- target_bmi * (height_cm[i]/100)^2
  }
  weight_kg[i] <- base_weight
}

# Create participant IDs
participant_id <- sprintf("ID%03d", 1:n)

# Add some realistic demographic variables
# Country/region (for international dataset feel)
country <- sample(c("UK", "USA", "Canada", "Australia", "Netherlands", "Brazil"),
                  n, replace = TRUE, prob = c(0.25, 0.25, 0.15, 0.15, 0.15, 0.05))

# Study site (realistic for multi-center study)
study_site <- sample(c("Site_A", "Site_B", "Site_C", "Site_D"),
                     n, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15))

# Measurement date (spread over 2 years)
measurement_date <- sample(seq(as.Date("2022-01-01"), as.Date("2023-12-31"), by = "day"),
                           n, replace = TRUE)

# Calculate actual BMI
bmi_calculated <- weight_kg / (height_cm/100)^2

# Introduce missing data pattern: if BMI is NA, then either height or weight should be NA
missing_bmi_indices <- sample(1:n, size = round(n * 0.02))
missing_type <- sample(c("height", "weight"), length(missing_bmi_indices), replace = TRUE)

for (j in seq_along(missing_bmi_indices)) {
  idx <- missing_bmi_indices[j]
  if (missing_type[j] == "height") {
    height_cm[idx] <- NA
  } else {
    weight_kg[idx] <- NA
  }
}

# Recalculate BMI after introducing missing values
bmi_calculated <- weight_kg / (height_cm/100)^2

# Generate additional anthropometric measurements
# Head circumference (realistic values by age and gender, extended range)
head_circumference_cm <- numeric(n)
for (i in 1:n) {
  if (gender[i] == 1) {  # Boys
    if (age_years[i] < 2) {
      # Infant head growth (birth ~35cm, rapid growth)
      base_hc <- 35 + age_years[i] * 11
    } else {
      # Head circumference grows rapidly early, plateaus around age 16
      base_hc <- 46 + (age_years[i] - 2) * 1.2 + (age_years[i] - 2)^0.5 * 2
      if (age_years[i] > 16) base_hc <- base_hc - (age_years[i] - 16) * 0.1  # Slight plateau
    }
    head_circumference_cm[i] <- rnorm(1, mean = base_hc, sd = 1.2)
  } else {  # Girls
    if (age_years[i] < 2) {
      # Infant head growth (slightly smaller than boys)
      base_hc <- 34.5 + age_years[i] * 10.5
    } else {
      base_hc <- 45.5 + (age_years[i] - 2) * 1.1 + (age_years[i] - 2)^0.5 * 1.8
      if (age_years[i] > 16) base_hc <- base_hc - (age_years[i] - 16) * 0.1
    }
    head_circumference_cm[i] <- rnorm(1, mean = base_hc, sd = 1.1)
  }
}

# Sitting height - only for UK participants, others get NA
sitting_height_cm <- rep(NA_real_, n)
uk_indices <- which(country == "UK")
if (length(uk_indices) > 0) {
  for (i in uk_indices) {
    # Approximately 50-55% of total height, varies by age
    sitting_height_proportion <- 0.52 + (18 - pmin(age_years[i], 18)) * 0.002
    sitting_height_cm[i] <- height_cm[i] * sitting_height_proportion + rnorm(1, 0, 1.5)
  }
}

# Leg length - only for UK participants (calculated from sitting height)
leg_length_cm <- rep(NA_real_, n)
leg_length_cm[uk_indices] <- height_cm[uk_indices] - sitting_height_cm[uk_indices] + rnorm(length(uk_indices), 0, 1.0)

# Waist circumference - only for UK participants
waist_circumference_cm <- rep(NA_real_, n)
if (length(uk_indices) > 0) {
  for (i in uk_indices) {
    if (age_years[i] < 2) {
      # Infant waist (smaller, proportional)
      base_waist <- 35 + age_years[i] * 10
    } else {
      # Base waist size increases with age and BMI
      base_waist <- 45 + (age_years[i] - 2) * 2.8 + (bmi_calculated[i] - 16) * 1.8
    }
    waist_circumference_cm[i] <- rnorm(1, mean = base_waist, sd = 3.2)
  }
}

# Body fat percentage - only for UK participants
body_fat_percent <- rep(NA_real_, n)
if (length(uk_indices) > 0) {
  for (i in uk_indices) {
    if (age_years[i] < 2) {
      # Infants have higher body fat percentage
      base_bf <- 20 + rnorm(1, 0, 3)
    } else if (gender[i] == 1) {  # Boys
      # Boys typically have lower body fat, increases with BMI
      base_bf <- 8 + (bmi_calculated[i] - 16) * 1.2 + (age_years[i] - 10) * 0.3
    } else {  # Girls
      # Girls typically have higher body fat, especially after puberty
      base_bf <- 12 + (bmi_calculated[i] - 16) * 1.4 + pmax(0, (age_years[i] - 12)) * 0.8
    }
    body_fat_percent[i] <- pmax(5, rnorm(1, mean = base_bf, sd = 3.5))  # Minimum 5%
  }
}

# Arm circumference (mid-upper arm, related to age and body composition)
arm_circumference_cm <- numeric(n)
for (i in 1:n) {
  if (age_years[i] < 2) {
    # Infant arm circumference
    base_arm <- 8 + age_years[i] * 6
  } else {
    # Base arm circumference increases with age and BMI
    base_arm <- 14 + (age_years[i] - 2) * 0.8 + (bmi_calculated[i] - 16) * 0.7
  }
  arm_circumference_cm[i] <- rnorm(1, mean = base_arm, sd = 1.5)
}

# Subscapular skinfold (back, related to body fat)
subscapular_skinfold_mm <- numeric(n)
for (i in 1:n) {
  if (age_years[i] < 2) {
    # Infants have different skinfold patterns
    base_subscap <- 8 + rnorm(1, 0, 2)
  } else {
    # Skinfolds are highly related to body fat percentage
    # Use overall body fat estimate for non-UK participants
    estimated_bf <- if (country[i] == "UK" && !is.na(body_fat_percent[i])) {
      body_fat_percent[i]
    } else {
      # Estimate body fat from BMI for non-UK participants
      if (gender[i] == 1) {
        8 + (bmi_calculated[i] - 16) * 1.2
      } else {
        12 + (bmi_calculated[i] - 16) * 1.4
      }
    }
    base_subscap <- 6 + estimated_bf * 0.4 + rnorm(1, 0, 1.5)
  }
  subscapular_skinfold_mm[i] <- pmax(3, base_subscap)  # Minimum 3mm
}

# Triceps skinfold (arm, also related to body fat)
triceps_skinfold_mm <- numeric(n)
for (i in 1:n) {
  if (age_years[i] < 2) {
    # Infants have different skinfold patterns
    base_triceps <- 9 + rnorm(1, 0, 2.5)
  } else {
    # Triceps usually slightly higher than subscapular
    # Use overall body fat estimate for non-UK participants
    estimated_bf <- if (country[i] == "UK" && !is.na(body_fat_percent[i])) {
      body_fat_percent[i]
    } else {
      # Estimate body fat from BMI for non-UK participants
      if (gender[i] == 1) {
        8 + (bmi_calculated[i] - 16) * 1.2
      } else {
        12 + (bmi_calculated[i] - 16) * 1.4
      }
    }
    base_triceps <- 7 + estimated_bf * 0.5 + rnorm(1, 0, 2.0)
  }
  triceps_skinfold_mm[i] <- pmax(4, base_triceps)  # Minimum 4mm
}

# Round measurements to realistic precision
age_years <- round(age_years, 2)
height_cm <- round(height_cm, 1)
weight_kg <- round(weight_kg, 2)
bmi_calculated <- round(bmi_calculated, 2)
head_circumference_cm <- round(head_circumference_cm, 1)
sitting_height_cm <- round(sitting_height_cm, 1)
leg_length_cm <- round(leg_length_cm, 1)
waist_circumference_cm <- round(waist_circumference_cm, 1)
body_fat_percent <- round(body_fat_percent, 1)
arm_circumference_cm <- round(arm_circumference_cm, 1)
subscapular_skinfold_mm <- round(subscapular_skinfold_mm, 1)
triceps_skinfold_mm <- round(triceps_skinfold_mm, 1)

# Create participant IDs
participant_id <- sprintf("ID%03d", 1:n)

# Create the final dataset
zanthror_testdata <- data.frame(
  participant_id = participant_id,
  age_years = age_years,
  gender = gender,
  gender_label = gender_labels,
  height_cm = height_cm,
  weight_kg = weight_kg,
  bmi = bmi_calculated,
  head_circumference_cm = head_circumference_cm,
  sitting_height_cm = sitting_height_cm,
  leg_length_cm = leg_length_cm,
  waist_circumference_cm = waist_circumference_cm,
  body_fat_percent = body_fat_percent,
  arm_circumference_cm = arm_circumference_cm,
  subscapular_skinfold_mm = subscapular_skinfold_mm,
  triceps_skinfold_mm = triceps_skinfold_mm,
  country = country,
  study_site = study_site,
  measurement_date = measurement_date,
  stringsAsFactors = FALSE
)

# Add some additional missing values for other measurements (realistic patterns)
zanthror_testdata$head_circumference_cm[sample(1:n, 2)] <- NA
zanthror_testdata$arm_circumference_cm[sample(1:n, 3)] <- NA
zanthror_testdata$subscapular_skinfold_mm[sample(1:n, 4)] <- NA  # Skinfolds often harder to measure
zanthror_testdata$triceps_skinfold_mm[sample(1:n, 4)] <- NA

# Sort by age for easier viewing
zanthror_testdata <- zanthror_testdata[order(zanthror_testdata$age_years), ]
rownames(zanthror_testdata) <- NULL

# Display summary of the dataset
cat("=== ZANTHROR Test Dataset Summary ===\n\n")
cat("Number of observations:", nrow(zanthror_testdata), "\n")
cat("Age range:", round(min(zanthror_testdata$age_years, na.rm = TRUE), 2),
    "to", round(max(zanthror_testdata$age_years, na.rm = TRUE), 2), "years\n")
cat("Gender distribution:\n")
print(table(zanthror_testdata$gender_label))
cat("\nCountry distribution:\n")
print(table(zanthror_testdata$country))
cat("\nBMI summary:\n")
print(summary(zanthror_testdata$bmi))
cat("\nBody fat percentage summary (UK only):\n")
print(summary(zanthror_testdata$body_fat_percent))
cat("\nWaist circumference summary (UK only):\n")
print(summary(zanthror_testdata$waist_circumference_cm))
cat("\nSitting height availability by country:\n")
print(table(zanthror_testdata$country, is.na(zanthror_testdata$sitting_height_cm), dnn = c("Country", "Sitting Height Missing")))
cat("\nFirst 10 rows:\n")
print(head(zanthror_testdata, 10))

# For package development, save as .rda:
usethis::use_data(zanthror_testdata, overwrite = TRUE)

# For export to Stata for testing locally
write.csv(x = zanthror_testdata, file = "data-raw/101_testdata/zanthror_testdata.csv", row.names = FALSE, na = "")
# For export to Stata for testing on Citrix machine
write.csv(x = zanthror_testdata,
          file = "Q:/PHD/Health Improvement Branch/Epidemiology/Apps/R/warren holroyd/zanthror/data-raw/101_testdata/zanthror_testdata.csv",
          row.names = FALSE, na = "")

# Test the zbmicat function on this dataset
# (Assuming your function is loaded)
# zbmicat_test_data$bmi_category <- zbmicat_stata(
#   bmi = zbmicat_test_data$bmi,
#   age = zbmicat_test_data$age_years,
#   gender = zbmicat_test_data$gender,
#   male_code = 1,
#   female_code = 2,
#   age_unit = "year",
#   wtabbr = FALSE,
#   return = "string"
# )
#
# # Show distribution of BMI categories
# cat("\nBMI category distribution:\n")
# print(table(zbmicat_test_data$bmi_category, useNA = "ifany"))
