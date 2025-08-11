# zanthror: Stata `zanthro` Extension Ported to R - Anthropometric Analysis Tools <a href="https://github.com/w4rhol/zanthror"><img src="man/figures/hexsticker.png" align="right" height="150"/></a>

[![R-CMD-check](https://github.com/w4rhol/zanthror/workflows/R-CMD-check/badge.svg)](https://github.com/w4rhol/zanthror/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/zanthror)](https://CRAN.R-project.org/package=zanthror)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

**zanthror** provides tools for anthropometric analysis in R, replicating and extending the functionality of the widely-used Stata `zanthro` extension developed by Vidmar, Cole, and Pan (2013). The package offers both exact replications of established Stata functions and enhanced approaches for child growth assessment and BMI classification using international standards.

## Key Features

- **Z-Score Calculation**: Calculate anthropometric z-scores using multiple international growth references (WHO, CDC, UK-WHO, IOTF)
- **Multiple Growth Chart Support**: 
  - **US CDC 2000**: Length/height-for-age, weight-for-age, BMI-for-age, head circumference, weight-for-length/height
  - **WHO Standards**: Child Growth Standards (0-5y) + Reference 2007 (5-19y) for height, weight, BMI, head circumference, arm circumference, skinfolds
  - **UK 1990**: British Growth Reference including specialized measurements (sitting height, leg length, waist circumference, body fat)
  - **UK-WHO Composite**: Preterm and term birth references
- **IOTF BMI Classification**: Classify children's BMI using International Obesity Task Force cutoffs with two implementation approaches:
  - `zbmicat()`: Exact replication of Stata's zbmicat command using interpolation
  - `zbmicat_lms()`: Enhanced LMS-based method using the [sitar](https://github.com/statist7/sitar) package for greater accuracy
- **Flexible Input/Output Options**: 
  - Multiple age units (years, months, weeks, days)
  - Gestational age adjustment
  - Various output formats (string, factor, labelled, haven, numeric)
- **Comprehensive Test Dataset**: Simulated dataset with 500 participants aged 0-21 years
- **Tidyverse-friendly**: Works with `magrittr` pipes

## Installation

### From GitHub (Development Version)

```r
# Install from GitHub
if (!require(devtools)) install.packages("devtools")
devtools::install_github("w4rhol/zanthror")
```

### Dependencies

The package requires different dependencies depending on which functions you use:

- **Core z-score functions (`zanthro`)**: Base R only
- **BMI classification with interpolation (`zbmicat`)**: Base R + `stats`
- **LMS-based BMI classification (`zbmicat_lms`)**: `sitar` package
- **Labelled outputs**: `labelled` and/or `haven` packages

## Quick Start

```r
library(zanthror)

# Load test data
data(zanthror_testdata)

# Calculate BMI z-scores using WHO charts
zanthror_testdata$bmi_zscore <- zanthro(
  measure = zanthror_testdata$bmi,
  xvar = zanthror_testdata$age_years,
  chart = "ba",  # BMI-for-age
  version = "WHO",
  gender = zanthror_testdata$gender_label,
  male_code = "Male",
  female_code = "Female"
)

# BMI classification using IOTF cutoffs (Stata replication)
zanthror_testdata$bmi_category <- zbmicat(
  bmi = zanthror_testdata$bmi,
  age = zanthror_testdata$age_years,
  gender = zanthror_testdata$gender,
  return = "factor"
)

# BMI classification using enhanced LMS method
zanthror_testdata$bmi_category_lms <- zbmicat_lms(
  bmi = zanthror_testdata$bmi,
  age = zanthror_testdata$age_years,
  gender = zanthror_testdata$gender,
  return = "factor"
)

# Compare approaches
table(zanthror_testdata$bmi_category, 
      zanthror_testdata$bmi_category_lms, 
      useNA = "ifany")

# Calculate height z-scores with gestational age adjustment
height_z <- zanthro(
  measure = zanthror_testdata$height_cm,
  xvar = zanthror_testdata$age_years,
  chart = "ha",  # Height-for-age
  version = "WHO",
  gender = zanthror_testdata$gender_label,
  male_code = "Male",
  female_code = "Female",
  gestage = rep(40, nrow(zanthror_testdata))  # Assuming term births
)

# View summary statistics
summary(zanthror_testdata[c("bmi_zscore", "bmi_category")])
```

## Main Functions

### Z-Score Calculation: `zanthro()`

The core function replicating Stata's `zanthro()` command for calculating anthropometric z-scores using the LMS method.

```r
# Basic z-score calculation
z_scores <- zanthro(
  measure = height_vector,     # Anthropometric measurement
  xvar = age_vector,          # Age (or length/height for wt-for-ht charts)
  chart = "ha",               # Chart code (see below)
  version = "WHO",            # Reference population
  gender = gender_vector,
  male_code = "Male",
  female_code = "Female"
)
```

#### Supported Chart Codes and Versions

| Chart Code | Description | Available Versions |
|------------|-------------|-------------------|
| `"ha"` | Height-for-age | US, UK, WHO, UKWHOpreterm, UKWHOterm |
| `"wa"` | Weight-for-age | US, UK, WHO, UKWHOpreterm, UKWHOterm |
| `"ba"` | BMI-for-age | US, UK, WHO, UKWHOpreterm, UKWHOterm |
| `"hca"` | Head circumference-for-age | US, UK, WHO, UKWHOpreterm, UKWHOterm |
| `"la"` | Length-for-age | US only |
| `"wl"` | Weight-for-length | US, WHO |
| `"wh"` | Weight-for-height | US, WHO |
| `"sha"` | Sitting height-for-age | UK only |
| `"lla"` | Leg length-for-age | UK only |
| `"wsa"` | Waist-for-age | UK only |
| `"bfa"` | Body fat-for-age | UK only |
| `"aca"` | Arm circumference-for-age | WHO only |
| `"ssa"` | Subscapular skinfold-for-age | WHO only |
| `"tsa"` | Triceps skinfold-for-age | WHO only |

### BMI Classification: `zbmicat()` and `zbmicat_lms()`

#### `zbmicat()` - Stata Replication
Exact replication of Stata's `zbmicat` command using interpolation methods and pre-calculated IOTF cutoffs.

```r
# Six-category BMI classification
categories <- zbmicat(
  bmi = bmi_vector,
  age = age_vector,
  gender = gender_vector,
  return = "string"  # "string", "factor", "labelled", "haven", "numeric"
)

# Categories returned:
# - Grade 3 thinness (BMI < 16 equivalent at age 18)
# - Grade 2 thinness (BMI 16-17 equivalent)  
# - Grade 1 thinness (BMI 17-18.5 equivalent)
# - Normal weight (BMI 18.5-25 equivalent)
# - Overweight (BMI 25-30 equivalent)
# - Obese (BMI ≥30 equivalent)
```

#### `zbmicat_lms()` - Enhanced LMS Method
Uses the `sitar` package for more accurate LMS-based cutoff calculation.

```r
categories_lms <- zbmicat_lms(
  bmi = bmi_vector,
  age = age_vector,
  gender = gender_vector,
  return = "factor"
)
```

## Advanced Usage

### Custom Gender Codes and Age Units

```r
# Character gender codes
result <- zbmicat(
  bmi = bmi_vector,
  age = age_vector,
  gender = c("M", "F", "M"),
  male_code = "M",
  female_code = "F"
)

# Age in months
age_months <- age_years * 12
z_scores <- zanthro(
  measure = height_vector,
  xvar = age_months,
  chart = "ha",
  version = "WHO",
  gender = gender_vector,
  male_code = 1,
  female_code = 2,
  ageunit = "month"
)
```

### Gestational Age Adjustment

```r
# Adjust age for preterm births
z_scores_adjusted <- zanthro(
  measure = weight_vector,
  xvar = age_weeks,
  chart = "wa",
  version = "UKWHOpreterm",
  gender = gender_vector,
  male_code = "Male",
  female_code = "Female",
  ageunit = "week",
  gestage = gestational_age_weeks  # Vector of gestational ages
)
```

### Multiple Output Formats

```r
# String output (human-readable, default)
string_result <- zbmicat(bmi, age, gender)

# Factor output (for statistical modeling)
factor_result <- zbmicat(bmi, age, gender, return = "factor")
levels(factor_result)  # Shows proper ordering

# Numeric output (-3 to 2, for data processing)
numeric_result <- zbmicat(bmi, age, gender, return = "numeric")

# Labelled output (for `labelled` compatibility)
labelled_result <- zbmicat(bmi, age, gender, return = "labelled")

# Haven-labelled output (for Stata/SPSS/SAS compatibility)
haven_result <- zbmicat(bmi, age, gender, return = "haven")
```

### Working with the Test Dataset

```r
# Explore the test dataset
data(zanthror_testdata)
str(zanthror_testdata)

# Dataset includes:
# - 500 participants aged 0-21 years
# - Anthropometric measurements
# - Missing data patterns

# Example analysis by country
library(dplyr)
results_by_country <- zanthror_testdata %>%
  mutate(
    bmi_z = zanthro(bmi, age_years, "ba", "WHO", gender_label, 
                   male_code = "Male", female_code = "Female"),
    bmi_cat = zbmicat(bmi, age_years, gender, return = "factor")
  ) %>%
  group_by(country) %>%
  summarise(
    n = n(),
    mean_bmi_z = mean(bmi_z, na.rm = TRUE),
    pct_overweight_obese = mean(bmi_cat %in% c("Overweight", "Obese"), na.rm = TRUE) * 100
  )
```

## Missing Data Handling

Functions return `NA` for cases with:
- Missing BMI, age, or gender values
- BMI ≤ 0 (invalid measurements)
- Age outside valid range for selected chart/version
- Invalid gender codes
- Z-scores with absolute values ≥5 (potential data entry errors, unless `nocutoff=TRUE`)

## Chart-Specific Age Ranges

| Chart/Version | Age Range | Notes |
|---------------|-----------|-------|
| **US CDC 2000** | | |
| Length-for-age | 0-35.5 months | |
| Height-for-age | 2-20 years | |
| Weight-for-age | 0-20 years | |
| BMI-for-age | 2-20 years | |
| Head circumference | 0-36 months | |
| Weight-for-length | 45-103.5 cm | |
| Weight-for-height | 77-121.5 cm | |
| **UK 1990** | | |
| Height/weight/BMI | 0-23 years | |
| Head circumference | 0-18y (M), 0-17y (F) | |
| Sitting height/leg length | 0-23 years | UK participants only |
| Waist circumference | 3-17 years | UK participants only |
| Body fat | 4.75-19.83 years | UK participants only |
| **WHO** | | |
| Height/BMI-for-age | 0-19 years | |
| Weight-for-age | 0-10 years | |
| Head circumference | 0-5 years | |
| ARM/skinfolds | 0.25-5 years | |

## Performance Notes

- All functions are vectorized for efficient processing of large datasets
- The `zbmicat_lms()` function may be slower than `zbmicat()` due to LMS calculations but provides greater accuracy
- Reference data is included in the package for offline use

## References

**Original Stata Extension:**
- Vidmar, S.I., Cole, T.J., Pan, H. (2013). Standardizing anthropometric measures in children and adolescents with functions for egen: Update. *Stata Journal*, 13(2), 366-378.

**IOTF References:**
- Cole, T. J., & Lobstein, T. (2012). Extended international (IOTF) body mass index cut-offs for thinness, overweight and obesity. *Pediatric Obesity*, 7(4), 284-294.
- Cole, T. J., Bellizzi, M. C., Flegal, K. M., & Dietz, W. H. (2000). Establishing a standard definition for child overweight and obesity worldwide: international survey. *BMJ*, 320(7244), 1240-1243.

**LMS Method:**
- Cole, T. J., & Green, P. J. (1992). Smoothing reference centile curves: the LMS method and penalized likelihood. *Statistics in Medicine*, 11(10), 1305-1319.

**Growth References:**
- WHO Child Growth Standards (2006) and WHO Reference 2007
- US CDC 2000 Growth Reference
- UK 1990 Growth Reference (Freeman et al., 1995)

## Issues and Bug Reports

Please report issues on our [GitHub Issues page](https://github.com/w4rhol/zanthror/issues) with:
- Reproducible example
- Expected vs actual behaviour
- System information (R version, OS, package versions)

## Acknowledgments

- **Suzanna I. Vidmar, Tim J. Cole & Huiqi Pan** for the original Stata zanthro implementation
- **Tim J. Cole** for the [sitar](https://github.com/statist7/sitar) package and pioneering work on the LMS method
- **International Obesity Task Force (IOTF)** for developing internationally standardized BMI cutoffs

## Citation

If you use zanthror in your research, please cite:

```r
citation("zanthror")
```

---

**GitHub Repository**: https://github.com/w4rhol/zanthror
