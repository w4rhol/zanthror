# zanthror: Stata `zanthro` Extension Ported to R - Anthropometric Analysis Tools <a href="https://github.com/w4rhol/zanthror"><img src="man/figures/hexsticker.png" align="right" height="150"/></a>

[![R-CMD-check](https://github.com/w4rhol/zanthror/workflows/R-CMD-check/badge.svg)](https://github.com/w4rhol/zanthror/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/zanthror)](https://CRAN.R-project.org/package=zanthror)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

**zanthror** provides comprehensive tools for anthropometric analysis in R, with a focus on child growth assessment and BMI classification using international standards. The package offers both exact replications of established Stata functions and enhanced LMS-based approaches for greater accuracy.

## Key Features

- **IOTF BMI Classification**: Classify children's BMI using International Obesity Task Force cutoffs
- **Multiple Implementation Approaches**: 
  - `zbmicat()`: Exact replication of Stata's zbmicat command with interpolation
  - `zbmicat_lms()`: Enhanced LMS-based method using the [sitar](https://github.com/statist7/sitar) package for greater accuracy
- **Flexible Output Formats**: String, factor, labelled, haven, or numeric outputs
- **Appropriate Age Handling**: Ages 2-18 years with multiple time units (years, months, weeks, days)
- **Compatible with tidyverse**: Compatible with `magrittr` pipes
- **Built-in test dataset**: Simulated dataset to assist with examples and user testing

## Installation

### From GitHub (Development Version)

```r
# Install from GitHub
if (!require(devtools)) install.packages("devtools")
devtools::install_github("w4rhol/zanthror")
```

### Dependencies

The package requires different dependencies depending on which functions you use:

- **Core functions**: Base R (no additional dependencies)
- **LMS-based functions**: `sitar` package
- **Labelled outputs**: `labelled` and/or `haven` packages

## Quick Start

```r
library(zanthror)

# Load test data
data(zanthror_testdata)

# BMI classification using pre-calculated IOTF cutoffs that were
# included with the Stata zanthro extension
zanthror_testdata$zbmicats <- zbmicat(
  bmi = zanthror_testdata$bmi,
  age = zanthror_testdata$age_years,
  gender = zanthror_testdata$gender,
  return = "factor"
  )
  
# BMI classification using sitar LMS method
zanthror_testdata$zbmicats_lms <- zbmicat(
  bmi = zanthror_testdata$bmi,
  age = zanthror_testdata$age_years,
  gender = zanthror_testdata$gender,
  return = "factor"
  )
  
# View crosstabulation of approaches
table(zanthror_testdata$zbmicats, 
      zanthror_testdata$zbmicats_lms, 
      useNA = "ifany")

# identify outliers
zanthror_testdata$zs = zanthro(
  measure = zanthror_testdata$height_cm,
  xvar = zanthror_testdata$age_years,
  chart = "la", 
  version = "US",
  gender = zanthror_testdata$gender_label,
  male_code = "Male",
  female_code = "Female"
  )

```

## Main Functions

### BMI Classification

#### `zbmicat()`
Exact replication of Stata's `zbmicat` command using interpolation methods.

```r
# String output (default)
categories <- zbmicat(
  bmi = bmi_vector,
  age = age_vector,
  gender = gender_vector
)

# Factor output for statistical analysis
bmi_factor <- zbmicat(
  bmi = bmi_vector,
  age = age_vector,
  gender = gender_vector,
  return = "factor"
)

# Labelled output for data export
bmi_labelled <- zbmicat(
  bmi = bmi_vector,
  age = age_vector,
  gender = gender_vector,
  return = "labelled"
)
```

#### `zbmicat_lms()`
Enhanced LMS-based approach using the `sitar` package for more accurate cutoffs.  Same return options are available as `zbmicat`.

```r
categories_lms <- zbmicat_lms(
  bmi = bmi_vector,
  age = age_vector,
  gender = gender_vector,
  return = "factor"
)
```

## Advanced Usage

### Custom Gender Codes

```r
# Using character gender codes
result <- zbmicat(
  bmi = bmi_vector,
  age = age_vector,
  gender = c("Male", "Female", "Male"),
  male_code = "Male",
  female_code = "Female"
)
```

### Different Age Units

```r
# Age in months
age_months <- age_years * 12
result <- zbmicat(
  bmi = bmi_vector,
  age = age_months,
  gender = gender_vector,
  ageunit = "month"
)

# Age in days
age_days <- age_years * 365.25
result <- zbmicat(
  bmi = bmi_vector,
  age = age_days,
  gender = gender_vector,
  ageunit = "day"
)
```

### Multiple Output Formats

```r
# String output (human-readable)
string_result <- zbmicat(bmi, age, gender, return = "string")

# Factor output (for statistical modeling)
factor_result <- zbmicat(bmi, age, gender, return = "factor")

# Numeric output (for data processing)
numeric_result <- zbmicat(bmi, age, gender, return = "numeric")

# Labelled output (for data export)
labelled_result <- zbmicat(bmi, age, gender, return = "labelled")

# Haven-labelled output (for SPSS/SAS compatibility)
haven_result <- zbmicat(bmi, age, gender, return = "haven")
```

### Missing Data Handling

Functions return `NA` for cases with:
- Missing BMI, age, or gender values
- BMI ≤ 0
- Age < 2 or > 18 years
- Invalid gender codes

## Validation

The package includes comprehensive test data (`zanthror_testdata`) and has been validated against:
- Original Stata command outputs
- WHO growth reference standards
- Published IOTF cutoff tables

## References

**IOTF References:**
- Cole, T. J., & Lobstein, T. (2012). Extended international (IOTF) body mass index cut-offs for thinness, overweight and obesity. *Pediatric Obesity*, 7(4), 284-294.
- Cole, T. J., Bellizzi, M. C., Flegal, K. M., & Dietz, W. H. (2000). Establishing a standard definition for child overweight and obesity worldwide: international survey. *BMJ*, 320(7244), 1240-1243.

**LMS Method:**
- Cole, T. J., & Green, P. J. (1992). Smoothing reference centile curves: the LMS method and penalized likelihood. *Statistics in Medicine*, 11(10), 1305-1319.

## Issues and Bug Reports

Please report issues on our [GitHub Issues page](https://github.com/w4rhol/zanthror/issues) with:
- Reproducible example
- Expected vs actual behavior
- System information (R version, OS, package versions)

## Acknowledgments

- **Suzanna I. Vidmar, Tim J. Cole & Huiqi Pan** for the [original zbmicat implementation](https://www.stata-journal.com/article.html?article=dm0004_1)
- **Tim J. Cole** for the [sitar](https://github.com/statist7/sitar) package

## Citation

If you use zanthror in your research, please derive a citation from:

```
citation("zanthror")
```

---

**Package Website**: https://w4rhol.github.com/zanthror/
