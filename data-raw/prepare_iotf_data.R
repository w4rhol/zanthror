# This script processes the raw IOTF data and saves it as package internal data

# Load and clean the IOTF reference data
iotf_data <- read.csv("data-raw/iotf.csv", stringsAsFactors = FALSE)

# Apply any cleaning/standardization
source("R/zbmicat_stata.R")  # To get load_iotf_data function
iotf_data <- load_iotf_data("data-raw/iotf.csv")

# Save as internal package data
usethis::use_data(iotf_data, internal = TRUE, overwrite = TRUE)
