# This script processes the raw IOTF data and saves it as package internal data

# Optimized helper function to load IOTF data
load_iotf_data <- function() {

  # Read dta
  iotf_data <- haven::read_dta("Q:/PHD/Health Improvement Branch/Epidemiology/Apps/Stata/ado/plus/z/zbmicat.dta")

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

# Apply any cleaning/standardization
iotf_data <- load_iotf_data()

