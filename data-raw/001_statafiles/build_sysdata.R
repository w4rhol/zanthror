# Script to build sysdata.rda with both iotf_data and zanthro_ref_data
# This script rebuilds the entire sysdata.rda file each time
# Expects both dataframes to be already loaded in the environment

library(usethis)

cat("Building sysdata.rda with internal package data...\n")

# Check for required dataframes in the current environment
required_data <- c("iotf_data", "zanthro_ref_data")
missing_data <- character()

for (data_name in required_data) {
  if (!exists(data_name, envir = .GlobalEnv)) {
    missing_data <- c(missing_data, data_name)
  } else {
    # Check if it's actually a dataframe
    data_obj <- get(data_name, envir = .GlobalEnv)
    if (!is.data.frame(data_obj)) {
      missing_data <- c(missing_data, paste0(data_name, " (not a dataframe)"))
    }
  }
}

if (length(missing_data) > 0) {
  cat("✗ Missing required dataframes:\n")
  for (missing in missing_data) {
    cat("  -", missing, "\n")
  }
  cat("\nPlease ensure the following dataframes are loaded in your environment:\n")
  cat("  - iotf_data: IOTF reference data\n")
  cat("  - zanthro_ref_data: Combined zanthro reference data\n")
  cat("\nYou can load them by:\n")
  cat("  1. Running the R .dta merge script: source('r_merge_dta_files.R')\n")
  cat("  2. Loading from RDS: zanthro_ref_data <- readRDS('zanthro_reference_data.rds')\n")
  cat("  3. Loading from CSV: zanthro_ref_data <- read.csv('zanthro_reference_data.csv')\n")
  cat("  4. Recreating iotf_data from your source\n")
  stop("Required dataframes not found in environment")
}

cat("✓ Found all required dataframes in environment\n")

# Get the dataframes from global environment
iotf_data <- get("iotf_data", envir = .GlobalEnv)
zanthro_ref_data <- get("zanthro_ref_data", envir = .GlobalEnv)

# Verify data integrity
cat("\n=== DATA VERIFICATION ===\n")

# Check iotf_data
cat("iotf_data structure:\n")
cat("  Rows:", nrow(iotf_data), "\n")
cat("  Columns:", ncol(iotf_data), "\n")
cat("  Column names:", paste(names(iotf_data), collapse = ", "), "\n")
cat("  Class:", class(iotf_data), "\n")

# Check zanthro_ref_data
cat("\nzanthro_ref_data structure:\n")
cat("  Rows:", nrow(zanthro_ref_data), "\n")
cat("  Columns:", ncol(zanthro_ref_data), "\n")
cat("  Column names:", paste(names(zanthro_ref_data), collapse = ", "), "\n")
cat("  Class:", class(zanthro_ref_data), "\n")

# Check unique filenames in reference data
if ("filename" %in% names(zanthro_ref_data)) {
  unique_files <- unique(zanthro_ref_data$filename)
  cat("  Unique reference files:", length(unique_files), "\n")
  cat("  Reference files:", paste(head(unique_files, 5), collapse = ", "),
      ifelse(length(unique_files) > 5, "...", ""), "\n")
} else {
  cat("  Warning: No 'filename' column found in zanthro_ref_data\n")
}

# Check for required columns in zanthro_ref_data
required_cols <- c("filename")
zanthro_cols <- names(zanthro_ref_data)
sex_cols <- grep("sex", zanthro_cols, value = TRUE, ignore.case = TRUE)
xvar_cols <- grep("xvar", zanthro_cols, value = TRUE, ignore.case = TRUE)
lms_cols <- grep("__SVJCKH[lms]", zanthro_cols, value = TRUE)

cat("  Key column groups found:\n")
cat("    Sex columns:", length(sex_cols), ifelse(length(sex_cols) > 0, paste0(" (", paste(head(sex_cols, 2), collapse = ", "), ")"), ""), "\n")
cat("    Age/xvar columns:", length(xvar_cols), ifelse(length(xvar_cols) > 0, paste0(" (", paste(head(xvar_cols, 2), collapse = ", "), ")"), ""), "\n")
cat("    LMS columns:", length(lms_cols), ifelse(length(lms_cols) > 0, paste0(" (", paste(head(lms_cols, 3), collapse = ", "), ")"), ""), "\n")

# Data quality checks
cat("\n=== DATA QUALITY CHECKS ===\n")

# Check for missing values in key columns
if (length(sex_cols) > 0) {
  sex_missing <- sum(is.na(zanthro_ref_data[[sex_cols[1]]]))
  cat("Missing values in", sex_cols[1], ":", sex_missing, "\n")
}

if ("filename" %in% names(zanthro_ref_data)) {
  filename_missing <- sum(is.na(zanthro_ref_data$filename))
  cat("Missing filenames:", filename_missing, "\n")
}

# Check data ranges
numeric_cols <- sapply(zanthro_ref_data, is.numeric)
if (any(numeric_cols)) {
  cat("Numeric columns ranges (first 3):\n")
  numeric_summary <- sapply(zanthro_ref_data[, numeric_cols][1:min(3, sum(numeric_cols))], function(x) {
    paste0("[", round(min(x, na.rm = TRUE), 3), " to ", round(max(x, na.rm = TRUE), 3), "]")
  })
  for (i in seq_along(numeric_summary)) {
    cat("  ", names(numeric_summary)[i], ":", numeric_summary[i], "\n")
  }
}

# Create sysdata.rda with both datasets
cat("\n=== CREATING SYSDATA.RDA ===\n")
cat("Creating sysdata.rda with iotf_data and zanthro_ref_data...\n")

# Use usethis to create internal data
# Note: use_data with internal=TRUE will overwrite the entire sysdata.rda file
tryCatch({
  usethis::use_data(iotf_data, zanthro_ref_data, internal = TRUE, overwrite = TRUE)
  cat("✓ sysdata.rda created successfully\n")
}, error = function(e) {
  cat("✗ Error creating sysdata.rda:", e$message, "\n")
  stop("Failed to create sysdata.rda")
})

# Verify the created file
if (file.exists("R/sysdata.rda")) {
  cat("✓ sysdata.rda file exists at R/sysdata.rda\n")

  # Load and verify contents
  cat("\n=== VERIFICATION ===\n")
  cat("Loading sysdata.rda to verify contents...\n")

  # Create a new environment to load into
  verify_env <- new.env()
  tryCatch({
    load("R/sysdata.rda", envir = verify_env)

    objects_in_sysdata <- ls(verify_env)
    cat("Objects in sysdata.rda:", paste(objects_in_sysdata, collapse = ", "), "\n")

    # Verify iotf_data
    if ("iotf_data" %in% objects_in_sysdata) {
      cat("✓ iotf_data found in sysdata.rda (", nrow(verify_env$iotf_data), " rows)\n")
    } else {
      cat("✗ iotf_data NOT found in sysdata.rda\n")
    }

    # Verify zanthro_ref_data
    if ("zanthro_ref_data" %in% objects_in_sysdata) {
      cat("✓ zanthro_ref_data found in sysdata.rda (", nrow(verify_env$zanthro_ref_data), " rows)\n")
    } else {
      cat("✗ zanthro_ref_data NOT found in sysdata.rda\n")
    }

    # File size info
    file_info <- file.info("R/sysdata.rda")
    cat("sysdata.rda file size:", round(file_info$size / 1024, 1), "KB\n")

  }, error = function(e) {
    cat("✗ Error verifying sysdata.rda:", e$message, "\n")
  })

} else {
  cat("✗ sysdata.rda file was not created at expected location\n")
  cat("Expected location: R/sysdata.rda\n")
  cat("Current working directory:", getwd(), "\n")

  # Check if it was created somewhere else
  possible_locations <- c("data/sysdata.rda", "sysdata.rda", "./R/sysdata.rda")
  for (loc in possible_locations) {
    if (file.exists(loc)) {
      cat("Found sysdata.rda at alternative location:", loc, "\n")
    }
  }
}

cat("\n=== COMPLETE ===\n")
cat("sysdata.rda build complete. The file contains:\n")
cat("  - iotf_data (", nrow(iotf_data), " rows)\n")
cat("  - zanthro_ref_data (", nrow(zanthro_ref_data), " rows)\n")
cat("\nBoth datasets are now available as internal package data.\n")
cat("You can now use the zanthro function without any external data files.\n")
