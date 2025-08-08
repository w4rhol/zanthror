# Script to build sysdata.rda with both iotf_data and zanthro_ref_data
# This script rebuilds the entire sysdata.rda file each time
# Loads dataframes from .rds files in data-raw/001_statafiles directory

library(usethis)

cat("Building sysdata.rda with internal package data...\n")
cat("Loading dataframes from .rds files in data-raw/001_statafiles/\n\n")

# Define the data directory
data_dir <- "data-raw/001_statafiles"

# Check if the data directory exists
if (!dir.exists(data_dir)) {
  cat("✗ Data directory not found:", data_dir, "\n")
  cat("Current working directory:", getwd(), "\n")
  cat("Please ensure the data-raw/001_statafiles directory exists\n")
  stop("Data directory not found")
}

cat("✓ Found data directory:", data_dir, "\n")

# Define the expected .rds files
rds_files <- list(
  iotf_data = "iotf_data.rds",
  zanthro_ref_data = "zanthro_ref_data.rds"
)

# Alternative file names to try if the standard names don't exist
alternative_names <- list(
  iotf_data = c("iotf_data.rds", "iotf.rds", "iotf_reference.rds"),
  zanthro_ref_data = c("zanthro_ref_data.rds", "zanthro_reference_data.rds",
                       "zanthro_data.rds", "zanthro_combined.rds")
)

# Function to find and load RDS file
load_rds_file <- function(data_name, expected_file, alternatives) {
  # Try the expected file first
  file_path <- file.path(data_dir, expected_file)

  if (file.exists(file_path)) {
    cat("Loading", data_name, "from:", expected_file, "\n")
    return(readRDS(file_path))
  }

  # Try alternative names
  for (alt_name in alternatives) {
    alt_path <- file.path(data_dir, alt_name)
    if (file.exists(alt_path)) {
      cat("Loading", data_name, "from alternative file:", alt_name, "\n")
      return(readRDS(alt_path))
    }
  }

  # If nothing found, list available files for debugging
  available_files <- list.files(data_dir, pattern = "\\.rds$", ignore.case = TRUE)
  cat("✗", data_name, "not found. Expected:", expected_file, "\n")
  cat("Available .rds files in", data_dir, ":\n")
  if (length(available_files) > 0) {
    for (file in available_files) {
      cat("  -", file, "\n")
    }
  } else {
    cat("  (no .rds files found)\n")
  }

  return(NULL)
}

# Load the dataframes
cat("=== LOADING DATAFRAMES ===\n")

# Load iotf_data
iotf_data <- load_rds_file("iotf_data", rds_files$iotf_data, alternative_names$iotf_data)

# Load zanthro_ref_data
zanthro_ref_data <- load_rds_file("zanthro_ref_data", rds_files$zanthro_ref_data, alternative_names$zanthro_ref_data)

# Check if both were loaded successfully
missing_data <- character()
if (is.null(iotf_data)) {
  missing_data <- c(missing_data, "iotf_data")
}
if (is.null(zanthro_ref_data)) {
  missing_data <- c(missing_data, "zanthro_ref_data")
}

if (length(missing_data) > 0) {
  cat("\n✗ Failed to load required dataframes:\n")
  for (missing in missing_data) {
    cat("  -", missing, "\n")
  }
  cat("\nPlease ensure the following .rds files exist in", data_dir, ":\n")
  cat("  - iotf_data.rds (IOTF reference data)\n")
  cat("  - zanthro_ref_data.rds (Combined zanthro reference data)\n")
  cat("\nYou can create these by:\n")
  cat("  1. Running your data preparation scripts\n")
  cat("  2. Saving with: saveRDS(data, 'data-raw/001_statafiles/filename.rds')\n")
  stop("Required dataframes not found")
}

# Validate that loaded objects are dataframes
if (!is.data.frame(iotf_data)) {
  stop("iotf_data is not a data.frame (class: ", class(iotf_data), ")")
}
if (!is.data.frame(zanthro_ref_data)) {
  stop("zanthro_ref_data is not a data.frame (class: ", class(zanthro_ref_data), ")")
}

cat("✓ Successfully loaded both dataframes\n")

# Verify data integrity
cat("\n=== DATA VERIFICATION ===\n")

# Check iotf_data
cat("iotf_data structure:\n")
cat("  File size:", file.size(file.path(data_dir, "iotf_data.rds")), "bytes\n")
cat("  Rows:", nrow(iotf_data), "\n")
cat("  Columns:", ncol(iotf_data), "\n")
cat("  Column names:", paste(names(iotf_data)[1:min(5, ncol(iotf_data))], collapse = ", "))
if (ncol(iotf_data) > 5) cat(", ...")
cat("\n")
cat("  Class:", paste(class(iotf_data), collapse = ", "), "\n")

# Sample data check
if (nrow(iotf_data) > 0) {
  cat("  Sample values (first row):\n")
  sample_cols <- names(iotf_data)[1:min(3, ncol(iotf_data))]
  for (col in sample_cols) {
    value <- iotf_data[1, col]
    # Handle different data types safely
    if (is.list(value)) {
      cat("    ", col, ": [list with", length(value[[1]]), "elements]\n")
    } else if (is.factor(value)) {
      cat("    ", col, ": ", as.character(value), " (factor)\n")
    } else if (is.character(value)) {
      cat("    ", col, ": \"", value, "\"\n")
    } else if (is.numeric(value)) {
      cat("    ", col, ":", round(value, 3), "\n")
    } else {
      cat("    ", col, ": ", toString(value), " (", class(value), ")\n")
    }
  }
}

# Check zanthro_ref_data
cat("\nzanthro_ref_data structure:\n")
cat("  File size:", file.size(file.path(data_dir, "zanthro_ref_data.rds")), "bytes\n")
cat("  Rows:", nrow(zanthro_ref_data), "\n")
cat("  Columns:", ncol(zanthro_ref_data), "\n")
cat("  Column names:", paste(names(zanthro_ref_data)[1:min(5, ncol(zanthro_ref_data))], collapse = ", "))
if (ncol(zanthro_ref_data) > 5) cat(", ...")
cat("\n")
cat("  Class:", paste(class(zanthro_ref_data), collapse = ", "), "\n")

# Check unique filenames in reference data
if ("filename" %in% names(zanthro_ref_data)) {
  unique_files <- unique(zanthro_ref_data$filename)
  cat("  Unique reference files:", length(unique_files), "\n")
  cat("  Sample reference files:", paste(head(unique_files, 3), collapse = ", "))
  if (length(unique_files) > 3) cat(", ...")
  cat("\n")
} else {
  cat("  Warning: No 'filename' column found in zanthro_ref_data\n")
}

# Check for required columns in zanthro_ref_data
zanthro_cols <- names(zanthro_ref_data)
sex_cols <- grep("sex", zanthro_cols, value = TRUE, ignore.case = TRUE)
xvar_cols <- grep("xvar|xmrg", zanthro_cols, value = TRUE, ignore.case = TRUE)
lms_cols <- grep("__SVJCKH[lms]", zanthro_cols, value = TRUE)

cat("  Key column groups found:\n")
cat("    Sex columns:", length(sex_cols))
if (length(sex_cols) > 0) cat(" (", paste(head(sex_cols, 2), collapse = ", "), ")")
cat("\n")
cat("    Age/xvar columns:", length(xvar_cols))
if (length(xvar_cols) > 0) cat(" (", paste(head(xvar_cols, 2), collapse = ", "), ")")
cat("\n")
cat("    LMS columns:", length(lms_cols))
if (length(lms_cols) > 0) cat(" (", paste(head(lms_cols, 3), collapse = ", "), ")")
cat("\n")

# Data quality checks
cat("\n=== DATA QUALITY CHECKS ===\n")

# Check for missing values in key columns
if (length(sex_cols) > 0) {
  sex_missing <- sum(is.na(zanthro_ref_data[[sex_cols[1]]]))
  cat("Missing values in", sex_cols[1], ":", sex_missing, "/", nrow(zanthro_ref_data), "\n")
}

if ("filename" %in% names(zanthro_ref_data)) {
  filename_missing <- sum(is.na(zanthro_ref_data$filename) | zanthro_ref_data$filename == "")
  cat("Missing/empty filenames:", filename_missing, "/", nrow(zanthro_ref_data), "\n")
}

# Check data ranges for numeric columns
numeric_cols <- sapply(zanthro_ref_data, is.numeric)
if (any(numeric_cols)) {
  numeric_names <- names(zanthro_ref_data)[numeric_cols]
  cat("Numeric columns ranges (sample):\n")
  sample_numeric <- numeric_names[1:min(3, length(numeric_names))]
  for (col in sample_numeric) {
    col_data <- zanthro_ref_data[[col]]
    finite_data <- col_data[is.finite(col_data)]
    if (length(finite_data) > 0) {
      cat("  ", col, ": [", round(min(finite_data), 3), " to ", round(max(finite_data), 3), "] (",
          sum(!is.finite(col_data)), " non-finite)\n")
    } else {
      cat("  ", col, ": all non-finite values\n")
    }
  }
}

# Memory usage
cat("Memory usage:\n")
cat("  iotf_data:", format(object.size(iotf_data), units = "auto"), "\n")
cat("  zanthro_ref_data:", format(object.size(zanthro_ref_data), units = "auto"), "\n")

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
expected_path <- "R/sysdata.rda"
if (file.exists(expected_path)) {
  cat("✓ sysdata.rda file exists at", expected_path, "\n")

  # Load and verify contents
  cat("\n=== VERIFICATION ===\n")
  cat("Loading sysdata.rda to verify contents...\n")

  # Create a new environment to load into
  verify_env <- new.env()
  tryCatch({
    load(expected_path, envir = verify_env)

    objects_in_sysdata <- ls(verify_env)
    cat("Objects in sysdata.rda:", paste(objects_in_sysdata, collapse = ", "), "\n")

    # Verify iotf_data
    if ("iotf_data" %in% objects_in_sysdata) {
      verify_iotf <- verify_env$iotf_data
      cat("✓ iotf_data verified in sysdata.rda (", nrow(verify_iotf), " rows, ", ncol(verify_iotf), " cols)\n")
    } else {
      cat("✗ iotf_data NOT found in sysdata.rda\n")
    }

    # Verify zanthro_ref_data
    if ("zanthro_ref_data" %in% objects_in_sysdata) {
      verify_zanthro <- verify_env$zanthro_ref_data
      cat("✓ zanthro_ref_data verified in sysdata.rda (", nrow(verify_zanthro), " rows, ", ncol(verify_zanthro), " cols)\n")
    } else {
      cat("✗ zanthro_ref_data NOT found in sysdata.rda\n")
    }

    # File size info
    file_info <- file.info(expected_path)
    cat("sysdata.rda file size:", round(file_info$size / 1024, 1), "KB\n")

    # Compare data integrity
    if (exists("verify_iotf") && identical(dim(iotf_data), dim(verify_iotf))) {
      cat("✓ iotf_data dimensions match original\n")
    }
    if (exists("verify_zanthro") && identical(dim(zanthro_ref_data), dim(verify_zanthro))) {
      cat("✓ zanthro_ref_data dimensions match original\n")
    }

  }, error = function(e) {
    cat("✗ Error verifying sysdata.rda:", e$message, "\n")
  })

} else {
  cat("✗ sysdata.rda file was not created at expected location:", expected_path, "\n")
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
cat("sysdata.rda build successful!\n")
cat("Package internal data now contains:\n")
cat("  - iotf_data (", nrow(iotf_data), " rows, ", ncol(iotf_data), " columns)\n")
cat("  - zanthro_ref_data (", nrow(zanthro_ref_data), " rows, ", ncol(zanthro_ref_data), " columns)\n")
cat("\nSource files:\n")
cat("  - data-raw/001_statafiles/iotf_data.rds\n")
cat("  - data-raw/001_statafiles/zanthro_ref_data.rds\n")
cat("\nBoth datasets are now available as internal package data.\n")
cat("The zanthro function can now access reference data without external files.\n")

rm(zanthro_ref_data)
