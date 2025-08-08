# R script to merge all zanthro reference .dta files using haven
# Equivalent to the Stata merging script but creates an R dataframe

library(haven)
library(dplyr)

cat("R script to merge all zanthro reference .dta files\n")
cat("==================================================\n\n")

# Define the base path - adjust as needed for your system
base_path <- "Q:/PHD/Health Improvement Branch/Epidemiology/Apps/Stata/ado/plus/z/"

# Alternative paths to try if the main path doesn't work
possible_paths <- c(
  "Q:/PHD/Health Improvement Branch/Epidemiology/Apps/Stata/ado/plus/z/",
  "Q:/PHD/Health Improvement Branch/Epidemiology/Apps/Stata/ado/plus/",
  "Q:/PHD/Health Improvement Branch/Epidemiology/Apps/Stata/ado/",
  "Q:/PHD/Health Improvement Branch/Epidemiology/Apps/Stata/",
  "./"
)

# Find the correct base path
actual_base_path <- NULL
for (path in possible_paths) {
  test_file <- file.path(path, "zllageuk.dta")
  if (file.exists(test_file)) {
    actual_base_path <- path
    cat("Found .dta files in:", actual_base_path, "\n")
    break
  }
}

if (is.null(actual_base_path)) {
  cat("Trying to locate files using common names...\n")
  # Try to find any .dta file with zanthro naming pattern
  for (path in possible_paths) {
    dta_files <- list.files(path, pattern = "^z.*\\.dta$", full.names = FALSE)
    if (length(dta_files) > 0) {
      actual_base_path <- path
      cat("Found", length(dta_files), ".dta files in:", actual_base_path, "\n")
      break
    }
  }
}

if (is.null(actual_base_path)) {
  stop("Cannot locate the .dta reference files. Please check the path.")
}

# Define all the .dta files referenced in the zanthro code
dta_files <- c(
  "zllageuk.dta", "zshtageuk.dta", "zwsageuk.dta", "zbfageuk.dta",
  "zacagewho.dta", "zssagewho.dta", "ztsagewho.dta", "zlenageius.dta",
  "zhcageuk.dta", "zhcageius.dta", "zhcagewho.dta", "zhcageukwhopreterm.dta",
  "zhcageukwhoterm.dta", "zbmiageuk.dta", "zbmiageus.dta", "zbmiagewho.dta",
  "zbmiageukwhopreterm.dta", "zbmiageukwhoterm.dta", "zwtageuk.dta",
  "zwtagecomus.dta", "zwtagewho.dta", "zwtageukwhopreterm.dta",
  "zwtageukwhoterm.dta", "zhtageuk.dta", "zhtageus.dta", "zlhagewho.dta",
  "zlhtageukwhopreterm.dta", "zlhtageukwhoterm.dta", "zwthtus.dta",
  "zwthtwho.dta", "zwtlenius.dta", "zwtlenwho.dta"
)

cat("Processing", length(dta_files), "reference files...\n\n")

# Initialize list to store dataframes
dta_list <- list()
failed_files <- character()
processed_count <- 0

# Process each file
for (i in seq_along(dta_files)) {
  filename <- dta_files[i]
  filepath <- file.path(actual_base_path, filename)

  cat(sprintf("Processing %2d/%d: %s\n", i, length(dta_files), filename))

  # Check if file exists
  if (!file.exists(filepath)) {
    cat("  -> File not found, skipping\n")
    failed_files <- c(failed_files, paste(filename, "- not found"))
    next
  }

  # Try to read the .dta file
  tryCatch({
    # Read the Stata file using haven
    dta_data <- haven::read_dta(filepath)

    # Convert to regular dataframe (removes haven attributes)
    dta_data <- as.data.frame(dta_data)

    # Add filename column to identify source
    dta_data$filename <- filename

    # Move filename to first column
    dta_data <- dta_data[, c("filename", setdiff(names(dta_data), "filename"))]

    # Display file information
    cat("  -> Successfully loaded:", nrow(dta_data), "rows,", ncol(dta_data) - 1, "variables\n")
    cat("  -> Variables:", paste(names(dta_data)[2:min(6, ncol(dta_data))], collapse = ", "))
    if (ncol(dta_data) > 6) cat(", ...")
    cat("\n")

    # Store in list
    dta_list[[filename]] <- dta_data
    processed_count <- processed_count + 1

  }, error = function(e) {
    cat("  -> Error reading file:", e$message, "\n")
    failed_files <- c(failed_files, paste(filename, "- read error"))
  })

  cat("\n")
}

# Combine all dataframes
if (length(dta_list) > 0) {
  cat("=== COMBINING DATAFRAMES ===\n")
  cat("Successfully loaded", length(dta_list), "files\n")

  # Get all unique column names across all dataframes
  all_columns <- unique(unlist(lapply(dta_list, names)))
  cat("Total unique columns found:", length(all_columns), "\n")
  cat("Columns:", paste(head(all_columns, 10), collapse = ", "))
  if (length(all_columns) > 10) cat(", ...")
  cat("\n\n")

  # Ensure all dataframes have the same columns (fill missing with NA)
  standardized_list <- lapply(dta_list, function(df) {
    missing_cols <- setdiff(all_columns, names(df))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        df[[col]] <- NA
      }
    }
    # Reorder columns to match standard order
    df[, all_columns]
  })

  # Combine using rbind
  cat("Combining all dataframes...\n")
  zanthro_ref_data <- do.call(rbind, standardized_list)

  # Reset row names
  rownames(zanthro_ref_data) <- NULL

  cat("✓ Combined dataframe created successfully\n")
  cat("Final dimensions:", nrow(zanthro_ref_data), "rows ×", ncol(zanthro_ref_data), "columns\n\n")

} else {
  stop("No .dta files were successfully loaded")
}

# Display summary information
cat("=== SUMMARY ===\n")
cat("Total files attempted:", length(dta_files), "\n")
cat("Successfully processed:", processed_count, "\n")
cat("Failed files:", length(failed_files), "\n")

if (length(failed_files) > 0) {
  cat("\nFailed files:\n")
  for (failure in failed_files) {
    cat(" -", failure, "\n")
  }
}

# Summary by filename
cat("\nSummary by reference file:\n")
file_summary <- zanthro_ref_data %>%
  group_by(filename) %>%
  summarise(
    rows = n(),
    unique_sex_values = length(unique(get(names(.)[grepl("sex", names(.), ignore.case = TRUE)][1]), na.rm = TRUE)),
    .groups = "drop"
  )

print(file_summary)

# # Check data structure
# cat("\n=== DATA STRUCTURE ===\n")
# cat("Column names and types:\n")
# str(zanthro_ref_data)
#
# # Save as RDS file (R's native format, more efficient than CSV)
# cat("\n=== SAVING DATA ===\n")
# saveRDS(zanthro_ref_data, "zanthro_reference_data.rds")
# cat("✓ Saved as zanthro_reference_data.rds\n")
#
# # Also save as CSV for compatibility
# write.csv(zanthro_ref_data, "zanthro_reference_data_from_r.csv", row.names = FALSE)
# cat("✓ Saved as zanthro_reference_data_from_r.csv\n")
#
# # Final verification
# cat("\n=== VERIFICATION ===\n")
# if (file.exists("zanthro_reference_data.rds")) {
#   test_load <- readRDS("zanthro_reference_data.rds")
#   cat("✓ RDS file verified:", nrow(test_load), "rows\n")
# }
#
# if (file.exists("zanthro_reference_data_from_r.csv")) {
#   cat("✓ CSV file created:", round(file.size("zanthro_reference_data_from_r.csv") / 1024, 1), "KB\n")
# }
#
# cat("\n=== COMPLETE ===\n")
# cat("Reference data successfully merged and saved!\n")
# cat("You can now use either:\n")
# cat("  - zanthro_ref_data (in memory)\n")
# cat("  - zanthro_reference_data.rds (R native format)\n")
# cat("  - zanthro_reference_data_from_r.csv (CSV format)\n")
