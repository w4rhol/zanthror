# R vs Stata zanthro comparison script
# Loads Stata results and replicates all combinations using R function

library(dplyr)
library(tidyr)
library(ggplot2)
library(zanthror)
library(magrittr)


# ── file synchronisation function ─────────────────────────────────────────────

sync_file <- function(src, dst) {
  # 1. Check source exists
  if (!file.exists(src)) {
    stop(sprintf("Source file not found:\n  %s", src), call. = FALSE)
  }

  # 2. If destination is missing, or older than source, copy
  need_copy <- !file.exists(dst) ||
    file.info(src)$mtime > file.info(dst)$mtime

  if (need_copy) {
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(src, dst, overwrite = TRUE)
    if (!ok) stop(sprintf("Copy failed:\n  %s  →  %s", src, dst), call. = FALSE)

    message(sprintf("✔  Copied: %s → %s", basename(src), dirname(dst)))
  } else {
    message(sprintf("↻  Up-to-date: %s", dst))
  }
}


# ── file synchronisation ──────────────────────────────────────────────────────

# define files to synchronise
file_pairs <- list(
  list(
    src = "Q:/PHD/Health Improvement Branch/Epidemiology/Apps/R/warren holroyd/zanthror/data-raw/101_testdata/zanthror_testdata_compare.csv",
    dst = file.path("data-raw", "101_testdata", "zanthror_testdata_compare.csv")
  ),
  list(
    src = "Q:/PHD/Health Improvement Branch/Epidemiology/Apps/R/warren holroyd/zanthror/data-raw/201_statabaseline/generate_stata_results.ado",
    dst = file.path("data-raw", "201_statabaseline", "generate_stata_results.ado")
  )
)

# synchronise files
invisible(lapply(file_pairs, \(p) sync_file(p$src, p$dst)))

# clean up
rm(sync_file,file_pairs)


# ── compare zanthro vs zanthror ───────────────────────────────────────────────

# Load the Stata comparison data (change to local dir if/when Stata is migrated away from Citrix)
cat("Loading Stata comparison data...\n")
stata_data <- read.csv("data-raw/101_testdata/zanthror_testdata_compare.csv", stringsAsFactors = FALSE)

cat("Stata data loaded:", nrow(stata_data), "rows,", ncol(stata_data), "columns\n")

# Display the z-score columns that Stata generated
z_cols_stata <- grep("^z_", names(stata_data), value = TRUE)
cat("Stata generated", length(z_cols_stata), "z-score columns:\n")
cat(paste(z_cols_stata, collapse = ", "), "\n\n")

# Define all valid chart-version combinations (matching Stata script)
combinations <- list(
  # US CDC 2000 Charts
  list(chart = "la", version = "US", measure = "height_cm", xvar = "age_years"),
  list(chart = "ha", version = "US", measure = "height_cm", xvar = "age_years"),
  list(chart = "wa", version = "US", measure = "weight_kg", xvar = "age_years"),
  list(chart = "ba", version = "US", measure = "bmi", xvar = "age_years"),
  list(chart = "hca", version = "US", measure = "head_circumference_cm", xvar = "age_years"),
  list(chart = "wl", version = "US", measure = "weight_kg", xvar = "height_cm"),
  list(chart = "wh", version = "US", measure = "weight_kg", xvar = "height_cm"),

  # UK 1990 Charts
  list(chart = "ha", version = "UK", measure = "height_cm", xvar = "age_years"),
  list(chart = "wa", version = "UK", measure = "weight_kg", xvar = "age_years"),
  list(chart = "ba", version = "UK", measure = "bmi", xvar = "age_years"),
  list(chart = "hca", version = "UK", measure = "head_circumference_cm", xvar = "age_years"),
  list(chart = "sha", version = "UK", measure = "sitting_height_cm", xvar = "age_years"),
  list(chart = "lla", version = "UK", measure = "leg_length_cm", xvar = "age_years"),
  list(chart = "wsa", version = "UK", measure = "waist_circumference_cm", xvar = "age_years"),
  list(chart = "bfa", version = "UK", measure = "body_fat_percent", xvar = "age_years"),

  # WHO Charts
  list(chart = "ha", version = "WHO", measure = "height_cm", xvar = "age_years"),
  list(chart = "wa", version = "WHO", measure = "weight_kg", xvar = "age_years"),
  list(chart = "ba", version = "WHO", measure = "bmi", xvar = "age_years"),
  list(chart = "hca", version = "WHO", measure = "head_circumference_cm", xvar = "age_years"),
  list(chart = "aca", version = "WHO", measure = "arm_circumference_cm", xvar = "age_years"),
  list(chart = "ssa", version = "WHO", measure = "subscapular_skinfold_mm", xvar = "age_years"),
  list(chart = "tsa", version = "WHO", measure = "triceps_skinfold_mm", xvar = "age_years"),
  list(chart = "wl", version = "WHO", measure = "weight_kg", xvar = "height_cm"),
  list(chart = "wh", version = "WHO", measure = "weight_kg", xvar = "height_cm"),

  # UK-WHO Preterm Charts
  list(chart = "ha", version = "UKWHOpreterm", measure = "height_cm", xvar = "age_years"),
  list(chart = "wa", version = "UKWHOpreterm", measure = "weight_kg", xvar = "age_years"),
  list(chart = "ba", version = "UKWHOpreterm", measure = "bmi", xvar = "age_years"),
  list(chart = "hca", version = "UKWHOpreterm", measure = "head_circumference_cm", xvar = "age_years"),

  # UK-WHO Term Charts
  list(chart = "ha", version = "UKWHOterm", measure = "height_cm", xvar = "age_years"),
  list(chart = "wa", version = "UKWHOterm", measure = "weight_kg", xvar = "age_years"),
  list(chart = "ba", version = "UKWHOterm", measure = "bmi", xvar = "age_years"),
  list(chart = "hca", version = "UKWHOterm", measure = "head_circumference_cm", xvar = "age_years")
)

cat("Testing", length(combinations), "chart-version combinations with R function...\n\n")

# Initialize comparison results
comparison_results <- data.frame()
failed_combinations <- character()

# Process each combination
for (i in seq_along(combinations)) {
  combo <- combinations[[i]]

  # Create variable names to match actual Stata output
  # Based on the actual Stata variable names we can see
  # Manual lookup for exact Stata variable names (based on actual data)
  stata_var_lookup <- list(
    "la_US_height_cm_age_years" = "z_la_US_heightcm_ageyrs",
    "ha_US_height_cm_age_years" = "z_ha_US_heightcm_ageyrs",
    "wa_US_weight_kg_age_years" = "z_wa_US_weightkg_ageyrs",
    "ba_US_bmi_age_years" = "z_ba_US_bmi_ageyrs",
    "hca_US_head_circumference_cm_age_years" = "z_hca_US_head_circumferencecm_ag",
    "wl_US_weight_kg_height_cm" = "z_wl_US_weightkg_heightcm",
    "wh_US_weight_kg_height_cm" = "z_wh_US_weightkg_heightcm",

    "ha_UK_height_cm_age_years" = "z_ha_UK_heightcm_ageyrs",
    "wa_UK_weight_kg_age_years" = "z_wa_UK_weightkg_ageyrs",
    "ba_UK_bmi_age_years" = "z_ba_UK_bmi_ageyrs",
    "hca_UK_head_circumference_cm_age_years" = "z_hca_UK_head_circumferencecm_ag",
    "sha_UK_sitting_height_cm_age_years" = "z_sha_UK_sitting_heightcm_ageyrs",
    "lla_UK_leg_length_cm_age_years" = "z_lla_UK_leg_lengthcm_ageyrs",
    "wsa_UK_waist_circumference_cm_age_years" = "z_wsa_UK_waist_circumferencecm_a",
    "bfa_UK_body_fat_percent_age_years" = "z_bfa_UK_body_fatpct_ageyrs",

    "ha_WHO_height_cm_age_years" = "z_ha_WHO_heightcm_ageyrs",
    "wa_WHO_weight_kg_age_years" = "z_wa_WHO_weightkg_ageyrs",
    "ba_WHO_bmi_age_years" = "z_ba_WHO_bmi_ageyrs",
    "hca_WHO_head_circumference_cm_age_years" = "z_hca_WHO_head_circumferencecm_a",
    "aca_WHO_arm_circumference_cm_age_years" = "z_aca_WHO_arm_circumferencecm_ag",
    "ssa_WHO_subscapular_skinfold_mm_age_years" = "z_ssa_WHO_subscapular_skinfoldmm",
    "tsa_WHO_triceps_skinfold_mm_age_years" = "z_tsa_WHO_triceps_skinfoldmm_age",
    "wl_WHO_weight_kg_height_cm" = "z_wl_WHO_weightkg_heightcm",
    "wh_WHO_weight_kg_height_cm" = "z_wh_WHO_weightkg_heightcm",

    "ha_UKWHOpreterm_height_cm_age_years" = "z_ha_UKWHOpreterm_heightcm_ageyr",
    "wa_UKWHOpreterm_weight_kg_age_years" = "z_wa_UKWHOpreterm_weightkg_ageyr",
    "ba_UKWHOpreterm_bmi_age_years" = "z_ba_UKWHOpreterm_bmi_ageyrs",
    "hca_UKWHOpreterm_head_circumference_cm_age_years" = "z_hca_UKWHOpreterm_head_circumfe",

    "ha_UKWHOterm_height_cm_age_years" = "z_ha_UKWHOterm_heightcm_ageyrs",
    "wa_UKWHOterm_weight_kg_age_years" = "z_wa_UKWHOterm_weightkg_ageyrs",
    "ba_UKWHOterm_bmi_age_years" = "z_ba_UKWHOterm_bmi_ageyrs",
    "hca_UKWHOterm_head_circumference_cm_age_years" = "z_hca_UKWHOterm_head_circumferen"
  )

  combo_key <- paste(combo$chart, combo$version, combo$measure, combo$xvar, sep = "_")
  stata_var_name <- stata_var_lookup[[combo_key]]

  if (is.null(stata_var_name)) {
    cat("  -> Skipping: No manual lookup found for", combo_key, "\n\n")
    failed_combinations <- c(failed_combinations, paste(combo$chart, combo$version, "- no lookup"))
    next
  }

  r_var_name <- paste0(stata_var_name, "_r")

  cat("Processing combination", i, "of", length(combinations), ":",
      combo$chart, combo$version, combo$measure, combo$xvar, "\n")
  cat("  Stata variable:", stata_var_name, "\n")
  cat("  R variable:", r_var_name, "\n")

  # Check if the required variables exist in the data
  if (!combo$measure %in% names(stata_data)) {
    cat("  -> Skipping: measure variable", combo$measure, "not found\n\n")
    failed_combinations <- c(failed_combinations, paste(combo$chart, combo$version, "- missing measure"))
    next
  }

  if (!combo$xvar %in% names(stata_data)) {
    cat("  -> Skipping: xvar variable", combo$xvar, "not found\n\n")
    failed_combinations <- c(failed_combinations, paste(combo$chart, combo$version, "- missing xvar"))
    next
  }

  # Check if Stata generated this combination
  if (!stata_var_name %in% names(stata_data)) {
    cat("  -> Skipping: Stata variable", stata_var_name, "not found in data\n\n")
    failed_combinations <- c(failed_combinations, paste(combo$chart, combo$version, "- not in Stata results"))
    next
  }

  # Count valid observations for this combination
  valid_obs <- sum(!is.na(stata_data[[combo$measure]]) &
                     !is.na(stata_data[[combo$xvar]]) &
                     !is.na(stata_data$gender_label))

  if (valid_obs == 0) {
    cat("  -> Skipping: No valid observations\n\n")
    failed_combinations <- c(failed_combinations, paste(combo$chart, combo$version, "- no valid data"))
    next
  }

  cat("  -> Valid observations:", valid_obs, "\n")

  # Generate R z-scores
  tryCatch({
    r_result <- zanthro(measure = stata_data[[combo$measure]],
                        xvar = stata_data[[combo$xvar]],
                        chart = combo$chart,
                        version = combo$version,
                        gender = stata_data$gender_label,
                        male_code = "Male",
                        female_code = "Female")

    # Add R results to the data
    stata_data[[r_var_name]] <- r_result

    # Calculate comparison statistics
    stata_values <- stata_data[[stata_var_name]]
    r_values <- stata_data[[r_var_name]]

    # Count non-missing values
    stata_valid <- sum(!is.na(stata_values))
    r_valid <- sum(!is.na(r_values))
    both_valid <- sum(!is.na(stata_values) & !is.na(r_values))

    # Calculate differences for cases where both are non-missing
    if (both_valid > 0) {
      differences <- r_values - stata_values
      differences <- differences[!is.na(differences)]

      comparison_results <- rbind(comparison_results, data.frame(
        combination = paste(combo$chart, combo$version, combo$measure, combo$xvar),
        chart = combo$chart,
        version = combo$version,
        measure = combo$measure,
        xvar = combo$xvar,
        stata_variable = stata_var_name,
        r_variable = r_var_name,
        stata_valid_n = stata_valid,
        r_valid_n = r_valid,
        both_valid_n = both_valid,
        mean_difference = mean(differences),
        sd_difference = sd(differences),
        max_abs_difference = max(abs(differences)),
        rmse = sqrt(mean(differences^2)),
        correlation = cor(stata_values, r_values, use = "complete.obs"),
        stringsAsFactors = FALSE
      ))

      cat("  -> Success: Stata", stata_valid, "valid, R", r_valid, "valid,", both_valid, "overlap\n")
      cat("  -> Mean difference:", round(mean(differences), 6),
          "Max |diff|:", round(max(abs(differences)), 6), "\n")
    } else {
      cat("  -> Warning: No overlapping valid values\n")
    }

  }, error = function(e) {
    cat("  -> Error:", e$message, "\n")
    failed_combinations <- c(failed_combinations, paste(combo$chart, combo$version, "- R error"))
  })

  cat("\n")
}

# Display comparison summary
cat("=== COMPARISON SUMMARY ===\n")
cat("Total combinations attempted:", length(combinations), "\n")
cat("Successfully compared:", nrow(comparison_results), "\n")
cat("Failed combinations:", length(failed_combinations), "\n\n")

if (length(failed_combinations) > 0) {
  cat("Failed combinations:\n")
  for (fail in failed_combinations) {
    cat(" -", fail, "\n")
  }
  cat("\n")
}

# Show detailed comparison results
if (nrow(comparison_results) > 0) {
  cat("=== DETAILED COMPARISON RESULTS ===\n")

  # Sort by maximum absolute difference
  comparison_results <- comparison_results[order(comparison_results$max_abs_difference, decreasing = TRUE), ]

  print(comparison_results[, c("combination", "both_valid_n", "mean_difference",
                               "max_abs_difference", "rmse", "correlation")])

  cat("\n=== SUMMARY STATISTICS ===\n")
  cat("Mean of mean differences:", round(mean(comparison_results$mean_difference, na.rm = TRUE), 6), "\n")
  cat("SD of mean differences:", round(sd(comparison_results$mean_difference, na.rm = TRUE), 6), "\n")
  cat("Maximum absolute difference overall:", round(max(comparison_results$max_abs_difference, na.rm = TRUE), 6), "\n")
  cat("Mean RMSE:", round(mean(comparison_results$rmse, na.rm = TRUE), 6), "\n")
  cat("Mean correlation:", round(mean(comparison_results$correlation, na.rm = TRUE), 4), "\n")

  # Identify problematic combinations
  large_diff_threshold <- 0.01  # z-score difference > 0.01
  problematic <- comparison_results[comparison_results$max_abs_difference > large_diff_threshold, ]

  if (nrow(problematic) > 0) {
    cat("\n=== COMBINATIONS WITH LARGE DIFFERENCES (>", large_diff_threshold, ") ===\n")
    print(problematic[, c("combination", "max_abs_difference", "mean_difference", "correlation")])
  }

  # Create a simple visualization
  cat("\n=== CREATING COMPARISON PLOTS ===\n")

  # Plot 1: Distribution of differences
  if (nrow(comparison_results) > 0) {
    p1 <- ggplot(comparison_results, aes(x = mean_difference)) +
      geom_histogram(bins = 20, alpha = 0.7, fill = "skyblue") +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Distribution of Mean Differences (R - Stata)",
           x = "Mean Difference", y = "Count") +
      theme_minimal()

    print(p1)

    # Plot 2: Max absolute difference by combination
    p2 <- ggplot(comparison_results, aes(x = reorder(combination, max_abs_difference),
                                         y = max_abs_difference)) +
      geom_col(fill = "coral") +
      geom_hline(yintercept = large_diff_threshold, color = "red", linetype = "dashed") +
      labs(title = "Maximum Absolute Difference by Combination",
           x = "Combination", y = "Max |Difference|") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

    print(p2)
  }
}

#####  zbmicat  ################################################################

stata_data %<>%
  mutate(
    zbmicat_r = zbmicat(
      bmi = bmi,
      age = age_years,
      gender = gender,
      return = "string", wtabbr = TRUE
    )
  )

table(stata_data$bmicat,stata_data$zbmicat_r, useNA = "ifany")

cat("\nComparison complete!\n")
