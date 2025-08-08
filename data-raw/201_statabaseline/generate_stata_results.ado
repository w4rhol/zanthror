* Stata script to test all zanthro combinations on test data
* and save results for comparison with R implementation

clear all
set more off

*set wd
cd "Q:\PHD\Health Improvement Branch\Epidemiology\Apps\R\warren holroyd\zanthror\data-raw\101_testdata"

* Load the test data
import delimited "zanthror_testdata.csv", clear

* Display data summary
describe
summarize

* Check the data structure
list participant_id age_years gender gender_label height_cm weight_kg in 1/5

egen bmicat = zbmicat(bmi), xvar(age_years) gender(gender) gencode(male=1, female=2) ageunit(year)

* Define all valid chart-version combinations with proper variable names
* Format: chart|version|measure_variable|xvar_variable

local combinations ""

* US CDC 2000 Charts
local combinations "`combinations' la|US|height_cm|age_years"           // length-for-age (using height as proxy)
local combinations "`combinations' ha|US|height_cm|age_years"           // height-for-age
local combinations "`combinations' wa|US|weight_kg|age_years"           // weight-for-age
local combinations "`combinations' ba|US|bmi|age_years"                 // BMI-for-age
local combinations "`combinations' hca|US|head_circumference_cm|age_years" // head circumference-for-age
local combinations "`combinations' wl|US|weight_kg|height_cm"           // weight-for-length (xvar=height)
local combinations "`combinations' wh|US|weight_kg|height_cm"           // weight-for-height (xvar=height)

* UK 1990 Charts
local combinations "`combinations' ha|UK|height_cm|age_years"           // height-for-age
local combinations "`combinations' wa|UK|weight_kg|age_years"           // weight-for-age
local combinations "`combinations' ba|UK|bmi|age_years"                 // BMI-for-age
local combinations "`combinations' hca|UK|head_circumference_cm|age_years" // head circumference-for-age
local combinations "`combinations' sha|UK|sitting_height_cm|age_years"  // sitting height-for-age
local combinations "`combinations' lla|UK|leg_length_cm|age_years"      // leg length-for-age
local combinations "`combinations' wsa|UK|waist_circumference_cm|age_years" // waist-for-age
local combinations "`combinations' bfa|UK|body_fat_percent|age_years"   // body fat-for-age

* WHO Charts
local combinations "`combinations' ha|WHO|height_cm|age_years"          // height-for-age
local combinations "`combinations' wa|WHO|weight_kg|age_years"          // weight-for-age
local combinations "`combinations' ba|WHO|bmi|age_years"                // BMI-for-age
local combinations "`combinations' hca|WHO|head_circumference_cm|age_years" // head circumference-for-age
local combinations "`combinations' aca|WHO|arm_circumference_cm|age_years" // arm circumference-for-age
local combinations "`combinations' ssa|WHO|subscapular_skinfold_mm|age_years" // subscapular skinfold-for-age
local combinations "`combinations' tsa|WHO|triceps_skinfold_mm|age_years" // triceps skinfold-for-age
local combinations "`combinations' wl|WHO|weight_kg|height_cm"          // weight-for-length
local combinations "`combinations' wh|WHO|weight_kg|height_cm"          // weight-for-height

* UK-WHO Preterm Charts
local combinations "`combinations' ha|UKWHOpreterm|height_cm|age_years" // height-for-age
local combinations "`combinations' wa|UKWHOpreterm|weight_kg|age_years" // weight-for-age
local combinations "`combinations' ba|UKWHOpreterm|bmi|age_years"       // BMI-for-age
local combinations "`combinations' hca|UKWHOpreterm|head_circumference_cm|age_years" // head circumference-for-age

* UK-WHO Term Charts
local combinations "`combinations' ha|UKWHOterm|height_cm|age_years"    // height-for-age
local combinations "`combinations' wa|UKWHOterm|weight_kg|age_years"    // weight-for-age
local combinations "`combinations' ba|UKWHOterm|bmi|age_years"          // BMI-for-age
local combinations "`combinations' hca|UKWHOterm|head_circumference_cm|age_years" // head circumference-for-age

display ""
display "Testing the following chart-version combinations:"
local combo_count : word count `combinations'
display "Total combinations to test: `combo_count'"
display ""

* Loop through each combination and generate z-scores
local test_count = 0
foreach combo of local combinations {
    local test_count = `test_count' + 1

    * Parse the combination string using | as delimiter
    tokenize "`combo'", parse("|")
    local chart "`1'"
    local version "`3'"
    local measure_var "`5'"
    local xvar_var "`7'"

    display "Test `test_count'/`combo_count': Testing `chart' `version' with measure=`measure_var' xvar=`xvar_var'"

    * Check if required variables exist and have data
    capture confirm variable `measure_var'
    if _rc != 0 {
        display "  -> Skipping: Variable `measure_var' not found"
        continue
    }

    capture confirm variable `xvar_var'
    if _rc != 0 {
        display "  -> Skipping: Variable `xvar_var' not found"
        continue
    }

    * Count non-missing observations for this combination
    count if !missing(`measure_var') & !missing(`xvar_var') & !missing(gender)
    local valid_obs = r(N)

    if `valid_obs' == 0 {
        display "  -> Skipping: No valid observations for this combination"
        continue
    }

    display "  -> Valid observations: `valid_obs'"

    * Generate the z-score variable name
    local z_var_name "z_`chart'_`version'_`measure_var'_`xvar_var'"

    * Replace problematic characters in variable name
    local z_var_name = subinstr("`z_var_name'", "_cm", "cm", .)
    local z_var_name = subinstr("`z_var_name'", "_kg", "kg", .)
    local z_var_name = subinstr("`z_var_name'", "_mm", "mm", .)
    local z_var_name = subinstr("`z_var_name'", "_percent", "pct", .)
    local z_var_name = subinstr("`z_var_name'", "_years", "yrs", .)

    * Ensure variable name isn't too long (Stata limit is 32 characters)
    if length("`z_var_name'") > 32 {
        local z_var_name = substr("`z_var_name'", 1, 32)
    }

    display "  -> Creating variable: `z_var_name'"

    * Generate z-scores using zanthro
    capture {
        if "`chart'" == "wl" | "`chart'" == "wh" {
            * Weight-for-length/height charts (no ageunit needed)
            egen `z_var_name' = zanthro(`measure_var',`chart',`version'), ///
                xvar(`xvar_var') gender(gender) gencode(male=1, female=2)
        }
        else {
            * Age-based charts
            egen `z_var_name' = zanthro(`measure_var',`chart',`version'), ///
                xvar(`xvar_var') gender(gender) gencode(male=1, female=2) ageunit(year)
        }
    }

    if _rc != 0 {
        display "  -> ERROR: Failed to generate z-scores for `combo'"
        display "  -> Error code: " _rc
    }
    else {
        * Count successful z-score calculations
        count if !missing(`z_var_name')
        local z_count = r(N)
        display "  -> Successfully calculated `z_count' z-scores"

        * Show some summary statistics
        summarize `z_var_name', detail
        display "  -> Mean: " r(mean) ", SD: " r(sd) ", Range: [" r(min) ", " r(max) "]"
    }

    display ""
}

* Display summary of all created z-score variables
display "Summary of all generated z-score variables:"
describe z_*

* Count total z-score variables created
local z_var_count = 0
foreach var of varlist z_* {
    local z_var_count = `z_var_count' + 1
}
display ""
display "Total z-score variables created: `z_var_count'"

* Save the enhanced dataset
export delimited using "zanthror_testdata_compare.csv", replace

display ""
display "Results saved to: zanthror_testdata_compare.csv"
display "Dataset now contains original variables plus z-score results from all valid combinations"

* Create a summary table of results
display ""
display "Summary table of z-score variables and their validity:"
display "{hline 70}"
display "Variable Name" _col(35) "Valid N" _col(45) "Mean" _col(55) "SD" _col(65) "Range"
display "{hline 70}"

foreach var of varlist z_* {
    quietly summarize `var'
    local valid_n = r(N)
    local mean = r(mean)
    local sd = r(sd)
    local min = r(min)
    local max = r(max)

    display "`var'" _col(35) %8.0f `valid_n' _col(45) %8.2f `mean' _col(55) %6.2f `sd' _col(65) "[" %5.2f `min' "," %5.2f `max' "]"
}
display "{hline 70}"

display ""
display "Test completed successfully!"
display "You can now import zanthror_testdata_compare.csv into R for comparison testing."
