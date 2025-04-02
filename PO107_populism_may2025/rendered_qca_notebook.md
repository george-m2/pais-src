QCA Analysis of Democratic Erosion Relative to Populist Movements in
Europe (2000-2020)
================
George Maycock
2025-04-02

This notebook performs Qualitative Comparative Analysis (QCA) to explore
the conditions associated with democratic erosion in a set of European
countries between 2000 and 2020.

## 1. Setup

### Libraries

``` r
# install.packages(c("QCA", "SetMethods", "tidyverse", "vdemdata", "countrycode"))
library(QCA)
library(SetMethods)
library(tidyverse)
library(vdemdata)
library(countrycode)
```

Next, we define the core parameters for the analysis: the countries
included (cases), the overall time period, and specific time windows for
calculating the outcome variable. This outcome is defined as democratic
erosion based on the V-Dem Liberal Democracy Index.

``` r
case_countries <- c("Austria", "Belgium", "Czech Republic", "Denmark", "France",
                    "Greece", "Hungary", "Italy", "Netherlands", "Poland",
                    "Slovakia", "Spain", "Sweden", "Switzerland", "United Kingdom")

start_year_period <- 2000
end_year_period <- 2020
start_years_ldi_start <- 2000:2002 # Start period for LDI baseline
end_years_ldi_end <- 2018:2020   # End period
```

We load World Bank external data for the Gini coefficient, which will be
used as one of the conditions.

``` r
gini_file <- "src/gini.csv"
if (!file.exists(gini_file)) stop("Gini data file not found at: ", gini_file)
gini_raw <- read_csv(gini_file, show_col_types = FALSE)
```

Now, we load the comprehensive V-Dem dataset from vdemdata::vdem

``` r
cat("Loading V-Dem data...\n")
```

    ## Loading V-Dem data...

``` r
vdem_full <- tryCatch({ vdemdata::vdem }, error = function(e) {
    stop("Failed to load V-Dem data. Error: ", e$message)
})
cat("V-Dem data loaded.\n")
```

    ## V-Dem data loaded.

## 2. Data cleaning

We begin by standardising the country names to ensure consistency
between different data sources (V-Dem/ WB Gini dataset)

``` r
case_countries_std <- countrycode(case_countries, origin = 'country.name', destination = 'country.name')

# Handle potential NAs from countrycode conversion
if(any(is.na(case_countries_std))) {
  failed_countries <- case_countries[is.na(case_countries_std)]
  warning("Could not standardise names for: ", paste(failed_countries, collapse=", "), ". These countries will be excluded if standardisation failed.")
  case_countries_std <- case_countries_std[!is.na(case_countries_std)] # keep only successfully standardised names
}
if(length(case_countries_std) == 0) stop("No case countries could be standardized.")

print(case_countries_std)
```

    ##  [1] "Austria"        "Belgium"        "Czechia"        "Denmark"       
    ##  [5] "France"         "Greece"         "Hungary"        "Italy"         
    ##  [9] "Netherlands"    "Poland"         "Slovakia"       "Spain"         
    ## [13] "Sweden"         "Switzerland"    "United Kingdom"

``` r
# Create a tibble to hold data for our cases
country_data <- tibble(country_name_std = case_countries_std)
```

### 2a. Calculate EROSION outcome

The outcome variable, `EROSION`, measures the degree of democratic
erosion. We operationalise this as the difference in the V-Dem Liberal
Democracy Index (`v2x_libdem`) between an early period (average of
2000-2002) and a later period (average of 2018-2020). A higher score
indicates greater erosion (a larger drop in the LDI).

``` r
cat("Calculating EROSION...\n")
```

    ## Calculating EROSION...

``` r
ldi_data <- vdem_full %>%
  filter(year >= min(start_years_ldi_start) & year <= max(end_years_ldi_end)) %>% # flter years 
  mutate(country_name_std = countrycode(country_name, origin = 'country.name', destination = 'country.name')) %>% # mutate and standardise country names
  filter(country_name_std %in% case_countries_std, !is.na(country_name_std)) %>% # filter countries
  select(country_name_std, year, LDI = v2x_libdem) %>%
  na.omit()

# Average LDI at the start period
ldi_start_avg <- ldi_data %>%
  filter(year %in% start_years_ldi_start) %>%
  group_by(country_name_std) %>%
  summarise(LDI_start = mean(LDI, na.rm = TRUE), n_start = n(), .groups = "drop")

# End period
ldi_end_avg <- ldi_data %>%
  filter(year %in% end_years_ldi_end) %>%
  group_by(country_name_std) %>%
  summarise(LDI_end = mean(LDI, na.rm = TRUE), n_end = n(), .groups = "drop")

# raw erosion score (start LDI - end LDI)
erosion_calc <- ldi_start_avg %>%
  inner_join(ldi_end_avg, by = "country_name_std") %>%
  filter(n_start > 0, n_end > 0) %>% # does data exist for both periods?
  mutate(EROSION_raw = LDI_start - LDI_end) %>% # higher score, greater degree of erosion
  select(country_name_std, EROSION_raw)

# Add erosion score to dataset
country_data <- country_data %>% left_join(erosion_calc, by = "country_name_std")
cat("Outcome variable 'EROSION_raw' calculated.\n")
```

    ## Outcome variable 'EROSION_raw' calculated.

### 2b. Calculate conditions

Now we calculate the potential explanatory conditions for democratic
erosion. These are averaged over the full period (2000-2020).

#### Condition 1: INST

This condition represents the strength of legislative constraints on the
executive, using `v2xlg_legcon` and averaged over the period for each
country. Higher values indicate stronger constraints

``` r
cat("Calculating INST...\n")
```

    ## Calculating INST...

``` r
inst_constraints <- vdem_full %>%
  filter(year >= start_year_period & year <= end_year_period) %>%
  mutate(country_name_std = countrycode(country_name, origin = 'country.name', destination = 'country.name')) %>%
  filter(country_name_std %in% case_countries_std, !is.na(country_name_std)) %>%
  group_by(country_name_std) %>%
  summarise(INST_raw = mean(v2xlg_legcon, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(INST_raw)) # remove NaN countries
country_data <- country_data %>% left_join(inst_constraints, by = "country_name_std")
cat("Condition 'INST_raw' calculated.\n")
```

    ## Condition 'INST_raw' calculated.

#### Condition 2: GINI

Average Gini coefficient over the period for each country.

``` r
# suppress warnings from countrycode here as we handle NAs later
cat("Calculating: GINI...\n")
```

    ## Calculating: GINI...

``` r
gini_avg <- gini_raw %>%
  select(country_raw = Entity, year = Year, gini = `Gini coefficient`) %>%
  filter(year >= start_year_period & year <= end_year_period) %>%
  mutate(country_name_std = suppressWarnings(countrycode(country_raw, origin = 'country.name', destination = 'country.name'))) %>%
  filter(country_name_std %in% case_countries_std, !is.na(country_name_std)) %>%
  group_by(country_name_std) %>%
  summarise(GINI_raw = mean(gini, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(GINI_raw)) 
country_data <- country_data %>% left_join(gini_avg, by = "country_name_std")
cat("Condition 'GINI_raw' calculated.\n")
```

    ## Condition 'GINI_raw' calculated.

#### Condition 3: POL

Degree of political polarisation, derived from `v2cacamps` and averaged
over the period. Higher values indicate greater polarisation.

``` r
cat("Calculating: POL...\n")
```

    ## Calculating: POL...

``` r
polarisation <- vdem_full %>%
  filter(year >= start_year_period & year <= end_year_period) %>%
  mutate(country_name_std = countrycode(country_name, origin = 'country.name', destination = 'country.name')) %>%
  filter(country_name_std %in% case_countries_std, !is.na(country_name_std)) %>%
  group_by(country_name_std) %>%
  summarise(POL_raw = mean(v2cacamps, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(POL_raw)) 
country_data <- country_data %>% left_join(polarisation, by = "country_name_std")
cat("Condition 'POL_raw' calculated.\n")
```

    ## Condition 'POL_raw' calculated.

#### Condition 4: JUDATTACK_comp

This condition aims to capture the extent to which the judiciary is
undermined or attacked. It’s a compound index created from several V-Dem
variables: \* `v2juhcind`: High court independence (inverted) \*
`v2juncind`: Lower court independence (inverted) \* `v2jucomp`: Judicial
compliance with the executive (inverted) \* `v2jupoatck`: Government
attacks on the judiciary (rescaled 0-1)

We invert the independence/compliance variables so that higher scores
mean *less* independence/compliance. We rescale the attack variable to
0-1 (higher = more attacks). The final score is the average of these
four transformed components, averaged over the period 2000-2020.

``` r
cat("Calculating 'JUDATTACK_comp_raw'...\n")
```

    ## Calculating 'JUDATTACK_comp_raw'...

``` r
judicial_vars <- c("v2juhcind", "v2juncind", "v2jucomp", "v2jupoatck")

# Filter data and prepare components
vdem_filtered_jud <- vdem_full %>%
  filter(year >= start_year_period & year <= end_year_period) %>%
  mutate(country_name_std = countrycode(country_name, origin = 'country.name', destination = 'country.name')) %>%
  filter(country_name_std %in% case_countries_std, !is.na(country_name_std)) %>%
  select(country_name_std, year, all_of(judicial_vars)) %>%
  # Remove rows where *any* of the selected judicial variables are NA for a given year
  na.omit()

# Check if data remains after NA removal
if (nrow(vdem_filtered_jud) == 0) {
    warning("No complete judicial data found for the specified countries and years after removing NAs. Skipping JUDATTACK_comp.")
    country_data$JUDATTACK_comp_raw <- NA_real_ 
} else {
    # Find observed min/max for v2jupoatck for rescaling
    actual_min_jupoatck <- min(vdem_filtered_jud$v2jupoatck, na.rm = TRUE)
    actual_max_jupoatck <- max(vdem_filtered_jud$v2jupoatck, na.rm = TRUE)
    cat(paste0("Observed range for v2jupoatck (Govt Attacks): [",
               round(actual_min_jupoatck, 2), ", ", round(actual_max_jupoatck, 2), "]\n"))
    # If min == max, rescaling is impossible. Add a small epsilon to max.
    if (actual_max_jupoatck == actual_min_jupoatck) {
        actual_max_jupoatck <- actual_max_jupoatck + 1e-6
        warning("Min and Max of v2jupoatck are identical. Added epsilon for rescaling.")
    }

    # Transform variables: invert independence/compliance, rescale attacks
    judicial_transformed <- vdem_filtered_jud %>%
      mutate(
        # Invert independence and compliance (higher score = WORSE situation)
        inv_v2juhcind = 1 - v2juhcind,
        inv_v2juncind = 1 - v2juncind,
        inv_v2jucomp = 1 - v2jucomp,
        # Rescale v2jupoatck to 0-1 (higher score = worse situation)
        # ...clamp between 0 and 1
        rescaled_v2jupoatck = (v2jupoatck - actual_min_jupoatck) / (actual_max_jupoatck - actual_min_jupoatck),
        rescaled_v2jupoatck = pmax(0, pmin(1, rescaled_v2jupoatck)) # Ensure strictly within [0, 1]
      ) %>%
      select(country_name_std, year, starts_with("inv_"), starts_with("rescaled_"))

    # Calculate the average of each transformed component over the period for each country
    judicial_compound_avg <- judicial_transformed %>%
      group_by(country_name_std) %>%
      summarise(
        mean_inv_juhcind = mean(inv_v2juhcind, na.rm = TRUE),
        mean_inv_juncind = mean(inv_v2juncind, na.rm = TRUE),
        mean_inv_jucomp = mean(inv_v2jucomp, na.rm = TRUE),
        mean_rescaled_jupoatck = mean(rescaled_v2jupoatck, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Calculate the final compound score by averaging the means of the components
      rowwise() %>%
      mutate(JUDATTACK_comp_raw = mean(c(mean_inv_juhcind, mean_inv_juncind, mean_inv_jucomp, mean_rescaled_jupoatck), na.rm = TRUE)) %>%
      ungroup() %>%
      select(country_name_std, JUDATTACK_comp_raw) %>%
      filter(!is.nan(JUDATTACK_comp_raw)) # Ensure final score is not NaN

    # Join the compound score back to the main data
    country_data <- country_data %>% left_join(judicial_compound_avg, by = "country_name_std")
    cat("Condition 'JUDATTACK_comp_raw' calculated.\n")
}
```

    ## Observed range for v2jupoatck (Govt Attacks): [-4.42, 2.67]
    ## Condition 'JUDATTACK_comp_raw' calculated.

## 3. Final Data Preparation & Calibration

We now consolidate the calculated outcome and conditions into a final
dataset for QCA. We remove any countries (cases) that have missing
values (`NA`) for any of the selected variables.

``` r
# Select the final set of raw variables for QCA
qca_final_cs <- country_data %>%
  select(country_name_std, EROSION_raw, INST_raw, GINI_raw, POL_raw, JUDATTACK_comp_raw) %>%
  na.omit() # Remove rows with NA in *any* selected column

# Report dropped countries and final N
dropped_countries <- setdiff(case_countries_std, qca_final_cs$country_name_std)
if (length(dropped_countries) > 0) {
    cat("Warning: Countries dropped due to missing values in outcome or conditions:\n", paste(dropped_countries, collapse=", "), "\n")
}
cat(sprintf("Initial countries: %d. Countries remaining after removing NAs: %d.\n", length(case_countries_std), nrow(qca_final_cs)))
```

    ## Initial countries: 15. Countries remaining after removing NAs: 15.

``` r
# Are there enough cases?
if (nrow(qca_final_cs) == 0) stop("No complete cases remaining.")
if (nrow(qca_final_cs) < 5) warning("Only ", nrow(qca_final_cs), " cases remain. QCA results may be unreliable.")

# Prepare for calibration (remove country name, set as rownames)
# Suppress warning about rownames on tibble, it's expected here and will shout about depreciation, but works for QCA package
qca_final_cs_for_calib <- suppressWarnings({
  df_temp <- qca_final_cs %>% select(-country_name_std)
  rownames(df_temp) <- qca_final_cs$country_name_std
  df_temp
})


cat("\nFinal raw data for calibration:\n")
```

    ## 
    ## Final raw data for calibration:

``` r
print(qca_final_cs_for_calib)
```

    ## # A tibble: 15 × 5
    ##    EROSION_raw INST_raw GINI_raw POL_raw JUDATTACK_comp_raw
    ##  *       <dbl>    <dbl>    <dbl>   <dbl>              <dbl>
    ##  1   -0.000333    0.926    0.302  -0.495           -0.485  
    ##  2   -0.0160      0.907    0.283  -1.85            -0.558  
    ##  3    0.0910      0.894    0.261  -1.73            -0.326  
    ##  4   -0.00800     0.981    0.269  -3.00            -1.32   
    ##  5    0.00633     0.880    0.320   0.111           -0.466  
    ##  6    0.0690      0.886    0.344  -1.09            -0.00922
    ##  7    0.389       0.739    0.299   1.34            -0.132  
    ##  8   -0.0307      0.938    0.347   0.229           -0.236  
    ##  9   -0.00600     0.973    0.285  -1.31            -0.508  
    ## 10    0.313       0.847    0.326   1.53            -0.635  
    ## 11    0.0103      0.854    0.260  -0.754           -0.0652 
    ## 12    0.0220      0.825    0.346  -1.17            -0.671  
    ## 13    0.00367     0.974    0.278  -2.10            -1.05   
    ## 14   -0.0247      0.926    0.328  -1.75            -1.14   
    ## 15   -0.00467     0.954    0.343  -0.630           -0.448

### Calibration

<https://bookdown.org/dusadrian/QCAbook/calibration.html>

(fs)QCA requires variables to be calibrated into set memberships
(ranging from 0 to 1). We use the direct method of calibration with
three qualitative anchors: full non-membership (0.05), the crossover
point (0.50), and full membership (0.95). We estimate these anchors
using quantiles of the raw data distribution (specifically, the 25th,
50th, and 75th percentiles, with fallbacks for variables with low
variation).

``` r
# quantile-based thresholds
get_thresholds_string <- function(x, name = deparse(substitute(x)), probs = c(0.25, 0.5, 0.75), na.rm = TRUE) { 
    var_name <- name; unique_vals <- unique(na.omit(x)); quant_probs_labels <- paste0(probs * 100, "%") # "25% 50% 75%"
    if (length(unique_vals) < 3) {
        # Handle variables with very few unique values
        warning(paste0("Variable '", var_name, "' has < 3 unique values. Using min/median/max for thresholds."), call. = FALSE)
        q <- quantile(x, probs = c(0, 0.5, 1), na.rm = na.rm, type = 8); quant_probs_labels <- c("Min", "Median", "Max") # 0% 50% 100%
        sd_x <- sd(x, na.rm=TRUE); if (is.na(sd_x) || sd_x == 0) sd_x <- 1e-6 # Avoid division by zero SD 
        # Ensure thresholds are distinct, adding small noise if needed
        if(q[1]==q[2] && q[2]==q[3]) { q[1] <- q[1] - 1e-9; q[3] <- q[3] + 1e-9}
        else {
           if (q[3] <= q[2]) q[3] <- q[2] + sd_x * 0.01 + 1e-9
           if (q[2] <= q[1]) q[1] <- q[2] - sd_x * 0.01 - 1e-9
        }
         # Fallback if initial adjustment failed
         if(q[1] >= q[2] || q[2] >= q[3]) {
             q <- sort(quantile(x, probs=c(0.1, 0.5, 0.9), na.rm=TRUE, type=8)); quant_probs_labels <- c("10%", "50%", "90%")
             if(length(unique(q))<3) q <- c(min(x, na.rm=T), median(x, na.rm=T), max(x, na.rm=T)); quant_probs_labels <- c("Min", "Median", "Max")
             if (q[3] <= q[2]) q[3] <- q[2] + 1e-9
             if (q[2] <= q[1]) q[1] <- q[2] - 1e-9
             if(q[1] == q[2] && q[2] == q[3]) { q[1] <- q[1] - 1e-9; q[3] <- q[3] + 1e-9}
         }
    } else {
        # Standard case: use specified quantiles
        q <- quantile(x, probs = probs, na.rm = na.rm, type = 8)
        sd_x <- sd(x, na.rm=TRUE); if (is.na(sd_x) || sd_x == 0) sd_x <- 1e-6
        # Ensure thresholds are distinct
        if (q[3] <= q[2]) q[3] <- q[2] + sd_x*0.01 + 1e-9
        if (q[2] <= q[1]) q[1] <- q[2] - sd_x*0.01 - 1e-9
        # Fallback if adjustment failed
        if(q[1] >= q[2] || q[2] >= q[3]){
             warning(paste0("Could not ensure distinct quantile thresholds for '", var_name, "'. Using 10/50/90 percentiles as fallback."), call.=FALSE)
             q <- sort(quantile(x, probs=c(0.1, 0.5, 0.9), na.rm=TRUE, type=8)); quant_probs_labels <- c("10%", "50%", "90%")
             if(length(unique(q))<3) q <- c(min(x, na.rm=T), median(x, na.rm=T), max(x, na.rm=T)); quant_probs_labels <- c("Min", "Median", "Max")
             if (q[3] <= q[2]) q[3] <- q[2] + 1e-9
             if (q[2] <= q[1]) q[1] <- q[2] - 1e-9
             if(q[1] == q[2] && q[2] == q[3]) { q[1] <- q[1] - 1e-9; q[3] <- q[3] + 1e-9}
        }
    }
    # Format thresholds as string "full_membership,crossover,full_non_membership"
    thresholds_str <- paste(format(q[3], digits = 3, nsmall=3), format(q[2], digits = 3, nsmall=3), format(q[1], digits = 3, nsmall=3), sep = ",")
    cat(var_name, "Calibration thresholds (", paste0(quant_probs_labels, collapse="/"), " for full_memb/cross/non_memb):", thresholds_str, "\n")
    return(thresholds_str)
}
```

Now we apply the QCA calibration function to each raw variable to
generate fuzzy set scores.

``` r
cat("--- Calibrating Data ---\n")
```

    ## --- Calibrating Data ---

``` r
qca_calibrated_data_cs <- qca_final_cs_for_calib

# EROSION
qca_calibrated_data_cs$fEROSION <- calibrate(qca_calibrated_data_cs$EROSION_raw, type = "fuzzy", thresholds = get_thresholds_string(qca_calibrated_data_cs$EROSION_raw, name="EROSION_raw"))
```

    ## EROSION_raw Calibration thresholds ( 25%/50%/75%  for full_memb/cross/non_memb): 0.0612,0.00367,-0.00767

``` r
# Calibrate conditions
qca_calibrated_data_cs$fINST <- calibrate(qca_calibrated_data_cs$INST_raw, type = "fuzzy", thresholds = get_thresholds_string(qca_calibrated_data_cs$INST_raw, name="INST_raw"))
```

    ## INST_raw Calibration thresholds ( 25%/50%/75%  for full_memb/cross/non_memb): 0.951,0.907,0.858

``` r
qca_calibrated_data_cs$fGINI <- calibrate(qca_calibrated_data_cs$GINI_raw, type = "fuzzy", thresholds = get_thresholds_string(qca_calibrated_data_cs$GINI_raw, name="GINI_raw"))
```

    ## GINI_raw Calibration thresholds ( 25%/50%/75%  for full_memb/cross/non_memb): 0.341,0.302,0.278

``` r
qca_calibrated_data_cs$fPOL <- calibrate(qca_calibrated_data_cs$POL_raw, type = "fuzzy", thresholds = get_thresholds_string(qca_calibrated_data_cs$POL_raw, name="POL_raw"))
```

    ## POL_raw Calibration thresholds ( 25%/50%/75%  for full_memb/cross/non_memb): 0.0103,-1.090,-1.750

``` r
qca_calibrated_data_cs$fJUDATTACK_comp <- calibrate(qca_calibrated_data_cs$JUDATTACK_comp_raw, type = "fuzzy", thresholds = get_thresholds_string(qca_calibrated_data_cs$JUDATTACK_comp_raw, name="JUDATTACK_comp_raw"))
```

    ## JUDATTACK_comp_raw Calibration thresholds ( 25%/50%/75%  for full_memb/cross/non_memb): -0.251,-0.485,-0.665

``` r
# Define the final set of *calibrated* conditions used in the analysis
conditions_cs <- c("fINST", "fGINI", "fPOL", "fJUDATTACK_comp")

# Select final calibrated columns
qca_calibrated_data_cs_final <- qca_calibrated_data_cs %>% select(starts_with("f"))

print("Summary of final calibrated data:")
```

    ## [1] "Summary of final calibrated data:"

``` r
print(summary(qca_calibrated_data_cs_final))
```

    ##     fEROSION          fINST             fGINI              fPOL          
    ##  Min.   :0.0000   Min.   :0.00702   Min.   :0.03327   Min.   :0.0008939  
    ##  1st Qu.:0.1577   1st Qu.:0.07548   1st Qu.:0.08074   1st Qu.:0.1037959  
    ##  Median :0.5002   Median :0.49761   Median :0.50048   Median :0.4999363  
    ##  Mean   :0.5477   Mean   :0.48900   Mean   :0.49602   Mean   :0.4945585  
    ##  3rd Qu.:0.9394   3rd Qu.:0.89627   3rd Qu.:0.93080   3rd Qu.:0.9486561  
    ##  Max.   :0.9999   Max.   :1.00000   Max.   :0.99443   Max.   :0.9997999  
    ##  fJUDATTACK_comp   
    ##  Min.   :0.002506  
    ##  1st Qu.:0.080238  
    ##  Median :0.500794  
    ##  Mean   :0.515952  
    ##  3rd Qu.:0.937928  
    ##  Max.   :1.000000

``` r
# Convert to df and ensure rownames are set for QCA functions
qca_calibrated_data_cs_df <- as.data.frame(qca_calibrated_data_cs_final)
rownames(qca_calibrated_data_cs_df) <- rownames(qca_final_cs_for_calib)
```

## 4. QCA analysis (Cross-Sectional)

With the data calibrated, we proceed to the QCA analysis, starting with
the analysis of necessity.

``` r
cat("--- QCA Analysis ---\n")
```

    ## --- QCA Analysis ---

``` r
if (nrow(qca_calibrated_data_cs_df) < 5) stop("Too few valid cases remaining for QCA analysis.")
```

### 4a. Necessity analysis

We test whether any individual condition (or its absence) is necessary
for the outcome (democratic erosion, `fEROSION`) or its absence
(`~fEROSION`). We use an inclusion threshold (`incl.cut`) of 0.9 and a
coverage threshold (`cov.cut`) of 0.5.

``` r
cat("--- Necessity Analysis for fEROSION ---\n")
```

    ## --- Necessity Analysis for fEROSION ---

``` r
# Check if outcome exists and has variation before proceeding
if (!"fEROSION" %in% names(qca_calibrated_data_cs_df) || var(qca_calibrated_data_cs_df$fEROSION, na.rm=TRUE) == 0) {
    cat("Skipping Necessity for fEROSION: Outcome missing or has no variation.\n")
} else {
    necessity_results_cs <- tryCatch(
        superSubset(qca_calibrated_data_cs_df, outcome = "fEROSION", incl.cut = 0.9, cov.cut = 0.5, relation = "necessity", conditions = conditions_cs), # calculate necessity
        error = function(e) { cat("Error in Necessity Analysis for fEROSION:", e$message, "\n"); NULL }
    )
    if (!is.null(necessity_results_cs) && nrow(necessity_results_cs$incl.cov) > 0) {
        print(necessity_results_cs)
    } else {
        cat("No necessary conditions found for fEROSION (Incl. >= 0.9, Cov >= 0.5).\n")
    }
}
```

    ## 
    ##                                        inclN   RoN   covN  
    ## ---------------------------------------------------------- 
    ## 1  ~fINST + ~fPOL                      0.910  0.411  0.628 
    ## 2  ~fINST + fPOL                       0.926  0.670  0.757 
    ## 3  ~fINST + fJUDATTACK_comp            0.910  0.626  0.726 
    ## 4  ~fGINI + fPOL                       0.918  0.366  0.615 
    ## 5  ~fPOL + fJUDATTACK_comp             0.914  0.376  0.616 
    ## 6  ~fINST + ~fGINI + ~fJUDATTACK_comp  0.922  0.195  0.559 
    ## 7  ~fINST + fGINI + ~fJUDATTACK_comp   0.929  0.397  0.632 
    ## 8  fGINI + fPOL + ~fJUDATTACK_comp     0.919  0.385  0.622 
    ## ----------------------------------------------------------

``` r
cat("--- Necessity Analysis for Outcome ~fEROSION (Absence) ---\n")
```

    ## --- Necessity Analysis for Outcome ~fEROSION (Absence) ---

``` r
# negated outcome
qca_calibrated_data_cs_df$negEROSION <- 1 - qca_calibrated_data_cs_df$fEROSION

# Check if negated outcome exists and has variation
if (!"negEROSION" %in% names(qca_calibrated_data_cs_df) || var(qca_calibrated_data_cs_df$negEROSION, na.rm=TRUE) == 0) {
    cat("Skipping Necessity for ~fEROSION: Negated outcome missing or has no variation.\n")
} else {
    necessity_neg_results_cs <- tryCatch(
        superSubset(qca_calibrated_data_cs_df, outcome = "negEROSION", incl.cut = 0.9, cov.cut = 0.5, relation = "necessity", conditions = conditions_cs),
        error = function(e) { cat("Error in Necessity Analysis for ~fEROSION:", e$message, "\n"); NULL }
    )
    if (!is.null(necessity_neg_results_cs) && nrow(necessity_neg_results_cs$incl.cov) > 0) {
        print(necessity_neg_results_cs)
    } else {
        cat("No necessary conditions found for ~fEROSION (Incl. >= 0.9, Cov >= 0.5).\n")
    }
}
```

    ## 
    ##                                       inclN   RoN   covN  
    ## --------------------------------------------------------- 
    ## 1  fINST + fGINI                      0.959  0.451  0.582 
    ## 2  fINST + fPOL                       0.955  0.392  0.556 
    ## 3  fINST + ~fJUDATTACK_comp           0.903  0.576  0.619 
    ## 4  fINST + fJUDATTACK_comp            0.924  0.392  0.541 
    ## 5  ~fGINI + ~fPOL + ~fJUDATTACK_comp  0.913  0.438  0.556 
    ## 6  fGINI + ~fPOL + ~fJUDATTACK_comp   0.955  0.259  0.506 
    ## ---------------------------------------------------------

### 4b. Sufficiency analysis

Next, we perform the sufficiency analysis to identify combinations of
conditions that are sufficient for the outcome (or its absence). We
first generate a truth table and then minimize it to find the solution
formulas. We use an inclusion threshold of 0.80 and require at least 1
case per configuration (`n.cut = 1`).

#### Sufficiency for `fEROSION`

``` r
cat("--- Sufficiency Analysis for Outcome fEROSION ---\n")
```

    ## --- Sufficiency Analysis for Outcome fEROSION ---

``` r
# Check if outcome exists and has variation
if (!"fEROSION" %in% names(qca_calibrated_data_cs_df) || var(qca_calibrated_data_cs_df$fEROSION, na.rm=TRUE) == 0) {
    cat("Skipping Sufficiency for fEROSION: Outcome missing or has no variation.\n")
} else {
    # Create Truth Table
    tt_erosion <- tryCatch(
        truthTable(qca_calibrated_data_cs_df, outcome = "fEROSION", conditions = conditions_cs,
                   incl.cut = 0.80, n.cut = 1, show.cases = TRUE, sort.by = c("incl", "n"), decreasing = TRUE),
        error = function(e) { cat("Error creating Truth Table for fEROSION:", e$message, "\n"); NULL }
    )

    if (!is.null(tt_erosion) && nrow(tt_erosion$tt) > 0) {
        cat("\n--- Truth Table (fEROSION, Incl >= 0.80, N >= 1) ---\n")
        print(tt_erosion)
        cat("\n--- Solutions for Outcome fEROSION ---\n")

        # Conservative solution (CS)
        sol_c_erosion <- tryCatch(
             minimize(tt_erosion, details = TRUE, method = "eQMC", show.cases = TRUE),
             error = function(e) { cat("Error minimizing (Conservative) for fEROSION:", e$message, "\n"); NULL }
        )
        if (!is.null(sol_c_erosion)) {
            cat("\n--- Conservative Solution (fEROSION) ---\n")
            print(sol_c_erosion)
        } else {
             cat("Conservative solution could not be generated for fEROSION.\n")
        }

        # Parsimonious solution (PS)
        sol_p_erosion <- tryCatch(
             minimize(tt_erosion, details = TRUE, method = "eQMC", include = "?", show.cases = TRUE),
             error = function(e) { cat("Error minimizing (Parsimonious) for fEROSION:", e$message, "\n"); NULL }
        )
        if (!is.null(sol_p_erosion)) {
            cat("\n--- Parsimonious Solution (fEROSION) ---\n")
            print(sol_p_erosion)
        } else {
            cat("Parsimonious solution could not be generated for fEROSION.\n")
        }

        # Intermediate Solution (Requires Theoretical Directional Expectations)
        cat("\n--- Intermediate Solution (fEROSION - Requires dir.exp) ---\n")
        cat("To generate the Intermediate Solution, define directional expectations ('dir.exp') based on theory.\n")
        cat("Example: dir_exp_erosion <- c(fINST=\"0\", fGINI=\"1\", fPOL=\"1\", fJUDATTACK_comp=\"1\") # Expect erosion with low INST, high GINI, POL, JUDATTACK\n")
        cat("# Then run: minimize(tt_erosion, details=TRUE, method=\"eQMC\", include=\"?\", dir.exp = dir_exp_erosion, show.cases=TRUE)\n")

    } else {
        cat("Skipping minimization for fEROSION: Truth table is empty or could not be generated (check inclusion/N cuts and data variation).\n")
    }
}
```

    ## 
    ## --- Truth Table (fEROSION, Incl >= 0.80, N >= 1) ---
    ## 
    ##   OUT: output value
    ##     n: number of cases in configuration
    ##  incl: sufficiency inclusion score
    ##   PRI: proportional reduction in inconsistency
    ## 
    ##      fINST fGINI fPOL fJUDATTACK_comp   OUT    n  incl  PRI  
    ##  4     0     0    1          1           1     1  0.999 0.999
    ##  6     0     1    0          1           1     1  0.943 0.889
    ##  1     0     0    0          0           1     2  0.917 0.888
    ##  8     0     1    1          1           1     4  0.834 0.778
    ## 12     1     0    1          1           0     1  0.749 0.521
    ## 15     1     1    1          0           0     1  0.566 0.271
    ## 10     1     0    0          1           0     1  0.530 0.105
    ## 13     1     1    0          0           0     2  0.502 0.055
    ##  9     1     0    0          0           0     2  0.482 0.116
    ##      cases                             
    ##  4   Switzerland                       
    ##  6   Austria                           
    ##  1   Italy,United Kingdom              
    ##  8   Belgium,Denmark,Netherlands,Sweden
    ## 12   Spain                             
    ## 15   Czechia                           
    ## 10   Poland                            
    ## 13   Hungary,Slovakia                  
    ##  9   France,Greece                     
    ## 
    ## 
    ## --- Solutions for Outcome fEROSION ---
    ## 
    ## --- Conservative Solution (fEROSION) ---
    ## 
    ## M1: ~fINST*fGINI*fJUDATTACK_comp + ~fINST*fPOL*fJUDATTACK_comp +
    ##     ~fINST*~fGINI*~fPOL*~fJUDATTACK_comp -> fEROSION
    ## 
    ##                                          inclS   PRI   covS   covU  
    ## ------------------------------------------------------------------- 
    ## 1          ~fINST*fGINI*fJUDATTACK_comp  0.848  0.790  0.418  0.000 
    ## 2           ~fINST*fPOL*fJUDATTACK_comp  0.857  0.818  0.464  0.080 
    ## 3  ~fINST*~fGINI*~fPOL*~fJUDATTACK_comp  0.917  0.888  0.288  0.156 
    ## ------------------------------------------------------------------- 
    ##                                      M1  0.868  0.836  0.677 
    ## 
    ##                                          cases 
    ## ---------------------------------------------- 
    ## 1          ~fINST*fGINI*fJUDATTACK_comp  Austria; Belgium,Denmark,Netherlands,Sweden
    ## 2           ~fINST*fPOL*fJUDATTACK_comp  Switzerland; Belgium,Denmark,Netherlands,Sweden
    ## 3  ~fINST*~fGINI*~fPOL*~fJUDATTACK_comp  Italy,United Kingdom
    ## ---------------------------------------------- 
    ## 
    ## 
    ## --- Parsimonious Solution (fEROSION) ---
    ## 
    ## M1: ~fINST -> fEROSION
    ## 
    ##            inclS   PRI   covS   covU  
    ## ------------------------------------- 
    ## 1  ~fINST  0.841  0.810  0.785    -   
    ## ------------------------------------- 
    ##        M1  0.841  0.810  0.785 
    ## 
    ##            cases 
    ## ---------------- 
    ## 1  ~fINST  Italy,United Kingdom; Switzerland; Austria; Belgium,Denmark,Netherlands,Sweden
    ## ---------------- 
    ## 
    ## 
    ## --- Intermediate Solution (fEROSION - Requires dir.exp) ---
    ## To generate the Intermediate Solution, define directional expectations ('dir.exp') based on theory.
    ## Example: dir_exp_erosion <- c(fINST="0", fGINI="1", fPOL="1", fJUDATTACK_comp="1") # Expect erosion with low INST, high GINI, POL, JUDATTACK
    ## # Then run: minimize(tt_erosion, details=TRUE, method="eQMC", include="?", dir.exp = dir_exp_erosion, show.cases=TRUE)

#### Sufficiency for `~fEROSION`

Now we repeat the sufficiency analysis for the absence of democratic
erosion (`negEROSION`).

``` r
cat("--- Sufficiency Analysis for Outcome ~fEROSION ---\n")
```

    ## --- Sufficiency Analysis for Outcome ~fEROSION ---

``` r
# Check if negated outcome exists and has variation
if (!"negEROSION" %in% names(qca_calibrated_data_cs_df) || var(qca_calibrated_data_cs_df$negEROSION, na.rm=TRUE) == 0) {
    cat("Skipping Sufficiency for ~fEROSION: Negated outcome missing or has no variation.\n")
} else {
    # Truth table calculation
    tt_neg_erosion <- tryCatch(
        truthTable(qca_calibrated_data_cs_df, outcome = "negEROSION", conditions = conditions_cs,
                   incl.cut = 0.80, n.cut = 1, show.cases = TRUE, sort.by = c("incl", "n"), decreasing = TRUE),
        error = function(e) { cat("Error creating Truth Table for ~fEROSION:", e$message, "\n"); NULL }
    )

    if (!is.null(tt_neg_erosion) && nrow(tt_neg_erosion$tt) > 0) {
        cat("\n--- Truth Table (~fEROSION, Incl >= 0.80, N >= 1) ---\n")
        print(tt_neg_erosion)
        cat("\n--- Solutions for Outcome ~fEROSION ---\n")

        # CS
        sol_c_neg_erosion <- tryCatch(
             minimize(tt_neg_erosion, details = TRUE, method = "eQMC", show.cases = TRUE),
             error = function(e) { cat("Error minimizing (Conservative) for ~fEROSION:", e$message, "\n"); NULL }
        )
        if (!is.null(sol_c_neg_erosion)) {
            cat("\n--- Conservative Solution (~fEROSION) ---\n")
            print(sol_c_neg_erosion)
        } else {
            cat("Conservative solution could not be generated for ~fEROSION.\n")
        }

        # PS
        sol_p_neg_erosion <- tryCatch(
             minimize(tt_neg_erosion, details = TRUE, method = "eQMC", include = "?", show.cases = TRUE),
             error = function(e) { cat("Error minimizing (Parsimonious) for ~fEROSION:", e$message, "\n"); NULL }
        )
        if (!is.null(sol_p_neg_erosion)) {
            cat("\n--- Parsimonious Solution (~fEROSION) ---\n")
            print(sol_p_neg_erosion)
        } else {
            cat("Parsimonious solution could not be generated for ~fEROSION.\n")
        }

        # Intermediate Solution 
        cat("\n--- Intermediate Solution (~fEROSION - Requires dir.exp) ---\n")
        cat("To generate the Intermediate Solution, define directional expectations ('dir.exp') based on theory for the *absence* of erosion.\n")
        cat("Example: dir_exp_neg_erosion <- c(fINST=\"1\", fGINI=\"0\", fPOL=\"0\", fJUDATTACK_comp=\"0\") # Expect absence of erosion with high INST, low GINI, POL, JUDATTACK\n")
        cat("# Then run: minimize(tt_neg_erosion, details=TRUE, method=\"eQMC\", include=\"?\", dir.exp = dir_exp_neg_erosion, show.cases=TRUE)\n")

    } else {
        cat("Skipping minimization for ~fEROSION: Truth table is empty or could not be generated (check inclusion/N cuts and data variation).\n")
    }
}
```

    ## 
    ## --- Truth Table (~fEROSION, Incl >= 0.80, N >= 1) ---
    ## 
    ##   OUT: output value
    ##     n: number of cases in configuration
    ##  incl: sufficiency inclusion score
    ##   PRI: proportional reduction in inconsistency
    ## 
    ##      fINST fGINI fPOL fJUDATTACK_comp   OUT    n  incl  PRI  
    ## 10     1     0    0          1           1     1  0.945 0.895
    ##  9     1     0    0          0           1     2  0.919 0.861
    ## 13     1     1    0          0           1     2  0.910 0.828
    ## 15     1     1    1          0           1     1  0.839 0.729
    ## 12     1     0    1          1           0     1  0.727 0.479
    ##  6     0     1    0          1           0     1  0.546 0.111
    ##  1     0     0    0          0           0     2  0.341 0.112
    ##  4     0     0    1          1           0     1  0.314 0.001
    ##  8     0     1    1          1           0     4  0.284 0.041
    ##      cases                             
    ## 10   Poland                            
    ##  9   France,Greece                     
    ## 13   Hungary,Slovakia                  
    ## 15   Czechia                           
    ## 12   Spain                             
    ##  6   Austria                           
    ##  1   Italy,United Kingdom              
    ##  4   Switzerland                       
    ##  8   Belgium,Denmark,Netherlands,Sweden
    ## 
    ## 
    ## --- Solutions for Outcome ~fEROSION ---
    ## 
    ## --- Conservative Solution (~fEROSION) ---
    ## 
    ## M1: fINST*~fGINI*~fPOL + fINST*fGINI*~fJUDATTACK_comp -> negEROSION
    ## 
    ##                                  inclS   PRI   covS   covU  
    ## ----------------------------------------------------------- 
    ## 1            fINST*~fGINI*~fPOL  0.871  0.812  0.443  0.290 
    ## 2  fINST*fGINI*~fJUDATTACK_comp  0.798  0.707  0.369  0.215 
    ## ----------------------------------------------------------- 
    ##                              M1  0.814  0.745  0.659 
    ## 
    ##                                  cases 
    ## -------------------------------------- 
    ## 1            fINST*~fGINI*~fPOL  France,Greece; Poland
    ## 2  fINST*fGINI*~fJUDATTACK_comp  Hungary,Slovakia; Czechia
    ## -------------------------------------- 
    ## 
    ## 
    ## --- Parsimonious Solution (~fEROSION) ---
    ## 
    ## M1: fINST*fGINI + fINST*~fPOL -> negEROSION 
    ## M2: fINST*~fPOL + fINST*~fJUDATTACK_comp -> negEROSION 
    ## M3: fINST*~fPOL + fGINI*~fJUDATTACK_comp -> negEROSION 
    ## M4: fINST*~fPOL + fPOL*~fJUDATTACK_comp -> negEROSION 
    ## M5: fINST*~fJUDATTACK_comp + ~fGINI*~fPOL*fJUDATTACK_comp -> negEROSION 
    ## 
    ## 
    ##                                                ---------------------------------
    ##                                  inclS   PRI   covS   covU   (M1)   (M2)   (M3)  
    ## -------------------------------------------------------------------------------- 
    ## 1                   fINST*fGINI  0.717  0.609  0.380  0.001  0.094               
    ## 2                   fINST*~fPOL  0.877  0.824  0.640  0.018  0.354  0.189  0.364 
    ## 3        fINST*~fJUDATTACK_comp  0.843  0.787  0.585  0.041         0.134        
    ## 4        fGINI*~fJUDATTACK_comp  0.694  0.582  0.414  0.000                0.138 
    ## 5         fPOL*~fJUDATTACK_comp  0.748  0.653  0.311  0.000                      
    ## 6  ~fGINI*~fPOL*fJUDATTACK_comp  0.767  0.601  0.324  0.000                      
    ## -------------------------------------------------------------------------------- 
    ##                              M1  0.781  0.714  0.734 
    ##                              M2  0.833  0.781  0.774 
    ##                              M3  0.761  0.689  0.779 
    ##                              M4  0.807  0.744  0.781 
    ##                              M5  0.799  0.731  0.782 
    ## 
    ## --------------------------------------------- 
    ##                                  (M4)   (M5)  
    ## --------------------------------------------- 
    ## 1                   fINST*fGINI               
    ## 2                   fINST*~fPOL  0.470        
    ## 3        fINST*~fJUDATTACK_comp         0.458 
    ## 4        fGINI*~fJUDATTACK_comp               
    ## 5         fPOL*~fJUDATTACK_comp  0.140        
    ## 6  ~fGINI*~fPOL*fJUDATTACK_comp         0.197 
    ## --------------------------------------------- 
    ## 
    ##                                  cases 
    ## -------------------------------------- 
    ## 1                   fINST*fGINI  Hungary,Slovakia; Czechia
    ## 2                   fINST*~fPOL  France,Greece; Poland; Hungary,Slovakia
    ## 3        fINST*~fJUDATTACK_comp  France,Greece; Hungary,Slovakia; Czechia
    ## 4        fGINI*~fJUDATTACK_comp  Hungary,Slovakia; Czechia
    ## 5         fPOL*~fJUDATTACK_comp  Czechia
    ## 6  ~fGINI*~fPOL*fJUDATTACK_comp  Poland
    ## -------------------------------------- 
    ## 
    ## 
    ## --- Intermediate Solution (~fEROSION - Requires dir.exp) ---
    ## To generate the Intermediate Solution, define directional expectations ('dir.exp') based on theory for the *absence* of erosion.
    ## Example: dir_exp_neg_erosion <- c(fINST="1", fGINI="0", fPOL="0", fJUDATTACK_comp="0") # Expect absence of erosion with high INST, low GINI, POL, JUDATTACK
    ## # Then run: minimize(tt_neg_erosion, details=TRUE, method="eQMC", include="?", dir.exp = dir_exp_neg_erosion, show.cases=TRUE)

## 5. Conclusion

``` r
cat("--- Summary (Cross-Sectional, N=", nrow(qca_calibrated_data_cs_df), ") ---\n")
```

    ## --- Summary (Cross-Sectional, N= 15 ) ---

``` r
cat("Outcome: fEROSION (Fuzzy membership in Democratic Erosion, based on LDI change 2000-02 vs 2018-20)\n")
```

    ## Outcome: fEROSION (Fuzzy membership in Democratic Erosion, based on LDI change 2000-02 vs 2018-20)

``` r
cat("Conditions:", paste(conditions_cs, collapse=", "), "\n\n")
```

    ## Conditions: fINST, fGINI, fPOL, fJUDATTACK_comp

**Guide:**

1.  **Necessity analysis:** Examine the results printed under “Necessity
    Analysis for Outcome fEROSION” and “~fEROSION”. Look for conditions
    (or their absence, indicated by lowercase letters) with high
    consistency (`Incl.`) scores (typically \> 0.9) and potentially
    meaningful coverage (`Cov.`). A necessary condition must be present
    for the outcome to occur.
2.  **Sufficiency analysis:**
    - **Truth table:** Review the truth tables for `fEROSION` and
      `~fEROSION`. These show the observed combinations of conditions
      (rows), the number of cases (`n`), the consistency (`incl`) of
      that configuration leading to the outcome, and the outcome value
      assigned (`OUT`). Configurations with `OUT=1` are used for
      minimisation.
    - **Solutions (Conservative & Parsimonious):** Examine the solution
      formulas produced by the `minimize` function for both `fEROSION`
      and `~fEROSION`.
      - Each line in the solution represents a *pathway* (a combination
        of conditions) sufficient for the outcome.
      - Uppercase letters indicate the *presence* of a condition;
        lowercase letters indicate its *absence*.
      - `*` represents ‘AND’.
      - `+` represents ‘OR’ **operator** (separating different
        sufficient pathways).
      - Check the `inclS` (consistency) and `covS` (coverage) for each
        pathway, and the overall solution consistency (`sol.incl`) and
        coverage (`sol.cov`). High consistency (e.g., \> 0.80 or 0.85)
        is desired for sufficiency. Coverage indicates the empirical
        relevance of a pathway or solution.
      - The Conservative solution uses only configurations with observed
        cases supporting the outcome. The Parsimonious solution also
        uses logical remainders (unobserved configurations) to simplify
        the formula, based on simplifying assumptions. The Intermediate
        solution (if generated using `dir.exp`) balances these, using
        only theoretically plausible remainders. Comparing these
        solutions helps assess the robustness of the findings.
