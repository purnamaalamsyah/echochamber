# ======================================================
# RATING SCALE MODEL (RSM) ANALYSIS SCRIPT - ENHANCED VERSION
# With Multidimensional Analysis Support
# Version: 2025.05.24.v2
# ======================================================

# Start timer for execution tracking
start_time <- Sys.time()

# ======================================================
# 0. SCRIPT INITIALIZATION
# ======================================================

# Display R version information
cat("\nR Version Information:\n")
print(R.version.string)
cat("\nScript started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")

# Modern package management approach with pacman
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cran.r-project.org")
}

# Define required packages
required_packages <- c(
  # Core psychometric packages
  "TAM", "psych", "WrightMap", "difR", "eRm", "RaschSampler", "mirt",
  # Analysis packages
  "car", "lavaan", "paran", "fmsb", "nFactors",
  # Tidyverse ecosystem
  "tidyverse", "dplyr", "tidyr", "purrr", "forcats", "stringr",
  # Enhanced visualization
  "ggplot2", "ggthemes", "cowplot", "viridis", "scales", "patchwork",
  "reshape2", "corrplot", "lattice", "gridExtra", "ggrepel",
  # Data handling
  "readxl", "janitor", "tibble", "fs",
  # Reporting
  "knitr", "kableExtra"
)

# Load packages with pacman
cat("\nLoading required packages...\n")
suppressMessages(
  pacman::p_load(char = required_packages, install = TRUE, update = FALSE)
)
cat("All packages loaded successfully.\n")

# Create output directory with modern fs package approach
output_dir <- "rasch_analysis_output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat(sprintf("\nCreated output directory: %s\n", output_dir))
} else {
  cat(sprintf("\nUsing existing output directory: %s\n", output_dir))
}

# Set global options for better output
options(
  scipen = 999,  # Avoid scientific notation
  digits = 4,    # Set default digits
  width = 100    # Set console width
)

# ======================================================
# 1. DATA PREPARATION
# ======================================================

# Display analysis information
cat("\n============================================================\n")
cat("RATING SCALE MODEL (RSM) ANALYSIS FOR AI ETHICS ASSESSMENT\n")
cat("============================================================\n")
cat("This script performs a comprehensive Rasch analysis using the Rating Scale Model\n")
cat("with enhanced multidimensional analysis capabilities.\n\n")

# Function to safely load Excel dataset with enhanced error handling
load_excel_data <- function(file_path, sheet = 1) {
  tryCatch({
    # Check if file exists first
    if (!file.exists(file_path)) {
      stop(paste("File not found:", file_path))
    }

    # Check file extension
    if (!grepl("\\.xlsx?$", file_path, ignore.case = TRUE)) {
      stop("File must be an Excel file (.xls or .xlsx)")
    }

    # Try to read the Excel file with readxl
    data <- readxl::read_excel(file_path, sheet = sheet, col_types = "guess")

    # Check if data is empty
    if (nrow(data) == 0) {
      stop("Excel file appears to be empty")
    }

    cat("Excel dataset loaded successfully.\n")
    cat(sprintf("- Loaded %d rows and %d columns\n", nrow(data), ncol(data)))

    # Convert to tibble for tidyverse compatibility
    return(tibble::as_tibble(data))

  }, error = function(e) {
    stop(paste("Failed to load Excel dataset:", e$message,
               "\nPlease check file path and format."))
  })
}

# Load dataset with error handling
cat("\nLoading dataset...\n")
data_file <- "RASCH_brin_02052025_all.xlsx"

# Try to load the data
data <- tryCatch({
  load_excel_data(data_file)
}, error = function(e) {
  # If default file not found, provide helpful message
  cat("\nError:", e$message, "\n")
  cat("\nPlease ensure the data file 'RASCH_brin_02052025_all.xlsx' is in the working directory.\n")
  cat("Current working directory:", getwd(), "\n")
  stop("Data loading failed. Please check the file location.")
})

# Display dataset structure
cat("\nDataset structure:\n")
glimpse(data)

# Check for completely empty rows and remove them
n_empty_rows <- sum(apply(data, 1, function(x) all(is.na(x) | x == "")))
if (n_empty_rows > 0) {
  cat(sprintf("\nRemoving %d completely empty rows...\n", n_empty_rows))
  data <- data[!apply(data, 1, function(x) all(is.na(x) | x == "")), ]
}

# Clean column names and remove empty columns
cat("\nCleaning column names and removing empty columns...\n")
data <- data %>%
  select(where(~!all(is.na(.)))) %>%  # Remove entirely empty columns
  janitor::clean_names()  # Standardize column names

# Display cleaned column names
cat("Cleaned column names:\n")
cat(paste(names(data), collapse = ", "), "\n")

# Standardize demographic variable names
standardize_colnames <- function(data) {
  # Create comprehensive mapping dictionary
  name_mappings <- c(
    # Year of birth variations
    "year_of_birth" = "year_of_birth",
    "yearofbirth" = "year_of_birth",
    "dob" = "year_of_birth",
    "birth_year" = "year_of_birth",
    "tahun_lahir" = "year_of_birth",

    # Gender variations
    "gender" = "gender",
    "sex" = "gender",
    "jenis_kelamin" = "gender",

    # Job title variations
    "job_title" = "job_title",
    "position" = "job_title",
    "jabatan" = "job_title",
    "pekerjaan" = "job_title",

    # Education variations
    "education" = "education",
    "education_level" = "education",
    "pendidikan" = "education",
    "tingkat_pendidikan" = "education"
  )

  # Apply standardization
  data %>%
    rename_with(~{
      lower_name <- tolower(.)
      if (lower_name %in% names(name_mappings)) {
        return(name_mappings[lower_name])
      } else {
        return(.)
      }
    })
}

# Apply standardization
data <- standardize_colnames(data)
cat("\nColumn names standardized for consistency.\n")

# Convert demographic variables to factors
cat("\nConverting demographic variables to factors...\n")
demographic_vars <- c("gender", "job_title", "year_of_birth", "education")

# Check and convert demographic variables
for (var in demographic_vars) {
  if (var %in% names(data)) {
    if (!is.factor(data[[var]])) {
      data[[var]] <- factor(data[[var]])
      cat(sprintf("- Converted %s to factor with %d levels\n",
                  var, length(levels(data[[var]]))))
    } else {
      cat(sprintf("- %s is already a factor with %d levels\n",
                  var, length(levels(data[[var]]))))
    }
  } else {
    cat(sprintf("- Warning: Variable %s not found in dataset\n", var))
  }
}

# Identify item columns
cat("\nIdentifying item columns...\n")

# Define patterns for item columns
item_patterns <- c(
  "^x\\d+$",      # X1, X2, etc.
  "^item\\d+$",   # item1, item2, etc.
  "^q\\d+$",      # q1, q2, etc.
  "^p\\d+$"       # p1, p2, etc.
)

# Find item columns using regex patterns
item_cols <- names(data)[sapply(names(data), function(name) {
  any(sapply(item_patterns, function(pattern) {
    grepl(pattern, name, ignore.case = TRUE)
  }))
})]

# If no items found with patterns, try to identify numeric columns
if (length(item_cols) == 0) {
  cat("No item columns found with standard patterns. Searching for numeric columns...\n")

  # Get all numeric columns that are not demographic variables
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  non_demo_numeric <- setdiff(numeric_cols, demographic_vars)

  if (length(non_demo_numeric) > 0) {
    cat(sprintf("Found %d numeric columns that could be items:\n", length(non_demo_numeric)))
    cat(paste(non_demo_numeric, collapse = ", "), "\n")

    # Check if these look like rating scale data
    sample_col <- data[[non_demo_numeric[1]]]
    unique_vals <- sort(unique(sample_col[!is.na(sample_col)]))

    if (length(unique_vals) <= 10 && all(unique_vals == floor(unique_vals))) {
      cat("These appear to be rating scale items. Using them for analysis.\n")
      item_cols <- non_demo_numeric
    }
  }
}

# Final check for item columns
if (length(item_cols) == 0) {
  stop("No item columns found in the dataset. Please check column names and data structure.")
} else {
  cat(sprintf("\nFound %d item columns: %s\n",
              length(item_cols), paste(item_cols, collapse = ", ")))
}

# Extract and validate items
items <- data %>%
  select(all_of(item_cols)) %>%
  mutate(across(everything(), ~{
    # Convert to numeric with proper error handling
    result <- suppressWarnings(as.numeric(.))
    if (sum(is.na(result)) > sum(is.na(.))) {
      warning(paste("Some non-numeric values in column", cur_column(), "were converted to NA"))
    }
    result
  }))

# Check rating scale properties
cat("\nChecking rating scale properties...\n")
unique_values <- sort(unique(unlist(items, use.names = FALSE)))
unique_values <- unique_values[!is.na(unique_values)]

cat("Unique values in item responses:", paste(unique_values, collapse = ", "), "\n")
cat("Number of rating categories:", length(unique_values), "\n")

# Validate rating scale
if (length(unique_values) > 10) {
  warning("More than 10 unique values found. This may not be appropriate for Rating Scale Model.")
} else if (length(unique_values) < 2) {
  stop("Less than 2 unique values found. This is not appropriate for Rating Scale Model.")
}

# Check for missing values
missing_summary <- items %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "Item",
               values_to = "Missing_Count") %>%
  mutate(
    Missing_Percent = round((Missing_Count / nrow(items)) * 100, 2),
    Status = case_when(
      Missing_Percent == 0 ~ "Complete",
      Missing_Percent < 5 ~ "Acceptable",
      Missing_Percent < 10 ~ "Moderate",
      TRUE ~ "High"
    )
  )

cat("\nMissing values summary:\n")
print(missing_summary)

# Check if too many missing values
high_missing <- missing_summary %>% filter(Status == "High")
if (nrow(high_missing) > 0) {
  cat("\nWarning: The following items have high missing values (>10%):\n")
  print(high_missing)
  cat("Consider removing these items or using appropriate missing data techniques.\n")
}

# Calculate descriptive statistics
cat("\nItem descriptive statistics:\n")
item_summary <- psych::describe(items) %>%
  as.data.frame() %>%
  mutate(Item = rownames(.)) %>%
  select(Item, n, mean, sd, min, max, skew, kurtosis) %>%
  arrange(mean)

print(item_summary, row.names = FALSE)

# Check for potential ceiling/floor effects at item level
ceiling_floor_check <- items %>%
  summarise(across(everything(), ~{
    list(
      floor_pct = sum(. == min(unique_values), na.rm = TRUE) / sum(!is.na(.)) * 100,
      ceiling_pct = sum(. == max(unique_values), na.rm = TRUE) / sum(!is.na(.)) * 100
    )
  })) %>%
  pivot_longer(cols = everything(), names_to = "Item") %>%
  mutate(
    Floor_Percent = round(map_dbl(value, "floor_pct"), 1),
    Ceiling_Percent = round(map_dbl(value, "ceiling_pct"), 1)
  ) %>%
  select(Item, Floor_Percent, Ceiling_Percent)

cat("\nCeiling/Floor effects by item:\n")
print(ceiling_floor_check)

# Flag problematic items
problematic_ceiling_floor <- ceiling_floor_check %>%
  filter(Floor_Percent > 50 | Ceiling_Percent > 50)

if (nrow(problematic_ceiling_floor) > 0) {
  cat("\nWarning: The following items show extreme ceiling/floor effects (>50%):\n")
  print(problematic_ceiling_floor)
}

# Save cleaned data
write_csv(data, file.path(output_dir, "cleaned_data.csv"))
cat("\nCleaned data saved to:", file.path(output_dir, "cleaned_data.csv"), "\n")

# Create a data summary report
data_summary <- list(
  n_respondents = nrow(data),
  n_items = length(item_cols),
  n_categories = length(unique_values),
  categories = unique_values,
  n_complete_cases = sum(complete.cases(items)),
  pct_complete_cases = round(sum(complete.cases(items)) / nrow(items) * 100, 1)
)

cat("\n=== DATA SUMMARY ===\n")
cat("Number of respondents:", data_summary$n_respondents, "\n")
cat("Number of items:", data_summary$n_items, "\n")
cat("Number of rating categories:", data_summary$n_categories, "\n")
cat("Rating scale values:", paste(data_summary$categories, collapse = ", "), "\n")
cat("Complete cases:", data_summary$n_complete_cases,
    sprintf("(%.1f%%)", data_summary$pct_complete_cases), "\n")

# ======================================================
# 2. ENHANCED UNIDIMENSIONALITY TESTING WITH MULTIDIMENSIONAL ANALYSIS
# ======================================================

cat("\n============================================================\n")
cat("ENHANCED UNIDIMENSIONALITY TESTING\n")
cat("============================================================\n")
cat("Testing the assumption that all items measure a single construct.\n")
cat("If multidimensional, will identify the number of dimensions and item loadings.\n\n")

# Correlation matrix
cat("Calculating item correlation matrix...\n")
item_cor <- cor(items, use = "pairwise.complete.obs")

# Check if correlation matrix is valid
if (any(is.na(item_cor))) {
  warning("Correlation matrix contains NA values. This may affect subsequent analyses.")
}

# Create enhanced correlation matrix visualization
cat("Creating correlation matrix visualization...\n")
pdf(file.path(output_dir, "correlation_matrix.pdf"), width = 10, height = 8)

# Custom color palette
cor_colors <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                                  "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                                  "#4393C3", "#2166AC", "#053061"))(200)

corrplot(item_cor,
         method = "color",
         type = "upper",
         order = "hclust",
         col = cor_colors,
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         title = "Item Correlation Matrix",
         mar = c(0, 0, 2, 0),
         cl.lim = c(-1, 1))

dev.off()
cat("Correlation matrix visualization saved.\n")

# Calculate correlation statistics
cor_values <- item_cor[lower.tri(item_cor)]
cor_stats <- data.frame(
  Statistic = c("Mean", "Median", "SD", "Min", "Max", "% > 0.3", "% > 0.5"),
  Value = c(
    mean(cor_values, na.rm = TRUE),
    median(cor_values, na.rm = TRUE),
    sd(cor_values, na.rm = TRUE),
    min(cor_values, na.rm = TRUE),
    max(cor_values, na.rm = TRUE),
    sum(cor_values > 0.3, na.rm = TRUE) / length(cor_values) * 100,
    sum(cor_values > 0.5, na.rm = TRUE) / length(cor_values) * 100
  )
) %>%
  mutate(Value = round(Value, 3))

cat("\nInter-item correlation statistics:\n")
print(cor_stats, row.names = FALSE)

# Interpretation
if (cor_stats$Value[cor_stats$Statistic == "Mean"] < 0.2) {
  cat("\nWarning: Low average inter-item correlation suggests items may not be measuring the same construct.\n")
} else if (cor_stats$Value[cor_stats$Statistic == "Mean"] > 0.7) {
  cat("\nWarning: Very high average inter-item correlation suggests potential redundancy among items.\n")
} else {
  cat("\nInter-item correlations are within acceptable range for unidimensional construct.\n")
}

# Bartlett's test of sphericity
cat("\nPerforming Bartlett's test of sphericity...\n")
bartlett_test <- psych::cortest.bartlett(item_cor, n = nrow(items))
cat("Chi-square:", round(bartlett_test$chisq, 2), "\n")
cat("df:", bartlett_test$df, "\n")
cat("p-value:", format.pval(bartlett_test$p.value, digits = 4), "\n")

if (bartlett_test$p.value < 0.05) {
  cat("Result: Correlation matrix differs significantly from identity matrix (good for factor analysis).\n")
} else {
  cat("Warning: Correlation matrix may not differ from identity matrix.\n")
}

# Kaiser-Meyer-Olkin (KMO) test
cat("\nPerforming Kaiser-Meyer-Olkin (KMO) test...\n")
kmo_test <- psych::KMO(item_cor)
cat("Overall MSA:", round(kmo_test$MSA, 3), "\n")

# KMO interpretation
kmo_interpretation <- case_when(
  kmo_test$MSA >= 0.9 ~ "Marvelous - Proceed with confidence",
  kmo_test$MSA >= 0.8 ~ "Meritorious - Proceed with confidence",
  kmo_test$MSA >= 0.7 ~ "Middling - Proceed with caution",
  kmo_test$MSA >= 0.6 ~ "Mediocre - Consider improving items",
  kmo_test$MSA >= 0.5 ~ "Miserable - Questionable for factor analysis",
  TRUE ~ "Unacceptable - Do not proceed"
)
cat("Interpretation:", kmo_interpretation, "\n")

# Individual item MSA values
cat("\nIndividual item MSA values:\n")
item_msa <- data.frame(
  Item = names(kmo_test$MSAi),
  MSA = round(kmo_test$MSAi, 3)
) %>%
  arrange(MSA)
print(item_msa, row.names = FALSE)

# Flag items with low MSA
low_msa_items <- item_msa %>% filter(MSA < 0.5)
if (nrow(low_msa_items) > 0) {
  cat("\nWarning: The following items have low MSA values (<0.5):\n")
  print(low_msa_items, row.names = FALSE)
}

# ENHANCED PARALLEL ANALYSIS WITH MULTIPLE METHODS
cat("\n=== ENHANCED DIMENSIONALITY ASSESSMENT ===\n")

# 1. Standard Parallel Analysis
cat("\nPerforming parallel analysis...\n")
set.seed(42)

pa_result <- tryCatch({
  psych::fa.parallel(items,
                     fa = "both",  # Both FA and PC
                     fm = "minres",
                     n.iter = 100,
                     main = "Parallel Analysis Scree Plot",
                     quant = .95)
}, error = function(e) {
  cat("Standard parallel analysis failed. Trying alternative method...\n")
  psych::fa.parallel(items,
                     fa = "pc",
                     n.iter = 100,
                     main = "Parallel Analysis Scree Plot",
                     quant = .95)
})

cat("Parallel analysis suggests", pa_result$nfact, "factor(s)\n")
cat("Parallel analysis suggests", pa_result$ncomp, "component(s)\n")

# 2. MAP (Minimum Average Partial) Test
cat("\nPerforming MAP test...\n")
map_test <- tryCatch({
  psych::nfactors(items, n = ncol(items), rotate = "varimax", fm = "minres")
}, error = function(e) {
  cat("MAP test failed with minres, trying with pa...\n")
  psych::nfactors(items, n = ncol(items), rotate = "varimax", fm = "pa")
})

# 3. Very Simple Structure (VSS)
cat("\nPerforming VSS analysis...\n")
vss_result <- tryCatch({
  psych::vss(item_cor, n = min(8, ncol(items)/2), rotate = "varimax",
             fm = "minres", n.obs = nrow(items))
}, error = function(e) {
  cat("VSS failed, continuing with other methods...\n")
  NULL
})

# 4. Optimal Coordinates and Acceleration Factor
cat("\nCalculating Optimal Coordinates and Acceleration Factor...\n")
eigenvalues <- eigen(item_cor)$values
nScree_result <- nFactors::nScree(x = eigenvalues)
nScree_plot <- plot(nScree_result)

# Summary of dimensionality tests
cat("\n=== DIMENSIONALITY TEST SUMMARY ===\n")
cat("Parallel Analysis (FA):", pa_result$nfact, "factors\n")
cat("Parallel Analysis (PCA):", pa_result$ncomp, "components\n")
if (!is.null(map_test)) {
  cat("MAP test:", which.min(map_test$map), "factors\n")
}
if (!is.null(vss_result)) {
  cat("VSS complexity 1:", vss_result$vss.stats$cfit[1], "\n")
}
cat("Optimal Coordinates:", nScree_result$Components$noc, "factors\n")
cat("Acceleration Factor:", nScree_result$Components$naf, "factors\n")

# Determine number of dimensions
suggested_dimensions <- c(
  pa_result$nfact,
  which.min(map_test$map) %||% NA,
  nScree_result$Components$noc,
  nScree_result$Components$naf
)
suggested_dimensions <- suggested_dimensions[!is.na(suggested_dimensions)]
n_dimensions <- as.integer(median(suggested_dimensions))

cat("\nBased on multiple criteria, the suggested number of dimensions is:", n_dimensions, "\n")

# MULTIDIMENSIONAL ANALYSIS IF NEEDED
if (n_dimensions > 1) {
  cat("\n============================================================\n")
  cat("MULTIDIMENSIONAL STRUCTURE ANALYSIS\n")
  cat("============================================================\n")
  cat("Data appears to be multidimensional. Performing detailed analysis...\n\n")

  # Perform factor analysis with determined number of factors
  cat("Extracting", n_dimensions, "factors...\n")

  fa_multi <- tryCatch({
    psych::fa(items,
              nfactors = n_dimensions,
              rotate = "oblimin",  # Oblique rotation allows factors to correlate
              fm = "minres",
              scores = "regression")
  }, error = function(e) {
    cat("Factor analysis with minres failed. Trying principal axis...\n")
    psych::fa(items,
              nfactors = n_dimensions,
              rotate = "oblimin",
              fm = "pa")
  })

  # Extract loadings and create factor assignment
  loadings_matrix <- fa_multi$loadings

  # Create a dataframe with loadings
  loadings_df <- as.data.frame(unclass(loadings_matrix))
  loadings_df$Item <- rownames(loadings_df)
  loadings_df <- loadings_df %>%
    relocate(Item, .before = 1)

  # Determine primary factor for each item
  loadings_df$Primary_Factor <- apply(abs(loadings_matrix), 1, which.max)
  loadings_df$Max_Loading <- apply(abs(loadings_matrix), 1, max)

  # Sort by factor and loading
  loadings_df <- loadings_df %>%
    arrange(Primary_Factor, desc(Max_Loading))

  cat("\n=== FACTOR LOADINGS AND ITEM ASSIGNMENTS ===\n")
  print(loadings_df %>%
          mutate(across(where(is.numeric) & !c(Primary_Factor), ~round(., 3))),
        row.names = FALSE)

  # Factor interpretation
  cat("\n=== FACTOR INTERPRETATION ===\n")
  for (i in 1:n_dimensions) {
    cat(sprintf("\nFactor %d:\n", i))
    factor_items <- loadings_df %>%
      filter(Primary_Factor == i) %>%
      arrange(desc(Max_Loading))

    cat("Items loading on this factor:\n")
    for (j in 1:nrow(factor_items)) {
      cat(sprintf("  - %s (loading = %.3f)\n",
                  factor_items$Item[j],
                  factor_items$Max_Loading[j]))
    }

    # Variance explained
    var_explained <- fa_multi$Vaccounted[2, i] * 100
    cat(sprintf("Variance explained: %.1f%%\n", var_explained))
  }

  # Factor correlations
  if (n_dimensions > 1) {
    cat("\n=== FACTOR CORRELATIONS ===\n")
    factor_cors <- fa_multi$Phi
    if (!is.null(factor_cors)) {
      print(round(factor_cors, 3))

      # Check if factors are highly correlated
      high_cors <- which(abs(factor_cors) > 0.7 & factor_cors != 1, arr.ind = TRUE)
      if (nrow(high_cors) > 0) {
        cat("\nWarning: High correlations between factors detected.\n")
        cat("Consider using a single dimension or hierarchical model.\n")
      }
    }
  }

  # Save multidimensional results
  write_csv(loadings_df, file.path(output_dir, "factor_loadings.csv"))

  # Create visualization of factor structure
  cat("\nCreating factor structure visualization...\n")

  # Prepare data for visualization
  loadings_long <- loadings_df %>%
    select(-Primary_Factor, -Max_Loading) %>%
    pivot_longer(cols = -Item,
                 names_to = "Factor",
                 values_to = "Loading") %>%
    mutate(Factor = gsub("MR", "Factor ", Factor))

  # Create heatmap
  factor_heatmap <- ggplot(loadings_long,
                           aes(x = Factor, y = Item, fill = Loading)) +
    geom_tile() +
    geom_text(aes(label = round(Loading, 2)), size = 3) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0, limits = c(-1, 1)) +
    labs(title = "Factor Loading Heatmap",
         subtitle = paste("Multidimensional structure with", n_dimensions, "factors"),
         x = "Factor", y = "Item") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 10))

  ggsave(file.path(output_dir, "factor_heatmap.pdf"),
         plot = factor_heatmap, width = 8, height = 10, dpi = 300)

  # RECOMMENDATIONS FOR MULTIDIMENSIONAL DATA
  cat("\n=== RECOMMENDATIONS FOR MULTIDIMENSIONAL DATA ===\n")
  cat("The data shows evidence of multidimensionality. Options:\n")
  cat("1. Proceed with separate Rasch analyses for each dimension\n")
  cat("2. Use multidimensional Rasch models (e.g., MultiTAM)\n")
  cat("3. Select items from the primary dimension only\n")
  cat("4. Consider if dimensions are substantively meaningful\n")

  # Identify items for unidimensional subset
  primary_factor_items <- loadings_df %>%
    filter(Primary_Factor == 1, Max_Loading > 0.4) %>%
    pull(Item)

  cat("\nItems strongly loading on Factor 1 (primary dimension):\n")
  cat(paste(primary_factor_items, collapse = ", "), "\n")
  cat("\nConsider using these items for unidimensional Rasch analysis.\n")

} else {
  # UNIDIMENSIONAL ANALYSIS
  cat("\n=== UNIDIMENSIONAL STRUCTURE CONFIRMED ===\n")

  # Perform single factor analysis for details
  fa_result <- tryCatch({
    psych::fa(items,
              nfactors = 1,
              rotate = "none",
              fm = "minres",
              scores = "regression")
  }, error = function(e) {
    cat("Factor analysis with minres failed. Trying principal axis factoring...\n")
    psych::fa(items,
              nfactors = 1,
              rotate = "none",
              fm = "pa")
  })

  # Extract and display factor loadings
  loadings_df <- data.frame(
    Item = item_cols,
    Loading = as.vector(fa_result$loadings),
    Communality = fa_result$communality,
    Uniqueness = fa_result$uniquenesses
  ) %>%
    mutate(
      Loading_Strength = case_when(
        abs(Loading) >= 0.7 ~ "Strong",
        abs(Loading) >= 0.4 ~ "Moderate",
        TRUE ~ "Weak"
      )
    ) %>%
    arrange(desc(abs(Loading)))

  cat("\nFactor loadings summary:\n")
  print(loadings_df, row.names = FALSE)

  # Summary of factor analysis
  cat("\n=== FACTOR ANALYSIS SUMMARY ===\n")
  cat("Variance explained by first factor:",
      round(fa_result$Vaccounted[2,1] * 100, 2), "%\n")
  cat("Tucker-Lewis Index (TLI):", round(fa_result$TLI, 3), "\n")
  cat("RMSEA:", round(fa_result$RMSEA[1], 3), "\n")
  cat("BIC:", round(fa_result$BIC, 2), "\n")

  # Overall unidimensionality assessment
  cat("\n=== UNIDIMENSIONALITY ASSESSMENT ===\n")

  unidim_criteria <- data.frame(
    Criterion = c(
      "First factor variance > 40%",
      "Parallel analysis suggests 1 factor",
      "All loadings > 0.3",
      "TLI > 0.9",
      "RMSEA < 0.08"
    ),
    Met = c(
      fa_result$Vaccounted[2,1] > 0.4,
      pa_result$nfact == 1,
      all(abs(loadings_df$Loading) > 0.3),
      fa_result$TLI > 0.9,
      fa_result$RMSEA[1] < 0.08
    )
  )

  cat("\nUnidimensionality criteria:\n")
  print(unidim_criteria, row.names = FALSE)

  n_criteria_met <- sum(unidim_criteria$Met)
  if (n_criteria_met >= 4) {
    cat("\nConclusion: Strong evidence for unidimensionality. Proceed with Rasch analysis.\n")
  } else if (n_criteria_met >= 3) {
    cat("\nConclusion: Moderate evidence for unidimensionality. Proceed with caution.\n")
  } else {
    cat("\nConclusion: Weak evidence for unidimensionality. Consider revising items.\n")
  }
}

# Create scree plot regardless of dimensionality
eigenvalues <- eigen(item_cor)$values
scree_data <- data.frame(
  Component = 1:length(eigenvalues),
  Eigenvalue = eigenvalues,
  Proportion = eigenvalues / sum(eigenvalues) * 100,
  Cumulative = cumsum(eigenvalues / sum(eigenvalues) * 100)
)

# Enhanced scree plot
scree_plot <- ggplot(scree_data, aes(x = Component, y = Eigenvalue)) +
  geom_line(size = 1.2, color = "blue") +
  geom_point(size = 3, color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = round(Eigenvalue, 2)), vjust = -1, size = 3) +
  scale_x_continuous(breaks = 1:nrow(scree_data)) +
  labs(title = "Scree Plot with Eigenvalues",
       subtitle = paste("First factor explains",
                       round(scree_data$Proportion[1], 1), "% of variance"),
       x = "Component Number",
       y = "Eigenvalue") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(output_dir, "scree_plot_enhanced.pdf"),
       plot = scree_plot, width = 10, height = 6, dpi = 300)

# ======================================================
# 3. RATING SCALE MODEL (RSM) FITTING
# ======================================================

cat("\n============================================================\n")
cat("RATING SCALE MODEL (RSM) FITTING\n")
cat("============================================================\n")

# Decision point based on dimensionality
if (n_dimensions > 1) {
  cat("\nData is multidimensional. Options for proceeding:\n")
  cat("1. Analyzing all items (may violate unidimensionality assumption)\n")
  cat("2. Analyzing only items from primary dimension\n")
  cat("\nProceeding with all items but results should be interpreted with caution.\n\n")

  # Store information about multidimensionality
  multidim_warning <- TRUE
} else {
  multidim_warning <- FALSE
}

# Prepare data for TAM
cat("Preparing data for Rasch analysis...\n")

# Ensure items are properly coded (starting from 0)
min_cat <- min(unique_values)
if (min_cat != 0) {
  cat(sprintf("Recoding items to start from 0 (original minimum was %d)...\n", min_cat))
  items_recoded <- items - min_cat
} else {
  items_recoded <- items
}

# Clear memory before fitting model
gc(verbose = FALSE)

# Fit RSM model
cat("\nFitting Rating Scale Model (RSM)...\n")
rsm_control <- list(
  maxiter = 1000,
  conv = 0.0001,
  progress = TRUE,
  fac.iter = 10
)

set.seed(42)
rsm_start_time <- Sys.time()

rsm_model <- tryCatch({
  TAM::tam.mml(
    resp = as.data.frame(items_recoded),
    irtmodel = "RSM",
    control = rsm_control,
    verbose = FALSE
  )
}, error = function(e) {
  cat("Error in RSM fitting:", e$message, "\n")
  cat("Trying with more relaxed convergence criteria...\n")

  rsm_control$conv <- 0.001
  TAM::tam.mml(
    resp = as.data.frame(items_recoded),
    irtmodel = "RSM",
    control = rsm_control,
    verbose = FALSE
  )
})

rsm_fit_time <- difftime(Sys.time(), rsm_start_time, units = "secs")
cat("RSM fitting completed in", round(rsm_fit_time, 1), "seconds\n")

# Check convergence
if (rsm_model$iter >= rsm_control$maxiter) {
  warning("Model reached maximum iterations without converging.")
} else {
  cat("Model converged successfully after", rsm_model$iter, "iterations\n")
}

# Model fit statistics
cat("\n=== RSM MODEL FIT STATISTICS ===\n")
cat("Log-likelihood:", round(rsm_model$loglike, 2), "\n")
cat("AIC:", round(rsm_model$AIC, 2), "\n")
cat("BIC:", round(rsm_model$BIC, 2), "\n")
cat("CAIC:", round(rsm_model$CAIC, 2), "\n")
cat("Number of parameters:", rsm_model$Npars, "\n")

if (multidim_warning) {
  cat("\nWARNING: Data showed evidence of multidimensionality.\n")
  cat("Model fit statistics may be affected by violation of unidimensionality assumption.\n")
}

# Compare with Partial Credit Model (PCM)
cat("\nFitting Partial Credit Model (PCM) for comparison...\n")
set.seed(42)
pcm_start_time <- Sys.time()

pcm_model <- tryCatch({
  TAM::tam.mml(
    resp = as.data.frame(items_recoded),
    irtmodel = "PCM",
    control = rsm_control,
    verbose = FALSE
  )
}, error = function(e) {
  cat("PCM fitting failed:", e$message, "\n")
  NULL
})

if (!is.null(pcm_model)) {
  pcm_fit_time <- difftime(Sys.time(), pcm_start_time, units = "secs")
  cat("PCM fitting completed in", round(pcm_fit_time, 1), "seconds\n")

  # Model comparison
  cat("\n=== MODEL COMPARISON ===\n")
  model_comparison <- data.frame(
    Model = c("RSM", "PCM"),
    Parameters = c(rsm_model$Npars, pcm_model$Npars),
    Loglik = c(rsm_model$loglike, pcm_model$loglike),
    AIC = c(rsm_model$AIC, pcm_model$AIC),
    BIC = c(rsm_model$BIC, pcm_model$BIC),
    CAIC = c(rsm_model$CAIC, pcm_model$CAIC)
  ) %>%
    mutate(across(Loglik:CAIC, ~round(., 2)))

  print(model_comparison, row.names = FALSE)

  # Likelihood ratio test
  lr_stat <- 2 * (pcm_model$loglike - rsm_model$loglike)
  lr_df <- pcm_model$Npars - rsm_model$Npars
  lr_p <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)

  cat("\nLikelihood Ratio Test (PCM vs RSM):\n")
  cat("LR statistic:", round(lr_stat, 2), "\n")
  cat("df:", lr_df, "\n")
  cat("p-value:", format.pval(lr_p, digits = 4), "\n")

  if (lr_p > 0.05) {
    cat("Result: No significant difference. RSM is preferred (more parsimonious).\n")
  } else {
    cat("Result: PCM fits significantly better, but RSM may still be preferred for interpretability.\n")
  }
}

# Save model
saveRDS(rsm_model, file = file.path(output_dir, "rsm_model.rds"))
cat("\nRSM model saved to:", file.path(output_dir, "rsm_model.rds"), "\n")

# If multidimensional, also save dimension information
if (n_dimensions > 1) {
  dimension_info <- list(
    n_dimensions = n_dimensions,
    factor_loadings = loadings_df,
    factor_correlations = if(exists("fa_multi")) fa_multi$Phi else NULL,
    variance_explained = if(exists("fa_multi")) fa_multi$Vaccounted else NULL,
    multidim_warning = TRUE
  )
  saveRDS(dimension_info, file = file.path(output_dir, "dimension_info.rds"))
  cat("Dimension information saved to:", file.path(output_dir, "dimension_info.rds"), "\n")
}

# ======================================================
# 4. ITEM FIT STATISTICS
# ======================================================

cat("\n============================================================\n")
cat("ITEM FIT STATISTICS\n")
cat("============================================================\n")

# Calculate item fit
cat("Calculating item fit statistics...\n")
item_fit_stats <- TAM::tam.fit(rsm_model, progress = FALSE)

# Extract item parameters and fit statistics
item_params <- rsm_model$item
item_difficulties <- rsm_model$xsi[1:length(item_cols), "xsi"]

# Create comprehensive item statistics dataframe
item_fit <- data.frame(
  Item = item_cols,
  Difficulty = item_difficulties,
  SE_Difficulty = rsm_model$xsi[1:length(item_cols), "se.xsi"],
  Infit = item_fit_stats$itemfit$Infit,
  Infit_t = item_fit_stats$itemfit$Infit_t,
  Outfit = item_fit_stats$itemfit$Outfit,
  Outfit_t = item_fit_stats$itemfit$Outfit_t
) %>%
  mutate(
    # Fit evaluation based on Wright & Linacre (1994) criteria
    Infit_Status = case_when(
      Infit >= 0.7 & Infit <= 1.3 ~ "Good",
      Infit >= 0.5 & Infit < 0.7 ~ "Acceptable",
      Infit > 1.3 & Infit <= 1.5 ~ "Acceptable",
      TRUE ~ "Poor"
    ),
    Outfit_Status = case_when(
      Outfit >= 0.7 & Outfit <= 1.3 ~ "Good",
      Outfit >= 0.5 & Outfit < 0.7 ~ "Acceptable",
      Outfit > 1.3 & Outfit <= 1.5 ~ "Acceptable",
      TRUE ~ "Poor"
    ),
    # T-statistics evaluation
    Infit_t_Status = case_when(
      abs(Infit_t) <= 2 ~ "Acceptable",
      TRUE ~ "Significant"
    ),
    Outfit_t_Status = case_when(
      abs(Outfit_t) <= 2 ~ "Acceptable",
      TRUE ~ "Significant"
    ),
    # Overall evaluation
    Overall_Fit = case_when(
      Infit_Status == "Good" & Outfit_Status == "Good" ~ "Excellent",
      Infit_Status %in% c("Good", "Acceptable") &
        Outfit_Status %in% c("Good", "Acceptable") ~ "Acceptable",
      TRUE ~ "Problematic"
    )
  ) %>%
  arrange(Difficulty)

# Add dimension information if multidimensional
if (n_dimensions > 1 && exists("loadings_df")) {
  item_fit <- item_fit %>%
    left_join(loadings_df %>% select(Item, Primary_Factor, Max_Loading),
              by = "Item")
}

cat("\nItem fit statistics (sorted by difficulty):\n")
if (n_dimensions > 1) {
  print(item_fit %>%
          select(Item, Difficulty, Infit, Outfit, Overall_Fit, Primary_Factor) %>%
          mutate(across(c(Difficulty, Infit, Outfit), ~round(., 3))),
        row.names = FALSE)
} else {
  print(item_fit %>%
          select(Item, Difficulty, Infit, Outfit, Overall_Fit) %>%
          mutate(across(c(Difficulty, Infit, Outfit), ~round(., 3))),
        row.names = FALSE)
}

# Fit summary
fit_summary <- item_fit %>%
  count(Overall_Fit) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

cat("\n=== ITEM FIT SUMMARY ===\n")
print(fit_summary, row.names = FALSE)

# If multidimensional, show fit by dimension
if (n_dimensions > 1 && "Primary_Factor" %in% names(item_fit)) {
  cat("\n=== FIT BY DIMENSION ===\n")
  fit_by_dim <- item_fit %>%
    group_by(Primary_Factor) %>%
    summarise(
      N_Items = n(),
      Mean_Infit = mean(Infit),
      SD_Infit = sd(Infit),
      Mean_Outfit = mean(Outfit),
      SD_Outfit = sd(Outfit),
      Pct_Good_Fit = sum(Overall_Fit %in% c("Excellent", "Acceptable")) / n() * 100
    ) %>%
    mutate(across(Mean_Infit:Pct_Good_Fit, ~round(., 2)))

  print(fit_by_dim, row.names = FALSE)

  if (any(fit_by_dim$Pct_Good_Fit < 50)) {
    cat("\nWarning: Some dimensions have poor item fit.\n")
    cat("Consider separate analyses for each dimension.\n")
  }
}

# Identify problematic items
problematic_items <- item_fit %>%
  filter(Overall_Fit == "Problematic") %>%
  select(Item, Difficulty, Infit, Outfit, Infit_Status, Outfit_Status)

if (nrow(problematic_items) > 0) {
  cat("\nProblematic items requiring review:\n")
  print(problematic_items, row.names = FALSE)

  # Detailed diagnostics for problematic items
  cat("\nDetailed diagnostics:\n")
  for (i in 1:nrow(problematic_items)) {
    item <- problematic_items$Item[i]
    cat(sprintf("\n%s:\n", item))

    if (problematic_items$Infit[i] > 1.5) {
      cat("  - High infit: Item shows more variability than expected (noisy)\n")
    } else if (problematic_items$Infit[i] < 0.5) {
      cat("  - Low infit: Item shows less variability than expected (redundant)\n")
    }

    if (problematic_items$Outfit[i] > 1.5) {
      cat("  - High outfit: Unexpected responses from persons far from item difficulty\n")
    } else if (problematic_items$Outfit[i] < 0.5) {
      cat("  - Low outfit: Too predictable, may be redundant with other items\n")
    }
  }
}

# Save item fit statistics
write_csv(item_fit, file.path(output_dir, "item_fit_statistics.csv"))

# Create enhanced item fit visualization
cat("\nCreating item fit visualizations...\n")

# 1. Infit-Outfit plot
fit_plot <- ggplot(item_fit, aes(x = Infit, y = Outfit)) +
  # Add acceptable region
  annotate("rect", xmin = 0.7, xmax = 1.3, ymin = 0.7, ymax = 1.3,
           alpha = 0.2, fill = "green") +
  # Add marginal regions
  annotate("rect", xmin = 0.5, xmax = 0.7, ymin = 0.5, ymax = 1.5,
           alpha = 0.1, fill = "yellow") +
  annotate("rect", xmin = 1.3, xmax = 1.5, ymin = 0.5, ymax = 1.5,
           alpha = 0.1, fill = "yellow") +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0.5, ymax = 0.7,
           alpha = 0.1, fill = "yellow") +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 1.3, ymax = 1.5,
           alpha = 0.1, fill = "yellow") +
  # Add items
  geom_point(aes(color = Overall_Fit, size = abs(Difficulty)), alpha = 0.8) +
  geom_text_repel(aes(label = Item), size = 3, max.overlaps = 20) +
  # Add reference lines
  geom_hline(yintercept = c(0.7, 1.3), linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = c(0.7, 1.3), linetype = "dashed", color = "darkgreen") +
  # Styling
  scale_color_manual(values = c("Excellent" = "darkgreen",
                               "Acceptable" = "orange",
                               "Problematic" = "red")) +
  scale_size_continuous(range = c(3, 8)) +
  labs(title = "Item Fit Statistics",
       subtitle = ifelse(multidim_warning,
                        "Note: Data shows multidimensional structure",
                        "Green region: Acceptable fit (0.7-1.3 for both statistics)"),
       x = "Infit Mean Square",
       y = "Outfit Mean Square",
       color = "Fit Status",
       size = "|Difficulty|") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = ifelse(multidim_warning, "red", "black")),
    legend.position = "right"
  ) +
  coord_cartesian(xlim = c(0.4, 1.6), ylim = c(0.4, 1.6))

# Add dimension information if multidimensional
if (n_dimensions > 1 && "Primary_Factor" %in% names(item_fit)) {
  fit_plot <- fit_plot +
    facet_wrap(~paste("Dimension", Primary_Factor), ncol = 2)
}

ggsave(file.path(output_dir, "item_fit_plot.pdf"),
       plot = fit_plot, width = ifelse(n_dimensions > 1, 12, 10),
       height = 8, dpi = 300)

# 2. Item difficulty vs fit plot
difficulty_fit_plot <- item_fit %>%
  pivot_longer(cols = c(Infit, Outfit),
               names_to = "Fit_Type",
               values_to = "Fit_Value") %>%
  ggplot(aes(x = Difficulty, y = Fit_Value, color = Fit_Type)) +
  geom_hline(yintercept = c(0.7, 1.3), linetype = "dashed", color = "gray50") +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(aes(group = Item), alpha = 0.3) +
  geom_text_repel(aes(label = Item), size = 2.5,
                   data = . %>% filter(Fit_Type == "Infit"),
                   max.overlaps = 20) +
  scale_color_manual(values = c("Infit" = "blue", "Outfit" = "red")) +
  labs(title = "Item Fit by Difficulty",
       x = "Item Difficulty (logits)",
       y = "Fit Statistic",
       color = "Fit Type") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(output_dir, "difficulty_fit_plot.pdf"),
       plot = difficulty_fit_plot, width = 10, height = 6, dpi = 300)

cat("Item fit visualizations saved.\n")

# ======================================================
# 5. RATING SCALE DIAGNOSTICS
# ======================================================

cat("\n============================================================\n")
cat("RATING SCALE DIAGNOSTICS\n")
cat("============================================================\n")

# Extract threshold parameters
cat("Extracting rating scale structure...\n")

# Get threshold parameters from the model
# Extract step parameters for RSM
threshold_indices <- grep("step", rownames(rsm_model$xsi))
if (length(threshold_indices) > 0) {
  step_params <- rsm_model$xsi[threshold_indices, "xsi"]
  step_se <- rsm_model$xsi[threshold_indices, "se.xsi"]
} else {
  # Alternative extraction method
  n_cat <- length(unique_values)
  n_items <- length(item_cols)
  step_params <- rsm_model$xsi[(n_items+1):nrow(rsm_model$xsi), "xsi"]
  step_se <- rsm_model$xsi[(n_items+1):nrow(rsm_model$xsi), "se.xsi"]
}

# Create threshold dataframe
n_thresholds <- length(unique_values) - 1
threshold_df <- data.frame(
  Transition = paste0("Cat", 0:(n_thresholds-1), "->", 1:n_thresholds),
  Threshold = step_params[1:n_thresholds],
  SE = step_se[1:n_thresholds]
) %>%
  mutate(
    Lower_CI = Threshold - 1.96 * SE,
    Upper_CI = Threshold + 1.96 * SE
  )

cat("\nRating scale threshold parameters:\n")
print(threshold_df %>% mutate(across(Threshold:Upper_CI, ~round(., 3))), row.names = FALSE)

# Check threshold ordering
ordered_thresholds <- all(diff(threshold_df$Threshold) > 0)
cat("\n=== THRESHOLD ORDERING CHECK ===\n")
if (ordered_thresholds) {
  cat("✓ Thresholds are properly ordered (monotonically increasing)\n")
  cat("  This indicates that the rating scale categories are functioning as intended.\n")
} else {
  cat("✗ Warning: Thresholds are DISORDERED!\n")
  cat("  This suggests problems with the rating scale structure.\n")
  cat("  Some categories may not be functioning as distinct levels.\n")

  # Identify disordered thresholds
  disordered <- which(diff(threshold_df$Threshold) <= 0)
  for (i in disordered) {
    cat(sprintf("  - Threshold %d (%.3f) >= Threshold %d (%.3f)\n",
                i, threshold_df$Threshold[i], i+1, threshold_df$Threshold[i+1]))
  }
  cat("\n  Recommendations:\n")
  cat("  - Consider collapsing adjacent categories\n")
  cat("  - Review category labels and definitions\n")
  cat("  - Check if respondents understand the rating scale\n")
}

# Calculate Andrich thresholds (relative to item difficulty)
andrich_thresholds <- threshold_df$Threshold
cat("\nAndrich thresholds (relative distances between categories):\n")
for (i in 1:length(andrich_thresholds)) {
  cat(sprintf("  Step %d: %.3f logits\n", i, andrich_thresholds[i]))
}

# Category usage analysis
cat("\n=== CATEGORY USAGE ANALYSIS ===\n")
category_counts <- table(unlist(items_recoded))
category_props <- prop.table(category_counts) * 100

category_usage <- data.frame(
  Category = names(category_counts),
  Count = as.numeric(category_counts),
  Percentage = round(as.numeric(category_props), 2)
) %>%
  mutate(
    Original_Value = as.numeric(Category) + min_cat,
    Usage_Status = case_when(
      Percentage < 1 ~ "Very Low",
      Percentage < 5 ~ "Low",
      Percentage < 10 ~ "Moderate",
      TRUE ~ "Adequate"
    )
  )

print(category_usage, row.names = FALSE)

# Check for underused categories
underused <- category_usage %>% filter(Usage_Status %in% c("Very Low", "Low"))
if (nrow(underused) > 0) {
  cat("\nWarning: The following categories are underused:\n")
  print(underused %>% select(Category, Original_Value, Percentage, Usage_Status),
        row.names = FALSE)
  cat("\nConsider collapsing underused categories with adjacent ones.\n")
}

# Average measures by category
cat("\n=== AVERAGE MEASURES BY CATEGORY ===\n")
# This analysis shows if higher categories correspond to higher abilities
person_params <- TAM::tam.wle(rsm_model, progress = FALSE)

# Calculate average ability by response category
category_ability <- data.frame()
for (item in item_cols) {
  item_responses <- items_recoded[[item]]
  for (cat in 0:(length(unique_values)-1)) {
    respondents_in_cat <- which(item_responses == cat)
    if (length(respondents_in_cat) > 0) {
      avg_ability <- mean(person_params$theta[respondents_in_cat], na.rm = TRUE)
      category_ability <- rbind(category_ability,
                               data.frame(Item = item,
                                        Category = cat,
                                        Avg_Ability = avg_ability,
                                        N = length(respondents_in_cat)))
    }
  }
}

# Aggregate across items
avg_measures <- category_ability %>%
  group_by(Category) %>%
  summarise(
    N = sum(N),
    Avg_Ability = weighted.mean(Avg_Ability, N),
    SD_Ability = sqrt(weighted.mean((Avg_Ability - weighted.mean(Avg_Ability, N))^2, N))
  ) %>%
  mutate(Original_Category = Category + min_cat) %>%
  arrange(Category)

print(avg_measures %>% mutate(across(Avg_Ability:SD_Ability, ~round(., 3))),
      row.names = FALSE)

# Check monotonicity
monotonic_increase <- all(diff(avg_measures$Avg_Ability) > 0)
if (monotonic_increase) {
  cat("\n✓ Average abilities increase monotonically with categories (good)\n")
} else {
  cat("\n✗ Warning: Average abilities do not increase monotonically with categories\n")
  cat("  This may indicate category confusion or reversal\n")
}

# Create category probability curves
cat("\nCreating category probability curves...\n")

# Define ability range
theta_range <- seq(-4, 4, by = 0.1)

# Calculate category probabilities
# For RSM, we need the cumulative probabilities
cat_probs <- matrix(0, nrow = length(theta_range), ncol = length(unique_values))

for (i in 1:length(theta_range)) {
  theta <- theta_range[i]

  # Calculate numerators for each category
  numerators <- numeric(length(unique_values))
  numerators[1] <- 1  # Category 0 (baseline)

  if (length(unique_values) > 1) {
    for (k in 2:length(unique_values)) {
      # Cumulative sum of thresholds
      if (k <= length(andrich_thresholds) + 1) {
        cum_threshold <- sum(andrich_thresholds[1:(k-1)])
        numerators[k] <- exp((k-1) * theta - cum_threshold)
      }
    }
  }

  # Calculate probabilities
  cat_probs[i,] <- numerators / sum(numerators)
}

# Create visualization data
prob_viz_data <- expand.grid(
  Theta = theta_range,
  Category = 0:(length(unique_values)-1)
) %>%
  mutate(
    Probability = as.vector(cat_probs),
    Original_Category = Category + min_cat,
    Category_Label = paste("Category", Original_Category)
  )

# Create the plot
cat_prob_plot <- ggplot(prob_viz_data,
                        aes(x = Theta, y = Probability, color = factor(Category_Label))) +
  geom_line(size = 1.2) +
  scale_color_viridis_d(option = "plasma") +
  labs(title = "Rating Scale Category Probability Curves",
       subtitle = ifelse(multidim_warning,
                        "Note: Based on unidimensional model despite multidimensional data",
                        "Probability of selecting each category across the ability spectrum"),
       x = "Person Ability (logits)",
       y = "Probability",
       color = "Response") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = ifelse(multidim_warning, "red", "black")),
    legend.position = "right"
  ) +
  coord_cartesian(ylim = c(0, 1))

ggsave(file.path(output_dir, "category_probability_curves.pdf"),
       plot = cat_prob_plot, width = 10, height = 6, dpi = 300)

# Find category peaks (where each category is most probable)
cat_peaks <- prob_viz_data %>%
  group_by(Theta) %>%
  slice_max(Probability, n = 1) %>%
  group_by(Category_Label) %>%
  summarise(
    Peak_Theta = mean(Theta),
    Max_Probability = max(Probability)
  ) %>%
  arrange(Peak_Theta)

cat("\nCategory peak locations:\n")
print(cat_peaks %>% mutate(across(Peak_Theta:Max_Probability, ~round(., 3))),
      row.names = FALSE)

# Check if all categories have distinct peaks
if (nrow(cat_peaks) < length(unique_values)) {
  cat("\nWarning: Not all categories have distinct peaks.\n")
  cat("Some categories may never be the most probable response.\n")
}

# ======================================================
# 6. PERSON PARAMETERS AND RELIABILITY
# ======================================================

cat("\n============================================================\n")
cat("PERSON PARAMETERS AND RELIABILITY\n")
cat("============================================================\n")

# Person parameters were already calculated
cat("Calculating person parameters...\n")

# Create person summary
person_summary <- data.frame(
  Person_ID = 1:nrow(items),
  Raw_Score = rowSums(items_recoded, na.rm = TRUE),
  Max_Score = rowSums(!is.na(items_recoded)) * (length(unique_values) - 1),
  N_Items = rowSums(!is.na(items_recoded)),
  Theta = person_params$theta,
  SE = person_params$error
) %>%
  mutate(
    Percent_Score = round(Raw_Score / Max_Score * 100, 1),
    Reliability = 1 - (SE^2 / var(Theta))
  )

# Summary statistics
person_stats <- person_summary %>%
  summarise(
    N = n(),
    Mean_Theta = mean(Theta),
    SD_Theta = sd(Theta),
    Min_Theta = min(Theta),
    Max_Theta = max(Theta),
    Mean_SE = mean(SE),
    Median_SE = median(SE)
  ) %>%
  mutate(across(everything(), ~round(., 3)))

cat("\n=== PERSON PARAMETER SUMMARY ===\n")
print(person_stats, row.names = FALSE)

# Calculate reliability indices
cat("\n=== RELIABILITY INDICES ===\n")

# WLE reliability (Wright & Masters, 1982)
wle_reliability <- 1 - (mean(person_params$error^2) / var(person_params$theta))
cat("WLE Person Separation Reliability:", round(wle_reliability, 3), "\n")

# EAP reliability if available
if ("EAP" %in% names(rsm_model)) {
  eap_reliability <- rsm_model$EAP.rel
  cat("EAP Reliability:", round(eap_reliability, 3), "\n")
}

# Person separation index
person_separation <- sqrt(var(person_params$theta) / mean(person_params$error^2) - 1)
cat("Person Separation Index:", round(person_separation, 2), "\n")
cat("Number of distinct ability strata:", round((4 * person_separation + 1) / 3, 1), "\n")

# Classical test theory reliability (for comparison)
if (length(item_cols) > 1) {
  cronbach_alpha <- psych::alpha(items_recoded, warnings = FALSE)$total$raw_alpha
  cat("Cronbach's Alpha (for comparison):", round(cronbach_alpha, 3), "\n")
}

# Interpretation
cat("\n=== RELIABILITY INTERPRETATION ===\n")
reliability_interpretation <- case_when(
  wle_reliability >= 0.9 ~ "Excellent - suitable for high-stakes individual decisions",
  wle_reliability >= 0.8 ~ "Good - suitable for individual assessment",
  wle_reliability >= 0.7 ~ "Acceptable - suitable for group comparisons",
  wle_reliability >= 0.6 ~ "Marginal - use with caution",
  TRUE ~ "Poor - not suitable for measurement purposes"
)
cat("Interpretation:", reliability_interpretation, "\n")

if (multidim_warning) {
  cat("\nNote: Reliability may be inflated due to multidimensionality.\n")
  cat("Consider calculating reliability separately for each dimension.\n")
}

# Test information
cat("\n=== TEST INFORMATION ===\n")
test_info_theta <- seq(-4, 4, by = 0.5)
test_info <- numeric(length(test_info_theta))

for (i in 1:length(test_info_theta)) {
  # For RSM, information is sum of category information across items
  theta <- test_info_theta[i]
  item_info <- 0

  # Calculate information for each item
  for (j in 1:length(item_cols)) {
    # Get probabilities for this theta
    probs <- numeric(length(unique_values))
    probs[1] <- 1

    if (length(unique_values) > 1) {
      for (k in 2:length(unique_values)) {
        if (k <= length(andrich_thresholds) + 1) {
          cum_thresh <- sum(andrich_thresholds[1:(k-1)])
          probs[k] <- exp((k-1) * (theta - item_difficulties[j]) - cum_thresh)
        }
      }
    }
    probs <- probs / sum(probs)

    # Calculate information (variance of scores)
    expected <- sum((0:(length(unique_values)-1)) * probs)
    variance <- sum((0:(length(unique_values)-1))^2 * probs) - expected^2
    item_info <- item_info + variance
  }

  test_info[i] <- item_info
}

test_info_df <- data.frame(
  Theta = test_info_theta,
  Information = round(test_info, 2),
  SE = round(1/sqrt(test_info), 3)
)

cat("Test information across ability range:\n")
print(test_info_df, row.names = FALSE)

# Maximum information
max_info_idx <- which.max(test_info)
cat("\nMaximum test information:", round(max(test_info), 2),
    "at theta =", test_info_theta[max_info_idx], "\n")

# Save person parameters
write_csv(person_summary, file.path(output_dir, "person_parameters.csv"))
cat("\nPerson parameters saved.\n")

# Create person distribution plot
person_dist_plot <- ggplot(person_summary, aes(x = Theta)) +
  geom_histogram(aes(y = ..density..), bins = 30,
                 fill = "skyblue", color = "white", alpha = 0.8) +
  geom_density(color = "darkblue", size = 1) +
  geom_vline(xintercept = person_stats$Mean_Theta,
             linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = c(person_stats$Mean_Theta - person_stats$SD_Theta,
                           person_stats$Mean_Theta + person_stats$SD_Theta),
             linetype = "dotted", color = "red", size = 0.8) +
  labs(title = "Distribution of Person Abilities",
       subtitle = paste("Mean =", round(person_stats$Mean_Theta, 2),
                       ", SD =", round(person_stats$SD_Theta, 2),
                       ", N =", person_stats$N),
       x = "Person Ability (logits)",
       y = "Density") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave(file.path(output_dir, "person_distribution.pdf"),
       plot = person_dist_plot, width = 10, height = 6, dpi = 300)

# ======================================================
# 7. WRIGHT MAP
# ======================================================

cat("\n============================================================\n")
cat("WRIGHT MAP\n")
cat("============================================================\n")

# Prepare Wright Map
cat("Creating Wright Map...\n")

# Use item difficulties from the model
item_locations <- data.frame(
  Item = item_cols,
  Difficulty = item_difficulties
) %>%
  arrange(Difficulty)

# Add dimension information if multidimensional
if (n_dimensions > 1 && exists("loadings_df")) {
  item_locations <- item_locations %>%
    left_join(loadings_df %>% select(Item, Primary_Factor), by = "Item")
}

# Create Wright Map
pdf(file.path(output_dir, "wright_map.pdf"), width = 10, height = 8)

# Set up plot area
par(mar = c(5, 4, 4, 4))

# Create the Wright Map using WrightMap package
WrightMap::wrightMap(
  thetas = person_params$theta,
  thresholds = matrix(item_difficulties, ncol = 1),
  label.items = item_cols,
  main.title = ifelse(multidim_warning,
                     "Wright Map: Person-Item Map (Note: Multidimensional Data)",
                     "Wright Map: Person-Item Map"),
  label.items.srt = 45,
  axis.persons = "Persons",
  axis.items = "Items",
  min.logit.pad = 0.5,
  max.logit.pad = 0.5,
  item.prop = 0.5,
  return.thresholds = FALSE,
  use.hist = TRUE
)

dev.off()
cat("Wright Map saved.\n")

# Also create a custom Wright Map with ggplot2
cat("Creating enhanced Wright Map...\n")

# Prepare data for custom Wright Map
person_hist_data <- hist(person_params$theta, breaks = 30, plot = FALSE)
person_dist_df <- data.frame(
  Theta = person_hist_data$mids,
  Count = person_hist_data$counts
) %>%
  mutate(Proportion = Count / sum(Count))

# Create the enhanced plot
wright_map_gg <- ggplot() +
  # Person distribution (left side)
  geom_col(data = person_dist_df,
           aes(x = -Proportion, y = Theta),
           fill = "skyblue", alpha = 0.7, width = 0.2) +
  # Item difficulties (right side)
  geom_point(data = item_locations,
             aes(x = 0.1, y = Difficulty,
                 color = if(n_dimensions > 1 && "Primary_Factor" %in% names(item_locations))
                   factor(Primary_Factor) else "All"),
             size = 3) +
  geom_text(data = item_locations,
            aes(x = 0.15, y = Difficulty, label = Item),
            hjust = 0, size = 3) +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, color = "black") +
  # Scales and labels
  scale_x_continuous(
    breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
    labels = c("20%", "10%", "0", "", ""),
    limits = c(-0.25, 0.4)
  ) +
  labs(title = "Enhanced Wright Map",
       subtitle = ifelse(multidim_warning,
                        "Distribution of person abilities and item difficulties (multidimensional data)",
                        "Distribution of person abilities and item difficulties"),
       x = "Proportion of Persons | Items",
       y = "Measure (logits)",
       color = "Dimension") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5,
                                color = ifelse(multidim_warning, "red", "black")),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = if(n_dimensions > 1) "bottom" else "none"
  ) +
  annotate("text", x = -0.12, y = max(person_params$theta) + 0.5,
           label = "Persons", fontface = "bold") +
  annotate("text", x = 0.2, y = max(item_difficulties) + 0.5,
           label = "Items", fontface = "bold")

# Add color scale if multidimensional
if (n_dimensions > 1 && "Primary_Factor" %in% names(item_locations)) {
  wright_map_gg <- wright_map_gg +
    scale_color_viridis_d(option = "plasma")
}

ggsave(file.path(output_dir, "wright_map_enhanced.pdf"),
       plot = wright_map_gg, width = 8, height = 10, dpi = 300)

# Analyze person-item targeting
cat("\n=== PERSON-ITEM TARGETING ANALYSIS ===\n")

targeting <- list(
  mean_person = mean(person_params$theta),
  mean_item = mean(item_difficulties),
  difference = mean(person_params$theta) - mean(item_difficulties),
  sd_person = sd(person_params$theta),
  sd_item = sd(item_difficulties),
  range_person = range(person_params$theta),
  range_item = range(item_difficulties)
)

cat("Mean person ability:", round(targeting$mean_person, 3), "logits\n")
cat("Mean item difficulty:", round(targeting$mean_item, 3), "logits\n")
cat("Difference (targeting):", round(targeting$difference, 3), "logits\n")

# Targeting interpretation
if (abs(targeting$difference) < 0.5) {
  cat("\nGood targeting: Assessment difficulty well-matched to sample ability.\n")
} else if (targeting$difference > 0.5) {
  cat("\nAssessment may be too easy for this sample.\n")
  cat("Consider adding more difficult items.\n")
} else {
  cat("\nAssessment may be too difficult for this sample.\n")
  cat("Consider adding easier items.\n")
}

# Coverage analysis
cat("\nCoverage analysis:\n")
cat("Person ability range:", round(targeting$range_person[1], 2),
    "to", round(targeting$range_person[2], 2), "\n")
cat("Item difficulty range:", round(targeting$range_item[1], 2),
    "to", round(targeting$range_item[2], 2), "\n")

# Identify gaps
item_diff_sorted <- sort(item_difficulties)
gaps <- diff(item_diff_sorted)
large_gaps <- which(gaps > 1.0)  # Gaps larger than 1 logit

if (length(large_gaps) > 0) {
  cat("\nLarge gaps in item coverage:\n")
  for (gap_idx in large_gaps) {
    cat(sprintf("  Between %.2f and %.2f logits (gap = %.2f)\n",
                item_diff_sorted[gap_idx],
                item_diff_sorted[gap_idx + 1],
                gaps[gap_idx]))
  }
}

# ======================================================
# 8. PERSON CATEGORIZATION
# ======================================================

cat("\n============================================================\n")
cat("PERSON CATEGORIZATION\n")
cat("============================================================\n")

# Define categories based on distribution
cat("Defining readiness categories based on person distribution...\n")

# Use percentile-based approach for more stable categories
percentiles <- quantile(person_params$theta, probs = c(0, 0.33, 0.67, 1))

# Or use fixed cutoffs based on SD
cutoff_lower <- targeting$mean_person - targeting$sd_person
cutoff_upper <- targeting$mean_person + targeting$sd_person

# Create categories
person_summary <- person_summary %>%
  mutate(
    Category_Percentile = cut(Theta,
                             breaks = percentiles,
                             labels = c("Developing", "Proficient", "Advanced"),
                             include.lowest = TRUE),
    Category_SD = cut(Theta,
                     breaks = c(-Inf, cutoff_lower, cutoff_upper, Inf),
                     labels = c("Developing", "Proficient", "Advanced"),
                     include.lowest = TRUE)
  )

# Category distribution
cat("\n=== READINESS CATEGORIES (SD-based) ===\n")
cat_dist_sd <- person_summary %>%
  count(Category_SD) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))

print(cat_dist_sd, row.names = FALSE)

cat("\n=== READINESS CATEGORIES (Percentile-based) ===\n")
cat_dist_pct <- person_summary %>%
  count(Category_Percentile) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))

print(cat_dist_pct, row.names = FALSE)

# Detailed category descriptions
cat("\n=== CATEGORY DESCRIPTIONS ===\n")

cat("\n1. DEVELOPING (Bottom third or < Mean - 1SD):\n")
cat("   - Beginning understanding of AI ethics principles\n")
cat("   - May struggle with complex ethical scenarios\n")
cat("   - Requires foundational training and support\n")
cat("   - Focus on basic concepts and awareness building\n")

cat("\n2. PROFICIENT (Middle third or Mean ± 1SD):\n")
cat("   - Solid understanding of AI ethics principles\n")
cat("   - Can identify and analyze common ethical issues\n")
cat("   - Ready for practical application with guidance\n")
cat("   - Benefits from advanced training and case studies\n")

cat("\n3. ADVANCED (Top third or > Mean + 1SD):\n")
cat("   - Comprehensive understanding of AI ethics\n")
cat("   - Can navigate complex ethical dilemmas independently\n")
cat("   - Ready to lead ethics initiatives and mentor others\n")
cat("   - Suitable for policy development and decision-making roles\n")

# Create raw score conversion table
cat("\n=== RAW SCORE CONVERSION TABLE ===\n")

conversion_table <- person_summary %>%
  group_by(Raw_Score) %>%
  summarise(
    N = n(),
    Mean_Theta = mean(Theta),
    SD_Theta = sd(Theta),
    Category_SD = names(sort(table(Category_SD), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  arrange(Raw_Score) %>%
  mutate(
    Mean_Theta = round(Mean_Theta, 3),
    SD_Theta = round(SD_Theta, 3),
    Percentile = round(percent_rank(Mean_Theta) * 100, 0)
  )

# Display a sample of the conversion table
cat("Sample of raw score to ability conversion:\n")
print(head(conversion_table, 10), row.names = FALSE)

# Save full conversion table
write_csv(conversion_table, file.path(output_dir, "score_conversion_table.csv"))
cat("\nFull conversion table saved.\n")

# ======================================================
# 9. ANOVA ANALYSIS
# ======================================================

cat("\n============================================================\n")
cat("ANOVA ANALYSIS\n")
cat("============================================================\n")
cat("Analyzing variance in person measures across demographic groups...\n\n")

# Combine person parameters with demographic data
person_data <- person_summary %>%
  bind_cols(data %>% select(any_of(demographic_vars)))

# Function to safely perform ANOVA
safe_anova <- function(formula, data) {
  tryCatch({
    model <- aov(formula, data = data)
    summary_result <- summary(model)
    f_value <- summary_result[[1]]$"F value"[1]
    p_value <- summary_result[[1]]$"Pr(>F)"[1]
    return(list(model = model, summary = summary_result, f_value = f_value, p_value = p_value))
  }, error = function(e) {
    cat("Error in ANOVA:", e$message, "\n")
    return(list(model = NULL, summary = NULL, f_value = NA, p_value = NA))
  })
}

# Initialize ANOVA results
anova_results <- list()

for (dem_var in demographic_vars) {
    if (dem_var %in% names(person_data)) {
        cat(paste0("\n### ANOVA analysis based on ", dem_var, " ###\n"))
        formula <- as.formula(paste("Theta ~", dem_var))
        anova_results[[dem_var]] <- safe_anova(formula, person_data)
        if (!is.null(anova_results[[dem_var]]$summary)) {
            print(anova_results[[dem_var]]$summary)
        }
    }
}


# Create ANOVA table
if (length(anova_results) > 0) {
    anova_summary_list <- lapply(names(anova_results), function(var) {
      res <- anova_results[[var]]
      data.frame(
        Demographic_Variable = var,
        F_test = ifelse(is.na(res$f_value), "NA", sprintf("%.2f", res$f_value)),
        p_value = ifelse(is.na(res$p_value), "NA", sprintf("%.3f", res$p_value)),
        Significant = ifelse(!is.na(res$p_value) && res$p_value < 0.05, "*", "")
      )
    })

    if (length(anova_summary_list) > 0) {
        anova_table <- do.call(rbind, anova_summary_list)
        cat("\nANOVA Summary Table:\n")
        print(anova_table, row.names = FALSE)
        if (any(anova_table$Significant == "*", na.rm = TRUE)) {
            cat("Note(s): * p < 0.05\n")
        } else {
            cat("Note(s): No significant differences found\n")
        }
    }
}


# Create enhanced visualization of ANOVA results
cat("\nCreating ANOVA visualization...\n")

# Boxplots for group of demographic variable
anova_plots <- list()
for (dem_var in demographic_vars) {
    if (dem_var %in% names(person_data) && nlevels(person_data[[dem_var]]) > 1) {
        p <- ggplot(person_data, aes_string(x = dem_var, y = "Theta")) +
            geom_boxplot(fill = "skyblue", alpha = 0.7, outlier.shape = NA) +
            geom_jitter(width = 0.2, alpha = 0.3) +
            labs(title = paste("Person Measures by", str_to_title(gsub("_", " ", dem_var))),
                 x = str_to_title(gsub("_", " ", dem_var)),
                 y = "Person Measure (logits)") +
            theme_minimal(base_size = 12) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
                  plot.title = element_text(hjust = 0.5, face = "bold"))
        anova_plots[[dem_var]] <- p
    }
}

# Save plots
if (length(anova_plots) > 0) {
    anova_plot_grid <- cowplot::plot_grid(plotlist = anova_plots, ncol = 2)
    ggsave(file.path(output_dir, "anova_boxplots.pdf"),
           plot = anova_plot_grid, width = 12, height = 10, dpi = 300)
    cat("ANOVA boxplots saved to anova_boxplots.pdf\n")
}

# ======================================================
# 10. DIFFERENTIAL ITEM FUNCTIONING (DIF)
# ======================================================

cat("\n============================================================\n")
cat("DIFFERENTIAL ITEM FUNCTIONING (DIF) ANALYSIS\n")
cat("============================================================\n")

if (multidim_warning) {
  cat("Note: DIF analysis may be confounded by multidimensionality.\n")
  cat("Results should be interpreted with caution.\n\n")
}

# Function to perform DIF analysis
perform_dif <- function(items_data, group_var, group_name, min_group_size = 30) {

  # Check if group variable exists and has valid values
  if (!group_var %in% names(data)) {
    cat(sprintf("Variable '%s' not found in dataset. Skipping DIF analysis.\n", group_var))
    return(NULL)
  }

  group_data <- data[[group_var]]

  # Remove missing values
  valid_idx <- !is.na(group_data)
  items_subset <- items_data[valid_idx, ]
  group_subset <- group_data[valid_idx]

  # Get group frequencies
  group_table <- table(group_subset)

  # Find groups with sufficient size
  valid_groups <- names(group_table[group_table >= min_group_size])

  if (length(valid_groups) < 2) {
    cat(sprintf("Insufficient group sizes for %s. Need at least 2 groups with n >= %d.\n",
                group_name, min_group_size))
    return(NULL)
  }

  # Select two largest groups
  two_groups <- names(sort(group_table[valid_groups], decreasing = TRUE)[1:2])

  cat(sprintf("\nDIF analysis by %s:\n", group_name))
  cat(sprintf("Comparing %s (n=%d) vs %s (n=%d)\n",
              two_groups[1], group_table[two_groups[1]],
              two_groups[2], group_table[two_groups[2]]))

  # Create binary group indicator
  focal_idx <- group_subset == two_groups[2]
  analysis_items <- items_subset[group_subset %in% two_groups, ]
  analysis_group <- as.numeric(focal_idx[group_subset %in% two_groups])

  # Perform DIF analysis
  dif_result <- tryCatch({
    difR::difLogistic(
      Data = as.data.frame(analysis_items),
      group = analysis_group,
      focal.name = 1,
      model = "Rasch",
      purify = TRUE,
      nrIter = 10
    )
  }, error = function(e) {
    cat("Error in DIF analysis:", e$message, "\n")
    return(NULL)
  })

  return(dif_result)
}

# Perform DIF analyses for available demographic variables
dif_results <- list()

# Gender DIF
if ("gender" %in% names(data)) {
  cat("\n--- DIF Analysis by Gender ---\n")
  dif_results$gender <- perform_dif(items_recoded, "gender", "Gender")

  if (!is.null(dif_results$gender)) {
    dif_items_gender <- dif_results$gender$DIFitems
    if (length(dif_items_gender) > 0) {
      cat("\nItems showing DIF by gender:\n")
      for (item_idx in dif_items_gender) {
        cat(sprintf("  - %s (Effect size: %.3f)\n",
                    item_cols[item_idx],
                    dif_results$gender$deltaR2[item_idx]))
      }
    } else {
      cat("No items show significant DIF by gender.\n")
    }
  }
}

# Job Title DIF
if ("job_title" %in% names(data)) {
  cat("\n--- DIF Analysis by Job Title ---\n")
  dif_results$job_title <- perform_dif(items_recoded, "job_title", "Job Title")

  if (!is.null(dif_results$job_title)) {
    dif_items_job <- dif_results$job_title$DIFitems
    if (length(dif_items_job) > 0) {
      cat("\nItems showing DIF by job title:\n")
      for (item_idx in dif_items_job) {
        cat(sprintf("  - %s (Effect size: %.3f)\n",
                    item_cols[item_idx],
                    dif_results$job_title$deltaR2[item_idx]))
      }
    } else {
      cat("No items show significant DIF by job title.\n")
    }
  }
}

# Education DIF
if ("education" %in% names(data)) {
  cat("\n--- DIF Analysis by Education ---\n")
  dif_results$education <- perform_dif(items_recoded, "education", "Education Level")

  if (!is.null(dif_results$education)) {
    dif_items_edu <- dif_results$education$DIFitems
    if (length(dif_items_edu) > 0) {
      cat("\nItems showing DIF by education:\n")
      for (item_idx in dif_items_edu) {
        cat(sprintf("  - %s (Effect size: %.3f)\n",
                    item_cols[item_idx],
                    dif_results$education$deltaR2[item_idx]))
      }
    } else {
      cat("No items show significant DIF by education level.\n")
    }
  }
}

# Summary of DIF findings
cat("\n=== DIF SUMMARY ===\n")
all_dif_items <- unique(unlist(lapply(dif_results, function(x) {
  if (!is.null(x)) x$DIFitems else NULL
})))

if (length(all_dif_items) > 0) {
  cat("Total items showing DIF:", length(all_dif_items), "\n")
  cat("Items with DIF:\n")
  for (item_idx in all_dif_items) {
    cat("  -", item_cols[item_idx], "\n")
  }
  cat("\nRecommendation: Review these items for potential bias.\n")
} else {
  cat("No items show significant DIF across analyzed groups.\n")
  cat("The assessment appears to function equivalently across demographic groups.\n")
}

# ======================================================
# 11. COMPREHENSIVE REPORT
# ======================================================

cat("\n============================================================\n")
cat("GENERATING COMPREHENSIVE REPORT\n")
cat("============================================================\n")

# Create comprehensive report
report_file <- file.path(output_dir, "rasch_analysis_report.txt")
sink(report_file)

cat("============================================================\n")
cat("RATING SCALE MODEL (RSM) ANALYSIS REPORT\n")
cat("============================================================\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R Version:", R.version.string, "\n")
cat("Script Version: 2025.05.24.v2 (Enhanced with Multidimensional Support)\n\n")

cat("=== EXECUTIVE SUMMARY ===\n")
cat("This report presents a comprehensive Rasch Rating Scale Model analysis\n")
cat("of an AI ethics assessment instrument.\n\n")

cat("Key Findings:\n")
cat("- Number of respondents:", nrow(data), "\n")
cat("- Number of items:", length(item_cols), "\n")
cat("- Rating scale categories:", length(unique_values), "\n")
cat("- Dimensionality:", n_dimensions, "dimension(s) detected\n")
cat("- WLE Reliability:", round(wle_reliability, 3), "\n")
cat("- Items with good fit:",
    sum(item_fit$Overall_Fit %in% c("Excellent", "Acceptable")),
    "/", length(item_cols), "\n")
cat("- Evidence for unidimensionality:",
    ifelse(n_criteria_met >= 3, "Yes", "Limited"), "\n\n")

# Add multidimensional warning if applicable
if (multidim_warning) {
  cat("*** IMPORTANT NOTE ***\n")
  cat("The data shows evidence of multidimensionality (", n_dimensions, "dimensions).\n")
  cat("While the analysis proceeded with a unidimensional RSM model,\n")
  cat("results should be interpreted with caution.\n")
  cat("Consider:\n")
  cat("1. Analyzing dimensions separately\n")
  cat("2. Using multidimensional IRT models\n")
  cat("3. Selecting items from a single dimension\n\n")
}

# Include detailed findings
cat("=== DETAILED FINDINGS ===\n\n")

cat("1. UNIDIMENSIONALITY ASSESSMENT\n")
cat("   - Parallel analysis suggested:", n_dimensions, "dimension(s)\n")
cat("   - First factor variance explained:",
    round(scree_data$Proportion[1], 1), "%\n")
cat("   - Mean inter-item correlation:",
    round(cor_stats$Value[cor_stats$Statistic == "Mean"], 3), "\n")
cat("   - KMO MSA:", round(kmo_test$MSA, 3), "\n")

if (n_dimensions > 1) {
  cat("\n   MULTIDIMENSIONAL STRUCTURE:\n")
  for (i in 1:n_dimensions) {
    dim_items <- loadings_df %>% filter(Primary_Factor == i) %>% pull(Item)
    cat(sprintf("   - Dimension %d: %d items (%s)\n",
                i, length(dim_items),
                paste(head(dim_items, 3), collapse = ", ")))
  }
}

cat("\n2. MODEL FIT\n")
cat("   - Model converged:", ifelse(rsm_model$iter < rsm_control$maxiter, "Yes", "No"), "\n")
cat("   - AIC:", round(rsm_model$AIC, 2), "\n")
cat("   - BIC:", round(rsm_model$BIC, 2), "\n")

cat("\n3. ITEM QUALITY\n")
cat("   - Items with excellent fit:",
    sum(item_fit$Overall_Fit == "Excellent"), "\n")
cat("   - Items with acceptable fit:",
    sum(item_fit$Overall_Fit == "Acceptable"), "\n")
cat("   - Items with problematic fit:",
    sum(item_fit$Overall_Fit == "Problematic"), "\n")

cat("\n4. RATING SCALE FUNCTIONING\n")
cat("   - Thresholds ordered:", ifelse(ordered_thresholds, "Yes", "No"), "\n")
cat("   - Categories with low usage (<5%):",
    sum(category_usage$Usage_Status %in% c("Low", "Very Low")), "\n")
cat("   - Monotonic category progression:",
    ifelse(monotonic_increase, "Yes", "No"), "\n")

cat("\n5. MEASUREMENT PRECISION\n")
cat("   - WLE Reliability:", round(wle_reliability, 3), "\n")
cat("   - Person Separation Index:", round(person_separation, 2), "\n")
cat("   - Test targets at theta =", test_info_theta[max_info_idx],
    "with max info =", round(max(test_info), 2), "\n")

cat("\n6. DIFFERENTIAL ITEM FUNCTIONING\n")
cat("   - Total items showing DIF:", length(all_dif_items), "\n")
if (length(all_dif_items) > 0) {
  cat("   - Items with DIF:", paste(item_cols[all_dif_items], collapse = ", "), "\n")
}

# Close report file
sink()
cat("Comprehensive report saved to:", report_file, "\n")

# ======================================================
# 12. CREATE SUMMARY DASHBOARD
# ======================================================

cat("\n============================================================\n")
cat("CREATING SUMMARY DASHBOARD\n")
cat("============================================================\n")

# Create a one-page summary dashboard
dashboard_plots <- list()

# 1. Model fit overview
fit_overview <- item_fit %>%
  count(Overall_Fit) %>%
  ggplot(aes(x = Overall_Fit, y = n, fill = Overall_Fit)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_manual(values = c("Excellent" = "darkgreen",
                              "Acceptable" = "orange",
                              "Problematic" = "red")) +
  labs(title = "Item Fit Overview",
       x = "Fit Status", y = "Number of Items") +
  theme_minimal() +
  theme(legend.position = "none")

dashboard_plots[[1]] <- fit_overview

# 2. Person distribution
person_dist_mini <- ggplot(person_summary, aes(x = Theta)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "white") +
  geom_vline(xintercept = targeting$mean_person,
             linetype = "dashed", color = "red") +
  labs(title = "Person Ability Distribution",
       x = "Ability (logits)", y = "Count") +
  theme_minimal()

dashboard_plots[[2]] <- person_dist_mini

# 3. Category usage
cat_usage_plot <- category_usage %>%
  ggplot(aes(x = factor(Original_Value), y = Percentage,
             fill = Usage_Status)) +
  geom_col() +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Adequate" = "darkgreen",
                              "Moderate" = "yellow",
                              "Low" = "orange",
                              "Very Low" = "red")) +
  labs(title = "Rating Scale Usage",
       x = "Category", y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "bottom")

dashboard_plots[[3]] <- cat_usage_plot

# 4. Key metrics
metrics_data <- data.frame(
  Metric = c("Reliability", "Unidimensional", "Good Fit %", "Ordered Thresholds"),
  Value = c(
    round(wle_reliability, 3),
    ifelse(n_dimensions == 1, 1, 0),
    round(sum(item_fit$Overall_Fit %in% c("Excellent", "Acceptable")) /
            length(item_cols), 2),
    ifelse(ordered_thresholds, 1, 0)
  )
)

metrics_plot <- metrics_data %>%
  ggplot(aes(x = Metric, y = Value, fill = Value > 0.7)) +
  geom_col() +
  geom_text(aes(label = Value), vjust = -0.5) +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "orange")) +
  labs(title = "Key Quality Indicators",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

dashboard_plots[[4]] <- metrics_plot

# Add dimensionality plot if multidimensional
if (n_dimensions > 1 && exists("loadings_df")) {
  dim_summary <- loadings_df %>%
    count(Primary_Factor) %>%
    mutate(Dimension = paste("Dim", Primary_Factor))

  dim_plot <- ggplot(dim_summary, aes(x = Dimension, y = n, fill = Dimension)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5) +
    scale_fill_viridis_d() +
    labs(title = "Items per Dimension",
         x = "Dimension", y = "Number of Items") +
    theme_minimal() +
    theme(legend.position = "none")

  dashboard_plots[[5]] <- dim_plot
}

# Combine into dashboard
if (length(dashboard_plots) == 5) {
  dashboard <- (dashboard_plots[[1]] + dashboard_plots[[2]] + dashboard_plots[[5]]) /
               (dashboard_plots[[3]] + dashboard_plots[[4]] + plot_spacer()) +
    plot_annotation(
      title = "Rasch Analysis Dashboard",
      subtitle = paste("Dataset:", data_file, "| Date:", format(Sys.Date(), "%Y-%m-%d"),
                      "| Dimensions:", n_dimensions),
      theme = theme(plot.title = element_text(size = 16, face = "bold"))
    )
} else {
  dashboard <- (dashboard_plots[[1]] + dashboard_plots[[2]]) /
               (dashboard_plots[[3]] + dashboard_plots[[4]]) +
    plot_annotation(
      title = "Rasch Analysis Dashboard",
      subtitle = paste("Dataset:", data_file, "| Date:", format(Sys.Date(), "%Y-%m-%d")),
      theme = theme(plot.title = element_text(size = 16, face = "bold"))
    )
}

ggsave(file.path(output_dir, "analysis_dashboard.pdf"),
       plot = dashboard, width = 12, height = 10, dpi = 300)

cat("Summary dashboard created.\n")

# ======================================================
# 13. EXPORT KEY RESULTS FOR FURTHER ANALYSIS
# ======================================================

cat("\n============================================================\n")
cat("EXPORTING KEY RESULTS\n")
cat("============================================================\n")

# Create a comprehensive results list
analysis_results <- list(
  # Data summary
  data_summary = data_summary,

  # Dimensionality
  dimensionality = list(
    n_dimensions = n_dimensions,
    factor_loadings = if(exists("loadings_df")) loadings_df else NULL,
    parallel_analysis = pa_result,
    unidimensionality_criteria = if(exists("unidim_criteria")) unidim_criteria else NULL
  ),

  # Model information
  model_info = list(
    model = rsm_model,
    convergence = rsm_model$iter < rsm_control$maxiter,
    fit_statistics = data.frame(
      Statistic = c("Log-likelihood", "AIC", "BIC", "CAIC", "N_parameters"),
      Value = c(rsm_model$loglike, rsm_model$AIC, rsm_model$BIC,
                rsm_model$CAIC, rsm_model$Npars)
    )
  ),

  # Item statistics
  item_results = list(
    item_fit = item_fit,
    item_summary = item_summary,
    problematic_items = problematic_items
  ),

  # Rating scale
  rating_scale = list(
    threshold_parameters = threshold_df,
    ordered_thresholds = ordered_thresholds,
    category_usage = category_usage,
    category_peaks = cat_peaks
  ),

  # Person parameters
  person_results = list(
    person_summary = person_summary,
    person_stats = person_stats,
    conversion_table = conversion_table
  ),

  # Reliability
  reliability = list(
    wle_reliability = wle_reliability,
    person_separation = person_separation,
    cronbach_alpha = if(exists("cronbach_alpha")) cronbach_alpha else NULL,
    test_information = test_info_df
  ),

  # Targeting
  targeting = targeting,

  # DIF results
  dif_results = dif_results,

  # Warnings
  warnings = list(
    multidimensional = multidim_warning,
    disordered_thresholds = !ordered_thresholds,
    low_reliability = wle_reliability < 0.7,
    poor_targeting = abs(targeting$difference) > 1
  )
)

# Save comprehensive results
saveRDS(analysis_results, file = file.path(output_dir, "analysis_results.rds"))
cat("Comprehensive results saved to:", file.path(output_dir, "analysis_results.rds"), "\n")

# Export key tables to Excel for easier access
cat("\nExporting key tables to Excel...\n")
library(openxlsx)

wb <- createWorkbook()

# Add sheets
addWorksheet(wb, "Item_Statistics")
writeData(wb, "Item_Statistics", item_fit)

addWorksheet(wb, "Person_Parameters")
writeData(wb, "Person_Parameters", person_summary)

addWorksheet(wb, "Conversion_Table")
writeData(wb, "Conversion_Table", conversion_table)

addWorksheet(wb, "Threshold_Parameters")
writeData(wb, "Threshold_Parameters", threshold_df)

addWorksheet(wb, "Category_Usage")
writeData(wb, "Category_Usage", category_usage)

if (n_dimensions > 1 && exists("loadings_df")) {
  addWorksheet(wb, "Factor_Loadings")
  writeData(wb, "Factor_Loadings", loadings_df)
}

# Save Excel file
saveWorkbook(wb, file.path(output_dir, "analysis_results.xlsx"), overwrite = TRUE)
cat("Results exported to Excel:", file.path(output_dir, "analysis_results.xlsx"), "\n")

# ======================================================
# 14. FINAL CLEANUP AND SUMMARY
# ======================================================

cat("\n============================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================================\n")

# Calculate total execution time
total_time <- difftime(Sys.time(), start_time, units = "mins")

cat("\nExecution Summary:\n")
cat("- Total execution time:", round(total_time, 1), "minutes\n")
cat("- Output directory:", normalizePath(output_dir), "\n")
cat("- Number of files created:", length(list.files(output_dir)), "\n")

# List all output files
cat("\nOutput files created:\n")
output_files <- list.files(output_dir, full.names = FALSE)
for (file in output_files) {
  file_size <- file.size(file.path(output_dir, file))
  cat(sprintf("  - %s (%.1f KB)\n", file, file_size / 1024))
}

# Key recommendations summary
cat("\n=== KEY RECOMMENDATIONS ===\n")

# 1. Dimensionality recommendations
if (n_dimensions > 1) {
  cat("1. ADDRESS MULTIDIMENSIONALITY:\n")
  cat("   - Data shows", n_dimensions, "dimensions\n")
  cat("   - Consider separate analyses for each dimension\n")
  cat("   - Or use multidimensional IRT models\n")
  cat("   - Items per dimension:")
  if (exists("loadings_df")) {
    dim_counts <- table(loadings_df$Primary_Factor)
    for (i in 1:length(dim_counts)) {
      cat(sprintf(" Dim%d=%d", i, dim_counts[i]))
    }
  }
  cat("\n")
} else {
  cat("1. Unidimensionality confirmed. No action needed.\n")
}

# 2. Item recommendations
problematic_count <- sum(item_fit$Overall_Fit == "Problematic")
if (problematic_count > 0) {
  cat(sprintf("\n2. REVIEW ITEMS: %d problematic items need attention\n",
              problematic_count))
  prob_items <- item_fit %>%
    filter(Overall_Fit == "Problematic") %>%
    pull(Item)
  cat("   Items to review:", paste(prob_items, collapse = ", "), "\n")
} else {
  cat("\n2. All items show acceptable fit. No revisions needed.\n")
}

# 3. Scale recommendations
if (!ordered_thresholds || nrow(underused) > 0) {
  cat("\n3. REVISE RATING SCALE:\n")
  if (!ordered_thresholds) {
    cat("   - Thresholds are disordered\n")
    cat("   - Categories may not be distinct\n")
  }
  if (nrow(underused) > 0) {
    cat("   - Some categories are underused (<5%)\n")
    cat("   - Consider collapsing categories\n")
  }
} else {
  cat("\n3. Rating scale functioning well. No changes needed.\n")
}

# 4. Coverage recommendations
if (abs(targeting$difference) > 0.5 || length(large_gaps) > 0) {
  cat("\n4. IMPROVE TEST COVERAGE:\n")
  if (targeting$difference > 0.5) {
    cat("   - Test is too easy (mean person > mean item by",
        round(targeting$difference, 2), "logits)\n")
    cat("   - Add more difficult items\n")
  } else if (targeting$difference < -0.5) {
    cat("   - Test is too difficult (mean item > mean person by",
        round(abs(targeting$difference), 2), "logits)\n")
    cat("   - Add easier items\n")
  }
  if (length(large_gaps) > 0) {
    cat("   - Fill", length(large_gaps), "gaps in item difficulty range\n")
  }
} else {
  cat("\n4. Test coverage is adequate. No additional items needed.\n")
}

# 5. DIF recommendations
if (length(all_dif_items) > 0) {
  cat(sprintf("\n5. INVESTIGATE BIAS: %d items show DIF\n",
              length(all_dif_items)))
  cat("   Review these items for potential group bias\n")
} else {
  cat("\n5. No DIF detected. Items function equivalently across groups.\n")
}

# 6. Reliability recommendations
if (wle_reliability < 0.7) {
  cat("\n6. IMPROVE RELIABILITY:\n")
  cat("   - Current reliability (", round(wle_reliability, 3), ") is below acceptable\n")
  cat("   - Add more items or improve existing items\n")
} else if (wle_reliability < 0.8) {
  cat("\n6. RELIABILITY is acceptable but could be improved for individual assessment.\n")
} else {
  cat("\n6. Reliability is good. Suitable for individual assessment.\n")
}

cat("\n============================================================\n")
cat("Thank you for using the Enhanced Rasch RSM Analysis Script!\n")
cat("For questions or support, please refer to the documentation.\n")
cat("============================================================\n\n")

# Create a quick reference card
cat("QUICK REFERENCE - Key Statistics:\n")
cat("--------------------------------\n")
cat("Dimensions detected:", n_dimensions, "\n")
cat("Reliability (WLE):", round(wle_reliability, 3), "\n")
cat("Items with good fit:", sum(item_fit$Overall_Fit %in% c("Excellent", "Acceptable")),
    "/", length(item_cols), "\n")
cat("Ordered thresholds:", ifelse(ordered_thresholds, "Yes", "No"), "\n")
cat("Mean person ability:", round(targeting$mean_person, 3), "\n")
cat("Mean item difficulty:", round(targeting$mean_item, 3), "\n")
cat("Items with DIF:", length(all_dif_items), "\n")

# End of script
