# ======================================================
# RATING SCALE MODEL (RSM) ANALYSIS SCRIPT
# for R 4.4+
# ======================================================

# Check R version
R.version.string

# Install and load required packages
packages <- c("TAM", "psych", "ggplot2", "reshape2", "WrightMap", "difR", "car", "gridExtra", "lattice", "eRm","fmsb","knitr","RaschSampler","lavaan","paran","corrplot")
for(pkg in packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ======================================================
# 1. DATA PREPARATION
# ======================================================

# Information about the analysis
cat("\n============================================================\n")
cat("RATING SCALE MODEL (RSM) ANALYSIS FOR AI ETHICS ASSESSMENT\n")
cat("============================================================\n")
cat("This script performs a comprehensive Rasch analysis using the Rating Scale Model\n")
cat("to evaluate AI ethics assessment items.\n\n")

# Load dataset (adjust file path as needed)
cat("Loading dataset...\n")
data <- read.csv("RASCH_brin_24032025.csv", sep=";", stringsAsFactors=FALSE)
cat("Dataset loaded successfully.\n")

# Display dataset structure
cat("\nDataset structure:\n")
str(data)
cat("\nFirst few rows of the dataset:\n")
print(head(data))

# Remove empty columns (if any)
if("" %in% names(data)) {
  data <- data[, names(data) != ""]
  cat("\nEmpty columns removed.\n")
}

# Rename Year_of_Birth column for consistency (if needed)
if("Year_of_Birth" %in% names(data)) {
  names(data)[names(data) == "Year_of_Birth"] <- "Year_of_Birth"
} else if("Year of Birth" %in% names(data)) {
  names(data)[names(data) == "Year of Birth"] <- "Year_of_Birth"
  cat("\nYear of Birth column renamed for consistency.\n")
}

# Convert demographic variables to factors (explicit method for R 4.4+)
cat("\nConverting demographic variables to factors...\n")
data$Gender <- factor(data$Gender)
data$Job_Title <- factor(data$Job_Title)
data$Year_of_Birth <- factor(data$Year_of_Birth)
data$Education <- factor(data$Education)

# Define item columns
item_cols <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")

# Ensure all items are numeric
items <- data.frame(lapply(data[, item_cols], as.numeric))

# Check for missing values
missing_values <- colSums(is.na(items))
cat("\nMissing values per item:\n")
print(missing_values)

# Descriptive statistics
item_summary <- psych::describe(items)
cat("\nItem descriptive statistics:\n")
print(item_summary)

# ======================================================
# 2. UNIDIMENSIONALITY TESTING 
# ======================================================

cat("\n============================================================\n")
cat("UNIDIMENSIONALITY TESTING\n")
cat("============================================================\n")
cat("Testing the assumption that all items measure a single construct.\n\n")

# Correlation matrix
item_cor <- cor(items, use = "pairwise.complete.obs")
cat("Item correlation matrix:\n")
print(round(item_cor, 2))

# Calculate average correlation between all items 
cor_values <- item_cor[lower.tri(item_cor)]
mean_cor <- mean(cor_values)
cat("\nAverage correlation between all items:", round(mean_cor, 2), "\n")

# Exploratory factor analysis to verify unidimensionality
cat("\nPerforming exploratory factor analysis...\n")
fa_result <- psych::fa(items, nfactors = 1, rotate = "none", fm = "ml")
cat("Factor Analysis Results (1 factor):\n")
print(fa_result)

# Proportion of variance explained
cat("\nProportion of variance explained by the first factor:", 
    round(fa_result$Vaccounted[2,1], 2), "\n")

# Scree plot with enhanced visualization
cat("\nCreating Scree Plot...\n")
pdf("scree_plot.pdf", width = 10, height = 6)
psych::scree(items, factors = FALSE, main = "Scree Plot of Eigenvalues")
dev.off()
cat("Scree plot saved as 'scree_plot.pdf'\n")



# ======================================================
# 3. RATING SCALE MODEL (RSM) FITTING
# ======================================================
# Fit the Rasch Rating Scale Model, which assumes:
# 1. All items share the same rating scale structure
# 2. Distances between categories are equal across items

cat("\n============================================================\n")
cat("RATING SCALE MODEL (RSM) FITTING\n")
cat("============================================================\n")
cat("Fitting the Rating Scale Model to the data...\n\n")

# Fit RSM model using TAM
tryCatch({
  # Clear memory before fitting large model
  gc()
  
  cat("\nFitting Rating Scale Model...\n")
  rsm_model <- TAM::tam.mml(resp = items, 
                         irtmodel = "RSM", 
                         control = list(maxiter = 1000, 
                                       conv = 0.001, 
                                       progress = TRUE, 
                                       Msteps = 10))
  
  # Check convergence
  if (rsm_model$iter == rsm_model$control$maxiter) {
    warning("Model fitting reached maximum iterations without converging. Results may be unstable.")
  } else {
    cat("Model converged after", rsm_model$iter, "iterations\n")
  }
  
  # Model summary
  # cat("\nModel summary:\n")
  # print(summary(rsm_model))
  
  # Compare with Partial Credit Model (PCM) if needed
  # PCM allows different rating scales for each item
  cat("\nFor comparison, also fitting Partial Credit Model...\n")
  pcm_model <- TAM::tam.mml(resp = items, 
                          irtmodel = "PCM", 
                          control = list(maxiter = 1000, 
                                        conv = 0.001))
  
  # Compare model fit - lower deviance/AIC/BIC indicates better fit
  model_comparison <- data.frame(
    Model = c("RSM", "PCM"),
    Deviance = c(rsm_model$deviance, pcm_model$deviance),
    AIC = c(rsm_model$ic$AIC, pcm_model$ic$AIC),
    BIC = c(rsm_model$ic$BIC, pcm_model$ic$BIC),
    Parameters = c(rsm_model$ic$np, pcm_model$ic$np)
  )
  
  cat("\nModel comparison (lower values indicate better fit):\n")
  print(model_comparison)
  
  # Choose the model with better fit (RSM is preferred for parsimony if fits are similar)
  rsm_better <- rsm_model$ic$BIC < pcm_model$ic$BIC + 10
  cat("\nBased on BIC, the", ifelse(rsm_better, "RSM", "PCM"), "model fits better.\n")
  
  # Stick with RSM for the rest of the analysis unless PCM fits much better
  if (!rsm_better) {
    cat("However, for interpretability and theoretical consistency, continuing with RSM.\n")
    cat("If PCM fit is substantially better, consider using PCM instead for full analysis.\n")
  }
  
}, error = function(e) {
  stop(paste("Error in RSM model fitting:", e$message))
})


# ======================================================
# 4. PERSON PARAMETER ESTIMATION
# ======================================================

cat("\n============================================================\n")
cat("PERSON PARAMETER ESTIMATION\n")
cat("============================================================\n")
cat("Estimating person ability parameters...\n\n")

# Extract person parameters - must be done before Wright Map
person_params <- TAM::tam.wle(rsm_model)
cat("Person Parameters Summary:\n")
print(summary(person_params$theta))

# Extract item difficulties for Wright Map
item_difficulties <- rsm_model$xsi$xsi[1:length(item_cols)]

# Create Wright Map with improved visualization
cat("\nCreating Wright Map...\n")
pdf("wright_map.pdf", width = 10, height = 8)
WrightMap::wrightMap(person_params$theta, item_difficulties, 
                   item.names = item_cols,
                   main.title = "Figure 1. Map of Person Abilities and Item Difficulties", 
                   p.main = "Person", 
                   i.main = "Item",
                   min.logit.pad = 0.5,
                   max.logit.pad = 0.5)
mtext("Each '#' represents 4 persons, and each '.' represents 1-3 persons.", 
     side = 1, line = 4, cex = 0.8)
dev.off()
cat("Wright Map saved as 'wright_map.pdf'\n")

# ======================================================
# 5. ITEM PARAMETER ANALYSIS
# ======================================================

cat("\n============================================================\n")
cat("ITEM PARAMETER ANALYSIS\n")
cat("============================================================\n")
cat("Analyzing item parameters and fit statistics...\n\n")

# Extract parameters with error handling
tryCatch({
  item_params <- TAM::tam.threshold(rsm_model)
  cat("Item Parameters:\n")
  print(item_params)
}, error = function(e) {
  cat("Error occurred in tam.threshold(), trying alternative method\n")
  print(rsm_model$xsi)
})

# Extract fit statistics
fit_stats <- TAM::tam.fit(rsm_model)
cat("\nFit Statistics:\n")
print(fit_stats)

fit_logLik <- logLik(rsm_model)
fit_logLikrounded <- round(fit_logLik, digits = 2)
item_params <- rsm_model$xsi; # ini sebelumnya response.difficulty
item_params <- item_params[!grepl("^Cat", rownames(item_params)), ]

person_params <- TAM::tam.wle(rsm_model)  #  dulunya response.ability
person_fit <- TAM::tam.personfit(rsm_model) #person_fit dulunya response.person.fit

# Create comprehensive item parameter table
fit_stats <- as.data.frame(fit_stats$itemfit); 
fit_stats <- fit_stats[!grepl("^Cat", fit_stats$parameter), ]

# Model summary fit statistics:
summary.table.statistics <- c("Outfit MNSQ Mean",
                              "Outfit MNSQ SD",
                              "Outfit ZSTD Mean",
                              "Outfit ZSTD SD",
                              "Infit MNSQ Mean",
                              "Infit MNSQ SD",
                              "Infit ZSTD Mean",
                              "Infit ZSTD SD",
                              "Reliability of Separation")
item.summary.results <- rbind(mean(fit_stats$Outfit),
                              sd(fit_stats$Outfit),
                              mean(fit_stats$Outfit_t),
                              sd(fit_stats$Outfit_t),
                              mean(fit_stats$Infit),
                              sd(fit_stats$Infit),
                              mean(fit_stats$Infit_t),
                              sd(fit_stats$Infit_t),
                              rsm_model$EAP.rel) 
item.summary.results_rounded <- round(item.summary.results, digits = 2)
person.summary.results <- rbind(mean(person_fit$outfitPerson),
                                sd(person_fit$outfitPerson),
                                mean(person_fit$outfitPerson_t),
                                sd(person_fit$outfitPerson_t),
                                mean(person_fit$infitPerson),
                                sd(person_fit$infitPerson),
                                mean(person_fit$infitPerson_t),
                                sd(person_fit$infitPerson_t),
                                mean(person_params$WLE.rel))
person.summary.results_rounded <- round(person.summary.results, digits = 2)
response.model.summary.table <- cbind.data.frame(summary.table.statistics,
                                                item.summary.results_rounded, 
                                                person.summary.results_rounded)
names(response.model.summary.table) <- c("Statistic", "Items", "Persons")  
knitr::kable(
  response.model.summary.table, 
  booktabs = TRUE, caption = "Model Summary Table"
)

# Item Characteristic Curves (ICC)
plot(rsm_model, type = "items", high = 4, export = FALSE)

# Items calibration and fit.
response.item.statistics <- cbind.data.frame(
  "Item" = fit_stats$parameter,  
  "Infit.MNSQ" = round(fit_stats$Infit, digits = 2),  
  "Infit.ZSTD" = round(fit_stats$Infit_t, digits = 2), 
  "Outfit.MNSQ" = round(fit_stats$Outfit, digits = 2),             
  "Outfit.ZSTD" = round(fit_stats$Outfit_t, digits = 2),
  "Difficulty" = round(item_params$xsi, digits = 2),           
  "Difficulty.SE" = round(item_params$se.xsi, digits = 2)      
)
response.item.statistics <- response.item.statistics[!grepl("^Cat", response.item.statistics$Item), ]
knitr::kable(
  response.item.statistics, 
  booktabs = TRUE,
  caption = "Item Calibration Statistics and Fit Indices"
)

# Person calibration and fit.
response.person.statistics <- cbind.data.frame(
  "Person.ID" = person_params$pid,
  "Infit.MNSQ" = round(person_fit$infitPerson, digits = 2),
  "Infit.ZSTD" = round(person_fit$infitPerson_t, digits = 2),
  "Outfit.MNSQ" = round(person_fit$outfitPerson, digits = 2),
  "Outfit.ZSTD" = round(person_fit$outfitPerson_t, digits = 2),
  "Ability" = round(person_params$theta, digits = 2),
  "Ability.SE" = round(person_params$error, digits = 2)
)
knitr::kable(
  response.person.statistics, 
  booktabs = TRUE,
  caption = "Person Calibration Statistics and Fit Indices"
)

###################### Item Difficulty Map for RESPOND ########################

item_params.IDM <- item_params
item_params.IDM$Item <- rownames(item_params)

ggplot2::ggplot(item_params.IDM, ggplot2::aes(x = reorder(Item, xsi), y = xsi)) +
  ggplot2::geom_point(size = 3, color = "black") +
  ggplot2::geom_errorbar(ggplot2::aes(
    ymin = xsi - se.xsi,
    ymax = xsi + se.xsi
  ),
  width = 0.2, color = "black") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Item Difficulty Map for Potential",
    x = "Item",
    y = "Difficulty (Logits)"
  ) +
  ggplot2::theme_minimal()


####################
# Create enhanced visualization of item fit
cat("\nCreating item fit visualization...\n")
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)

# Infit plot with improved styling
plot(fit_stats$Infit,
     item_difficulties,
     xlim = c(0.5, 1.5),
     type = "n",
     xlab = "Infit Mean Square",
     ylab = "Item Location (logits)",
     main = "Item Fit Analysis – Infit")
text(fit_stats$Infit,
     item_difficulties,
     labels = item_cols,
     cex = 0.9, font = 2)
abline(v = 1,    lwd = 2)
abline(v = c(0.70, 1.30), col = "red", lty = 2, lwd = 2)
legend("topright",
       legend = c("Expected Value", "Acceptable Range"),
       col    = c("black", "red"),
       lty    = c(1, 2),
       lwd    = c(2, 2),
       bty    = "n")

# Outfit plot with improved styling
# 2) Outfit plot
plot(fit_stats$Outfit,
     item_difficulties,
     xlim = c(0.5, 1.5),
     type = "n",
     xlab = "Outfit Mean Square",
     ylab = "Item Location (logits)",
     main = "Item Fit Analysis – Outfit")
text(fit_stats$Outfit,
     item_difficulties,
     labels = item_cols,
     cex = 0.9, font = 2)
abline(v = 1,    lwd = 2)
abline(v = c(0.70, 1.30), col = "red", lty = 2, lwd = 2)
legend("topright",
       legend = c("Expected Value", "Acceptable Range"),
       col    = c("black", "red"),
       lty    = c(1, 2),
       lwd    = c(2, 2),
       bty    = "n")

# Reset layout
par(mfrow = c(1, 1))

# ======================================================
# 6. RATING SCALE ANALYSIS
# ======================================================

cat("\n============================================================\n")
cat("RATING SCALE ANALYSIS\n")
cat("============================================================\n")
cat("Analyzing rating scale properties...\n\n")

# Extract rating scale statistics with improved error handling
tryCatch({
  # Create category frequency table
  category_counts <- table(unlist(items))
  total_responses <- sum(category_counts)
  category_percent <- (category_counts / total_responses) * 100

  # Create rating scale table
  if (length(rsm_model$threshold) > 0) {
    rating_table <- data.frame(
      Category = c("1 = Strongly disagree", "2 = Disagree", "3 = Agree", "4 = Strongly agree"),
      Frequency_Percent = as.numeric(sprintf("%.1f", category_percent)),
      Data_Category_Count = as.numeric(category_counts)
    )
    
    # Add thresholds if available
    thresholds <- c(NA, rsm_model$threshold)
    thresholds_se <- c(NA, rsm_model$se.threshold)
    
    # Special handling for R 4.4+ - ensure vector length matches
    if (length(thresholds) >= nrow(rating_table)) {
      rating_table$Rasch_Andrich_Threshold <- c("None", sprintf("%.2f", thresholds[2:nrow(rating_table)]))
      rating_table$SE <- c("-", sprintf("%.2f", thresholds_se[2:nrow(rating_table)]))
    }
  } else {
    rating_table <- data.frame(
      Category = c("1 = Strongly disagree", "2 = Disagree", "3 = Agree", "4 = Strongly agree"),
      Frequency_Percent = as.numeric(sprintf("%.1f", category_percent)),
      Data_Category_Count = as.numeric(category_counts))
  }
  
  cat("Table 2. Rating Scale Analysis Statistics:\n")
  print(rating_table)
  
  # Calculate distances between thresholds
  if (exists("thresholds") && length(thresholds) > 2) {
    threshold_distances <- diff(rsm_model$threshold)
    category_pairs <- c("1 to 2", "2 to 3", "3 to 4")
    
    # Print threshold distances
    cat("\nDistances between threshold values:\n")
    for(i in 1:length(threshold_distances)) {
      cat(sprintf("From category %s: %.2f logits\n", 
                 category_pairs[i], threshold_distances[i]))
      cat(sprintf("Within ideal range (1.4-5.0): %s\n", 
                 threshold_distances[i] >= 1.4 & threshold_distances[i] <= 5.0))
    }
  }
}, error = function(e) {
  cat("Error occurred in rating scale analysis:", e$message, "\n")
})

# Create enhanced visualization of category probability curves
cat("\nCreating category probability curves...\n")
plot(rsm_model, 
      type = "items", 
      export = FALSE, 
      package = "lattice", 
      observed = TRUE, 
      low = -6, 
      high = 6)

# Create enhanced visualization of expected score curves
cat("\nCreating expected score curves...\n")
plot(rsm_model, 
     type = "expected", 
     ngroups = 6, 
     low  = -6, 
     high = 6,
     package = "lattice", overlay = FALSE)

# Create enhanced visualization of Overlay the expected score curves
cat("\nCreating overlay the expected score curves...\n")
plot(rsm_model, 
     type = "expected", 
     ngroups = 6, 
     low  = -6, 
     high = 6,
     package = "lattice", 
     overlay = TRUE,
     observed = FALSE)

# ======================================================
# 7. PERSON AND ITEM SUMMARY
# ======================================================

cat("\n============================================================\n")
cat("PERSON AND ITEM SUMMARY\n")
cat("============================================================\n")
cat("Summarizing person and item statistics...\n\n")

# Item parameters summary
cat("Item Parameters Summary:\n")
print(summary(rsm_model$xsi$xsi[1:length(item_cols)]))

# Calculate separation indices
person_SD <- sd(person_params$theta)
item_SD <- sd(rsm_model$xsi$xsi[1:length(item_cols)])

# Person separation
person_error_variance <- mean(person_params$error^2)
person_true_variance <- person_SD^2 - person_error_variance
person_separation <- sqrt(person_true_variance / person_error_variance)
person_reliability <- person_true_variance / (person_true_variance + person_error_variance)

# Item separation
item_error_variance <- mean(rsm_model$xsi$se.xsi[1:length(item_cols)]^2)
item_true_variance <- item_SD^2 - item_error_variance
item_separation <- sqrt(item_true_variance / item_error_variance)
item_reliability <- item_true_variance / (item_true_variance + item_error_variance)

# Calculate Cronbach's alpha
alpha_result <- psych::alpha(items)
cronbach_alpha <- alpha_result$total$raw_alpha

# Calculate chi-square
chi_square <- rsm_model$deviance
chi_square_p <- 1 - pchisq(chi_square, df = rsm_model$ic$n.ik - rsm_model$ic$np)

# Calculate outfit mean-square
if (exists("fit_stats")) {
    
    person_outfit_mean <- mean(person_fit$outfitPerson)
    person_outfit_sd <- sd(person_fit$outfitPerson)
    item_outfit_mean <- mean(fit_stats$Outfit)
    item_outfit_sd <- sd(fit_stats$Outfit)
} else {
    # Default values if tam.fit() fails
    person_outfit_mean <- NA
    person_outfit_sd <- NA
    item_outfit_mean <- NA
    item_outfit_sd <- NA
}

# Create summary table
summary_table <- data.frame(
    Metric = c("Separation", "Reliability", "Cronbach's alpha", "Chi-square (χ²)", 
               "Outfit mean-square", "Mean", "SD", "Note(s)"),
    Person = c(
        sprintf("%.2f", person_separation), 
        sprintf("%.2f", person_reliability), 
        sprintf("%.2f", cronbach_alpha), 
        "", 
        "", 
        ifelse(is.na(person_outfit_mean), "NA", sprintf("%.2f", person_outfit_mean)), 
        ifelse(is.na(person_outfit_sd), "NA", sprintf("%.2f", person_outfit_sd)), 
        "**p < 0.01"
    ),
    Item = c(
        sprintf("%.2f", item_separation), 
        sprintf("%.2f", item_reliability), 
        "", 
        sprintf("%.0f**", chi_square), 
        "", 
        ifelse(is.na(item_outfit_mean), "NA", sprintf("%.2f", item_outfit_mean)), 
        ifelse(is.na(item_outfit_sd), "NA", sprintf("%.2f", item_outfit_sd)), 
        ""
    )
)

# Logit summary table
logit_summary_table <- data.frame(
  Metric = c("N", "", "Measures", "Mean", "SD"),
  Person = c(nrow(data), "", "", 
             sprintf("%.2f", mean(person_params$theta)), 
             sprintf("%.2f", sd(person_params$theta))),
  Item = c(length(item_cols), "", "", 
           sprintf("%.2f", mean(rsm_model$xsi$xsi[1:length(item_cols)])), 
           sprintf("%.2f", sd(rsm_model$xsi$xsi[1:length(item_cols)])))
)

# Print tables
cat("Table 3. Summary of Person and Item Separation Index:\n")
print(summary_table)

cat("\nTable 4. Summary of Person and Item Logit Values:\n")
print(logit_summary_table)

# ======================================================
# 8. PERSON AND ITEM CATEGORIZATION
# ======================================================

cat("\n============================================================\n")
cat("PERSON AND ITEM CATEGORIZATION\n")
cat("============================================================\n")
cat("Categorizing persons and items based on logit values...\n\n")

# Get person and item distribution statistics
person_mean <- mean(person_params$theta)
person_sd <- sd(person_params$theta)
item_mean <- mean(rsm_model$xsi$xsi[1:length(item_cols)])
item_sd <- sd(rsm_model$xsi$xsi[1:length(item_cols)])

# Recalculate person cutoffs based on mean and SD
# Using standard deviations as boundaries:
# Not Ready: < mean - 1*SD
# Mostly Ready: >= mean - 1*SD and <= mean + 1*SD
# Very Ready: > mean + 1*SD
person_lower_cutoff <- person_mean - 1 * person_sd
person_upper_cutoff <- person_mean + 1 * person_sd

cat("Person categorization cutoffs (based on mean and SD):\n")
cat("Mean:", round(person_mean, 2), ", SD:", round(person_sd, 2), "\n")
cat("Lower cutoff (mean - 1*SD):", round(person_lower_cutoff, 2), "\n")
cat("Upper cutoff (mean + 1*SD):", round(person_upper_cutoff, 2), "\n")

# Categorize persons based on the cutoffs
person_params$Category <- cut(person_params$theta, 
                             breaks = c(-Inf, person_lower_cutoff, person_upper_cutoff, Inf),
                             labels = c("Not Ready", "Mostly Ready", "Very Ready"))

# Define readiness categories for interpretation
cat("\n=== DEFINING READINESS CATEGORIES ===\n")
cat("The following readiness categories are defined based on logit values:\n\n")
cat(sprintf("1. Not Ready (< %.2f logits):\n", person_lower_cutoff))
cat("   Individuals in this category demonstrate insufficient understanding of AI ethics principles.\n")
cat("   They require substantial training before engaging with AI ethics assessments or decision-making.\n")
cat("   Recommended actions: Comprehensive training program focusing on fundamental AI ethics concepts.\n\n")

cat(sprintf("2. Mostly Ready (%.2f to %.2f logits):\n", person_lower_cutoff, person_upper_cutoff))
cat("   Individuals in this category demonstrate adequate understanding of AI ethics principles.\n")
cat("   They can engage with AI ethics assessments with supervision or in collaborative settings.\n")
cat("   Recommended actions: Targeted training on specific areas of weakness identified in the assessment.\n\n")

cat(sprintf("3. Very Ready (> %.2f logits):\n", person_upper_cutoff))
cat("   Individuals in this category demonstrate strong understanding of AI ethics principles.\n")
cat("   They can independently lead AI ethics assessments and decision-making processes.\n")
cat("   Recommended actions: Advanced training to maintain expertise and potential mentorship roles.\n\n")

# Calculate raw score to logit conversion table
cat("=== RAW SCORE TO LOGIT CONVERSION ===\n")
cat("Using Rasch person measures (logits) as the primary score is recommended for the following reasons:\n")
cat("1. Logit values represent a true interval scale, unlike raw scores which are ordinal\n")
cat("2. Logit measures account for item difficulty, producing more accurate ability estimates\n")
cat("3. Logit values enable meaningful comparisons between persons and items on the same scale\n")
cat("4. Statistical analyses are more appropriate with interval-level logit measures\n\n")

# Calculate the raw score for each person
raw_scores <- rowSums(items, na.rm = TRUE)

# Create a conversion table
score_conversion <- data.frame(
  Raw_Score = sort(unique(raw_scores)),
  Frequency = as.numeric(table(raw_scores)),
  Mean_Logit = NA,
  SD_Logit = NA,
  Readiness_Category = NA
)

# Calculate mean and SD logit for each raw score
for(i in 1:nrow(score_conversion)) {
  score <- score_conversion$Raw_Score[i]
  logits <- person_params$theta[raw_scores == score]
  score_conversion$Mean_Logit[i] <- mean(logits)
  score_conversion$SD_Logit[i] <- sd(logits)
  
  # Assign readiness category based on mean logit
  if(score_conversion$Mean_Logit[i] < person_lower_cutoff) {
    score_conversion$Readiness_Category[i] <- "Not Ready"
  } else if(score_conversion$Mean_Logit[i] <= person_upper_cutoff) {
    score_conversion$Readiness_Category[i] <- "Mostly Ready"
  } else {
    score_conversion$Readiness_Category[i] <- "Very Ready"
  }
}

cat("Table 5a. Raw Score to Logit Conversion Table:\n")
print(score_conversion)
cat("\nNote: This conversion table can be used to interpret raw scores, but logit measures\n")
cat("should be used for all statistical analyses and decision-making.\n\n")

# Save conversion table for future use
write.csv(score_conversion, "raw_to_logit_conversion.csv", row.names = FALSE)
cat("Raw score to logit conversion table saved as 'raw_to_logit_conversion.csv'\n\n")

# Calculate item difficulty strata cutoffs based on mean and SD
# Dividing into 5 strata using mean and SD:
# Difficulty Strata I (Very Easy): > mean + 1.5*SD
# Difficulty Strata II (Easy): > mean + 0.5*SD to <= mean + 1.5*SD
# Difficulty Strata III (Moderate): > mean - 0.5*SD to <= mean + 0.5*SD
# Difficulty Strata IV (Difficult): > mean - 1.5*SD to <= mean - 0.5*SD
# Difficulty Strata V (Very Difficult): <= mean - 1.5*SD

strata_cutoffs <- c(
  item_mean - 1.5 * item_sd,
  item_mean - 0.5 * item_sd,
  item_mean + 0.5 * item_sd,
  item_mean + 1.5 * item_sd
)

cat("\nItem difficulty categorization cutoffs (based on mean and SD):\n")
cat("Mean:", round(item_mean, 2), ", SD:", round(item_sd, 2), "\n")
cat("Strata V/IV cutoff (mean - 1.5*SD):", round(strata_cutoffs[1], 2), "\n")
cat("Strata IV/III cutoff (mean - 0.5*SD):", round(strata_cutoffs[2], 2), "\n")
cat("Strata III/II cutoff (mean + 0.5*SD):", round(strata_cutoffs[3], 2), "\n")
cat("Strata II/I cutoff (mean + 1.5*SD):", round(strata_cutoffs[4], 2), "\n")

# Categorize items based on the cutoffs
item_categories <- cut(item_difficulties,
                      breaks = c(-Inf, strata_cutoffs, Inf),
                      labels = c("Difficulty strata V", "Difficulty strata IV", 
                                "Difficulty strata III", "Difficulty strata II", 
                                "Difficulty Strata I"))

# Create item categorization table
item_category_table <- data.frame(
  Item = item_cols,
  Logit = round(item_difficulties, 2),
  Category = item_categories
)

# Print item categorization info
cat("\nTable 5b. Item Difficulty Categories Based on Logit Values (LVI):\n")
cat("==================================================================\n")
cat("Difficulty levels (based on mean and SD):\n")
cat(sprintf("Difficulty Strata I (LVI > %.2f): Very Easy\n", strata_cutoffs[4]))
cat(sprintf("Difficulty Strata II (%.2f < LVI ≤ %.2f): Easy\n", strata_cutoffs[3], strata_cutoffs[4]))
cat(sprintf("Difficulty strata III (%.2f < LVI ≤ %.2f): Moderate\n", strata_cutoffs[2], strata_cutoffs[3]))
cat(sprintf("Difficulty strata IV (%.2f < LVI ≤ %.2f): Difficult\n", strata_cutoffs[1], strata_cutoffs[2]))
cat(sprintf("Difficulty strata V (LVI ≤ %.2f): Very Difficult\n", strata_cutoffs[1]))
cat("------------------------------------------------------------------\n")
cat("Item Categorization:\n")
print(item_category_table)

# Combine person parameters with demographic data
person_data <- cbind(data[, c("Gender", "Job_Title", "Year_of_Birth", "Education")], 
                    Raw_Score = raw_scores,
                    Theta = person_params$theta, 
                    Category = person_params$Category)

# Create cross-tabulations with demographic variables
gender_table <- table(person_data$Gender, person_data$Category)
job_title_table <- table(person_data$Job_Title, person_data$Category)
birth_year_table <- table(person_data$Year_of_Birth, person_data$Category)
education_table <- table(person_data$Education, person_data$Category)

# Print person categorization tables
cat("\nTable 6. Researcher Readiness Categories Based on Person Logit Values (LVP):\n")
cat("========================================================================\n")
cat(sprintf("Not Ready (LVP < %.2f)\n", person_lower_cutoff))
cat(sprintf("Mostly Ready (%.2f ≤ LVP ≤ %.2f)\n", person_lower_cutoff, person_upper_cutoff))
cat(sprintf("Very Ready (LVP > %.2f)\n", person_upper_cutoff))
cat("------------------------------------------------------------------------\n")
cat("Gender:\n")
print(gender_table)
cat("\nJob Title:\n")
print(job_title_table)
cat("\nYear of Birth:\n")
print(birth_year_table)
cat("\nEducation:\n")
print(education_table)

# Create enhanced visualization of person distribution with categories
cat("\nCreating person distribution visualization with readiness categories...\n")
pdf("person_distribution.pdf", width = 10, height = 8)
# Person logit histogram with improved styling and category regions
hist(person_params$theta, 
    breaks = 20,
    main = "Distribution of Person Logit Values with Readiness Categories",
    xlab = "Logit Value", 
    col = "skyblue",
    border = "white",
    xaxt = "n")  # Suppress x-axis to customize it

# Add custom x-axis with category labels
axis(1, at = seq(floor(min(person_params$theta)), ceiling(max(person_params$theta)), by = 1))

# Add vertical lines for cutoffs
abline(v = mean(person_params$theta), col = "red", lwd = 2)
abline(v = person_lower_cutoff, col = "blue", lty = 2, lwd = 2)
abline(v = person_upper_cutoff, col = "blue", lty = 2, lwd = 2)

# Add shaded regions for categories
usr <- par("usr")
rect(usr[1], usr[3], person_lower_cutoff, usr[4], col = rgb(1, 0.7, 0.7, alpha = 0.3), border = NA)
rect(person_lower_cutoff, usr[3], person_upper_cutoff, usr[4], col = rgb(0.7, 1, 0.7, alpha = 0.3), border = NA)
rect(person_upper_cutoff, usr[3], usr[2], usr[4], col = rgb(0.7, 0.7, 1, alpha = 0.3), border = NA)

# Add text labels for categories
text(person_lower_cutoff - 1, usr[4] * 0.9, "Not Ready", cex = 1.2, col = "darkred")
text(mean(c(person_lower_cutoff, person_upper_cutoff)), usr[4] * 0.9, "Mostly Ready", cex = 1.2, col = "darkgreen")
text(person_upper_cutoff + 1, usr[4] * 0.9, "Very Ready", cex = 1.2, col = "darkblue")

legend("topright", 
       legend = c("Mean", "Category Cutoffs"), 
       col = c("red", "blue"), 
       lty = c(1, 2), 
       lwd = 2)

dev.off()
cat("Person distribution visualization saved as 'person_distribution.pdf'\n")

# Create scatterplot comparing raw scores and logit measures
pdf("raw_vs_logit.pdf", width = 10, height = 8)
plot(raw_scores, person_params$theta,
     main = "Relationship Between Raw Scores and Logit Measures",
     xlab = "Raw Score",
     ylab = "Logit Measure",
     pch = 19,
     col = rgb(0, 0, 1, 0.5))

# Add the estimated conversion line
unique_scores <- sort(unique(raw_scores))
mean_logits <- sapply(unique_scores, function(s) mean(person_params$theta[raw_scores == s]))
lines(unique_scores, mean_logits, col = "red", lwd = 2)

# Add category regions
abline(h = person_lower_cutoff, col = "blue", lty = 2, lwd = 2)
abline(h = person_upper_cutoff, col = "blue", lty = 2, lwd = 2)

# Add text labels for categories
text(max(raw_scores) * 0.9, person_lower_cutoff - 0.2, "Not Ready", cex = 1.2, col = "darkred")
text(max(raw_scores) * 0.9, (person_lower_cutoff + person_upper_cutoff)/2, "Mostly Ready", cex = 1.2, col = "darkgreen")
text(max(raw_scores) * 0.9, person_upper_cutoff + 0.2, "Very Ready", cex = 1.2, col = "darkblue")

legend("bottomright", 
       legend = c("Individual Measures", "Conversion Line", "Category Cutoffs"), 
       col = c("blue", "red", "blue"), 
       pch = c(19, NA, NA),
       lty = c(NA, 1, 2),
       lwd = c(NA, 2, 2))

dev.off()
cat("Raw score vs logit measure comparison saved as 'raw_vs_logit.pdf'\n")


# ======================================================
# 9. DIF ANALYSIS (Differential Item Functioning)
# ======================================================

cat("\n============================================================\n")
cat("DIFFERENTIAL ITEM FUNCTIONING (DIF) ANALYSIS\n")
cat("============================================================\n")
cat("Analyzing item functioning across different demographic groups...\n\n")

# Function to calculate DIF based on mean differences
calculate_dif <- function(data, items, group_var, item_cols) {
  # Convert group variable to factor
  group_factor <- factor(data[[group_var]])
  group_levels <- levels(group_factor)
  
  # Initialize results matrix
  dif_values <- matrix(0, nrow = length(item_cols), ncol = length(group_levels))
  rownames(dif_values) <- item_cols
  colnames(dif_values) <- group_levels
  
  # Calculate mean differences for each item in each group
  for (i in 1:length(item_cols)) {
    item_col <- item_cols[i]
    item_mean <- mean(items[[item_col]], na.rm = TRUE)
    
    for (g in 1:length(group_levels)) {
      group_level <- group_levels[g]
      group_data <- items[group_factor == group_level, item_col]
      dif_values[i, g] <- mean(group_data, na.rm = TRUE) - item_mean
    }
  }
  
  return(dif_values)
}

# Function to perform chi-square DIF test
perform_dif_chisq <- function(data, items, group_var, item_cols) {
  # Convert group variable to factor
  group_factor <- factor(data[[group_var]])
  
  # Check if there are enough observations in each group
  group_counts <- table(group_factor)
  if (any(group_counts < 5)) {
    warning("Some groups have fewer than 5 observations, chi-square test may not be reliable")
  }
  
  # Initialize results matrix
  dif_results <- matrix(NA, nrow = length(item_cols), ncol = 3)
  rownames(dif_results) <- item_cols
  colnames(dif_results) <- c("Chi-square", "df", "p-value")
  
  # Perform chi-square test for each item
  for (i in 1:length(item_cols)) {
    item_col <- item_cols[i]
    # Create contingency table
    item_scores <- factor(items[[item_col]])
    contingency_table <- table(group_factor, item_scores)
    
    # Check if contingency table is valid for chi-square test
    expected <- chisq.test(contingency_table)$expected
    if (any(expected < 5)) {
      # Use Fisher's exact test for small expected frequencies
      tryCatch({
        fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE)
        dif_results[i, "Chi-square"] <- NA
        dif_results[i, "df"] <- NA
        dif_results[i, "p-value"] <- fisher_test$p.value
      }, error = function(e) {
        dif_results[i, "Chi-square"] <- NA
        dif_results[i, "df"] <- NA
        dif_results[i, "p-value"] <- NA
      })
    } else {
      # Use chi-square test for adequate expected frequencies
      tryCatch({
        chi_square_test <- chisq.test(contingency_table)
        dif_results[i, "Chi-square"] <- chi_square_test$statistic
        dif_results[i, "df"] <- chi_square_test$parameter
        dif_results[i, "p-value"] <- chi_square_test$p.value
      }, error = function(e) {
        dif_results[i, "Chi-square"] <- NA
        dif_results[i, "df"] <- NA
        dif_results[i, "p-value"] <- NA
      })
    }
  }
  
  return(dif_results)
}

# Function to plot DIF with enhanced visualization
plot_dif <- function(dif_values, title, group_var) {
  require(reshape2)
  require(ggplot2)
  
  # Convert matrix to data.frame for ggplot
  dif_df <- reshape2::melt(dif_values, varnames = c("Item", group_var), value.name = "DIF_Measure")
  
  # Plot DIF with improved styling
  p <- ggplot(dif_df, aes(x = Item, y = DIF_Measure, group = get(group_var), color = get(group_var))) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(title = title,
         x = "Items", 
         y = "DIF Measure (diff.)",
         color = group_var) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )
  
  return(p)
}

# Perform DIF analysis for each demographic variable
demographic_vars <- c("Gender", "Job_Title", "Year_of_Birth", "Education")

cat("=== DIF ANALYSIS FOR ALL DEMOGRAPHIC VARIABLES ===\n")

# Create PDF for DIF plots
pdf("dif_analysis.pdf", width = 12, height = 10)
par(mfrow = c(2, 2))

# Store all DIF plots for combined visualization
dif_plots <- list()

# Store DIF results for recommendations section
dif_significant_items <- list()

for (dem_var in demographic_vars) {
  cat(paste0("\n### DIF analysis based on ", dem_var, " ###\n"))
  
  # Check if there are enough groups for DIF analysis
  if (length(unique(data[[dem_var]])) < 2) {
    cat("Not enough groups for DIF analysis based on", dem_var, "\n")
    next
  }
  
  # Mean differences DIF
  dif_values <- calculate_dif(data, items, dem_var, item_cols)
  cat(paste0("DIF values (mean differences) based on ", dem_var, ":\n"))
  print(round(dif_values, 2))
  
  # Chi-square DIF test
  tryCatch({
    dif_chisq <- perform_dif_chisq(data, items, dem_var, item_cols)
    cat(paste0("\nChi-square DIF test based on ", dem_var, ":\n"))
    dif_results_df <- as.data.frame(dif_chisq)
    dif_results_df$Significant <- dif_results_df$`p-value` < 0.05
    print(dif_results_df)
    
    # Store significant items for recommendations
    sig_items <- rownames(dif_results_df)[dif_results_df$Significant]
    if(length(sig_items) > 0) {
      dif_significant_items[[dem_var]] <- sig_items
    }
    
    # Plot DIF
    title <- paste0("Person DIF Plot Based on ", dem_var)
    dif_plots[[dem_var]] <- plot_dif(dif_values, title, dem_var)
    print(dif_plots[[dem_var]])
  }, error = function(e) {
    cat("Error in DIF analysis for", dem_var, ":", e$message, "\n")
  })
}

# For comparison, create a combined plot with gridExtra if we have multiple plots
if(require(gridExtra) && length(dif_plots) > 1) {
  grid.arrange(grobs = dif_plots, ncol = 2)
}

dev.off()
cat("DIF analysis plots saved as 'dif_analysis.pdf'\n")

# ======================================================
# 10. ANOVA ANALYSIS
# ======================================================

cat("\n============================================================\n")
cat("ANOVA ANALYSIS\n")
cat("============================================================\n")
cat("Analyzing variance in person measures across demographic groups...\n\n")

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

# ANOVA based on Gender
cat("ANOVA based on Gender:\n")
anova_results$Gender <- safe_anova(Theta ~ Gender, person_data)
if (!is.null(anova_results$Gender$summary)) {
  print(anova_results$Gender$summary)
}

# ANOVA based on Job Title
cat("\nANOVA based on Job Title:\n")
anova_results$Job_Title <- safe_anova(Theta ~ Job_Title, person_data)
if (!is.null(anova_results$Job_Title$summary)) {
  print(anova_results$Job_Title$summary)
}

# ANOVA based on Year of Birth
cat("\nANOVA based on Year of Birth:\n")
anova_results$Year_of_Birth <- safe_anova(Theta ~ Year_of_Birth, person_data)
if (!is.null(anova_results$Year_of_Birth$summary)) {
  print(anova_results$Year_of_Birth$summary)
}

# ANOVA based on Education
cat("\nANOVA based on Education:\n")
anova_results$Education <- safe_anova(Theta ~ Education, person_data)
if (!is.null(anova_results$Education$summary)) {
  print(anova_results$Education$summary)
}

# Create ANOVA table
anova_table <- data.frame(
  No = 1:4,
  Demographic_variable = c("Gender", "Job Title", "Year of Birth", "Education"),
  F_test = c(
    ifelse(is.na(anova_results$Gender$f_value), "NA", sprintf("%.2f", anova_results$Gender$f_value)),
    ifelse(is.na(anova_results$Job_Title$f_value), "NA", sprintf("%.2f", anova_results$Job_Title$f_value)),
    ifelse(is.na(anova_results$Year_of_Birth$f_value), "NA", sprintf("%.2f", anova_results$Year_of_Birth$f_value)),
    ifelse(is.na(anova_results$Education$f_value), "NA", sprintf("%.2f", anova_results$Education$f_value))
  ),
  p_value = c(
    ifelse(is.na(anova_results$Gender$p_value), "NA", sprintf("%.3f", anova_results$Gender$p_value)),
    ifelse(is.na(anova_results$Job_Title$p_value), "NA", sprintf("%.3f", anova_results$Job_Title$p_value)),
    ifelse(is.na(anova_results$Year_of_Birth$p_value), "NA", sprintf("%.3f", anova_results$Year_of_Birth$p_value)),
    ifelse(is.na(anova_results$Education$p_value), "NA", sprintf("%.3f", anova_results$Education$p_value))
  )
)

# Determine significant variables
significance <- c(
  !is.na(anova_results$Gender$p_value) && anova_results$Gender$p_value < 0.05,
  !is.na(anova_results$Job_Title$p_value) && anova_results$Job_Title$p_value < 0.05,
  !is.na(anova_results$Year_of_Birth$p_value) && anova_results$Year_of_Birth$p_value < 0.05,
  !is.na(anova_results$Education$p_value) && anova_results$Education$p_value < 0.05
)

# Add significance markers
anova_table$Significant <- ifelse(significance, "*", "")

# Add note about significance
if (any(significance, na.rm = TRUE)) {
  note <- "Note(s): * p < 0.05"
} else {
  note <- "Note(s): No significant differences found"
}

cat("\nTable 7. ANOVA for Demographic Variables:\n")
print(anova_table)
cat(note, "\n")

# Comprehensive ANOVA with all demographic variables
cat("\nComprehensive ANOVA (all demographic variables):\n")
tryCatch({
  full_anova <- aov(Theta ~ Gender + Job_Title + Year_of_Birth + Education, data = person_data)
  print(summary(full_anova))
}, error = function(e) {
  cat("Error in comprehensive ANOVA:", e$message, "\n")
  cat("This may be due to insufficient data or collinearity among predictors.\n")
})

# Create enhanced visualization of ANOVA results
cat("\nCreating ANOVA visualization...\n")
par(cex.axis = 0.7)

# Function to safely create boxplots
safe_boxplot <- function(formula, data, main, xlab, ylab, col) {
  tryCatch({
    boxplot(formula, data = data, 
            main = main,
            xlab = xlab, ylab = ylab,
            col = col)
  }, error = function(e) {
    plot(1, type="n", axes=FALSE, xlab="", ylab="")
    text(1, 1, paste("Error:", e$message), cex=1.2)
    title(main)
  })
}

par(mfrow = c(2, 2))

# 1) By Gender
par(mfrow = c(1,1))
safe_boxplot(Theta ~ Gender,
             data = person_data,
             main = "Person Measures by Gender",
             xlab = "Gender",
             ylab = "Person Measure (logits)",
             col = "lightblue")

# 2) By Job Title
par(mfrow = c(1,1))
person_data$Job_Title <- factor(person_data$Job_Title,
                                levels = c("Assistant Researcher",
                                           "Junior Researcher",
                                           "Senior Researcher",
                                           "Principal Researcher"))
safe_boxplot(Theta ~ Job_Title,
             data = person_data,
             main = "Person Measures by Job Title",
             xlab = "Job Title",
             ylab = "Person Measure (logits)",
             col = "lightgreen")

# 3) By Year of Birth
par(mfrow = c(1,1))
safe_boxplot(Theta ~ Year_of_Birth,
             data = person_data,
             main = "Person Measures by Year of Birth",
             xlab = "Year of Birth",
             ylab = "Person Measure (logits)",
             col = "lightyellow")

# 4) By Education
par(mfrow = c(1,1))
safe_boxplot(Theta ~ Education,
             data = person_data,
             main = "Person Measures by Education",
             xlab = "Education",
             ylab = "Person Measure (logits)",
             col = "lightpink")

# Reset the font axis to default
par(cex.axis = 1)

# Boxplots for group of demographic variable
cat("\nCreating ANOVA visualization in one image...\n")
# Arrange 4 plots in a 2×2 grid, shrink axis labels to 70%
par(mfrow = c(2, 2),
    mar   = c(5, 4, 4, 2) + 0.1,  # bottom, left, top, right margins
    cex.axis = 0.7)               # 70% axis font size

# 1. By Gender
safe_boxplot(Theta ~ Gender, data = person_data,
             main = "Person Measures by Gender",
             xlab = "Gender", ylab = "Person Measure (logits)",
             col = "lightblue")

# 2. By Job Title (ensure factor ordering first)
person_data$Job_Title <- factor(person_data$Job_Title,
                                levels = c("Assistant Researcher",
                                           "Junior Researcher",
                                           "Senior Researcher",
                                           "Principal Researcher"))
safe_boxplot(Theta ~ Job_Title, data = person_data,
             main = "Person Measures by Job Title",
             xlab = "Job Title", ylab = "Person Measure (logits)",
             col = "lightgreen")

# 3. By Year of Birth
safe_boxplot(Theta ~ Year_of_Birth, data = person_data,
             main = "Person Measures by Year of Birth",
             xlab = "Year of Birth", ylab = "Person Measure (logits)",
             col = "lightyellow")

# 4. By Education
safe_boxplot(Theta ~ Education, data = person_data,
             main = "Person Measures by Education",
             xlab = "Education", ylab = "Person Measure (logits)",
             col = "lightpink")

# Reset plotting parameters to default
par(mfrow   = c(1, 1),
    cex.axis = 1)

# ======================================================
# 11. SUMMARY AND CONCLUSION
# ======================================================

cat("\n============================================================\n")
cat("RASCH ANALYSIS SUMMARY\n")
cat("============================================================\n")
cat("Summary of key findings from the Rasch analysis...\n\n")

cat("RELIABILITY MEASURES:\n")
cat("Person Reliability:", round(person_reliability, 2), "\n")
cat("Item Reliability:", round(item_reliability, 2), "\n")
cat("Cronbach's Alpha:", round(cronbach_alpha, 2), "\n")

cat("\nITEM DIFFICULTY RANGE:\n")
cat("Easiest Item:", item_cols[which.min(item_difficulties)], 
    "Logit:", round(min(item_difficulties), 2), "\n")
cat("Hardest Item:", item_cols[which.max(item_difficulties)], 
    "Logit:", round(max(item_difficulties), 2), "\n")

cat("\nPERSON ABILITY RANGE:\n")
cat("Lowest Ability:", round(min(person_params$theta), 2), "logits\n")
cat("Highest Ability:", round(max(person_params$theta), 2), "logits\n")
cat("Mean Ability:", round(mean(person_params$theta), 2), "logits\n")

cat("\nRATING SCALE QUALITY:\n")
if (exists("threshold_distances")) {
  for(i in 1:length(threshold_distances)) {
    cat(sprintf("Distance from category %s: %.2f logits\n", 
               category_pairs[i], threshold_distances[i]))
    cat(sprintf("Within ideal range (1.4-5.0): %s\n", 
               threshold_distances[i] >= 1.4 & threshold_distances[i] <= 5.0))
  }
}

cat("\nITEM FIT SUMMARY:\n")
cat("Items with potential misfit (Infit or Outfit > 1.3 or < 0.7):\n")
misfit_items <- item_table[item_table$Infit_MNSQ > 1.3 | item_table$Infit_MNSQ < 0.7 | 
                          item_table$Outfit_MNSQ > 1.3 | item_table$Outfit_MNSQ < 0.7, ]
if(nrow(misfit_items) > 0) {
  print(misfit_items)
} else {
  cat("No items with significant misfit detected.\n")
}

cat("\nDEMOGRAPHIC DIFFERENCES:\n")
significant_vars <- demographic_vars[significance]
if(length(significant_vars) > 0) {
  cat("Significant differences found in the following demographic variables:\n")
  for(var in significant_vars) {
    cat("- ", var, "\n")
  }
} else {
  cat("No significant differences found across demographic variables.\n")
}

# ======================================================
# 12. RECOMMENDATIONS
# ======================================================

cat("\n============================================================\n")
cat("PRACTICAL RECOMMENDATIONS\n")
cat("============================================================\n")
cat("Actionable recommendations based on the Rasch analysis results\n\n")

# ------------------------------------------------------
# 12.1 Item Revision Recommendations
# ------------------------------------------------------
cat("A. ITEM REVISION RECOMMENDATIONS\n")
cat("==============================\n\n")

# Identify problematic items based on multiple criteria
cat("Items requiring attention based on psychometric properties:\n\n")

# 1. Misfit items (Infit/Outfit > 1.3 or < 0.7)
cat("1. Items with problematic fit statistics:\n")
if(nrow(misfit_items) > 0) {
  cat("The following items show misfit to the Rasch model and should be reviewed:\n")
  for(i in 1:nrow(misfit_items)) {
    item <- misfit_items$Item[i]
    infit <- misfit_items$Infit_MNSQ[i]
    outfit <- misfit_items$Outfit_MNSQ[i]
    
    # Diagnose the specific issue
    if(infit > 1.3 && outfit > 1.3) {
      issue <- "shows noise and inconsistency (high infit and outfit)"
      recommendation <- "Consider revising or removing this item. It may be measuring a different construct or have ambiguous wording."
    } else if(infit < 0.7 && outfit < 0.7) {
      issue <- "shows redundancy or over-predictability (low infit and outfit)"
      recommendation <- "This item may be measuring the same aspect as other items. Consider merging with similar items or revising to increase uniqueness."
    } else if(infit > 1.3) {
      issue <- "shows unexpected response patterns from persons whose ability matches the item difficulty (high infit)"
      recommendation <- "Examine the item wording for clarity and relevance. It may be confusing to the target population."
    } else if(outfit > 1.3) {
      issue <- "shows unexpected responses from persons whose ability is far from the item difficulty (high outfit)"
      recommendation <- "Check for potential scoring errors or technical issues with this item."
    } else if(infit < 0.7) {
      issue <- "shows overly predictable responses for persons whose ability matches the item difficulty (low infit)"
      recommendation <- "This item may be too deterministic. Consider making it more discriminating."
    } else if(outfit < 0.7) {
      issue <- "shows overly predictable responses from persons whose ability is far from the item difficulty (low outfit)"
      recommendation <- "While technically overfit, this may not be problematic unless the assessment needs more stochastic properties."
    }
    
    cat(sprintf("   - Item %s (Infit: %.2f, Outfit: %.2f) %s\n     Recommendation: %s\n\n", 
               item, infit, outfit, issue, recommendation))
  }
} else {
  cat("   None of the items show problematic fit statistics. All items function well within the model.\n\n")
}

# 2. Items with extreme difficulties
extreme_easy_items <- item_category_table[item_category_table$Category == "Difficulty Strata I", ]
extreme_difficult_items <- item_category_table[item_category_table$Category == "Difficulty strata V", ]

cat("2. Items with extreme difficulties:\n")

if(nrow(extreme_easy_items) > 0) {
  cat("   Very easy items (might not be discriminating for high-ability respondents):\n")
  for(i in 1:nrow(extreme_easy_items)) {
    cat(sprintf("   - Item %s (Logit: %.2f)\n     Recommendation: Consider making this item more challenging or replacing it with a more difficult item.\n\n", 
               extreme_easy_items$Item[i], extreme_easy_items$Logit[i]))
  }
}

if(nrow(extreme_difficult_items) > 0) {
  cat("   Very difficult items (might not be discriminating for low-ability respondents):\n")
  for(i in 1:nrow(extreme_difficult_items)) {
    cat(sprintf("   - Item %s (Logit: %.2f)\n     Recommendation: Consider making this item less challenging or replacing it with an easier item.\n\n", 
               extreme_difficult_items$Item[i], extreme_difficult_items$Logit[i]))
  }
}

if(nrow(extreme_easy_items) == 0 && nrow(extreme_difficult_items) == 0) {
  cat("   None of the items have extreme difficulties. The item difficulty spread is appropriate.\n\n")
}

# 3. Items showing DIF
cat("3. Items showing Differential Item Functioning (DIF):\n")
if(length(dif_significant_items) > 0 && sum(lengths(dif_significant_items)) > 0) {
  for(dem_var in names(dif_significant_items)) {
    items_with_dif <- dif_significant_items[[dem_var]]
    if(length(items_with_dif) > 0) {
      cat(sprintf("   Items showing significant DIF based on %s:\n", dem_var))
      for(item in items_with_dif) {
        cat(sprintf("   - Item %s\n     Recommendation: Review this item for potential bias related to %s. Consider rewording to ensure fairness across all demographic groups.\n\n", 
                   item, dem_var))
      }
    }
  }
} else {
  cat("   No items show significant DIF. The assessment appears to function similarly across demographic groups.\n\n")
}

# 4. Items affecting scale coverage
cat("4. Scale coverage analysis:\n")

# Calculate item-person map statistics
person_item_gap <- person_mean - item_mean
person_max <- max(person_params$theta)
person_min <- min(person_params$theta)
item_max <- max(item_difficulties)
item_min <- min(item_difficulties)

# Check for gaps in the item difficulty distribution
item_diffs_sorted <- sort(item_difficulties)
item_gaps <- diff(item_diffs_sorted)
large_gaps <- which(item_gaps > (mean(item_gaps) + sd(item_gaps)))

if(length(large_gaps) > 0) {
  cat("   The following gaps in item difficulty coverage were detected:\n")
  for(i in large_gaps) {
    lower_item <- item_cols[which(item_difficulties == item_diffs_sorted[i])]
    upper_item <- item_cols[which(item_difficulties == item_diffs_sorted[i+1])]
    gap_size <- item_gaps[i]
    
    cat(sprintf("   - Gap of %.2f logits between items %s (%.2f) and %s (%.2f)\n     Recommendation: Consider adding a new item with difficulty around %.2f logits to improve measurement precision in this ability range.\n\n", 
               gap_size, lower_item, item_diffs_sorted[i], upper_item, item_diffs_sorted[i+1], 
               (item_diffs_sorted[i] + item_diffs_sorted[i+1])/2))
  }
}

# Check for ceiling/floor effects
ceiling_gap <- person_max - item_max
floor_gap <- item_min - person_min

if(ceiling_gap > 2) {
  cat(sprintf("   Ceiling effect detected: The highest person ability (%.2f) is considerably higher than the most difficult item (%.2f)\n", person_max, item_max))
  cat("   Recommendation: Add more challenging items to better discriminate among high-ability respondents.\n\n")
}

if(floor_gap > 2) {
  cat(sprintf("   Floor effect detected: The lowest person ability (%.2f) is considerably lower than the easiest item (%.2f)\n", person_min, item_min))
  cat("   Recommendation: Add easier items to better discriminate among low-ability respondents.\n\n")
}

if(length(large_gaps) == 0 && ceiling_gap <= 2 && floor_gap <= 2) {
  cat("   The item difficulty distribution covers the person ability range adequately. No significant gaps or ceiling/floor effects detected.\n\n")
}

# ------------------------------------------------------
# 12.2 Psychometric Implications
# ------------------------------------------------------
cat("B. PSYCHOMETRIC IMPLICATIONS\n")
cat("==========================\n\n")

# 1. Reliability assessment
cat("1. Reliability assessment:\n")
if(person_reliability >= 0.9) {
  cat(sprintf("   Person reliability (%.2f) is excellent. The assessment provides highly precise measurement of person abilities.\n", person_reliability))
} else if(person_reliability >= 0.8) {
  cat(sprintf("   Person reliability (%.2f) is good. The assessment provides reliable measurement for group-level decisions.\n", person_reliability))
} else if(person_reliability >= 0.7) {
  cat(sprintf("   Person reliability (%.2f) is acceptable but could be improved. The assessment may be suitable for group-level decisions but not for high-stakes individual assessment.\n", person_reliability))
} else {
  cat(sprintf("   Person reliability (%.2f) is below recommended thresholds. Consider adding more items or reviewing existing items to improve measurement precision.\n", person_reliability))
}

if(item_reliability >= 0.9) {
  cat(sprintf("   Item reliability (%.2f) is excellent. The item hierarchy is very stable and would likely replicate with different samples.\n\n", item_reliability))
} else if(item_reliability >= 0.8) {
  cat(sprintf("   Item reliability (%.2f) is good. The item hierarchy is relatively stable.\n\n", item_reliability))
} else {
  cat(sprintf("   Item reliability (%.2f) is below optimal levels. The item hierarchy may not be stable across different samples. Consider increasing sample size in future studies.\n\n", item_reliability))
}

# 2. Rating scale functioning
cat("2. Rating scale functioning:\n")
if(exists("threshold_distances")) {
  optimal_thresholds <- all(threshold_distances >= 1.4 & threshold_distances <= 5.0)
  
  if(optimal_thresholds) {
    cat("   All threshold distances are within the optimal range (1.4-5.0 logits). The rating scale is functioning well with clear distinction between categories.\n\n")
  } else {
    cat("   Some threshold distances are outside the optimal range (1.4-5.0 logits):\n")
    for(i in 1:length(threshold_distances)) {
      if(threshold_distances[i] < 1.4) {
        cat(sprintf("   - Categories %s with distance %.2f logits: These categories may not be distinct enough. Consider collapsing categories %s.\n\n", 
                   category_pairs[i], threshold_distances[i], category_pairs[i]))
      } else if(threshold_distances[i] > 5.0) {
        cat(sprintf("   - Categories %s with distance %.2f logits: The gap between these categories is too large. Consider adding an intermediate category.\n\n", 
                   category_pairs[i], threshold_distances[i]))
      }
    }
  }
} else {
  cat("   Rating scale threshold information is not available. Further analysis is needed to evaluate category functioning.\n\n")
}

# 3. Construct validity
cat("3. Construct validity:\n")
if(exists("fa_result")) {
  variance_explained <- fa_result$Vaccounted[2,1]
  
  if(variance_explained >= 0.7) {
    cat(sprintf("   Excellent unidimensionality evidence: %.1f%% of variance is explained by the first factor.\n", variance_explained * 100))
    cat("   The assessment appears to measure a single construct well, supporting its construct validity.\n\n")
  } else if(variance_explained >= 0.5) {
    cat(sprintf("   Acceptable unidimensionality evidence: %.1f%% of variance is explained by the first factor.\n", variance_explained * 100))
    cat("   The primary construct is dominant, though secondary dimensions may be present.\n\n")
  } else {
    cat(sprintf("   Limited unidimensionality evidence: Only %.1f%% of variance is explained by the first factor.\n", variance_explained * 100))
    cat("   The assessment may be measuring multiple constructs. Consider factor analysis to identify distinct dimensions and potentially separate the assessment into subscales.\n\n")
  }
} else {
  cat("   Unidimensionality information is not available. Further analysis is needed to evaluate construct validity.\n\n")
}

# 4. Measurement precision
cat("4. Measurement precision across ability levels:\n")

# Analyze fit between person ability and item difficulty distributions
if(abs(person_item_gap) < 1) {
  cat("   The item difficulty distribution is well-targeted to the person ability distribution.\n")
  cat(sprintf("   Average person measure (%.2f logits) is close to the average item difficulty (%.2f logits).\n\n", person_mean, item_mean))
} else if(person_item_gap > 0) {
  cat("   The assessment may be too easy for the sample.\n")
  cat(sprintf("   Average person measure (%.2f logits) is higher than average item difficulty (%.2f logits).\n", person_mean, item_mean))
  cat("   Recommendation: Add more difficult items to better discriminate among high-ability respondents.\n\n")
} else {
  cat("   The assessment may be too difficult for the sample.\n")
  cat(sprintf("   Average person measure (%.2f logits) is lower than average item difficulty (%.2f logits).\n", person_mean, item_mean))
  cat("   Recommendation: Add easier items to better discriminate among low-ability respondents.\n\n")
}

# ------------------------------------------------------
# 12.3 Follow-up Recommendations
# ------------------------------------------------------
cat("C. FOLLOW-UP RECOMMENDATIONS\n")
cat("==========================\n\n")

# 1. Assessment revision recommendations
cat("1. Assessment revision strategy:\n")

# Create a prioritized list of items to revise
revision_priority <- data.frame(
  Item = item_cols,
  Priority = rep(0, length(item_cols)),
  Issues = rep("", length(item_cols))
)

# Add priority points for misfit items
if(nrow(misfit_items) > 0) {
  for(i in 1:nrow(misfit_items)) {
    item_idx <- which(revision_priority$Item == misfit_items$Item[i])
    revision_priority$Priority[item_idx] <- revision_priority$Priority[item_idx] + 3
    revision_priority$Issues[item_idx] <- paste(revision_priority$Issues[item_idx], "Misfit;", sep = "")
  }
}

# Add priority points for extreme difficulties
if(nrow(extreme_easy_items) > 0) {
  for(i in 1:nrow(extreme_easy_items)) {
    item_idx <- which(revision_priority$Item == extreme_easy_items$Item[i])
    revision_priority$Priority[item_idx] <- revision_priority$Priority[item_idx] + 1
    revision_priority$Issues[item_idx] <- paste(revision_priority$Issues[item_idx], "Too easy;", sep = "")
  }
}

if(nrow(extreme_difficult_items) > 0) {
  for(i in 1:nrow(extreme_difficult_items)) {
    item_idx <- which(revision_priority$Item == extreme_difficult_items$Item[i])
    revision_priority$Priority[item_idx] <- revision_priority$Priority[item_idx] + 1
    revision_priority$Issues[item_idx] <- paste(revision_priority$Issues[item_idx], "Too difficult;", sep = "")
  }
}

# Add priority points for DIF items
if(length(dif_significant_items) > 0) {
  for(dem_var in names(dif_significant_items)) {
    items_with_dif <- dif_significant_items[[dem_var]]
    for(item in items_with_dif) {
      item_idx <- which(revision_priority$Item == item)
      revision_priority$Priority[item_idx] <- revision_priority$Priority[item_idx] + 2
      revision_priority$Issues[item_idx] <- paste(revision_priority$Issues[item_idx], paste0("DIF(", dem_var, ");"), sep = "")
    }
  }
}

# Clean up the Issues field and sort by priority
revision_priority$Issues <- gsub("^;|;$", "", revision_priority$Issues)
revision_priority <- revision_priority[order(-revision_priority$Priority), ]

# Print prioritized revision list
if(any(revision_priority$Priority > 0)) {
  cat("   Prioritized item revision list (ordered by psychometric importance):\n")
  for(i in 1:nrow(revision_priority)) {
    if(revision_priority$Priority[i] > 0) {
      cat(sprintf("   %d. Item %s - Issues: %s\n", 
                 i, revision_priority$Item[i], revision_priority$Issues[i]))
    }
  }
  cat("\n")
} else {
  cat("   No items require immediate revision based on psychometric properties.\n\n")
}

# 2. Additional data collection recommendations
cat("2. Additional data collection:\n")

if(nrow(data) < 100) {
  cat("   The current sample size is small for Rasch analysis. Consider collecting additional data to improve parameter stability.\n")
  cat(sprintf("   Recommended minimum sample size: 200-250 respondents (current: %d).\n\n", nrow(data)))
} else if(nrow(data) < 250) {
  cat("   The current sample size is adequate but could be improved for more stable parameter estimation.\n")
  cat(sprintf("   Recommended sample size: 250+ respondents (current: %d).\n\n", nrow(data)))
} else {
  cat("   The current sample size is sufficient for stable parameter estimation.\n\n")
}

# Check for demographic representation
demographic_representation <- TRUE
for(dem_var in demographic_vars) {
  counts <- table(data[[dem_var]])
  if(any(counts < 30)) {
    demographic_representation <- FALSE
    cat(sprintf("   Consider collecting more data from under-represented %s groups to improve DIF analysis.\n", dem_var))
    print(counts)
    cat("\n")
  }
}

if(demographic_representation) {
  cat("   All demographic groups have adequate representation in the current sample.\n\n")
}

# 3. Implementation suggestions
cat("3. Implementation suggestions:\n")

if(person_reliability >= 0.8 && all(item_table$Infit_MNSQ > 0.7 & item_table$Infit_MNSQ < 1.3 & 
                                   item_table$Outfit_MNSQ > 0.7 & item_table$Outfit_MNSQ < 1.3)) {
  cat("   The assessment is psychometrically sound and ready for operational use. Consider the following implementation steps:\n\n")
} else {
  cat("   The assessment requires revisions before operational use. After implementing the recommended changes, follow these steps:\n\n")
}

cat("   a) Create an operational scoring guide based on the Rasch calibration:\n")
cat(sprintf("      - Define readiness categories using the cutoffs: Not Ready (< %.2f), Mostly Ready (%.2f to %.2f), Very Ready (> %.2f)\n", 
           person_lower_cutoff, person_lower_cutoff, person_upper_cutoff, person_upper_cutoff))
cat("      - Consider using the Rasch person measures (logits) as the primary score rather than raw scores\n\n")

cat("   b) Develop interpretation guidelines for stakeholders:\n")
cat("      - Create a simple explanation of what each readiness category means in practical terms\n")
cat("      - Develop targeted recommendations for individuals in each category\n\n")

cat("   c) Consider implementing computerized adaptive testing (CAT) for more efficient assessment:\n")
cat("      - The Rasch calibration provides item parameters needed for CAT implementation\n")
cat("      - CAT can reduce testing time while maintaining measurement precision\n\n")

# 4. Future research and development
cat("4. Future research and development:\n")

cat("   a) Longitudinal validation:\n")
cat("      - Conduct a follow-up study to examine the stability of the Rasch measures over time\n")
cat("      - Investigate the predictive validity of the assessment for relevant outcomes\n\n")

cat("   b) Expand the item bank:\n")
cat("      - Develop and calibrate additional items to improve measurement precision across the ability continuum\n")
cat("      - Consider creating parallel forms for repeated testing\n\n")

# Write a condensed, prioritized action plan
cat("\nD. PRIORITIZED ACTION PLAN\n")
cat("========================\n\n")

cat("Based on the comprehensive analysis, here is a prioritized action plan:\n\n")

# Priority 1: Critical item revisions
if(any(revision_priority$Priority >= 3)) {
  cat("1. Immediate actions (high priority):\n")
  high_priority_items <- revision_priority[revision_priority$Priority >= 3, ]
  for(i in 1:nrow(high_priority_items)) {
    cat(sprintf("   - Revise or replace Item %s due to %s\n", 
               high_priority_items$Item[i], high_priority_items$Issues[i]))
  }
  cat("\n")
} else {
  cat("1. Immediate actions (high priority):\n")
  cat("   - No critical item issues requiring immediate attention\n\n")
}

# Priority 2: Scale optimization
cat("2. Short-term improvements (medium priority):\n")
if(person_reliability < 0.8) {
  cat("   - Improve reliability by adding more items to the assessment\n")
}

if(exists("threshold_distances") && any(threshold_distances < 1.4 | threshold_distances > 5.0)) {
  cat("   - Revise the rating scale structure to improve category functioning\n")
}

if(length(large_gaps) > 0) {
  cat("   - Add items to fill gaps in the difficulty distribution\n")
}

if(ceiling_gap > 2 || floor_gap > 2) {
  cat("   - Add items to address ceiling or floor effects\n")
}

if(person_reliability >= 0.8 && !exists("threshold_distances") && length(large_gaps) == 0 && ceiling_gap <= 2 && floor_gap <= 2) {
  cat("   - The assessment structure is sound; focus on item-level improvements\n")
}
cat("\n")

# Priority 3: Documentation and implementation
cat("3. Implementation steps (ongoing):\n")
cat("   - Develop a comprehensive scoring manual with interpretation guidelines\n")
cat("   - Create user-friendly reports for communicating results to stakeholders\n")
cat("   - Train administrators on proper use and interpretation of the assessment\n\n")

# Priority 4: Future development
cat("4. Long-term development (future):\n")
cat("   - Conduct predictive validity studies to link assessment results with practical outcomes\n")
cat("   - Explore computerized adaptive testing implementation for more efficient assessment\n")
cat("   - Consider developing specialized modules for specific subpopulations if needed\n\n")

cat("\n============================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================================\n")
cat("All visualizations have been saved as PDF files.\n")
cat("Thank you for using the Rating Scale Model Analysis Script!\n")