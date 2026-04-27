# ==============================================================================
# Lego Dataset
# Group Members: 
# Utkarsh Awasthi
# ==============================================================================

# ============================================
# Load Libraries and Packages
# ============================================
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(car)
library(broom)
library(stargazer)
library(tidyverse)
library(tidyr)

# ============================================
# Load csv file and convert to DataFrame
# ============================================
# To ensure that the data cleaning processes for each question does not affect the 
# analyses of other questions, three separate data frames were created. 
# Each dataframe corresponds to a specific question.

# Load Lego Dataset
lego <- read.csv("LegoData.csv")

# Create DataFrame for Question 1
lego_df <- data.frame(lego)

# Create DataFrame for Question 2
Q2_df <- data.frame(lego)

# Create DataFrame for Question 3
Q3_df <- data.frame(lego)


# ==============================================================================
# QUESTION 1: 
# Is there's a relationship between theme and the number of unique pieces? 
# Do licensed sets feature more unique pieces?
# ==============================================================================

# ============================================
# INVESTIGATING NA THEMES [Slide 5]
# ============================================

# Remove special characters from the Theme column.
lego_df$Theme <- str_replace_all(lego_df$Theme, "[^[:alnum:] ]", "")  

cat("=== CHECKING FOR NA THEMES ===\n")

# Count NA themes
na_theme_count <- sum(is.na(lego_df$Theme))
cat("Number of sets with NA Theme:", na_theme_count, "\n")

# View sets with NA themes
na_theme_sets <- lego_df[is.na(lego_df$Theme), ]
cat("Total rows in dataset:", nrow(lego_df), "\n")
cat("Rows with NA Theme:", nrow(na_theme_sets), "\n")
cat("Percentage:", round((nrow(na_theme_sets)/nrow(lego_df))*100, 2), "%\n")


# ============================================================
# CATEGORISING THEMES INTO LICENSED VS. NON.LICENSED [Slide 5]
# ============================================================

# Count the unique themes and confirm the number
unique_themes_count <- length(unique(na.omit(lego_df$Theme)))

# Print the number of unique themes
print(unique_themes_count)

# Categorise themes into 'Licensed' and 'Non-licensed'

# Licensed themes (based on external IP)
licensed_themes <- c("Batman™", "DC", "Disney™", "Harry Potter™", "Ideas", "Jurassic World™", 
                     "LEGO® Frozen 2", "LEGO® Super Mario™", "Marvel", "Minecraft™", 
                     "Minions", "Overwatch®", "Powerpuff Girls™", "Speed Champions",  "Spider-Man", 
                     "Star Wars™", "Stranger Things", "THE LEGO® MOVIE 2™", 
                     "Trolls World Tour", "Unikitty!™")

# Non-licensed themes (LEGO original)
non_licensed_themes <- c("Architecture", "BrickHeadz", "City", "Classic", "Creator 3-in-1", 
                         "Creator Expert", "DOTS", "DUPLO®", "Friends", "Hidden Side", 
                         "Juniors", "LEGO® Art", "LEGO® Brick Sketches™", 
                         "LEGO® Education", "Minifigures", "Monkie Kid", "NINJAGO®", 
                         "Powered UP", "Technic™", "Xtra")

# Print the number of licensed themes
print(licensed_themes)

# Print the number of non_licensed themes
print(non_licensed_themes)

cat("Defined Licensed themes:", length(licensed_themes), "\n")
cat("Defined of Non-licensed themes:", length(non_licensed_themes), "\n\n")

# Clean the theme lists to match the cleaned Theme column
licensed_themes_clean <- str_replace_all(licensed_themes, "[^[:alnum:] ]", "") 
non_licensed_themes_clean <- str_replace_all(non_licensed_themes, "[^[:alnum:] ]", "")  

# Create Licensing Status column
lego_df$Licensing_Status <- ifelse(lego_df$Theme %in% licensed_themes_clean, 1, 0)

# Create Licensing Status column
#lego_df$Licensing_Status <- ifelse(grepl(paste(licensed_themes, collapse = "|"), 
#                                        lego_df$Theme), 1, 0)

# Create categorical variable
lego_df$License_Category <- ifelse(lego_df$Licensing_Status == 1, 
                                   "Licensed", "Non-Licensed")

# Check the first few rows of the dataset to confirm the changes
head(lego_df)

# Summary statistics
cat("Dataset Summary:\n")
cat("Total rows:", nrow(lego_df), "\n")
cat("Unique themes:", length(unique(na.omit(lego_df$Theme))), "\n")
cat("Number of Licensed themed sets within the dataset:", sum(lego_df$Licensing_Status == 1), "\n")
cat("Number of Non-licensed themes sets within the dataset:", sum(lego_df$Licensing_Status == 0), "\n")


# ===================================================
# PERFORMING STATISTICAL TESTS -
# MORE PIECES IN A LICENSED SET VS. NON-LICENSED SETS
# T-test, Box-plot, Bar-chart: [Slide 5]
# ===================================================

#=======================================  
# Summary Statistics
#=======================================

# Summary statistics for pieces by licensing status
piece_summary <- lego_df %>%
  group_by(License_Category) %>%
  summarise(
    count = n(),
    mean_pieces = mean(Pieces, na.rm = TRUE),
    median_pieces = median(Pieces, na.rm = TRUE),
    sd_pieces = sd(Pieces, na.rm = TRUE),
    min_pieces = min(Pieces, na.rm = TRUE),
    max_pieces = max(Pieces, na.rm = TRUE)
  )

print(piece_summary)

# Calculate the difference
licensed_mean <- piece_summary$mean_pieces[piece_summary$License_Category == "Licensed"]
non_licensed_mean <- piece_summary$mean_pieces[piece_summary$License_Category == "Non-Licensed"]

difference <- licensed_mean - non_licensed_mean
percentage_diff <- (difference / non_licensed_mean) * 100

# Clear statement of results
cat("\n=== COMPARISON RESULTS ===\n")
cat("Licensed sets average:", round(licensed_mean, 2), "pieces\n")
cat("Non-licensed sets average:", round(non_licensed_mean, 2), "pieces\n")
cat("Difference:", round(difference, 2), "pieces\n")
cat("Percentage difference:", round(percentage_diff, 2), "%\n\n")

if (licensed_mean > non_licensed_mean) {
  cat("CONCLUSION: Licensed sets have MORE pieces on average than non-licensed sets.\n")
  cat("Licensed sets have", round(difference, 2), "more pieces on average (", 
      round(percentage_diff, 2), "% more).\n")
} else {
  cat("CONCLUSION: Non-licensed sets have MORE pieces on average than licensed sets.\n")
  cat("Non-licensed sets have", round(abs(difference), 2), "more pieces on average (", 
      round(abs(percentage_diff), 2), "% more).\n")
}


# ============================================
# 2. Bar chart comparison [Slide 5]
# ============================================

ggplot(piece_summary, aes(x = License_Category, y = mean_pieces, fill = License_Category)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(mean_pieces, 0)), vjust = -0.5, size = 5) +
  labs(title = "Average Number of Pieces: Licensed vs Non-Licensed Sets",
       subtitle = paste0("Difference: ", round(difference, 0), " pieces (", 
                         round(percentage_diff, 1), "%)"),
       x = "Licensing Status", 
       y = "Average Number of Pieces") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))


# ============================================
# 3. Box plot comparison
# ============================================

ggplot(lego_df, aes(x = License_Category, y = Pieces, fill = License_Category)) +
  geom_boxplot() +
  labs(title = "Number of Pieces: Licensed vs Non-Licensed Sets",
       x = "Licensing Status", 
       y = "Number of Pieces") +
  theme_minimal() +
  theme(legend.position = "none")


# Optional: Filter out extreme outliers for better visualisation
# (e.g., sets with more than 2000 pieces)
lego_df_filtered <- lego_df %>% filter(Pieces <= 2000)

ggplot(lego_df_filtered, aes(x = License_Category, y = Pieces, fill = License_Category)) +
  geom_boxplot() +
  labs(title = "Number of Pieces: Licensed vs Non-Licensed (≤2000 pieces)",
       x = "Licensing Status", 
       y = "Number of Pieces") +
  theme_minimal() +
  theme(legend.position = "none")


# ============================================
# 4. Welch t-test [Slide 6]
# ============================================

t_test_result <- t.test(Pieces ~ License_Category, data = lego_df)
print(t_test_result)

# Statistical t-test to confirm if difference is significant
t_test <- t.test(Pieces ~ License_Category, data = lego_df)
cat("\nStatistical significance (p-value):", round(t_test$p.value, 4), "\n")
if (t_test$p.value < 0.05) {
  cat("The difference IS statistically significant (p < 0.05)\n")
} else {
  cat("The difference is NOT statistically significant (p >= 0.05)\n")
}

#=======================================  
# UNIQUE PIECES ANALYSIS:
# LICENSED SETS VS. NON-LICENSED SETS
#=======================================

# Get licensed sets (Licensing_Status = 1)
licensed_sets <- lego_df[lego_df$Licensing_Status == 1, ]

# Get non-licensed sets (Licensing_Status = 0)
non_licensed_sets <- lego_df[lego_df$Licensing_Status == 0, ]


# ============================================
# ASSUMPTION CHECKING: NORMALITY
# STATISTICAL TESTS: UNIQUE PIECES - LICENSED VS NON-LICENSED
# ============================================

# ============================================
# 1. Summary Statistics [Slide 7]
# ============================================

cat("=== SUMMARY STATISTICS ===\n")
cat("\nLicensed Sets:\n")
summary(licensed_sets$Unique_Pieces)
cat("\nNon-Licensed Sets:\n")
summary(non_licensed_sets$Unique_Pieces)

# ============================================
# 2. Check for missing values
# ============================================

cat("\n=== MISSING VALUES ===\n")
cat("NAs in Licensed sets:", sum(is.na(licensed_sets$Unique_Pieces)), "\n")
cat("NAs in Non-Licensed sets:", sum(is.na(non_licensed_sets$Unique_Pieces)), "\n")

# ============================================
# 3. Variance Analysis [Slide 7]
# ============================================

cat("\n=== VARIANCE ANALYSIS ===\n")
# Check variances
var_licensed <- var(licensed_sets$Unique_Pieces, na.rm = TRUE)
var_non_licensed <- var(non_licensed_sets$Unique_Pieces, na.rm = TRUE)

cat("Variance - Licensed:", round(var_licensed, 2), "\n")
cat("Variance - Non-Licensed:", round(var_non_licensed, 2), "\n")
cat("Variance Ratio:", round(var_licensed / var_non_licensed, 2), "\n")

# Levene's test for equal variances
cat("\n=== LEVENE'S TEST (Homogeneity of Variance) ===\n")
levene_result <- leveneTest(Unique_Pieces ~ License_Category, data = lego_df)
print(levene_result)

if(levene_result$`Pr(>F)`[1] > 0.05) {
  cat("\nConclusion: Variances are equal (p > 0.05)\n")
} else {
  cat("\nConclusion: Variances are NOT equal (p < 0.05)\n")
}


##### Box-Plot comparing the mean unique pieces: licensed vs. non-licensed
# Create plot_data_facet for visualisations
plot_data_facet <- lego_df %>%  
  filter(!is.na(Unique_Pieces)) %>%  
  select(Unique_Pieces, License_Category, Licensing_Status)

plot_data_facet$License_Category <- factor(plot_data_facet$License_Category,   
                                           levels = c("Non-Licensed", "Licensed"))  

# Calculate slope (will be used from regression, but calculate early for plots) 
slope <- mean(licensed_sets$Unique_Pieces, na.rm = TRUE) - 
  mean(non_licensed_sets$Unique_Pieces, na.rm = TRUE)  
intercept <- mean(non_licensed_sets$Unique_Pieces, na.rm = TRUE) 

ggplot(plot_data_facet, aes(x = License_Category, y = Unique_Pieces, fill = License_Category)) +
  geom_boxplot(alpha = 0.6, width = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               fill = "red", color = "darkred") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = round(after_stat(y), 1)), 
               vjust = -1.5, size = 5, fontface = "bold") +
  labs(title = "Unique Pieces Comparison: Licensed vs Non-Licensed",
       subtitle = paste0("Red diamonds = mean | Difference = ", round(slope, 2), " pieces"),
       x = "Licensing Status",
       y = "Number of Unique Pieces") +
  scale_fill_manual(values = c("Non-Licensed" = "skyblue", "Licensed" = "coral")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "none")


##### Frequency diagram displaying the mean & median unique pieces: licensed vs. non-licensed [Slide 8]
# Calculate medians for both groups
median_licensed <- median(licensed_sets$Unique_Pieces, na.rm = TRUE)
median_non_licensed <- median(non_licensed_sets$Unique_Pieces, na.rm = TRUE)

# Side-by-side histograms with mean AND median lines
ggplot(plot_data_facet, aes(x = Unique_Pieces, fill = License_Category)) +
  geom_histogram(bins = 30, alpha = 0.7, color = "white") +
  
  # Mean lines (red, dashed)
  geom_vline(data = data.frame(
    License_Category = c("Non-Licensed", "Licensed"),
    mean_val = c(intercept, intercept + slope)
  ), aes(xintercept = mean_val), color = "red", linetype = "dashed", linewidth = 1.2) +
  
  # Median lines (blue, solid)
  geom_vline(data = data.frame(
    License_Category = c("Non-Licensed", "Licensed"),
    median_val = c(median_non_licensed, median_licensed)
  ), aes(xintercept = median_val), color = "blue", linetype = "solid", linewidth = 1.2) +
  
  # Mean labels
  geom_text(data = data.frame(
    License_Category = c("Non-Licensed", "Licensed"),
    mean_val = c(intercept, intercept + slope),
    label = c(paste("Mean =", round(intercept, 1)),
              paste("Mean =", round(intercept + slope, 1)))
  ), aes(x = mean_val, y = Inf, label = label), 
  vjust = 2, hjust = 0.5, size = 4, fontface = "bold", color = "red") +
  
  # Median labels
  geom_text(data = data.frame(
    License_Category = c("Non-Licensed", "Licensed"),
    median_val = c(median_non_licensed, median_licensed),
    label = c(paste("Median =", round(median_non_licensed, 1)),
              paste("Median =", round(median_licensed, 1)))
  ), aes(x = median_val, y = Inf, label = label), 
  vjust = 4, hjust = 0.5, size = 4, fontface = "bold", color = "blue") +
  
  facet_wrap(~License_Category, scales = "free_y") +
  labs(title = "Distribution of Unique Pieces: Licensed vs Non-Licensed",
       subtitle = paste0("Red dashed = mean, Blue solid = median | Mean difference = ", 
                         round(slope, 2), " pieces"),
       x = "Number of Unique Pieces",
       y = "Frequency") +
  scale_fill_manual(values = c("Non-Licensed" = "skyblue", "Licensed" = "coral")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 12))


# ============================================
# 4. Shapiro-Wilk Normality Test [Slide 8-9]
# ============================================

cat("\n=== SHAPIRO-WILK TEST RESULTS ===\n")
shapiro_test_licensed <- shapiro.test(licensed_sets$Unique_Pieces)
shapiro_test_non_licensed <- shapiro.test(non_licensed_sets$Unique_Pieces)

cat("\nLicensed Sets:\n")
print(shapiro_test_licensed)
cat("\nNon-Licensed Sets:\n")
print(shapiro_test_non_licensed)


##### Frequency diagram - Shapiro-Wilk
plot_data <- rbind(
  data.frame(Unique_Pieces = licensed_sets$Unique_Pieces[!is.na(licensed_sets$Unique_Pieces)], 
             Group = paste0("Licensed (n=", sum(!is.na(licensed_sets$Unique_Pieces)), 
                            ")\np=", round(shapiro_test_licensed$p.value, 4))),
  data.frame(Unique_Pieces = non_licensed_sets$Unique_Pieces[!is.na(non_licensed_sets$Unique_Pieces)], 
             Group = paste0("Non-Licensed (n=", sum(!is.na(non_licensed_sets$Unique_Pieces)),
                            ")\np=", round(shapiro_test_non_licensed$p.value, 4)))
)

ggplot(plot_data, aes(x = Unique_Pieces, fill = Group)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.8) +
  facet_wrap(~Group, scales = "free_y") +
  labs(title = "Distribution of Unique Pieces by Licensing Status",
       subtitle = "Shapiro-Wilk Test for Normality",
       x = "Number of Unique Pieces",
       y = "Frequency") +
  scale_fill_manual(values = c("purple", "red")) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"))


##### Q-Q Plots - Shapiro-Wilk
# Create combined dataset for both groups
qq_data <- rbind(
  data.frame(Unique_Pieces = licensed_sets$Unique_Pieces,
             Group = paste0("Licensed (n=", sum(!is.na(licensed_sets$Unique_Pieces)), 
                            ")\nShapiro-Wilk p=", round(shapiro_test_licensed$p.value, 4))),
  data.frame(Unique_Pieces = non_licensed_sets$Unique_Pieces,
             Group = paste0("Non-Licensed (n=", sum(!is.na(non_licensed_sets$Unique_Pieces)), 
                            ")\nShapiro-Wilk p=", round(shapiro_test_non_licensed$p.value, 4)))
)

# Remove NAs
qq_data <- na.omit(qq_data)

# Generate side-by-side QQ plots using facet_wrap 
ggplot(qq_data, aes(sample = Unique_Pieces, color = Group)) + 
  stat_qq(size = 2) + 
  stat_qq_line(color = "red", linetype = "dashed", lwd = 1.1) + 
  facet_wrap(~Group, scales = "fixed") +
  labs(title = "Normal Q-Q Plots: Unique Pieces by Licensing Status",
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles") + 
  scale_color_manual(values = c("navy", "orange")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(face = "bold", size = 10))


# ============================================
# 5. Welch t-test [Slide 8-9]
# ============================================

cat("\n=== WELCH T-TEST RESULTS ===\n")
welch_t_test_result <- t.test(licensed_sets$Unique_Pieces, 
                              non_licensed_sets$Unique_Pieces, 
                              alternative = "two.sided")
print(welch_t_test_result)

if(welch_t_test_result$p.value < 0.05) {
  cat("\nConclusion: There IS a significant difference (p < 0.05)\n")
} else {
  cat("\nConclusion: There is NO significant difference (p >= 0.05)\n")
}


# ============================================
# SUMMARY COMPARISON - UNIQUE PIECES ANALYSIS 
# ============================================

cat("\n=== TEST COMPARISON SUMMARY: UNIQUE PIECES in LICENSED vs.NON-LICENSED ===\n")

# Normality Tests
cat("\n--- Normality Tests ---\n")
cat("Shapiro-Wilk (Licensed) p-value:", round(shapiro_test_licensed$p.value, 4), "\n")
cat("Shapiro-Wilk (Non-Licensed) p-value:", round(shapiro_test_non_licensed$p.value, 4), "\n")
cat("Levene's Test (Equal Variances) p-value:", round(levene_result$`Pr(>F)`[1], 4), "\n")

# Statistical Tests
cat("\n--- Statistical Tests ---\n")
cat("Welch t-test p-value:", round(welch_t_test_result$p.value, 4), "\n")

# Summary Statistics
cat("\n--- Summary Statistics ---\n")
cat("Licensed Sets:\n")
cat("  n =", sum(!is.na(licensed_sets$Unique_Pieces)), "\n")
cat("  Mean =", round(mean(licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "unique pieces\n")
cat("  Median =", round(median(licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "unique pieces\n")
cat("  SD =", round(sd(licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "\n")
cat("  Variance =", round(var(licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "\n")

cat("\nNon-Licensed Sets:\n")
cat("  n =", sum(!is.na(non_licensed_sets$Unique_Pieces)), "\n")
cat("  Mean =", round(mean(non_licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "unique pieces\n")
cat("  Median =", round(median(non_licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "unique pieces\n")
cat("  SD =", round(sd(non_licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "\n")
cat("  Variance =", round(var(non_licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "\n")

cat("\n=== EFFECT SIZE: UNIQUE PIECES ===\n")
cat("Mean difference:", round(mean(licensed_sets$Unique_Pieces, na.rm = TRUE) - 
                                mean(non_licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "unique pieces\n")
cat("Percentage difference:", 
    round((mean(licensed_sets$Unique_Pieces, na.rm = TRUE) - 
             mean(non_licensed_sets$Unique_Pieces, na.rm = TRUE)) / 
            mean(non_licensed_sets$Unique_Pieces, na.rm = TRUE) * 100, 2), "%\n")

cat("\n=== FINAL CONCLUSION: UNIQUE PIECES in LICENSED vs.NON-LICENSED ===\n")
if(welch_t_test_result$p.value < 0.05) {
  cat("Licensed sets have significantly more unique pieces than non-licensed sets.\n")
  cat("Mean Licensed:", round(mean(licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "pieces\n")
  cat("Mean Non-Licensed:", round(mean(non_licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "pieces\n")
} else {
  cat("There is no significant difference in unique pieces between licensed and non-licensed sets.\n")
}


# ============================================
# THEME ANALYSIS
# STATISTICAL TESTS: UNIQUE PIECES - LICENSED VS NON-LICENSED
# ============================================

# ============================================
# 1. Summary Statistics by Theme
# ============================================

cat("\n=== UNIQUE PIECES BY THEME ===\n")

# Remove NA themes before analysis
lego_df_themes <- lego_df[!is.na(lego_df$Theme), ]

cat("Removed", sum(is.na(lego_df$Theme)), "sets with NA themes\n")
cat("Analysing", nrow(lego_df_themes), "sets across", 
    length(unique(lego_df_themes$Theme)), "themes\n\n")

# Calculate summary statistics for each theme
theme_summary <- lego_df_themes %>%
  group_by(Theme, License_Category) %>%
  summarise(
    count = n(),
    mean_pieces = mean(Unique_Pieces, na.rm = TRUE),
    median_pieces = median(Unique_Pieces, na.rm = TRUE),
    sd_pieces = sd(Unique_Pieces, na.rm = TRUE),
    min_pieces = min(Unique_Pieces, na.rm = TRUE),
    max_pieces = max(Unique_Pieces, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_pieces))

# Enhanced summary output
cat("=== THEME SUMMARY STATISTICS ===\n")
cat("Total themes analysed:", nrow(theme_summary), "\n")
cat("  - Licensed themes:", sum(theme_summary$License_Category == "Licensed"), "\n")
cat("  - Non-Licensed themes:", sum(theme_summary$License_Category == "Non-Licensed"), "\n\n")

# Overall statistics across all themes
cat("Overall Statistics Across All Themes:\n")
cat("  - Total sets analysed:", sum(theme_summary$count), "\n")
cat("  - Average sets per theme:", round(mean(theme_summary$count), 2), "\n")
cat("  - Average unique pieces (across themes):", round(mean(theme_summary$mean_pieces, na.rm = TRUE), 2), "\n")
cat("  - Median unique pieces (across themes):", round(median(theme_summary$mean_pieces, na.rm = TRUE), 2), "\n")
cat("  - Range:", round(min(theme_summary$mean_pieces, na.rm = TRUE), 2), "to", 
    round(max(theme_summary$mean_pieces, na.rm = TRUE), 2), "pieces\n\n")

# Theme with most pieces
cat("Theme with MOST unique pieces:\n")
cat("  - Theme:", theme_summary$Theme[1], "\n")
cat("  - Licensing:", theme_summary$License_Category[1], "\n")
cat("  - Mean pieces:", round(theme_summary$mean_pieces[1], 2), "\n")
cat("  - Number of sets:", theme_summary$count[1], "\n\n")

# Theme with least pieces
cat("Theme with LEAST unique pieces:\n")
cat("  - Theme:", theme_summary$Theme[nrow(theme_summary)], "\n")
cat("  - Licensing:", theme_summary$License_Category[nrow(theme_summary)], "\n")
cat("  - Mean pieces:", round(theme_summary$mean_pieces[nrow(theme_summary)], 2), "\n")
cat("  - Number of sets:", theme_summary$count[nrow(theme_summary)], "\n\n")

# Licensed vs Non-Licensed comparison at theme level
licensed_themes <- theme_summary %>% filter(License_Category == "Licensed")
non_licensed_themes <- theme_summary %>% filter(License_Category == "Non-Licensed")

cat("Comparison by Licensing Status:\n")
cat("Licensed Themes:\n")
cat("  - Number of themes:", nrow(licensed_themes), "\n")
cat("  - Total sets:", sum(licensed_themes$count), "\n")
cat("  - Average unique pieces:", round(mean(licensed_themes$mean_pieces, na.rm = TRUE), 2), "\n")
cat("  - Median unique pieces:", round(median(licensed_themes$mean_pieces, na.rm = TRUE), 2), "\n")

cat("\nNon-Licensed Themes:\n")
cat("  - Number of themes:", nrow(non_licensed_themes), "\n")
cat("  - Total sets:", sum(non_licensed_themes$count), "\n")
cat("  - Average unique pieces:", round(mean(non_licensed_themes$mean_pieces, na.rm = TRUE), 2), "\n")
cat("  - Median unique pieces:", round(median(non_licensed_themes$mean_pieces, na.rm = TRUE), 2), "\n")

# ============================================
# 2. Top 10 Themes by Mean Unique Pieces
# ============================================

cat("\n=== TOP 10 THEMES BY MEAN UNIQUE PIECES ===\n")
top_10_themes <- theme_summary %>%
  arrange(desc(mean_pieces)) %>%
  head(10)

print(top_10_themes)

# ============================================
# 3. Bottom 10 Themes by Mean Unique Pieces
# ============================================

cat("\n=== BOTTOM 10 THEMES BY MEAN UNIQUE PIECES ===\n")
bottom_10_themes <- theme_summary %>%
  arrange(mean_pieces) %>%
  head(10)

print(bottom_10_themes)

# ============================================
# 4. Visualisation of LEGO data by Theme
# ============================================

# ----------------------------------------
# Visualisation 1: Bar Chart (Top 10 Themes)
# ----------------------------------------

cat("\n1. Creating bar chart: Top 10 themes by mean unique pieces...\n")

top_10_themes <- theme_summary %>%
  arrange(desc(mean_pieces)) %>%
  head(10)

ggplot(top_10_themes, aes(x = reorder(Theme, mean_pieces), y = mean_pieces, fill = License_Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(mean_pieces, 0)), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(title = "Top 10 Themes by Mean Unique Pieces",
       x = "Theme",
       y = "Mean Number of Unique Pieces") +
  scale_fill_manual(values = c("Licensed" = "darkgreen", "Non-Licensed" = "pink")) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom")

# ----------------------------------------
# Visualisation 2: Box Plot (Top 10 Themes) [Slide 10]
# ----------------------------------------

cat("\n2. Creating box plot: Distribution of top 10 themes...\n")

top_10_theme_names <- top_10_themes$Theme

lego_top_10 <- lego_df_themes %>%
  filter(Theme %in% top_10_theme_names)

ggplot(lego_top_10, aes(x = reorder(Theme, Unique_Pieces, FUN = median), 
                        y = Unique_Pieces, fill = License_Category)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  labs(title = "Distribution of Unique Pieces: Top 10 Themes",
       subtitle = "Box plots showing variation within each theme",
       x = "Theme",
       y = "Number of Unique Pieces") +
  scale_fill_manual(values = c("Licensed" = "green", "Non-Licensed" = "orange")) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom")


# ----------------------------------------
# Visualisation 3: Scatter Plot (Themes with 5+ Sets)
# ----------------------------------------

cat("\n3. Creating scatter plot: Themes with 5+ sets...\n")

themes_with_data <- theme_summary %>%
  filter(count >= 5)

cat("   Analysing", nrow(themes_with_data), "themes with 5 or more sets\n")

lego_filtered <- lego_df_themes %>%
  filter(Theme %in% themes_with_data$Theme)

ggplot(lego_filtered, aes(x = Theme, y = Unique_Pieces, color = License_Category)) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "red", color = "darkred") +
  coord_flip() +
  labs(title = "Unique Pieces by Theme (Themes with 5+ Sets)",
       subtitle = "Red diamonds = mean | Individual points show each set",
       x = "Theme",
       y = "Number of Unique Pieces") +
  scale_color_manual(values = c("Licensed" = "forestgreen", "Non-Licensed" = "coral")) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom")


# =================================================================
# 5. Summary - Licensed vs Non-Licensed Theme Comparison [Slide 11]
# =================================================================

cat("\n=== LICENSED VS NON-LICENSED THEMES COMPARISON ===\n")

# Compare licensed themes
licensed_theme_summary <- theme_summary %>%
  filter(License_Category == "Licensed") %>%
  arrange(desc(mean_pieces))

cat("\nTop 5 Licensed Themes:\n")
print(head(licensed_theme_summary, 5))

# Compare non-licensed themes
non_licensed_theme_summary <- theme_summary %>%
  filter(License_Category == "Non-Licensed") %>%
  arrange(desc(mean_pieces))

cat("\nTop 5 Non-Licensed Themes:\n")
print(head(non_licensed_theme_summary, 5))

# Statistical comparison

cat("Theme-level statistics (each theme weighted equally):\n")
cat("  - Number of Licensed themes:", nrow(licensed_theme_summary), "\n")
cat("  - Number of Non-Licensed themes:", nrow(non_licensed_theme_summary), "\n")
cat("  - Highest Licensed theme:", licensed_theme_summary$Theme[1], 
    "with", round(licensed_theme_summary$mean_pieces[1], 0), "pieces\n")
cat("  - Highest Non-Licensed theme:", non_licensed_theme_summary$Theme[1], 
    "with", round(non_licensed_theme_summary$mean_pieces[1], 0), "pieces\n")

cat("\n=== CONCLUSION ) ===\n")
cat("Mean unique pieces for Licensed SETS:", 
    round(mean(licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "\n")
cat("Mean unique pieces for Non-Licensed SETS:", 
    round(mean(non_licensed_sets$Unique_Pieces, na.rm = TRUE), 2), "\n")
cat("Difference: +", 
    round(mean(licensed_sets$Unique_Pieces, na.rm = TRUE) - 
            mean(non_licensed_sets$Unique_Pieces, na.rm = TRUE), 2), 
    " pieces (", 
    round(((mean(licensed_sets$Unique_Pieces, na.rm = TRUE) - 
              mean(non_licensed_sets$Unique_Pieces, na.rm = TRUE)) / 
             mean(non_licensed_sets$Unique_Pieces, na.rm = TRUE)) * 100, 2), 
    "% more)\n", sep = "")




# ==============================================================================
# QUESTION 2: 
# Is there a relationship between theme and price? 
# Does this relationship change when looking at Retail Price VS Amazon Price?
# ==============================================================================

# ============================================
# 1. Exploratory Data Analysis
# ============================================

# Assess the structure of the data 
str(Q2_df)

# Print the summary statistics of the lego dataset
summary(Q2_df$Amazon_price_clean)

# Count the number of missing values of each attribute
colSums(is.na(Q2_df))


# ============================================
# 1.1 Cleaning Theme Column [slide 12]
# ============================================

# Check how many themes are in the dataset
unique(Q2_df$Theme)

# Count the number of each theme
sort(table(Q2_df$Theme), decreasing=TRUE)

##### Handling the special characters
# source: <https://stringr.tidyverse.org/articles/regular-expressions.html>

# Removing the special characters from theme
Q2_df$Theme <- str_replace_all(Q2_df$Theme, "[^[:alnum:] ]", "")

# Check the cleaned theme 
table(Q2_df$Theme)


##### Subset the themes into lego original and external [Slide - 14]
#sources: 
#  - <https://stackoverflow.com/questions/52334544/r-how-to-add-data-to-a-new-column-by-subset-conditions> 
#  - <https://stackoverflow.com/questions/56078729/r-how-to-add-a-new-column-to-an-existing-dataframe-only-for-a-subset-of-rows-an> 
#  - <https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/ifelse>

# group the lego original themes
lego_originals <- c("Architecture", "BrickHeadz", "City",
                    "Classic", "Creator 3in1", "Creator Expert",
                    "DOTS", "DUPLO", "Friends", "Hidden Side", 
                    "Ideas", "Juniors", "LEGO Art", "Powered UP", 
                    "Speed Champions", "Technic", "THE LEGO MOVIE 2")

# If theme is in the lego_originals list, assign as "Lego Originals", else assign as "External"
# Save the data into a new column Theme_type
Q2_df$Theme_type <- ifelse(Q2_df$Theme %in% lego_originals, 
                           "Lego Originals", "External")

# Check the first 30 data on new column Theme_type
Q2_df$Theme_type[1:30]


# ==========================================================
# 1.2 Cleaning The Price and Amazon Price Columns [slide 12]
# ==========================================================

##### Remove dollar sign and missing values from price and amazon price
# source = <https://www.statology.org/remove-dollar-sign-in-r/>

###### Retail Price
# Duplicate the price column
Q2_df$Price_clean = Q2_df$Price

# Remove dollar sign
Q2_df$Price_clean = as.numeric(gsub("\\$", "", Q2_df$Price_clean))

# View all of the NA from price_new Column
Q2_df.na <- lego_df[is.na(Q2_df$Price_clean), ]

# Remove NA from price column
Q2_df <- Q2_df[!is.na(Q2_df$Price_clean), ]

# Check the cleaned Price column
Q2_df$Price_clean[1:30]


###### Amazon Price
# Duplicate the amazon price column
Q2_df$Amazon_price_clean = Q2_df$Amazon_Price

# Remove dollar sign
Q2_df$Amazon_price_clean = as.numeric(gsub("\\$", "", Q2_df$Amazon_price_clean))

# View all of the NA from Amazon_price_clean Column
Q2_df.na <- Q2_df[is.na(Q2_df$Amazon_price_clean), ]

# Remove NA from amazon price column
Q2_df <- Q2_df[!is.na(Q2_df$Amazon_price_clean), ]

# Check the cleaned Amazon Price column
Q2_df$Amazon_price_clean[1:30]


# ================================================================
# 1.3 Check the Distribution of Prices across Themes [Slide 12-13]
# ================================================================

# sources: 
# - <https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2> 
# - <https://ggplot2.tidyverse.org/reference/scale_continuous.html>

# Calculate the mean retail price and mean amazon price
means_retail <- aggregate(Price_clean ~ Theme_type, data = Q2_df, mean)
means_amazon <- aggregate(Amazon_price_clean ~ Theme_type, data = Q2_df, mean)

# Theme vs lego price
ggplot(Q2_df, aes(x = Theme, y = Price_clean)) +
  geom_boxplot(outlier.color = "red", fill="skyblue", alpha=0.5) +
  theme(axis.text.x = element_text(size=8, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=6))+
  scale_y_continuous(breaks = seq(0, 800, by = 100)) +
  ggtitle("Retail Prices Across Themes") + xlab("Themes") + ylab("Retail Price")

# Themes vs amazon price
ggplot(Q2_df, aes(x = Theme, y = Amazon_price_clean)) +
  geom_boxplot(outlier.color = "red", fill="orange", alpha=0.5) +
  theme(axis.text.x = element_text(size=8, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=6))+
  scale_y_continuous(breaks = seq(0, 800, by = 100)) +
  ggtitle("Amazon Prices Across Themes") + xlab("Themes") + ylab("Amazon Price")

# Theme type vs lego price
ggplot(Q2_df, aes(x = Theme_type, y = Price_clean)) + 
  geom_boxplot(outlier.color = "red",fill=c("skyblue", "orange"), alpha=0.5) +
  theme(axis.text.x = element_text(size=10, angle = 0), 
        axis.text.y = element_text(size=5)) +
  scale_y_continuous(breaks = seq(0, 1000, by = 100)) +
  ggtitle("Retail Price by Theme Type") + xlab("Theme Type") + ylab("Retail Price")

# Theme type vs Amazon price
ggplot(Q2_df, aes(x = Theme_type, y = Amazon_price_clean)) + 
  geom_boxplot(outlier.color = "red",fill=c("skyblue", "orange"), alpha=0.5) +
  theme(axis.text.x = element_text(size=10, angle = 0), 
        axis.text.y = element_text(size=5)) +
  scale_y_continuous(breaks = seq(0, 1000, by = 100)) +
  ggtitle("Amazon Prices by Theme Types") + xlab("Theme Type") + ylab("Amazon Price")


# =====================================
# 2. Hypothesis Testing for Question 2
# =====================================

# Assumptions:
# -   n > 30 but we don't know the population standard deviation is unknown, use t-test.
# -   comparing Price with theme types- use two-samples (two-sided) t-tests.
# -   comparing retail vs amazon price across theme types - use paired t-test.
# Source:  <https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/t.test>

# =============================================================
# 2.1 two-samples, two-tailed t-test (Amazone price) [Slide 15]
# =============================================================

# -   H0: mean amazon price of lego originals the same as mean price of external.
# -   H1: mean amazon price of lego originals not equal to the mean price of external.

t.test(Q2_df$Amazon_price_clean ~ Q2_df$Theme_type, alternative = "two.sided")

# P-value = 0.3697 > 0.05, there is not enough evidence to show that the mean amazon price of Lego-originals is the same as the mean price of external lego theme sets. 
# Therefore, we fail to reject the null hypothesis.
# Confidence interval [-4.99, 13.39]. The mean price for Lego-originals could be $4.99 more expensive or $13.39 cheaper than external.

# ============================================================
# 2.2 two-samples, two-tailed t-test (retail price) [Slide 16]
# ============================================================

# -   H0: mean retail price of lego originals the same as mean price of external themes.
# -   H1: mean retail price of lego originals not equal to the mean price of external themes.

t.test(Q2_df$Price_clean ~ Q2_df$Theme_type, alternative = "two.sided")

# P-value = 0.5447 > 0.05, there is not enough evidence that the mean retail price of lego originals is the same as the mean price of external lego themes. 
# Therefore, we fail to reject the null hypothesis.
# Confidence interval [-9.77, 5.16] lego originals mean price could be $9.77 more expensive or $5.16 cheaper than external.



# ==========================================================================================================
# # 2.3 Comparing the mean price of retail vs amazon price across the theme types (paired t-test) [Slide 17]
# ==========================================================================================================

# -   H0: mean of retail price is the same as the mean of amazon prices.
# -   H1: mean of retail price is not the same as the mean of amazon prices.

t.test(x = Q2_df$Price_clean, y = Q2_df$Amazon_price_clean,
       paired = TRUE,
       alternative = "two.sided")

# P-value = 2.2e-16 < 0.05, there is enough evidence that the mean retail price is not equal to the mean amazon price. Therefore, we reject the null hypothesis.
# t-value = -11.2, meaning that the mean retail price is higher than the mean amazon price.
# Based on the confidence interval and mean differences, the retail price is about 9 to 14 dollars more expensive than amazon price. 
# Mean difference implies that Lego originals are on average $12 more expensive than external. 


# =============================================================================
# 2.4 Summary Table (Means and Standard Deviation for each category) [Slide 18]
# =============================================================================

# Calculate the mean of retail price across theme types
aggregate(Price_clean ~ Theme_type, data = Q2_df, FUN = mean)

# Calculate the standard deviation of retail price for each theme types
aggregate(Price_clean ~ Theme_type, data = Q2_df, FUN = sd)

# Calculate the mean of Amazon price across theme types
aggregate(Amazon_price_clean ~ Theme_type, data = Q2_df, FUN = mean)

# Calculate the standard deviation of Amazon price for each theme types
aggregate(Amazon_price_clean ~ Theme_type, data = Q2_df, FUN = sd)




# ==============================================================================
# QUESTION 3: 
# Are there significant differences in the number of unique pieces between sets? 
# Is this due to them being aimed at a younger or older age groups?
# ==============================================================================

# ======================================
# Background for presentation [slide 19]
# ======================================

#On Brickset.com, a "unique piece" refers to a specific element (combination of design and 
#colour) that, at the time the set was released, was available in that particular set only. 
#This means the piece in that specific colour and/or with a specific print has not appeared 
#in any other LEGO set's inventory. 

#Scale: LEGO has produced over 3,700 active unique elements in recent years, though the 
#total catalog across history exceeds 10,000+ unique molds.

#The Unique_Pieces column represents the count of distinct brick types included in each set.
#It is interesting because tracking unique pieces helps identify which sets introduce rare 
#parts that might be valuable for custom builds or resale. Summing across all sets gives us 
#148,405 unique pieces in total.

#Unique_Pieces is a powerful complexity metric. Unlike total piece count, unique pieces 
#captures: design complexity, variety of elements, building challenge level.
#This makes it ideal for modelling how complexity scales with age.

# ============================================
# 1.1 Clean the Ages Variable [slide 20]
# ============================================
clean_age <- function(age_str) {
  x <- age_str
  
  # Replace half fractions (multiple encodings) this was necessary to change 1.5 to 2, I kept
  # ending up with age 1 as an option.
  x <- str_replace_all(x, "\u00BD", ".5") # standard ½
  x <- str_replace_all(x, "\xbd", ".5")   # your data encoding
  
  # Remove "Age" / "Ages"
  x <- str_replace_all(x, "Ages?", "")
  
  # Remove plus signs and spaces
  x <- str_replace_all(x, "\\+| ", "")
  
  # Extract first number (handles decimals)
  x <- str_extract(x, "\\d+(\\.\\d+)?")
  
  # Convert to numeric
  x <- as.numeric(x)
  
  # Always round .5 up
  x <- ifelse(!is.na(x) & x %% 1 == 0.5, ceiling(x), x)
  
  x
}

# Apply cleaning
Q3_df <- Q3_df %>%
  mutate(Age_clean = clean_age(Ages))


# ============================================
# 1.2 Ensure Unique_Pieces & Age has no NA
# ============================================
lego_clean <- Q3_df %>%
  filter(!is.na(Age_clean),
         !is.na(Unique_Pieces))


# ============================================
# 1.3 Visualisations [slide 20]
# ============================================

## Histogram of Age ##
# ----------------------
ggplot(lego_clean, aes(x = Age_clean)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Recommended Age",
    x = "Age (years)",
    y = "Frequency"
  )


## Histogram of Unique Pieces ##
# --------------------------------
ggplot(lego_clean, aes(x = Unique_Pieces)) +
  geom_histogram(binwidth = 20, fill = "darkorange", color = "white") +
  labs(
    title = "Distribution of Unique Pieces",
    x = "Unique Pieces",
    y = "Frequency"
  )


## Scatterplot: Unique Pieces vs Age ##
# --------------------------------------
ggplot(lego_clean, aes(x = Age_clean, y = Unique_Pieces)) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(
    title = "Unique Pieces vs Age",
    x = "Age (years)",
    y = "Unique Pieces"
  )


## Boxplot: Unique Pieces by Recommended Age ##
# ---------------------------------------------
ggplot(lego_clean, aes(x = factor(Age_clean), y = Unique_Pieces)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.alpha = 0.4) +
  labs(
    title = "Unique Pieces by Recommended Age",
    x = "Age (years)",
    y = "Unique Pieces"
  )



## Mean unique pieces by age ##
# ------------------------------
avg_age_unique <- lego_clean %>%
  group_by(Age_clean) %>%
  summarise(mean_unique = mean(Unique_Pieces),
            n_sets = n())

ggplot(avg_age_unique, aes(x = Age_clean, y = mean_unique)) +
  geom_line(color = "blue") +
  geom_point(aes(size = n_sets), color = "blue") +
  labs(title = "Average Unique Pieces by Age",
       x = "Age (years)",
       y = "Mean unique pieces")


# ============================================
# 1.4 Exploratory Data Analysis [slide 21-22]
# ============================================
##### Summary statistics
summary(lego_clean$Age_clean)
summary(lego_clean$Unique_Pieces)

##### Grouped summary
lego_clean %>%
  group_by(Age_clean) %>%
  summarise(
    n_sets = n(),
    mean_unique = mean(Unique_Pieces),
    sd_unique = sd(Unique_Pieces),
    median_unique = median(Unique_Pieces)
  ) %>%
  arrange(Age_clean)


##### Bar chart of tibble
summary_tibble <- tibble::tibble(
  Age_clean = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 18),
  n_sets = c(70, 2, 77, 122, 287, 212, 161, 66, 99, 7, 15, 4, 28, 17),
  mean_unique = c(29.6, 41, 87.3, 85.2, 87.1, 109, 174, 253, 110, 214, 256, 350, 409, 214),
  sd_unique = c(23.2, 11.3, 64.3, 60.9, 86.0, 68.2, 84.2, 103, 90.7, 52.0, 167, 244, 212, 158),
  median_unique = c(23.5, 41, 72, 68, 52, 100, 175, 264, 75, 234, 211, 314, 406, 172)
)

##### Add custom age groups (same for ANOVA later)
summary_tibble <- summary_tibble %>%
  mutate(age_group = case_when(
    Age_clean <= 4 ~ "Infant",          # under 4
    Age_clean >= 5 & Age_clean <= 12 ~ "Child",   # 5 to 12
    Age_clean >= 13 ~ "Teen/Adult"     # 13+
  ))

##### Bar chart with custom groups
ggplot(summary_tibble, aes(x = factor(Age_clean), y = mean_unique, fill = age_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_unique - sd_unique,
                    ymax = mean_unique + sd_unique),
                width = 0.2, color = "black") +
  scale_fill_manual(values = c("Infant" = "magenta", "Child" = "gold", "Teen/Adult" = "steelblue")) +
  labs(
    title = "Mean Unique Pieces by Age with Standard Deviation",
    x = "Age",
    y = "Mean Unique Pieces",
    fill = "Age Group"
  ) +
  theme_minimal()


##### Density plots for ages leading towards the idea of Anova (infant, child, teen/adult).
# Subset for ages 4, 10, 16
lego_subset <- lego_clean %>%
  filter(Age_clean %in% c(4, 10, 16))

# Calculate means for each age
mean_values <- lego_subset %>%
  group_by(Age_clean) %>%
  summarise(mean_unique = mean(Unique_Pieces))

# Density plot with mean lines + labels
ggplot(lego_subset, aes(x = Unique_Pieces, color = factor(Age_clean), fill = factor(Age_clean))) +
  geom_density(alpha = 0.3) +
  geom_vline(data = mean_values, aes(xintercept = mean_unique, color = factor(Age_clean)),
             linetype = "dashed", size = 1) +
  geom_text(data = mean_values,
            aes(x = mean_unique, y = 0.005,  # adjust y for label placement
                label = paste("Mean =", round(mean_unique, 1)),
                color = factor(Age_clean)),
            angle = 90, vjust = -0.5, hjust = 0) +
  labs(
    title = "Density of Unique Pieces: Ages 4 vs 10 vs 16",
    x = "Unique Pieces",
    y = "Density",
    color = "Age",
    fill = "Age"
  ) +
  theme_minimal()

# ============================================
# 2. Hypothesis Testing for Question 3
# ============================================

# =======================================================
# 2.1 ANOVA with individual ages as groups. [Slide 22-23]
# =======================================================

#Justification for this method is that Lego themselves sell their Lego in clear age groups
#with the new system changing from ranges to ages like 10+, 18+ etc.

#Do age groups have different numbers of unique pieces?

# Treat Age_clean as a categorical factor
lego_clean$Age_factor <- factor(lego_clean$Age_clean)

# One-way ANOVA
anova_model <- aov(Unique_Pieces ~ Age_factor, data = lego_clean)
summary(anova_model)

#Quality and Assumptions plots
par(mfrow = c(2,2))
plot(anova_model)

# Just Q-Q plot of residuals
par(mfrow = c(1,1))
qqnorm(residuals(anova_model))
qqline(residuals(anova_model))

ggplot(lego_clean, aes(x = factor(Age_clean), y = Unique_Pieces)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Unique Pieces by Age Group",
    x = "Age (years)",
    y = "Unique Pieces"
  )

#Boxplot above: Unique Pieces by Age


TukeyHSD(anova_model)

lego_clean %>%
  group_by(Age_clean) %>%
  summarise(mean_unique = mean(Unique_Pieces)) %>%
  ggplot(aes(x = Age_clean, y = mean_unique)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    title = "Mean Unique Pieces by Age",
    x = "Age (years)",
    y = "Mean Unique Pieces"
  )

ggplot(lego_clean, aes(x = factor(Age_clean), y = Unique_Pieces)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Unique Pieces by Age Group",
    x = "Age (years)",
    y = "Unique Pieces"
  )

#The dip at age 10
#Data Entry or Cleaning Issues: If age parsing grouped ranges like “9–10” or “10–11” 
#inconsistently, it could artificially depress the count at age 10.

#Visually confirming the dip is real and not just noise
lego_clean %>%
  group_by(Age_clean) %>%
  summarise(
    mean_unique = mean(Unique_Pieces),
    sd_unique = sd(Unique_Pieces),
    n = n(),
    se = sd_unique / sqrt(n)
  ) %>%
  ggplot(aes(x = Age_clean, y = mean_unique)) +
  geom_line(color = "blue") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_unique - se, ymax = mean_unique + se), width = 0.2) +
  labs(title = "Mean Unique Pieces by Age (with SE)", x = "Age", y = "Mean Unique Pieces")

#Right now, the dip looks more like noise than a strong signal, because the error bars are 
#fairly wide. The overall trend is upward, and the dip is small relative to variability.
#Without statistical testing, we can’t claim it’s a real effect.

#The dip could be an artefact of limited data
lego_clean %>%
  count(Age_clean)

#Age 10 has a large sample size
#Age 10 has 99 sets, which is more than age 9 (66), far more than ages 11–18
#This means the dip is not caused by small sample noise. It’s a real structural feature of 
#the dataset.

#Ages 11–18 have tiny sample sizes
#Look at these:
#Age 11 → 7 sets
# Age 12 → 15 sets
# Age 14 → 4 sets
# Age 16 → 28 sets
# Age 18 → 17 sets
#These tiny groups are dominated by high‑complexity sets (Technic, Star Wars, advanced 
#Creator, etc.).
#That artificially inflates their means, making age 10 look low by comparison.
#So the “dip” is partly an illusion created by:
#age 10 having many mid‑complexity sets
#ages 11–18 having a handful of very complex sets.


# ======================================
# 2.2 ANOVA with age categories [23-24]
# ======================================

#H0: There is no significant difference between the mean unique pieces for each age category.
#Mu1 = Mu2 = Mu3
#H1: At least one mean is different. 

# Create age categories
lego_clean <- lego_clean %>%
  mutate(Age_group = case_when(
    Age_clean <= 4 ~ "Infant",          # ages 2–4
    Age_clean >= 5 & Age_clean <= 12 ~ "Child",   # ages 5–12
    Age_clean >= 13 ~ "Teen/Adult"      # ages 13+
  ))

# Convert to factor so they are treated like categorical variables
lego_clean$Age_group <- factor(lego_clean$Age_group,
                               levels = c("Infant", "Child", "Teen/Adult"))

# Run ANOVA
anova_group <- aov(Unique_Pieces ~ Age_group, data = lego_clean)
summary(anova_group)

# QQ plot of residuals
par(mfrow = c(1,1))
qqnorm(residuals(anova_group), 
       main = "QQ Plot of ANOVA Residuals")
qqline(residuals(anova_group), col = "red")

# Shapiro Wilks test on residuals
shapiro.test(residuals(anova_group))

# ===============
# [Slide 25-26]
# ===============

# Non‑parametric test (e.g., Kruskal–Wallis) to check if the group differences hold without
#the normality assumption.
kruskal.test(Unique_Pieces ~ Age_group, data = lego_clean)

# Pairwise post-hoc comparisons
pairwise.wilcox.test(lego_clean$Unique_Pieces, lego_clean$Age_group,
                     p.adjust.method = "bonferroni")

# Post-hoc test (Tukey HSD)
TukeyHSD(anova_group)


# Boxplot of Unique Pieces by Age Group
ggplot(lego_clean, aes(x = Age_group, y = Unique_Pieces)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.alpha = 0.4) +
  labs(
    title = "Unique Pieces by Age Category",
    x = "Age Group",
    y = "Unique Pieces"
  )

# Bar chart with error bars (mean ± SE)
lego_clean %>%
  group_by(Age_group) %>%
  summarise(
    mean_unique = mean(Unique_Pieces),
    sd_unique   = sd(Unique_Pieces),
    n           = n(),
    se_unique   = sd_unique / sqrt(n)
  ) %>%
  ggplot(aes(x = Age_group, y = mean_unique, fill = Age_group)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean_unique - se_unique,
                    ymax = mean_unique + se_unique),
                width = 0.2) +
  labs(
    title = "Mean Unique Pieces by Age Category (± SE)",
    x = "Age Group",
    y = "Mean Unique Pieces"
  ) +
  theme_minimal()


# ==============================================================================
# Extended Analysis for Research Question 3
# Linear Regression for Unique Pieces versus Ages [Slide 27-28]
# ==============================================================================

lm1 <- lm(Unique_Pieces ~ Age_clean, data = lego_clean)
summary(lm1)

# QQ plot of residuals 
qqnorm(residuals(lm1), 
       main = "QQ Plot of Linear Regression Residuals") 
qqline(residuals(lm1), col = "red")

#Intercept = -15.99: At age 0 (not meaningful here, since data starts at 2), the expected 
#mean unique pieces would be slightly negative.

#Slope = 19.90: For each additional year of age, the mean number of unique pieces increases 
#by about 20.

#Highly significant (p < 2e-16): Age is a strong predictor of unique pieces overall.

#R² ≈ 0.26: Age explains about 26% of the variation. That’s decent, but it means there’s 
#still a lot of variability not captured by a simple straight line.

# Regression plot
ggplot(lego_clean, aes(x = Age_clean, y = Unique_Pieces)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(title = "Linear Regression: Unique Pieces ~ Age",
       x = "Age (years)",
       y = "Unique pieces")


# =========================================
# Testing Normality of Residuals [Slide 29]
# =========================================

# Hypotheses testing for Durbin-Watson test.
# H0: Residuals are not autocorrelated
# H1: Residuals have autocorrelation at lag 1

# Perform the Durbin-Watson test to check independence
lmtest::dwtest(lm1)

# The Durbin-Watson test score is 1.0971, hence, the linear regression has a positive autocorrelation.
# This means that as Ages for the lego set increases, the number of unique pieces will increase.

# P-value less than 2.2x10^-16, indicating that the test is significant at 5% level.


# =============================================================
# Testing for Constance Variance (homoscedasticity) [Slide 29]
# =============================================================

# H0: Homoscedasticity is present 
# H1: Heteroscedasticity is present

# Breusch-Pagan test for constant Variance
car::ncvTest(lm1)

# The P-value for the Breusch-Pagan test is less than 2.22x10^-16, indicating that 
# the test is significant at 5% level.

# Therefore, heteroscedasticity is present and that there is an uneven spread of variability
# in the data. 











