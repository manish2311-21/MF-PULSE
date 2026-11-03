# ================================================================
# MUTUAL FUND ANALYSIS USING R
# ================================================================

required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "forcats", "readr", "scales"
)

new_packages <- required_packages[
  !(required_packages %in% rownames(installed.packages()))
]

if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(readr)
  library(scales)
})

# ---------------------------
# 2. Create output folders
# ---------------------------
if (!dir.exists("outputs")) dir.create("outputs")
if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)
if (!dir.exists("outputs/figures")) dir.create("outputs/figures", recursive = TRUE)

# ---------------------------
# 3. Read and clean the data
# ---------------------------
input_file <- "mf_cleaned.xlsx"

if (!file.exists(input_file)) {
  message("mf_cleaned.xlsx was not found in the current working directory.")
  message("Please select the Excel file in the file-picker window.")
  input_file <- file.choose()
}

mf <- read_excel(input_file, sheet = 1)

# Standardize column names manually
names(mf) <- tolower(names(mf))
names(mf) <- gsub("[^a-z0-9]+", "_", names(mf))
names(mf) <- gsub("_$", "", names(mf))

required_columns <- c(
  "scheme_name", "rating", "category", "sub_category",
  "returns_1yr", "returns_3yr", "returns_5yr"
)

missing_columns <- setdiff(required_columns, names(mf))
if (length(missing_columns) > 0) {
  stop(
    paste(
      "The following required columns are missing:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

mf <- mf %>%
  mutate(
    rating = as.numeric(rating),
    returns_1yr = as.numeric(returns_1yr),
    returns_3yr = as.numeric(returns_3yr),
    returns_5yr = as.numeric(returns_5yr)
  ) %>%
  distinct(scheme_name, .keep_all = TRUE)

write_csv(mf, "outputs/tables/cleaned_mutual_funds.csv")

# ---------------------------
# 4. Basic data exploration
# ---------------------------
cat("\n================ DATASET OVERVIEW ================\n")
cat("Number of unique schemes:", nrow(mf), "\n")
cat("Number of categories:", n_distinct(mf$category), "\n")
cat("\nCategory counts:\n")
print(count(mf, category, sort = TRUE))

cat("\nMissing-value summary:\n")
missing_summary <- data.frame(
  variable = names(mf),
  missing_count = sapply(mf, function(x) sum(is.na(x))),
  missing_percent = sapply(mf, function(x) mean(is.na(x)) * 100)
)
print(missing_summary)
write_csv(missing_summary, "outputs/tables/missing_value_summary.csv")

# Summary statistics for returns
return_summary <- mf %>%
  summarise(
    across(
      c(returns_1yr, returns_3yr, returns_5yr),
      list(
        count = ~ sum(!is.na(.x)),
        minimum = ~ min(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        maximum = ~ max(.x, na.rm = TRUE),
        standard_deviation = ~ sd(.x, na.rm = TRUE)
      )
    )
  )

print(return_summary)
write_csv(return_summary, "outputs/tables/return_summary.csv")

# ---------------------------
# 5. Category-wise analysis
# ---------------------------
category_metrics <- mf %>%
  group_by(category) %>%
  summarise(
    schemes = n(),
    rating_coverage_percent = mean(!is.na(rating)) * 100,
    return_1yr_coverage_percent = mean(!is.na(returns_1yr)) * 100,
    return_3yr_coverage_percent = mean(!is.na(returns_3yr)) * 100,
    return_5yr_coverage_percent = mean(!is.na(returns_5yr)) * 100,
    median_1yr_return = median(returns_1yr, na.rm = TRUE),
    median_3yr_return = median(returns_3yr, na.rm = TRUE),
    median_5yr_return = median(returns_5yr, na.rm = TRUE),
    negative_1yr_return_percent = mean(returns_1yr < 0, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print(category_metrics)
write_csv(category_metrics, "outputs/tables/category_metrics.csv")

# ---------------------------
# 6. Investor scoring system
# ---------------------------

ranked_funds <- mf %>%
  mutate(
    return_1yr_percentile = percent_rank(returns_1yr) * 100,
    return_3yr_percentile = percent_rank(returns_3yr) * 100,
    return_5yr_percentile = percent_rank(returns_5yr) * 100,
    
    performance_score =
      0.20 * return_1yr_percentile +
      0.30 * return_3yr_percentile +
      0.50 * return_5yr_percentile,
    
    investor_score = case_when(
      !is.na(rating) ~
        0.80 * performance_score + 0.20 * (rating / 5 * 100),
      TRUE ~ performance_score
    ),
    
    complete_return_history = !is.na(returns_1yr) &
      !is.na(returns_3yr) &
      !is.na(returns_5yr),
    
    negative_1yr_return_flag = if_else(
      !is.na(returns_1yr) & returns_1yr < 0,
      "Yes", "No", missing = "Unavailable"
    ),
    
    low_rating_flag = if_else(
      !is.na(rating) & rating <= 2,
      "Yes", "No", missing = "Unavailable"
    ),
    
    risk_proxy_count =
      (negative_1yr_return_flag == "Yes") +
      (low_rating_flag == "Yes")
  )

# Main ranking: only funds with all three return horizons
primary_ranking <- ranked_funds %>%
  filter(complete_return_history) %>%
  arrange(desc(investor_score), desc(returns_5yr)) %>%
  mutate(rank = row_number())

write_csv(ranked_funds, "outputs/tables/all_funds_with_scores.csv")
write_csv(primary_ranking, "outputs/tables/ranked_funds_complete_history.csv")
write_csv(head(primary_ranking, 20), "outputs/tables/top_20_funds.csv")

# Top 5 funds within each broad category
top_5_by_category <- primary_ranking %>%
  group_by(category) %>%
  slice_max(order_by = investor_score, n = 5, with_ties = FALSE) %>%
  ungroup()

write_csv(top_5_by_category, "outputs/tables/top_5_funds_by_category.csv")

# Top funds by sub-category
top_3_by_subcategory <- primary_ranking %>%
  group_by(sub_category) %>%
  slice_max(order_by = investor_score, n = 3, with_ties = FALSE) %>%
  ungroup()

write_csv(top_3_by_subcategory, "outputs/tables/top_3_funds_by_subcategory.csv")

cat("\n================ TOP 20 FUNDS ================\n")
print(
  primary_ranking %>%
    select(
      rank, scheme_name, category, sub_category, rating,
      returns_1yr, returns_3yr, returns_5yr, investor_score
    ) %>%
    head(20)
)

# ---------------------------
# 7. Visualizations
# ---------------------------
plot_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "bottom"
  )

# Chart 1: Number of funds by category
p1 <- ggplot(mf, aes(x = fct_infreq(category))) +
  geom_bar(fill = "#2166AC") +
  coord_flip() +
  labs(
    title = "Number of mutual-fund schemes by category",
    x = "Category",
    y = "Number of schemes"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "outputs/figures/01_category_counts.png",
  p1, width = 9, height = 5, dpi = 180
)

# Chart 2: Return distributions
return_long <- mf %>%
  select(category, returns_1yr, returns_3yr, returns_5yr) %>%
  pivot_longer(
    cols = starts_with("returns_"),
    names_to = "horizon",
    values_to = "return_percent"
  ) %>%
  mutate(
    horizon = recode(
      horizon,
      returns_1yr = "1-year return",
      returns_3yr = "3-year return",
      returns_5yr = "5-year return"
    )
  )

p2 <- ggplot(return_long, aes(x = category, y = return_percent, fill = category)) +
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +
  facet_wrap(~ horizon, scales = "free_y") +
  labs(
    title = "Return distributions by category",
    x = "Category",
    y = "Return (%)"
  ) +
  plot_theme

ggsave(
  "outputs/figures/02_return_distributions.png",
  p2, width = 12, height = 6, dpi = 180
)

# Chart 3: 3-year versus 5-year returns
p3 <- mf %>%
  filter(!is.na(returns_3yr), !is.na(returns_5yr)) %>%
  ggplot(aes(x = returns_5yr, y = returns_3yr, color = category)) +
  geom_point(alpha = 0.65, size = 2) +
  labs(
    title = "Relationship between 3-year and 5-year returns",
    x = "5-year return (%)",
    y = "3-year return (%)",
    color = "Category"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "outputs/figures/03_return_relationship.png",
  p3, width = 9, height = 6, dpi = 180
)

# Chart 4: Top 10 funds
p4 <- primary_ranking %>%
  slice_head(n = 10) %>%
  mutate(scheme_name = fct_reorder(scheme_name, investor_score)) %>%
  ggplot(aes(x = investor_score, y = scheme_name, fill = category)) +
  geom_col() +
  labs(
    title = "Top 10 funds by composite investor score",
    x = "Investor score (0-100)",
    y = "Fund"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "outputs/figures/04_top_10_funds.png",
  p4, width = 10, height = 6, dpi = 180
)

# Chart 5: Median returns by category
category_medians <- category_metrics %>%
  select(category, median_1yr_return, median_3yr_return, median_5yr_return) %>%
  pivot_longer(
    cols = starts_with("median_"),
    names_to = "horizon",
    values_to = "median_return"
  )

p5 <- ggplot(category_medians, aes(x = category, y = median_return, fill = horizon)) +
  geom_col(position = "dodge") +
  labs(
    title = "Median returns by category",
    x = "Category",
    y = "Median return (%)",
    fill = "Return horizon"
  ) +
  plot_theme

ggsave(
  "outputs/figures/05_category_median_returns.png",
  p5, width = 10, height = 6, dpi = 180
)

# Display all plots in the RStudio Plots pane as well as saving them as PNGs.
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)

# ---------------------------
# 8. Correlation analysis
# ---------------------------
correlation_data <- mf %>%
  select(rating, returns_1yr, returns_3yr, returns_5yr)

correlation_matrix <- cor(
  correlation_data,
  use = "pairwise.complete.obs"
)

write.csv(
  correlation_matrix,
  "outputs/tables/correlation_matrix.csv"
)

cat("\n================ CORRELATION MATRIX ================\n")
print(round(correlation_matrix, 3))

# ---------------------------
# 9. Final message
# ---------------------------
cat("\nAnalysis completed successfully.\n")
cat("Check the outputs/tables folder for CSV results.\n")
cat("Check the outputs/figures folder for PNG charts.\n")
cat("Main ranking file: outputs/tables/ranked_funds_complete_history.csv\n")
