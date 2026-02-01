# Data inspection & validation

# Setup
install.packages("pacman")
pacman::p_load(here, readr, tidyverse, skimr)
set.seed(42)

# Load data
reviews_raw <- read_csv(here("data", "raw", "glassdoor-job-reviews.csv"))
dim(reviews_raw)
glimpse(reviews_raw)

# Missingness summary
reviews_raw |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") |>
  mutate(pct_missing = round(n_missing / nrow(reviews_raw) * 100, 2)) |>
  arrange(desc(n_missing))

cols <- colnames(reviews_raw)
text_cols <- c("headline", "pros", "cons")
rating_cols <- c(
  "overall_rating", "work_life_balance", "culture_values",
  "career_opp", "comp_benefits", "senior_mgmt"
)

reviews_raw |>
  select(all_of(rating_cols)) |>
  skim()

# Text fields inspection
reviews_raw |>
  mutate(
    pros_length = nchar(pros),
    cons_length = nchar(cons),
    headline_length = nchar(headline)
  ) |>
  select(ends_with("_length")) |>
  summary()

# Sample text for manual inspection
reviews_raw |>
  slice_sample(n = 5) |>
  select(headline, pros, cons)

# Short/empty text check
reviews_raw |>
  summarise(
    pros_na = sum(is.na(pros)),
    cons_na = sum(is.na(cons)),
    pros_short = sum(nchar(pros) < 20, na.rm = TRUE),
    cons_short = sum(nchar(cons) < 20, na.rm = TRUE)
  )

cat("\n========== DATA VALIDATION SUMMARY ==========\n\n")

cat("DIMENSIONS:\n")
cat(sprintf("  Rows: %s\n", format(nrow(reviews_raw), big.mark = ",")))
cat(sprintf("  Columns: %d\n\n", ncol(reviews_raw)))

# TODO: add a line on how many data there is

cat("\nDATE RANGE:\n")
cat(sprintf("  From: %s\n", min(reviews_raw$date_review, na.rm = TRUE)))
cat(sprintf("  To: %s\n\n", max(reviews_raw$date_review, na.rm = TRUE)))

cat("UNIQUE COUNTS:\n")
cat(sprintf("  Firms: %d\n", n_distinct(reviews_raw$firm)))
cat(sprintf("  Job titles: %d\n", n_distinct(reviews_raw$job_title)))
cat(sprintf("  Locations: %d\n\n", n_distinct(reviews_raw$location)))
