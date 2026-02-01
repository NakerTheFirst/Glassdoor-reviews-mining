# Data inspection & validation

# Setup
install.packages("pacman")
pacman::p_load(
  here,
  readr,
  tidyverse,
  skimr,
  tidytext,
  textstem,
  text2vec,
  Matrix,
  reshape2,
  slam,
  cluster,
  factoextra,
  fpc,
  mclust
)

source("R/utils.R")
theme_update(plot.title = element_text(hjust = 0.5))

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

cat("\n--- Data Validation Sumamry ---\n\n")
cat("Dimensions:\n")
cat(sprintf("  Rows: %s\n", format(nrow(reviews_raw), big.mark = ",")))
cat(sprintf("  Columns: %d\n\n", ncol(reviews_raw)))

cat("\nData range:\n")
cat(sprintf("  From: %s\n", min(reviews_raw$date_review, na.rm = TRUE)))
cat(sprintf("  To: %s\n\n", max(reviews_raw$date_review, na.rm = TRUE)))

cat("Unique counts:\n")
cat(sprintf("  Firms: %d\n", n_distinct(reviews_raw$firm)))
cat(sprintf("  Job titles: %d\n", n_distinct(reviews_raw$job_title)))
cat(sprintf("  Locations: %d\n\n", n_distinct(reviews_raw$location)))

# ---Data Preprocessing---

# Subset to 2020 only
reviews_2020 <- reviews_raw |>
  filter(
    date_review >= as.Date("2020-01-01"),
    date_review < as.Date("2021-01-01")
  ) |>
  # Drop diversity_inclusion (74% NA)
  select(-diversity_inclusion) |>
  # Drop rows with NA in ratings or text
  drop_na(all_of(rating_cols), headline, pros, cons, location, job_title) |>
  # Drop short reviews
  filter(nchar(pros) >= 20, nchar(cons) >= 20) |>
  # Shuffle
  slice_sample(prop = 1) |>
  # Add primary key
  mutate(id = row_number(), .before = 1)

cols <- colnames(reviews_2020)

# Text preprocessing for free-text columns
reviews_2020 <- reviews_2020 |>
  mutate(
    across(c(pros, cons, headline), ~ .x |>
        str_remove_all("[\r\n\t]") |>
        str_to_lower() |>
        str_remove_all("[[:punct:]]") |>
        str_remove_all("[0-9]") |>
        str_squish()
    )
  )

# Light cleaning for categorical columns
reviews_2020 <- reviews_2020 |>
  mutate(
    across(c(firm, job_title, current, location), ~ .x |>
        str_squish()
    )
  )

# Verify
cat(sprintf(
  "Clean dataset: %s rows (%.1f%% of original)\n",
  format(nrow(reviews_2020), big.mark = ","),
  100 * nrow(reviews_2020) / nrow(reviews_raw)
))

glimpse(reviews_2020)

# Detailed summary with skimr
reviews_2020 |>
  select(all_of(cols)) |>
  skim()

# Check if ratings are on expected 1-5 scale
rating_value_counts <- reviews_2020 |>
  select(all_of(rating_cols)) |>
  pivot_longer(everything(), names_to = "rating_type", values_to = "value") |>
  count(rating_type, value) |>
  arrange(rating_type, value)
print(rating_value_counts, n = 30)

# Visualisations
reviews_2020 |>
  select(all_of(rating_cols)) |>
  pivot_longer(everything(), names_to = "rating_type", values_to = "value") |>
  count(rating_type, value) |>
  group_by(rating_type) |>
  mutate(percentage = n / sum(n)) |>
  ggplot(aes(x = value, y = percentage)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~rating_type) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Percentage") +
  theme_minimal()

# Encode sentiment columns (recommend, ceo_approv, outlook)
reviews_2020 <- reviews_2020 |>
  mutate(
    # Binary: did they express an opinion?
    recommend_has_opinion = recommend != "o",
    ceo_approv_has_opinion = ceo_approv != "o",
    outlook_has_opinion = outlook != "o",

    # Ordinal scores (NA if no opinion)
    recommend_score = encode_sentiment(recommend),
    ceo_approv_score = encode_sentiment(ceo_approv),
    outlook_score = encode_sentiment(outlook)
  )

glimpse(reviews_2020)

# Add character count columns for text fields
reviews_2020 <- reviews_2020 |>
  mutate(
    headline_length = nchar(headline),
    pros_length = nchar(pros),
    cons_length = nchar(cons)
  )

# Pros and cons lengths together
reviews_2020 |>
  select(pros_length, cons_length) |>
  pivot_longer(everything(), names_to = "field", values_to = "length") |>
  filter(length > 0) |>
  ggplot(aes(x = length)) +
  geom_histogram(bins = 50, fill = "steelblue") +
  facet_wrap(~field, scales = "free_y") +
  scale_x_log10() +
  labs(
    title = "Pros & Cons Length Distributions",
    x = "Character Count (log scale)",
    y = "Count"
  ) +
  theme_minimal()

# Headline length
reviews_2020 |>
  ggplot(aes(x = headline_length)) +
  geom_histogram(bins = 50, fill = "steelblue") +
  labs(
    title = "Headline Length Distribution",
    x = "Character Count",
    y = "Count"
  ) +
  theme_minimal()

# Correlation between ratings
reviews_2020 |>
  select(all_of(rating_cols)) |>
  cor(use = "pairwise.complete.obs") |>
  round(2)

glimpse(reviews_2020)

# Z-score outlier detection for text lengths
reviews_2020 <- reviews_2020 |>
  mutate(
    headline_zscore = (headline_length - mean(headline_length)) / sd(headline_length), # nolint: line_length_linter.
    pros_zscore = (pros_length - mean(pros_length)) / sd(pros_length),
    cons_zscore = (cons_length - mean(cons_length)) / sd(cons_length)
  )

glimpse(reviews_2020)
# Count before dropping
n_before <- nrow(reviews_2020)

# Drop outliers (|z| > 3)
reviews_2020 <- reviews_2020 |>
  filter(
    abs(headline_zscore) <= 3,
    abs(pros_zscore) <= 3,
    abs(cons_zscore) <= 3
  ) |>
  select(-ends_with("_zscore"))

# Report
cat(sprintf(
  "Dropped %d outliers (%.2f%%). Remaining: %s rows\n",
  n_before - nrow(reviews_2020),
  100 * (n_before - nrow(reviews_2020)) / n_before,
  format(nrow(reviews_2020), big.mark = ",")
))

# Reindex id column after dropping rows
reviews_2020 <- reviews_2020 |>
  mutate(id = row_number())

# Discretise rating columns for association rules later
reviews_2020 <- reviews_2020 |>
  mutate(
    rating_level = case_when(
      overall_rating <= 2 ~ "low",
      overall_rating == 3 ~ "medium",
      overall_rating >= 4 ~ "high"
    ),
    recommend_binary = ifelse(recommend == "v", "yes", "no")
  )

# Tokenize and combine all text fields
combined_tokens <- bind_rows(
  reviews_2020 |> select(id, text = pros) |> mutate(source = "pros"),
  reviews_2020 |> select(id, text = cons) |> mutate(source = "cons"),
  reviews_2020 |> select(id, text = headline) |> mutate(source = "headline")
) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word") |>
  mutate(word_lemma  = lemmatize_words(word))

# Verify
combined_tokens |> count(source)
combined_tokens |> head(20)

# Save tokens and reviews_2020 data
write_csv(combined_tokens, here("data", "processed", "reviews-2020-tokens.csv"))
write_csv(reviews_2020, here("data", "processed", "reviews-2020-clean.csv"))

# Combine tokens per document (treating each review as one document)
doc_tokens <- combined_tokens |>
  group_by(id) |>
  summarise(tokens = list(word_lemma)) |>
  pull(tokens, name = id)

# Create vocabulary
vocab_iterator <- itoken(doc_tokens, progressbar = FALSE)
vocabulary <- create_vocabulary(vocab_iterator)

# Inspect vocabulary size
cat(sprintf("Vocabulary size: %d terms\n", nrow(vocabulary)))

# Create document-term matrix
vectoriser <- vocab_vectorizer(vocabulary)
dtm <- create_dtm(vocab_iterator, vectoriser)

cat(sprintf("DTM dimensions: %d documents x %d terms\n", nrow(dtm), ncol(dtm)))

# Apply TF-IDF weighting
tfidf_model <- TfIdf$new()
tfidf_matrix <- fit_transform(dtm, tfidf_model)

cat(sprintf("TF-IDF matrix: %d × %d (sparse)\n", nrow(tfidf_matrix), ncol(tfidf_matrix))) # nolint
cat(sprintf("Sparsity: %.2f%%\n", 100 * (1 - nnzero(tfidf_matrix) / length(tfidf_matrix)))) # nolint

# Check a sample of terms with highest TF-IDF values
sample_doc <- tfidf_matrix[1, ]
top_terms <- sort(sample_doc, decreasing = TRUE)[1:10]
print(top_terms)

# LDA using text2vec
k <- 10
lda_model <- text2vec::LDA$new(n_topics = k, doc_topic_prior = 0.1, topic_word_prior = 0.01) # nolint
doc_topics <- lda_model$fit_transform(dtm, n_iter = 1000, convergence_tol = 0.001,  # nolint
                                       n_check_convergence = 25, progressbar = TRUE) # nolint

cat(sprintf("Doc-topic matrix: %d × %d\n", nrow(doc_topics), ncol(doc_topics)))

# Top terms per topic
top_terms <- lda_model$get_top_words(n = 10, lambda = 1)
print(top_terms)

# Assign dominant topic per document
dominant_topic <- apply(doc_topics, 1, which.max)
table(dominant_topic)

length(dominant_topic)

# Create a data frame with topic assignments keyed by id
topic_assignments <- tibble(
  id = as.integer(rownames(doc_topics)),
  dominant_topic = apply(doc_topics, 1, which.max),
  topic_max_prob = apply(doc_topics, 1, max)
)

# Join back to reviews
reviews_2020 <- reviews_2020 |>
  inner_join(topic_assignments, by = "id")

# Save
saveRDS(lda_model, here("models", "lda_model.rds"))
saveRDS(doc_topics, here("data", "processed", "doc_topics.rds"))

sample_idx <- sample(nrow(cluster_data), size = 3000)
cluster_sample <- cluster_data[sample_idx, ]

# Optimal k on sample
fviz_nbclust(cluster_sample, kmeans, method = "silhouette", k.max = 20) +
  labs(title = "Optimal k (Silhouette) - Sampled")

fviz_nbclust(cluster_sample, kmeans, method = "wss", k.max = 20) +
  labs(title = "Elbow Method - Sampled")

# Apply K-Means to FULL data (this is fine — kmeans scales well)
k_chosen <- 11
km_model <- kmeans(cluster_data, centers = k_chosen, nstart = 25, iter.max = 100) # nolint

# Silhouette on sample only (full is too expensive)
sil <- silhouette(km_model$cluster[sample_idx], dist(cluster_sample))
cat(sprintf("K-Means silhouette (sampled): %.3f\n", mean(sil[, 3])))

# Hierarchical on sample only
hc_model <- hclust(dist(cluster_sample), method = "ward.D2")
hc_clusters_sample <- cutree(hc_model, k = k_chosen)

# Compare on sample
mclust::adjustedRandIndex(km_model$cluster[sample_idx], hc_clusters_sample)

# Continue with full data for the rest...
reviews_2020 <- reviews_2020 |>
  mutate(cluster_kmeans = km_model$cluster)

# Cluster vs rating/recommend
reviews_2020 |>
  group_by(cluster_kmeans) |>
  summarise(
    n = n(),
    mean_rating = mean(overall_rating),
    pct_recommend = mean(recommend == "v") * 100
  )

# Save
write_csv(reviews_2020, here("data", "processed", "reviews-2020-clustered.csv"))
