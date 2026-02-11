# Glassdoor Reviews Mining: Topic Discovery, Clustering & Association Rules
An unsupervised learning analysis of ~77,000 Glassdoor employee reviews to identify what drives negative workplace experiences and actionable patterns for HR teams.

📄 [View Full Report on RPubs](https://rpubs.com/NakerTheFirst/Glassdoor-reviews-mining) | 🔗 [Dataset on Kaggle](https://www.kaggle.com/datasets/davidgauthier/glassdoor-job-reviews)

![t-SNE Visualization of Review Clusters](https://github.com/NakerTheFirst/Glassdoor-reviews-mining/blob/main/outputs/figures/tsne_clusters.png?raw=true)

## Business Question
What drives negative employee reviews on Glassdoor, and what actionable patterns can HR teams use to improve employee satisfaction?

## Data

| Attribute | Value |
|-----------|-------|
| Source | Kaggle Glassdoor Job Reviews |
| Time Period | 2020 (subset) |
| Raw Size | 700,000+ reviews |
| After Cleaning | 77,397 reviews |
| Features Used | `pros`, `cons`, `headline` (text); `overall_rating`, `recommend`, sub-ratings |

**Preprocessing:**
- Text cleaning (lowercase, punctuation, numbers, whitespace removal)
- Stopword removal + lemmatization
- Z-score outlier removal (|z| > 3 on text length)
- TF-IDF vectorization → 10,823 vocabulary terms

## Methods

### Dimensionality Reduction
- **LDA (Latent Dirichlet Allocation):** 10 topics extracted from TF-IDF matrix
- Reduces 10,000+ word dimensions → 10 interpretable topic probabilities per document

### Clustering
- **Spherical K-Means** (cosine distance) - k=11 clusters
- **Hierarchical Clustering** (Ward's method) - for validation
- Optimal k selected via silhouette analysis
- Adjusted Rand Index = 0.63 (substantial agreement between methods)

### Association Rules
- **Apriori algorithm** (support ≥ 0.01, confidence ≥ 0.30)
- Transactions: top topics + discretized ratings + recommend status
- 287 rules generated, filtered for negative outcome predictors

### Visualization
- **t-SNE** for 2D cluster visualization

## Findings

### 1. Management Quality is the #1 Predictor of Negative Outcomes

| Rule | Confidence | Lift |
|------|------------|------|
| {recommend_no, topic_8} → {rating_low} | 60.3% | 4.72 |
| {recommend_no, topic_3, topic_8} → {rating_low} | 71.5% | 5.60 |
| {rating_low, topic_8} → {recommend_no} | 96.8% | 2.92 |

Topic 8 = "management", "manager", "bad", "poor", "staff"

### 2. Problem Cluster Identified

**Cluster 8:** Mean rating 2.67, only 33% would recommend - the most dissatisfied employee segment.

### 3. Topic Labels Discovered

| Topic | Top Words | Label |
|-------|-----------|-------|
| 8 | management, manager, bad, poor | **Management Issues** |
| 6 | leadership, culture, process, change | Leadership & Culture |
| 3 | benefit, time, leave, health | Benefits & Time Off |
| 4 | hour, shift, pay, customer | Hourly/Shift Conditions |

## Business Recommendations

1. **Implement manager feedback loops and leadership training**
   - Topic 8 dominates negative reviews
   - Expected impact: Reduce negative reviews by 15–20%

2. **Audit departments with management + benefits complaints**
   - Topic 8 + Topic 3 co-occurrence strongly predicts negative outcomes
   - Expected impact: Improve retention by 10–15%

3. **Conduct culture assessments in low-rated business units**
   - Topic 6 (leadership/culture) also appears in negative rules
   - Expected impact: Improve "would recommend" rate by 10%

## Project Structure
```
├── analysis/
│   └── main.R                 <- Full analysis pipeline
├── data/
│   ├── raw/                   <- Original Glassdoor dataset
│   └── processed/             <- Cleaned data, tokens, topic distributions
├── models/
│   ├── lda_model.rds          <- Fitted LDA model
│   └── association_rules.rds  <- Apriori rules object
├── outputs/
│   └── figures/               <- t-SNE plots, rule visualizations
├── R/
│   └── utils.R                <- Helper functions
├── report/
│   └── final-report.Rmd       <- RPubs report source
└── article.Rmd                <- Main analysis report
```

## Tech Stack

**Language:** R

**Key Packages:**
- `text2vec` - TF-IDF, LDA
- `skmeans` - Spherical K-Means
- `arules` / `arulesViz` - Association rule mining
- `Rtsne` - t-SNE visualization
- `tidytext` / `textstem` - Text preprocessing

## Limitations

- **Glassdoor bias:** Disgruntled employees may be over-represented
- **Correlational only:** No causal claims
- **2020 data only:** COVID-19 effects may influence results
- **Topic coherence:** Interpretation is subjective

## References

- Singh, H. - [Clustering of text documents by implementation of K-means algorithms](https://www.researchgate.net/publication/353807146_Text_Clustering_using_K-MEAN)
- Blei, D., Ng, A., Jordan, M. (2003) - [Latent Dirichlet Allocation](https://dl.acm.org/doi/10.5555/944919.944937)
