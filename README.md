# R Data Science Project Template
A minimal, opinionated project template for R-based data science analyses. Designed for individual or small-team projects where simplicity matters more than enterprise tooling.

## Project Organisation
```
├── README.md                   <- The top-level README for this project
├── project-name.Rproj          <- RStudio project file; open this to launch the project
│
├── data
│   ├── raw                     <- Original, immutable data files
│   └── processed               <- Cleaned, transformed data ready for analysis
│
├── R                           <- Reusable R functions and utilities
│   └── utils.R                 <- Helper functions sourced by analysis scripts
│
├── analysis                    <- Numbered analysis scripts, run in sequence
│   ├── 01_data-cleaning.R      <- Data import, validation, and cleaning
│   ├── 02_eda.R                <- Exploratory data analysis
│   ├── 03_analysis.R           <- Primary analysis (modelling, clustering, etc.)
│   └── 04_supplementary.R      <- Additional analyses as needed
│
├── outputs                     <- Generated files from analysis scripts
│   ├── figures                 <- Plots and visualisations (.png, .pdf, etc.)
│   └── tables                  <- Summary tables and exported results (.csv, .xlsx)
│
└── report                      <- Final reporting documents
    └── final-report.Rmd        <- R Markdown report compiling results and narrative
```

## Getting Started
### Using the `here` Package

This template relies on the `here` package for relative path management. Combined with the `.Rproj` file, it eliminates the need for `setwd()` calls.
```r
install.packages("here")

# Then in any script:
library(here)
data <- read.csv(here("data", "raw", "your_data.csv"))
```

Always open your project via the `.Rproj` file to ensure paths resolve correctly.

### Adjusting .gitignore

Ensure you adjust the `.gitignore` file according to your project needs. By default, the `/data/` folder is excluded from version control:
```plaintext
# exclude data from source control by default
/data/
```

You may want to track data if it's small and non-sensitive, or ensure it's excluded if it contains personally identifiable information or large files.

### Environment Management (Optional)

For projects requiring strict reproducibility, initialise `renv`:
```r
install.packages("renv")
renv::init()

# After installing packages:
renv::snapshot()
```

For coursework or exploratory analyses, this is typically unnecessary.

## Workflow

1. Place raw data in `data/raw/` — never modify these files directly
2. Run analysis scripts in order (`01_`, `02_`, etc.)
3. Save processed datasets to `data/processed/`
4. Export figures and tables to `outputs/`
5. Compile final report from `report/final-report.Rmd`
