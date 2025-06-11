# MRDC 911 Assignment 1: Exploratory Data Analysis & Preprocessing

##  Project Overview
This project involves exploratory data analysis (EDA) and data preprocessing on a dataset of university students in Kenya. The objective is to clean, explore, and engineer features that support deeper academic insights and prepare the data for future machine learning tasks.

##  Dataset
- **File**: `kenya_student_data.csv`
- **Size**: 5,000 records, 31 variables
- **Target Variable**: `academic_performance` (Poor, Average, Good, Excellent)

## Tasks Performed
- Identification and treatment of missing values
- Outlier detection and capping using the IQR method
- Feature engineering (binned study hours, income groups, total score)
- Categorical vs. performance analysis
- Correlation and chi-square testing
- Data visualization using `ggplot2` and `corrplot`

## How to Run the Code
1. Open the `student_analysis.R` or `analysis_report.Rmd` file in **RStudio**.
2. Ensure that `kenya_student_data.csv` is in your working directory.
3. Run all chunks sequentially or knit the `.Rmd` file to generate a PDF.
4. The preprocessed dataset will be saved as:  
   `kenya_student_data_preprocessed.csv`

## Key Findings
- Students with higher `study_hours_weekly` and access to the internet tend to perform better academically.
- Academic performance correlates positively with `math_score`, `science_score`, and `english_score`.
- Income level and extracurricular involvement show clear patterns related to performance.
- Outliers and missing values were successfully treated using robust preprocessing methods.

## Files Included
- `kenya_student_data.csv` — Raw dataset
- `Q1.R` — Core script for analysis
- `kenya_student_data_preprocessed.csv` — Cleaned dataset
- `analysis_report.pdf` — Summary of results and plots (knitted from `.Rmd`)
- `README.md` — This file

## Author
Sila Kimeli Ronoh  
MRDC 911 – MSc. Data Science & Computational Intelligence  
June 2025
ADM.NO:25ZAD111181
