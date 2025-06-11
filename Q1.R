# =============================
# MRDC 911 Assignment 1 - EDA & Preprocessing on Kenyan  Student  Dataset
# Student Name: [SILA KIMELI RONOH]
# Date: [11/06/2025]
# =============================
#Load the data set and display its structure (e.g., column names, data types, first few rows). 
#How many numerical and categorical variables are there?
# =============================
library(ggplot2)
library(tidyverse)
library(corrplot)
# ===========================
# Q1: Load the dataset and display structure
# ===========================

# Load required libraries
library(tidyverse)   # For data manipulation and visualization

# Read the dataset
data <- read_csv("kenya_student_data.csv")

# View the structure of the dataset
str(data)

# View a quick preview of the dataset
head(data)

# View column types and summary of types
glimpse(data)

# Count numeric and categorical variables
# Explanation: We use sapply() to check the class of each column, then count types
var_classes <- sapply(data, class)

# Count how many are numeric (double or integer)
num_vars <- sum(var_classes == "numeric" | var_classes == "double" | var_classes == "integer")

# Count how many are categorical (character or factor)
cat_vars <- sum(var_classes == "character" | var_classes == "factor")

# Print counts
cat("Number of numerical variables:", num_vars, "\n")
cat("Number of categorical variables:", cat_vars, "\n")


# Explanation:
# The dataset contains 31 variables and 5000 rows.
# There are 15 numeric variables (like age, income, study hours)
# and 16 categorical variables (like gender, residency, and performance).
# This helps us know how to handle each variable in analysis and preprocessing.
# ------------------------------
#Q2:# Compute summary statistics (mean, median, min, max, etc.) for all numerical variables (e.g., family_income, study_hours_weekly).What insights do these provide about the data? 
# =============================
# Load necessary library
library(tidyverse)

# Select only numeric variables
numeric_data <- select(data, where(is.numeric))

# Compute summary statistics for each numeric column
summary(numeric_data)
# ------------------------------
#The summary statistics reveal a wide range of values in key variables. For instance, family_income has extreme values (from negative KES to over 200,000), suggesting potential data entry errors or outliers. Variables like study_hours_weekly and sleep_hours are normally distributed, while commute_time and mobile_money_usage also show negative minimums, which are not realistic and likely represent errors. Missing values are found in family_income, math_score, and attendance_rate, which will need to be addressed during data cleaning.
# ------------------------------
# ===========================
# Boxplots to visualize outliers in key numeric variables
# ===========================

# Boxplot for family_income
ggplot(data, aes(y = family_income)) +
  geom_boxplot(fill = "tomato", outlier.color = "black") +
  labs(title = "Boxplot of Family Income",
       y = "Family Income (KES)") +
  theme_minimal()

# Boxplot for commute_time
ggplot(data, aes(y = commute_time)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red") +
  labs(title = "Boxplot of Commute Time",
       y = "Commute Time (minutes)") +
  theme_minimal()

# Boxplot for math_score
ggplot(data, aes(y = math_score)) +
  geom_boxplot(fill = "palegreen", outlier.color = "darkgreen") +
  labs(title = "Boxplot of Math Score",
       y = "Math Score") +
  theme_minimal()
# ------------------------------
#Q3:Create a bar plot to visualize the distribution of academic_performance.Is the target variable balanced across its classes (Poor, Average, Good, Excellent)? 
# ===========================
ggplot(data, aes(x = academic_performance)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Academic Performance",
       x = "Academic Performance",
       y = "Number of Students") +
  theme_minimal()
library(dplyr)
data %>%
  count(academic_performance) %>%
  mutate(percent = round(n / sum(n) * 100, 1))
# -----------------------------
#variable is well balanced across the four main categories—Poor, Average, Good, and Excellent—with each category having approximately 23% of the total observations. Specifically, each group contains around 1,152–1,153 students, showing almost equal distribution. However, about 7.8% (391 students) have missing values (NA), which will need to be addressed during data cleaning.
# ===========================
#Q4:Visualize the distribution of study_hours_weekly using a histogram.How does it vary between urban and rural students (use a faceted histogram)?
# -----------------------------
# Histogram of study hours (overall)
ggplot(data, aes(x = study_hours_weekly)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Weekly Study Hours (All Students)",
       x = "Study Hours per Week",
       y = "Count") +
  theme_minimal()
# ===========================
ggplot(data, aes(x = study_hours_weekly)) +
  geom_histogram(binwidth = 2, fill = "#443785") +
  facet_wrap(~residency) +
  labs(title = "Study Hours by Residency")
# ===========================
#Urban students tend to have a slightly wider spread of study hours, with more students studying below 10 and above 20 hours while Rural students appear to be more concentrated around the average (12–18 hours), indicating a more consistent study pattern.This suggests potential differences in time availability, access to study environments, or daily schedules
# ===========================
#Q5:Create boxplots of math_score by academic_performance and gender.What patterns do you observe?
# ===========================
ggplot(data, aes(x = academic_performance, y = math_score, fill = gender)) +
  geom_boxplot() +
  labs(title = "Math Score by Academic Performance and Gender")
# ===========================
#The boxplot reveals a positive association between math scores and academic performance—students in the "Excellent" category tend to have higher math scores, while those in the "Poor" category show lower central values and wider variability. The visualization also highlights gender-based trends: across all performance categories, male and female students exhibit overlapping score distributions, though in some categories, one gender may slightly outperform the other. 
# ===========================
#Q6:Compute the proportion of each category in extracurricular_actiivities and faculty.Which categories are most common?
# ===========================
data %>% count(extracurricular_activities) %>% mutate(prop = n / sum(n))
data %>% count(faculty) %>% mutate(prop = n / sum(n))
# ===========================
#The analysis shows that students are distributed across a variety of extracurricular activities, with some participating in both sports and clubs, while a notable portion report no involvement at all. This may reflect varying levels of access or institutional encouragement.
# ===========================
# Q7:Create a correlation matrix for numerical variables (excluding student_id)  and visualize it using heatmap. Which categories are most common?
# ===========================
num_data <- data %>% select(where(is.numeric), -student_id)
corr_matrix <- cor(num_data, use = "complete.obs")
corrplot(corr_matrix, method = "color", type = "lower")
# ===========================
# Select only numeric columns (excluding student_id)
numeric_data <- data %>%
  select(where(is.numeric)) %>%
  select(-student_id)

# Remove rows with missing values for correlation computation
numeric_data_clean <- na.omit(numeric_data)

# Compute correlation matrix
cor_matrix <- cor(numeric_data_clean, use = "complete.obs")

# Visualize correlation matrix using corrplot
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black", # optional: add correlation values
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.7,
         title = "Correlation Matrix of Numeric Variables",
         mar = c(0, 0, 1, 0))
# ===========================
#math_score, science_score, and english_score are positively correlated, indicating that students who perform well in one subject tend to perform well in others. Similarly, previous_grade is moderately correlated with current scores, suggesting some consistency in academic performance over time.
# ===========================
#Q8:
# ===========================
# Remove rows with missing values in the relevant columns
chi_data <- data %>%
  filter(!is.na(internet_access), !is.na(academic_performance))

# Create a contingency table
table_access_perf <- table(chi_data$internet_access, chi_data$academic_performance)

# View the contingency table
print(table_access_perf)

# Perform Chi-Squared Test
chi_test <- chisq.test(table_access_perf)

# View results
print(chi_test)
# =========================
#The Chi-squared test reveals a significant relationship between internet access and academic performance (X² = 163.55, df = 3, p < 0.001). Therefore, internet access is not independent of academic outcomes, highlighting the potential impact of digital accessibility on student success.
# =========================
#Q(:9.	Identify columns with missing values and report their percentages. Why might these variables have missing data in a Kenyan context?)
# =========================
# Calculate number and percentage of missing values for each column
missing_summary <- data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_percent = round((missing_count / nrow(data)) * 100, 2)) %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_percent))

# View summary
print(missing_summary)
# =========================
#The variable with the highest missing umbers is academic_performance, the target variable, with 7.8% of entries missing. This could affect supervised learning models, which require complete labels. family_income and attendance_rate each have 5% missing, which may affect feature-based analysis and need to be imputed carefully. math_score has the fewest missing values at 3%, making it easier to handle with median or mean imputation.
# =========================
#Q10:10.	Impute missing values in family_income and math_score using the median. Justify why the median is appropriate for these variables.
# =========================
data$family_income[is.na(data$family_income)] <- median(data$family_income, na.rm = TRUE)
data$math_score[is.na(data$math_score)] <- median(data$math_score, na.rm = TRUE)
data$family_income[is.na(data$family_income)] <- median(data$family_income, na.rm = TRUE)
data$math_score[is.na(data$math_score)] <- median(data$math_score, na.rm = TRUE)
data$attendance_rate[is.na(data$attendance_rate)] <- mean(data$attendance_rate, na.rm = TRUE)
# =========================
#To preserve data and avoid dropping rows, missing values were imputed using central tendency measures:
  
  #family_income and math_score were imputed with the median, which is more robust to outliers.

#attendance_rate, being a normalized variable (0–1 scale), was imputed using the mean.

#This approach ensures that the dataset remains complete while minimizing distortion due to extreme values.
# =========================
#Q11:11.	Impute missing values in attendance_rate using the mean. Compare the distributions before and after imputation using histograms.
# =========================
# Backup original data before imputation
original_data <- data

# Create a version with missing attendance_rate
before_impute <- original_data %>% 
  filter(!is.na(attendance_rate)) %>%
  mutate(stage = "Before Imputation")

# After imputation was done in Q10, so we tag the full dataset as "After"
after_impute <- data %>%
  mutate(stage = "After Imputation")

# Combine the two for plotting
combined <- bind_rows(before_impute, after_impute)

# Plot histograms side-by-side
ggplot(combined, aes(x = attendance_rate, fill = stage)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~stage) +
  labs(title = "Distribution of Attendance Rate Before and After Imputation",
       x = "Attendance Rate",
       y = "Number of Students") +
  theme_minimal()
# =========================
#The histogram comparison shows that the overall shape of the attendance_rate distribution is preserved after imputation. By using the mean to fill in missing values, we avoided skewing the distribution significantly. The imputed values filled small gaps without altering the underlying trend, making this a safe and effective strategy for this continuous variable.
# =========================
#Q12:Detect outliers in family_income using the IQR method. How many outliers are there, and what might they represent in a Kenyan context?
# =========================
iqr <- IQR(data$family_income, na.rm = TRUE)
lower <- quantile(data$family_income, 0.25, na.rm = TRUE) - 1.5 * iqr
upper <- quantile(data$family_income, 0.75, na.rm = TRUE) + 1.5 * iqr
sum(data$family_income < lower | data$family_income > upper)
# =========================
#The IQR method identified outliers in all three variables. These are data points that lie beyond 1.5 times the interquartile range above or below the 25th and 75th percentiles. For example:
#family_income had a large number of extreme values, including some that were negative, which are illogical and likely data entry errors.

#commute_time included negative values, which are not feasible and should be flagged for correction.

#math_score showed a few outliers above 100, suggesting possible bonus points or data inconsistencies.

# =========================
#Q13:Cap outliers in family_income at the 1.5*IQR bounds. Visualize the distribution before and after capping using boxplots.
# =========================
# Function to cap outliers using the IQR method
cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

# Apply capping to the selected variables
data$family_income <- cap_outliers(data$family_income)
data$commute_time <- cap_outliers(data$commute_time)
data$math_score <- cap_outliers(data$math_score)
# =========================
#Outliers in family_income, commute_time, and math_score were capped using the IQR method. This technique limits extreme values by replacing those beyond 1.5×IQR with the nearest acceptable boundary.
# =========================
#Q14.	Discretize study_hours_weekly into four bins (e.g., Low, Moderate, High, Very High). Create a bar plot of the binned variable.
# =========================
# Create binned version of study hours
data$study_hours_binned <- cut(
  data$study_hours_weekly,
  breaks = c(-Inf, 10, 15, 20, Inf),
  labels = c("Low", "Moderate", "High", "Very High"),
  right = FALSE
)

# View the count in each bin
table(data$study_hours_binned)

# Plot the distribution of binned study hours
library(ggplot2)

ggplot(data, aes(x = study_hours_binned)) +
  geom_bar(fill = "mediumseagreen") +
  labs(title = "Binned Study Hours per Week",
       x = "Study Hours Category",
       y = "Number of Students") +
  theme_minimal()
# =========================
#Q15.	Discretize family_income into quartiles (Low, Medium-Low, Medium-High, High). How does the binned variable correlate with academic_performance?
# =========================
# Bin family_income into 4 quartile-based categories
data$family_income_binned <- cut(
  data$family_income,
  breaks = quantile(data$family_income, probs = seq(0, 1, 0.25), na.rm = TRUE),
  labels = c("Low", "Lower-Mid", "Upper-Mid", "High"),
  include.lowest = TRUE
)

# View the relationship using a contingency table
table(data$family_income_binned, data$academic_performance)
ggplot(data, aes(x = family_income_binned, fill = academic_performance)) +
  geom_bar(position = "fill") +
  labs(title = "Academic Performance by Family Income Bracket",
       x = "Family Income Bracket",
       y = "Proportion of Students") +
  theme_minimal()
# =========================
#Q16.	Create a new feature total_score by averaging math_score, science_score, and english_score. Visualize its distribution.
# =========================
# Create total_score as the average of the three subject scores
data$total_score <- rowMeans(
  data[, c("math_score", "science_score", "english_score")],
  na.rm = TRUE
)

# Quick summary
summary(data$total_score)

# Histogram of total_score
ggplot(data, aes(x = total_score)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Total Academic Score",
       x = "Total Score",
       y = "Number of Students") +
  theme_minimal()
# =========================
#Q17:Create a contingency table for extracurricular_activities vs. academic_performance. What patterns suggest about student involvement?
# =========================
# Create a contingency table
table(data$extracurricular_activities, data$academic_performance)

# Visualize using a grouped bar plot
ggplot(data, aes(x = extracurricular_activities, fill = academic_performance)) +
  geom_bar(position = "fill") +
  labs(title = "Academic Performance by Extracurricular Activities",
       x = "Extracurricular Activities",
       y = "Proportion of Students") +
  theme_minimal()
# =========================
#The contingency table and bar plot reveal that students involved in both sports and clubs tend to have higher proportions of Good and Excellent academic performance. In contrast, students with no extracurricular participation show a higher proportion of Poor performance, suggesting a possible positive association between well-rounded engagement and academic outcomes.
# =========================
# Scatter plot of study hours vs total score, colored by residency
ggplot(data, aes(x = study_hours_weekly, y = total_score, color = residency)) +
  geom_point(alpha = 0.6) +
  labs(title = "Study Hours vs. Total Score by Residency",
       x = "Weekly Study Hours",
       y = "Total Academic Score") +
  theme_minimal()
# =========================
# Save the cleaned and processed dataset as a CSV file
write_csv(data, "kenya_student_data_preprocessed.csv", row.names = FALSE)
write_csv(data, "kenya_student_data_preprocessed.csv")
