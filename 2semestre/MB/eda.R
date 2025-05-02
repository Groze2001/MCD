# ================================
# Exploratory Data Analysis (EDA)
# Project: [Your Project Name]
# Dataset: [Dataset Name]
# Date: [Date]
# ================================

# ---- 1. Setup ----
# Install required packages (if not installed)
packages <- c("tidyverse", "skimr", "DataExplorer", "GGally", "corrplot", "janitor")
installed_packages <- rownames(installed.packages())
to_install <- packages[!packages %in% installed_packages]
if(length(to_install)) install.packages(to_install)

# Load libraries
library(tidyverse)
library(skimr)
library(DataExplorer)
library(GGally)
library(corrplot)
library(janitor)

# ---- 2. Load Data ----
# Adjust the path to your dataset
data <- read.csv("./project/data.csv")

# Preview data
glimpse(data)
head(data)

# ---- 3. Clean Data ----
# Check and clean column names
data <- clean_names(data)

# Check for duplicates
data <- data %>% distinct()

# Handle missing values (initial overview)
summary(is.na(data))
colSums(is.na(data))

# Optionally: Remove rows/columns with too many NAs
# data <- data %>% drop_na() # or custom logic

# ---- 4. Univariate Analysis ----
# Summary statistics
summary(data)
skim(data)

# Distribution of numerical variables
data %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything()) %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    facet_wrap(~name, scales = "free")

# # Bar plots for categorical variables
# data %>%
#     select(where(is.character)) %>%
#     pivot_longer(everything()) %>%
#     ggplot(aes(x = value)) +
#     geom_bar(fill = "coral") +
#     facet_wrap(~name, scales = "free") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- 5. Bivariate Analysis ----
# Correlation plot for numeric variables
numeric_data <- data %>% select(where(is.numeric))
corr_matrix <- cor(na.omit(numeric_data))
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8)

# Scatter plot matrix
GGally::ggpairs(numeric_data)

# Boxplots: Numeric vs. Categorical
# Replace 'category_var' with actual variable
# ggplot(data, aes(x = category_var, y = numeric_var)) + geom_boxplot()

# ---- 6. Missing Data Visualization ----
plot_missing(data)
plot_histogram(data) # To understand distributions better

# ---- 7. Outliers Detection ----
# Boxplots for each numeric variable
data %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything()) %>%
    ggplot(aes(x = name, y = value)) +
    geom_boxplot(fill = "lightblue") +
    coord_flip()

# ---- 8. Automated EDA Report (Optional) ----
create_report(data, output_file = "eda_report.html")

# ---- 9. Save Cleaned Data (Optional) ----
write_csv(data, "project/cleaned_data.csv")

# ================================
# End of EDA Script
# ================================
