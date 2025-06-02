# Load the data
data <- read.csv("./data.csv")

# Mean imputation
data_mean_imputed <- data
for (col in names(data_mean_imputed)) {
    if (is.numeric(data_mean_imputed[[col]])) {
        mean_val <- mean(data_mean_imputed[[col]], na.rm = TRUE)
        data_mean_imputed[[col]][is.na(data_mean_imputed[[col]])] <- mean_val
    }
}

# Median imputation
data_median_imputed <- data
for (col in names(data_median_imputed)) {
    if (is.numeric(data_median_imputed[[col]])) {
        median_val <- median(data_median_imputed[[col]], na.rm = TRUE)
        data_median_imputed[[col]][is.na(data_median_imputed[[col]])] <- median_val
    }
}

# Copy data to modify
data_pairwise_imputed <- data

# Loop through numeric columns with missing values
numeric_cols <- names(data)[sapply(data, is.numeric)]
for (col in numeric_cols) {
    missing_idx <- which(is.na(data[[col]]))
    if (length(missing_idx) > 0) {
        # Create a formula using other numeric variables
        predictors <- setdiff(numeric_cols, col)
        formula <- as.formula(paste(col, "~", paste(predictors, collapse = "+")))

        # Fit the model on rows without missing values in the target column
        fit <- lm(formula, data = data, na.action = na.exclude)

        # Predict missing values
        predicted_values <- predict(fit, newdata = data[missing_idx, ])

        # Impute the missing values
        data_pairwise_imputed[[col]][missing_idx] <- predicted_values
    }
}

# Simple row deletion where any column is NA
data_row_deleted <- na.omit(data)


# Save the mean-imputed dataset
write.csv(data_mean_imputed, "./data_mean_imputed.csv", row.names = FALSE)

# Save the median-imputed dataset
write.csv(data_median_imputed, "./data_median_imputed.csv", row.names = FALSE)

# Save the pairwise-imputed dataset
write.csv(data_pairwise_imputed, "./data_pairwise_imputed.csv", row.names = FALSE)

# Save the cleaned dataset
write.csv(data_row_deleted, "./data_row_deleted.csv", row.names = FALSE)
