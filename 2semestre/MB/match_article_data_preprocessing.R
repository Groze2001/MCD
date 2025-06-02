## Imports
library(rstan)
library(lavaan)
library(blavaan)
library(brms)
library(psych)
library(bayesplot)


# Global CFA
model.cfa <- 'erp_complexity            =~ erpuso2     + erpuso3     + erpuso4
              perceived_usefulness_tech =~ erpresul2   + erpresul3   + erpresul4
              bmi_complexity            =~ bmiarch1    + bmiarch2    + bmiarch3
              bmi_cost                  =~ bmicost1    + bmicost2
              bmi_revenue               =~ bmirevenue1 + bmirevenue2'


# List of dataset filenames
files <- c(
    "./data_mean_imputed.csv",
    "./data_median_imputed.csv",
    "./data_pairwise_imputed.csv",
    "./data_row_deleted.csv"
)

# Function to apply the CFA model
apply_model <- function(file) {
    cat("\n===== Processing:", file, "=====\n")

    # Load the dataset
    data <- read.csv(file)

    # Fit the CFA model
    modelfit.cfa <- bcfa(
        model.cfa,
        data = data,
        std.lv = TRUE,
        n.chains = 3,
        burnin = 5000,
        sample = 1000,
        target = "stan"
    )

    # Standardized posterior
    std_all <- standardizedposterior(modelfit.cfa)

    # Posterior summary
    print(posterior_summary(std_all[,1:13]))
}

# Apply the function to each dataset
lapply(files, apply_model)

