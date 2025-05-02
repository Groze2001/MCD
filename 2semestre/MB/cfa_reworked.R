# Load Required Libraries
suppressMessages({
    library(rstan)
    library(lavaan)
    library(blavaan)
    library(brms)
    library(psych)
    library(bayesplot)
})

# Stan Configuration
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load Data
data_path <- "./project/data.csv"
if (!file.exists(data_path)) stop("Data file not found.")
data <- read.csv(data_path)
print(dim(data))
print(summary(data))

# Define CFA Models
cfa_models <- list(
    erp_complexity            = 'erp_complexity            =~ erpuso2     + erpuso3     + erpuso4',
    perceived_usefulness_tech = 'perceived_usefulness_tech =~ erpresul2   + erpresul3   + erpresul4',
    bmi_complexity            = 'bmi_complexity            =~ bmiarch1    + bmiarch2    + bmiarch3',
    bmi_cost                  = 'bmi_cost                  =~ bmicost1    + bmicost2',
    bmi_revenue               = 'bmi_revenue               =~ bmirevenue1 + bmirevenue2'
)

# Helper Function: Fit and summarize a CFA model
fit_and_summarize_cfa <- function(model_name, model_string, data, summary_cols=1:6) {
    cat("\n====================", toupper(model_name), "====================\n")
    fit <- tryCatch({
        bcfa(
            model_string,
            data = data,
            std.lv = TRUE,
            n.chains = 3,
            burnin = 5000,
            sample = 1000,
            target = "stan"
        )
    }, error = function(e) {
        warning(paste("Model fitting failed for", model_name, ":", e$message))
        return(NULL)
    })

    if (!is.null(fit)) {
        summary_out <- summary(fit, standardized = TRUE, rsquare = TRUE, neff = TRUE, postmedian = TRUE)
        print(summary_out)

        fit_measures <- fitMeasures(fit)
        print(fit_measures)

        std_post <- standardizedposterior(fit)
        posterior <- posterior_summary(std_post[, summary_cols, drop=FALSE])
        print(posterior)
    }

    return(invisible(fit))
}

# Fit and summarize all local models
fitted_models <- list()
for (model_name in names(cfa_models)) {
    cols_to_summarize <- if (model_name %in% c("bmi_cost", "bmi_revenue")) 1:4 else 1:6
    fitted_models[[model_name]] <- fit_and_summarize_cfa(model_name, cfa_models[[model_name]], data, cols_to_summarize)
}

# Global CFA Model
model.global_cfa <- '
  erp_complexity            =~ erpuso2     + erpuso3     + erpuso4
  perceived_usefulness_tech =~ erpresul2   + erpresul3   + erpresul4
  bmi_complexity            =~ bmiarch1    + bmiarch2    + bmiarch3
  bmi_cost                  =~ bmicost1    + bmicost2
  bmi_revenue               =~ bmirevenue1 + bmirevenue2
'

cat("\n==================== GLOBAL CFA ====================\n")

modelfit.global_cfa <- bcfa(
    model.global_cfa,
    data = data,
    std.lv = TRUE,
    n.chains = 3,
    burnin = 5000,
    sample = 1000,
    target = "stan"
)

summary_global <- summary(modelfit.global_cfa, standardized = TRUE, rsquare = TRUE, neff = TRUE, postmedian = TRUE)
print(summary_global)

fit_measures_global <- fitMeasures(modelfit.global_cfa)
print(fit_measures_global)

std_post_global <- standardizedposterior(modelfit.global_cfa)
print(head(std_post_global))
print(posterior_summary(std_post_global[, 1:13]))
print(posterior_summary(std_post_global[, 32:41]))
