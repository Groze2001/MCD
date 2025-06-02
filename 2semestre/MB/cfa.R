## Imports
library(rstan)
library(lavaan)
library(blavaan)
library(brms)
library(psych)
library(bayesplot)

# Data Loading
data <- read.csv("./data_row_deleted.csv")
dim(data)
summary(data)


# Stan lib config
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Local CFA
model.erp_complexity_cfa            <- 'erp_complexity            =~ erpuso2     + erpuso3     + erpuso4'

modelfit.erp_complexity_cfa <-  bcfa(
    model.erp_complexity_cfa, # CFA model
    data=data,                # dataset
    std.lv=T,                 # Standardize latent variables
    n.chains = 3,             # Number of chains
    burnin=5000,              # Burn-in period (samples discarded initially)
    sample=1000,              # Number of samples to draw after the burn-in phase
    target = "stan"           # Sampling method
)

summary(
    modelfit.erp_complexity_cfa,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)

fitMeasures(modelfit.erp_complexity_cfa)

std_all <- standardizedposterior(modelfit.erp_complexity_cfa)
posterior_summary(std_all[, 1:6])


################################################################################



model.perceived_usefulness_tech_cfa <- 'perceived_usefulness_tech =~ erpresul2   + erpresul3   + erpresul4
erpresul3 ~~ erpresul4
erpresul3 ~~ erpresul2'

modelfit.perceived_usefulness_tech_cfa <-  bcfa(
    model.perceived_usefulness_tech_cfa, # CFA model
    data=data,                           # dataset
    std.lv=T,                            # Standardize latent variables
    n.chains = 3,                        # Number of chains
    burnin=5000,                         # Burn-in period (samples discarded initially)
    sample=1000,                         # Number of samples to draw after the burn-in phase
    target = "stan"                      # Sampling method
)

summary(
    modelfit.perceived_usefulness_tech_cfa,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)


fitMeasures(modelfit.perceived_usefulness_tech_cfa)

std_all <- standardizedposterior(modelfit.perceived_usefulness_tech_cfa)
posterior_summary(std_all[, 1:8])


################################################################################


model.bmi_complexity_cfa            <- 'bmi_complexity            =~ bmiarch1    + bmiarch2    + bmiarch3'

modelfit.bmi_complexity_cfa <-  bcfa(
    model.bmi_complexity_cfa, # CFA model
    data=data,                # dataset
    std.lv=T,                 # Standardize latent variables
    n.chains = 3,             # Number of chains
    burnin=5000,              # Burn-in period (samples discarded initially)
    sample=1000,              # Number of samples to draw after the burn-in phase
    target = "stan"           # Sampling method
)

summary(
    modelfit.bmi_complexity_cfa,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)


fitMeasures(modelfit.bmi_complexity_cfa)

std_all <- standardizedposterior(modelfit.bmi_complexity_cfa)
posterior_summary(std_all[, 1:6])


################################################################################

model.bmi_cost_cfa                  <- 'bmi_cost                  =~ bmicost1    + bmicost2'

modelfit.bmi_cost_cfa <-  bcfa(
    model.bmi_cost_cfa,       # CFA model
    data=data,                # dataset
    std.lv=T,                 # Standardize latent variables
    n.chains = 3,             # Number of chains
    burnin=5000,              # Burn-in period (samples discarded initially)
    sample=1000,              # Number of samples to draw after the burn-in phase
    target = "stan"           # Sampling method
)

summary(
    modelfit.bmi_cost_cfa,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)


fitMeasures(modelfit.bmi_cost_cfa)

std_all <- standardizedposterior(modelfit.bmi_cost_cfa)
posterior_summary(std_all[, 1:4])


################################################################################

model.bmi_revenue_cfa               <- 'bmi_revenue               =~ bmirevenue1 + bmirevenue2'

modelfit.bmi_revenue_cfa <-  bcfa(
    model.bmi_revenue_cfa,    # CFA model
    data=data,                # dataset
    std.lv=T,                 # Standardize latent variables
    n.chains = 3,             # Number of chains
    burnin=5000,              # Burn-in period (samples discarded initially)
    sample=1000,              # Number of samples to draw after the burn-in phase
    target = "stan"           # Sampling method
)

summary(
    modelfit.bmi_revenue_cfa,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)

fitMeasures(modelfit.bmi_revenue_cfa)

std_all <- standardizedposterior(modelfit.bmi_revenue_cfa)
posterior_summary(std_all[, 1:4])


################################################################################


model.erp_complexity_cfa_modified <- 'erp_complexity =~ erpuso2  + erpuso4'

modelfit.erp_complexity_cfa_modified <-  bcfa(
    model.erp_complexity_cfa_modified, # CFA model
    data=data,                # dataset
    std.lv=T,                 # Standardize latent variables
    n.chains = 3,             # Number of chains
    burnin=5000,              # Burn-in period (samples discarded initially)
    sample=1000,              # Number of samples to draw after the burn-in phase
    target = "stan"           # Sampling method
)

summary(
    modelfit.erp_complexity_cfa_modified,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)

fitMeasures(modelfit.erp_complexity_cfa_modified)

std_all <- standardizedposterior(model.erp_complexity_cfa_modified)
posterior_summary(std_all[, 1:4])


################################################################################

model.bmi_complexity_cfa_modified <- 'bmi_complexity =~ bmiarch2 + bmiarch3'


modelfit.bmi_complexity_cfa_modified <-  bcfa(
    model.bmi_complexity_cfa_modified, # CFA model
    data=data,                # dataset
    std.lv=T,                 # Standardize latent variables
    n.chains = 3,             # Number of chains
    burnin=5000,              # Burn-in period (samples discarded initially)
    sample=1000,              # Number of samples to draw after the burn-in phase
    target = "stan"           # Sampling method
)


summary(
    modelfit.bmi_complexity_cfa_modified,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)

fitMeasures(modelfit.bmi_complexity_cfa_modified)


std_all <- standardizedposterior(modelfit.bmi_complexity_cfa_modified)
posterior_summary(std_all[, 1:4])


################################################################################



# Global CFA
model.global_cfa <- '# Cross-loadings
                     erp_complexity            =~ erpuso2     + erpuso3     + erpuso4
                     perceived_usefulness_tech =~ erpresul2   + erpresul3   + erpresul4
                     bmi_complexity            =~ bmiarch1    + bmiarch2    + bmiarch3
                     bmi_cost                  =~ bmicost1    + bmicost2
                     bmi_revenue               =~ bmirevenue1 + bmirevenue2
                     # Residual correlations
                     erpresul3 ~~ erpresul4
                     erpresul3 ~~ erpresul2'

modelfit.global_cfa <-  bcfa(
    model.global_cfa,  # CFA model
    data=data,            # dataset
    std.lv=T,             # Standardize latent variables
    n.chains = 3,         # Number of chains
    burnin=5000,          # Burn-in period (samples discarded initially)
    sample=2000,          # Number of samples to draw after the burn-in phase
    target = "stan"       # Sampling method
)

summary(
    modelfit.global_cfa,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)


fitMeasures(modelfit.global_cfa)

std_all <- standardizedposterior(modelfit.global_cfa)

# Cross-loadings
posterior_summary(std_all[,1:13])

# Residual correlations
posterior_summary(std_all[,14:15])


################################################################################
######### add informative prior


# Parameters for the normal prior
mean <- 0
sd   <- 0.01

# Plot the normal density function
curve(dnorm(x, mean = mean, sd = sd),
      from = 0, to = 0.5,
      col = "darkgreen", lwd = 2,
      ylab = "Density", xlab = "x",
      main = paste("Normal Prior (mean =", mean, ", sd =", sd, ")"))




# Global CFA
model.global_cfa <- '# Cross-loadings
                     erp_complexity            =~ erpuso2     + erpuso3     + erpuso4
                     perceived_usefulness_tech =~ erpresul2   + erpresul3   + erpresul4
                     bmi_complexity            =~ bmiarch1    + bmiarch2    + bmiarch3
                     bmi_cost                  =~ bmicost1    + bmicost2
                     bmi_revenue               =~ bmirevenue1 + bmirevenue2
                     perceived_usefulness_tech ~ "normal(0, 0.01)" * erp_complexity
                     # Residual correlations
                     erpresul3 ~~ erpresul4
                     erpresul3 ~~ erpresul2'

modelfit.global_cfa <-  bcfa(
    model.global_cfa,  # CFA model
    data=data,            # dataset
    std.lv=T,             # Standardize latent variables
    n.chains = 3,         # Number of chains
    burnin=5000,          # Burn-in period (samples discarded initially)
    sample=2000,          # Number of samples to draw after the burn-in phase
    target = "stan"       # Sampling method
)

summary(
    modelfit.global_cfa,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)


fitMeasures(modelfit.global_cfa)

std_all <- standardizedposterior(modelfit.global_cfa)
head(std_all)

# Cross-loadings
posterior_summary(std_all[,1:13])

# Residual correlations
posterior_summary(std_all[,14])


