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



model.sem <- '
    # measurement model
    erp_complexity            =~ erpuso2     + erpuso3     + erpuso4
    perceived_usefulness_tech =~ erpresul2   + erpresul3   + erpresul4
    bmi_complexity            =~ bmiarch1    + bmiarch2    + bmiarch3
    bmi_cost                  =~ bmicost1    + bmicost2
    bmi_revenue               =~ bmirevenue1 + bmirevenue2

    # Residual correlations
    erpresul3 ~~ erpresul4
    erpresul3 ~~ erpresul2

    # regressions
    perceived_usefulness_tech ~ erp_complexity
    bmi_complexity            ~ erp_complexity + perceived_usefulness_tech
    bmi_cost                  ~ bmi_complexity
    bmi_revenue               ~ bmi_complexity + bmi_cost
'


modelfit.sem <- bcfa(
    model.sem, # SEM model
    data=data, # dataset
    std.lv=T, # Standardize latent variables
    n.chains = 4, # Number of chains
    burnin=10000, # Burn-in period (samples discarded initially)
    sample=2000, # Number of samples to draw after the burn-in phase
    target = "stan" # Sampling method
)

# Summary of the SEM model
summary(modelfit.sem, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)


################################################################################


model.sem_refined <- '
    # measurement model
    erp_complexity            =~ erpuso2     + erpuso4
    perceived_usefulness_tech =~ erpresul2   + erpresul3   + erpresul4
    bmi_complexity            =~ bmiarch2    + bmiarch3
    bmi_cost                  =~ bmicost1    + bmicost2
    bmi_revenue               =~ bmirevenue1 + bmirevenue2

    # Residual correlations
    erpresul3 ~~ erpresul4
    erpresul3 ~~ erpresul2

    # regressions
    perceived_usefulness_tech ~ erp_complexity
    bmi_complexity            ~ erp_complexity + perceived_usefulness_tech
    bmi_cost                  ~ bmi_revenue       + bmi_complexity
'


modelfit.sem_refined <- bcfa(
    model.sem_refined, # SEM model
    data=data, # dataset
    std.lv=T, # Standardize latent variables
    n.chains = 4, # Number of chains
    burnin=10000, # Burn-in period (samples discarded initially)
    sample=2000, # Number of samples to draw after the burn-in phase
    target = "stan" # Sampling method
)

# Summary of the SEM model
summary(modelfit.sem_refined, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)

fits_st <- cbind(fitMeasures(modelfit.sem_refined), fitMeasures(modelfit.sem))
round(fits_st, 4)


blavCompare(modelfit.sem_refined, modelfit.sem)


################################################################################

### ADD INFORMATIVE PRIORS

# Parameters for the normal prior
mean <- 0
sd   <- 0.01

# Plot the normal density function -> change to poisson dist
curve(dnorm(x, mean = mean, sd = sd),
      from = -0.1, to = 0.1,
      col = "darkgreen", lwd = 2,
      ylab = "Density", xlab = "x",
      main = paste("Normal Prior (mean =", mean, ", sd =", sd, ")"))

# Set shape parameters
alpha <- 12
beta <- 2

# Plot the Beta distribution
curve(dbeta(x, alpha, beta), from = 0, to = 1,
      main = paste("Beta(", alpha, ",", beta, ") Distribution"),
      xlab = "x", ylab = "Density",
      col = "blue", lwd = 2)

# SEM
model.sem_refined_prior_inf <- '
    # measurement model
    erp_complexity            =~ erpuso2     + erpuso4
    perceived_usefulness_tech =~ erpresul2   + erpresul3   + erpresul4
    bmi_complexity            =~ bmiarch2    + bmiarch3
    bmi_cost                  =~ bmicost1    + bmicost2
    bmi_revenue               =~ bmirevenue1 + bmirevenue2

    # Residual correlations
    erpresul3 ~~ erpresul4
    erpresul3 ~~ erpresul2

    # regressions
    perceived_usefulness_tech ~ erp_complexity
    bmi_complexity            ~ erp_complexity + "normal(0, 0.01)" * perceived_usefulness_tech
    bmi_cost                  ~ bmi_complexity
    bmi_revenue               ~ bmi_complexity + "beta(12, 2)" * bmi_cost
'

modelfit.sem_refined_prior_inf <-  bcfa(
    model.sem_refined_prior_inf,  # CFA model
    data=data,            # dataset
    std.lv=T,             # Standardize latent variables
    n.chains = 3,         # Number of chains
    burnin=5000,          # Burn-in period (samples discarded initially)
    sample=2000,          # Number of samples to draw after the burn-in phase
    target = "stan"       # Sampling method
)

summary(
    modelfit.sem_refined_prior_inf,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)


fitMeasures(modelfit.sem_refined_prior_inf)

std_all <- standardizedposterior(modelfit.sem_refined_prior_inf)

posterior_summary(std_all)

