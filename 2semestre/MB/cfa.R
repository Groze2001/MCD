## Imports
library(rstan)
library(lavaan)
library(blavaan)
library(brms)
library(psych)
library(bayesplot)

# Data Loading
data <- read.csv("./project/data.csv")
dim(data)
summary(data)


# Stan lib config
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# CFA
model.cfa <- 'erp_complexity            =~ erpuso2     + erpuso3     + erpuso4
              perceived_usefulness_tech =~ erpresul2   + erpresul3   + erpresul4
              bmi_complexity            =~ bmiarch1    + bmiarch2    + bmiarch3
              bmi_cost                  =~ bmicost1    + bmicost2
              bmi_revenue               =~ bmirevenue1 + bmirevenue2'

modelfit.cfa <-  bcfa(
    model.cfa,        # CFA model
    data=data,        # dataset
    std.lv=T,         # Standardize latent variables
    n.chains = 3,     # Number of chains
    burnin=5000,      # Burn-in period (samples discarded initially)
    sample=1000,      # Number of samples to draw after the burn-in phase
    target = "stan"   # Sampling method
)

summary(
    modelfit.cfa,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)


fitMeasures(modelfit.cfa)

std_all <- standardizedposterior(modelfit.cfa)
head(std_all)


posterior_summary(std_all[,7:12])

posterior_summary(1-std_all[,7:12]) ## R2



