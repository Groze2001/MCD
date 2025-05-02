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

# Local CFA
model.erp_complexity_cfa            <- 'erp_complexity            =~ erpuso2     + erpuso3     + erpuso4'
model.perceived_usefulness_tech_cfa <- 'perceived_usefulness_tech =~ erpresul2   + erpresul3   + erpresul4'
model.bmi_complexity_cfa            <- 'bmi_complexity            =~ bmiarch1    + bmiarch2    + bmiarch3'
model.bmi_cost_cfa                  <- 'bmi_cost                  =~ bmicost1    + bmicost2'
model.bmi_revenue_cfa               <- 'bmi_revenue               =~ bmirevenue1 + bmirevenue2'

modelfit.erp_complexity_cfa <-  bcfa(
    model.erp_complexity_cfa, # CFA model
    data=data,                # dataset
    std.lv=T,                 # Standardize latent variables
    n.chains = 3,             # Number of chains
    burnin=5000,              # Burn-in period (samples discarded initially)
    sample=1000,              # Number of samples to draw after the burn-in phase
    target = "stan"           # Sampling method
)

modelfit.perceived_usefulness_tech_cfa <-  bcfa(
    model.perceived_usefulness_tech_cfa, # CFA model
    data=data,                           # dataset
    std.lv=T,                            # Standardize latent variables
    n.chains = 3,                        # Number of chains
    burnin=5000,                         # Burn-in period (samples discarded initially)
    sample=1000,                         # Number of samples to draw after the burn-in phase
    target = "stan"                      # Sampling method
)

modelfit.bmi_complexity_cfa <-  bcfa(
    model.bmi_complexity_cfa, # CFA model
    data=data,                # dataset
    std.lv=T,                 # Standardize latent variables
    n.chains = 3,             # Number of chains
    burnin=5000,              # Burn-in period (samples discarded initially)
    sample=1000,              # Number of samples to draw after the burn-in phase
    target = "stan"           # Sampling method
)

modelfit.bmi_cost_cfa <-  bcfa(
    model.bmi_cost_cfa,       # CFA model
    data=data,                # dataset
    std.lv=T,                 # Standardize latent variables
    n.chains = 3,             # Number of chains
    burnin=5000,              # Burn-in period (samples discarded initially)
    sample=1000,              # Number of samples to draw after the burn-in phase
    target = "stan"           # Sampling method
)

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
    modelfit.erp_complexity_cfa,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)


fitMeasures(modelfit.erp_complexity_cfa)

std_all <- standardizedposterior(modelfit.erp_complexity_cfa)
posterior_summary(std_all[, 1:6])

###

summary(
    modelfit.perceived_usefulness_tech_cfa,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)


fitMeasures(modelfit.perceived_usefulness_tech_cfa)

std_all <- standardizedposterior(modelfit.perceived_usefulness_tech_cfa)
posterior_summary(std_all[, 1:6])

###

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

###

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

###

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



###################################


model.erp_complexity_cfa_modified <- 'erp_complexity =~ erpuso2  + erpuso4'
model.bmi_complexity_cfa_modified <- 'bmi_complexity =~ bmiarch2 + bmiarch3'

modelfit.erp_complexity_cfa_modified <-  bcfa(
    model.erp_complexity_cfa_modified, # CFA model
    data=data,                # dataset
    std.lv=T,                 # Standardize latent variables
    n.chains = 3,             # Number of chains
    burnin=5000,              # Burn-in period (samples discarded initially)
    sample=1000,              # Number of samples to draw after the burn-in phase
    target = "stan"           # Sampling method
)

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
    modelfit.erp_complexity_cfa_modified,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)

fitMeasures(modelfit.erp_complexity_cfa_modified)


summary(
    modelfit.bmi_complexity_cfa_modified,
    standardized=T,
    rsquare=T,
    neff=TRUE,
    postmedian=T
)

fitMeasures(modelfit.bmi_complexity_cfa_modified)


std_all <- standardizedposterior(modelfit.bmi_revenue_cfa)
posterior_summary(std_all[, 1:4])

std_all <- standardizedposterior(modelfit.bmi_complexity_cfa_modified)
posterior_summary(std_all[, 1:4])


###################################



# Global CFA
model.global_cfa <- 'erp_complexity            =~ erpuso2     + erpuso3     + erpuso4
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


posterior_summary(std_all[,1:13])
posterior_summary(std_all[,32:41])




