# #libraries used
# library("dplyr") # data wrangling
# library("tidyr") # data wrangling
# library("janitor") # clean var names
# library("here") # directory management
# library("skimr") # data inspection
# #library("Amelia") # multiple imputation
# library("ggplot2") # graphics
# library("purrr") # data analysis
# library("patchwork") # graphics
# library("kableExtra") #tables
# library("parameters") # working with models
# library("mice") # multiple imputation
# library("table1") # nice tables (html, latex)
# library("ggokabeito")   # color palette
# library("brms") # bayesian estimation
# library("rstan")
# library("lme4") # for multilevel model comparisons
# library("ggpubr") # themes and also graphics manipulation
# #library("MatchThem") # weights for propensity scores with multiple datasets
# library("marginaleffects") # what it says
# #library("cobalt") # Weights for propensity scores
# #library("WeightIt")# Weights for propensity scores
# #library("optmatch")# Weights for propensity scores
# library("cmdstanr") # Bayesian estimation
# library("formula.tools") #
# library("CMAverse") # causal mediation
# library("splines") # estimation
# library("gghighlight") # highlight points
# library("parameters") # working with models
# library("naniar") # missing data
# library("conflicted") # don't have conflicts #
# library("Amelia") # multiple imputation
# library("miceadds") # convert amelia to mice objects and vice versa
# library("fs")
# #conflict_prefer("pool", "mice")
# #conflict_prefer("filter", "dplyr")
# #conflict_prefer("select", "dplyr")
# #conflict_prefer("cbind", "base")
# #library("speedglm") # not needed currently
# library("stdReg") # g-computation !
# rstan_options(auto_write = TRUE) # bayesian estimation
# options(mc.cores = parallel::detectCores ()) # use all course
# theme_set(theme_pubclean()) # nice theme
# library(geepack)

# function for installing dependencies
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c(
  "tidyverse",
  "remotes",
  "devtools",
  "janitor",
  "fs",
  "here",
  "purrr",
  "ggplot2",
  "stdReg",
  "mice",
  "miceadds",
  "Amelia",
  "conflicted",
  "naniar",
  "skimr",
  "marginaleffects",
  "MatchThem",
  "WeightIt",
  "optmatch",
  "splines",
  "MatchIt",
  "clarify",
  "cobalt",
  "broom",
  "gtsummary",
  # "CMAverse",
  # "gghighlight",
  "formula.tools",
  "ggpubr",
  "lme4",
  #"rstan",
  #"cmdstanr",
  "geepack",
  #"brms",
  "gt",
  "ggdist",
  "ggokabeito",
  "table1",
  "kableExtra",
  "parameters",
  "patchwork",
  "tidyr",
  "katex",
  "ggeffects",
  "lubridate",
  "katex",
  "pdftools",
  "arrow",
  "seqHMM",
  "msm",
  "knitr",
  "clarify",
  "conflicted",
  "parallel",
  "rlang",
  "sandwich",
  "lubridate",
  "glue", # for reports
  "DeclareDesign",  #for simulation and modelling of design
  "fabricatr", # for simulation and modelling of design
  "randomizr", # for simulation and modelling of design
  "estimatr", # for simulation and modelling of design/ robust errors
  "DesignLibrary",
  "future", # multicore processing
  "future.apply",  # multicore processing
  "xgboost", #ML
  "ranger",#ML
  "earth",#ML
  "glmnet",
  "glue",
  "grf",
  "lmtp",
  # the best! time-varying causal identification using superlearner
  "SuperLearner",
  # for lmtp
  "xgboost",
  # for lmtp
  "glmnet",
  "ranger",
  "ggdag",
  "progressr", # progress bars
  "survey", # survey weights, design
  "ipw", # iptweighting -- for simple uses
  "gtsummary"
  # for simulation and modelling of design
  # see: https://declaredesign.org/getting-started.html
)


ipak(packages)



# targeted max liklihood
if (!require(lmtp)) {
  devtools::install_github("nt-williams/lmtp@sl3")
}


if (!require(SuperLearner)) {
  remotes::install_github("ecpolley/SuperLearner")
}

#devtools::install_github("nt-williams/lmtp@sl3")

# outdated
# for missing data imputation and gcomputation on the fly
# if (!require(gFormulaMI)) {
#   devtools::install_github("jwb133/gFormulaMI")
# }

# brms options uncomment if using

# rstan_options(auto_write = TRUE) # bayesian estimation
# options(mc.cores = parallel::detectCores ()) # use all course
# theme_set(theme_pubclean()) # nice theme
# ## settings for BMRS
# rstan_options(auto_write = TRUE) # bayesian estimation
# options(mc.cores = parallel::detectCores ()) # use all course



## user needs to configure cmdstanr, instructions: https://mc-stan.org/cmdstanr/

# if (!require(lazerhawk)) {
#   devtools::install_github('m-clark/lazerhawk')
#   install_cmdstan()
# }


## preferences
conflict_prefer("pool", "mice")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("cbind", "base")
conflict_prefer("lead", "dplyr")
conflict_prefer("lag", "dplyr")

