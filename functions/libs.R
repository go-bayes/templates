
#libraries used
library("dplyr") # data wrangling
library("tidyr") # data wrangling
library("janitor") # clean var names
library("here") # directory management
library("skimr") # data inspection
#library("Amelia") # multiple imputation
library("ggplot2") # graphics
library("purrr") # data analysis
library("patchwork") # graphics
library("kableExtra") #tables
library("parameters") # working with models
library("mice") # multiple imputation
library("table1") # nice tables (html, latex)
library("ggokabeito")   # color palette
library("brms") # bayesian estimation
library("rstan")
library("lme4") # for multilevel model comparisons
library("ggpubr") # themes and also graphics manipulation
#library("MatchThem") # weights for propensity scores with multiple datasets
library("marginaleffects") # what it says
#library("cobalt") # Weights for propensity scores
#library("WeightIt")# Weights for propensity scores
#library("optmatch")# Weights for propensity scores
library("cmdstanr") # Bayesian estimation
library("formula.tools") #
library("CMAverse") # causal mediation
library("splines") # estimation
library("gghighlight") # highlight points
library("parameters") # working with models
library("naniar") # missing data
library("conflicted") # don't have conflicts #
library("Amelia") # multiple imputation
library("miceadds") # convert amelia to mice objects and vice versa
#conflict_prefer("pool", "mice")
#conflict_prefer("filter", "dplyr")
#conflict_prefer("select", "dplyr")
#conflict_prefer("cbind", "base")
#library("speedglm") # not needed currently
library("stdReg") # g-computation !
rstan_options(auto_write = TRUE) # bayesian estimation
options(mc.cores = parallel::detectCores ()) # use all course
theme_set(theme_pubclean()) # nice theme
library(geepack)
