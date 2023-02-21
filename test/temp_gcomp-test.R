## GCOMP IMPUTE METHOD Test


options(scipen = 999)

#libraries and functions
# read libraries
#source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
#source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

#conflict_prefer("pool", "mice")
#conflict_prefer("cbind", "base")
# for saving models
push_mods <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/mods")
push_figs <-
  fs::path_expand("~/Users/joseph/The\ Virtues\ Project\ Dropbox/outcomewide/figs")

# read data
pull_path <-
  fs::path_expand(
    "~/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/ldf.5"
  )


