# new R template for gcomp.


# step one: import libraries:
options(scipen = 999)

#libraries and functions
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")


# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# read data
# set your folder path : unique to each user
pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# set path to where you will push data: unique for each user

# for saving models
push_mods <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/templates/mods")

# for saving figures
push_figs <-
  fs::path_expand(" /Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/templates/figs")

pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# note that you need use the arrow package
dat <- arrow::read_parquet(pull_path)

# check data
str(dat)

# count unique individuals
n_unique(dat$Id)

# set your


