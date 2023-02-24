#LMTP EXAMPLES

# Example: Church attendance on depression: Static yes no effects
# same but:
# K6 over 5 yes or no
# Target trial for church, among those who start with no Church

dat <- arrow::read_parquet(pull_path)
dp  <- dat %>%
  arrange(Id, Wave) |>
  mutate(time = as.numeric(Wave) - 1) |>
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0),
                SexualOrientation = as.factor(if_else(
                  SexualOrientationL1 == 1,
                  "Heterosexual",
                  if_else(SexualOrientationL1 ==
                            2, "Homosexual", "OtherSexuality")
                ))) %>%
  dplyr::mutate(Gender3 = as.factor(ifelse(
    GendAll == 0,
    "Female",
    if_else(GendAll == 1, "Male", "GenderDiverse")
  ))) |>
  # dplyr::rename(
  #  # My life has a clear sense of purpose.
  #   meaning_purpose = LifeMeaning01,
  #  # I have a good sense of what makes my life meaningful.
  #   meaning_sense = LifeMeaning02,
  #   #…  you feel so depressed that nothing could cheer you up?
  # ) |>
  group_by(Id) |>
  ungroup() |>
  dplyr::filter((
    Wave == 2013  & YearMeasured  == 1  & !is.na(Religion.Church) | # change for target trial
      Wave == 2014 & YearMeasured  == 1 & !is.na(Religion.Church) |
      Wave == 2015 & YearMeasured   == 1 & !is.na(Religion.Church) |
      Wave == 2016 & YearMeasured   == 1 & !is.na(Religion.Church) |
      Wave == 2017 & YearMeasured   == 1 & !is.na(Religion.Church) |
      Wave == 2018 & YearMeasured   == 1 & !is.na(Religion.Church) |
      Wave == 2019
  )
  ) |>
  # Wave == 2020 & YearMeasured  != -1 |
  # Wave == 2021 & YearMeasured  != -1 )) %>% # Eligibility criteria
  #dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
  arrange(Wave, Id) |>
  group_by(Id) %>%
  dplyr::mutate(org =  ifelse(Wave == 2013 &
                                YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold = mean(org, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org1 =  ifelse(Wave == 2014 &
                                 YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold1 = mean(org1, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold1 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2 =  ifelse(Wave == 2015 &
                                 YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold2 = mean(org2, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold2 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org3 =  ifelse(Wave == 2016 &
                                 YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold3 = mean(org3, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold3 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org4 =  ifelse(Wave == 2017 &
                                 YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold4 = mean(org4, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold4 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org5 =  ifelse(Wave == 2018 &
                                 YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold5 = mean(org5, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold5 > 0) %>% # hack to enable repeat of baselin
  ungroup() %>%
  droplevels() |>
  mutate(Time = as.numeric(Wave)) |>
  arrange(Id, Wave)

# Ids
n_unique(dp$Id) #5942


# wrangling to make sure censoring works
dp <- dp %>%
  arrange(Id, Time) %>%
  group_by(Id) %>%
  mutate(Censoring = ifelse(Time == max(Time), 0,
                            ifelse(
                              lead(YearMeasured) == -1 |
                                lead(YearMeasured) == 0,
                              0,
                              ifelse(is.na(lead(kessler_worthless)), 0, 1)
                            )))


# chck
table1::table1( ~ Censoring + KESSLER6sum | Time, data = dp)

# wrangling to make wide data
dp<- data.frame(dp)
dpp <- dp |>
  select(
    Id,
    Time,
    Wave,
    Censoring,
    Religion.Church,
    #remove for now
    #  nn_church,
    YearMeasured,
    Gender3,
    #   Partner,
    EthCat,
    GenCohort,
    #   NZSEI13,
    #    CONSCIENTIOUSNESS,
    #   OPENNESS,
    #  HONESTY_HUMILITY,
    # EXTRAVERSION,
    #  NEUROTICISM,
    # AGREEABLENESS,
    # Edu,
    #  NZDep2013,
    #  Employed,
    # HomeOwner,
    # Pol.Orient,
    # SDO,
    #  RWA,
    #  Rural_GCH2018,
    #  Household.INC,
    #  Parent,
    # Relid,
    # Believe.Spirit,
    # Believe.God,
    #  Spiritual.Identification,
    # SWB.SoC01,
    #  EmotionRegulation1,
    #  EmotionRegulation2,
    #  EmotionRegulation3,
    #  Bodysat,
    #  VENGEFUL.RUMIN,
    # retired,
    # semiretired,
    BornNZ,
    #  kessler_hopeless,
    #   # …  you feel hopeless?
    kessler_worthless)  |>#,
  #   #…  you feel so depressed that nothing could cheer you up?
  #    kessler_restless,
  #   #…  you feel restless or fidgety?
  #    kessler_effort,
  #   #…  you feel that everything was an effort?
  #    kessler_worthless,
  #   #…  you feel worthless?
  #   kessler_nervous,
  #…  you feel nervous?
  # KESSLER6sum,
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x)))|>
  dplyr::arrange(Id, Wave) |>
  dplyr::mutate(a_Church = ifelse(Religion.Church > 4, 5, Religion.Church)) |>
  #   # dplyr::mutate(across(c(pets),
  #   #                      ~ lead(.x, n = 1),
  #   #                      .names = "{col}_lead1"))%>% # make leads
  #   # dplyr::mutate(pets_lost1 = ifelse(pets_lead1 == 0, 1, 0)) |>
  #  dplyr::rename_with(~paste("_y", ., sep = ""),
  #   c(
  #NZSEI13,
  #Household.INC,
  #    Standard.Living,
  #  NZDep.2018,
  #  Employed,
#  Household.INC,

#    Hours.Work,
#    HLTH.Disability,
#    EmotionRegulation1,
#    EmotionRegulation2,
#    EmotionRegulation3,
#    Bodysat,
#   #    VENGEFUL.RUMIN,
#   kessler_hopeless,
#   #   # …  you feel hopeless?
#   kessler_depressed,
#   #   #…  you feel so depressed that nothing could cheer you up?
#   kessler_restless,
#   #   #…  you feel restless or fidgety?
#   kessler_effort,
#   #   #…  you feel that everything was an effort?
#   kessler_worthless,
#   #   #…  you feel worthless?
#   kessler_nervous,
#   #…  you feel nervous?
#   # KESSLER6sum,
#   HLTH.Fatigue,
#   #  Rumination,
#   #  Smoker,
#   #  HLTH.BMI,
#   BELONG,
#   SUPPORT,
#   community,
#   # CharityDonate,
#   # HoursCharity,
#   # GRATITUDE,
#   # Hours.Work,
#   # HLTH.SleepHours,
#   # HLTH.Disability,
#   # Hours.Exercise,
#   #  LIFEMEANING,
#   LIFESAT,
#   # PWI, can reconstruct later
#   # NWI,
#   SFHEALTH,
#   #  SELF.CONTROL,
#   #   SFHEALTH,
#   #  SELF.ESTEEM,
#   #  Respect.Self,
#   #  SELF.ESTEEM,
#   #  SELF.CONTROL,
#   #  Emp.WorkLifeBalance,
#   #  Alcohol.Frequency,
#   #  Alcohol.Intensity,
#   #  SexualSatisfaction,
#   #  POWERDEPENDENCE1,
#   #  POWERDEPENDENCE2,
#   Your.Future.Security,
#   Your.Personal.Relationships,
#   Your.Health,
#   Standard.Living
# )) |> # |> dplyr::filter(Wave == 2015)|>
# dplyr::filter(!is.na(pets)) %>% # no missingness in intervention
# dplyr::filter(!is.na(pets_lead1)) |>
#  dplyr::filter(pets == 1) |>
#  dplyr::filter(Hours.Pets > 5) |>
arrange(Id, Wave) |>
  # group_by(Id) |>
  # mutate(lead_church = dplyr::lead(a_Church, order_by = time)) |>
  # mutate(dep =  ifelse(
  #   !is.na(kessler_depressed) |
  #     (Wave == 2019 &
  #        is.na(kessler_depressed)) |
  #     (Wave == 2019 & !is.na(kessler_depressed)),
  #   1,
  #   0
  # )) |>
  # filter(dep == 1) |>
ungroup() |>
  dplyr::select(-c(Religion.Church,
                   Wave,
                   YearMeasured)) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Time) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  rename(time = Time) |>
  mutate(time = time -1) |>
  arrange(Id)

# Check
table1::table1( ~ a_Church | time, data = dpp)
table1::table1( ~ Censoring + kessler_worthless | time, data = dpp)

# check
naniar::vis_miss(dpp)

length(unique(dp$Id)) #

# make wider
dp_wide <- dpp |>
  pivot_wider(
    id_cols = Id,
    names_from = time,
    values_from = -c(Id, time),
    names_glue = "l{time}_{.value}",
    names_prefix = "l"
  )

# check
colnames(dp_wide)

# wrangle
dp_w <- dp_wide |>
  rename(
    Gender3 = l0_Gender3,
    EthCat = l0_EthCat,
    GenCohort = l0_GenCohort,
    BornNZ = l0_BornNZ,
    a0 = l0_a_Church,
    a1 = l1_a_Church,
    a2 = l2_a_Church,
    a3 = l3_a_Church,
    a4 = l4_a_Church,
    a5 = l5_a_Church,
    y_worthless = l6_kessler_worthless,
  ) |>
  select(
    c(
      Gender3,
      EthCat,
      GenCohort,
      BornNZ,
      a0,
      a1,
      a2,
      a3,
      a4,
      a5,
      #  a6,
      #  a7,
      starts_with("l0_"),
      starts_with("l1_"),
      starts_with("l2_"),
      starts_with("l3_"),
      starts_with("l4_"),
      starts_with("l5_"),
      # ends_with("_t6"),
      #  ends_with("_t7"),
      starts_with("y_")
    )
  ) |>
  select(-c(
    ends_with("_Gender3"),
    ends_with("_EthCat"),
    ends_with("_GenCohort"),
    ends_with("_Gender3"),
    ends_with("_BornNZ")
  )) |>
  rename(
    l0_Gender3 = Gender3,
    l0_EthCat = EthCat,
    l0_BornNZ = BornNZ,
    l0_GenCohort = GenCohort
  )

# check
table(dp_w$l5_censoring)


# wrangling
dp_ww <-  dp_w |>
  select(
    c(
      starts_with("l0_"),
      starts_with("l1_"),
      starts_with("l2_"),
      starts_with("l3_"),
      starts_with("l4_"),
      starts_with("l5_"),
      starts_with("a"),
      starts_with("y_")
    )
  ) |>
  dplyr::relocate(c(l0_Gender3, l0_EthCat, l0_GenCohort, l0_BornNZ),
                  .before  = c(ends_with("t_0"))) |>
  dplyr::relocate(a0, .after = c(starts_with("l0_"))) |>
  dplyr::relocate(a1, .after =  c(starts_with("l1_"))) |>
  dplyr::relocate(a2, .after =  c(starts_with("l2_"))) |>
  dplyr::relocate(a3, .after =  c(starts_with("l3_"))) |>
  dplyr::relocate(a4, .after = c(starts_with("l4_"))) |>
  dplyr::relocate(a5, .after =  c(starts_with("l5_"))) |>
  # dplyr::relocate(a6, .after = c(ends_with("_t6"))) |>
  # dplyr::relocate(a7, .after = c(ends_with("_t7"))) |>
  #  dplyr::relocate(a7, .before = (starts_with("y_")))
  dplyr::relocate(a5, .before = (starts_with("y_"))) |>
  rename(
    C0 = l0_Censoring,
    C1 = l1_Censoring,
    C2 = l2_Censoring,
    C3 = l3_Censoring,
    C4 = l4_Censoring,
    C5 = l5_Censoring
  ) |>
  dplyr::relocate(C0, .after = a0) |>
  dplyr::relocate(C1, .after = a1) |>
  dplyr::relocate(C2, .after = a2) |>
  dplyr::relocate(C3, .after = a3) |>
  dplyr::relocate(C4, .after = a4) |>
  dplyr::relocate(C5, .after = a5)


# not needed
#dp_ww2<- dp_ww[complete.cases(dp_ww[, 1:28]),]

# check
table(is.na(dp_ww$y_worthless))

#check
naniar::vis_miss(dp_ww)

# ids
nrow(dp_ww)

#
#
# devtools::install_github("nt-williams/lmtp@sl3")
#
# library(lmtp)
# head(sim_t4)
# policy <- function(data, trt) {
#   a <- data[[trt]]
#   (a - 1) * (a - 1 >= 1) + a * (a - 1 < 1)
# }
#
# dp_ww

#devtools::install_github("nt-williams/lmtp@devel")
#devtools::install_github("nt-williams/lmtp@sl3")

library(lmtp)
library(SuperLearner)
library(sl3)


# Settings from https://github.com/kathoffman/steroids-trial-emulation
# Thank you KAT!!


lrn_lasso <- Lrnr_glmnet$new(alpha = 1)
lrn_ridge <- Lrnr_glmnet$new(alpha = 0)
lrn_enet <- Lrnr_glmnet$new(alpha = 0.5)
lrn_bart <- Lrnr_bartMachine$new()
lrn_mean <- Lrnr_mean$new()

mars_grid_params <- list(# manually create a grid of MARS learners
  degree = c(2, 3),
  penalty = c(1, 2, 3))
mars_grid <- expand.grid(mars_grid_params, KEEP.OUT.ATTRS = FALSE)
mars_learners <-
  apply(mars_grid, MARGIN = 1, function(tuning_params) {
    do.call(Lrnr_earth$new, as.list(tuning_params))
  })


learners <-
  unlist(list(#mars_learners, # don't include commented out libraries in demo code due to time
    lrn_lasso,
    #lrn_ridge,
    #lrn_enet,
    #lrn_bart,
    lrn_mean),
    recursive = TRUE)
lrnrs <-
  make_learner(Stack, learners) # stack all learners together, see sl3 documentation


dp_ww <- as.data.frame(dp_ww)
head(dp_ww)
a <- c("a0", "a1", "a2", "a3", "a4", "a5")
bs <- c("l0_Gender3", "l0_EthCat", "l0_GenCohort", "l0_BornNZ")
l <- list(
  c("l0_kessler_worthless"),
  c("l1_kessler_worthless"),
  c("l2_kessler_worthless"),
  c("l3_kessler_worthless"),
  c("l4_kessler_worthless"),
  c("l5_kessler_worthless")
)

folds <- 2 # use more
k <-
  5 # we assume that data from k years previously is sufficient for the persons's covariate history
y <- "y_worthless"

# policy<- function(data, trt){
#   a
# }


# static treatment
no_church <- function(data, trt) {
  data[[trt]] <-
    0 # just change the entire treatment vector of interest to 0
  data[[trt]] # and return
}

# static treatment
all_church <- function(data, trt) {
  data[[trt]] <-
    5# just change the entire treatment vector of interest to 0
  data[[trt]] # and return
}

##
censor <- c("C0", "C1", "C2", "C3", "C4", "C5")

censor
dp_ww
dp_ww <- as.data.frame(dp_ww)

hist( dp_ww$l0_kessler_worthless )



# test not working
mm0f <- lmtp_sdr(
  dp_ww,
  trt = a,
  outcome = y,
  baseline = bs,
  cens = censor,
  time_vary = l,
  outcome_type = "continuous",
  shift = no_church,
  k = 5,
  learners_outcome = lrnrs,
  learners_trt = lrnrs,
  folds = 5,
  .trim = 0.95,
  intervention_type = "static"
)


tidy(mm0f)
warnings()

mm0f %>% tidy()


# estimator estimate std.error conf.low conf.high
# <chr>        <dbl>     <dbl>    <dbl>     <dbl>
#   1 TMLE         0.375    0.0127    0.350     0.400
#

# real deal
mm1f <- lmtp_sdr(
  dp_ww,
  #  test,
  trt = a,
  outcome = y,
  baseline = bs,
  cens = censoring,
  time_vary = l,
  outcome_type = "continuous",
  shift = all_church,
  k = 2,
  learners_outcome = lrnrs,
  learners_trt = lrnrs,
  folds = 5,
  .trim = 0.95,
  intervention_type = "static"
)

tidy(mm1f)
mm1f %>% tidy()
mm0f %>% tidy()

library(tidyverse)
library(gt)
library(lmtp)


ests <-
  imap_dfr(list("No Church" = mm0f, "All Church" = mm1f),
           function(x, y) {
             x %>%
               tidy() %>%
               mutate(
                 intervention = y,
                 estimate = 1 - estimate,
                 conf_low = 1 - conf.high,
                 conf_high = 1 - conf.low
               ) %>%
               select(intervention, estimate, conf_low, conf_high)

           })

diff <- lmtp_contrast(mm1f, ref = mm0f)$vals %>%
  mutate(
    estimate = -theta,
    conf_low = -conf.high,
    conf_high = -conf.low,
    intervention = "Difference"
  )  %>%
  select(intervention, estimate, conf_low, conf_high)



ests %>%
  bind_rows(diff) %>%
  gt() %>%
  fmt_number(columns = 2:4) |>  #decimals = 1, scale_by=.1) %>%
  cols_merge(columns = 2:4, pattern = c("{1} ({2}, {3})")) %>%
  cols_label(intervention = "Intervention",
             estimate = "Estimated 7 year Depression Rate \n(95% CI)")





