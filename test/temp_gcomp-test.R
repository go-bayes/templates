## GCOMP IMPUTE METHOD Test


options(scipen = 999)

#libraries and functions
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")



# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

#conflict_prefer("pool", "mice")
#conflict_prefer("cbind", "base")
# for saving models
# push_mods <-
#   fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/mods")
# push_figs <-
#   fs::path_expand("~/Users/joseph/The\ Virtues\ Project\ Dropbox/outcomewide/figs")

# read data
# read data
pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# note that you need use the arrow package
dat <- arrow::read_parquet(pull_path)


dat_new <- dat %>%
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
  #%>%
  # dplyr::rename(. # did this in the man dataframe
  #   kessler_hopeless = SWB.Kessler01,
  #   # …  you feel hopeless?
  #   kessler_depressed = SWB.Kessler02,
  #   #…  you feel so depressed that nothing could cheer you up?
  #   kessler_restless  = SWB.Kessler03,
  #   #…  you feel restless or fidgety?
  #   kessler_effort = SWB.Kessler04,
  #   #…  you feel that everything was an effort?
  #   kessler_worthless = SWB.Kessler05,
#   #…  you feel worthless?
#   kessler_nervous = SWB.Kessler06 #…  you feel nervous?
# ) |>
# dplyr::rename(
#  # My life has a clear sense of purpose.
#   meaning_purpose= LifeMeaning01,
#  # I have a good sense of what makes my life meaningful.
#   meaning_sense = LifeMeaning02,
#   #…  you feel so depressed that nothing could cheer you up?
#
# ) |>
dplyr::filter((Wave == 2015  & YearMeasured  == 1) |
                (Wave == 2019  &
                   YearMeasured  == 1) |
                (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
  group_by(Id) %>%
  dplyr::mutate(org2015 =  ifelse(Wave == 2015 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold15 = mean(org2015, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold15 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup() %>%
  mutate(
    Pets = factor(Pets),
    pets = as.numeric(Pets),
    pets_coded = as.numeric(Pets.coded.catdog_factor) - 1
  ) |>
  droplevels() %>%
  arrange(Id, Wave)


###



dat_prep  <- dat_new %>%
  arrange(Wave, Id) |>
  mutate( time = as.numeric(Wave)-1) |>
  select(
    Id,
    time,
    pets,
    YearMeasured,
    Wave,
    Gender3,
    Partner,
    EthCat,
    GenCohort,
    NZSEI13,
    pets,
    Hours.Pets,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Edu,
    NZDep2013,
    Employed,
    # HomeOwner,
    Pol.Orient,
    SDO,
    RWA,
    Rural_GCH2018,
    Household.INC,
    Parent,
    Relid,
    Religion.Church,
    Believe.Spirit,
    Believe.God,
    #  Spiritual.Identification,
    SWB.SoC01,
    #  EmotionRegulation1,
    #  EmotionRegulation2,
    #  EmotionRegulation3,
    #  Bodysat,
    #  VENGEFUL.RUMIN,
    # retired,
    # semiretired,
    BornNZ,
    kessler_hopeless,
    #   # …  you feel hopeless?
    kessler_depressed,
    #   #…  you feel so depressed that nothing could cheer you up?
    kessler_restless,
    #   #…  you feel restless or fidgety?
    kessler_effort,
    #   #…  you feel that everything was an effort?
    kessler_worthless,
    #   #…  you feel worthless?
    kessler_nervous,
    #…  you feel nervous?
    # KESSLER6sum,
    HLTH.Fatigue,
    # Rumination,
    Smoker,
    ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    #  GRATITUDE,
    Hours.Work,
    HLTH.SleepHours,
    HLTH.Disability,
    Hours.Exercise,
    #   LIFEMEANING,
    LIFESAT,
    # PWI,  ##  we use the individual
    #  NWI,
    SFHEALTH,
    #  SELF.CONTROL,
    # SFHEALTH,
    #  SELF.ESTEEM,
    #    Respect.Self,
    #  GenCohort,
    #  SELF.ESTEEM,
    #    SELF.CONTROL,
    #  Emp.WorkLifeBalance,
    #   Alcohol.Frequency,
    #  Alcohol.Intensity,
    #   HLTH.BMI,
    #   Smoker,
    ChildrenNum,
    # GenCohort,
    #  partnerlost_job,
    #  lost_job,
    #  began_relationship,
    # Alcohol.Intensity,
    # Alcohol.Frequency,
    # SexualOrientation,
    #   SexualSatisfaction,
    #  POWERDEPENDENCE1,
    #  POWERDEPENDENCE2,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living#,
    #    PERFECTIONISM,
    #   PermeabilityIndividual,
    #   ImpermeabilityGroup,
    #  Emp.JobSecure. # lots missing
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  arrange(Id, Wave) %>%
  dplyr::mutate(#  Volunteers = if_else(HoursCharity == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church), ) %>%
  dplyr::mutate(#  Volunteers = if_else(HoursCharity == 1, 1, 0),
    Hours.Pets_log = log(Hours.Pets + 1)) %>%
#   arrange(Id, Wave)  %>% # dplyr::mutate(Hours.Work_lead1 = lead(Hours.Work, n = 1)) %>%
  dplyr::rename(a_pets = pets) |>
#   # dplyr::mutate(across(c(pets),
#   #                      ~ lead(.x, n = 1),
#   #                      .names = "{col}_lead1"))%>% # make leads
#   # dplyr::mutate(pets_lost1 = ifelse(pets_lead1 == 0, 1, 0)) |>
  dplyr::rename_with(~paste("y_", ., sep = ""),
    c(
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
      #    VENGEFUL.RUMIN,
      kessler_hopeless,
      #   # …  you feel hopeless?
      kessler_depressed,
      #   #…  you feel so depressed that nothing could cheer you up?
      kessler_restless,
      #   #…  you feel restless or fidgety?
      kessler_effort,
      #   #…  you feel that everything was an effort?
      kessler_worthless,
      #   #…  you feel worthless?
      kessler_nervous,
      #…  you feel nervous?
      # KESSLER6sum,
      HLTH.Fatigue,
      #  Rumination,
      #  Smoker,
      #  HLTH.BMI,
      BELONG,
      SUPPORT,
      community,
      # CharityDonate,
      # HoursCharity,
      # GRATITUDE,
      # Hours.Work,
      # HLTH.SleepHours,
      # HLTH.Disability,
      # Hours.Exercise,
      #  LIFEMEANING,
      LIFESAT,
      # PWI, can reconstruct later
      # NWI,
      SFHEALTH,
      #  SELF.CONTROL,
      #   SFHEALTH,
      #  SELF.ESTEEM,
      #  Respect.Self,
      #  SELF.ESTEEM,
      #  SELF.CONTROL,
      #  Emp.WorkLifeBalance,
      #  Alcohol.Frequency,
      #  Alcohol.Intensity,
      #  SexualSatisfaction,
      #  POWERDEPENDENCE1,
      #  POWERDEPENDENCE2,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    )) |> # |> dplyr::filter(Wave == 2015)|>
 # dplyr::filter(!is.na(pets)) %>% # no missingness in intervention
 # dplyr::filter(!is.na(pets_lead1)) |>
#  dplyr::filter(pets == 1) |>
#  dplyr::filter(Hours.Pets > 5) |>
  dplyr::select(-c(Religion.Church,
                   Church,
                   # Respect.Self_lead2,
                   # not there
                   # Emp.WorkLifeBalance,
                   # not at baseline,
                  # Hours.Pets,
                  # pets,
                 #  pets_lead1,
                   YearMeasured)) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

dt <- dat_prep |>
  select(Id,
         time,
         Gender3,
         Partner,
         EthCat,
         GenCohort,
         Hours.Pets_log,
         Partner,
         NZSEI13,
         Edu,
         CONSCIENTIOUSNESS,
         OPENNESS,
         HONESTY_HUMILITY,
         EXTRAVERSION,
         NEUROTICISM,
         AGREEABLENESS,
         NZDep2013,
         Employed,
         Pol.Orient,
         SDO,
         RWA,
         Rural_GCH2018,
         Household.INC,
         Parent,
         Relid,
         Believe.Spirit,
         Believe.God,
         BornNZ,
         Smoker,
         ChildrenNum,
         NWI,
         CharityDonate,
         HoursCharity,
         Hours.Work,
         HLTH.SleepHours,
         HLTH.Disability,
         Hours.Exercise,
         a_pets,
         starts_with("y_"))



# make wider

dt_wide <- dt |>
  pivot_wider(
    id_cols = Id,
    names_from = time,
    values_from = -c(Id, GenCohort, time, Gender3, EthCat),
    names_prefix = "t" )


head(dt_wide[, 135:145])
dt_wide
#
# library(tidyverse)
# suffixes <- unique(dt$time)
# suffixes
#
#
# names_to_order <- map(suffixes, ~ names(dt_wide)[grep(paste0("_", .x), names(dt_wide))]) %>% unlist
# names_id <- setdiff(names(dt_wide), names_to_order)
#
# dt_wide %>%
#   select(names_id, all_of(names_to_order))
# mice impute -------------------------------------------------------------


# mice impute
library(mice)
str(mice_a)
mice_a <- dat_prep %>%
  dplyr::select(-c(Wave, Id))  # won't otherwise run

library(naniar)
naniar::gg_miss_var(mice_a)

dev.off()
# any col.inear vars?
mice:::find.collinear(mice_a)

# impute
ppm_mice <- mice::mice(mice_a,  seed = 0, m = 10)

# save
saveRDS(
  ppm_mice,
  here::here(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets",
    "outcomewide-pets-ppm_mice_lost-gcomp"
  )
)

# read
ppm_mice <-
  readRDS(
    here::here(
      "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets",
      "outcomewide-pets-ppm_mice_lost-gcomp"
    )
  )




# test
library(gFormulaMI)

# read data, imputed already from the pets -lost file
pets_lost1

imps <- gFormulaImpute(
  data = ppm_mice,
  M = 10,
  trtVarStem = "pets_",
  timePoints = 1,
  trtRegimes = list(c(0), c(1))
)
