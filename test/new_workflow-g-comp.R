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

# explore data
# describe full data set

# example
dat |>
  filter(Wave == 2018 & YearMeasured==1) |>
  select(PERFECTIONISM, Gender, Id, Wave) |>
  drop_na() |>
  summarise(count_distinct = n_distinct(Id))


# another example
dat |>
  filter(Wave == 2018 & YearMeasured==1) |>
  select(PERFECTIONISM, Gender, Id, Wave) |>
  mutate(Male = as.factor(Gender)) |>
  drop_na() |>
  ggplot(aes(x=as.factor(Gender), y=PERFECTIONISM, colour = factor(Gender))) +
  geom_boxplot(notch = TRUE) + geom_jitter(shape=16, position=position_jitter(0.2), alpha = .1) + labs(
    title = "Perfectionism by Gender: NZAVS years 2018-2019, N = 47823",
    y = "Doing my best never seems to be enough.\nMy performance rarely measures up to my standards.\nI am hardly ever satisfied with my performance.",
    x = "Male coded as 1, other identities coded as 0") + scale_color_viridis_d(option = "D")

# clear graph
dev.off()




# create wide format data with baseline

# Example



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
  dplyr::mutate(Male = as.factor(ifelse(
    GendAll == 1,
    "Male", "Not_male"
  ))) |>
# dplyr::rename(.  # use this for some studies
#  # My life has a clear sense of purpose.
#   meaning_purpose= LifeMeaning01,
#  # I have a good sense of what makes my life meaningful.
#   meaning_sense = LifeMeaning02,
#   #…  you feel so depressed that nothing could cheer you up?
# ) |>
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
  # dplyr::filter(Id != 9630) %>% # problematic for income
  group_by(Id) %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup() %>%
  droplevels() %>%
  arrange(Id, Wave)



# data wrangle ------------------------------------------------------------

# to do, add labels for every measure in comments

dat_prep  <- dat_new %>%
  arrange(Wave, Id) |>
  mutate(time = as.numeric(Wave) - 1) |>
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
  dplyr::rename(Community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  arrange(Id, Wave) %>%
  dplyr::mutate(#  Volunteers = if_else(HoursCharity == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),) %>%
  dplyr::mutate(
    #  Volunteers = if_else(HoursCharity == 1, 1, 0),
    Hours.Pets_log = log(Hours.Pets + 1),
    Household.INC_log = log(Household.INC  + 1)
  ) %>%
  #   arrange(Id, Wave)  %>% # dplyr::mutate(Hours.Work_lead1 = lead(Hours.Work, n = 1)) %>%
  dplyr::mutate(a_pets = pets - 1) |>
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
  select(
    Id,
    time,
    Gender3,
    Partner,
    EthCat,
    GenCohort,
    Hours.Pets_log,
    #  NZSEI13,
    #  Edu,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    #  NZDep2013,
    #  Employed,
    #  Pol.Orient,
    # SDO,
    #  RWA,
    #   Rural_GCH2018,
    #  Household.INC_log,
    #  Parent,
    #  Relid,
    #  Believe.Spirit,
    #   Believe.God,
    BornNZ,
    #   Smoker,
    #   ChildrenNum,
    # CharityDonate,
    # HoursCharity,
    Hours.Work,
    #  HLTH.SleepHours,
    #  HLTH.Disability,
    #  Hours.Exercise,
    a_pets,
    community,
    #  kessler_hopeless,
    #  kessler_depressed,
    #  kessler_restless,
    #  kessler_effort,
    #  kessler_worthless,
    #  kessler_nervous,
    #  HLTH.Fatigue,
    #  BELONG,
    #   SUPPORT,
    #  HLTH.SleepHours,
    # HLTH.Disability,
    #  Hours.Exercise,
    #   LIFEMEANING,
    # LIFESAT,
    SFHEALTH,
    community
    #  Your.Future.Security,
    # Your.Personal.Relationships,
    #Your.Health,
    #   Standard.Living
  )




