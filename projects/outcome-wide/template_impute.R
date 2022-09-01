# imputation template

# set up digits
options(scipen = 999)
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# for original NZAVS data -- need to contact Chris Sibley for access

my_data <- "dff"  # whatever your object is called here
pull_path <- here::here("data", my_data)

dff <- readRDS(pull_path)

# Worked example selecting waves 2018 -- 2020 with exposure year as 2019

tab_in <- dff %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::rename(
    kessler_hopeless = SWB.Kessler01,
    # …  you feel hopeless?
    kessler_depressed = SWB.Kessler02,
    #…  you feel so depressed that nothing could cheer you up?
    kessler_restless  = SWB.Kessler03,
    #…  you feel restless or fidgety?
    kessler_effort = SWB.Kessler04,
    #…  you feel that everything was an effort?
    kessler_worthless = SWB.Kessler05,
    #…  you feel worthless?
    kessler_nervous = SWB.Kessler06 #…  you feel nervous?
  ) |>
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


# check n # 34782
length(unique(tab_in$Id)) # 34783

# how is something changing? (Attrition of disabled people?)
# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?

tab_in %>%
  group_by(Wave) %>%
  summarise(mean(HLTH.Disability, na.rm = TRUE))


# select-variables  -------------------------------------------------------
## select vars
data_start <- tab_in %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  #dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    YearMeasured,
    Wave,
    Partner,
    EthCat,
    Age,
    Male,
    NZSEI13,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Edu,
    NZdep,
    Employed,
    HomeOwner,
    Pol.Orient,
    SDO,
    RWA,
    Urban,
    Household.INC,
    Parent,
    Relid,
    Religion.Church,
    Believe.Spirit,
    Believe.God,
    Spiritual.Identification,
    SWB.SoC01,
    EmotionRegulation1,
    EmotionRegulation2,
    EmotionRegulation3,
    Bodysat,
    VENGEFUL.RUMIN,
    retired,
    semiretired,
    BornNZ,
    KESSLER6sum,
    HLTH.Fatigue,
    Rumination,
    Smoker,
    ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    GRATITUDE,
    Hours.Work,
    HLTH.SleepHours,
    HLTH.Disability,
    Hours.Exercise,
    LIFEMEANING,
    LIFESAT,
    # PWI,  ##  we use the individual
    NWI,
    SFHEALTH,
    SELF.CONTROL,
    SFHEALTH,
    SELF.ESTEEM,
    Respect.Self,
    #  GenCohort,
    SELF.ESTEEM,
    SELF.CONTROL,
    Emp.WorkLifeBalance,
    Alcohol.Frequency,
    Alcohol.Intensity,
    HLTH.BMI,
    Smoker,
    ChildrenNum,
    # GenCohort,
    partnerlost_job,
    lost_job,
    began_relationship,
    Alcohol.Intensity,
    Alcohol.Frequency,
    SexualSatisfaction,
    POWERDEPENDENCE1,
    POWERDEPENDENCE2,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    PERFECTIONISM,
    PermeabilityIndividual,
    ImpermeabilityGroup,
    Emp.JobSecure,
    Volunteers,
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  arrange(Id, Wave) %>%
  dplyr::mutate(
    Volunteers = if_else(HoursCharity == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
  ) %>%
  arrange(Id, Wave)  %>% # dplyr::mutate(Hours.Work_lead1 = lead(Hours.Work, n = 1)) %>%
  dplyr::mutate(across(c(Hours.Work,
                         HLTH.Disability),
                       ~ lead(.x, n = 1),
                       .names = "{col}_lead1")) %>% # make leads
  dplyr::mutate(across(
    c(
      NZSEI13,
      NZdep,
      Employed,
      Household.INC,
      community,
      Hours.Work,
      HLTH.Disability,
      EmotionRegulation1,
      EmotionRegulation2,
      EmotionRegulation3,
      Bodysat,
      VENGEFUL.RUMIN,
      KESSLER6sum,
      HLTH.Fatigue,
      Rumination,
      Smoker,
      HLTH.BMI,
      BELONG,
      SUPPORT,
      CharityDonate,
      HoursCharity,
      GRATITUDE,
      Hours.Work,
      HLTH.SleepHours,
      HLTH.Disability,
      Hours.Exercise,
      LIFEMEANING,
      LIFESAT,
      # PWI, can reconstruct later
      NWI,
      SFHEALTH,
      SELF.CONTROL,
      SFHEALTH,
      SELF.ESTEEM,
      Respect.Self,
      SELF.ESTEEM,
      SELF.CONTROL,
      Emp.WorkLifeBalance,
      Alcohol.Frequency,
      Alcohol.Intensity,
      SexualSatisfaction,
      POWERDEPENDENCE1,
      POWERDEPENDENCE2,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living,
      PERFECTIONISM,
      PermeabilityIndividual,
      ImpermeabilityGroup,
      HomeOwner,
      Volunteers
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
  dplyr::filter(Household.INC >= 30975) %>% # min income
  dplyr::filter(!is.na(Hours.Work)) %>%
  dplyr::filter(!is.na(Hours.Work_lead1)) %>%
  dplyr::select(
    -c(
      Religion.Church,
      HoursCharity,
      Respect.Self_lead2,
      # not there
      Emp.WorkLifeBalance,
      # not at baseline
      YearMeasured,
      HLTH.Disability_lead1
    )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)


# number of ids
length(unique(data_start$Id)) #28676

data_start$Hom

# inspect data, with eye to large missingness
skim(data_start) |>
  arrange(n_missing)

# MICE --------------------------------------------------------------------
# mice model  -------------------------------------------------------------
library(mice)

mice_cc <- data_start %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>%
  #  mutate(across(where(is.double), as.numeric)) |>
  dplyr::select(-c(Wave, Id)) |> data.frame()

library(naniar)
naniar::gg_miss_var(mice_cc)

# any colinear vars?
mice:::find.collinear(mice_cc)

# impute
mice_cc <- mice::mice(mice_cc,  seed = 0, m = 10)

# save your mice model
saveRDS(mice_cc, here::here("data", "mice_imputed"))

# check your mice model
outlist2 <-
  row.names(mice_cc)[mice_cc$outflux < 0.5]
length(outlist2)

# checks
head(mice_cc$loggedEvents, 10)

# read your mice model
mice_cc<- readRDS(here::here("data", "mice_imputed")

# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

cc_l <- mice::complete(mice_cc, "long", inc = TRUE)



# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(cc_l) |>
  arrange(n_missing)

# for keeping track of ID's in mice data
N <- length(unique(data_start$Id))
N
# create variables in z score
cc_l2 <- cc_l %>%
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
 # dplyr::mutate(income_log_lead1 = log(Household.INC_lead1 + 1)) |>
  dplyr::mutate(income_lead2_log = log(Household.INC_lead2 + 1)) |>
  dplyr::mutate(Church = ifelse(Church > 8, 8, Church)) |>
 # dplyr::mutate(Church_lead1 = ifelse(Church_lead1 > 8, 8, Church_lead1)) |>
  # dplyr::mutate( inc_prop = (income_log / (income_log_lead1) - 1)) |>
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0)) %>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0)) %>%
 # dplyr::mutate(Volunteers = if_else(HoursCharity > 1, 1, 0)) |>
#  dplyr::mutate(Volunteers_lead2 = if_else(HoursCharity_lead2 > 1, 1, 0)) |>
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2 + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2 + 1)) %>%
  dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) %>%
  dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0))) %>%
  dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0))) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(
    Your.Personal.Relationships_lead2, digits = 0
  ) + 1)) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0))) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0) + 1)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0) + 1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(alcohol_bin2 = if_else(Alcohol.Frequency > 3, 1, 0)) %>%
  dplyr::mutate(alcohol_bin = if_else(Alcohol.Frequency > 2, 1, 0)) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  dplyr::mutate(Hours.Work_lead1_10 =  Hours.Work_lead1 / 10) %>%
  # dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1 / 10)) %>%
  # dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1))) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2 / 10)) %>%
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  dplyr::group_by(id) |>
  dplyr::mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  dplyr::mutate(PWI_lead2 = mean(
    c(
      Your.Future.Security_lead2,
      Your.Personal.Relationships_lead2,
      Your.Health_lead2,
      Standard.Living_lead2
    ),
    na.rm = TRUE
  )) |>
  # dplyr::mutate(KESSLER6sum = rowSums(across(
  #   c(
  #     kessler_hopeless,
  #     # …  you feel hopeless?
  #     kessler_depressed,
  #     #…  you feel so depressed that nothing could cheer you up?
  #     kessler_restless,
  #     #…  you feel restless or fidgety?
  #     kessler_effort,
  #     #…  you feel that everything was an effort?
  #     kessler_worthless,
  #     #…  you feel worthless?
  #     kessler_nervous #…  you feel nervous?
  #   )
  # ))) |>
  # dplyr::mutate(KESSLER6sum_lead2 = rowSums(across(
  #   c(
  #     kessler_hopeless_lead2,
  #     # …  you feel hopeless?
  #     kessler_depressed_lead2,
  #     #…  you feel so depressed that nothing could cheer you up?
  #     kessler_restless_lead2,
  #     #…  you feel restless or fidgety?
  #     kessler_effort_lead2,
  #     #…  you feel that everything was an effort?
  #     kessler_worthless_lead2,
  #     #…  you feel worthless?
  #     kessler_nervous_lead2
  #   ) #…  you feel nervous?
  # ))) |>
  ungroup() |>
  droplevels() |>
  dplyr::mutate(KESSLER6sum = round(as.integer(KESSLER6sum, 0))) %>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0))) %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) %>%
  dplyr::mutate(EthCat = as.factor(EthCat))


# Check
# cc_l2 %>%
#   select( kessler_hopeless, # …  you feel hopeless?
#           kessler_depressed, #…  you feel so depressed that nothing could cheer you up?
#           kessler_restless, #…  you feel restless or fidgety?
#           kessler_effort, #…  you feel that everything was an effort?
#           kessler_worthless, #…  you feel worthless?
#           kessler_nervous, KESSLER6sum)
# # Get data into shape
cc_l2 <- cc_l2 %>% mutate_if(is.matrix, as.vector)
# cc3l <- cc_l %>% mutate_if(is.matrix, as.vector)

# imputed data
data_imputed <- mice::as.mids(cc_l2)

# imputed data in long format

data_long <- mice::complete(data_imputed, "long", inc = F)

# raw data (pre-imputation) for sensitivity analysis
data_raw <- data_long |>
  slice(1:N)


## SAVE DATA
saveRDS(data_raw, here::here("data","data_raw"))
saveRDS(data_long, here::here("data","data_long"))
saveRDS(data_imputed, here::here("data","data_imputed"))





# EXAMPLE DEMOGRAPHIC TABLE -----------------------------------------------


df_crr <-  df_cr |>
  dplyr::mutate(Volunteers = if_else(HoursCharity > 0, 1, 0))

df_crr <- df_cr |> dplyr::group_by(Id) |> mutate(PWI = mean(
  c(
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living
  ),
  na.rm = TRUE
))

df_crr$Male <- factor(df_cr$Male, labels = c("No", "Yes"))
df_crr$EthCat <-
  factor(df_cr$EthCat, labels = c("Euro", "Maori", "Pacific", "Asian"))
df_crt$Believe.Spirit <-
  factor(df_cr$Believe.Spirit, labels = c("No", "Yes"))
df_crt$Believe.God <-
  factor(df_cr$Believe.God, labels = c("No", "Yes"))
df_crt$Employed <- factor(df_cr$Employed, labels = c("No", "Yes"))
df_crt$Volunteers <-
  factor(df_crt$Volunteers, labels = c("No", "Yes"))
df_crt$Parent <- factor(df_cr$Parent, labels = c("No", "Yes"))
df_crt$Partner <- factor(df_cr$Partner, labels = c("No", "Yes"))
df_crt$Retired <- factor(df_cr$retired, labels = c("No", "Yes"))
df_crt$SemiRetired <-
  factor(df_cr$semiretired, labels = c("No", "Yes"))
df_crt$Urban <- factor(df_cr$Urban, labels = c("No", "Yes"))
df_crt$BigDoms <-
  factor(df_cr$BigDoms,
         labels = c("Buddhist", "Christian", "Muslim", "TheOthers"))
df_crt$NeighbourhoodCommunity <- df_cr$community
df_crt$MajorDenominations <- df_cr$BigDoms





#and continue this way to obtain factor labels ...etc.

table1::table1(
  ~ Age +
    BornNZ +
    Edu +
    Employed +
    EthCat +
    NZdep +
    NZSEI13 +
    Parent +
    Partner +
    Pol.Orient +
    Male +
    Urban,
  data = df_crr,
  transpose = F
)


# Personality


table1::table1(
  ~ AGREEABLENESS +
    CONSCIENTIOUSNESS +
    EXTRAVERSION +
    HONESTY_HUMILITY +
    NEUROTICISM +
    OPENNESS +
    KESSLER6sum,
  data = df_crr,
  transpose = F
)

table1::table1(
  ~ LIFESAT +
    PWI +
    Respect.Self +
    RWA +
    SDO +
    SELF.CONTROL +
    SELF.ESTEEM +
    SFHEALTH,
  data = df_crr,
  transpose = F
)

# religious
table1::table1(
  ~ Religion.CongregationSize +
    Relid +
    Believe.Spirit +
    Believe.God +
    Church +
    Religion.Prayer +
    Religion.Scripture +
    MajorDenominations,
  data = df_crr,
  transpose = F
)


# Social variables

table1::table1(
  ~ BELONG +
    NeighbourhoodCommunity +
    SUPPORT +
    National.Identity +
    PATRIOT,
  data = df_crr,
  transpose = F
)


# MULTILEVEL DATA ---------------------------------------------------------



### Create multi-level data for comparisions
data_ml <- tab_in |>
  select(Id,
         YearMeasured,
         Wave,
         Partner,
         EthCat,
         Age,
         Male,
         NZSEI13,
         CONSCIENTIOUSNESS,
         OPENNESS,
         HONESTY_HUMILITY,
         EXTRAVERSION,
         NEUROTICISM,
         AGREEABLENESS,
         Edu,
         NZdep,
         Employed,
         HomeOwner,
         Pol.Orient,
         SDO,
         RWA,
         Urban,
         Household.INC,
         Parent,
         Relid,
         Religion.Church,
         Believe.Spirit,
         Believe.God,
         Spiritual.Identification,
         SWB.SoC01,
         EmotionRegulation1,
         EmotionRegulation2,
         EmotionRegulation3,
         Bodysat,
         VENGEFUL.RUMIN,
         retired,
         semiretired,
         BornNZ,
         KESSLER6sum,
         HLTH.Fatigue,
         Rumination,
         Smoker,
         ChildrenNum,
         NWI,
         BELONG,
         SUPPORT,
         CharityDonate,
         HoursCharity,
         GRATITUDE,
         Hours.Work,
         HLTH.SleepHours,
         HLTH.Disability,
         Hours.Exercise,
         LIFEMEANING,
         LIFESAT,
         # PWI,  ##  we use the individual
         NWI,
         SFHEALTH,
         SELF.CONTROL,
         SFHEALTH,
         SELF.ESTEEM,
         Respect.Self,
         #  GenCohort,
         SELF.ESTEEM,
         SELF.CONTROL,
         Emp.WorkLifeBalance,
         Alcohol.Frequency,
         Alcohol.Intensity,
         HLTH.BMI,
         Smoker,
         ChildrenNum,
         # GenCohort,
         partnerlost_job,
         lost_job,
         began_relationship,
         Alcohol.Intensity,
         Alcohol.Frequency,
         SexualSatisfaction,
         POWERDEPENDENCE1,
         POWERDEPENDENCE2,
         Your.Future.Security,
         Your.Personal.Relationships,
         Your.Health,
         Standard.Living,
         PERFECTIONISM,
         PermeabilityIndividual,
         ImpermeabilityGroup,
         Emp.JobSecure) |>
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(wave = as.numeric(Wave)-1) |>
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(Church = ifelse(Religion.Church > 8, 8, Religion.Church)) |>
  # dplyr::mutate( inc_prop = (income_log / (income_log_lead1) - 1)) |>
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Volunteers = if_else(HoursCharity > 1, 1, 0)) |>
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate_log= log(CharityDonate + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity+ 1)) %>%
  dplyr::mutate(Exercise_log = log(Hours.Exercise+ 1)) %>%
  dplyr::mutate(Rumination_ord = as.integer(round(Rumination, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(SUPPORT_ord = as.integer(round(SUPPORT, digits = 0))) %>%
  dplyr::mutate(PERFECTIONISM_ord = as.integer(round(PERFECTIONISM, digits = 0))) %>%
  dplyr::mutate(VENGEFUL.RUMIN_ord = as.integer(round(VENGEFUL.RUMIN, digits = 0))) %>%
  dplyr::mutate(Standard.Living_ord = as.integer(round(Standard.Living, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_ord = as.integer(round(
    Your.Personal.Relationships, digits = 0
  ) + 1)) %>%
  dplyr::mutate(LIFEMEANING_ord = as.integer(round(LIFEMEANING, digits = 0))) %>%
  dplyr::mutate(HLTH.Fatigue_ord = as.integer(round(HLTH.Fatigue, digits = 0) + 1)) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  # dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1 / 10)) %>%
  # dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1))) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  dplyr::group_by(Id, Wave) |>
  dplyr::mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  # dplyr::mutate(KESSLER6sum = rowSums(across(
  #   c(
  #     kessler_hopeless,
  #     # …  you feel hopeless?
  #     kessler_depressed,
  #     #…  you feel so depressed that nothing could cheer you up?
  #     kessler_restless,
  #     #…  you feel restless or fidgety?
  #     kessler_effort,
  #     #…  you feel that everything was an effort?
  #     kessler_worthless,
  #     #…  you feel worthless?
  #     kessler_nervous #…  you feel nervous?
  #   )
  # ))) |>
  ungroup() |>
  droplevels() |>
  dplyr::mutate(KESSLER6sum = round(as.integer(KESSLER6sum, 0))) %>%
  dplyr::mutate(across(!c(Id, Wave, EthCat), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  dplyr::mutate(EthCat = as.factor(EthCat))  # labels = c("Euro", "Maori", "Pacific", "Asian")



table1::table1(~ KESSLER6sum_z |Wave, data = data_ml)


saveRDS(data_ml, here::here("data","data_ml"))



