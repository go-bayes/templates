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
dat$REGC_2018

sw$REGC_2022.T10.L


dt1 <- dat  |>
  dplyr::filter(YearMeasured == 1 & Wave == 2019) |> # JM's focus
  dplyr::select(REGC_2022,  COVID19.Timeline) |> # select vars of interest
  dplyr::mutate(
    cum_lockdowns_baseline = dplyr::case_when(
      COVID19.Timeline < 1.2 ~ 0,
      COVID19.Timeline > 1.2 & COVID19.Timeline < 2 ~ 2,
      COVID19.Timeline > 2 & REGC_2022 == 2 |
        COVID19.Timeline > 2 & REGC_2022 == 1 ~ 3,
    )
  )

table(dt1$cum_lockdowns_baseline)


dt_fix <- dat |>
  filter(YearMeasured == 1 & Wave == 2019) |>
  select(REGC_2022, COVID19.Timeline) |>
  mutate(
    cum_lockdowns_baseline = case_when(
      COVID19.Timeline < 1.2 ~ 0,
      COVID19.Timeline > 1.2 & COVID19.Timeline < 2 ~ 2,
      COVID19.Timeline > 2 & REGC_2022 %in% c(1, 2) ~ 3,
      COVID19.Timeline > 2 & REGC_2022 == 4 ~ 4,
    )
  )


table(dt_fix$cum_lockdowns_baseline)






table(dt4$cum_lockdowns_baseline)


dt0 <- dat  |>
  dplyr::filter(YearMeasured == 1 & Wave == 2019) |> # JM's focus
  dplyr::select(REGC_2022,  COVID19.Timeline) |> # select vars of interest
  dplyr::mutate(cum_lockdowns_baseline = if_else(
    COVID19.Timeline < 1.2,
    0,
    if_else(
      COVID19.Timeline > 1.2 & COVID19.Timeline < 2,
      2,
      if_else(
        COVID19.Timeline > 2 &
          REGC_2022 == 2 |
          COVID19.Timeline > 2 & REGC_2022 == 1,
        4,
        3
      )
    )
  ))
table(dt0$cum_lockdowns_baseline)


dat |>
  filter(Wave == 2018 & YearMeasured == 1) |>
  select(PERFECTIONISM, Gender, Id, Wave) |>
  drop_na() |>
  summarise(count_distinct = n_distinct(Id))


# another
dat |>
  filter(Wave == 2018 & YearMeasured == 1) |>
  select(PERFECTIONISM, Gender, Id, Wave) |>
  mutate(Male = as.factor(Gender)) |>
  drop_na() |>
  ggplot(aes(
    x = as.factor(Gender),
    y = PERFECTIONISM,
    colour = factor(Gender)
  )) +
  geom_boxplot(notch = TRUE) + geom_jitter(shape = 16,
                                           position = position_jitter(0.2),
                                           alpha = .1) + labs(title = "Perfectionism by Gender: NZAVS years 2018-2019, N = 47823",
                                                              y = "Doing my best never seems to be enough.\nMy performance rarely measures up to my standards.\nI am hardly ever satisfied with my performance.",
                                                              x = "Male coded as 1, other identities coded as 0") + scale_color_viridis_d(option = "D")



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
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  arrange(Id, Wave) %>%
  dplyr::mutate(#  Volunteers = if_else(HoursCharity == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church), ) %>%
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



# make wider
dt_wide <- dt |>
  pivot_wider(
    id_cols = Id,
    names_from = time,
    values_from = -c(Id, time),
    #  names_glue = "{time}_{.value}",
    names_prefix = "t"
  )


dt_w <- dt_wide |>
  select(ends_with("GenCohort_t2_t0"), "a_pets_t1", ends_with("_t2")) |>
  select(
    -c(
      "a_pets_t2",
      "Hours.Pets_log_t2",
      "BornNZ_t2",
      "GenCohort_t2",
      "BornNZ_t2",
      "Gender3_t2",
      "EthCat_t2"
    )
  )

head(dt_w)

dt_wide$community_t2
# another attempt simple
toy <- dt_wide |>
  select(
    ends_with("_t0"),
    "a_pets_t0",
    "SFHEALTH_t1",
    "a_pets_t1",
    "SFHEALTH_t2",
    "community_t2"
  ) |>
  rename(
    #l0 = "SFHEALTH_t0",
    # lb0 = "Gender3_t0",
    a0 = "a_pets_t0",
    l1 =  "SFHEALTH_t1",
    a1 = "a_pets_t1",
    y2_sf = "SFHEALTH_t2",
    y2_com = "community_t2"
  ) |>
  filter(!is.na(a0), !is.na(a1))



toy2 <- toy %>% relocate(a0, .before = a1) |>
  relocate(l1, .after = a0)
toy2


t3 <-
  toy2 |> mutate(across(ends_with("_t0"), .names = "l0_{col}")) |>
  select(starts_with(
    c(
      "l0_GenCohort",
      "l0_SFHEALTH",
      "l0_CONSCIENTIOUSNESS",
      "l0_OPENNESS",
      "l0_HONESTY_HUMILITY",
      "l0_EXTRAVERSION",
      "l0_NEUROTICISM",
      "l0_AGREEABLENESS"
    )
  ), a0, l1, a1, y2_sf)

# t3<- toy2 |> mutate(across(ends_with("_t0"), .names = "l0_{col}")) |>
# select(starts_with(c("l0_")), a0, l1, a1, y2_sf)

#
# t4<- toy2 |> mutate(across(ends_with("_t0"), .names = "l0_{col}")) |>
#     select(starts_with(c("l0_GenCohort","l0_SFHEALTH",
#                          "l0_CONSCIENTIOUSNESS",
#                          "l0_OPENNESS",
#                          "l0_HONESTY_HUMILITY",
#                          "l0_EXTRAVERSION",
#                          "l0_NEUROTICISM",
#                          "l0_AGREEABLENESS")), a0,l1, a1, y2_sf) |>
#   rename(l0_pets_baseline = "a0") |>
#   select(starts_with(c("l0_")), l1, a1, y2_sf)


head(t4)
nrow(toy2) # 8675

colnames(t3)
table(is.na((toy2$y2_com)))

names(t3)
# mice impute -------------------------------------------------------------


# mice impute
library(mice)
str(toy2)
dev.off()
library(naniar)
naniar::gg_miss_var(t3)

# any col.inear vars?
mice:::find.collinear(t3)



# impute
ppm_mice <- mice::mice(t3,  seed = 0, m = 10)

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

outlist2 <-
  row.names(ppm_mice)[ppm_mice$outflux < 0.5]
length(outlist2)

# checks
head(ppm_mice$loggedEvents, 10)
# simplify
ppm_mice

library(gFormulaMI)


colnames(ppm_mice$data)
# read data, imputed already from the pets -lost file
head(ppm_mice)



imps <- gFormulaImpute(
  data = ppm_mice,
  M = 30,
  trtVarStem = "a",
  timePoints = 2,
  trtRegimes = list(0, 1)
  #  trtRegimes = list(c(0,0), c(0,1))
)


fits <- with(imps, lm(y2_sf ~ factor(regime)))

syntheticPool(fits)



# TRY AGAIN REMOVING THE OUTCOME
#
# t3_r <- t3[, !colnames(t3) %in% "y2_sf"]
#
# t3_r
#
# # mice impute
# library(mice)
#
# library(naniar)
# naniar::gg_miss_var(t3_r)
#
# # any col.inear vars?
# mice:::find.collinear(t3_r)
#
#
#
# # impute
# ppm_mice <- mice::mice(t3_r,  seed = 0, m = 10)
#
# ppm_mice$data$y2_sf <- t3$y2_sf
#
#
# # check
# mf <- complete(ppm_mice)
# head(mf)
#
# # tests
# outlist2 <-
#   row.names(ppm_mice)[ppm_mice$outflux < 0.5]
# length(outlist2)
#
# # checks
# head(ppm_mice$loggedEvents, 10)
# # simplify
# ppm_mice
#
# library(gFormulaMI)
#
#
# colnames(ppm_mice$data)
# # read data, imputed already from the pets -lost file
# head(ppm_mice)
#
#
# imps <- gFormulaImpute(
#   data = ppm_mice,
#   M = 30,
#   trtVarStem = "a",
#   timePoints = 2,
#   trtRegimes = list(c(0, 0), c(1, 1))
# )
#
#
#
#
# fits <- with(imps, lm(y2_sf ~ factor(regime)))
#
# syntheticPool(fits)

# comparison of doubly robust method ---------------------------------------


cvars = c(
  "l0_GenCohort_t0",
  "l0_SFHEALTH_t0",
  "l0_CONSCIENTIOUSNESS_t0",
  "l0_OPENNESS_t0",
  "l0_HONESTY_HUMILITY_t0",
  "l0_EXTRAVERSION_t0" ,
  "l0_NEUROTICISM_t0" ,
  "l0_AGREEABLENESS_t0"  ,
  "a0" ,
  "l1"
)




############### NEXT SET UP VARIABLES FOR MODELS AND GRAPHS

X = "a1"
Y  = "y2_sf"

# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK

xlab = "Do you have any pets? (lost)"  ## Weekly hours devided by 10


# SET THE RANGE OF EXPOSURE
min = 0
max =  1

# set full range of X
x =  min:max
x


# range for some graphs
minmax <- paste(c(x), sep = ",")

# baseline condition
r = 0

# focal contrast for X
f = 1

# REQUIRED for certain model model functions
c = x

# contrast for graphs -- absolute distance from baseline
p = c(r, f) #


# Needed for E-VALUES -- how much do we move on the X scale to obtain our effect?

delta = abs(r - f)

ylim = c(-.5, .5)  # SET AS YOU LIKE -- here, how much movement across a standard deviation unit of the outcome
ylim_contrast = c(0, 3)  # SET AS YOU LIKE (FOR CONTRASTS )

# mice imputed data
## THIS IS KEY, NAME THE DATA I GAVE YOU "DF"

# n imputations
m = 10

# standard deviation of the outcome (for evalues)
# We have stanadardised the (non-binary) outcomes for comparable effect sizes.
sd = 1


family = "gaussian"

# set data
df = ppm_mice

# match -------------------------------------------------------------------
library(MatchThem)
library(optmatch)
library(MatchIt)
library(WeightIt)
library(cobalt)



# church-use R
# set digits = 3
options(scipen = 999)




# WEIGHTS

match_pets <- weightthem(
  as.formula(paste(as.formula(paste(
    paste("a1", "~",
          paste(cvars, collapse = "+"))
  )))),
  df,
  approach = "within",
  estimand = "ATT",
  stabilize = TRUE,
  method = "ebal"
)


sum <- summary(match_pets)
plot(sum)
sum
bal.tab(match_pets)


length(unique(mlf$id))




#Extracting the original dataset with missing value
maindataset <- complete(match_pets, action = 0)
#Some spit-and-polish
maindataset <-
  data.frame(.imp = 0, .id = seq_len(nrow(maindataset)), maindataset)

#Extracting imputed-weighted datasets in the long format
alldataset  <- complete(match_pets, action = "long")

#Binding them together
alldataset  <- rbind(maindataset, alldataset)

#Converting to .mids
newmids <- as.mids(alldataset)




# NOAH'S WAY

fits <- lapply(complete(match_pets, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "+"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})

mice::pool(fits)



# TWO WAYS TO COMUTE CAUSAL EFFECTS

library("marginaleffects")
comp.imp <- lapply(fits, function(fit) {
  avg_comparisons(
    fit,
    newdata = subset(fit$data, a1 == 1),
    variables = X,
    wts = "weights",
    vcov = "HC3"#,
    #transform_pre = "lnratioavg"
  )
})

pooled.comp <- mice::pool(comp.imp)

summary(pooled.comp, conf.int = TRUE,
        exponentiate = FALSE)



## another way -- very nice
library(clarify)
sim.imp <- misim(fits, n = 1000, vcov = "HC3")
sim.imp
summary(sim.imp)

sim.att <- sim_ame(
  sim.imp,
  var = "a1",
  subset = a1 == 1,
  cl = 8,
  verbose = FALSE
)
sim.att

sim.att <- transform(sim.att, RR = `E[Y(1)]` / `E[Y(0)]`)

sim.att
summary(sim.att, null = c(RR = 1))

sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)
summary(sim_est, null = c(`RD` = 0))
plot(sim_est)


# first method ------------------------------------------------------------
# impute
ppm_mice <- mice::mice(t3,  seed = 0, m = 50)
X = "a1"
Y  = "y2_sf"


# method for pooling
pool_stglm_contrast <- function(out, df, m = 50 , x, X, r) {
  nx <- length(x)
  est.all <- matrix(nrow = nx, ncol = m)
  var.all <- matrix(nrow = nx, ncol = m)
  for (i in 1:m) {
    g.comp <-
      stdGlm(
        fit = out$analyses[[i]],
        data = complete(df, i),
        X = X,
        x = x
      )
    ss <- summary(object = g.comp,
                  contrast = "difference",
                  reference = r)
    #est.all[, i] <- g.comp$est
    est.all[, i] <- ss$est.table[, "Estimate"]
    #var.all[, i] <- diag(g.comp$vcov)
    var.all[, i] <- ss$est.table[, "Std. Error"] ^ 2
  }

  #estimate
  est <- rowMeans(est.all)

  #within-variance
  W <- rowMeans(var.all)

  #between-variance
  B <- apply(X = est.all, MARGIN = 1, FUN = var)

  #total variance
  var <-
    ((1 + 1 / m) * B) + W # need to amend? Does RUBINS rule overstate variance

  #total standard error
  se <- sqrt(var)

  #confidence intervals
  ci <- cbind(est - 1.96 * se, est + 1.96 * se)

  # lower interval
  ui <- est + (1.96 * se)

  #upper interval
  li <- est - (1.96 * se)
  # row units
  row <- x
  # make data frame
  outp <- as.data.frame(cbind(row, est, se, ui, li))
  outp
}



sub = "In general, would you say your health is...\nI seem to get sick a little easier than other people.\nI expect my health to get worse."
#Y = "SFHEALTH_lead2_z"
main = "SF Health"
ylab = "SF Health (SD)"

# regression

out_m <-
  mice_generalised_lin(
    df = ppm_mice,
    X = X,
    Y = Y,
    cvars = cvars,
    family = "gaussian"
  )

summary(out_m)


X
## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = ppm_mice,
    m = 50,
    X = X,
    x = x,
    r = r
  )

out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")


sfhealth_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(
    c(f + 1 - min),
    bold = T,
    color = "white",
    background = "dodgerblue"
  ) |>
  kable_minimal(full_width = F)
# show table
sfhealth_t
# graph
sfhealth_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
sfhealth_p

# coef + estimate
sfhealth_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
sfhealth_c





# no weights --------------------------------------------------------------
dff <-
  readRDS(
    here::here(
      "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets",
      "outcomewide-pets-ml_lost"
    )
  )


sub = "In general, would you say your health is...\nI seem to get sick a little easier than other people.\nI expect my health to get worse."
#Y = "SFHEALTH_lead2_z"
main = "SF Health"
ylab = "SF Health (SD)"

# regression

out_m <-
  mice_iptw_lin(
    df = dff,
    X = X,
    Y = Y,
    cvars = cvars,
    family = "gaussian"
  )

summary(out_m)



## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")


sfhealth_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(
    c(f + 1 - min),
    bold = T,
    color = "white",
    background = "dodgerblue"
  ) |>
  kable_minimal(full_width = F)
# show table
sfhealth_t
# graph
sfhealth_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
sfhealth_p

# coef + estimate
sfhealth_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
sfhealth_c





# try with church attendance.  --------------------------------------------



dat <- arrow::read_parquet(pull_path)


# df <- df %>%
#   arrange(Id, Time) %>%
#   group_by(Id) %>%
#   mutate(Censoring = ifelse(Time == max(Time), 1, ifelse(YearMeasured[which.min(Time) + 1] == -1, 1, 0)),
#          Religion.Church2 = ifelse(Religion.Church2 == 1, 1, NA)) %>%
#   mutate(Religion.Church2[if(length(which(Religion.Church2 == 1)) == 0) seq_along(Religion.Church2) else (which(Religion.Church2 == 1)[1]):n()] <- 1,
#          Outcome = ifelse(Censoring == 0, NA, Religion.Church2))
#
# table1::table1(~ Religion.Church2 |Wave, data = df)



#
#
# dat_new <- dat %>%
#   dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0),
#                 SexualOrientation = as.factor(if_else(
#                   SexualOrientationL1 == 1,
#                   "Heterosexual",
#                   if_else(SexualOrientationL1 ==
#                             2, "Homosexual", "OtherSexuality")
#                 ))) %>%
#   dplyr::mutate(Gender3 = as.factor(ifelse(
#     GendAll == 0,
#     "Female",
#     if_else(GendAll == 1, "Male", "GenderDiverse")
#   ))) |>
#   #%>%
#   # dplyr::rename(. # did this in the man dataframe
#   #   kessler_hopeless = SWB.Kessler01,
#   #   # …  you feel hopeless?
#   #   kessler_depressed = SWB.Kessler02,
#   #   #…  you feel so depressed that nothing could cheer you up?
#   #   kessler_restless  = SWB.Kessler03,
#   #   #…  you feel restless or fidgety?
#   #   kessler_effort = SWB.Kessler04,
#   #   #…  you feel that everything was an effort?
#   #   kessler_worthless = SWB.Kessler05,
# #   #…  you feel worthless?
# #   kessler_nervous = SWB.Kessler06 #…  you feel nervous?
# # ) |>
# # dplyr::rename(
# #  # My life has a clear sense of purpose.
# #   meaning_purpose= LifeMeaning01,
# #  # I have a good sense of what makes my life meaningful.
# #   meaning_sense = LifeMeaning02,
# #   #…  you feel so depressed that nothing could cheer you up?
# #
# # ) |>
# dplyr::filter((Wave == 2015  & YearMeasured  == 1) |
#                 (Wave == 2019  &
#                    YearMeasured  == 1) |
#                 (Wave == 2020))  %>% # Eligibility criteria
#   dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
#   group_by(Id) %>%
#   dplyr::mutate(org2015 =  ifelse(Wave == 2015 &
#                                     YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
#   dplyr::mutate(hold15 = mean(org2015, na.rm = TRUE)) %>%  # Hack
#   dplyr::filter(hold15 > 0) %>% # hack to enable repeat of baseline
#   dplyr::mutate(org2019 = ifelse(Wave == 2019 &
#                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
#   dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
#   dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
#   ungroup() %>%
#   mutate(
#     Pets = factor(Pets),
#     pets = as.numeric(Pets),
#     pets_coded = as.numeric(Pets.coded.catdog_factor) - 1
#   ) |>
#   droplevels() %>%
#   arrange(Id, Wave)


###



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
  #  dplyr::mutate(cens = ifelse(dplyr::lead(YearMeasured) == 0 |
  #                                dplyr::lead(YearMeasured) == -1,
  # 0, 1) )|> # note this is strange
  ungroup() |>
  dplyr::filter((
    Wave == 2013  & YearMeasured  == 1  & !is.na(Religion.Church) |
      Wave == 2014 &
      YearMeasured  == 1 & !is.na(Religion.Church) |
      Wave == 2015 &
      YearMeasured   == 1 & !is.na(Religion.Church) |
      Wave == 2016 &
      YearMeasured   == 1 & !is.na(Religion.Church) |
      Wave == 2017 &
      YearMeasured   == 1 & !is.na(Religion.Church) |
      Wave == 2018 &
      YearMeasured   == 1 & !is.na(Religion.Church) |
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
  dplyr::filter(hold5 > 0) %>% # hack to enable repeat of baseline
  # dplyr::mutate(n_church = if_else(Wave == 2020 & YearMeasured == 1, dplyr::lag(Religion.Church), Religion.Church)) |>
  # dplyr::mutate(nn_church = if_else(Wave == 2021 & YearMeasured == 1 & is.na(Religion.Church), dplyr::lag(n_church), n_church)) |> # FIX

  ungroup() %>%
  droplevels() |>
  mutate(Time = as.numeric(Wave)) |>
  arrange(Id, Wave)

max(dp$Time)

n_unique(dp$Id) #7289

dp <- dp %>%
  arrange(Id, Time) %>%
  group_by(Id) %>%
  mutate(Censoring = ifelse(Time == max(Time), 0,
                            ifelse(
                              lead(YearMeasured) == -1 |
                                lead(YearMeasured) == 0,
                              0,
                              ifelse(is.na(lead(kessler_depressed)), 0, 1)
                            )))


table1::table1(~ Censoring + kessler_depressed |
                 Time, data = dp)


# THIS IS NOT WORKING
# conflicts_prefer(dplyr::lag)
# df <- df %>%
#   arrange( Id, Time) %>%
#   group_by(Id) %>%
#   mutate(censoring = ifelse(lead(YearMeasured) == -1 | lead(YearMeasured)== 0 | lead(YearMeasured) == NA, 0, 1))|>
#   mutate(censoring = ifelse(is.na(lead(Religion.Church)), 0, censoring)) |>
#   mutate(church_sens = ifelse(lag(censoring) == 0 & Time != 1, NA, Religion.Church))
#
#
# table1::table1( ~ church_sens +  censoring + Religion.Church |
#                   Wave, data = df)

dp <- data.frame(dp)
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
    kessler_depressed
  )  |> #,
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
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) |>
  dplyr::arrange(Id, Wave) |>
  dplyr::mutate(a_Church = ifelse(Religion.Church > 0, 1, Religion.Church)) |>
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
  # filter(censoring == 1 | (Wave == 2018) | Wave == 2019) |>
  # mutate(censoring = ifelse(Wave == 2019 & is.na(a_Church), 0, 1)) |>
  dplyr::select(-c(Religion.Church,
                   #  nn_church,
                   Wave,
                   # Censoring,
                   #   cens,
                   #   lead_church,
                   # not there
                   # Emp.WorkLifeBalance,
                   # not at baseline,
                   # Hours.Pets,
                   # pets,
                   #  pets_lead1,
                   #  Household.INC#,
                   YearMeasured)) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Time) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  rename(time = Time) |>
  mutate(time = time - 1) |>
  arrange(Id)



#table(dpp$cens)
table1::table1(~ a_Church | time, data = dpp)

table1::table1(~ Censoring + kessler_depressed |
                 time, data = dpp)

naniar::vis_miss(dpp)
length(unique(dp$Id)) # 18261

# make wider
dp_wide <- dpp |>
  pivot_wider(
    id_cols = Id,
    names_from = time,
    values_from = -c(Id, time),
    names_glue = "l{time}_{.value}",
    names_prefix = "l"
  )

colnames(dp_wide)

# library(campfin)
# test <- campfin::rename_prefix(dp_wide, suffix = c(".x", ".y"), punct = TRUE)
# colnames(test)


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
    y_depressed = l6_kessler_depressed,
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

# |>
# select(starts_with("y_"), starts_with("a"), starts_with("l")) #%>%
# filter(rowSums(!is.na(select(., starts_with("l")))) == ncol(select(., starts_with("l"))))


table(dp_w$l5_censoring)
dim(dp_wide)



# 7170
# ## TRY CENSORING
# library(gformula)
#
# # Load data
# data(mydata)

# Set up the model
# model <- gformula(Y ~ A + L1 + L2,
#                   family = binomial(),
#                   data = mydata,
#                   exposure = "A",
#                   time.points = c(0, 1))

# Fit the model and ignore missing values
#fit <- gfit(model, na.action = "na.pass")





# my_vars <- function() {
#   c(any_of(c("name", "species")), ends_with("color"))
# }
#
# my_vars <- function() {
#   c(ends_with("_t0"),a0, ends_with("_t1"),a1)
# }
# dplyr::select(starwars, my_vars())

colnames(dp_w)
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


#dp_ww2<- dp_ww[complete.cases(dp_ww[, 1:28]),]



table(is.na(dp_ww2$y_depressed))
naniar::vis_miss(dp_ww)




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

lrn_lasso <- Lrnr_glmnet$new(alpha = 1)
lrn_ridge <- Lrnr_glmnet$new(alpha = 0)
lrn_enet <- Lrnr_glmnet$new(alpha = 0.5)
lrn_bart <- Lrnr_bartMachine$new()
lrn_mean <- Lrnr_mean$new()

mars_grid_params <-
  list(# manually create a grid of MARS learners
    degree = c(2, 3),
    penalty = c(1, 2, 3))
mars_grid <-
  expand.grid(mars_grid_params, KEEP.OUT.ATTRS = FALSE)
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
  c("l0_kessler_depressed"),
  c("l1_kessler_depressed"),
  c("l2_kessler_depressed"),
  c("l3_kessler_depressed"),
  c("l4_kessler_depressed"),
  c("l5_kessler_depressed")
)

folds <- 2
k <-
  1 # we assume that data from k years previously is sufficient for the persons's covariate history
y <- "y_depressed"

# policy<- function(data, trt){
#   a
# }


no_church <- function(data, trt) {
  data[[trt]] <-
    0 # just change the entire treatment vector of interest to 0
  data[[trt]] # and return
}

all_church <- function(data, trt) {
  data[[trt]] <-
    1# just change the entire treatment vector of interest to 0
  data[[trt]] # and return
}

censor <- c("C0", "C1", "C2", "C3", "C4", "C5")

censor



length(a)
length(l)
length(censoring)
str(dp_ww)
colnames(dp_ww)
test <- dp_ww |>
  drop_na()

nrow(test)
create_node_list(
  trt = a ,
  5,
  time_vary = l,
  baseline = bs,
  k = 1
)

mm0 <- lmtp_tmle(
  # dp_ww,
  test,
  trt = a,
  outcome = y,
  baseline = bs,
  # cens = censoring,
  time_vary = l,
  outcome_type = "continuous",
  shift = no_church,
  k = k,
  learners_outcome = lrnrs,
  learners_trt = lrnrs,
  folds = folds,
  intervention_type = "static"
)

tidy(mm0)

mm0 %>% tidy()


mm0f <- lmtp_tmle(
  dp_ww,
  trt = a,
  outcome = y,
  baseline = bs,
  cens = censor,
  time_vary = l,
  outcome_type = "continuous",
  shift = no_church,
  k = k,
  learners_outcome = lrnrs,
  learners_trt = lrnrs,
  folds = folds,
  intervention_type = "static"
)

tidy(mm0f)
warnings()

mm0 %>% tidy()
dim(dp_ww)
is.na(dp_ww[, 25:29])
head(dp_ww[, 25:29])


# estimator estimate std.error conf.low conf.high
# <chr>        <dbl>     <dbl>    <dbl>     <dbl>
#   1 TMLE         0.375    0.0127    0.350     0.400
#

mm1f <- lmtp_tmle(
  dp_ww,
  #  test,
  trt = a,
  outcome = y,
  baseline = bs,
  cens = censoring,
  time_vary = l,
  outcome_type = "continuous",
  shift = all_church,
  k = k,
  learners_outcome = lrnrs,
  learners_trt = lrnrs,
  folds = folds,
  intervention_type = "static"
)

tidy(mm0)

mm1 %>% tidy()
mm0 %>% tidy()

library(tidyverse)
library(gt)
library(lmtp)


ests <-
  imap_dfr(list("No Church" = mm0, "All Church" = mm1),
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

diff <- lmtp_contrast(mm1, ref = mm0)$vals %>%
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
             estimate = "Estimated 7 year depression rate \n(95% CI)")










# t3 <-
#   toy2 |> mutate(across(ends_with("_t0"), .names = "l0_{col}")) |>
#   select(starts_with(
#     c(
#       "l0_GenCohort",
#       "l0_SFHEALTH",
#       "l0_CONSCIENTIOUSNESS",
#       "l0_OPENNESS",
#       "l0_HONESTY_HUMILITY",
#       "l0_EXTRAVERSION",
#       "l0_NEUROTICISM",
#       "l0_AGREEABLENESS"
#     )
#   ), a0, l1, a1, y2_sf)

# t3<- toy2 |> mutate(across(ends_with("_t0"), .names = "l0_{col}")) |>
# select(starts_with(c("l0_")), a0, l1, a1, y2_sf)

#
# t4<- toy2 |> mutate(across(ends_with("_t0"), .names = "l0_{col}")) |>
#     select(starts_with(c("l0_GenCohort","l0_SFHEALTH",
#                          "l0_CONSCIENTIOUSNESS",
#                          "l0_OPENNESS",
#                          "l0_HONESTY_HUMILITY",
#                          "l0_EXTRAVERSION",
#                          "l0_NEUROTICISM",
#                          "l0_AGREEABLENESS")), a0,l1, a1, y2_sf) |>
#   rename(l0_pets_baseline = "a0") |>
#   select(starts_with(c("l0_")), l1, a1, y2_sf)

# mice impute -------------------------------------------------------------


# mice impute
library(mice)
str(dp_ww)

# get test data
dp_test <-
  dp_ww |>
  slice(200:800)

colnames(dp_ww)
library(naniar)
naniar::gg_miss_var(dp_ww)

# any col.inear vars?
mice:::find.collinear(dp_ww)



# impute
ppm_mice <- mice::mice(dp_ww,  seed = 0, m = 10)

# save
saveRDS(
  ppm_mice,
  here::here(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/test",
    "church"
  )
)

# read
ppm_mice <-
  readRDS(
    here::here(
      "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/test",
      "church"
    )
  )

outlist2 <-
  row.names(ppm_mice)[ppm_mice$outflux < 0.5]
length(outlist2)

# checks
head(ppm_mice$loggedEvents, 10)
# simplify
ppm_mice

library(gFormulaMI)


colnames(ppm_mice$data)
# read data, imputed already from the pets -lost file
head(ppm_mice)
class(ppm_mice)

head(sleep)

head(mice::complete(ppm_mice))

imps <- gFormulaImpute(
  data = ppm_mice,
  M = 10,
  trtVarStem = "a",
  timePoints = 5,
  trtRegimes = list(c(0, 0, 0, 0, 0), c(1, 1, 1, 1, 1))
)

fits <- with(imps, lm(y_depressed ~ factor(regime)))

syntheticPool(fits)







## NOT WORKING TRY SOMETHING ELSE
