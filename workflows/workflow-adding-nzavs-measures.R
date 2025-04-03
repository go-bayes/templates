# test
if (!boilerplate_path_exists(unified_db$results, "grf")) {
  unified_db$results$grf <- list()
}

# initialise measures
# install from GitHub if not already installed
if (!require(boilerplate, quietly = TRUE)) {
  # install devtools if necessary
  if (!require(devtools, quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("go-bayes/boilerplate")
}
#devtools::install_github("go-bayes/boilerplate")

# initial wrangling -- INGNORE
# master_text_path = here::here("/Users/joseph/GIT/templates/databases/methods")
# master_measures_path = here::here("/Users/joseph/GIT/templates/databases/measures")
# #
# #
# # # read
# master_methods_db = margot::here_read("master_methods_db", master_text_path)
# master_measures_db = margot::here_read("measures_db", master_measures_path)
#
# # methods_db = margot::here_read("master_methods_db", master_text_path)
# # measures_db = margot::here_read("measures_db", master_measures_path)
#
# #
# # # set library path
# my_project_path <- "/Users/joseph/GIT/templates/boilerplate_data"
# #
# # # save the other data
# boilerplate_save("methods_db", category = "methods", my_project_path)
# boilerplate_save("measures_db", category = "measures", my_project_path)
#
# # Initialize databases in your custom location
# boilerplate_init(
#   categories = c("measures", "methods", "results", "discussion", "appendix", "template"),
#   data_path = my_project_path,  # specify custom path here
#   create_dirs = TRUE,
#   confirm = FALSE
# )
#
#
# # import all
# unified_db <- boilerplate_import( data_path = my_project_path)
#
# methods_db <- boilerplate_methods(unified_db)
# measures_db <- boilerplate_measures(unified_db)
# results_db <- boilerplate_results(unified_db)
# methods_db
# str(measures_db)
# str(master_measures_db)
#
# merged_db <- boilerplate_merge_databases(master_measures_db, measures_db)
#
#
# margot::here_save(merged_db, "measures_db", my_project_path)
#
#



# import data -------------------------------------------------------------
rm(unified_db)
unified_db <- boilerplate_import( data_path = my_project_path)

# ------------------------------------------------------
# sample section
# ------------------------------------------------------
sample_information <- "
### Sample

Data were collected as part of the New Zealand Attitudes and Values Study (NZAVS), an annual longitudinal national probability panel assessing New Zealand residents’ social attitudes, personality, ideology, and health outcomes. The panel began in 2009 and has since expanded to include over fifty researchers, with responses from {{n_total}} participants to date. The study operates independently of political or corporate funding and is based at a university. It employs prize draws to incentivise participation. The NZAVS tends to slightly under-sample males and individuals of Asian descent and to over-sample females and Māori (the Indigenous people of New Zealand). To enhance the representativeness of our sample population estimates for the target population of New Zealand, we apply census-based survey weights that adjust for age, gender, and ethnicity (New Zealand European, Asian, Māori, Pacific) [@sibley2021]. For more information about the NZAVS, visit: [OSF.IO/75SNB](https://doi.org/10.17605/OSF.IO/75SNB). Refer to [Appendix A](#appendix-timeline) for a histogram of daily responses for this cohort.
"

unified_db<- boilerplate_add_entry(
  db = unified_db,
  path = "methods.sample",
  value = list()
)


# or
unified_db<- boilerplate_add_entry(
    db = unified_db,
    path = "methods.sample.nzavs",
    value = sample_information
  )


# save
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)




# target population -------------------------------------------------------


# ------------------------------------------------------
# target population section
# ------------------------------------------------------
target_population <- "
### Target Population

The target population for this study comprises New Zealand residents as represented in the {{baseline_wave}} of the New Zealand Attitudes and Values Study (NZAVS) during the years {{baseline_wave}} weighted by New Zealand Census weights for age, gender, and ethnicity (refer to @sibley2021). The NZAVS is a national probability study designed to reflect the broader New Zealand population accurately. Despite its comprehensive scope, the NZAVS has some limitations in its demographic representation. Notably, it tends to under-sample males and individuals of Asian descent while over-sampling females and Māori (the indigenous peoples of New Zealand). To address these disparities and enhance the accuracy of our findings, we apply New Zealand Census survey weights to the sample data."


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.target_population",
  value = target_population
)

# save
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


# consider
# boilerplate_update_entry(
#   db = unified_db,
#   path = "methods.target_population",
#   value = target_population
# )
# boilerplate_remove_entry(
#   db = unified_db,
#   path = "methods.some_outdated_path"
# )


# ------------------------------------------------------
# causal interventions section
# ------------------------------------------------------


basic <- "#### Interventions
This study considers the following causal interventions on the exposure variable '{{exposure_var}}':

{{interventions_list}}

#### Contrasts

{{contrasts_text}}

This approach to defining interventions and contrasts allows us to systematically evaluate the causal effects of interest in our study."



unified_db<- boilerplate_add_entry(
  db = unified_db,
  path = "methods.causal_intervention",
  value = list()
)

unified_db<- boilerplate_add_entry(
  db = unified_db,
  path = "methods.causal_intervention.basic",
  value = basic
)




# complex causal interventions --------------------------------------------



### Causal Interventions
# We using the six most recent NZAVS waves (Times 10–15) because Wave 10 containes the largest cohort, enabling us to maximise power.
lmtp_multi_wave <-
"When psychologists analyse time-series data, they often use growth models to describe how variables evolve over time. However, many questions are causal: we want to know what would happen if we could intervene on certain variables (such as {{exposure_variable}}). To investigate these questions with observational time-series data, we must clearly define our causal question and design our analysis to emulate a hypothetical randomised controlled trial—often called a **target trial** [@hernan2016]. A target trial asks, setting aside practicalities and ethics, *what experiment are we attempting to emulate with our data?* Without explicitly stating this hypothetical experiment, it can be unclear which causal effect we are actually estimating.

Here, we ask:

  > 'What if, at each wave, we intervened to set {{exposure_variable}} to a certain level, and then measured everyone's outcomes at the final wave?'

To answer this, we compare two hypothetical interventions. Each intervention shifts {{exposure_variable}} across {{number_exposure_wave}} waves, with outcomes measured after the year following the final exposure wave.  A rich set of indicators covariates in the baseline wave -- the wave before the first exposure wave -- as well measurements of time-varying confounders at each exposure wave obtained there after are necessary to control for common causes of the exposures and outcomes measured at the end of study (refer to @@tbl-plan).

Following a modified treatment policies approach, we define **shift functions** describing each intervention:

1. **{{name_exposure_regime}}**

{{value_exposure_regime}}

2. **{{name_control_regime}}**

{{value_control_regime}}

::: {#tbl-plan}
```{=latex}
\\vizfive
```
We contrast outcomes from two treatment regimes: (1) {{name_exposure_regime}} (2) {{name_control_regime}}. $a^{+}$ denotes {{value_exposure_regime}}; $a^{-}$ denotes {{value_control_regime}}. Our statistical models control for baseline-wave confounders, and subsequent time-varying confounders for all exposure waves. We include baseline measurments of religious service attendance and baseline measurements of all outcomes as confounders. We assume that conditional on these confounders, treatment assignment is 'as good as random.' Outcomes, here denoted $Y_\\tau$, are measured in the wave following the final treatment.
:::

 We then organise our data to resemble a randomised sequential experiment that assigns each person to one of two longitudinal treatment strategies *{{name_exposure_regime}}* and *{{name_control_regime}}*. We define a 'confounder' as a variable that, once included in the model, along with other included variables, removes any non-causal association between the treatment and outcome. Here, as mentioned, we adjust for a rich set of demographic and personality variables, as well as baseline religious service attendance and baseline measures of all outcomes. We also adjust for time-varying confounders at each wave {{time_varying_confounders}}. We assume these time-varying confounders can influence {{exposure_variable}} and outcomes, potentially biasing our estimates. We ensure there is no reverse causation by measuring the outcomes at the end of the study, one year after the final treatment wave.


### Causal Contrasts

We compute the average expected outcome under and {{name_exposure_regime}} and {{name_control_regime}}, and then contrast these expected averages on the difference scale (i.e., subtracting the expected outcome under {{name_exposure_regime}} from that under {{name_control_regime}}. We obtain confidence intervals using the cross-fitted influence-function approach in the `lmtp` package [@williams2021]. This approach employs sequentially doubly robust (SDR) estimator, as developed by @diaz2021_non_parametric_lmtp, which remains valid if either the outcome model or the propensity model is correctly specified, thereby requiring weaker assumptions than standard approaches. By setting up our data as if it came from a hypothetical experiment, we gain clarity about which causal effects we are estimating and as well as confidence about our causal effect estimates (refer to @hernan2024WHATIF, @bulbulia2022; @bulbulia2023).

Our estimation relies on standard causal assumptions:
1.  **No unmeasured confounding**: all relevant common causes of the exposure and outcome are included in $X$.
2.  **Consistency**: an individual's observed outcome corresponds to their potential outcome under the exposure they actually received.
3.  **Positivity**: within strata defined by $X$, there is a non-zero probability of receiving either exposure level (`{{value_exposure}}` or `{{value_control}}`).

(Refer to [{{appendix_assumptions}}](#appendix-assumptions) for further details). The target population is {{name_target_population}}."



grf_causal <- "
### Causal Analysis Framework

When researchers analyse data, they often seek to understand causal relationships: what would happen if we could intervene on certain variables? To investigate such questions with observational data, we must clearly define our causal question and design our analysis to emulate a hypothetical randomised controlled trial—often called a **target trial** [@hernan2016]. A target trial asks, setting aside practicalities and ethics, *what experiment are we attempting to emulate with our data?* Without explicitly stating this hypothetical experiment, it can be unclear which causal effect we are actually estimating.

Here, we ask:

> 'What is the effect on an outcome if we set a binary exposure variable to a specific level for everyone, compared to setting it to an alternative level, considering individual characteristics?'

To answer this, we compare two hypothetical exposures:

1.  **{{name_exposure_threshold}}**: We hypothetically assign everyone to have the exposure set to `{{value_exposure}}`.
2.  **{{name_control_threshold}}**: We hypothetically assign everyone to have the exposure set to `{{value_control}}`.

Our goal is to estimate how the effect of the {{name_exposure_threshold}} exposure compared to the {{name_control_threshold}} exposure varies across individuals based on their baseline characteristics. We aim to compute the **Conditional Average Treatment Effect (CATE)**, defined as:

$$ \\tau(x) = E[Y({{value_exposure}}) - Y({{value_control}})|X = x] $$

Here, $Y({{value_exposure}})$ is the potential outcome if an individual received the {{name_exposure_threshold}} exposure, $Y({{value_control}})$ is the potential outcome if they received the {{name_control_threshold}} exposure, and $X$ represents the full set of baseline covariates measured before the exposure. The CATE, $\tau(x)$, estimates the average difference in outcomes between the two exposures for individuals with specific characteristics $x$.

To estimate this effect using observational data, we must account for **confounding**. Confounders are variables associated with both the exposure and the outcome that can distort the estimated relationship. In this study, we adjust for a rich set of baseline covariates, $X$, including demographic factors, personality traits, and baseline measures relevant to the outcome. We assume there are no time-varying confounders relevant to this cross-sectional design. By including these baseline covariates in our statistical model, we aim to remove non-causal associations between the exposure and the outcome, assuming that, conditional on $X$, the exposure is 'as good as random.'

Our estimation relies on standard causal assumptions:
1.  **No unmeasured confounding**: all relevant common causes of the exposure and outcome are included in $X$.
2.  **Consistency**: an individual's observed outcome corresponds to their potential outcome under the exposure they actually received.
3.  **Positivity**: within strata defined by $X$, there is a non-zero probability of receiving either exposure level (`{{value_exposure}}` or `{{value_control}}`).

(Refer to [{{appendix_assumptions}}](#appendix-assumptions) for further details). The target population is {{name_target_population}}.

### General Approach to Estimating Heterogeneity

Our main aim was to look beyond the average treatment effect (ATE) and explore whether the intervention’s impact varied by individual characteristics. We estimated conditional average treatment effects (CATEs) using causal forests [@grf2024], which are machine learning methods tailored for detecting treatment-effect heterogeneity.

We standardised effect directions by inverting outcomes where 'lower is better', ensuring that positive values always signify improvement. The following outcomes were inverted: {{flipped_list}}.

To reduce overfitting and distinguish real heterogeneity from noise, we split the sample 50/50. We trained the causal forest on the first half, then tested its predictions exclusively on the second half. In this held-out data, we checked calibration by comparing the mean of the predicted CATEs to the overall ATE, and we used a differential prediction test to assess whether the predicted variation was genuine [@grf2024]. We also computed the Rank-Weighted Average Treatment Effect (RATE), which estimates the benefit of targeting individuals predicted to benefit most [@grf2024; @wager2018].

Qini curves [@grf2024] were used to illustrate how a targeted allocation might outperform a uniform approach. Specifically, we compared:
1.  **Uniform Allocation**: treating or not treating everyone based on the ATE,
2.  **Targeted Allocation**: treating those with the highest predicted CATEs first.

Positive Qini values suggest that a targeted strategy is superior, helping to identify if personalisation is worthwhile and at what scale.

Finally, if we found reliable heterogeneity, we used policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] to derive simple, rule-based recommendations (e.g., ‘Treat if baseline score > X’). All heterogeneity analyses, including calibration tests, RATE, Qini curves, and policy trees, were done in R using `grf` [@grf2024], `policytree` [@policytree_package_2024], and `margot` [@margot2024]. This approach allowed us to identify individualised effects, confirm their robustness, estimate the potential value of targeting, and propose straightforward strategies for personalised treatment."

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.causal_intervention.lmtp_multi_wave",
  value = lmtp_multi_wave
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.causal_intervention.grf",
  value = grf_causal)


unified_db$methods$causal_intervention$lmtp_multi_wave

boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


# test --------------------------------------------------------------------

unified_db <- boilerplate_import( data_path = my_project_path)
unified_db$methods$sample$sample_information

# ------------------------------------------------------
# identification assumptions section
# ------------------------------------------------------

# remove current identification assumptions
# methods_db <- boilerplate_manage_text(
#   category = "methods",
#   action = "remove",
#   name = "identification_assumptions",
#   db = unified_db
# )

# add
causal_identification_criteria <- "
### Causal Identification Assumptions

This study relies on the following identification assumptions for estimating the causal effect of {{exposure_variable}}:

1. **Consistency**: the observed outcome under the observed {{exposure_variable}} is equal to the potential outcome under that exposure level. As part of consistency, we assume no interference: the potential outcomes for one individual are not affected by the {{exposure_variable}} status of other individuals.

2. **No unmeasured confounding**: all variables that affect both {{exposure_variable}} and the outcome have been measured and accounted for in the analysis.

3. **Positivity**: there is a non-zero probability of receiving each level of {{exposure_variable}} for every combination of values of {{exposure_variable}} and confounders in the population.
"


# add criteria
cat(unified_db$methods$causal_identification_criteria)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.causal_identification_criteria",
  value = causal_identification_criteria
)

cat(unified_db$methods$causal_identification_criteria)

boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


# outcome domains ---------------------------------------------------------
outcomewide_flourishing <- "
## Wellbeing Outcomes

We investigate well-being using an VanderWeele and colleague's 'outcome-wide' approach. We categorised outcomes into five domains—health, psychological well-being, present-reflective outcomes, life-reflective outcomes, and social outcomes—based on validated scales and measures. Outcomes were based on those modelled in an earlier outcome-wide paper @pedro_2024effects. @tbl-outcomes summarises each domain and its associated measures. For instance, health outcomes included BMI and hours of sleep, whereas psychological well-being included anxiety and depression. Outcomes were converted to z-scores (standardised), and the causal effect estimates may be therefore be interpreted as effect sizes.

|       Domain        |                 Dimension                |
  |---------------------|------------------------------------------------|
  |       Health        | BMI, Hours of Sleep, Hours of Exercise, Short Form Health |
  | Psychological Well-Being | Anxiety, Depression, Fatigue, Rumination      |
  | Present-Reflective  | Body Satisfaction, Forgiveness, Perfectionism, Self-Control, Self-Esteem, Sexual Satisfaction |
  | Life-Reflective     | Gratitude, Life Satisfaction, Meaning (Sense & Purpose), Personal Wellbeing Index |
  |       Social        | Social Belonging, Social Support, Neighbourhood Community |

: Outcome domains and example dimensions. Data summaries for all measures used in this study are provided in {{appendix_outcomes}}.

"

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.outcomes.outcomewide_flourishing",
  value = outcomewide_flourishing
)


boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


# ------------------------------------------------------
# confounding control section
# ------------------------------------------------------
vanderweele <- "
### Sensitivity Analysis

To manage confounding in our analysis, we implement @vanderweele2019's *modified disjunctive cause criterion* by following these steps:

1. **Identified all common causes** of both the treatment and outcomes.
2. **Excluded instrumental variables** that affect the exposure but not the outcome. Instrumental variables do not contribute to controlling confounding and can reduce the efficiency of the estimates.
3. **Included proxies for unmeasured confounders** affecting both exposure and outcome. According to the principles of d-separation @pearl2009a, using proxies allows us to control for their associated unmeasured confounders indirectly.
4. **Controlled for baseline exposure** and **baseline outcome**. Both are used as proxies for unmeasured common causes, enhancing the robustness of our causal estimates, refer to @vanderweele2020.
"


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.sensitivity_analysis.evalue",
  value = vanderweele
)


boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


# ------------------------------------------------------
# eligibility criteria section
# ------------------------------------------------------
standard <- "
### Eligibility Criteria

To be included in the analysis of this study, participants needed to participate in the {{baseline_wave}} of the study and respond to the baseline measure of {{exposure_variable}}.

Participants may have been lost to follow-up at the end of the study if they met eligibility criteria at {{baseline_wave}}. We adjusted for attrition and non-response using censoring weights, described below.

A total of {{n_participants}} individuals met these criteria and were included in the study.
"


# if (!boilerplate_path_exists(unified_db$methods, "eligibility")) {
#   unified_db$eligibility <- list()
# }

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.eligibility.standard",
  value = standard
)




# ------------------------------------------------------
# missing data handling section
# ------------------------------------------------------
missing_lmtp_simple <- "
### Missing Responses and Attrition

To mitigate bias from missing data, we used the following strategies:

**Baseline missingness**: we employed the `ppm` algorithm from the `mice` package in R [@vanbuuren2018] to impute missing baseline data (wave {{baseline_wave}}). This method allowed us to reconstruct incomplete datasets by estimating a plausible value for missing observation. Because we could only pass one data set to the lmtp, we employed single imputation. Approximately {{baseline_missing_data_proportion}}% of covariate values were missing at {{baseline_wave}}. We only used baseline data to impute baseline wave missingness (refer to @zhang2023shouldMultipleImputation).

**Outcome missingness**: to address confounding and selection bias arising from missing responses and panel attrition at the end of study {{outcome_wave}}, we applied censoring weights obtained using nonparametric machine learning ensembles afforded by the `lmtp` package (and its dependencies) in R [@williams2021]."

missing_lmtp_time_varying <- "
### Missing Responses and Attrition

To mitigate bias from missing data, we implement the following strategies:

**Baseline missingness**: We used predictive mean matching from the `mice` package [@vanbuuren2018] to impute missing baseline values (comprising `r percent_missing_baseline` of the baseline data). Following [@zhang2023shouldMultipleImputation], we performed single imputation using only baseline data. For each column with missing values, we created a binary indicator of missingness so that the machine learning algorithms we employed could condition on missingness information during estimation (see `lmtp` documentation [@williams2021]).

**Missingness in Time-Varying Variables**: When a time-varying value was missing in any wave but a future value was observed, we carried forward the previous response and included a missingness indicator. Again, this approach let the patterns of missingness inform nonparametric machine learning. If no future value was observed, we considered the participant censored and used inverse probability of treatment weights to address attrition.

**Outcome missingness**: to address confounding and selection bias arising from missing responses and panel attrition at the end of study {{outcome_wave}}, we applied censoring weights obtained using nonparametric machine learning ensembles afforded by the `lmtp` package (and its dependencies) in R [@williams2021]."


missing_grf_simple <- "
### Missing Responses and Attrition

The GRF package accepts missing values at baseline. To obtain valid inference for missing responses we computed inverse probability of censoring weights for censoring of the exposure, given that systematic censoring following the baseline wave may lead to selection bias that limit generalistion to the baseline target population [@bulbulia2024wierd]. See Appendix {{grf_appendix}}."


# make category
# unified_db$methods$missing_data <- list()

# if (!boilerplate_path_exists(unified_db, "methods.statistical_models")) {
#   boilerplate_add_entry(
#     db = unified_db,
#     path = "methods.statistical_models",
#     value = list()
#   )
# }


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.missing_data.missing_lmtp_simple",
  value = missing_lmtp_simple
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.missing_data.missing_lmtp_time_varying",
  value = missing_lmtp_time_varying
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.missing_data.missing_grf_simple",
  value = missing_grf_simple
)

# checks
cat(unified_db$methods$missing_data$missing_lmtp_simple)

# checks
cat(unified_db$methods$missing_data$missing_lmtp_time_varying)

# checks
cat(unified_db$methods$missing_data$missing_grf_simple)


# save progress
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)



# ------------------------------------------------------
# statistical estimator sections
# ------------------------------------------------------

# lmtp short
lmtp_short_explanation <- "
### Statistical Estimator

We estimate causal effects using the Longitudinal Modified Treatment Policy (LMTP) estimator within a Targeted Minimum Loss-based Estimation (TMLE) framework [@van2014targeted; @van2012targeted]. LMTP draws on machine learning for flexible outcome and treatment modeling and accounts for time-varying confounding [@van2014discussion; @vanderlaan2011; @vanderlaan2018]. This estimator is consistent if either the outcome model or the treatment mechanism is correct (double robustness). We use cross-validation to reduce overfitting [@bulbulia2024PRACTICAL]. We perform estimation with the `lmtp` package [@williams2021] and visualise outputs with the `margot` package [@margot2024]."

# lmtp long
lmtp_long_explanation <- "
### Statistical Estimator

We perform statistical estimation using a Targeted Minimum Loss-based Estimation (TMLE) approach, specifically the Longitudinal Modified Treatment Policy (LMTP) estimator [@van2014targeted; @van2012targeted]. TMLE is a flexible framework for causal inference that provides valid uncertainty estimates. LMTP extends TMLE to handle time-varying treatments and confounders.

**Workflow:**
1. **Initial Modeling:** We use machine learning algorithms to flexibly model relationships among treatments, covariates, and outcomes. This approach accommodates complex, high-dimensional datasets without imposing strict parametric assumptions [@van2014discussion; @vanderlaan2011; @vanderlaan2018].
2. **Targeting:** TMLE then iteratively refines (or 'targets') these initial estimates, guided by the efficient influence function to align estimates more closely with the true causal effect.

**Advantages:**
- **Double robustness:** The estimator remains consistent if either the outcome model or the treatment mechanism model is correct.
- **Time-varying structure:** LMTP handles complex longitudinal data and repeated treatments.
- **Integration with machine learning:** TMLE benefits from flexible initial fits.
- **Censoring and attrition:** LMTP accommodates missing data through inverse-probability weighting.

We use cross-validation to avoid overfitting and to improve predictive performance [@bulbulia2024PRACTICAL]. The `lmtp` package [@williams2021] implements LMTP. We rely on the `SuperLearner` framework (with base learners such as `SL.ranger`, `SL.glmnet`, and `SL.xgboost`) for initial model fitting [@polley2023; @xgboost2023; @Ranger2017; @SuperLearner2023]. We create graphs, tables, and output with the `margot` package [@margot2024]."

# sdr short
sdr_short_explanation <- "
### Sequentially Doubly Robust (SDR) Estimator

We estimate causal effects of time-varying treatment policies using a Sequential Doubly Robust (SDR) estimator with the `lmtp` package [@williams2021; @díaz2021; @hoffman2023]. SDR involves two main steps. First, flexible machine learning models capture complex relationships among treatments, covariates, and outcomes [@díaz2021]. Second, SDR targets these initial fits to refine causal effect estimates. This design is multiply robust if treatments repeat over multiple waves [@diaz2023lmtp; @hoffman2023], ensuring consistency when either the outcome or treatment model is correct. We use `SuperLearner` [@SuperLearner2023] with `SL.ranger`, `SL.glmnet`, and `SL.xgboost` [@polley2023; @xgboost2023; @Ranger2017]. We use cross-validation to reduce overfitting and improve finite-sample performance. We create graphs, tables, and output with the `margot` package [@margot2024]."


sdr_long_explanation <- "
### Sequentially Doubly Robust (SDR) Estimator

We employ a Sequentially Doubly Robust (SDR) estimator to assess the causal effects of time-varying treatment policies [@díaz2021]. SDR belongs to the broader class of doubly robust targeted learning estimators [@vanderlaan2011; @vanderlaan2018].

**Process:**
1. **Initial Modeling:** SDR uses machine learning to flexibly model relationships among treatments, covariates, and outcomes at each time point, capturing complex dependencies without strict parametric assumptions.
2. **Sequential Updating:** SDR works backwards in time, combining outcome regression and propensity models to construct unbiased estimating equations for each time interval.

**Advantages:**
- **Sequential double robustness:** The estimator remains consistent at each time point if either the outcome or treatment mechanism model is correct (but not necessarily both).
- **Time-varying confounders:** SDR naturally incorporates time-dependent structures.
- **Flexible estimation:** It accommodates non-linearities and interactions through machine learning.
- **Missingness:** The method handles attrition or loss-to-follow-up with inverse-probability weighting.

We use cross-validation to reduce overfitting and improve finite-sample performance. We implement SDR through the `lmtp` package [@williams2021; @hoffman2023; @diaz2023lmtp], relying on `SuperLearner` with base learners such as `SL.ranger`, `SL.glmnet`, and `SL.xgboost` [@polley2023; @xgboost2023; @Ranger2017; @SuperLearner2023]. For more details, see [@hoffman2022; @hoffman2023; @díaz2021]. We use the `margot` package [@margot2024] for reporting and visualisation.
"

grf_short_explanation <- "
### Statistical Estimation

We estimate heterogeneous treatment effects with Generalized Random Forests (GRF) [@grf2024]. GRF extends random forests for causal inference by focusing on conditional average treatment effects (CATE). It handles complex interactions and non-linearities without explicit model specification, and it provides 'honest' estimates by splitting data between model-fitting and inference. GRF is doubly robust because it remains consistent if either the outcome model or the propensity model is correct. We evaluate policies with the `policytree` package [@policytree_package_2024; @athey_2021_policy_tree_econometrica] and visualise results with `margot` [@margot2024]."



# make category
# unified_db$methods$statistical_models <- list()

# if (!boilerplate_path_exists(unified_db$methods, "eligibility")) {
#   unified_db$eligibility <- list()
# }

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.lmtp_short_explanation",
  value = lmtp_short_explanation
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.lmtp_long_explanation",
  value = lmtp_long_explanation
)


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.sdr_short_explanation",
  value = sdr_short_explanation
)


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.sdr_long_explanation",
  value = sdr_long_explanation
)


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.grf_short_explanation",
  value = grf_short_explanation
)


# checks
cat(unified_db$methods$statistical_models$lmtp_short_explanation)
cat(unified_db$methods$statistical_models$lmtp_long_explanation)
cat(unified_db$methods$statistical_models$sdr_short_explanation)
cat(unified_db$methods$statistical_models$sdr_long_explanation)
cat(unified_db$methods$statistical_models$grf_short_explanation)

# save progress
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)




# approach text -----------------------------------------------------------

general_approach_cate_long <- "
### Individualised Treatment Policies

Our primary goal was to move beyond average treatment effects (ATE) and assess whether the intervention's causal effects varied systematically across individuals. To do this, we estimated conditional average treatment effects (CATEs) using causal forests [@grf2024]. Causal forests are a machine learning method designed to detect heterogeneity in treatment effects across subgroups defined by covariates. Such effects help us understand for whom the treatment is most beneficial, for whom it may be less effective, and for whom it could be harmful.

For consistency across outcomes, we inverted variables where 'lower is better' so that positive treatment effects uniformly denote improvement. The following outcomes were inverted: {{flipped_list}}. This ensures that larger positive CATE estimates always indicate greater benefit.

A major concern when investigating individual differences is distinguishing genuine heterogeneity from noise. We addressed this by employing a 50/50 sample split: the first half trained the causal forest, while the second half (held-out data) was reserved for evaluation.

On the held-out data, we checked calibration [@grf2024] by comparing (i) the mean predicted CATEs against the overall ATE in that evaluation sample, and (ii) the reliability of the predicted variation. A differential prediction test provided an omnibus $p$-value indicating whether the model captured statistically significant heterogeneity in outcomes. We also calculated the Rank-Weighted Average Treatment Effect (RATE) [@grf2024; @wager2018], which measures how effectively a model-based targeting strategy might outperform a simpler 'treat all or none' approach.

We then used Qini curves [@grf2024] to visualise the potential gains of targeting individuals with higher predicted CATEs. These curves compare:
1.  **Uniform Allocation**: treating everyone (or no one) based on the ATE alone,
2.  **Targeted Allocation**: prioritising those with the highest predicted CATEs first.

The Qini curve plots cumulative gains (or losses) as the proportion of treated individuals increases. Positive Qini values suggest a personalised strategy outperforms uniform treatment, helping identify whether (and how) a targeted approach is worthwhile.

Finally, if reliable heterogeneity was detected, we used policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] to derive simple, interpretable rules (e.g., ‘Treat individuals if baseline score > X and age < Y’). These rules aim to highlight subgroups with notably different treatment responses, offering pragmatic guidance for personalised interventions.

All heterogeneity analyses—including calibration tests, RATE, Qini curves, and policy trees—were implemented in R using the `grf` [@grf2024], `policytree` [@policytree_package_2024], and `margot` [@margot2024] packages, with figures generated by `margot`. This multi-step framework allows us to estimate individualised treatment effects, verify their reliability, evaluate the added benefit of targeting, and propose straightforward decision rules if a personalised approach is justified by the data.
"

general_approach_cate_short <- "
### Individualised Treatment Policies

Our main aim was to look beyond the average treatment effect (ATE) and explore whether the intervention’s impact varied by individual characteristics. We estimated conditional average treatment effects (CATEs) using causal forests [@grf2024], which are machine learning methods tailored for detecting treatment-effect heterogeneity.

We standardised effect directions by inverting outcomes where 'lower is better', ensuring that positive values always signify improvement. The following outcomes were inverted: {{flipped_list}}.

To reduce overfitting and distinguish real heterogeneity from noise, we split the sample 50/50. We trained the causal forest on the first half, then tested its predictions exclusively on the second half. In this held-out data, we checked calibration by comparing the mean of the predicted CATEs to the overall ATE, and we used a differential prediction test to assess whether the predicted variation was genuine [@grf2024]. We also computed the Rank-Weighted Average Treatment Effect (RATE), which estimates the benefit of targeting individuals predicted to benefit most [@grf2024; @wager2018].

Qini curves [@grf2024] were used to illustrate how a targeted allocation might outperform a uniform approach. Specifically, we compared:
1.  **Uniform Allocation**: treating or not treating everyone based on the ATE,
2.  **Targeted Allocation**: treating those with the highest predicted CATEs first.

Positive Qini values suggest that a targeted strategy is superior, helping to identify if personalisation is worthwhile and at what scale.

Finally, if we found reliable heterogeneity, we used policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] to derive simple, rule-based recommendations (e.g., ‘Treat if baseline score > X’). All heterogeneity analyses, including calibration tests, RATE, Qini curves, and policy trees, were done in R using `grf` [@grf2024], `policytree` [@policytree_package_2024], and `margot` [@margot2024]. This approach allowed us to identify individualised effects, confirm their robustness, estimate the potential value of targeting, and propose straightforward strategies for personalised treatment."


general_approach_cate_long_no_flip <- "
### Individualised Treatment Policies

Our primary goal was to move beyond average treatment effects (ATE) and assess whether the intervention's causal effects varied systematically across individuals. To do this, we estimated conditional average treatment effects (CATEs) using causal forests [@grf2024]. Causal forests are a machine learning method designed to detect heterogeneity in treatment effects across subgroups defined by covariates. Such effects help us understand for whom the treatment is most beneficial, for whom it may be less effective, and for whom it could be harmful.

A major concern when investigating individual differences is distinguishing genuine heterogeneity from noise. We addressed this by employing a 50/50 sample split: the first half trained the causal forest, while the second half (held-out data) was reserved for evaluation.

On the held-out data, we checked calibration [@grf2024] by comparing (i) the mean predicted CATEs against the overall ATE in that evaluation sample, and (ii) the reliability of the predicted variation. A differential prediction test provided an omnibus $p$-value indicating whether the model captured statistically significant heterogeneity in outcomes. We also calculated the Rank-Weighted Average Treatment Effect (RATE) [@grf2024; @wager2018], which measures how effectively a model-based targeting strategy might outperform a simpler 'treat all or none' approach.

We then used Qini curves [@grf2024] to visualise the potential gains of targeting individuals with higher predicted CATEs. These curves compare:
1.  **Uniform Allocation**: treating everyone (or no one) based on the ATE alone,
2.  **Targeted Allocation**: prioritising those with the highest predicted CATEs first.

The Qini curve plots cumulative gains (or losses) as the proportion of treated individuals increases. Positive Qini values suggest a personalised strategy outperforms uniform treatment, helping identify whether (and how) a targeted approach is worthwhile.

Finally, if reliable heterogeneity was detected, we used policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] to derive simple, interpretable rules (e.g., ‘Treat individuals if baseline score > X and age < Y’). These rules aim to highlight subgroups with notably different treatment responses, offering pragmatic guidance for personalised interventions.

All heterogeneity analyses—including calibration tests, RATE, Qini curves, and policy trees—were implemented in R using the `grf` [@grf2024], `policytree` [@policytree_package_2024], and `margot` [@margot2024] packages, with figures generated by `margot`. This multi-step framework allows us to estimate individualised treatment effects, verify their reliability, evaluate the added benefit of targeting, and propose straightforward decision rules if a personalised approach is justified by the data.
"

general_approach_cate_short_no_flip <- "
### Individualised Treatment Policies

Our main aim was to look beyond the average treatment effect (ATE) and explore whether the intervention’s impact varied by individual characteristics. We estimated conditional average treatment effects (CATEs) using causal forests [@grf2024], which are machine learning methods tailored for detecting treatment-effect heterogeneity.

To reduce overfitting and distinguish real heterogeneity from noise, we split the sample 50/50. We trained the causal forest on the first half, then tested its predictions exclusively on the second half. In this held-out data, we checked calibration by comparing the mean of the predicted CATEs to the overall ATE, and we used a differential prediction test to assess whether the predicted variation was genuine [@grf2024]. We also computed the Rank-Weighted Average Treatment Effect (RATE), which estimates the benefit of targeting individuals predicted to benefit most [@grf2024; @wager2018].

Qini curves [@grf2024] were used to illustrate how a targeted allocation might outperform a uniform approach. Specifically, we compared:
1.  **Uniform Allocation**: treating or not treating everyone based on the ATE,
2.  **Targeted Allocation**: treating those with the highest predicted CATEs first.

Positive Qini values suggest that a targeted strategy is superior, helping to identify if personalisation is worthwhile and at what scale.

Finally, if we found reliable heterogeneity, we used policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] to derive simple, rule-based recommendations (e.g., ‘Treat if baseline score > X’). All heterogeneity analyses, including calibration tests, RATE, Qini curves, and policy trees, were done in R using `grf` [@grf2024], `policytree` [@policytree_package_2024], and `margot` [@margot2024]. This approach allowed us to identify individualised effects, confirm their robustness, estimate the potential value of targeting, and propose straightforward strategies for personalised treatment."

simple_general_approach_cate_long <- "
### Individualised Treatment Policies

In simple terms, we wanted to see if a treatment works differently for different people, rather than just figuring out one overall benefit for everyone. We used a method called 'causal forests' (think of it like a special type of decision tree) to find which groups of people might benefit the most, and which groups might benefit the least.

Because some of our measures work in opposite directions ('lower is better', e.g. depression), we flipped those measures so that 'higher' always means 'better.' That way, if we talk about a 'positive effect,' it consistently means the person improved.

To avoid fooling ourselves by spotting fake patterns in the data, we split our sample in half. We used the first half to teach our causal forest model what to look for, then tested it on the second half—so we only trust what shows up in that new data, where the model wasn’t allowed to 'peek.'

We tested whether the model’s predictions were sensible in two ways:
1.  **Do the average predictions match the overall effect?** This checks if the model's idea of an 'average effect' fits what we actually see in the second half of the data.
2.  **Is there real variety in who benefits most?** The model estimates different effects for different people, but we need to confirm that these differences aren not just random noise.

We also used two extra tools:
- **Qini curves**: these plots show if focusing on the people who the model says would benefit most actually leads to better outcomes than just treating (or not treating) everyone.
- **Policy trees**: these are straightforward 'if-then' rules (e.g., 'If a person’s baseline score is above X, recommend treatment'). Qini curves give more insight than 'black box' machine learning models, and can help guide decisions in real-world practice.

Putting it all together, this step-by-step approach lets us investigate whether treatments should be applied the same way for everyone, or whether we can do better by matching treatments to the people who stand to benefit the most (refer to appendix {{appendix_analytic_approach}}."

simple_general_approach_cate_short <- "
### Individualised Treatment Policies

We used a method called 'causal forests' to check if a treatment helped some people more than others. After flipping any scales so that ‘higher’ always means ‘better,’ we taught the model on half the data and tested it on the other half. This helped us see whether the differences we found were real rather than accidental. We then compared outcomes when we targeted treatment to those predicted to benefit the most (using Qini curves) against simply giving the treatment to everyone. Finally, we used policy trees to boil down these results into simple, if-then rules for deciding who’s likely to benefit most from the treatment (refer to appendix {{appendix_analytic_approach}}."


simple_general_approach_cate_long_no_flip <- "
### Individualised Treatment Policies

In simple terms, we wanted to see if a treatment works differently for different people, rather than just figuring out one overall benefit for everyone. We used a method called 'causal forests' (think of it like a special type of decision tree) to find which groups of people might benefit the most, and which groups might benefit the least.

To avoid fooling ourselves by spotting fake patterns in the data, we split our sample in half. We used the first half to teach our causal forest model what to look for, then tested it on the second half—so we only trust what shows up in that new data, where the model wasn’t allowed to 'peek.'

We tested whether the model’s predictions were sensible in two ways:
1.  **Do the average predictions match the overall effect?** This checks if the model's idea of an 'average effect' fits what we actually see in the second half of the data.
2.  **Is there real variety in who benefits most?** The model estimates different effects for different people, but we need to confirm that these differences are not just random noise.

We also used two extra tools:
- **Qini curves**: these plots show if focusing on the people who the model says would benefit most actually leads to better outcomes than just treating (or not treating) everyone.
- **Policy trees**: these are straightforward 'if-then' rules (e.g., 'If a person’s baseline score is above X, recommend treatment'). Qini curves give more insight than 'black box' machine learning models, and can help guide decisions in real-world practice.

Putting it all together, this step-by-step approach lets us investigate whether treatments should be applied the same way for everyone, or whether we can do better by matching treatments to the people who stand to benefit the most (refer to appendix {{appendix_analytic_approach}}."

simple_general_approach_cate_short_no_flip <- "
### Individualised Treatment Policies

We used a method called 'causal forests' to check if a treatment helped some people more than others. We trained the model on half the data and tested it on the other half. This helped us understand whether the differences we found were real rather than accidental. We then compared outcomes when we targeted treatment to those predicted to benefit the most (using Qini curves) against simply giving the treatment to everyone. Finally, we used policy trees to boil down these results into simple, if-then rules for deciding who's likely to benefit most from the treatment (refer to appendix {{appendix_analytic_approach}}."


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.general_approach_cate_long",
  value = general_approach_cate_long
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.general_approach_cate_short",
  value = general_approach_cate_short
)


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.general_approach_cate_long_no_flip",
  value = general_approach_cate_long_no_flip
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.general_approach_cate_short_no_flip",
  value = general_approach_cate_short_no_flip
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.simple_general_approach_cate_long",
  value = simple_general_approach_cate_long
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.simple_general_approach_cate_short",
  value = simple_general_approach_cate_short
)


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.simple_general_approach_cate_long_no_flip",
  value = simple_general_approach_cate_long_no_flip
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.simple_general_approach_cate_short_no_flip",
  value = simple_general_approach_cate_short_no_flip
)

# checks
cat(unified_db$methods$analytic_approach$general_approach_cate_long)
cat(unified_db$methods$analytic_approach$general_approach_cate_short)
cat(unified_db$methods$analytic_approach$general_approach_cate_long_no_flip)
cat(unified_db$methods$analytic_approach$general_approach_cate_short_no_flip)

cat(unified_db$methods$analytic_approach$simple_general_approach_cate_long)
cat(unified_db$methods$analytic_approach$simple_general_approach_cate_short)

cat(unified_db$methods$analytic_approach$simple_general_approach_cate_long_no_flip)
cat(unified_db$methods$analytic_approach$simple_general_approach_cate_short_no_flip)

# # save progress
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


# sensitivity analysis ----------------------------------------------------
unified_db$methods$sensitivity_analysis$evalue
# add a new custom text entry
short_evalue <- "
### Sensitivity Analysis

We perform sensitivity analyses using the E-value metric [@vanderweele2017; @linden2020EVALUE]. The E-value represents the minimum association strength (on the risk ratio scale) that an unmeasured confounder would need to have with both the exposure and outcome—after adjusting for measured covariates—to explain away the observed exposure-outcome association [@vanderweele2020; @linden2020EVALUE]."


unified_db <- boilerplate_add_entry(
  db = unified_db,
  path = "methods.sensitivity_analysis.short_evalue",
  value = short_evalue)


# yes save
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)



# cleaner protocol --------------------------------------------------------
#
#
# # First, import the database
# unified_db <- boilerplate_import(data_path = my_project_path)
# unified_db$methods$causal_identification_criteria
#
# # Causal identification criteria
# unified_db<- boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.causal_identification_criteria",
#   value = causal_identification_criteria
# )
#
# # Confounding control - create structure and add vanderweele
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.confounding_control",
#   value = list()
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.confounding_control.vanderweele",
#   value = vanderweele
# )
#
# # Eligibility
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.eligibility",
#   value = list()
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.eligibility.standard",
#   value = standard
# )
#
# # Missing data - first check if it exists already
# if (!boilerplate_path_exists(unified_db, "methods.missing_data")) {
#   boilerplate_add_entry(
#     db = unified_db,
#     path = "methods.missing_data",
#     value = list()
#   )
# }
# # Add the missing data entries
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.missing_data.missing_lmtp_simple",
#   value = missing_lmtp_simple
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.missing_data.missing_lmtp_time_varying",
#   value = missing_lmtp_time_varying
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.missing_data.missing_grf_simple",
#   value = missing_grf_simple
# )
#
# # Statistical models
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.statistical_models",
#   value = list()
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.statistical_models.lmtp_short_explanation",
#   value = lmtp_short_explanation
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.statistical_models.lmtp_long_explanation",
#   value = lmtp_long_explanation
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.statistical_models.sdr_short_explanation",
#   value = sdr_short_explanation
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.statistical_models.sdr_long_explanation",
#   value = sdr_long_explanation
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.statistical_models.grf_short_explanation",
#   value = grf_short_explanation
# )
#
# # Analytic approach
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.analytic_approach",
#   value = list()
# )
# # Add the analytic approach entries
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.analytic_approach.general_approach_cate_long",
#   value = general_approach_cate_long
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.analytic_approach.general_approach_cate_short",
#   value = general_approach_cate_short
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.analytic_approach.general_approach_cate_long_no_flip",
#   value = general_approach_cate_long_no_flip
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.analytic_approach.general_approach_cate_short_no_flip",
#   value = general_approach_cate_short_no_flip
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.analytic_approach.simple_general_approach_cate_long",
#   value = simple_general_approach_cate_long
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.analytic_approach.simple_general_approach_cate_short",
#   value = simple_general_approach_cate_short
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.analytic_approach.simple_general_approach_cate_long_no_flip",
#   value = simple_general_approach_cate_long_no_flip
# )
# boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.analytic_approach.simple_general_approach_cate_short_no_flip",
#   value = simple_general_approach_cate_short_no_flip
# )
#
# # Finally, save the database
# boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)
#
# # To verify the changes
# unified_db <- boilerplate_import(data_path = my_project_path)
# # Check one entry to verify it's been saved correctly
# cat(boilerplate_get_entry(unified_db, "methods.missing_data.missing_lmtp_simple"))



# results -----------------------------------------------------------------

interpretation_ominibus_test_negative <-  "
#### Omnibus Test

The omnibus test indicates that the model found real differences in how individuals respond to the treatment. In other words, some people appear to benefit more than others, which justifies exploring targeted approaches or subgroups in more detail. (Note: for any outcomes we flipped, a 'higher' effect still means greater improvement.) Here, the omnibus test did not provide statistically reliable evidence for overall treatment effect heterogeneity based on the examined covariates [@grf2024]. This suggests that, across all individuals and covariates considered, the model did not reliably detect variation beyond chance according to this specific test. However, omnibus tests can lack power for detecting subtle or localised heterogeneity. Therefore, we examined more specific indicators of potential targeting benefits and subgroup differences."

interpretation_ominibus_test_positive <- "
#### Omnibus Test

The omnibus test indicates that the model found real differences in how individuals respond to the treatment. In other words, some people appear to benefit more than others, which justifies exploring targeted approaches or subgroups in more detail. (Note: for any outcomes we flipped, a ‘higher’ effect still means greater improvement.)"


interpretation_rate <- "
#### Rate Test

The RATE tells us how much better we could do by offering treatment first to those who are predicted to benefit most, rather than treating everyone the same. A higher RATE suggests that targeting people with higher predicted benefits (where ‘higher’ always means better, due to flipping some outcomes) can lead to better overall results."


interpretation_rate_no_flip <- "
#### Rate Test

The RATE tells us how much better we could do by offering treatment first to those who are predicted to benefit most, rather than treating everyone the same. A higher RATE suggests that targeting people with higher predicted benefits can lead to better overall results."

interpretation_qini <- "
#### Qini Curves

The Qini curve helps us understand whether focusing on the people predicted to benefit the most actually leads to better overall outcomes than treating everyone (or no one). We start with those who are forecast to benefit the most, then gradually include more people. If the Qini curve remains above the line for treating everyone equally, it implies a targeted approach is likely beneficial."


interpretation_policy_tree <- "
#### Policy Trees

We used policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] to find straightforward ‘if-then’ rules for who benefits most from treatment, based on participant characteristics. Because we flipped some measures, a higher predicted effect always means greater improvement. Policy trees can uncover small but important subgroups whose treatment responses stand out, even when the overall differences might be modest."

# boilerplate_remove_entry(
#   db = unified_db,
#   path = "results.results"
# )

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "results.grf.interpretation_ominibus_test_negative",
  value = interpretation_ominibus_test_negative
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "results.grf.interpretation_ominibus_test_positive",
  value = interpretation_ominibus_test_positive
)
unified_db$results$grf$interpretation_ominibus_test_positive

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "results.grf.interpretation_rate",
  value = interpretation_rate
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "results.grf.interpretation_qini",
  value = interpretation_qini
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "results.grf.interpretation_policy_tree",
  value = interpretation_policy_tree
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "results.grf.interpretation_rate_no_flip",
  value = interpretation_rate_no_flip
)


# Finally, save the database
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)

# To verify the changes
unified_db <- boilerplate_import(data_path = my_project_path)
# Check one entry to verify it's been saved correctly
cat(boilerplate_get_entry(unified_db, "methods.missing_data.missing_lmtp_simple"))




# Discussion --------------------------------------------------------------

strengths_grf_long <- "
### Strengths and Limitations of Our Approach

We used causal forests [@grf2024]—a statistical method designed to estimate Conditional Average Treatment Effects (CATEs)—to investigate how treatment effectiveness might vary across individuals with different characteristics. This strategy offers several advantages over simpler regression models, yet it also comes with important caveats.

#### Potential Limitations

First, our results depend heavily on having measured all key variables that affect both who receives treatment and how strongly they respond. If important factors are missing, the estimated differences in treatment effects may be biased. Selecting which characteristics to include in the model is also critical: omitting relevant factors or including inappropriate ones can yield misleading conclusions. Any further analyses that rely on these estimates—such as deciding whom to treat first—will inherit the same potential biases.

Second, interpreting subgroup effects can be challenging when many characteristics are considered simultaneously. Because these estimates are conditional on all factors in the model, a statistically significant finding for one subgroup may not necessarily hold when additional characteristics vary. Moreover, even if the model detects genuine differences, small or niche effects might not translate into meaningful real-world improvements.

#### Strengths

Despite these considerations, our approach provides a powerful way to uncover and potentially leverage treatment effect heterogeneity. Causal forests [@grf2024] do not assume a simple linear or additive structure, enabling them to detect complex interactions among diverse covariates. To ensure the findings are not just statistical artifacts, we used a 50/50 sample split: one half of the data to build the model and the other to test its predictions on unseen data. This guards against overfitting and gives a more reliable indication of how well the model generalises.

We also explicitly assess the reliability and practical value of the predicted differences in treatment effects. We perform statistical checks—such as calibration and differential prediction tests [@grf2024]—to determine whether these variations are genuine rather than noise. Additionally, the Rank-Weighted Average Treatment Effect (RATE) [@grf2024; @wager2018] estimates how much we might improve outcomes if we treat only the individuals predicted to benefit most, instead of treating everyone equally. To visualise this, Qini curves [@grf2024] show how much extra benefit accumulates as we expand treatment from the top-ranked individuals to a larger portion of the population. Finally, policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] translate these insights into straightforward ‘if-then’ rules based on key baseline characteristics, making the findings more accessible for real-world decision-making.

In sum, this combination of flexible modelling, rigorous testing, and practical tools for targeting treatment offers a robust framework for studying and applying treatment effect heterogeneity. Nonetheless, these benefits hinge on the completeness of our data and the accuracy of our assumptions.
"

# Short Version
strengths_grf_short <- "
**Strengths and Limitations of Our CATE Approach**

We used causal forests [@grf2024] to estimate how treatment effects may differ for individuals with different characteristics. This method is powerful but depends on measuring all major factors influencing both treatment selection and outcomes. If key factors are missed or included incorrectly, the results can be biased. Additionally, interpreting subgroup effects can be tricky when many characteristics are involved, and statistically significant differences may not always translate into meaningful real-world gains.

Despite these concerns, causal forests offer notable advantages. They allow for flexible, non-parametric modelling [@grf2024], avoiding strict assumptions that might miss complex interactions. We used a robust evaluation method—training our model on half the data and testing it on the remaining half—to avoid overfitting. We then checked whether the predicted differences were genuine and estimated how much benefit we might gain by targeting treatment to those likely to benefit most [@grf2024; @wager2018]. Qini curves [@grf2024] let us see the overall improvement from treating the top-ranked individuals first, and policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] turn these findings into simple ‘if-then’ rules. Together, this approach provides a practical means of identifying and acting on genuine treatment effect differences."



# check entries
boilerplate_get_entry(
  db = unified_db,
  path = "discussion"
)


# add path
# if (!boilerplate_path_exists(unified_db$discussion, "strengths")) {
#   unified_db$discussion$strengths <- list()
# }

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.strengths.strengths_grf_short",
  value = strengths_grf_short
)


unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.strengths.strengths_grf_long",
  value = strengths_grf_long
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.strengths.simple_general_approach_cate_long",
  value = simple_general_approach_cate_long
)


# save
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)

# import
unified_db <- boilerplate_import(data_path = my_project_path)

-----------------------------------
# save the updated methods database
# ------------------------------------------------------
# result <- boilerplate_manage_text(
#   category = "methods",
#   action = "save",
#   db = methods_db
# )

#
#
# # check if save was successful
# if (result) {
#   cat("Methods database saved successfully!")
# } else {
#   cat("Error saving methods database!")
# }



# ------------------------------------------------------
# now test generating text from the database
# ------------------------------------------------------
unified_db <- boilerplate_import(data_path = my_data_path)

cat(unified_db$methods$analytic_approach$simple_general_approach_cate_short)
unified_db$methods$sensitivity_analysis
unified_db$methods$causal_intervention$lmtp_multi_wave
# example usage of generate_text for methods section
methods_text <- boilerplate_generate_text(
  category = "methods",
  sections = c(
    "sample.sample_information",
    "causal_intervention.lmtp_multi_wave",
    "causal_intervention.grf",
    "target_population",
    "eligibility.standard",
    "causal_identification_criteria",
    "confounding_control.vanderweele",
    "sensitivity_analysis.evalue",
    "missing_data.missing_grf_simple",
    "analytic_approach.general_approach_cate_long"
  ),
  global_vars = list(
    exposure_variable = "Religious Service Attendance",
    n_total = 77000,
    n_participants = 48900,
    value_exposure_regime = "Regular Religious Service Attendance",
    value_treatment_regime = "No Religious Service Attendance",
    value_exposure_regime = " At each wave, if attendance is below four times per month, we shift it to four; otherwise, we leave it unchanged.",
    value_control_regime = "At each wave, if attendance is above zero, we shift it to zero; otherwise, we leave it unchanged.",
    name_exposure_regime = "Regular Religious Service Attendance.",
    name_control_regime = "No Religious Service Attendance.",
    name_exposure_threshold = "Attends Religious Service",
    name_control_threshold = "No Religious Service Attendance.",
    value_exposure = "Shift everyone in the target population to \\> 0 religious service attendance",
    value_control = "Shift everyone in the target population to 0 religious service attendance",
    name_target_population = "all adults in New Zealand from the years 2018-2024",
    number_exposure_wave = "five",
    time_varying_confounders = "(physical disability, employment status, partner status, and parenting status)",
    flipped_list = c("Neuroticism"),
    appendices_sample = "A",
    appendix_outcomes = "B",
    appendix_assumptions = "E",
    baseline_wave = "NZAVS time 10, years 2018-2019",
    exposure_wave = "NZAVS time 11, years 2019-2020",
    outcome_wave = "NZAVS time 12, years 2020-2021",
    appendix_ref = "C",
    grf_appendix = "D",
    confidence_level = "95",
    flipped_example = "Neuroticism",
    appendix_analytic_approach = "D",
    protocol_url = "https://osf.io/ce4t9/"
  ),
  db = unified_db
)

# print the generated text
cat(methods_text)


# results example ---------------------------------------------------------
unified_db$results$grf$interpretation_policy_tree

results_text <- boilerplate_generate_text(
  category = "results",
  sections = c(
    "grf.interpretation_ominibus_test_negative",
    "grf.interpretation_rate",
    "grf.interpretation_qini",
    "grf.interpretation_policy_tree"
  ),
  global_vars = list(
    exposure_var = "political_conservative"
  ),
  db = unified_db
)

cat(results_text
# example of using the boilerplate text database in a research workflow
# this demonstrates how to generate methods text for a specific study

library(tidyverse)

# ------------------------------------------------------
# define study-specific variables
# ------------------------------------------------------

# define the study parameters
exposure_var <- "political_conservative"
outcomes_health <- c("smoker_binary", "hlth_bmi", "log_hours_exercise")
outcomes_psychological <- c("hlth_fatigue", "kessler_latent_anxiety")
outcomes_social <- c("belong", "neighbourhood_community")

# combine all outcomes into a single list
all_outcomes <- list(
  health = outcomes_health,
  psychological = outcomes_psychological,
  social = outcomes_social
)

study_params <- list(
  exposure_var = exposure_var,
  n_total = 47000,
  var_name = "political_conservativism",
  appendices_sample = "A-C",
  baseline_wave = "NZAVS time 10, years 2018-2019",
  exposure_wave = "NZAVS time 11, years 2019-2020",
  outcome_wave = "NZAVS time 12, years 2020-2021",
  timeframe = "2018-2021",
  baseline_missing_data_proportion = 0.15,
  appendix_ref = "B",
  protocol_url = "https://osf.io/ce4t9/",
  causal_interventions = c("Increase political_conservative", "Do not change political_conservative"),
  contrasts = "null",
  null_intervention = "Do not change political_conservative",
  n_participants = 32451,
  population = "New Zealand Population in 2018",
  grf_appendix = "C"
)

# ------------------------------------------------------
# 1. generate the sample section
# ------------------------------------------------------

sample_text <- boilerplate_generate_text(
  category = "methods",
  sections = "sample",
  global_vars = study_params,
  db= methods_db
)
cat(sample_text)
------------------------------------------------------
  # 2. generate identification assumptions section
  # ------------------------------------------------------
identification_text <- boilerplate_generate_text(
  category = "methods",
  sections = "identification_assumptions.standard",
  global_vars = study_params,
  db = methods_db
)
cat(identification_text)
# ------------------------------------------------------
# 3. generate confounding control section
# ------------------------------------------------------
confounding_text <- boilerplate_generate_text(
  category = "methods",
  sections = "confounding_control.vanderweele",
  global_vars = study_params,
  db = methods_db
)
cat(confounding_text)
# --confounding_text# ------------------------------------------------------
# 4. generate missing data handling sections for multiple estimators
# ------------------------------------------------------
# for lmtp
missing_data_lmtp <- boilerplate_generate_text(
  category = "methods",
  sections = "missing_data.lmtp",
  global_vars = study_params,
  db = methods_db
)

cat(missing_data_lmtp)

# for grf
missing_data_grf <- boilerplate_generate_text(
  category = "methods",
  sections = "missing_data.grf",
  global_vars = study_params,
  db = methods_db
)

cat(missing_data_grf)

# ------------------------------------------------------
# 5. generate statistical estimator sections for multiple estimators
# ------------------------------------------------------
# for lmtp (using the short version)
lmtp_text <- boilerplate_generate_text(
  category = "methods",
  sections = "statistical_estimator.lmtp.short",
  global_vars = study_params,
  db = methods_db
)

str(lmtp_text)

# for grf (using the short version)
grf_text <- boilerplate_generate_text(
  category = "methods",
  sections = "statistical_estimator.grf.short",
  global_vars = study_params,
  db = methods_db
)

cat(grf_text)

# ------------------------------------------------------
# 6. combine all sections to create a complete methods section
# ------------------------------------------------------
methods_text <- paste(
  "# Methods\n\n",
  sample_text, "\n\n",
  identification_text, "\n\n",
  confounding_text, "\n\n",
  "## Missing Data Handling\n\n",
  "### LMTP Estimator\n",
  missing_data_lmtp, "\n\n",
  "### GRF Estimator\n",
  missing_data_grf, "\n\n",
  "## Statistical Estimators\n\n",
  "### LMTP\n",
  lmtp_text, "\n\n",
  "### GRF\n",
  grf_text,
  sep = ""
)

# ------------------------------------------------------
# 7. alternatively, generate the entire methods section at once
# ------------------------------------------------------
complete_methods <- boilerplate_generate_text(
  category = "methods",
  sections = c(
    "sample.default",
    "identification_assumptions.standard",
    "confounding_control.vanderweele",
    "missing_data.lmtp",
    "missing_data.grf",
    "statistical_estimator.lmtp.short",
    "statistical_estimator.grf.short"
  ),
  global_vars = study_params,
  db = methods_db
)

cat(complete_methods)

# ------------------------------------------------------
# 8. write the methods section to a Quarto markdown file
# ------------------------------------------------------
# using the combined text from step 6
writeLines(methods_text, "methods_section.qmd")

# or using the complete text from step 7
writeLines(complete_methods, "methods_section_complete.qmd")

# ------------------------------------------------------
# 9. add new entries to the database for future use
# ------------------------------------------------------
# first, get the current database

# add a new custom text entry

# save the updated database
# boilerplate_manage_text(
#   category = "methods",
#   action = "save",
#   db = methods_db
# )

# ------------------------------------------------------
# 10. creating a complete methods document with the updated database
# ------------------------------------------------------
# define additional parameters for the sensitivity analysis
study_params$primary_outcome <- "kessler_latent_anxiety"
study_params$confidence_level <- 95

# generate complete methods including the new sensitivity analysis section
complete_methods_updated <- boilerplate_generate_text(
  category = "methods",
  sections = c(
    "sample.default",
    "identification_assumptions.standard",
    "confounding_control.vanderweele",
    "missing_data.lmtp",
    "statistical_estimator.lmtp.short",
    "sensitivity_analysis.evalue"  # new section added to the database
  ),
  global_vars = study_params,
  db = methods_db
)

cat(complete_methods_updated)

# write the updated methods section to a Quarto markdown file
writeLines(complete_methods_updated, "methods_section_with_sensitivity.qmd")







# enhanced boilerplate text extensions
# this script adds specialized sections to the methods database

library(tidyverse)

# get the current methods database
# methods_db <- boilerplate_manage_text(category = "methods", action = "list")

# ------------------------------------------------------
# 1. heterogeneous treatment effects section
# ------------------------------------------------------
het_effects_text <- "### Heterogeneous Treatment Effects

To investigate whether the effect of {{exposure_var}} varies across different subgroups, we employ {{heterogeneity_method}} to estimate conditional average treatment effects (CATEs). This approach allows us to identify potential effect modifiers and understand how the intervention's impact differs across the population.

Our analysis focuses on the following pre-specified potential effect modifiers:
{{effect_modifiers}}

For each identified subgroup with significant effect heterogeneity, we report:
1. The estimated conditional average treatment effect
2. The difference from the overall average treatment effect
3. Statistical significance of the heterogeneity (p-values and confidence intervals)

This heterogeneity analysis provides critical insights for tailoring interventions and understanding which subpopulations may benefit most from the {{intervention_type}} intervention."

# add heterogeneous treatment effects section
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "heterogeneous_effects.standard",
  value = het_effects_text,
  db = methods_db
)

# ------------------------------------------------------
# 2. mediation analysis section
# ------------------------------------------------------
mediation_text <- "### Causal Mediation Analysis

To understand the mechanisms through which {{exposure_var}} affects {{outcome_var}}, we conduct a formal causal mediation analysis. This analysis decomposes the total effect into:

1. **Natural Direct Effect (NDE)**: The effect of {{exposure_var}} on {{outcome_var}} that occurs through pathways other than the proposed mediator {{mediator_var}}.
2. **Natural Indirect Effect (NIE)**: The effect of {{exposure_var}} on {{outcome_var}} that operates specifically through the mediator {{mediator_var}}.

Our mediation analysis relies on the following additional identification assumptions:

1. **No mediator-outcome confounders affected by treatment**: There are no variables that (a) are affected by {{exposure_var}}, (b) confound the relationship between {{mediator_var}} and {{outcome_var}}.
2. **Cross-world counterfactual independence**: The counterfactual outcome under one exposure value and the counterfactual mediator under another exposure value are independent.

We implement this analysis using the {{mediation_method}} approach [@{{mediation_citation}}], which accommodates non-linearities in the relationships between variables and allows for exposure-mediator interactions. The proportion mediated is calculated as the ratio of the natural indirect effect to the total effect."

# add mediation analysis section
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "mediation_analysis.standard",
  value = mediation_text,
  db = methods_db
)

# ------------------------------------------------------
# 3. longitudinal data structure section
# ------------------------------------------------------
longitudinal_text <- "### Longitudinal Data Structure

This study employs a longitudinal design spanning {{n_waves}} waves of data collection over {{time_period}}.

**Wave Structure**:
- **Baseline ({{baseline_wave}})**: Collection of baseline covariates and initial measurements of {{exposure_var}} and {{outcome_var}}
- **Intervention Period ({{intervention_waves}})**: {{intervention_description}}
- **Follow-up ({{follow_up_waves}})**: Assessment of outcomes and potential mediators

This longitudinal structure allows us to establish temporal precedence, with baseline covariates measured before the exposure, and outcomes measured after the exposure. The repeated measurements enable us to:

1. Account for time-varying confounding
2. Assess the stability of treatment effects over time
3. Investigate lagged effects and potential feedback loops
4. Track changes in both exposure and outcome variables

Our analytical approach explicitly incorporates this temporal structure through {{time_varying_approach}}."

# add longitudinal data structure section
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "longitudinal_structure.standard",
  value = longitudinal_text,
  db = methods_db
)

# ------------------------------------------------------
# 4. multi-level model section
# ------------------------------------------------------
multilevel_text <- "### Hierarchical Data Structure and Multi-Level Modeling

Our data has a hierarchical structure with {{level1_units}} (level 1) nested within {{level2_units}} (level 2){{#level3_units}}, which are further nested within {{level3_units}} (level 3){{/level3_units}}. To account for this clustering and potential non-independence of observations, we employ multi-level modeling techniques.

The hierarchical structure of our data is as follows:
- **Level 1**: {{level1_description}}
- **Level 2**: {{level2_description}}
{{#level3_units}}
- **Level 3**: {{level3_description}}
{{/level3_units}}

Our multi-level models include random effects for {{random_effects_description}}. This approach allows us to:
1. Account for correlation among observations within the same cluster
2. Estimate cluster-specific effects
3. Properly partition variance at different levels of analysis
4. Generate appropriate standard errors that account for clustering

All analyses are implemented using the {{multilevel_package}} package in R, with maximum likelihood estimation."

# add multi-level model section
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "multilevel_modeling.standard",
  value = multilevel_text,
  db = methods_db
)

# ------------------------------------------------------
# 5. Bayesian analysis section
# ------------------------------------------------------
bayesian_text <- "### Bayesian Statistical Approach

We employ a Bayesian framework for our statistical analysis, which offers several advantages for causal inference:

1. **Prior Information Integration**: We incorporate existing knowledge through informative priors based on {{prior_source}}. Specifically, we use {{prior_distribution}} priors for {{parameter_description}}.

2. **Full Posterior Distributions**: Rather than point estimates, we report entire posterior distributions for causal effects, providing a more nuanced understanding of uncertainty.

3. **Probabilistic Interpretation**: Results are presented as posterior probabilities and credible intervals, which have more intuitive interpretations than frequentist confidence intervals.

4. **Missing Data Handling**: The Bayesian approach naturally accommodates missing data through joint modeling of the complete data distribution.

We implement our models using {{bayesian_software}}, with {{mcmc_description}} to sample from the posterior distributions. Convergence is assessed through visual inspection of trace plots and the Gelman-Rubin diagnostic (R̂). We report posterior means, standard deviations, and {{credible_interval}}% credible intervals for all parameters of interest."

# add Bayesian analysis section
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "bayesian_approach.standard",
  value = bayesian_text,
  db = methods_db
)

# ------------------------------------------------------
# 6. save the updated methods database
# ------------------------------------------------------
result <- boilerplate_manage_text(
  category = "methods",
  action = "save",
  db = methods_db
)
result
# check if save was successful
if (result) {
  cat("Methods database with extensions saved successfully!")
} else {
  cat("Error saving methods database!")
}

# ------------------------------------------------------
# 7. example usage with advanced sections
# ------------------------------------------------------

# define additional parameters for advanced sections
advanced_params <- list(
  exposure_var = "cognitive_behavioral_therapy",
  outcome_var = "depression_score",
  mediator_var = "negative_automatic_thoughts",
  heterogeneity_method = "Causal Random Forests (CRF)",
  effect_modifiers = "age, gender, baseline depression severity, and comorbid anxiety",
  intervention_type = "cognitive-behavioral",
  mediation_method = "interventional effects",
  mediation_citation = "vanderweele2014",
  n_waves = 4,
  n_total = "47,940",
  time_period = "12 months",
  baseline_wave = "Month 0",
  intervention_waves = "Months 1-3",
  intervention_description = "Administration of the cognitive-behavioral therapy intervention",
  follow_up_waves = "Months 6 and 12",
  time_varying_approach = "marginal structural models",
  level1_units = "patients",
  level2_units = "clinicians",
  level3_units = "clinics",
  level1_description = "Individual patients receiving treatment (n = 1240)",
  level2_description = "Therapists providing treatment"
  }


# creating and using audience-specific method descriptions
library(tidyverse)

# get the current methods database
methods_db <- boilerplate_manage_text(category = "methods", action = "list")

# ------------------------------------------------------
# add audience-specific lmtp descriptions
# ------------------------------------------------------

# technical audience (statisticians/methodologists)
technical_lmtp_text <- "We estimate causal effects using the Longitudinal Modified Treatment Policy (LMTP) estimator within a Targeted Minimum Loss-based Estimation (TMLE) framework [@van2014targeted; @van2012targeted]. This semi-parametric estimator leverages the efficient influence function (EIF) to achieve double robustness and asymptotic efficiency. LMTP employs flexible machine learning for initial estimates of outcome regressions and treatment mechanisms, then updates these estimates through a targeted bias correction step that solves the efficient influence curve equation. The estimator remains consistent if either the outcome model or the treatment mechanism is correctly specified, providing dual protection against model misspecification. Implementation utilizes cross-validation to minimize overfitting risk [@bulbulia2024PRACTICAL] and incorporates the highly adaptive lasso (HAL) for complex functional forms. We rely on the `lmtp` package [@williams2021] with the `SuperLearner` ensemble framework [@polley2023] for implementation."

# applied researchers audience
applied_lmtp_text <- "We estimate causal effects using the Longitudinal Modified Treatment Policy (LMTP) estimator [@van2014targeted; @van2012targeted]. This approach combines machine learning with causal inference methods to estimate treatment effects while avoiding strict parametric assumptions. The estimator has a property called 'double robustness,' meaning it produces valid results if either our models for the outcome or for treatment assignment are correct. This provides additional protection against model misspecification compared to standard regression methods. We implement the approach using the `lmtp` package in R [@williams2021], with machine learning algorithms to flexibly capture relationships in the data. Cross-validation is employed to reduce overfitting [@bulbulia2024PRACTICAL]."

# general audience/policymakers
general_lmtp_text <- "We used advanced statistical methods that account for multiple factors that might influence both {{exposure_var}} and {{outcome_var}}. Our approach uses recent developments in causal inference that provide more reliable estimates than traditional statistical methods. This method helps us distinguish between mere association and actual causal effects by carefully accounting for other variables that might explain the relationship. Our analysis incorporates machine learning techniques that can detect complex patterns in the data without making overly simplistic assumptions about how variables relate to each other."

# add audience-specific entries to database
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "statistical_estimator.lmtp.technical_audience",
  value = technical_lmtp_text,
  db = methods_db
)

methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "statistical_estimator.lmtp.applied_audience",
  value = applied_lmtp_text,
  db = methods_db
)

methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "statistical_estimator.lmtp.general_audience",
  value = general_lmtp_text,
  db = methods_db
)

# save the updated database
boilerplate_manage_text(
  category = "methods",
  action = "save",
  db = methods_db
)

# ------------------------------------------------------
# usage example: generating audience-specific reports
# ------------------------------------------------------

# define study parameters
study_params <- list(
  exposure_var = "political_conservative",
  outcome_var = "social_wellbeing",
  n_total = 47000,
  baseline_wave = "NZAVS time 10, years 2018-2019",
  exposure_wave = "NZAVS time 11, years 2019-2020",
  outcome_wave = "NZAVS time 12, years 2020-2021"
)

# function to generate methods text for different audiences
generate_methods_by_audience <- function(audience = c("technical", "applied", "general")) {
  audience <- match.arg(audience)

  # select appropriate paths based on audience
  if (audience == "technical") {
    lmtp_path <- "statistical_estimator.lmtp.technical_audience"
  } else if (audience == "applied") {
    lmtp_path <- "statistical_estimator.lmtp.applied_audience"
  } else {
    lmtp_path <- "statistical_estimator.lmtp.general_audience"
  }

  # generate text
  boilerplate_generate_text(
    category = "methods",
    sections = c(
      "sample.nzavs",
      lmtp_path
    ),
    global_vars = study_params
  )
}

# generate reports for different audiences
technical_report <- generate_methods_by_audience("technical")
applied_report <- generate_methods_by_audience("applied")
general_report <- generate_methods_by_audience("general")

# write outputs to files
writeLines(technical_report, "methods_technical_audience.qmd")
writeLines(applied_report, "methods_applied_audience.qmd")
writeLines(general_report, "methods_general_audience.qmd")


# creating document templates that incorporate boilerplate text sections
library(tidyverse)
library(glue)

# get the current methods database
methods_db <- boilerplate_manage_text(category = "methods", action = "list")

# ------------------------------------------------------
# create document templates for different formats
# ------------------------------------------------------

# typical journal article template
journal_article_template <- "---
  title: \"{{title}}\"
author: \"{{authors}}\"
date: \"{{date}}\"
format:
  docx:
    reference-doc: journal_template.docx
bibliography: references.bib
---

# Abstract

{{abstract}}

# Introduction

{{introduction}}

# Methods

## Sample
{{methods.sample}}

## Measures
{{methods.measures}}

## Identification Assumptions
{{methods.identification_assumptions}}

## Confounding Control
{{methods.confounding_control}}

## Statistical Approach
{{methods.statistical_approach}}

# Results

{{results}}

# Discussion

{{discussion}}

# References
"

# add to database
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "document_templates.journal_article",
  value = journal_article_template,
  db = methods_db
)

# conference presentation template
conference_template <- "---
title: \"{{title}}\"
author: \"{{authors}}\"
date: \"{{date}}\"
format:
  revealjs:
    theme: default
    logo: institution_logo.png
bibliography: references.bib
---

## Research Question

{{research_question}}

## Methods
{{methods.sample_brief}}

### Approach
{{methods.statistical_approach_brief}}

## Key Findings
{{results_highlights}}

## Implications
{{implications}}

## Thank You
{{acknowledgments}}
"

# add to database
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "document_templates.conference_presentation",
  value = conference_template,
  db = methods_db
)

# grant proposal template
grant_template <- "---
title: \"{{title}}\"
author: \"{{authors}}\"
date: \"{{date}}\"
format: pdf
---

# Project Summary
{{project_summary}}

# Specific Aims
{{specific_aims}}

# Background and Significance
{{background}}

# Preliminary Studies
{{preliminary_studies}}

# Research Strategy

## Overview
{{research_strategy.overview}}

## Methods
{{methods.sample_planned}}
{{methods.measures_planned}}
{{methods.statistical_approach_planned}}

## Timeline
{{timeline}}

# Budget Justification
{{budget_justification}}

# References
"

# add to database
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "document_templates.grant_proposal",
  value = grant_template,
  db = methods_db
)

# save the updated database
boilerplate_manage_text(
  category = "methods",
  action = "save",
  db = methods_db
)

# ------------------------------------------------------
# function to generate a complete document from a template
# ------------------------------------------------------

generate_document <- function(template_path, study_params, template_vars) {
  # get the template
  template <- boilerplate_manage_text(
    category = "methods",
    action = "get",
    path = template_path
  )

  # generate all method sections
  methods <- list()

  # process each methods section specified in template_vars
  for (section_name in names(template_vars)) {
    # check if it's a methods section
    if (startsWith(section_name, "methods.")) {
      section_path <- template_vars[[section_name]]
      # generate the text for this section
      section_text <- boilerplate_generate_text(
        category = "methods",
        sections = section_path,
        global_vars = study_params
      )
      # store in methods list
      methods[[section_name]] <- section_text
      # update template_vars with actual text
      template_vars[[section_name]] <- section_text
    }
  }

  # apply all template variables to the template
  document <- template
  for (var_name in names(template_vars)) {
    var_placeholder <- paste0("{{", var_name, "}}")
    document <- gsub(var_placeholder, template_vars[[var_name]], document, fixed = TRUE)
  }

  return(document)
}

# ------------------------------------------------------
# usage example: generating a journal article
# ------------------------------------------------------

# define study parameters
study_params <- list(
  exposure_var = "political_conservative",
  outcome_var = "social_wellbeing",
  n_total = 47000,
  appendices_sample = "A-C",
  baseline_wave = "NZAVS time 10, years 2018-2019",
  exposure_wave = "NZAVS time 11, years 2019-2020",
  outcome_wave = "NZAVS time 12, years 2020-2021",
  appendix_ref = "B",
  protocol_url = "https://osf.io/ce4t9/"
)

# define template variables
template_vars <- list(
  title = "Political Orientation and Social Wellbeing in New Zealand: A Causal Analysis",
  authors = "Jane Smith, John Doe, and Robert Johnson",
  date = format(Sys.Date(), "%B %d, %Y"),
  abstract = "This study investigates the causal relationship between political orientation and social wellbeing using data from the New Zealand Attitudes and Values Study.",
  introduction = "Understanding the relationship between political beliefs and wellbeing has important implications for social policy and public health...",
  methods.sample = "sample.nzavs",
  methods.identification_assumptions = "identification_assumptions.standard",
  methods.confounding_control = "confounding_control.vanderweele",
  methods.statistical_approach = "statistical_estimator.lmtp.applied_audience",
  methods.measures = "Custom text about measures will go here.",
  results = "Our analysis revealed significant effects of political conservatism on social wellbeing...",
  discussion = "These findings suggest that political orientation may causally influence wellbeing through several pathways..."
)

# generate the document
journal_article <- generate_document(
  template_path = "document_templates.journal_article",
  study_params = study_params,
  template_vars = template_vars
)


# creating document templates that incorporate boilerplate text sections
library(tidyverse)
library(glue)

# get the current methods database
methods_db <- boilerplate_manage_text(category = "methods", action = "list")

# ------------------------------------------------------
# create document templates for different formats
# ------------------------------------------------------

# typical journal article template
journal_article_template <- "---
title: \"{{title}}\"
author: \"{{authors}}\"
date: \"{{date}}\"
format:
  docx:
    reference-doc: journal_template.docx
bibliography: references.bib
---

# Abstract

{{abstract}}

# Introduction

{{introduction}}

# Methods

## Sample
{{methods.sample}}

## Measures
{{methods.measures}}

## Identification Assumptions
{{methods.identification_assumptions}}

## Confounding Control
{{methods.confounding_control}}

## Statistical Approach
{{methods.statistical_approach}}

# Results

{{results}}

# Discussion

{{discussion}}

# References
"

# add to database
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  path = "document_templates.journal_article",
  value = journal_article_template,
  db = methods_db
)

# conference presentation template
conference_template <- "---
title: \"{{title}}\"
author: \"{{authors}}\"
date: \"{{date}}\"
format:
  revealjs:
    theme: default
    logo: institution_logo.png
bibliography: references.bib
---

## Research Question

{{research_question}}

## Methods
{{methods.sample_brief}}

### Approach
{{methods.statistical_approach_brief}}

## Key Findings
{{results_highlights}}

## Implications
{{implications}}

## Thank You
{{acknowledgments}}
"

# add to database
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  path = "document_templates.conference_presentation",
  value = conference_template,
  db = methods_db
)

# grant proposal template
grant_template <- "---
title: \"{{title}}\"
author: \"{{authors}}\"
date: \"{{date}}\"
format: pdf
---

# Project Summary
{{project_summary}}

# Specific Aims
{{specific_aims}}

# Background and Significance
{{background}}

# Preliminary Studies
{{preliminary_studies}}

# Research Strategy

## Overview
{{research_strategy.overview}}

## Methods
{{methods.sample_planned}}
{{methods.measures_planned}}
{{methods.statistical_approach_planned}}

## Timeline
{{timeline}}

# Budget Justification
{{budget_justification}}

# References
"

# add to database
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  path = "document_templates.grant_proposal",
  value = grant_template,
  db = methods_db
)

# save the updated database
boilerplate_manage_text(
  category = "methods",
  action = "save",
  db = methods_db
)

# ------------------------------------------------------
# function to generate a complete document from a template
# ------------------------------------------------------

generate_document <- function(template_path, study_params, template_vars) {
  # get the template
  template <- boilerplate_manage_text(
    category = "methods",
    action = "get",
    path = template_path
  )

  # generate all method sections
  methods <- list()

  # process each methods section specified in template_vars
  for (section_name in names(template_vars)) {
    # check if it's a methods section
    if (startsWith(section_name, "methods.")) {
      section_path <- template_vars[[section_name]]
      # generate the text for this section
      section_text <- boilerplate_generate_text(
        category = "methods",
        sections = section_path,
        global_vars = study_params
      )
      # store in methods list
      methods[[section_name]] <- section_text
      # update template_vars with actual text
      template_vars[[section_name]] <- section_text
    }
  }

  # apply all template variables to the template
  document <- template
  for (var_name in names(template_vars)) {
    var_placeholder <- paste0("{{", var_name, "}}")
    document <- gsub(var_placeholder, template_vars[[var_name]], document, fixed = TRUE)
  }

  return(document)
}

# ------------------------------------------------------
# usage example: generating a journal article
# ------------------------------------------------------

# define study parameters
study_params <- list(
  exposure_var = "political_conservative",
  outcome_var = "social_wellbeing",
  n_total = "47000",
  appendices_sample = "A-C",
  baseline_wave = "NZAVS time 10, years 2018-2019",
  exposure_wave = "NZAVS time 11, years 2019-2020",
  outcome_wave = "NZAVS time 12, years 2020-2021",
  appendix_ref = "B",
  protocol_url = "https://osf.io/ce4t9/"
)

# define template variables
template_vars <- list(
  title = "Political Orientation and Social Wellbeing in New Zealand: A Causal Analysis",
  authors = "Jane Smith, John Doe, and Robert Johnson",
  date = format(Sys.Date(), "%B %d, %Y"),
  abstract = "This study investigates the causal relationship between political orientation and social wellbeing using data from the New Zealand Attitudes and Values Study.",
  introduction = "Understanding the relationship between political beliefs and wellbeing has important implications for social policy and public health...",
  methods.sample = "sample.nzavs",
  methods.identification_assumptions = "identification_assumptions.standard",
  methods.confounding_control = "confounding_control.vanderweele",
  methods.statistical_approach = "statistical_estimator.lmtp.applied_audience",
  methods.measures = "Custom text about measures will go here.",
  results = "Our analysis revealed significant effects of political conservatism on social wellbeing...",
  discussion = "These findings suggest that political orientation may causally influence wellbeing through several pathways..."
)

# generate the document
journal_article <- generate_document(
  template_path = "document_templates.journal_article",
  study_params = study_params,
  template_vars = template_vars
)

# write to file
writeLines(journal_article, "political_orientation_article.qmd")

# write to file
writeLines(journal_article, "political_orientation_article.qmd")




# working with databases --------------------------------------------------
# defaults
l_measures_db <- boilerplate_manage_measures(action = "list")

measuremeasures_db <- boilerplate_manage_measures(
  action = "add",
  name = "alcohol_frequency_2",
  measure = list(
    description = "Frequency of alcohol consumption was measured using a single item.",
    reference = "nzavs2009",
    waves = "1-current",
    keywords = c("alcohol", "frequency", "consumption"),
    items = list("How often do you have a drink containing alcohol?",
                 "test item")
  ),
  db = test_measures_db
)
measuremeasures_db$alcohol_frequency_2






# old ---------------------------------------------------------------------



general_approach_cate_long <- "Our primary goal was to move beyond average treatment effects (ATE) and investigate whether the intervention's causale effects reliably varied across individuals based on their characteristics. To achieve this, we estimated conditional average treatment effects (CATEs) using causal forests [@grf2024]. Causal forests are a machine learning method adapted specifically for estimating how treatment effects differ across people defined by a set of covariates. These effects are interesting because they help to clarify for whom treatment effects are likely to have their strongest effects, and for whom treatments effects may not work, or be harmful.

For interpretability across different outcomes, we standardised the direction of effects. Some outcomes, like depression scores, are typically interpreted as 'lower is better'. We inverted the scales for such variables so that positive treatment effects consistently indicated improvement (e.g., a reduction in depression). This ensures that larger positive CATE estimates always signify greater benefit from the treatment. The following outcomes were inverted: {{flipped_list}}.

A central challenge when invesitigating individual differences using CATE is ensuring the detected variations are genuine and not just noise or overfitting. Therefore, we rigorously evaluated the causal forest estimates using a 50/50 sample splitting approach. Half the data were used to train the causal forest model (i.e., to identify patterns of differing treatment effects), and the other half (the held-out data) were used exclusively for evaluation.

On the held-out data, we first assessed the calibration of the predictions [@grf2024]. This involved checking if the average of the predicted CATEs accurately reflected the overall average treatment effect (ATE) found in the evaluation sample. More importantly, we tested whether the *predicted variation* in treatment effects (heterogeneous treatment effects, HTE) was reliable. A specific 'differential prediction' test acts as an omnibus indicator ($p$-value) of whether the model successfully captures statistically significant variability in how individuals respond to the treatment [@grf2024]. We also calculated the Rank-Weighted Average Treatment Effect (RATE) [@grf2024; @wager2018]. RATE quantifies the potential real-world value of using the model to target treatment – specifically, it measures the average outcome improvement we would expect if we prioritised treatment for the individuals predicted to benefit most, compared to a simpler strategy like treating everyone or no one based only on the ATE.

To visualise the benefit of prioritisation, we used Qini curves [@grf2024]. These curves compare two scenarios evaluated on the held-out data:
1.  **Uniform Allocation:** Treating (or not treating) individuals based solely on the overall ATE.
2.  **Targeted Allocation:** Prioritising treatment for individuals ranked highest on their predicted CATE from the causal forest.

The Qini curve plots the cumulative gain (or loss) achieved by the targeted approach as we increase the proportion of the sample receiving treatment (starting with those predicted to benefit most). Positive Qini values indicate that targeting treatment based on predicted CATEs yields better overall outcomes than the uniform approach for that proportion treated. This helps determine if a personalised strategy is advantageous, and potentially for what fraction of the population.

Finally, if we detected reliable heterogeneity , we aimed to translate these complex CATE predictions into simpler, actionable rules using policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica]. Policy trees learn simple decision rules (e.g., 'Treat individuals with baseline score > X and age < Y') that optimise treatment allocation based on the estimated CATEs. Policy tree estimation identify defining baseline characteristics that distinguish subgroups with markedly different responses, providing transparent, interpretable guidelines for potentially tailoring treatment in practice.

All heterogeneity analyses, including calibration tests, RATE calculations, Qini curves, and policy trees, were conducted using R, primarily with the `grf` [@grf2024], `policytree` [@policytree_package_2024], and `margot` [@margot2024] packages. The figures presented were generated using `margot`.

Collectively, this multi-stage approach allows us to first estimate individualised effects, rigorously test whether these estimated variations are reliable, evaluate the potential gains from targeting treatment, and finally, derive simple rules to guide personalised intervention strategies if justified by the data.
"
general_approach_cate_short <- "Our primary aim was to look beyond average treatment effects (ATE) and explore whether the intervention’s impact varied systematically across individuals. To that end, we estimated individualised treatment effects, also called conditional average treatment effects (CATEs), using causal forests [@grf2024], a machine learning approach tailored for detecting treatment-effect heterogeneity based on covariates.

We standardised effect directions by inverting any outcome where 'lower is better', so positive values consistently denote improvement. The following outcomes were inverted: {{flipped_list}}.

A common concern when modelling individual differences using CATE is distinguishing real variation from noise. To address this, we used a 50/50 sample split. We trained the causal forest on half the data, then tested model predictions exclusively on the remaining half to prevent overfitting.

In the held-out sample, we checked whether the predicted CATEs were well calibrated [@grf2024]. This involved comparing (i) the mean of the predicted effects to the overall ATE in the evaluation sample, and (ii) testing whether the predicted *variation* in effects was reliable. A differential prediction test provided an omnibus $p$-value for whether the model captured genuine heterogeneity in responses. We also calculated the Rank-Weighted Average Treatment Effect (RATE), which estimates how effectively a model-based targeting strategy would outperform a uniform treatment strategy [@grf2024; @wager2018].

To visualise the added value of targeting, we used Qini curves [@grf2024]. These compare:

1.  **Uniform Allocation**: treating or not treating everyone based on the overall ATE.
2.  **Targeted Allocation**: treating individuals ranked highest by their predicted CATE first.

The Qini curve shows the cumulative gain (or loss) of the targeted strategy as we treat larger proportions of the sample. Positive values suggest that prioritising high-CATE individuals outperforms uniform treatment, helping identify whether, and at what coverage level, personalisation is beneficial.

Finally, for practical implementation, we used policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] to derive simple, rule-based recommendations. Policy trees split participants into subgroups defined by key moderators, providing clear 'treat' or 'do not treat' cut-points, evaluated for effectiveness on the held-out data. All heterogeneity analyses—including calibration tests, RATE, Qini curves, and policy trees—were done in R using `grf` [@grf2024], `policytree` [@policytree_package_2024], and `margot` [@margot2024].

Taken together, this multi-stage approach first identifies individualised treatment effects, then tests their reliability, estimates the added value of targeted intervention, and, if justified, provides simple rules for personalising treatment in practice."

interpretation_rate_test <- "To assess the potential practical value of tailoring treatment based on individual predictions, we examined the Rank-Weighted Average Treatment Effect (RATE) and Qini curves [@grf2024; @wager2018]. These predictions are estimates (denoted $\\hat{\\tau}(X_i)$) of the underlying conditional average treatment effect (CATE, $\tau(X_i)$), which represents the average treatment effect for individuals with specific baseline covariates $X_i$. Recall that for some outcomes ({{flipped_outcomes}}), scales were inverted so higher predicted effects ($\\hat{\\tau}(X_i)$) always indicate greater benefit from treatment. RATE and Qini curves evaluate how well a strategy of prioritising treatment using these CATE predictions $\\hat{\\tau}(X_i)$ performs compared to simpler approaches (e.g., uniform treatment based on the overall average effect, ATE). Specifically, RATE estimates the average gain achieved among the group prioritised by the model [@wager2018]. Qini curves dynamically visualise this potential gain: they plot the cumulative benefit realised as we hypothetically treat an increasing proportion of the population, starting with those individuals having the highest predicted effects $\\hat{\\tau}(X_i)$ [@grf2024]."
