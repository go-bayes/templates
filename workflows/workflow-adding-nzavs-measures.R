# test
# if (!boilerplate_path_exists(unified_db$results, "grf")) {
#   unified_db$results$grf <- list()
# }
# devtools::load_all("/Users/joseph/GIT/boilerplate/")

# initialise measures
# install from GitHub if not already installed
if (!require(boilerplate, quietly = TRUE)) {
  # install devtools if necessary
  if (!require(devtools, quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("go-bayes/boilerplate")
}

library(boilerplate)



# set path ----------------------------------------------------------------
my_project_path <- "/Users/joseph/GIT/templates/boilerplate_data"
test_path <- "/Users/joseph/GIT/templates/test"



# tests -------------------------------------------------------------------

boilerplate_init(create_dirs = TRUE, confirm = TRUE)

boilerplate_init()


# import all databases into a unified structure
all_db <- boilerplate_import()

# add a new method entry directly to the unified database
all_db$methods$sample_selection <- "Participants were selected from {{population}} during {{timeframe}}."

# save all changes at once
boilerplate_save(all_db)

test_test_db <- boilerplate_import()

boilerplate_save(
  all_db
)


# generate text with variable substitution
methods_text <- boilerplate_generate_text(
  category = "methods",
  sections = c("sample", "sample_selection"),
  global_vars = list(
    population = "university students",
    timeframe = "2020-2021"
  ),
  db = all_db,  # pass the unified database
  add_headings = TRUE
)

cat(methods_text)









# import data -------------------------------------------------------------
proto_unified_db <- boilerplate_import( data_path = my_project_path)

boilerplate_save(
  proto_unified_db,
  data_path = my_project_path
)

# set path ----------------------------------------------------------------
student_path <- here::here("student_boilerplate_data")

# proto_unified_db$appendix$explain$grf_short
# proto_unified_db$template$conference_presentation
interpretation_rate_no_flip_text

boilerplate::boilerplate_export(
  proto_unified_db,
  select_elements = c("measures.*", "methods.student_sample.nzavs", "methods.student_target_population", "methods.statistical_models.grf_short_explanation","methods.causal_intervention.grf_simple_text", "methods.analytic_approach.simple_general_approach_cate_long", "methods.sensitivity_analysis.short_evalue", "methods.grf_simple_text", "methods.causal_assumptions.*", "methods.causal_identification_criteria", "methods.statistical_models.grf_short_explanation", "methods.missing_data.missing_grf_simple", "methods.exposure_indicator", "methods.analytic_approach.*","methods.causal_intervention.grf_simple_text", "methods.confounding_control.vanderweele","methods.eligibility.standard", "results.grf.*", "discussion.student_authors_statement"," discussion.student_ethics","discussion.student_data", "appendix.exposure", "appendix.baseline", "appendix.references", "appendix.strengths_grf_short_text", "discussion.*", "appendix.explain.grf_short",  "appendix.explain.grf_long", "template.conference_presentation"),
  data_path = student_path,
  output_file = "student_unified_test_db"
)

test_db <- boilerplate_import( data_path = student_path)
cat(test_db$discussion$strengths$strengths_grf_short)
cat(test_db$appendix$explain$grf_short)
cat(test_db$methods$causal_intervention$grf_simple_text)

# Using the new boilerplate_export() function
# boilerplate_export(
#   unified_db,
#   select_elements = c("methods.statistical.*", "results.main_effect"),
#   output_file = "selected_elements.rds",
#   data_path = test_path,
# )


boilerplate_save(
  test_db,
  output_file = "student_unified_test_db",
  data_path = student_path
)
test_db <- boilerplate_import( data_path = student_path)


# Introduction ------------------------------------------------------------
# import data

unified_db <- boilerplate_import()


unified_db$bibliography

# add bibliography --------------------------------------------------------

# Configure bibliography source
db <- boilerplate_add_bibliography(
  unified_db,
  url = "https://raw.githubusercontent.com/go-bayes/templates/refs/heads/main/bib/references.bib",
  local_path = "references2.bib"
)

# Download and copy bibliography
boilerplate_copy_bibliography(db, target_dir = "data/boilerplate")

# Save configuration
boilerplate_save(db, data_path = "boilerplate/data/")


# standardise measures ----------------------------------------------------



# check
unified_db$measures <- boilerplate_standardise_measures(unified_db$measures)
boilerplate_measures_report(unified_db$measures, return_report = TRUE)

# save db
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)





# save workflow as json ---------------------------------------------------









)

# 6. Validate before manuscript submission
# ----------------------------------------
validate_project_boilerplate <- function(project_path = ".") {
  data_path <- file.path(project_path, PROJECT_DATA)

  cli_h2("Validating boilerplate content")

  # Check JSON validity
  json_valid <- validate_boilerplate_json(
    file.path(data_path, "boilerplate_unified.json"),
    verbose = FALSE
  )

  if (!json_valid$valid) {
    cli_alert_danger("JSON validation failed")
    return(FALSE)
  }

  # Check for required content
  db <- boilerplate_import_json(data_path = data_path)

  # Example checks
  checks <- list(
    "Has methods section" = !is.null(db$methods),
    "Has measures section" = !is.null(db$measures),
    "Methods have references" = check_references(db$methods),
    "No template variables left" = !check_template_vars(db)
  )

  all_passed <- all(unlist(checks))

  for (check_name in names(checks)) {
    if (checks[[check_name]]) {
      cli_alert_success(check_name)
    } else {
      cli_alert_danger(check_name)
    }
  }

  return(all_passed)
}

# Helper functions
check_references <- function(obj) {
  # Check if all entries have references where expected
  # Returns TRUE if OK
  TRUE  # Simplified for example
}

check_template_vars <- function(obj) {
  # Check for unreplaced template variables
  # Returns TRUE if any found
  FALSE  # Simplified for example
}

my_project_path
my_project_path
db <- boilerplate_import(data_path = my_project_path)
boilerplate_save_json(db, data_path = my_project_path, "db")
boilerplate_save_json()
# edit database -----------------------------------------------------------
# Example 2: Update all references containing "NZAVS"
unified_db <- boilerplate_batch_edit(
  db = unified_db,
  field = "reference",
  new_value = "sibley2021",
  match_pattern = "NZAVS",
  category = "measures"
)


boilerplate_batch_edit(
  db = unified_db,
  field = "reference",
  new_value = "statsnz_ssga18",
  match_pattern = "Stats NZ Census Question",
  category = "measures",
  preview = TRUE
)

unified_db <-
  boilerplate_batch_edit(
    db = unified_db,
    field = "reference",
    new_value = "statsnz_ssga18",
    match_pattern = "Stats NZ Census Question",
    category = "measures",
    preview = FALSE
  )


boilerplate_batch_edit(
  db = unified_db,
  field = "reference",
  new_value = "statsnz_ssga18",
  match_pattern = "Stats NZ Census Question",
  category = "measures",
  preview = TRUE
)

unified_db <-
  boilerplate_batch_edit(
    db = unified_db,
    field = "reference",
    new_value = "statsnz_ssga18",
    match_pattern = "NZ Census",
    category = "measures",
    preview = TRUE
  )


boilerplate_save(unified_db, data_path = my_project_path, create_backup = TRUE)

entries_with_chars <- boilerplate_find_chars(
  db = unified_db,
  field = "reference",
  chars = c("@", "[", "]"),
  category = "measures"
)
print(entries_with_chars)

unified_db <- boilerplate_batch_clean(
  db = unified_db,
  field = "reference",
  remove_chars = c("@", "[", "]"),
  exclude_entries = c("forgiveness"),
  category = "measures",
  preview = TRUE
)

entries_with_chars <- boilerplate_find_chars(
  db = unified_db,
  field = "reference",
  chars = c("@", "[", "]"),
  category = "measures"
)
print(entries_with_chars)

unified_db <- boilerplate_batch_clean(
  db = unified_db,
  field = "reference",
  remove_chars = c("@", "[", "]"),
  exclude_entries = c("forgiveness"),
  category = "measures",
  preview = TRUE
)


boilerplate_find_chars(unified_db, field = "reference", chars = "string_is")

# check
boilerplate_batch_clean(
  db = unified_db,
  field = "reference",
  remove_chars =  "string_is",
  category = "measures",
  preview = TRUE
)

# edit
unified_db <- boilerplate_batch_clean(
  db = unified_db,
  field = "reference",
  remove_chars = "string_is",
  category = "measures"
)

# check
boilerplate_batch_clean(
  db = unified_db,
  field = "reference",
  remove_chars =  "string_is",
  category = "measures",
  preview = TRUE
)

# edit
boilerplate_batch_clean(
  db = unified_db,
  field = "reference",
  remove_chars = "NZ",
  category = "measures",
  preview = TRUE
)


# check
boilerplate_batch_clean(
  db = unified_db,
  field = "reference",
  remove_chars = c("@", "[", "]"),
  exclude_entries = c("forgiveness"),
  category = "measures",
  preview = TRUE
)

# apply
unified_db <- boilerplate_batch_clean(
  db = unified_db,
  field = "reference",
  remove_chars = c("@", "[", "]"),
  exclude_entries = c("forgiveness"),
  category = "measures"
)

boilerplate_save(unified_db, data_path = my_project_path, create_backup = TRUE)


s#
# # measurs -----------------------------------------------------------------
# unified_db$measures$sdo$description <- "Social Dominance Orientation was measured using the following items:"
# unified_db$measures$rwa$description <- "Right Wing Authoritarianism was measured using the following items:"
#




grf_introduction_text<- "## Introduction

Understanding the factors that shape {{name_outcomes_lower}} is a fundamental goal in psychological science. Previous research has linked {{name_exposure_capfirst}} to {{name_outcomes_lower}}, yet establishing a reliable *causal* relationship remains challenging. Moving beyond mere correlation to identify causal effects is crucial for accurately predicting how interventions, such as encouraging {{name_exposure_lower}}, might influence individual development.

Furthermore, individuals are not uniform; they respond differently to the same experiences. Relying solely on average treatment effects can mask significant heterogeneity, where effects vary substantially across different subgroups. Identifying who benefits most, least, or even differently from an intervention is vital for developing targeted and effective strategies.

Traditional parametric approaches, such as standard linear regression, often struggle to meet these needs. They typically impose restrictive assumptions about the functional form of relationships and the uniformity of effects, potentially leading to biased causal estimates and overlooking crucial individual differences [@HainmuellerMummoloXu2019]."


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "template.grf.simple",
  value = grf_introduction_text
)

boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


# student place holder
grf_introduction_text<- "## Introduction

Understanding the factors that shape {{name_outcomes_lower}} is a fundamental goal in psychological science. Previous research has linked{{name_exposure_capfirst}} to {{name_outcomes_lower}}, yet establishing a reliable *causal* relationship remains challenging. Moving beyond mere correlation to identify causal effects is crucial for accurately predicting how interventions, such as encouraging {{name_exposure_lower}}, might influence individual development.

Furthermore, individuals are not uniform; they respond differently to the same experiences. Relying solely on average treatment effects can mask significant heterogeneity, where effects vary substantially across different subgroups. Identifying who benefits most, least, or even differently from an intervention is vital for developing targeted and effective strategies.

Traditional parametric approaches, such as standard linear regression, often struggle to meet these needs. They typically impose restrictive assumptions about the functional form of relationships and the uniformity of effects, potentially leading to biased causal estimates and overlooking crucial individual differences [@HainmuellerMummoloXu2019].

This study addresses these limitations by combining large-scale, longitudinal national panel data (the New Zealand Attitudes and Values Study) with robust methodological approaches. We aim to estimate the causal effect of {{name_exposure_lower}} on {{name_outcomes_lower}} while carefully investigating heterogeneity in these effects using non-parametric machine learning [@grf2024]. By employing methods designed to handle complex confounding and detect variations across individuals, we seek more subtle insights than conventional techniques typically provide, offering clarity into how targeted interventions affect individuals across the population over time."

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "template.grf.simple",
  value = grf_introduction_text
)

boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


# METHODS -----------------------------------------------------------------



# ------------------------------------------------------
# sample section
# ------------------------------------------------------
# n_total, appendix_timeline
sample_information_text <- "
### Sample

Data were collected as part of the New Zealand Attitudes and Values Study (NZAVS), an annual longitudinal national probability panel assessing New Zealand residents’ social attitudes, personality, ideology, and health outcomes. The panel began in 2009 and has since expanded to include over fifty researchers, with responses from {{n_total}} participants to date. The study operates independently of political or corporate funding and is based at a university. It employs prize draws to incentivise participation. The NZAVS tends to slightly under-sample males and individuals of Asian descent and to over-sample females and Māori (the Indigenous people of New Zealand). To enhance the representativeness of our sample population estimates for the target population of New Zealand, we apply census-based survey weights that adjust for age, gender, and ethnicity (New Zealand European, Asian, Māori, Pacific) [@sibley2021]. For more information about the NZAVS, visit: [OSF.IO/75SNB](https://doi.org/10.17605/OSF.IO/75SNB).
"
sample_information_text_timeline <- "
### Sample

Data were collected as part of the New Zealand Attitudes and Values Study (NZAVS), an annual longitudinal national probability panel assessing New Zealand residents’ social attitudes, personality, ideology, and health outcomes. The panel began in 2009 and has since expanded to include over fifty researchers, with responses from {{n_total}} participants to date. The study operates independently of political or corporate funding and is based at a university. It employs prize draws to incentivise participation. The NZAVS tends to slightly under-sample males and individuals of Asian descent and to over-sample females and Māori (the Indigenous people of New Zealand). To enhance the representativeness of our sample population estimates for the target population of New Zealand, we apply census-based survey weights that adjust for age, gender, and ethnicity (New Zealand European, Asian, Māori, Pacific) [@sibley2021]. For more information about the NZAVS, visit: [OSF.IO/75SNB](https://doi.org/10.17605/OSF.IO/75SNB). Refer to [Appendix {{appendix_timeline}}](#appendix-timeline) for a histogram of daily responses for this cohort.
"


# unified_db<- boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.sample",
#   value = list()
# )


# or
unified_db<- boilerplate_update_entry(
    db = unified_db,
    path = "methods.sample.nzavs",
    value = sample_information_text
  )

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.sample.nzavs_timline",
  value = sample_information_text_timeline
)

boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


str(unified_db, max.level = 2)

student_sample_information_text <- "
### Sample

The data in this study are simulated for the purposes of instruction from data collected as part of the New Zealand Attitudes and Values Study (NZAVS). The NZAVS is an annual longitudinal national probability panel assessing New Zealand residents’ social attitudes, personality, ideology, and health outcomes. The panel began in 2009 and has since expanded to include over fifty researchers, with responses from {{n_total}} participants to date. The study operates independently of political or corporate funding and is based at a university. It employs prize draws to incentivise participation. The NZAVS tends to slightly under-sample males and individuals of Asian descent and to over-sample females and Māori (the Indigenous people of New Zealand). To enhance the representativeness of our sample population estimates for the target population of New Zealand, we apply census-based survey weights that adjust for age, gender, and ethnicity (New Zealand European, Asian, Māori, Pacific) [@sibley2021]. For more information about the NZAVS, visit: [OSF.IO/75SNB](https://doi.org/10.17605/OSF.IO/75SNB).
"

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.sudent_sample.nzavs",
  value = student_sample_information_text
)

# ------------------------------------------------------
# target population section
# ------------------------------------------------------
target_population_text <- "
### Target Population

The target population for this study comprises New Zealand residents as represented in the {{baseline_wave}} of the New Zealand Attitudes and Values Study (NZAVS) during the years {{baseline_wave}} weighted by New Zealand Census weights for age, gender, and ethnicity (refer to @sibley2021). The NZAVS is a national probability study designed to reflect the broader New Zealand population accurately. Despite its comprehensive scope, the NZAVS has some limitations in its demographic representation. Notably, it tends to under-sample males and individuals of Asian descent while over-sampling females and Māori (the indigenous peoples of New Zealand). To address these disparities and enhance the accuracy of our findings, we apply New Zealand Census survey weights to the sample data."


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.target_population",
  value = target_population_text
)

# save
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)

# for teaching
student_target_population_text <- "
### Target Population

The data are simulated from the New Zealand Attitudes and Values Study. For the purposes of this assessment, the target population for this study comprises New Zealand residents as represented in the {{baseline_wave}} of the New Zealand Attitudes and Values Study (NZAVS) during the years {{baseline_wave}} weighted by New Zealand Census weights for age, gender, and ethnicity (refer to @sibley2021). The NZAVS is a national probability study designed to reflect the broader New Zealand population accurately. Despite its comprehensive scope, the NZAVS has some limitations in its demographic representation. Notably, it tends to under-sample males and individuals of Asian descent while over-sampling females and Māori (the indigenous peoples of New Zealand). To address these disparities and enhance the accuracy of our findings, we apply New Zealand Census survey weights to the sample data."

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.student_target_population",
  value = student_target_population_text
)

# ------------------------------------------------------
# causal interventions section
# ------------------------------------------------------

# contrasts_text, interventions_list
basic_interventions_text <- "#### Interventions
This study considers the following causal interventions on the exposure variable '{{exposure_var}}':

{{interventions_list}}

#### Contrasts

{{contrasts_text}}

This approach to defining interventions and contrasts allows us to systematically evaluate the causal effects of interest in our study."



# unified_db<- boilerplate_add_entry(
#   db = unified_db,
#   path = "methods.causal_intervention",
#   value = list()
# )

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.causal_intervention.basic",
  value = basic_interventions_text
)




# complex causal interventions --------------------------------------------




# We using the six most recent NZAVS waves (Times 10–15) because Wave 10 containes the largest cohort, enabling us to maximise power.
lmtp_multi_wave_text <-"
### Causal Inference

When psychologists analyse time-series data, they often use growth models to describe how variables evolve over time. However, many questions are causal: we want to know what would happen if we could intervene on certain variables (such as {{name_exposure_variable}}). To investigate these questions with observational time-series data, we must clearly define our causal question and design our analysis to emulate a hypothetical randomised controlled trial—often called a **target trial** [@hernan2016]. A target trial asks, setting aside practicalities and ethics, *what experiment are we attempting to emulate with our data?* Without explicitly stating this hypothetical experiment, it can be unclear which causal effect we are actually estimating.

Here, we ask:

  > 'What if, at each wave, we intervened to set {{name_exposure_variable}} to a certain level, and then measured everyone's outcomes at the final wave?'

To answer this, we compare two hypothetical interventions. Each intervention shifts {{name_exposure_variable}} across {{number_exposure_waves}} waves, with outcomes measured after the year following the final exposure wave.  A rich set of indicators covariates in the baseline wave -- the wave before the first exposure wave -- as well measurements of time-varying confounders at each exposure wave obtained there after are necessary to control for common causes of the exposures and outcomes measured at the end of study (refer to @tbl-plan).

Following a modified treatment policies approach, we define **shift functions** describing each intervention:

1. **{{name_exposure_regime}}**

{{value_exposure_regime}}

2. **{{name_control_regime}}**

{{value_control_regime}}

::: {#tbl-plan}
```{=latex}
\\vizfive
```
We contrast outcomes from two treatment regimes: (1) {{name_exposure_regime}} (2) {{name_control_regime}}. $a^{+}$ denotes {{value_exposure_regime}}; $a^{-}$ denotes {{value_control_regime}}. Our statistical models control for baseline-wave confounders, and subsequent time-varying confounders for all exposure waves. We include baseline measurments of {{name_exposure_variable}} and baseline measurements of all outcomes as confounders. We assume that conditional on these confounders, treatment assignment is 'as good as random.' Outcomes, here denoted $Y_\\tau$, are measured in the wave following the final treatment.
:::

We then organise our data to resemble a randomised sequential experiment that assigns each person to one of two longitudinal treatment strategies *{{name_exposure_regime}}* and *{{name_control_regime}}*. We define a 'confounder' as a variable that, once included in the model, along with other included variables, removes any non-causal association between the treatment and outcome. Here, as mentioned, we adjust for a rich set of demographic and personality variables, as well as baseline {{name_exposure_variable}} and baseline measures of all outcomes. We also adjust for time-varying confounders at each wave {{time_varying_confounders}}. We assume these time-varying confounders can influence {{name_exposure_variable}} and outcomes, potentially biasing our estimates. We ensure there is no reverse causation by measuring the outcomes at the end of the study, one year after the final treatment wave.

::: {#tbl-feedback}
```{=latex}
\\feedbackB
```
Common cause of Treatment 1 and downstream confounder of Treatment 2 is a collider.
:::

### Causal Contrasts

We compute the average expected outcome under and {{name_exposure_regime}} and {{name_control_regime}}, and then contrast these expected averages on the difference scale (i.e., subtracting the expected outcome under {{name_exposure_regime}} from that under {{name_control_regime}}. We obtain confidence intervals using the cross-fitted influence-function approach in the `lmtp` package [@williams2021]. This approach employs sequentially doubly robust (SDR) estimator, as developed by @diaz2021_non_parametric_lmtp, which remains valid if either the outcome model or the propensity model is correctly specified, thereby requiring weaker assumptions than standard approaches. By setting up our data as if it came from a hypothetical experiment, we gain clarity about which causal effects we are estimating and as well as confidence about our causal effect estimates (refer to @hernan2024WHATIF, @bulbulia2022; @bulbulia2023).

Our estimation relies on standard causal assumptions:

1.  **No unmeasured confounding**: all relevant common causes of the exposure and outcome are included in $X$.
2.  **Consistency**: an individual's observed outcome corresponds to their potential outcome under the exposure they actually received.
3.  **Positivity**: within strata defined by $X$, there is a non-zero probability of receiving either exposure level (`{{value_exposure}}` or `{{value_control}}`).

(Refer to [Appendix {{appendix_assumptions}}](#appendix-assumptions) for further details). The target population is {{name_target_population}}."


lmtp_multi_wave_long_text <-"
### Causal Inference

When psychologists analyse time-series data, they often use growth models to describe how variables evolve over time. However, many questions are causal: we want to know what would happen if we could intervene on certain variables (such as {{name_exposure_variable}}). To investigate these questions with observational time-series data, we must clearly define our causal question and design our analysis to emulate a hypothetical randomised controlled trial, often called a **target trial** [@hernan2016]. A target trial asks, setting aside practicalities and ethics, *what experiment are we attempting to emulate with our data?* Without explicitly stating this causal question, it can be unclear which causal effect we are actually estimating.

Here, we ask:

  > 'What if, at each wave, we intervened to set {{name_exposure_variable}} to a certain level, and then measured everyone's outcomes at the final wave?'

To answer this, we compare two hypothetical interventions. Each intervention shifts {{name_exposure_variable}} across {{number_exposure_waves}} waves, with outcomes measured after the year following the final exposure wave. A rich set of indicators covariates in the baseline wave -- the wave before the first exposure wave -- as well measurements of time-varying confounders at each exposure wave obtained thereafter are necessary to control for common causes of the exposures and outcomes measured at the end of study (refer to @tbl-plan, and see @tbl-feedback for a detailed explanation of time-varying confounding).

Following a modified treatment policies approach, we define **shift functions** describing each intervention:

1. **{{name_exposure_regime}}**

   {{value_exposure_regime}}

2. **{{name_control_regime}}**

   {{value_control_regime}}

::: {#tbl-plan}
```{=latex}
\\vizfive
:::

We contrast outcomes from two treatment regimes: (1) {{name_exposure_regime}} (2) {{name_control_regime}}. $a^{+}$ denotes {{value_exposure_regime}}; $a^{-}$ denotes {{value_control_regime}}. Our statistical models control for baseline-wave confounders, and subsequent time-varying confounders for all exposure waves. We include baseline measurments of {{name_exposure_variable}} and baseline measurements of all outcomes as confounders. We assume that conditional on these confounders, treatment assignment is ‘as good as random.’ Outcomes, here denoted $Y_\\tau$, are measured in the wave following the final treatment.
:::


We then organise our data to resemble a randomised sequential experiment that assigns each person to one of two longitudinal treatment strategies {{name_exposure_regime}} and {{name_control_regime}}. We define a ‘confounder’ as a variable that, once included in the model, along with other included variables, removes any non-causal association between the treatment and outcome. Here, as mentioned, we adjust for a rich set of demographic and personality variables, as well as baseline {{name_exposure_variable}} and baseline measures of all outcomes. We also adjust for time-varying confounders at each wave {{time_varying_confounders}}. We assume these time-varying confounders can influence {{name_exposure_variable}} and outcomes, potentially biasing our estimates. We ensure there is no reverse causation by measuring the outcomes at the end of the study, one year after the final treatment wave.

::: {#tbl-feedback}
```{=latex}
\\feedbackB
```
Causal Diagram: Common cause of Treatment 1 and downstream confounder of Treatment 2 is a collider (biasing path is shown in red).
:::

#### Explanation of Time-Varying Confounding

Time-varying confounding arises when certain variables that influence both the exposure and the outcome can themselves change over time—often because they are affected by prior exposures or outcomes. By the next wave, these evolving confounders may affect future exposures and outcomes, creating a feedback loop. If we fail to account for these changing factors at each wave, we risk attributing changes in the outcome to our exposure when they may instead be driven by shifts in these confounders. Therefore, properly including time-varying confounders at each exposure wave is crucial to avoid bias and correctly isolate the causal effect of the exposure.


#### Causal Contrasts

We compute the average expected outcome under {{name_exposure_regime}} and {{name_control_regime}}, and then contrast these expected averages on the difference scale (i.e., subtracting the expected outcome under {{name_exposure_regime}} from that under {{name_control_regime}}). We obtain confidence intervals using the cross-fitted influence-function approach in the lmtp package [@williams2021]. This approach employs a sequentially doubly robust (SDR) estimator, as developed by @diaz2021_non_parametric_lmtp, which remains valid if either the outcome model or the propensity model is correctly specified, thereby requiring weaker assumptions than standard approaches. By setting up our data as if it came from a hypothetical experiment, we gain clarity about which causal effects we are estimating, as well as confidence about our causal effect estimates (refer to @hernan2024WHATIF, @bulbulia2022; @bulbulia2023)."


grf_causal_text <- "
### Causal Inference

When researchers analyse data, they often seek to understand causal relationships: what would happen if we could intervene on certain variables? To investigate such questions with observational data, we must clearly define our causal question and design our analysis to emulate a hypothetical randomised controlled trial—often called a **target trial** [@hernan2016]. A target trial asks, setting aside practicalities and ethics, *what experiment are we attempting to emulate with our data?* Without explicitly stating this hypothetical experiment, it can be unclear which causal effect we are actually estimating.

Here, we ask:

> 'What is the effect on an outcome if we set a binary exposure variable to a specific level for everyone, compared to setting it to an alternative level, considering individual characteristics?'

To answer this, we compare two hypothetical exposures:

1.  **{{name_exposure_threshold}}**: We hypothetically assign everyone to have the exposure set to `{{value_exposure}}`.
2.  **{{name_control_threshold}}**: We hypothetically assign everyone to have the exposure set to `{{value_control}}`.

Our goal is to estimate how the effect of the {{name_exposure_threshold}} exposure compared to the {{name_control_threshold}} exposure varies across individuals based on their baseline characteristics. We aim to compute the **Conditional Average Treatment Effect (CATE)**, defined as:

$$ \\tau(x) = E[Y({{value_exposure}}) - Y({{value_control}})|X = x] $$

Here, $Y({{value_exposure}})$ is the potential outcome if an individual received the {{name_exposure_threshold}} exposure, $Y({{value_control}})$ is the potential outcome if they received the {{name_control_threshold}} exposure, and $X$ represents the full set of baseline covariates measured before the exposure. The CATE, $\\tau(x)$, estimates the average difference in outcomes between the two exposures for individuals with specific characteristics $x$.

(Refer to [Appendix {{appendix_assumptions_grf}}](#appendix-assumptions_grf) and [{{appendix_explain_grf}}](#appendix-explain-grf), w further details). The target population is {{name_target_population}}."



# To estimate this effect using observational data, we must account for **confounding**. Confounders are variables associated with both the exposure and the outcome that can distort the estimated relationship. In this study, we adjust for a rich set of baseline covariates, $X$, including demographic factors, personality traits, and baseline measures relevant to the outcome. We assume there are no time-varying confounders relevant to this cross-sectional design. By including these baseline covariates in our statistical model, we aim to remove non-causal associations between the exposure and the outcome, assuming that, conditional on $X$, the exposure is 'as good as random.'
#
# Our estimation relies on standard causal assumptions:
#
#   1.  **Consistency**: an individual's observed outcome corresponds to their potential outcome under the exposure they actually received.
# 2.  **No unmeasured confounding**: all relevant common causes of the exposure and outcome are included in $X$.
# 3.  **Positivity**: within strata defined by $X$, there is a non-zero probability of receiving either exposure level (`{{value_exposure}}` or `{{value_control}}`).

grf_simple_text <- "
### Average treatment effect

To learn how the outcome would shift if everyone received a different exposure, we emulate a **target trial** [@hernan2016c]. Making the hypothetical experiment explicit fixes the estimand, the data requirements, and the assumptions.

Our guiding question is:

> *How would the outcomes change if, for every individual, we set the exposure to **{{value_exposure}}** rather than **{{value_control}}**, conditional on their baseline characteristics?*

We compare two interventions:

1. **{{name_exposure_threshold}}** — every participant is set to {{value_exposure}}.
2. **{{name_control_threshold}}** — every participant is set to {{value_control}}.

The difference in population means defines the **average treatment effect (ATE)**. Figure @fig-exposure plots the exposure distribution and its dichotomisation; the centre dashed line marks the mean, flanked by one standard deviation.

```{r}
#| label: fig-exposure
#| fig-cap: \"Histogram of exposure with binary groupin\"
#| eval: true
#| echo: false
#| fig-height: 12   # tweak if needed
#| fig-width: 12    # tweak if needed

graph_cut

```

Because we test several outcomes, we adjust the ATE confidence intervals for multiplicity with {{ate_adjustment}} at $\\alpha = {{ate_alpha}}$.

The longitudinal design and rich baseline covariates allow us -- under the standard identification assumptions of consistency, positivity, and no unmeasured confounding -- to attribute the differences between the two exposure means as a causal effect. Conditioning on demographics, personality traits, and other pretreatment factors renders exposure assignment ignorable [@rosenbaum1983central]."

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.causal_intervention.grf_simple_text",
  value = grf_simple_text
)
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.causal_intervention.lmtp_multi_wave",
  value = lmtp_multi_wave_text
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.causal_intervention.lmtp_multi_wave_long",
  value = lmtp_multi_wave_text
)


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.causal_intervention.grf",
  value = grf_causal_text
  )



cat(unified_db$methods$causal_intervention$lmtp_multi_wave)

boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


# To infer causal effects, we assume:
#
#   1. **No unmeasured confounding**: We have included all important variables that influence both exposure and outcome.
# 2. **Consistency**: The outcome we observe for each person under their actual exposure matches what we would expect if they truly received that exposure in an experiment.
# 3. **Positivity**: There is a real chance (greater than zero) for people with different traits to have received either exposure level (`{{value_exposure}}` or `{{value_control}}`).



# test --------------------------------------------------------------------

unified_db <- boilerplate_import( data_path = my_project_path)

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
causal_identification_criteria_text <- "
### Causal Identification Assumptions

This study relies on the following identification assumptions for estimating the causal effect of {{name_exposure_variable}}:

1. **Consistency**: the observed outcome under the observed {{name_exposure_variable}} is equal to the potential outcome under that exposure level. As part of consistency, we assume no interference: the potential outcomes for one individual are not affected by the {{name_exposure_variable}} status of other individuals.

2. **No unmeasured confounding**: all variables that affect both {{name_exposure_variable}} and the outcome have been measured and accounted for in the analysis.

3. **Positivity**: there is a non-zero probability of receiving each level of {{name_exposure_variable}} for every combination of values of {{name_exposure_variable}} and confounders in the population. Positivity is the only fundamental casual assumption that can be evaluated with data (refer to [Appendix {{appendix_positivity}}](#appendix-positivity))."


# add criteria
cat(unified_db$methods$causal_identification_criteria_text)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.causal_identification_criteria",
  value = causal_identification_criteria_text
)

cat(unified_db$methods$causal_identification_criteria)

boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)



# exposure indicator ------------------------------------------------------

methods_exposure_indicator_text <- "
### Exposure Indicator


The New Zealand Attitudes and Values Study assesses {{name_exposure_variable}} using the following question:


{{measures_exposure}}(Refer to [Appendix {{appendix_measures}}](#appendix-measures))."



# outcome domains ---------------------------------------------------------
outcomewide_flourishing_text <- "
### Wellbeing Outcomes

We adopt an outcome-wide approach, modelling every outcome in a domain. This strategy reduces cherry-picking and selectivity biases, and offers a meta-analytic perspective on the concepts of interest [@vanderweele2017a; @vanderweele2020]. We categorised outcomes into five domains—health, psychological well-being, present-reflective outcomes, life-reflective outcomes, and social outcomes—based on validated scales and measures. Outcomes were based on those modelled in an earlier outcome-wide flourispaper @pedro_2024effects. @tbl-outcomes summarises each domain and its associated measures. For instance, health outcomes included BMI and hours of sleep, whereas psychological well-being included anxiety and depression. Outcomes were converted to z-scores (standardised), and the causal effect estimates may be therefore be interpreted as effect sizes.

|       Domain        |                 Dimension                |
  |---------------------|------------------------------------------------|
  |       Health        | BMI, Hours of Sleep, Hours of Exercise, Short Form Health |
  | Psychological Well-Being | Anxiety, Depression, Fatigue, Rumination      |
  | Present-Reflective  | Body Satisfaction, Forgiveness, Perfectionism, Self-Control, Self-Esteem, Sexual Satisfaction |
  | Life-Reflective     | Gratitude, Life Satisfaction, Meaning (Sense & Purpose), Personal Wellbeing Index |
  |       Social        | Social Belonging, Social Support, Neighbourhood Community |

: Outcome domains and example dimensions. Data summaries for all measures used in this study are provided in [Appendix {{appendix_outcomes}}](#appendix-outcomes). {#tbl-outcomes}"

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.outcomes.outcomewide_flourishing",
  value = outcomewide_flourishing_text
)


boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


outcomewide_personality_text <- "### Personality Outcomes

We adopt an outcome-wide approach, modelling every outcome in a domain. This strategy reduces cherry-picking and selectivity biases, and offers a meta-analytic perspective on the concepts of interest [@vanderweele2017a; @vanderweele2020]. We categorised personality outcomes into six domains based on the Mini-IPIP6 [@sibley2011]. @tbl-outcomes summarises each domain and its associated measures. For instance, extraversion includes being the life of the party and talking to different people at parties, whereas agreeableness includes sympathising with others' feelings and feeling others' emotions. Outcomes were converted to z-scores (standardised), and the causal effect estimates may therefore be interpreted as effect sizes.

| Domain | Items |
|--------|-------|
| Extraversion | Am the life of the party.  |
|  | Don't talk a lot. (R) |
|  | Keep in the background. (R) |
|  | Talk to a lot of different people at parties. |
| Agreeableness | Sympathize with others' feelings. |
|  | Am not interested in other people's problems. (R) |
|  | Feel others' emotions. |
|  | Am not really interested in others. (R) |
| Conscientiousness | Get chores done right away. |
|  | Like order. |
|  | Make a mess of things. (R) |
|  | Often forget to put things back in their proper place. (R) |
| Neuroticism/Emotional Stability | Have frequent mood swings. |
|  | Am relaxed most of the time. (R) |
|  | Get upset easily. |
|  | Seldom feel blue. (R) |
| Openness to Experience | Have a vivid imagination. |
|  | Have difficulty understanding abstract ideas. (R) |
|  | Do not have a good imagination. (R) |
|  | Am not interested in abstract ideas. (R) |
| Honesty-Humility | Feel entitled to more of everything. (R) |
|  | Deserve more things in life. (R) |
|  | Would like to be seen driving around in a very expensive car. (R) |
|  | Would get a lot of pleasure from owning expensive luxury goods. (R) |

: Personality domains and associated items from the Mini-IPIP6. (R) indicates reverse-scored items. Data summaries for all measures used in this study are provided in [Appendix {{appendix_outcomes}}](#appendix-outcomes). {#tbl-outcomes}"

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.outcomes.outcomewide_personality",
  value = outcomewide_personality_text
)
cat(unified_db$methods$outcomes$outcomewide_personality)

boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


# ------------------------------------------------------
# confounding control section
# ------------------------------------------------------
vanderweele_text <- "
### Confounding Control

To manage confounding in our analysis, we implement @vanderweele2019's *modified disjunctive cause criterion* by following these steps:

1. **Identified all common causes** of both the treatment and outcomes.
2. **Excluded instrumental variables** that affect the exposure but not the outcome. Instrumental variables do not contribute to controlling confounding and can reduce the efficiency of the estimates.
3. **Included proxies for unmeasured confounders** affecting both exposure and outcome. According to the principles of d-separation @pearl2009a, using proxies allows us to control for their associated unmeasured confounders indirectly.
4. **Controlled for baseline exposure** and **baseline outcome**. Both are used as proxies for unmeasured common causes, enhancing the robustness of our causal estimates, refer to @vanderweele2020.
"

unified_db$methods$confounding_control$vanderweele

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.confounding_control.vanderweele",
  value = vanderweele_text
)


boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)


# ------------------------------------------------------
# eligibility criteria section
# ------------------------------------------------------
eligibility_standard_text <- "
### Eligibility Criteria

To be included in the analysis of this study, participants needed to participate in the {{baseline_wave}} of the study and respond to the baseline measure of {{name_exposure_variable}}.

Participants may have been lost to follow-up at the end of the study if they met eligibility criteria at {{baseline_wave}}. We adjusted for attrition and non-response using censoring weights, described below.

A total of {{n_participants}} individuals met these criteria and were included in the study.
"


# if (!boilerplate_path_exists(unified_db$methods, "eligibility")) {
#   unified_db$eligibility <- list()
# }

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.eligibility.standard",
  value = eligibility_standard_text
)




# ------------------------------------------------------
# missing data handling section
# ------------------------------------------------------
missing_lmtp_simple_text <- "
### Missing Responses and Attrition

To mitigate bias from missing data, we used the following strategies:

#### Baseline missingness

We employed the `ppm` algorithm from the `mice` package in R [@vanbuuren2018] to impute missing baseline data (wave {{baseline_wave}}). This method allowed us to reconstruct incomplete datasets by estimating a plausible value for missing observation. Because we could only pass one data set to the lmtp, we employed single imputation. Approximately {{baseline_missing_data_proportion}}% of covariate values were missing at {{baseline_wave}}. We only used baseline data to impute baseline wave missingness (refer to @zhang2023shouldMultipleImputation).

#### Outcome missingness

To address confounding and selection bias arising from missing responses and panel attrition at the end of study {{outcome_wave}}, we applied censoring weights obtained using nonparametric machine learning ensembles afforded by the `lmtp` package (and its dependencies) in R [@williams2021]."

missing_lmtp_time_varying_text <- "
### Missing Data

To mitigate bias from missing data, we implement the following strategies:

#### Baseline missingness

We used predictive mean matching from the `mice` package [@vanbuuren2018] to impute missing baseline values (comprising `r percent_missing_baseline` of the baseline data). Following [@zhang2023shouldMultipleImputation], we performed single imputation using only baseline data. For each column with missing values, we created a binary indicator of missingness so that the machine learning algorithms we employed could condition on missingness information during estimation (see `lmtp` documentation [@williams2021]).

#### Missingness in Time-Varying Variables

When a time-varying value was missing in any wave but a future value was observed, we carried forward the previous response and included a missingness indicator. Again, this approach let the patterns of missingness inform nonparametric machine learning. If no future value was observed, we considered the participant censored and used inverse probability of treatment weights to address attrition.

#### Outcome missingness

To address confounding and selection bias arising from missing responses and panel attrition at the end of study {{outcome_wave}}, we applied censoring weights obtained using nonparametric machine learning ensembles afforded by the `lmtp` package (and its dependencies) in R [@williams2021]."


missing_grf_simple_text <- "
### Missing Data

The GRF package accepts missing values at baseline. To obtain valid inference for missing responses we computed inverse probability of censoring weights for censoring of the exposure, given that systematic censoring following the baseline wave may lead to selection bias that limit generalistion to the baseline target population [@bulbulia2024wierd]. See [Appendix {{appendix_explain_grf}}](#appendix-explain-grf)."


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
  value = missing_lmtp_simple_text
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.missing_data.missing_lmtp_time_varying",
  value = missing_lmtp_time_varying_text
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.missing_data.missing_grf_simple",
  value = missing_grf_simple_text
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.missing_data.missing_lmtp_time_varying",
  value = missing_lmtp_time_varying_text
)


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.exposure_indicator",
  value = methods_exposure_indicator_text
)


methods_exposure_indicator_text
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
lmtp_short_explanation_text <- "
We estimated the causal effect of {{name_exposure_variable}} on {{name_outcome_variable}}, adjusting for covariates. We used a 'modified treatment policy' (MTP) estimand [@haneuse2013estimation; @diaz2012population; @young2014identification; @diaz2023lmtp].

This estimand captures the difference in the expected outcome under two scenarios, assuming no censoring ($C=1$):

1.  **Observed exposure:** $\\E(Y^{C=1})$.
2.  **Hypothetical exposure (intervention):** $\\E(Y^{\\dd, C=1})$, where $\\dd(A)$ is a function that specifies how {{name_exposure_variable}} ($A$) is modified.

The MTP estimand is:

$$
\\E\bigl(Y^{\\dd, C=1}\bigr) \\;-\\; \\E\bigl(Y^{C=1}\bigr).
$$

We interpret this difference as the causal effect of changing {{name_exposure_variable}} according to $\\dd(\\cdot)$.

We used three identifying assumptions:

1.  **Conditional exchangeability:** Given observed covariates, there is no unmeasured confounding for the exposure–outcome and censoring–outcome relationships.

2.  **Positivity:** Every individual has a non-zero probability of receiving their observed exposure level and remaining uncensored, within each covariate stratum.

3.  **Consistency:** The observed outcome corresponds to the potential outcome for each individual's actual exposure history.

We estimated this parameter using a cross-fitted ($k = {{n_folds}}$), doubly robust, non-parametric targeted minimum loss-based estimator (TMLE) for MTPs [@diaz2023lmtp; @williams2023lmtp]. This procedure involves estimating:
- The outcome mechanism,
- The exposure mechanism, and
- The censoring mechanism.

We used the Super Learner ensemble algorithm [@polley2023; @vanderlaan2007super] with `SL.glmnet`, `SL.ranger`, and `SL.xgboost` as base learners [@friedman2010regularization; @wright2017ranger; @chen2016xgboost].

We produced all results, tables, and figures with the `margot` R package [@margot2024]. See [Appendix {{appendix_technical_lmtp}}](#appendix-technical-lmtp) for technical details."



lmtp_short_explanation_text_2 <-  "We used a 'modified treatment policy' (MTP) estimand [@haneuse2013estimation; @diaz2012population; @young2014identification; @diaz2023lmtp] to quantify the effect of {{name_exposure_variable}} on {{name_outcome_variable}}.

This estimand captures the difference in the expected outcome under two scenarios, assuming no censoring ($C=1$):

1.  **Observed exposure:** The expected outcome ($Y$) given the observed exposure distribution, $\\E(Y^{C=1})$.
2.  **Hypothetical exposure (intervention):** The expected outcome under a modified exposure distribution, $\\E(Y^{\\dd, C=1})$, where $\\dd(A)$ is a function specifying how {{name_exposure_variable}} ($A$) is hypothetically changed (i.e., {{shift_intervention}}).

The MTP estimand is the difference between these two expectations:

$$
\\E\bigl(Y^{\\dd, C=1}\bigr) \\;-\\; \\E\bigl(Y^{C=1}\bigr).
$$

We interpret this difference causally under three standard identifying assumptions:

1.  **Conditional Exchangeability:** Given the measured covariates ($L$), exposure assignment and censoring are independent of potential outcomes (i.e., no unmeasured confounding).
2.  **Positivity:** Within strata defined by covariates, every individual has a non-zero probability of receiving their observed exposure level and remaining uncensored.
3.  **Consistency:** An individual's observed outcome is the potential outcome corresponding to their actual exposure history.

We estimated this target parameter using a cross-fitted ($k = {{n_folds}}$), doubly robust, non-parametric targeted minimum loss-based estimator (TMLE) specifically designed for MTPs [@diaz2023lmtp; @williams2023lmtp]. This TMLE procedure requires estimating several components of the data-generating process, often called nuisance parameters:

- **Outcome mechanism:** The expected outcome given exposure and covariates, $\\E(Y | A, L)$.
- **Exposure mechanism:** The probability (or density) of receiving a specific exposure level given covariates, $P(A | L)$ (also related to the propensity score).
- **Censoring mechanism:** The probability of remaining uncensored ($C=1$) given exposure and covariates, $P(C=1 | A, L)$.

We estimated these mechanisms using the Super Learner ensemble algorithm [@polley2023; @vanderlaan2007super]. Super Learner combines predictions from multiple algorithms (base learners) to improve estimation accuracy. Our base learners included regularised regression (`SL.glmnet`), random forests (`SL.ranger`), and gradient boosting (`SL.xgboost`) [@friedman2010regularization; @wright2017ranger; @chen2016xgboost].

We produced all results, tables, and figures with the `margot` R package [@margot2024]. Please see [Appendix {{appendix_technical_lmtp}}](#appendix-technical-lmtp) for complete technical details."

# lmtp long
lmtp_long_explanation_text <- "
We aimed to estimate the effect of {{name_exposure_variable}} on {{name_outcome_variable}} using observational data. Because simple comparisons or standard regression may not fully capture the impact of real-world scenarios or adequately handle confounding and participant dropout (censoring), we used advanced causal inference methods.

Our specific goal was to answer a 'what if?' question: How would the average {{name_outcome_variable}} change if we could implement a specific modification to {{name_exposure_variable}} in the population, compared to leaving things as they are? We define this modification using a rule, denoted $\\dd(A)$, which specifies how an individual's exposure ($A$) would hypothetically be changed (e.g., {{shift_intervention}}). This type of 'what if?' question simulates the potential effect of a realistic policy or intervention and is known as a 'modified treatment policy' (MTP) estimand [@haneuse2013estimation; @diaz2012population; @young2014identification; @diaz2023lmtp].

Formally, we estimate the difference between the expected outcome ($Y$) under the hypothetical intervention ($\\dd$) and the expected outcome under the observed exposure distribution, assuming no one dropped out (censoring indicator $C=1$): $\\E(Y^{\\dd, C=1}) - \\E(Y^{C=1})$.

To ensure our estimate reflects a causal effect, rather than just correlation, we rely on three standard assumptions:

1.  **No Unmeasured Confounding (Conditional Exchangeability):** We assume that by accounting for the measured covariates, we have effectively controlled for all factors that influence both the exposure and the outcome, similar to how randomisation works in experiments. We also assume this holds for factors influencing dropout.

2.  **Sufficient Overlap (Positivity):** We assume that within groups of individuals with similar characteristics (based on covariates), there are people who experience the different relevant levels of exposure and non-dropout. This overlap is necessary to fairly compare the effects of the exposure.

3.  **Consistency:** We assume that the outcome we observed for an individual is precisely the outcome they would have experienced given their actual exposure history. This links the observed data to the hypothetical scenarios.

To calculate the effect estimate under these assumptions, we used a state-of-the-art statistical method called Targeted Minimum Loss-based Estimation (TMLE) using the `lmtp` package in R [@diaz2023lmtp; @williams2023lmtp]. TMLE is designed specifically for estimating causal effects like MTPs. Advantages include:

- **Double Robustness:** We obtain reliable estimate if *either* our statistical model for the outcome *or* our models for exposure and dropout are correctly specified, offering some protection against modelling errors.
- **Machine Learning Integration:** We can use of flexible machine learning algorithms to model the complex relationships between covariates, exposure, dropout, and the outcome, without specifying the functional form of these relationships, potentially biasing the final effect estimate.

We used an ensemble method called Super Learner to implement the machine learning component [@polley2023; @vanderlaan2007super]. Instead of relying on a single algorithm, Super Learner combines predictions from several (here: regularised regression via `SL.glmnet`, random forests via `SL.ranger`, and gradient boosting via `SL.xgboost` to improve prediction accuracy for the different parts of the TMLE calculation [@polley2023; @xgboost2023; @Ranger2017; @SuperLearner2023]. We also used {{n_folds}}-fold cross-fitting, a resampling technique that helps prevent overfitting and ensures the statistical validity of our results.

We generated results, tables, and figures using the `margot` R package [@margot2024]. For readers interested in the mathematical and computational specifics, please see [Appendix {{appendix_technical_lmtp}}](#appendix-technical-lmtp)."

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.lmtp_short_explanation",
  value = lmtp_short_explanation_text
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.lmtp_short_explanation_2",
  value = lmtp_short_explanation_text_2
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.lmtp_long_explanation",
  value = lmtp_long_explanation_text
)

boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)




# sdr short
sdr_short_explanation_text <- "
### Sequentially Doubly Robust (SDR) Estimator

We estimate causal effects of time-varying treatment policies using a Sequential Doubly Robust (SDR) estimator with the `lmtp` package [@williams2021; @díaz2021; @hoffman2024studying]. SDR involves two main steps. First, flexible machine learning models capture complex relationships among treatments, covariates, and outcomes [@díaz2021]. Second, SDR targets these initial fits to refine causal effect estimates. This design is multiply robust if treatments repeat over multiple waves [@diaz2023lmtp; @hoffman2024studying], ensuring consistency when either the outcome or treatment model is correct. We use `SuperLearner` [@SuperLearner2023] with `SL.ranger`, `SL.glmnet`, and `SL.xgboost` [@polley2023; @xgboost2023; @Ranger2017]. We use cross-validation to reduce overfitting and improve finite-sample performance. We create graphs, tables, and output with the `margot` package [@margot2024]."


sdr_long_explanation_text <- "
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

We use cross-validation to reduce overfitting and improve finite-sample performance. We implement SDR through the `lmtp` package [@williams2021; @hoffman2024studying; @diaz2023lmtp], relying on `SuperLearner` with base learners such as `SL.ranger`, `SL.glmnet`, and `SL.xgboost` [@polley2023; @xgboost2023; @Ranger2017; @SuperLearner2023]. For more details, see [@hoffman2022; @hoffman2024studying; @díaz2021]. We use the `margot` package [@margot2024] for reporting and visualisation.
"

grf_short_explanation_text <- "
### Statistical Estimation

We estimate heterogeneous treatment effects with Generalized Random Forests (GRF) [@grf2024]. GRF extends random forests for causal inference by focusing on conditional average treatment effects (CATE). It handles complex interactions and non-linearities without explicit model specification, and it provides 'honest' estimates by splitting data between model-fitting and inference. GRF is doubly robust because it remains consistent if either the outcome model or the propensity model is correct. We evaluate policies with the `policytree` package [@policytree_package_2024; @athey_2021_policy_tree_econometrica] and visualise results with `margot` [@margot2024]. (Refer to [Appendix {{appendix_explain_grf}}](#appendix-explain-grf) for a detailed explanation of our approach.)"



# make category
# unified_db$methods$statistical_models <- list()

# if (!boilerplate_path_exists(unified_db$methods, "eligibility")) {
#   unified_db$eligibility <- list()
# }

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.lmtp_short_explanation",
  value = lmtp_short_explanation_text
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.lmtp_long_explanation",
  value = lmtp_long_explanation_text
)


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.sdr_short_explanation",
  value = sdr_short_explanation_text
)


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.sdr_long_explanation",
  value = sdr_long_explanation_text
)


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.statistical_models.grf_short_explanation",
  value = grf_short_explanation_text
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
# to revise

general_approach_cate_long_text <- "
### Moderators and treatment policies

We pursued two complementary objectives: (i) to test whether personalised targeting, based on individual conditional average treatment effects $\\hat\\tau(x)$, yields welfare gains, and (ii) to convert any such gains into transparent, practitioner‐ready decision rules.

#### Pre-processing and honest model training

Our protocol is as follows: in settings where the exposure is positive, outcomes for which lower is better are sign‐flipped so that larger values always index improvement. Similarly, where the exposure is negative, outcomes for which 'higher is better' are sign flipped. Here, we flipped: {{flipped_list}}.

Each model estimate used an honest {{sample_ratio_policy}} split: the training fold built the causal forest with grf [@grf2024], while the held-out fold powered all diagnostic checks and provided data for fitting policy trees.  This separation curbs over-fitting yet keeps the workflow simple.

#### Budget-based screening with Qini curves.

Before constructing rules we asked a budget question: If resources allow treatment of only the top 20% or 50% of individuals ranked by $\\hat\\tau(x)$, what uplift is purchased relative to treating everyone?
Qini curves quantified the incremental gain; outcomes whose 95% confidence intervals excluded zero at either spend level were labelled actionable, signalling meaningful heterogeneity in benefit.

#### Deriving transparent decision rules.

For each actionable outcome we trained depth-2 policy trees with policytree [@policytree_package_2024; @athey_2021_policy_tree_econometrica] on the validation data.  The resulting if–then statements maximise expected welfare under the same budget cap and remain auditable by domain experts.

####  Global heterogeneity tests (supplementary).

Appendix {{appendix_rate}} reports RATE-AUTOC and RATE-Qini statistics, asking whether any covariate information can beat a uniform policy.  These tests, controlled for false discovery at q={{cate_alpha}} via {{cate_adjustment}}, are informative but not required for the budget-first pipeline.

Overall, combining Qini curves to screen and shallow policy trees to act isolates budget-relevant treatment heterogeneity and distils it into actionable rules, avoiding the chase for spurious complexity. Full technical details appear in Appendix {{appendix_explain_grf}}.
"


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.general_approach_cate_long",
  value = general_approach_cate_long_text
)

general_approach_cate_short_text <- "
### Moderators and treatment policies

We asked whether individualised targeting, guided by conditional average treatment effects $\\hat\\tau(x)$, can out-perform a one-size-fits-all strategy, and how any gains might be converted into rules that practitioners trust.

#### Pre-processing and honest forests

Direction was standardised by sign-flipping outcomes whose valence opposed that of the exposure; the affected measures were {{flipped_list}}.  Each analysis used an honest {{sample_ratio_policy}} split: the training fold built a causal forest with grf [@grf2024], while the held-out fold supplied every diagnostic and served as the data set for learning policy trees.  This partition keeps each estimate out-of-sample and protects against over-fitting.

#### Diagnostics

On the evaluation fold we first verified forest calibration, then computed RATE-AUTOC and RATE-Qini statistics, formal tests of whether any covariate information can beat uniform treatment [@wager2018].  p-values were adjusted by the Benjamini–Hochberg procedure at q={{cate_alpha}} using {{cate_adjustment}} [@benjamini1995controlling]. Appendix {{appendix_rate}} reports RATE-AUTOC and RATE-Qini statistics.

#### Budget-focused evidence

Because programme budgets are finite, we next asked: If resources permit treating only the top 20 % or 50 % ranked by $\\hat\\tau(x)$, what uplift should planners expect?  Qini curves answer this question and flag an outcome as actionable when the 95% confidence interval for incremental gain excluded zero at either spending level.

Note that RATE and Qini provide complementary lenses—global versus budget-specific evidence -- and either can justify the move to rule learning.

#### Transparent decision rules

For each actionable outcome we fitted a depth-2 policy tree with policytree on the validation data [@policytree_package_2024; @athey_2021_policy_tree_econometrica].  The tree yields an if–then allocation rule that maximises expected welfare under the chosen budget cap and remains fully auditable.

The workflow—forest $\\rightarrow$ diagnostics $\\rightarrow$ tree—identifies meaningful heterogeneity, quantifies the payoff to targeting, and delivers concise decision rules that practitioners can implement.  Details of all algorithms appear in Appendix {{appendix_explain_grf}}."



general_approach_cate_long_no_flip_text <- "
### Moderators and treatment policies

We pursued two complementary objectives: (i) to test whether personalised targeting, based on individual conditional average treatment effects $\\hat\\tau(x)$, yields welfare gains, and (ii) to convert any such gains into transparent, practitioner‐ready decision rules.

#### Pre-processing and honest model training

Each model estimate used an honest {{sample_ratio_policy}} split: the training fold built the causal forest with grf [@grf2024], while the held-out fold powered all diagnostic checks and provided data for fitting policy trees.  This separation curbs over-fitting yet keeps the workflow simple.

#### Budget-based screening with Qini curves.

Before constructing rules we asked a budget question: If resources allow treatment of only the top 20% or 50% of individuals ranked by $\\hat\\tau(x)$, what uplift is purchased relative to treating everyone?
Qini curves quantified the incremental gain; outcomes whose 95% confidence intervals excluded zero at either spend level were labelled actionable, signalling meaningful heterogeneity in benefit.

#### Deriving transparent decision rules.

For each actionable outcome we trained depth-2 policy trees with policytree [@policytree_package_2024; @athey_2021_policy_tree_econometrica] on the validation data.  The resulting if–then statements maximise expected welfare under the same budget cap and remain auditable by domain experts.

####  Global heterogeneity tests (supplementary).

Appendix {{appendix_rate}} reports RATE-AUTOC and RATE-Qini statistics, asking whether any covariate information can beat a uniform policy.  These tests, controlled for false discovery at q={{cate_alpha}} via {{cate_adjustment}}, are informative but not required for the budget-first pipeline.

Overall, combining Qini curves to screen and shallow policy trees to act isolates budget-relevant treatment heterogeneity and distils it into actionable rules, avoiding the chase for spurious complexity. Full technical details appear in Appendix {{appendix_explain_grf}}.
"

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.general_approach_cate_long_no_flip",
  value = general_approach_cate_long_no_flip_text
)


general_approach_cate_short_no_flip_text <- "
### Moderators and treatment policies

We asked whether individualised targeting, guided by conditional average treatment effects $\\hat\\tau(x)$, can out-perform a one-size-fits-all strategy, and how any gains might be converted into rules that practitioners trust.

#### Pre-processing and honest forests

Each analysis used an honest {{sample_ratio_policy}} split: the training fold built a causal forest with grf [@grf2024], while the held-out fold supplied every diagnostic and served as the data set for learning policy trees.  This partition keeps each estimate out-of-sample and protects against over-fitting.

#### Diagnostics

On the evaluation fold we first verified forest calibration, then computed RATE-AUTOC and RATE-Qini statistics, formal tests of whether any covariate information can beat uniform treatment [@wager2018].  p-values were adjusted by the Benjamini–Hochberg procedure at q={{cate_alpha}} using {{cate_adjustment}} [@benjamini1995controlling]. Appendix {{appendix_rate}} reports RATE-AUTOC and RATE-Qini statistics.

#### Budget-focused evidence

Because programme budgets are finite, we next asked: If resources permit treating only the top 20 % or 50 % ranked by $\\hat\\tau(x)$, what uplift should planners expect?  Qini curves answer this question and flag an outcome as actionable when the 95% confidence interval for incremental gain excluded zero at either spending level.

Note that RATE and Qini provide complementary lenses—global versus budget-specific evidence -- and either can justify the move to rule learning.

#### Transparent decision rules

For each actionable outcome we fitted a depth-2 policy tree with policytree on the validation data [@policytree_package_2024; @athey_2021_policy_tree_econometrica].  The tree yields an if–then allocation rule that maximises expected welfare under the chosen budget cap and remains fully auditable.

The workflow—forest $\\rightarrow$ diagnostics $\\rightarrow$ tree—identifies meaningful heterogeneity, quantifies the payoff to targeting, and delivers concise decision rules that practitioners can implement.  Details of all algorithms appear in Appendix {{appendix_explain_grf}}."


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.general_approach_cate_short_no_flip",
  value = general_approach_cate_short_no_flip_text
)

simple_general_approach_cate_short_text <- "
### Learning Moderators and Deriving Practical Treatment Rules

After estimating the average treatment effect, we asked *for whom* the exposure helps and *how* to act on that knowledge.  The workflow:

1. **Orient outcomes.** All scales were re-coded so that higher scores had the same valence as the exposure (flipped: {{flipped_list}}).
2. **Honest causal forest.** A {{sample_ratio_policy}} split trained the forest and produced out-of-sample CATEs $\\hat\\tau(x)$ on the validation fold [@grf2024].
3. **Global heterogeneity test.** On the validation fold we computed **RATE-AUTOC** and **RATE-Qini**; FDR was controlled with {{cate_adjustment}} at q = {{cate_alpha}} [@benjamini1995controlling] (see Appendix {{appendix_rate}}).
4. **Budget lens.** **Qini curves** compared “treat top–ranked” to “treat all”, revealing expected uplift at budget caps (Appendix {{appendix_qini_curve}}).
5. **Transparent policy.** When heterogeneity looked actionable, we fitted depth-2 **policy trees** on the validation fold, turning the black-box forest into concise *if–then* rules (details in Appendix {{appendix_explain_grf}}).

This pipeline converts complex CATE estimates into interpretable, out-of-sample decision policies while controlling both over-fitting and multiple testing."


simple_general_approach_cate_long_no_flip_text<- "
### Learning Moderators and Deriving Practical Treatment Rules

After estimating the average treatment effect, we asked *for whom* the exposure helps and *how* to act on that knowledge.  The workflow:

1. **Orient outcomes.** All scales were re-coded so that higher scores had the same valence as the exposure (flipped: {{flipped_list}}).
2. **Honest causal forest.** A {{sample_ratio_policy}} split trained the forest and produced out-of-sample CATEs $\\hat\\tau(x)$ on the validation fold [@grf2024].
3. **Global heterogeneity test.** On the validation fold we computed **RATE-AUTOC** and **RATE-Qini**; FDR was controlled with {{cate_adjustment}} at q = {{cate_alpha}} [@benjamini1995controlling] (see Appendix {{appendix_rate}}).
4. **Budget lens.** **Qini curves** compared “treat top–ranked” to “treat all”, revealing expected uplift at budget caps (Appendix {{appendix_qini_curve}}).
5. **Transparent policy.** When heterogeneity looked actionable, we fitted depth-2 **policy trees** on the validation fold, turning the black-box forest into concise *if–then* rules (details in Appendix {{appendix_explain_grf}}).

This pipeline converts complex CATE estimates into interpretable, out-of-sample decision policies while controlling both over-fitting and multiple testing."


simple_general_approach_cate_short_no_flip_text <- "
### Moderators and Treatment Policies

We used a method called 'causal forests' to check if a treatment helped some people more than others. We trained the model on half the data and tested it on the other half. This helped us understand whether the differences we found were real rather than accidental. We then compared outcomes when we targeted treatment to those predicted to benefit the most (using Qini curves) against simply giving the treatment to everyone. Finally, we used policy trees to boil down these results into simple, if-then rules for deciding who's likely to benefit most from the treatment (refer to[Appendix {{appendix_explain_grf}}](#appendix-explain-grf))."



# ** PREFERRED
unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.general_approach_cate_short",
  value = general_approach_cate_short_text
)

# unified_db<- boilerplate_update_entry(
#   db = unified_db,
#   path = "methods.analytic_approach.simple_general_approach_cate_long",
#   value = simple_general_approach_cate_long_text
# )


unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.simple_general_approach_cate_short",
  value = simple_general_approach_cate_short_text
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.simple_general_approach_cate_long",
  value = simple_general_approach_cate_short_text
)



unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.simple_general_approach_cate_long_no_flip",
  value = simple_general_approach_cate_long_no_flip_text
)

unified_db<- boilerplate_update_entry(
  db = unified_db,
  path = "methods.analytic_approach.simple_general_approach_cate_short_no_flip",
  value = simple_general_approach_cate_short_no_flip_text
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

unified_db$methods$analytic_approach$general_approach_cate_short
# sensitivity analysis ----------------------------------------------------
# add a new custom text entry
short_evalue_text <- "
### Sensitivity Analysis
We perform sensitivity analyses using the E-value metric [@vanderweele2017; @linden2020EVALUE]. The E-value represents the minimum association strength (on the risk-ratio scale) that an unmeasured confounder would need with both exposure and outcome—after adjusting for measured covariates—to explain away the observed association [@vanderweele2020; @linden2020EVALUE]. Confidence intervals for each E-value were derived from the multiplicity-adjusted confidence intervals of the corresponding coefficient estimates ({{ate_adjustment}}, $\\alpha$ = {{ate_alpha}}), so the sensitivity analysis obeys the same error-control framework as the main results."


# unified_db <- boilerplate_remove_entry(
#   db = unified_db,
#   path = "methods.sensitivity_analysis.evalue")



unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "methods.sensitivity_analysis.short_evalue",
  value = short_evalue_text)


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




# Results -----------------------------------------------------------------





interpretation_ominibus_test_negative_text <-  "
#### Omnibus Test

The omnibus test did not provide statistically reliable evidence for overall treatment effect heterogeneity beyond chance. However, omnibus tests can lack power for detecting subtle or localised heterogeneity. Therefore, we examined more specific indicators of potential targeting benefits and subgroup differences. Refer to [Appendix {{appendix_cate_validation_grf}}](#appendix-cate-validation)."

interpretation_ominibus_test_positive_text <- "
#### Omnibus Test

The omnibus test (@tbl-omnibus) indicates that the model found differences in how individuals respond to {{omnibus_confirmed_heterogeneity_outcome}}.  Notably, omnibus tests can lack power for detecting subtle or localised heterogeneity. However, even with its anti-conservativism, the test confirmed heterogeneity for {{omnibus_confirmed_heterogeneity_outcome}}. Refer to [Appendix {{appendix_cate_validation_grf}}](#appendix-cate-validation)."




interpretation_rate_text <- "
#### Rate Test

The RATE metric shows how much extra gain (or avoided loss) we achieve by **targeting** instead of treating everyone identically.

**Technical note**: In code we always set `policy = \"treat_best\"`; for harmful exposures this is interpreted as *'treat-those-most-sensitive'* (i.e., prioritise protection or withholding).

* **Beneficial exposure:** we rank by positive CATEs and deliver the exposure to those predicted to **benefit most**.
* **Detrimental exposure:** we rank by increasingly **positive** CATEs (more predicted harm) and identify those who should be protected or withheld from the exposure.

Either way, a larger **absolute** RATE shows that a CATE-based targeting rule 'outperforms' a one-size-fits-all policy—by boosting outcomes for beneficial exposures or -- in the case where we are explore sensitivity to harm -- evaluating increasing harms for detrimental ones.

Recall we flipped {{flipped_list}} so **'higher' always tracks the analysis goal: higher = more benefit for beneficial exposures, higher = more harm for detrimental exposures.**

Because we test several outcomes, RATE *p*-values are adjusted with {{cate_adjustment}} (q = {{cate_alpha}}) before we decide whether heterogeneity is actionable."

interpretation_rate_no_flip_text <- "
#### Rate Test

The RATE metric shows how much extra gain in the outcome we achieve by **targeting** instead of treating everyone identically.

* **Beneficial exposure:** we rank by positive CATEs and deliver the exposure to those predicted to **benefit most**.
* **Detrimental exposure:** we rank by increasingly **positive** CATEs (more predicted harm) and identify those who should be protected or withheld from the exposure.

(Note the valence of the outcomes match the valence of the exposures.)

Either way, a larger **absolute** RATE shows that a CATE-based targeting rule 'outperforms' a one-size-fits-all policy—by boosting outcomes for beneficial exposures or -- in the case where we are explore sensitivity to harm -- evaluating increasing harms for detrimental ones.

Because we test several outcomes, RATE *p*-values are adjusted with {{cate_adjustment}} (q = {{cate_alpha}}) before we decide whether heterogeneity is actionable."

interpretation_qini_text <- "
#### Qini Curves

The Qini curve shows the cumulative **gain** as we expand a targeting rule down the CATE ranking.

* **Beneficial exposure:** we add individuals from the top positive CATEs downward; the baseline is 'expose everyone.'
* **Detrimental exposure:** we first flip outcome direction (so higher values represent **more harm**; see {{flipped_list}}), then *add* the exposure starting with individuals whose CATEs show the **greated harm**, gradually including those predicted to be more resistant to harm; the baseline is 'expose everyone.'  The curve therefore quantifies the harm by when those most suceptible to harm are exposed.

If the Qini curve stays above its baseline, a targeted policy increases the outcome more than a one-size-fits-all alternative."
#
# (Outcome directions were flipped where needed—{{flipped_list}}—so the positively valenced exposures always have positively valanced outcomes and negative exposures always have negatively valenced outcomes.)


interpretation_policy_tree_text <- "
#### Policy Trees

We used policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] to find straightforward ‘if-then’ rules for who benefits most from treatment, based on participant characteristics. Because we flipped some measures, a higher predicted effect always means greater improvement. Policy trees can uncover small but important subgroups whose treatment responses stand out, even when the overall differences might be modest."




results_long_outcomewide_flourishing_2025 <- "

## Results

### Health

```{r}
#| label: fig-health
#| fig-cap: \"Health effects\"
#| eval: true
#| echo: false
#| fig-height: 16
#| fig-width: 16

health_religious_vs_secular$plot
```

```{r}
#| label: tbl-health
#| tbl-cap: \"Health effects\"
#| eval: true
#| echo: false

health_religious_vs_secular$transformed_table|>
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  kbl(format = \"markdown\")
```


```{r, results = 'asis'}
cat(health_religious_vs_secular$interpretation)
```

{{< pagebreak >}}

### Psychological Well-Being

```{r}
#| label: fig-psych
#| fig-cap: \"Effects on Psychological Well-Being\"
#| eval: true
#| echo: false
#| fig-height: 16
#| fig-width: 16

psych_religious_vs_secular$plot
```

```{r}
#| label: tbl-psych
#| tbl-cap: \"Effects on Psychological Well-Being\"
#| eval: true
#| echo: false

psych_religious_vs_secular$transformed_table|>
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  kbl(format = \"markdown\")
```


```{r, results = 'asis'}
cat(psych_religious_vs_secular$interpretation)
```

{{< pagebreak >}}

### Present-Focussed Well-Being

```{r}
#| label: fig-present
#| fig-cap: \"Effects on Person-Focussed Well-Being\"
#| eval: true
#| echo: false
#| fig-height: 16
#| fig-width: 16

present_religious_vs_secular$plot
```

```{r}
#| label: tbl-present
#| tbl-cap: \"Effects on Person-Focussed Well-Being\"
#| eval: true
#| echo: false

present_religious_vs_secular$transformed_table|>
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  kbl(format = \"markdown\")
```


```{r, results = 'asis'}
cat(present_religious_vs_secular$interpretation)
```


{{< pagebreak >}}

### Life-Focussed Well-Being

```{r}
#| label: fig-life
#| fig-cap: \"Effects on Life-Focussed Well-Being\"
#| eval: true
#| echo: false
#| fig-height: 16
#| fig-width: 16

life_religious_vs_secular$plot
```

```{r}
#| label: tbl-life
#| tbl-cap: \"Effects on Life-Focussed Well-Being\"
#| eval: true
#| echo: false

life_religious_vs_secular$transformed_table|>
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  kbl(format = \"markdown\")
```


```{r, results = 'asis'}
cat(life_religious_vs_secular$interpretation)
```


{{< pagebreak >}}

### Social-Focussed Well-Being

```{r}
#| label: fig-social
#| fig-cap: \"Effects on Life-Focussed Well-Being\"
#| eval: true
#| echo: false
#| fig-height: 16
#| fig-width: 16

social_religious_vs_secular$plot
```

```{r}
#| label: tbl-social
#| tbl-cap: \"Effects on Social Well-Being\"
#| eval: true
#| echo: false

social_religious_vs_secular$transformed_table|>
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  kbl(format = \"markdown\")
```


```{r, results = 'asis'}
cat(social_religious_vs_secular$interpretation)
```
"



unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "results.grf.interpretation_ominibus_test_negative",
  value = interpretation_ominibus_test_negative_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "results.grf.interpretation_ominibus_test_positive",
  value = interpretation_ominibus_test_positive_text
)
cat(unified_db$results$grf$interpretation_ominibus_test_positive)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "results.grf.interpretation_rate",
  value = interpretation_rate_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "results.grf.interpretation_qini",
  value = interpretation_qini_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "results.grf.interpretation_policy_tree",
  value = interpretation_policy_tree_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "results.grf.interpretation_rate_no_flip",
  value = interpretation_rate_no_flip_text
)

# init
# unified_db <- boilerplate_update_entry(
#   db = unified_db,
#   path = "results.interpretation_rate_no_flip_text",
#   value = results_long_outcomewide_flourishing_2025
# )


# Finally, save the database
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)

# To verify the changes
unified_db <- boilerplate_import(data_path = my_project_path)
# Check one entry to verify it's been saved correctly
cat(boilerplate_get_entry(unified_db, "methods.missing_data.missing_lmtp_simple"))




# Discussion --------------------------------------------------------------

strengths_grf_long_text <- "
### Strengths and Limitations of Our Approach

We used causal forests [@grf2024]—a statistical method designed to estimate Conditional Average Treatment Effects (CATEs)—to investigate how treatment effectiveness might vary across individuals with different characteristics. This strategy offers several advantages over simpler regression models, yet it also comes with important caveats.

#### Potential Limitations

First, our results depend heavily on having measured all key variables that affect both who receives treatment and how strongly they respond. If important factors are missing, the estimated differences in treatment effects may be biased. Selecting which characteristics to include in the model is also critical: omitting relevant factors or including inappropriate ones can yield misleading conclusions. Any further analyses that rely on these estimates—such as deciding whom to treat first—will inherit the same potential biases.

Second, interpreting subgroup effects can be challenging when many characteristics are considered simultaneously. Because these estimates are conditional on all factors in the model, a statistically significant finding for one subgroup may not necessarily hold when additional characteristics vary. Moreover, even if the model detects genuine differences, small or niche effects might not translate into meaningful real-world improvements.

#### Strengths

Despite these considerations, our approach provides a powerful way to uncover and potentially leverage treatment effect heterogeneity. Causal forests [@grf2024] do not assume a simple linear or additive structure, enabling them to detect complex interactions among diverse covariates. To ensure the findings are not just statistical artifacts, we used a {{sample_split}} sample split: one half of the data to build the model and the other to test its predictions on unseen data. This guards against overfitting and gives a more reliable indication of how well the model generalises.

We also explicitly assess the reliability and practical value of the predicted differences in treatment effects. We perform statistical checks—such as calibration and differential prediction tests [@grf2024]—to determine whether these variations are genuine rather than noise. Additionally, the Rank-Weighted Average Treatment Effect (RATE) [@grf2024; @wager2018] estimates how much we might improve outcomes if we treat only the individuals predicted to benefit most, instead of treating everyone equally. To visualise this, Qini curves [@grf2024] show how much extra benefit accumulates as we expand treatment from the top-ranked individuals to a larger portion of the population. Finally, policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] translate these insights into straightforward ‘if-then’ rules based on key baseline characteristics, making the findings more accessible for real-world decision-making.

In sum, this combination of flexible modelling, rigorous testing, and practical tools for targeting treatment offers a robust framework for studying and applying treatment effect heterogeneity. Nonetheless, these benefits hinge on the completeness of our data and the accuracy of our assumptions.
"

# Short Version
strengths_grf_short_text <- "
### Strengths and Limitations of Our Approach

We used causal forests [@grf2024] to estimate how treatment effects may differ for individuals with different characteristics. This method is powerful, however it also depends on measuring all major variables influencing both treatment selection and outcomes. If such variables are missed or mismeasured, results can be biased. Additionally, interpreting subgroup effects can be tricky when many characteristics are involved and statistically significant differences may not always translate into meaningful real-world gains.

Despite these concerns, causal forests offer notable advantages. They allow for flexible, non-parametric modelling [@grf2024], avoiding strict assumptions that might miss complex interactions. We used a robust evaluation method—training our model on half the data and testing it on the remaining half—to avoid overfitting. We then checked whether the predicted differences were genuine and estimated how much benefit we might gain by targeting treatment to those likely to benefit most [@grf2024; @wager2018]. Qini curves [@grf2024] let us see the overall improvement from treating the top-ranked individuals first, and policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] turn these findings into simple ‘if-then’ rules. Together, this approach provides a practical means of identifying and acting on genuine treatment effect differences."

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.strengths.strengths_grf_short",
  value = strengths_grf_short_text
)

nzavs_ethics_2021_2027_text <- "
### Ethics

The University of Auckland Human Participants Ethics Committee reviews the NZAVS every three years. Our most recent ethics approval statement is as follows: The New Zealand Attitudes and Values Study was approved by the University of Auckland Human Participants Ethics Committee on 26/05/2021 for six years until 26/05/2027, Reference Number UAHPEC22576."


student_nzavs_ethics_2021_2027_text <- "
### Ethics

The data were simulated for the purposes of instruction. The Ethics Approval for the dataset from which the synthetic data were generated is as follows: The University of Auckland Human Participants Ethics Committee reviews the NZAVS every three years. The most recent ethics approval statement is as follows: The New Zealand Attitudes and Values Study was approved by the University of Auckland Human Participants Ethics Committee on 26/05/2021 for six years until 26/05/2027, Reference Number UAHPEC22576."


nzavs_data_availabily_text <- "
### Data Availability

The data described in the paper are part of the New Zealand Attitudes and Values Study. Members of the NZAVS management team and research group hold full copies of the NZAVS data. A de-identified dataset containing only the variables analysed in this manuscript is available upon request from the corresponding author or any member of the NZAVS advisory board for replication or checking of any published study using NZAVS data. The code for the analysis can be found at [OSF link](https://osf.io/ab7cx/)."


student_nzavs_data_availabily_text <- "
### Data Availability

The data described in the paper were *simulated* from the New Zealand Attitudes and Values Study (NZAVS). For more information contact professor Joseph Bulbulia at joseph.bulbulia@vuw.ac.nz. For more information about the NZAVS, see: [OSF link](https://osf.io/ab7cx/)."

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.student_data",
  value = student_nzavs_data_availabily_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.student_ethics",
  value = student_nzavs_ethics_2021_2027_text
)

nzavs_acknowledgements_2025_text <- "
### Acknowledgements

The New Zealand Attitudes and Values Study is supported by a grant from the Templeton Religious Trust (TRT0196; TRT0418). JB received support from the Max Plank Institute for the Science of Human History. The funders had no role in preparing the manuscript or deciding to publish it."




authors_statment_empty_text <-"
### Author Statement


"


sudent_authors_statment_empty_text <-"
### Author Statement

The Methods and Results section were created using standard protocols from the EPIC lab (Joseph Bulbulia). These were encoded using the `boilerplate` package [@boilerplate2024] and `margot package` [@margot2024] in R. The introduction and conclusion for this research are solely the student's work."

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.student_authors_statement",
  value = sudent_authors_statment_empty_text
)



# check entries
# unified_db <- boilerplate_add_entry(
#   db = unified_db,
#   path = "discussion.ethics",
#   value = list()
# )


# unified_db <- boilerplate_add_entry(
#   db = unified_db,
#   path = "discussion.acknowlegements",
#   value = list()
# )

# unified_db <- boilerplate_add_entry(
#   db = unified_db,
#   path = "discussion.authors_statement",
#   value = list()
# )

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.acknowlegements.nzavs_acknowledgements_2025",
  value = nzavs_acknowledgements_2025_text
)


unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.ethics.nzavs_2021_2027",
  value = nzavs_ethics_2021_2027_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.nzavs_acknowledgements_2025",
  value = nzavs_acknowledgements_2025_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.nzavs_acknowledgements_2025",
  value = nzavs_acknowledgements_2025_text
)



unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.authors_statment_empty",
  value = authors_statment_empty_text
)



# add path
# if (!boilerplate_path_exists(unified_db$discussion, "strengths")) {
#   unified_db$discussion$strengths <- list()
# }

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.strengths.strengths_grf_short",
  value = strengths_grf_short_text
)


unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.strengths.strengths_grf_long",
  value = strengths_grf_long_text
)

# unified_db <- boilerplate_update_entry(
#   db = unified_db,
#   path = "discussion.strengths.simple_general_approach_cate_long",
#   value = simple_general_approach_cate_long_text
# )

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "discussion.authors_statement.empty",
  value = authors_statment_empty_text
)

# save
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)

# boilerplate_import(unified_db, data_path = my_project_path, create_backup = FALSE)


# Appendix  ---------------------------------------------------------------

appendix_timeline_text <- "
## Appendix {{appendix_timeline}}: Daily Data Collection  {#appendix-timeline}

{{< pagebreak >}}

@fig-timeline presents the New Zealand Attitudes and Values Study Data Collection {{baseline_wave}} retained cohort from {{study_years}}.

```{r}
#| label: fig-timeline
#| fig-cap: \"Historgram of New Zealand Attitudes and Values Study Daily Data Collection for {{baseline_wave}} cohort: years 2018-2024.\"
#| eval: true
#| include: true
#| echo: false
#| fig-width: 12
#| fig-height: 12
#|
timeline_histogram
```
"
#check
cat(appendix_timeline_text)


appendix_baseline_text <- "
## Appendix B: Measures and Demographic Statistics {#appendix-baseline}

### Measures

```{r, results='asis'}
cat(appendix_text_all_measures) # make this with boilerplate_measures_text
```

### Sample Demographic Statistics

@tbl-appendix-baseline presents sample demographic statistics.

::: {#tbl-appendix-baseline}
```{r, results = 'asis'}
#| eval: true
#| include: true
#| echo: false

cat(latex_table_baseline)

```
Demographic statistics for New Zealand Attitudes and Values Cohort {{baseline_wave}}.
:::
"


appendix_exposure_text <- "
### Exposure Variable: {{name_exposure_variable}}  {#appendix-exposure}

@tbl-sample-appendix-exposures presents sample statistics for the {{name_exposure_variable}} during the {{baseline_wave}} and {{exposure_waves}}.

::: {#tbl-appendix-exposures}
```{r, results = 'asis'}
#| eval: true
#| include: true
#| echo: false

cat(latex_table_exposures)

```
Demographic statistics for New Zealand Attitudes and Values Cohort {{baseline_wave}}.
:::
"
cat(appendix_exposure_text)




appendix_outcomes_flourishing_2025_text <-  "
## Outcome Variables {#appendix-outcomes}

@tbl-appendix-outcomes presents sample outcomes at baseline and the end of study.

::: {#tbl-appendix-outcomes}
```{r, results = 'asis'}
#| eval: true
#| include: true
#| echo: false


print(markdown_table_outcomes_all)

```
Outcome variables measured at baseline and end of study.
:::
"


# check entries
boilerplate_get_entry(
  db = unified_db,
  path = "discussion"
)


appendix_outcomes_text <-  "
## Outcome Variables {#appendix-outcomes}

@tbl-appendix-outcomes presents sample outcomes at baseline and the end of study.

::: {#tbl-appendix-outcomes}
```{r, results = 'asis'}
#| eval: true
#| include: true
#| echo: false


print(markdown_table_outcomes_all)

```
Outcome variables measured at baseline and end of study.
:::
"


# check entries
boilerplate_get_entry(
  db = unified_db,
  path = "discussion"
)


appendix_confounding_lmtp_swig_text <- "
## Appendix {{appendix_confounding}}: Confouding Control {#appendix-confounding}

::: {#tbl-C}
```{=latex}
\tvtable
```
@tbl-C presents single-world intervention graphs showing time-fixed and time-varying sources of bias in our six waves (baseline, four exposure waves, followed by the outcome wave.) Time-fixed confounders are included in the baseline wave. Time-varying confounders are included in each of the four treatment waves (abbreviated here by '$\\dots$' to declutter the graph). When there is more than one exposure wave, identifying causal effects requires adjustment for time-varying confounders [@robins2008estimation; @bulbulia2024swigstime; @richardson2013].
:::

For confounding control, we employ a modified disjunctive cause criterion [@vanderweele2019], which involves:


1.	Identifying all common causes of both the treatment and outcomes.
2.	Excluding instrumental variables that affect the exposure but not the outcome.
3.	Including proxies for unmeasured confounders affecting both exposure and outcome.
4.	Controlling for baseline exposure and baseline outcome, serving as proxies for unmeasured common causes [@vanderweele2020].

Additionally, we control for time-varying confounders at each exposure wave [@robins2008estimation; @bulbulia2024swigstime; @richardson2013].

The covariates included for confounding control are described in @pedro_2024effects.

Where there are multiple exposures, causal inference may be threatened by time-varying confounding [@bulbulia2024swigstime]."

cat(appendix_confounding_lmtp_swig_text)
appendix_confounding_lmtp_dag_text <- "
## Appendix {{appendix_confounding}}: Confouding Control {#appendix-confounding}

::: {#tbl-C}
```{=latex}
\\feedbackB
```
@tbl-C Common cause of Treatment 1 and downstream confounder of Treatment 2 is a collider. To handle time-varying confounding we include time-fixed confounders in the baseline wave. Time-varying confounders are included in each of the successive treatment waves. When there is more than one exposure wave, identifying causal effects requires adjustment for time-varying confounders [@robins2008estimation; @bulbulia2024swigstime; @richardson2013].
:::

For confounding control, we employ a modified disjunctive cause criterion [@vanderweele2019], which involves:


1.	Identifying all common causes of both the treatment and outcomes.
2.	Excluding instrumental variables that affect the exposure but not the outcome.
3.	Including proxies for unmeasured confounders affecting both exposure and outcome.
4.	Controlling for baseline exposure and baseline outcome, serving as proxies for unmeasured common causes [@vanderweele2020].

Additionally, we control for time-varying confounders at each exposure wave [@robins2008estimation; @bulbulia2024swigstime; @richardson2013].

The covariates included for confounding control are described in @pedro_2024effects."

appendix_confounding_threewave_x_text <- "
##Appendix {{appendix_confounding}}: Confouding Control {#appendix-confounding}

::: {#tbl-confounding}
```{=latex}
\\threewavepanelX
```
The Causal Directed Acyclic Graphs (DAGS) report the causal identification problems in our three-wave studyand and or methods for addressing them solution.
:::

@tbl-confounding describes our strategy for confounding control, in which we employ a modified disjunctive cause criterion [@vanderweele2019], which involves:


1.	Identifying all common causes of both the treatment and outcomes.
2.	Excluding instrumental variables that affect the exposure but not the outcome.
3.	Including proxies for unmeasured confounders affecting both exposure and outcome.
4.	Controlling for baseline exposure and baseline outcome (@tbl-confounding) addresses the strongest confounders, given any residual confounding would need to be orthogonal to these measures [@vanderweele2020].

Nevertheless, confounding and selection biases owing to attrition remain a problem. Missing responses at baseline are estimated within our causal forests. To handle attrition from the baseline wave to the exposure wave, and from the exposure wave to the outcome wave, we employ inverse probability of censoring weighting. Because confounding may still remain a problem a problem we employ sensitivity analysis, as described in the main text."
cat(appendix_confounding_threewave_x_text)


appendix_confounding_threewave_l_text <- "
## Appendix {{appendix_confounding}}: Confouding Control {#appendix-confounding}

::: {#tbl-confounding}
```{=latex}
\\threewavepanel
```
The Causal Directed Acyclic Graphs (DAGS) report the causal identification problems in our three-wave studyand and or methods for addressing them solution.
:::

@tbl-confounding describes our strategy for confounding control, in which we employ a modified disjunctive cause criterion [@vanderweele2019], which involves:


1.	Identifying all common causes of both the treatment and outcomes.
2.	Excluding instrumental variables that affect the exposure but not the outcome.
3.	Including proxies for unmeasured confounders affecting both exposure and outcome.
4.	Controlling for baseline exposure and baseline outcome (@tbl-confounding) addresses the strongest confounders, given any residual confounding would need to be orthogonal to these measures [@vanderweele2020].

Nevertheless, confounding and selection biases owing to attrition remain a problem. Missing responses at baseline are estimated within our causal forests. To handle attrition from the baseline wave to the exposure wave, and from the exposure wave to the outcome wave, we employ inverse probability of censoring weighting. Because confounding may still remain a problem a problem we employ sensitivity analysis, as described in the main text."
cat(appendix_confounding_threewave_l_text)


appendix_assumptions_grf_text <- "
## Appendix {{appendix_assumptions_grf}}: How Statistical Models Influence Detecting Treatment Effect Differences

This explanation draws on @bulbulia2024swigstime.

Imagine a treatment or exposure ($A$) that might affect an outcome ($Y$). Sometimes, the effect of $A$ on $Y$ isn't the same for everyone; it might be stronger or weaker depending on other characteristics. This is called 'effect modification'. For effect modification to even be possible, the treatment must have some effect on at least some individuals [@bulbulia2024wierd].

We cannot simply look at average differences within our sample to understand how effects vary. Instead, we need to use statistical models to investigate this potential heterogeneity [@grf2024; @vansteelandt2022a]. Alternatively, we might want to compare if the treatment effect differs between specific groups in the population, such as different cultural groups or genders.

::: {#tbl-terminologyeffectmodification}
```{=latex}
\\terminologyeffectmodification
Graphical conventions used to represent effect modification.
:::

@tbl-terminologyeffectmodification shows the symbols we use in diagrams to discuss effect modification clearly. To focus purely on effect modification, we assume the treatment ($A$) was assigned randomly (like flipping a coin, $\\mathcal{R} \\rightarrow A$). This means we assume the basic relationship is $A$ causes $Y$ ($A \\to Y$), without other factors confusing this link.

Therefore, we don't need complex causal diagrams to adjust for confounding of the treatment effect itself. We also won't draw a causal arrow from the variable that modifies the effect ($F$) directly to the outcome ($Y$). This choice helps us focus solely on how $F$ changes the $A \to Y$ relationship. (For discussion on different arrow types, see @hernan2024WHATIF, pp. 126-127).

::: {#tbl-terminologyeffectmodificationtypes}
\\terminologyeffectmodificationtypes
Examples of Effect Modification Scenarios
:::

The diagrams in @tbl-terminologyeffectmodificationtypes illustrate the central point: whether we find effect modification depends on which variables we include in our statistical analysis.

In $\\mathcal{G}_1$, the variable $F$ directly influences how $A$ affects $Y$. The open arrow towards the $A \\to Y$ path indicates that $F$ is associated with differences in the effect, without necessarily claiming $F$ causes this difference.

In $\\mathcal{G}_2$, $F$ is an unobserved factor that modifies the $A \to Y$ effect. If the distribution of $F$ differs between populations (e.g., a study population vs. a target population), the average treatment effect found in the study might not apply elsewhere. Generalising findings requires careful consideration [@hernan2024WHATIF; @bulbulia2024wierd]. Estimates can be misleading if the target population differs from the study population regarding effect-modifying characteristics [@greenland2009commentary; @lash2020; @bulbulia2024wierd].

Now, consider the diagrams $\\mathcal{G}_3$ to $\\mathcal{G}_6$, which use the same underlying causal structure but different analysis choices. Let's use an example: $F$ is childhood deprivation, $G$ is educational achievement (influenced by $F$), $A$ is a government educational initiative, and $Y$ is recycling behaviour.

In $\\mathcal{G}_3$, $F$ (deprivation) directly modifies the effect of the initiative ($A$) on recycling ($Y$). $G$ (education) is influenced by $F$. If our statistical analysis accounts for $F$, we essentially block the connection from $G$ to $Y$ that goes through $F$. In this analysis, we would not find that the initiative's effectiveness varies by education level ($G$).

In $\\mathcal{G}_4$, we use the same scenario but analyse the data without accounting for $F$ (deprivation), only including $G$ (education). Because the link between education ($G$), deprivation ($F$), and recycling ($Y$) ($G \\leftarrow F \\rightarrow Y$) is not blocked in this analysis, we would find that the initiative's effect ($A \\to Y$) varies by education level ($G$). Here, $G$ appears to be an effect modifier simply because of our analysis choice.

In $\\mathcal{G}_5$, let's add another variable: $B$, representing depression (measured before $G$). Suppose $B$ also influences education ($G$). If we analyse the data including education ($G$) and depression ($B$), but not deprivation ($F$), something interesting happens. Because $G$ is influenced by both $F$ and $B$, including $G$ in the model creates an artificial statistical link between $B$ and $F$ (via $G$). This makes it look like the initiative's effect ($A \\to Y$) varies by depression level ($B$), even though $B$ has no actual causal link to $Y$ in this structure. The apparent effect modification by $B$ is an artefact of the analysis, not a reflection of causation.

In $\\mathcal{G}_6$, if we include the direct modifier $F$ (deprivation) in our analysis, it blocks the artificial pathways seen in $\\mathcal{G}_4$ and $\\mathcal{G}_5$. We would not find effect modification by education ($G$) or depression ($B$) in this case. This strongly highlights that our findings about effect modification depend on (1) the true underlying causal relationships and (2) which variables we choose to include in our statistical model.

Using principles from causal diagrams [@vanderweele2012], it becomes clear that analysing effect modification requires thinking about the assumed causal structure and explicitly stating which variables are included in the statistical model. Policymakers and researchers might reach wrong conclusions if they misinterpret effect modification findings without considering how the analysis was done [@bulbulia2024swigstime]. Remember, when we assess effect modification, we are asking if the treatment effect differs across groups, not whether the characteristics defining those groups themselves cause the outcome. For further reading on effect modification, see [@vanderweele2012; @vanderweele2007; @suzuki2013counterfactual;bulbulia2024swigstime].
"
cat(appendix_assumptions_grf_text)



# needs n_final_exposure_wave, n_outcome_wave, exposure_variable, name_control_regime, name_exposure_regime, value_exposure_set_high, value_exposure_set_low,appendix_positivity, number_exposure_waves, appendix_positivity

appendix_technical_lmtp_time_vary_text <- "
## Appendix {{appendix_explain_lmtp_time_vary}}: Causal Contrasts and Causal Assumptions {#appendix-assumptions}
### Notation
  - $A_k$: Observed {{name_exposure_variable}} at Wave $k$, for $k = 1, \\dots, {{n_final_exposure_wave}}$.
  - $Y_\\tau$: Outcomes measured at the end of the study (Wave {{n_outcome_wave}}).
  - $W_0$: Confounders measured at baseline (Wave 0) (including A_0, Y_0).
  - $L_k$: Time-varying confounders measured at Wave $k$ (for $k = 1, \\dots, {{n_final_exposure_wave}}$).
### Shift Functions
Let $\\boldsymbol{\\text{d}}(a_k)^+$ represent the {{name_exposure_regime}} treatment sequence and $\\boldsymbol{\\text{d}}(a_k)^\\emptyset$ the **{{name_control_regime}}** treatment sequence, where the interventions occur at each wave $k = 1\\dots {{n_final_exposure_wave}}; k\\in \\{0\\dots n_outcome_wave\\}$.
Formally:
#### {{name_exposure_regime}} $\\bigl(\\boldsymbol{\\text{d}}(a_k^+)\\bigr)$
$$
\\boldsymbol{\\text{d}} (a_k^+)
\\;=\\;
\\begin{cases}
{{value_exposure_set_low}}, & \\text{if } A_k < {{value_exposure_set_high}},\\\\[6pt]
A_k, & \\text{otherwise.}
\\end{cases}
$$
#### {{name_control_regime}} $\\bigl(\\boldsymbol{\\text{d}}(a_k^\\null)\\bigr)$
$$
\\boldsymbol{\\text{d}}(a_k^\\emptyset)
\\;=\\;
\\begin{cases}
{{value_exposure_set_low}}, & \\text{if } A_k > {{value_exposure_set_low}},\\\\[6pt]
A_k, & \\text{otherwise.}
\\end{cases}
$$
Here, $A_k$ is the observed {{exposure_var}} at Wave $k$. The shift function $\\boldsymbol{\\text{d}}$ 'nudges' $A_k$ to a target level (four times per month or zero) only if the current value is below (for {{name_exposure_regime}}) or above (for  {{name_control_regime}}) that target. Across the 1-{{n_final_exposure_wave}} waves, these shifts form a sequence $\\boldsymbol{\\bar{\\boldsymbol{\\text{d}}}}$, which defines a complete intervention regime.
### Causal Contrast
We compare the average well-being under the **{{name_exposure_regime}}** regime, $\\boldsymbol{\\bar{\\boldsymbol{\\text{d}}}}(a^+)$, to the average well-being under the **{{name_control_regime}}** regime, $\\boldsymbol{\\bar{\\boldsymbol{\\text{d}}}}(a^\\emptyset)$. Specifically, the average treatment effect (ATE) is given:
$$
\\text{ATE}^{\\text{outcomes}}
\\;=\\;
\\mathbb{E}
\\Bigl[
  Y_\\tau\\!\\bigl(\\boldsymbol{\\text{d}}(a^+)\\bigr)
  \\;-\\;
  Y_\\tau\\!\\bigl(\\boldsymbol{\\text{d}}(a^\\emptyset)\\bigr)
\\Bigr].
$$
### Assumptions
To estimate this effect from observational data, we assume:
1. **Conditional Exchangeability:** Once we condition on $W_0$ and each $L_k$, the interventions $\\boldsymbol{\\bar{\\boldsymbol{\\text{d}}}}(a^+)$ or $\\boldsymbol{\\bar{\\boldsymbol{\\text{d}}}}(a^\\emptyset)$ are effectively random with respect to potential outcomes.
2. **Consistency:** The potential outcome under a given treatment regime matches the observed outcome when that regime is followed.
3. **Positivity:** Everyone has a non-zero probability of receiving each level of {{name_exposure_variable}} (i.e., a chance to be 'shifted' up or down) given their covariates. The positivity assumption is the only causal assumption that can be evaluated with data. We evaluate this assumption in [Appendix {{appendix_positivity}}](#appendix-positivity)).
Mathematically, for conditional exchangeability, we write:
$$
\\Bigl\\{
  Y\\bigl(\\boldsymbol{\\text{d}}(a^+)\\bigr),
  \\;
  Y\\bigl(\\boldsymbol{\\text{d}}(a^\\emptyset)\\bigr)
\\Bigr\\}
\\coprod
A_k |
W_0,
L_k
$$
That is, we assume the potential outcomes under each treatment regime are independent of each treatment at every time point, conditional on baseline confounders and time-varying confounders (here: {{time_varying_confounders}}).
Under these assumptions, our statistical models permit us to estimate $\\text{ATE}^{\\text{outcomes}}$ from observational data. That contrast is (1) *{{name_exposure_regime}}* versus (2) *{{name_control_regime}}* at least {{number_exposure_waves}} waves*.  We define the target population as {{name_target_population}}, the years in which measurements were taken."


cat(appendix_causal_lmtp_time_vary_text)



# {{name_exposure_threshold}}
# {{name_control_threshold}}
# {{name_target_population}}


appendix_positivity_exposures_5_text <- "
## Appendix {{appendix_positivity}}: Transition Matrix to Check The Positivity Assumption {#appendix-positivity}


```{r}
#| eval: true
#| include: false
#| echo: false
transition_tables$quarto_code()
```

```{r, results='asis'}
cat(transition_tables$explanation)
```

```{r}
#| label: tbl-transition-waveb-wave1
#| tbl-cap: \"Transition Matrix From Baseline Wave to First Exposure Wave\"
transition_tables$tables[[1]]
```

```{r}
#| label: tbl-transition-wave1-wave2
#| tbl-cap: \"Transition Matrix From First Exposure Wave to Second Exposure Wave\"
transition_tables$tables[[2]]
```

```{r}
#| label: tbl-transition-wave2-wave3
#| tbl-cap: \"Transition Matrix From Second Exposure Wave to Third Exposure Wave\"
transition_tables$tables[[3]]
```

```{r}
#| label: tbl-transition-wave3-wave4
#| tbl-cap: \"Transition Matrix From Third Exposure Wave to Fourth Exposure Wave\"
transition_tables$tables[[4]]
```

```{r}
#| label: tbl-transition-wave4-wave5
#| tbl-cap: \"Transition Matrix From Fourth Exposure Wave to Fifth Exposure Wave\"
transition_tables$tables[[5]]
```

"



appendix_positivity_exposures_1_text <- "
## Appendix {{appendix_positivity}}: Transition Matrix to Check The Positivity Assumption {#appendix-positivity}

@tbl-transition-waveb-wave1 presents the transition matrix indicating change in the exposure variable between the baseline and exposure waves.

```{r}
#| eval: true
#| include: false
#| echo: false
transition_tables$quarto_code()
```

```{r, results='asis'}
cat(transition_tables$explanation)
```

```{r}
#| label: tbl-transition-waveb-wave1
#| tbl-cap: \"Transition Matrix From Baseline Wave to Exposure Wave\"
transition_tables$tables[[1]]
```
"

appendix_references_text <- "

## References {.appendix-refs}

"



appendix_positivity_exposures_1_binary_text <- "
## Appendix {{appendix_positivity}}: Transition Matrix to Check The Positivity Assumption {#appendix-positivity}


```{r}
#| eval: true
#| include: false
#| echo: false
transition_tables_binary$quarto_code()
```

```{r, results='asis'}
cat(transition_tables_binary$explanation)
```

```{r}
#| label: tbl-transition-waveb-wave1
#| tbl-cap: \"Transition Matrix From Baseline Wave to Exposure Wave\"
transition_tables_binary$tables[[1]]
```
"

appendix_references_text <- "

## References {.appendix-refs}

"


appendix_confusions_cross_lagged_model_deficiencies_text_old<-"
## Appendix {{appendix_confusions_cross_lagged_model_deficiencies}}: Inadequacy of Cross Lagged Models

Aiken's regression textbook to assert that three conditions must generally be met to establish causality.


Where $X$ represents the exposure or treatment and $Y$ the outcome, Aiken asserts:

1. $X$ and $Y$ must be correlated.
2. The correlation must not be spurious or due to a confounding factor.
3. $X$ must precede $Y$ in time.


The modern causal inference literature, which has advanced considerably over the past two decades in fields such as epidemiology, computer science, economics, and policy research, has shown that these assumptions are inadequate. Consider the following limitations of the traditional veiw.


### Condition 1: It is not true that causation implies correlation


Using Pearl’s graphical models, causality can be defined as $X \rightarrow Y$, where conditioning on a mediator is denoted by $\boxed{M}$. Pearl has shown that in the scenario $X \rightarrow \boxed{M} \rightarrow Y$, causality can exist without a direct association between $X$ and $Y$. This is crucial because, with only two waves of data, any variable measured at Wave 1 could act as a mediator, potentially obscuring the true association between $X$ at time 1 and $Y$ at time 2. To address mediator bias effectively, at least three waves of data are required to disentangle these causal pathways [@vanderweele2020].

### Condition 2: The confounding criterion is insufficient to evaluate causality


This condition essentially restates the idea that an association must be causal to be valid, which, although trivially true, adds no meaningful insight.

The confounding criterion typically applies when treatment $X$ and outcome $Y$ share a common cause, $L$. By adjusting for $L$, we aim to remove the confounding effect. However, consider a scenario where $X$ precedes $Y$ and both $X$ and $Y$ cause $L$, without a direct relationship between $X$ and $Y$. In Pearl's graphical terms, this would be represented as $X \\rightarrow \boxed{L} \\leftarrow Y$. Conditioning on $L$ would induce a spurious association between $X$ and $Y$ that would not exist without conditioning. This demonstrates even when we account for confounding by commong cause, the association between $X$ and $Y$ may be biased.

More fundamental, when including multiple varaibles in our models, we do not automatically obtain consistent causal effect estimates Take the simple case where $L \\rightarrow X \\rightarrow Y$, and suppose that $L \\rightarrow Y$.  To consistently estimate the a causal effect of $X \\rightarrow Y$ we must adjust for $\boxed{L}$.  If the causal relationships we have just described are correct, we may obtain valid causal inference for $X \\rightarrow Y$ by adjusting for $\\boxed{L}$. However the coefficient we obtain for $L$ in our stastical model will not consistently estimate the causal effect of $L \\to Y$ because $X$ is a mediator, such that  $L \\to \boxed{X} \to  Y$.

Inference become even more delicate when we add variables to our model because spurious correlations may be induced. For example, suppose that $L$ is no casually related to $Y$, but causally related to $X$. Suppose further that and unmesaured variable $U$ is related to both $X$ and $Y$ as a common cause.  In this case, there is unmeasured confounding for $X \\to Y$. However, conditioning on $X$ introduces bias for the path for $L$ to $Y$ because $\boxed{X}$ is a collider of $U$ and $L$. In a model that includes $\boxed{X}$ and to will appear that $L$ is associated with $Y$, however were we to simply focus on the unconditional realtionship between $L$ and $Y$ no correlation would be observed. Pearl's proved these points in @pearl1995, yet the remain under-appreciated in many social sciences, including psychological science. However, a consensus in causal inference holds that we should first clarify the causal effect of interest, then assess its identification conditions, then develop an appropriate estimator before we attempt a statistical model.  Practically this means investigators typically require separate models for each causal effect estimate of interest [@mcelreath2020; @westreich2013].


### Condition 3: Temporal precedence is necessary but does not clarify identification


It is true that for $X$ to cause $Y$, $X$ must precede $Y$ in time. However in observational seetings, we must typically collect information on prior states of $Y$ and prior states of $X$ to ensure valid inferences for the causal effects of later states of $X$ on (still) later states of $Y$ . Consider a feedback loop such as $X_0 \rightarrow Y_1 \rightarrow X_2 \rightarrow Y_3$. Here, we imagine that $X$ and $Y$ affect each other over time, (as might occur when threat perceptions affect authoritarian attitudes, which then reshape future threat perceptions...). To correctly estimate the causal effect of $X_t$ on $Y_{t+1}$, we would need to control for both $Y_{t-1}$ and $X_{t-1}$. In regression form, this would appear as:


$$ {\\tt model <- lm(Y_{t+1} \\sim A_t + Y_{t-1} + A_{t-1} + L_{t-1}, ...) }$$

Control for prior exposure and prior outcome is necessary wherever there are unmeasured common causes of both[@vanderweele2020]. Importantly, this model cannot be estimated with only two waves of data; three waves are the minimum required to capture these dynamics. Therefore, the temporal precedence condition, while necessary, is insufficient for drawing reliable causal inferences when data are limited.


### Time series data collected at a four month interval raise in a small student sample are unlikely to yield sufficiently many observations to obtain causal effect estimates


Beyond these conceptual issues, there are practical limitations to causal modelling. For a causal model to be credible, sufficient variation in $X$ over time is necessary. For instance, if we are estimating the causal effect of threat perceptions ($X$) on authoritarianism ($Y$) over a four-month period, we need enough individuals whose threat perceptions have changed to observe any real effects. This variation must also occur across all levels of covariates (e.g., $L$). Without sufficient change, we would not expect to observed causal effects [@vanderweele2020]. However, we cannot rule out causality over longer intervals, which may be required to obtain valid instances of threat triggering.

Notably, experiments are the gold standard for causal inference, and should be preferred when feasible and ethical. We are not given an explanation of why experiments were not used in this thesis. As indicated above, the decision to avoid experiments should be explained.


### Cross-Lagged panel models typically do not afford valid causal effect estimates


Lastly, Cross-Lagged Panel Models (CLPMs), such as those used in Study 4, do not typically offer valid causal contrasts needed to estimate causal effects. The probem is that Cross-Lagged Panel Models lack explicit counterfactual comparisons, which are essential for determining what $Y_{t+1}$ would have been if $X_t$ had taken a different value. Instead, Cross-Lagged Panel Models  only observe $Y_{t+1}$ given the actual value of $X_t$, making it impossible to model hypothetical scenarios. Furthermore, these models typically do not adequately adjust for confounding. As a result, the findings in Study 4 do not provide credible causal inferences.  Gerard notes these problems, but this reader is left in doubt about the scholarly value of the study if the coeffecients reported, because if Gerard's critical observations are right the coefficients are uninterpretable.
"

# yola
#
# explain_grf <- "
# Here, we explain how the `grf` R package (Generalized Random Forests) can be used to estimate causal effects with causal forests, following a typical analysis workflow. We start by estimating the overall average treatment effect (ATE), then test whether treatment effects vary across individuals (heterogeneity). If meaningful heterogeneity is found, we show how to estimate individualised treatment effects and refine treatment recommendations based on those differences.
#
# ### Step 1: Estimating the Average Treatment Effect (ATE)
#
# The Average Treatment Effect (ATE) is the overall effect of the treatment in the population. It answers the question: on average, how much does the treatment change the outcome compared to not treating? In a randomised trial, the ATE can be estimated simply by the difference in mean outcomes between the treated and control groups. For example, if a randomised intervention is designed to improve a psychological outcome such as well-being, the ATE would be the difference in the average outcome for those who received the intervention versus those who did not.
#
# Formally, we can define the ATE as $E[Y(1) - Y(0)]$, where $Y(1)$ is the outcome if an individual receives the treatment and $Y(0)$ is the outcome if they receive the control. In practice, the `grf` package provides functions like `average_treatment_effect()` that can compute the ATE (often using techniques like doubly robust estimation to improve precision). This initial step gives us a baseline: for instance, we might find that a new therapy improves an outcome by, say, .25 points (1-7 scale) on average.
#
# Why start with the ATE? The ATE tells us if the treatment works on average if everyone were randomised to treatment as compared with everyone not receiving the treatment. Note however that even if the ATE is essentially zero (or unreliable), there may neverthless be value in pursuing heterogeneity if some portion of the population is benefited or -- equally improtant -- if some portion is harmed. Moreover, even if the ATE is non-zero (e.g. the treatment has a positive overall effect), the next question is whether this effect is homogeneous (similar for everyone) or heterogeneous (different for different individuals). Thus, whether or not we detect reliable Average Treatment Effects we are led to investigate heterogeneous treatment effects.
#
# Because multiple outcomes were evaluated, we corrected results using {{ate_adjustment}} {{ate_alpha}}.
#
# ### Step 2: Assessing Heterogeneous Treatment Effects (HTE)
#
# Realistically, treatment effects are often heterogeneous, meaning the treatment's magnitude (and direction) of effect can differ across individuals and subgroups. For example, a therapy might work better for participants with a certain personality trait, or an educational intervention might benefit students with lower initial skills more than those with higher skills. Heterogeneous Treatment Effect (HTE) analysis seeks to estimate how the treatment effect varies as a function of individual characteristics. We denote the Conditional Average Treatment Effect (CATE) for an individual with features $x$ as:
#
# $$\\tau(x) = E[Y(1) - Y(0)\\mid X = x]$$
#
# where $X$ represents the covariates (individual characteristics). In plain language, $\\tau(x)$ is the expected treatment effect for an individual with attributes $x$. If $\\tau(x)$ is the same for all $x$, the treatment effect is homogeneous (everyone benefits the same amount). If $\\tau(x)$ varies — for instance, $\\tau(x)$ is higher for some people and lower or even negative for others — then we have treatment effect heterogeneity.
#
# **Standard parametric approaches vs. HTE**: Traditionally, researchers might use regression models with interaction terms to examine heterogeneity. For example, one might include terms like Treatment $\times$ Covariate in a linear regression to see if the effect differs by that covariate. However, standard parametric models impose strong assumptions. Often a simple model assumes a homogeneous effect (one treatment coefficient for all) unless specific interactions are added. Even when interactions are included, the model usually assumes a particular functional form (e.g. linear or additive effects of covariates). This can be very restrictive – real heterogeneity might involve complex, non-linear combinations of attributes that are hard to pre-specify. If we try to include many interaction terms or non-linear effects (such as polynomials or splines), we risk model misspecification (choosing the wrong form) and overfitting in small samples [@Sadique2022]. In short, classical regression approaches require us to guess the right pattern of heterogeneity in advance, and they treat everything outside that guess as noise.
#
# **Example:** imagine a study on a new depression therapy. A simple model might assume the therapy has the same effect on all patients (homogeneous effect). If we suspect younger patients respond differently than older patients, we could add an age interaction. But what if the effect actually depends on a combination of age and baseline symptom severity in a non-linear way? Capturing that with a parametric model would require carefully specifying complex interactions (e.g. Age $\times$ Severity, perhaps non-linearly) and would quickly become impractical with many covariates.
#
# Because of these challenges, we turn to a more flexible, data-driven approach: causal forests. These allow the data to reveal heterogeneity without us having to specify a particular form.
#
# ### Step 3: Estimating Individualised Effects with Causal Forests
#
# A causal forest (as implemented in grf) is a machine learning method that extends the idea of random forests to estimate $\\tau(x)$ (the CATE) for each individual. A causal forest is an ensemble of many decision trees that are grown specifically to capture differences in treatment effect across individuals [@grf2024]. In essence, each tree in a causal forest partitions the data based on covariates, aiming to group together individuals with similar treatment responses. By averaging over many such trees (each built on a random subset of data and covariates), the forest produces a robust estimate of the individual treatment effect for each person’s feature profile.
#
# **How causal forests avoid strong assumptions**: Unlike a parametric model, a causal forest model does not assume the treatment effect is constant or linear in the covariates. It can discover non-linear relationships and interactions automatically. For example, a causal forest might find that for younger participants the therapy is very effective, whereas for older participants with high baseline depression the effect is smaller – even if we didn't explicitly program an 'age $\times$ baseline' interaction. The forest algorithm will try splitting the data on different covariates to maximize differences in outcomes between treated and control within each split, effectively letting the data determine where the treatment effect differs. This means we are not forcing a single form of heterogeneity; the model can capture complex patterns (such as a treatment working only at certain combinations of characteristics) that a linear model might miss.
#
# **Causal forest mechanics:** Each tree in a causal forest is constructed in a way that is similar to a standard random forest, but the splitting criterion is tailored to treatment effect estimation. At each split, the algorithm looks for a covariate and a cut-point that best separate the data into two groups with different treatment effects. In other words, it tries to maximize the contrast in outcomes between treated and control in the two child nodes [@grf2024]. This is different from a standard regression tree that would split based on differences in outcome levels; the causal tree splits based on differences in **treatment effects. Once the tree is grown, the treatment effect within each final leaf can be estimated by comparing the average outcomes of treated vs. control units in that leaf. To get a prediction for a new individual with features $x$, we drop that individual down each tree (finding which leaf they land in), and take the leaf's estimated treatment effect. The causal forest averages those estimates across many trees, yielding $\\hat{\tau}(x)$, an estimate of that individual's treatment effect.
#
# Importantly, grf uses an approach called 'orthogonalisation' (or 'Robinson’s transformation') to handle confounding in observational data: it first estimates the outcome without treatment and the propensity of treatment, then focuses the forest on the residuals [@wager2018]. This ensures that the forest primarily learns the difference attributable to treatment, not just baseline outcome differences or propensities. (In a randomised experiment, this step is less critical since treatment assignment is independent of $X$, but it is very useful in observational studies.)
#
# By using a causal forest, we obtain an estimate $\\hat{\\tau}(x_i)$ for each individual $i$ in our sample (often called individual treatment effect or ITE estimates). These estimates tell us who benefits more or less from the treatment according to the model.
#
# ### Step 4: Honest Estimation and Out-of-Bag Validation
#
# One concern with flexible models (like forests) is overfitting – they might pick up random noise as if it were a real pattern, especially when trying to estimate many individual effects. The `grf` package addresses this through a technique called honest estimation and by using out-of-bag (OOB) validation.
#
# **Honest trees:** an honest causal tree is built so that the data used to decide where to split the tree is separate from the data used to estimate the treatment effect in each leaf [@wager2018]. In practice, this can be done by randomly splitting the sample into two halves: one half is used to grow the structure of the tree (find the splits based on covariates), and the other half is used to compute the treatment effect estimates within the leaves. Because an observation is either used for splitting or for estimation (but not both) in any given tree, the estimates in each leaf are unbiased by the selection of that leaf. This honest approach guards against a tree that carves the data too finely to chase noisy differences that will not hold up in new data [@wager2018]. Causal forests in `grf` implement honesty by default, meaning each tree in the forest is grown in a way that ensures the treatment effect estimate at the end is an 'out-of-sample' estimate relative to the splits that created the leaf [@grf2024].
#
# Out-of-bag predictions: in a random forest, each tree is typically built on a bootstrap sample (random sampling with replacement) of the data. This means about one-third of the observations are left out (not used) in that tree – these are called 'out-of-bag' observations for that tree. Since they were not used to train that tree, we can use them to get an unbiased prediction for those observations. `grf` uses this idea to produce OOB estimates of $\\tau(x)$ for every training point: each observation’s OOB estimate is the average of predictions from all trees where that observation was not in the training sample. This is like a built-in cross-validation. The OOB estimates are honest in the sense that for each individual, we're predicting their treatment effect using only trees that did not observe that individual's outcome during training [@grf2024]. These OOB estimates can be used to assess model fit and to perform certain tests (with caution, as we discuss later).
#
# By combining honesty and OOB estimation, causal forests avoid the worst of overfitting while still using all the data. In fact, these measures enable the forest to provide valid confidence intervals and variance estimates for the treatment effects. For example, grf can calculate standard errors for $\\hat{\\tau}(x)$ using the variability across trees (with a method that groups trees and compares their predictions) [@grf2024]. The bottom line is that the forest's individual effect estimates are 'honest' (out-of-sample) estimates, increasing our trust that these effects are not just reflecting noise.
#
# ### Step 5: Handling Missing Data and Model Validation
#
# Empirical data often have missing values in some covariates. Unlike many traditional methods that might require dropping observations or imputing values, `grf` can handle missing covariate values directly. `grf` uses a strategy called the Missing Incorporated in Attributes (MIA) splitting rule [@grf2024]. In simple terms, when considering a split on a variable that has some missing values, the algorithm treats 'missing' as its own category: it finds splits that can send missing values one way and non-missing another way. For example, suppose we have a covariate like income where some values are missing. A causal tree could have a split that says 'if Income > 50K go left, if Income <= 50K go right, and if Income is missing, also go right (or go left)' -- essentially handling the missingness within the tree structure. This way, we do not have to drop people with missing income; the forest can still use partial information from other covariates and also possibly learn if 'missingness' itself is informative (perhaps not reporting income correlates with some outcome). By using MIA, the causal forest implicitly handles missing data without a separate imputation step, preserving information and avoiding bias that might come from improper imputation [@grf2024].
#
# Beyond missing data, another core aspect of model validation is ensuring our findings are not artifacts of particular sample splits or tuning choices. Although random forests typically have a few hyperparameters (number of trees, depth, etc.), the defaults in `grf` are often reasonable, and we employ them here. Users can use cross-validation to fine-tune parameters such as minimum leaf size or complexity if needed. For example, one might try different minimum leaf sizes and check which yields the best out-of-sample predictive performance for treatment effects. However, because causal forests average over many trees and use honesty, they are relatively robust and often do not require extensive tuning.
#
#
# ### Step 6: Testing for Treatment Effect Heterogeneity
#
# After fitting a causal forest and obtaining individualised effect estimates $\\hat{\tau}(x)$, a crucial question is: do these estimates provide evidence that treatment effects truly vary, or could the apparent heterogeneity be just noise? In other words, we want to test the hypothesis that the treatment effect is actually the same for everyone.
#
# We begin with an approach implemented in the `grf` framework that uses the Rank-Weighted Average Treatment Effect (RATE) metrics as a basis for a hypothesis test [@grf2024]. The idea is to ask: if we were to use our estimated $\\hat{\\tau}(x)$ to prioritise who gets treated, would outcomes improve compared to treating people at random? If there were no heterogeneity, then any rule based on $X$ (including our $\\hat{\\tau}(x)$ estimates) should do no better than random assignment. If there is heterogeneity and we have identified it correctly, then targeting those with higher $\\hat{\\tau}(x)$ should yield better outcomes on average.
#
# Concretely, we can sort individuals by their estimated benefit $\\hat{\tau}(x)$ (from highest to lowest). Suppose we progressively treat people in that order – first the top 10% most likely to benefit, then 20%, and so on. If heterogeneity is meaningful, the people with higher $\\hat{\\tau}(x)$ should indeed have larger actual treatment effects on average, so treating them first gives us a bigger gain than treating a random 10% or 20%. We can summarise this with a Targeting Operator Characteristic (TOC) curve – analogous to an ROC curve in classification – which for each fraction $q$ of the population treated (x-axis) shows the excess treatment effect achieved by targeting based on $\\hat{\tau}$ (y-axis) compared to random treatment [@grf2024]. If there were no heterogeneity, this curve would be flat (no gain from targeting). If there were heterogeneity, the curve would rise – especially at low $q$, where we are focusing on the top predicted responders [refer to @yadlowsky2021evaluating].
#
# The test for heterogeneity can then be based on the area under this TOC curve or other summaries of it. Specifically, grf defines:
#
# - **AUTOC:** the Area Under the TOC Curve. This essentially integrates the benefit of targeting across all possible fractions treated [@grf2024]. If there's no heterogeneity, AUTOC should be zero (no area under the curve, since the curve is flat at zero gain).
#
# - **Qini:** a related metric (named after a concept by Radcliffe, 2007) which is a weighted area under the TOC [@radcliffe2007using]. The Qini index weights larger fractions more heavily (technically, Qini = $\\int_0^1 q \\cdot \\mathrm{TOC}(q),dq$) [@radcliffe2007using]. Intuitively, AUTOC tends to emphasise the extremes (are there a small group of people with very large effects?) whereas Qini gives more weight to broader improvements (moderate effects spread across more people) [@yadlowsky2021evaluating].
#
# Both metrics are different ways of aggregating how effective our priority-assignment rule is.
#
# To test for heterogeneity, we can check whether these metrics are significantly greater than zero. Under $H_0$ (no heterogeneity), we expect no gain from targeting, so, for example, AUTOC = 0. We can compute an estimate of AUTOC or Qini from the data (using held-out samples to avoid bias) and compute a standard error. Thanks to theoretical results, these metrics satisfy a central limit theorem, so we can do a simple $t$-test. Essentially, we test:
#
# - $H_0$: RATE (e.g., AUTOC or Qini) = 0 (no heterogeneity detectable)
# - $H_A$: RATE $> 0$
#
# If the test rejects $H_0$, we have evidence that some individuals benefit more than others, beyond what we would expect by chance [@grf2024]. This is strong evidence of treatment effect heterogeneity. If we fail to reject, it suggests that any differences our forest found might not be statistically reliable – in other words, we cannot be sure the treatment effect isn't basically uniform.
#
# To summarise this step: we use the forest's output to construct a test for heterogeneity, interpreting $p$-values after {{cate_adjustment}} correction at q = {{cate_alpha}} (controlling the expected false-discovery proportion [@benjamini1995controlling]). This moves us from simply eyeballing $\\hat{\\tau}(x)$ values (which can be noisy) to a rigorous statistical test of whether tailoring treatment based on $X$ has potential value.
#
# ### Step 7: Using RATE Metrics to Evaluate Targeted Treatment Rules
#
# Beyond hypothesis testing, the RATE framework is valuable for quantifying how much improvement we might gain by individualised treatment policies. RATE stands for Rank-Weighted Average Treatment Effect, which as described, comes from considering a treatment prioritisation rule $S(X)$ that ranks individuals by some score (in our case, the score is the predicted benefit $\\hat{\\tau}(X)$) [@grf2024]. We want to evaluate how good this rule is at targeting treatment to those who benefit the most.
#
# Two key RATE metrics have already been mentioned: AUTOC and Qini. Both summarise the TOC curve (the performance of our targeting rule across the range of possible treated fractions). To interpret them in plain terms:
#
# - AUTOC (Area Under the Targeting Curve): This is the integral of the improvement achieved by targeting, across all possible proportions treated. An equivalent interpretation is that AUTOC is a weighted average of treatment effects where higher weight is placed on the highest-score individuals (since at very low fractions $q$, we’re only treating the top-scoring people). A larger AUTOC means our rule does a very good job at ranking people – the top-ranked have much bigger effects than the average. We refer to this as one type of RATE metric.
#
# - Qini: this is another summary of the targeting curve, but uses a different weighting (in fact, linearly increasing weight by fraction treated) In practice, that means Qini cares more about how well we do when treating larger portions of the population, not just the very top slice. If treatment benefits are spread out, the Qini will capture that better. We can think of Qini as measuring the aggregate gain from using the rule as we expand treatment – it places equal emphasis on high-benefit individuals and those with more modest benefit (because as you increase $q$, the weight $q$ in the integral increases) [@yadlowsky2021evaluating].
#
# Both AUTOC and Qini are numbers (scalars) that we can compute for a given prioritisation rule (like the causal forest's ranking). They can be compared to baseline strategies. For instance, one baseline is a policy that does not use any covariates – e.g. treat a random fraction $q$ of people. That baseline would by definition have a TOC curve of zero (no preferential gain) and thus AUTOC = Qini = 0. Another baseline could be treating everyone (which yields the ATE as the outcome gain, but of course no targeting because everyone is treated.  This latter baseline is the rule that we use here. We are interested in incremental improvements over the ATE by targeting  $\\hat{\\tau}(X)$).
#
# In a policy-setting context, these metrics help answer the question: 'Could we improve outcomes by targeting the treatment to specific individuals instead of a one-size-fits-all approach?' and 'If yes, how large might that improvement be?' For example, suppose the ATE of a job training program is an earnings increase of USD 500 on average. A statistically reliable Qini or AUTOC might reveal that if we only gave the program to the half of people who benefit the most (according to some characteristics), the average impact for those treated could be much higher, say $1000, and essentially zero for those not treated – implying resources can be better allocated. On the other hand, if no heterogeneity were found, we cannot be certain that targeting would change the payoff. Additionally, we might find reliable evidence that targetting performs worse than random assignment, and advise caution against targetting if the aim is maximise benefits across the population.
#
# To make this concrete, `grf` provides a function rank_average_treatment_effect() that computes these metrics and even performs statistical tests. An analyst might use it in an applied study to report, for example: 'Using the causal forest’s predicted CATEs as a prioritisation rule, we find a rank-weighted treatment effect (AUTOC) of 0.15 (SE = 0.05), indicating a substantial improvement from targeted treatment over a non-targeted policy [@grf2024]. The Qini coefficient is 0.10 (SE = 0.03). Both are reliably greater than zero, reinforcing the presence of actionable heterogeneity. These numbers would be in the units of the outcome (or outcome difference), and they tell policymakers the potential gain from personalised treatment assignment.
#
# In summary, RATE metrics such as AUTOC and Qini serve a dual purpose: (1) they statistically test for any heterogeneity (as discussed in Step 6) and (2) they quantify how effective our estimated heterogeneity is for improving outcomes through targeting. They help translate complex heterogeneity estimates into a single measure of 'policy value.' If these metrics are small or insignificant, it means even if effects vary, it may not be useful for targeting (perhaps the variation is too minor or too uncertain). If they are large, it suggests real potential to increase impact by focusing the treatment where it works best.
#
# ### Step 8: Qini Curves for Policy Insight (Targeting at Top 20% or 50%)
#
# While single-number metrics (AUTOC, Qini indices) are useful, it is often enlightening to look at the Qini curve or the underlying targeting curve visually. The Qini curve plots the cumulative gain in outcome as we allocate treatment to a larger fraction of the population, ranking by predicted treatment effect [@grf2024]. The x-axis typically is the fraction (or percentage) of the population treated (sorted from highest predicted benefit downwards), and the y-axis is the net gain achieved by that policy compared to treating that fraction at random.
#
# For example, @fig-example-qini presents a Qini curve result in a causal forest analysis investigating the effects of religious service attendance on personality (here 'Agreeableness'). The straight, solid (orange) line indicates the expected gains from treating indiscriminately according to the ATE. The dashed (blue) curve indicates the expected gains from treating using  $\\hat{\\tau}(X)$). The x-axis ('spend') represents the fraction of individuals treated (scaled to a budget), and the y-axis ('gain') represents the improvement in outcome (e.g., additional treatment effect) from targeting those individuals versus random assignment. A steeper, higher dashed curve indicates that the model-based targeting yields much better outcomes for the treated fraction than random selection (solid line). This curve stops (or 'plateaus') after treating approximately 50% of population, because at that point we have assigned treatment to the units predicted to benefit, $\\hat{\\tau_i}(X)> 0)$.
#
#
# ```{r, results='asis'}
# #| label: fig-example-qini
# #| fig-cap: \"Example Qini Curve.\"
# #| eval: true
# #| include: true
# #| echo: false
# #| fig-width: 12
# #| fig-height: 12
# models_binary_batch_example$model_t2_agreeableness_z$policy_tree
# ```
#
#
# In the above illustration, the Qini curve rises sharply at the beginning, indicating that the first several percent of individuals treated (those with the highest predicted $\tau(x)$) yield a large gains to agreeableness. By contrast, the dashed line is roughly a 35-degree line starting at (0,0) – it represents a policy that doesn’t use the heterogeneity (just treating people arbitrarily), which yields, on average, a linear accumulation of the overall ATE. At $x=1.0$ (100% treated), both the model-based policy and the random policy coincide – at that point, everyone is treated, so the total gain is just the ATE. The fact that the Qini curve is above the solid line (forming a 'bulge') indicates that targeting is effective: we achieve more outcome gain for the same treatment fraction [@grf2024]. The area between the solid curve and dashed line up to a certain point corresponds to the RATE metrics discussed (QINI would be the total area between them from 0 to 1).
#
# In our analysis, we report budget-constrained thresholds for treating the top 20% or 50% of recipients. Policymakers often have limits on how many people can be treated (due to cost or other constraints). The Qini curve allows us to zoom in on specific treatment fractions. For instance, if we can only treat 20% of the population, we look at $q = 0.2$ on the x-axis. The y-value of the Qini curve at $0.2$ tells us the expected gain from treating the top 20% (as ranked by the model) compared to treating 20% randomly.
#
# The Qini curve allows us to visualise and explain how the effectiveness of the treatment allocation changes as we expand who gets treated. For an applied researcher, this is very useful. We do more than not that 'heterogeneity exists.'  We are able to advise whether, 'if we have a budget to treat X% of people, here's what outcome we can expect.' For example, suppose an education intervention has an average effect of 5 test score points. The Qini analysis might show that focusing on the top 50% of the population (maybe those with certain demographic profiles) could yield an average effect of .15 SD units for those expected to benefit, meaning we would save resources on those unlikely to gain. At 100% (treating everyone), we're back to average treatment effect, which in this example is about half this amount (0.07 SD units). So the policy insight is: if only 50% can be afforded, target that group to maximise impact.
#
# To summarise, Qini curves allow us to identify and communicate the value of targeted policies at specific coverage levels. They show whether the benefit of the treatment is concentrated in a small segment of the population or more widely distributed. When making policy recommendations, one could use the Qini curve to decide, for instance, 'we should implement the program for the top 50% most likely to benefit; beyond 50%, the returns diminish to roughly the average effect, so including more people isn’t cost-effective.' This level of analysis goes beyond saying 'there is heterogeneity'; instead, we quantifies *how much improvement* is possible and for what percentage of a target population.
#
# ### Step 9: Avoiding Overfitting in Policy Evaluation (Sample Splitting for RATE/Qini)
#
# An important statistical caution: when we use the same data to both train a model (such the causal forest) and evaluate its performance (like computing how well targeting does), we can get overly optimistic results. Even though the causal forest provides OOB estimates that are ostensibly out-of-sample for each individual, there is still a subtle form of overfitting possible if we are not careful. The forest was constructed to maximise heterogeneity in the training data, so using those same estimates to evaluate the policy can bias the evaluation upward (we might overstate the value of targeting because we are implicitly reusing the data).
#
# To ensure valid inference, the best practice is to use explicit sample splitting or cross-fitting for the evaluation stage. This means, for example, after training the causal forest on the whole dataset (or a portion of it), we assess the RATE or Qini metrics on a fresh hold-out set that was not used in training. Here we use a separate hold-out set in which we train {{traning_proportion}} of the data, and use the 'unseen' remainder as the validation set.
#
# Why do we do this? Even though the forest’s OOB predictions are not directly from a model that saw that observation's outcome, there remains correlation – each observation’s prediction is an average from many trees, and while any given tree didn't see that observation, it saw many others including some that are also used in evaluating the policy. There is also the fact that OOB predictions are a form of cross-validation but not a true independent test, especially since the forest structure was influenced by all data. To be rigorous, researchers often do a double sample split: one split to train the forest, and a second split (completely independent) to compute the policy metrics like Qini or evaluate a specific targeting rule. This double-splitting ensures that when we say 'targeting the top 20% yields X improvement,' that claim is verified on data that played no part in determining who was top 20%. Thus, our inference (confidence intervals, p-values for heterogeneity) remains valid and not overly optimistic.
#
# In plain terms, our workflow is as follows:
#
# 1. Split the data into two parts: A and B.
# 2. Use part A to train the causal forest and get $\\hat{\tau}(x)$.
# 3. Use part B to evaluate heterogeneity: calculate the actual outcomes if we apply the policy (treat those with highest $\\hat{\\tau}$ in B, compare to others in B, etc.). Compute RATsE/Qini metrics and perform the heterogeneity test on part B.
#
# By doing this, when we report 'p = 0.01 for heterogeneity' or 'Qini = 0.10 at 20% spending', we know these numbers are honest assessments of how the model would perform on new data, not just the data we fit it to.
#
# In short, even with OOB estimates, further sample splitting is used for final evaluation to ensure our conclusions about heterogeneous effects and the benefits of targeting are reliable. This extra step is crucial for rigour: it prevents us from convincing ourselves that a complicated model is useful when in fact it might be explaining noise.
#
# ### Step 10: Interpretable Treatment Rules with Policy Trees
#
# Finally, once we have evidence of heterogeneity and an effective way to target treatment, we might want to translate the complex model into a simple decision rule. Causal forests, while powerful, are 'black box' in nature – they may involve hundreds of trees and intricate splits that are not easy to interpret. For practical decision-making (especially in policy contexts), stakeholders often prefer a simple rule (for example, a checklist or a flowchart) that determines treatment eligibility. This is where policy trees come in.
#
# A policy tree is essentially a simple decision tree that assigns treatment or control based on a few covariates splits, optimised to yield good outcomes. The idea is to summarise the individual treatment effects $\\hat{\\tau}(x)$ (or directly use the data) into a set of if-then rules that approximate the optimal targeting. For instance, a policy tree might look like: 'If age < 25 and baseline severity is high, give treatment; if age ≥ 25 and baseline severity is low, do not treat,' etc. These rules are much easier to communicate than a black-box forest.
#
# `grf` integrates with the `policytree` package to derive an optimal decision tree given the causal forest's estimates [@policytree_package_2024]. Our procedure is as follows: use the forest's estimates or doubly robust scores as input to a decision tree algorithm that maximises the expected outcome (welfare) under that tree policy. In other words, we finds splits that best separate who should be treated vs not to improve the overall result. The result is a shallow tree (often depth 1, 2, or 3) that is much easier to interpret than the full forest. Here we use a decision tree of depth = 2.
#
# **Why use policy trees?** Apart from interpretability, policy trees can enforce fairness or simplicity constraints and avoid overfitting by limiting complexity. A shallow tree might capture the broad strokes of heterogeneity (e.g., young vs old, or high risk vs low risk) in a way that practitioners can double-check with domain knowledge. As an example from `grf` documentation: 'Deciding who to assign the program based on a complicated black-box CATE function may be undesirable if policymakers want transparent criteria. Likewise, it may be problematic for participants to learn they were denied a beneficial program solely because a black-box algorithm predicted low benefit. In such settings, we want an interpretable policy, for example, a shallow decision tree that says ‘Alice is assigned treatment because she is under 25 and lives in a disadvantaged neighborhood' see: [https://grf-labs.github.io/grf/articles/policy_learning.html](https://grf-labs.github.io/grf/articles/policy_learning.html). This nicely illustrates how a policy tree can provide a rationale in human terms.
#
# **Training and validation for policy trees:** As when construsting causal forests and evaluating heterogeneity in them using RATE AUTOC or Qini curves, when creating policy trees we must be careful to avoid overfitting. It's tempting to use the same data that suggested heterogeneity to also choose the best splits for the policy tree, but that can lead to optimistic results. The optimal tree is chosen to fit the training data well – if we do not validate it, we might pick a tree that works by chance quirks of the data. Therefore, we use cross-validation to select the tree's complexity (depth) and sample splitting to evaluate its performance, using {{train_proportion_decision_tree}} to train the tree and the remainder to valid it.
#
#
# ```{r, results='asis'}
# #| label: fig-example-decision-tree
# #| fig-cap: \"Example Decision Tree.\"
# #| eval: true
# #| include: true
# #| echo: false
# #| fig-width: 10
# #| fig-height: 14
# models_binary_batch_example$model_t2_agreeableness_z$combined_plot
# ```
#
# **Example**: Recall, @fig-example-qini shows reliable treatment heterogeneity for the effect of religious service attendance on agreeableness. @fig-example-decision-tree presents a policy tree analysis in which simple treatment rules are evaluated indicating who we should 'treat' with religious service if we hope to optimise agreeableness. Here we find that three variables predict treatment benefits: weekly hours commuting, weekly hours doing housework, and household income.  Specifically:
#
# - Participants who commute less than about 5.8 hours per week and have a household income above NZD 28,600 experience greater gains in agreeableness when attending religious services. However among those with very low income and lower commuting demands we would not expect added gains for agreeableness from religious service attendance.
# - Meanwhile, those with comparable higher commuting hours who household working hours are less than 16 hours per week are not expected to benefit in Agreeableness. However, those who are heavier commuters but and also doing more housework are expected to gain in agreeableness.
#
# This analysis holds both theoretical and practical interest. Rather than picking out a familiar category grouping such as gender or ethnicity, policy trees reveal that effect vary by resource availability.
#
# Overall, policy trees condense the insights from causal forests into actionable guidelines. We emphasise that deriving these guidelines includes rigorous validation: we use one part of the data to learn the policy and another to test it. This ensures that the simple rules we recommend (e.g. 'treat those with lower time demands; do not treat incomes at a certain level') are supported by evidence rather than being artifacts of noise.
#
# Although estimating heterogeneous treatment effects and deriving actionable policy rules can be both theoretically and practically important, two considerations should temper our enthusiasm.
#
# First, the factors highlighted in policy trees are predictors of treatment-effect variability, but these predictors do not themselves have a causal interpretation. Returning to the example above, we should not infer that intervening to set someone’s travel and housework times to different values would change the variability in response. To understand the causal effects of joint interventions, we would need a different analysis. Decision trees are important because they draw attention to segments of a population likely to benefit, but they do not clarify the effects of changing a population structure.
#
# Second, decisions about prioritising treatment rules are often ethical and political. Even if we set aside uncertainties in the modelling process, few would argue that fairness and justice should be determined by optimisation rules alone. Such questions are typically resolved through democratic processes that involve stakeholder consultations, reflection on social values, a reckoning with historical inequities, and considerations beyond the scope of statistical analyses.
# "

# unified_db <- boilerplate_add_entry(
#   db = unified_db,
#   path = "appendix.explain",
#   value = list()
# )



explain_grf_long <- "
In this appendix we show how to estimate causal effects with the `grf` R package (Generalised Random Forests).

#### Menu of HTE diagnostics

Investigators can choose one, some, or all of the following tools, depending on the scientific or operational question:

* **RATE AUTOC / RATE Qini (global evidence)** – 'Should we abandon a uniform policy at all?'  Tests the null of no actionable heterogeneity on a validation split.
* **Qini curves (budget lens)** – 'If we can treat only up to $p\\%$, what uplift do we expect?'  Reads the targeting curve at specific spend levels.
* **Policy trees (decision rule)** – 'Which simple, transparent allocation rule maximises expected welfare under constraints?'  Optimises welfare directly, agnostic to hypothesis-testing conventions.

These tools are complementary rather than sequential.  A study may stop after RATE, skip RATE and go straight to a decision rule, or report the full trio; the choice should reflect the primary inferential aim — evidence, optimisation, or communication.

Below we **illustrate** all three analyses in turn.  In practice an investigator may run only the subset that addresses their question.


However, we begin with the overall average treatment effect (ATE), investigate whether effects vary (heterogeneity), and --where useful -- estimate individualised effects and derive simple, actionable treatment rules.

### Step 1 Estimating the Average Treatment Effect (ATE)

The ATE answers a simple question: on average, how much does treatment change the outcome? In a well‑randomised trial (or an observational study with adequate control), we estimate it with the difference in mean outcomes between treated and control units. Formally,

$${\\rm ATE}=E\\,[Y(1)-Y(0)],$$

where $Y(1)$ and $Y(0)$ denote potential outcomes under treatment and control.
`grf::average_treatment_effect()` computes the ATE, using doubly‑robust estimation for precision. This step gives us a baseline—say, the new therapy lifts well‑being by 0.25 points (1–7 scale).

We start here because the ATE tells us whether the treatment works **on average** if everyone were treated versus no-one. Yet a zero (or noisy) ATE does not rule out helpful or harmful effects for sub-groups, and a non-zero ATE does not guarantee benefits for all.  We often want to examine heterogeneity.


+### Step 2 Assessing Heterogeneous Treatment Effects (HTE)

Treatments seldom work equally for everyone. We define the conditional average treatment effect (CATE) for covariate profile $x$ as
$$\\tau(x)=E\\,[Y(1)-Y(0)\\mid X=x].$$
If $\\tau(x)$ is constant, effects are homogeneous; if it varies, we have heterogeneity.

Classical regression tackles heterogeneity via interaction terms, but that approach demands strong functional‐form assumptions. Real‑world heterogeneity can be non‑linear and high‑dimensional; guessing the correct model risks misspecification or over‑fitting [@Sadique2022]. Thus we turn to causal forests, which let the data speak.


+### Step 3 Estimating Individualised Effects with Causal Forests

A causal forest is an ensemble of 'honest' causal trees grown to capture **differences** in treatment effects rather than outcome levels [@grf2024]. Each tree splits the data to maximise treated‑versus‑control contrasts within leaves, and the forest averages those leaf‑level estimates to give
$$\\hat\\tau(x)$$
for every individual.

 Advantages:

- **Flexibility** – no need to pre‑specify interactions or non‑linearities.
- **Orthogonalisation** – residualising outcomes and treatment probabilities focuses the forest on causal signal [@wager2018].
- **Per‑person estimates** – we obtain $\\hat\\tau(x_i)$ for every $i$.

+### Step 4 Honest Estimation and Out-of-Bag Validation

Over‑fitting lurks in flexible models. `grf` combats this with 'honest' trees: one half‑sample picks splits, the other half estimates effects, ensuring each leaf estimate is out‑of‑sample. Out‑of‑bag (OOB) predictions—averaging over trees that did **not** train on an observation—provide further unbiased validation and standard errors [@grf2024].

+### Step 5 Handling Missing Data and Basic Validation

Missing covariate values? No drama. `grf` uses the MIA (Missing Incorporated in Attributes) rule: it treats 'missing' as a legitimate split category, so we keep cases rather than impute or drop them [@grf2024]. Hyper‑parameters rarely need fine‑tuning, though cross‑validation can refine minimum leaf size, tree depth, and the like.

+### Step 6 Global evidence with RATE (AUTOC & Qini)

Do the estimated $\\hat\\tau(x)$ really differ, or is it just noise? The RATE framework answers this. We rank individuals by $\\hat\\tau$ and ask whether treating the top scorers improves outcomes relative to random assignment. The Targeting Operator Characteristic (TOC) curve plots this gain over treatment fractions $q$. Two scalar summaries are

* **AUTOC** – area under the TOC; accentuates extreme responders.
* **Qini** – a weighted area (weights grow with $q$); favours broader gains [@yadlowsky2021evaluating].

Under $H_0$ (no heterogeneity) both metrics equal zero; `grf::rank_average_treatment_effect()` supplies estimates, standard errors, and $t$‑tests. Rejecting $H_0$ signals actionable heterogeneity.

Both metrics aggregate the whole targeting curve, so a single negative stretch can drag the average below zero.  Thus a reliably negative RATE warns that *no blanket ranking policy should be adopted*, no matter what the tail behaviour looks like.

RATE therefore answers a *global* question: if we were to use our estimated $\\hat{\\tau}(x)$ to prioritise who gets treated **across the entire population**, would outcomes improve compared with treating everyone (or no-one) identically?  If there were no heterogeneity, then any rule based on $X$ (including our $\\hat{\\tau}(x)$ estimates) should do no better than random assignment.

+### Step 7 Quantifying policy value with RATE metrics

Beyond significance, AUTOC and Qini quantify the **magnitude** of improvement from targeting. A large AUTOC means a prioritisation rule sharply distinguishes high‑benefit individuals; a large Qini means gains persist over wider coverage. Policymakers can weigh these gains against costs—because budgets rarely stretch to treating everyone (as any Kiwi running a lab grant knows too well).

### Step 8 Budget-constrained gains via Qini curves

RATE tells us whether heterogeneity exists in aggregate; the Qini curve drills down to **how much uplift we get at each spend level**.  A planner who can only afford to treat the top 20 % cares about the leftmost fifth of the curve, even if the global RATE is negative.

*Practical note:* because the Qini curve is read off the **same validation split** used for RATE, comparisons are on equal footing and over-optimism is kept in check.


```{r, results='asis'}
#| label: fig-example-qini
#| fig-cap: \"Example Qini Curve.\"
#| eval: true
#| echo: false
#| fig-width: 12
#| fig-height: 12
models_binary_batch_example$model_t2_agreeableness_z$policy_tree

```


In the above illustration, the Qini curve rises sharply at the beginning, indicating that the first several percent of individuals treated (those with the highest predicted $\tau(x)$) yield a large gains to agreeableness. By contrast, the dashed line is roughly a 35-degree line starting at (0,0) – it represents a policy that doesn’t use the heterogeneity (just treating people arbitrarily), which yields, on average, a linear accumulation of the overall ATE. At $x=1.0$ (100% treated), both the model-based policy and the random policy coincide – at that point, everyone is treated, so the total gain is just the ATE. The fact that the Qini curve is above the solid line (forming a 'bulge') indicates that targeting is effective: we achieve more outcome gain for the same treatment fraction [@grf2024]. The area between the solid curve and dashed line up to a certain point corresponds to the RATE metrics discussed (QINI would be the total area between them from 0 to 1).

In our analysis, we report budget-constrained thresholds for treating the top 20% or 50% of recipients. Policymakers often have limits on how many people can be treated (due to cost or other constraints). The Qini curve allows us to zoom in on specific treatment fractions. For instance, if we can only treat 20% of the population, we look at $q = 0.2$ on the x-axis. The y-value of the Qini curve at $0.2$ tells us the expected gain from treating the top 20% (as ranked by the model) compared to treating 20% randomly.

The Qini curve allows us to visualise and explain how the effectiveness of the treatment allocation changes as we expand who gets treated. For an applied researcher, this is very useful. We do more than not that 'heterogeneity exists.'  We are able to advise whether, 'if we have a budget to treat X% of people, here's what outcome we can expect.' For example, suppose an education intervention has an average effect of 5 test score points. The Qini analysis might show that focusing on the top 50% of the population (maybe those with certain demographic profiles) could yield an average effect of .15 SD units for those expected to benefit, meaning we would save resources on those unlikely to gain. At 100% (treating everyone), we're back to average treatment effect, which in this example is about half this amount (0.07 SD units). So the policy insight is: if only 50% can be afforded, target that group to maximise impact.

To summarise, Qini curves allow us to identify and communicate the value of targeted policies at specific coverage levels. They show whether the benefit of the treatment is concentrated in a small segment of the population or more widely distributed. When making policy recommendations, one could use the Qini curve to decide, for instance, 'we should implement the program for the top 50% most likely to benefit; beyond 50%, the returns diminish to roughly the average effect, so including more people isn’t cost-effective.' This level of analysis goes beyond saying 'there is heterogeneity'; instead, we quantifies *how much improvement* is possible and for what percentage of a target population.

### Step 9 Avoiding over-fitting in policy evaluation (sample splitting for RATE/Qini)

An important statistical caution: when we use the same data to both train a model (such the causal forest) and evaluate its performance (like computing how well targeting does), we can get overly optimistic results. Even though the causal forest provides OOB estimates that are ostensibly out-of-sample for each individual, there is still a subtle form of overfitting possible if we are not careful. The forest was constructed to maximise heterogeneity in the training data, so using those same estimates to evaluate the policy can bias the evaluation upward (we might overstate the value of targeting because we are implicitly reusing the data).

To ensure valid inference, the best practice is to use explicit sample splitting or cross-fitting for the evaluation stage. This means, for example, after training the causal forest on the whole dataset (or a portion of it), we assess the RATE or Qini metrics on a fresh hold-out set that was not used in training. Here we use a separate hold-out set in which we train {{traning_proportion}} of the data, and use the unseen remainder as the validation set.

Why do we do this? Even though the forest’s OOB predictions are not directly from a model that saw that observation's outcome, there remains correlation – each observation’s prediction is an average from many trees, and while any given tree didn't see that observation, it saw many others including some that are also used in evaluating the policy. There is also the fact that OOB predictions are a form of cross-validation but not a true independent test, especially since the forest structure was influenced by all data. To be rigorous, researchers often do a double sample split: one split to train the forest, and a second split (completely independent) to compute the policy metrics like Qini or evaluate a specific targeting rule. This double-splitting ensures that when we say 'targeting the top 20% yields X improvement,' that claim is verified on data that played no part in determining who was top 20%. Thus, our inference (confidence intervals, p-values for heterogeneity) remains valid and not overly optimistic.

In plain terms, our workflow is as follows:

1. Split the data into two parts: A and B.
2. Use part A to train the causal forest and get $\\hat{\\tau}(x)$.
3. Use part B to evaluate heterogeneity: apply the targeting rule in B, compute observed gains, and estimate RATE/Qini metrics on that held-out data.

By doing this, when we report 'p = 0.01 for heterogeneity' or 'Qini = 0.10 at 20% spending', we know these numbers are honest assessments of how the model would perform on new data, not just the data we fit it to.

Even with OOB estimates, further sample splitting guards against optimistic bias.  This extra step is crucial for rigour: it prevents us from convincing ourselves that a complicated model is useful when in fact it might be explaining noise.

### Step 10: Translating Insights into a Policy Tree

Causal forests, while powerful, are 'black box' in nature – they may involve hundreds of trees and intricate splits that are not easy to interpret. For practical decision-making (especially in policy contexts), stakeholders often prefer a simple rule (for example, a checklist or a flowchart) that determines treatment eligibility. This is where policy trees come in. We addresse the *operational* question: given a budget and evidence of heterogeneity, which covariate splits deliver the largest welfare gain in a form stakeholders can audit?  Rather than picking out a familiar category grouping such as gender or ethnicity, policy trees reveal how effect variation aligns with resource availability.

A policy tree is essentially a simple decision tree that assigns treatment or control based on a few covariates splits, optimised to yield good outcomes. The idea is to summarise the individual treatment effects $\\hat{\\tau}(x)$ (or directly use the data) into a set of if-then rules that approximate the optimal targeting. For instance, a policy tree might look like: 'If age < 25 and baseline severity is high, give treatment; if age ≥ 25 and baseline severity is low, do not treat,' etc. These rules are much easier to communicate than a black-box forest.

`grf` integrates with the `policytree` package to derive an optimal decision tree given the causal forest's estimates [@policytree_package_2024]. We feed either $\\hat\\tau(x)$ or doubly-robust scores into `policytree::policy_tree()`, which searches for splits that maximise expected welfare subject to a depth constraint.  The result is a shallow tree (often depth 1–2) that is far easier to communicate than the full forest.


**Why use policy trees?** Apart from interpretability, policy trees can enforce fairness or simplicity constraints and avoid overfitting by limiting complexity. A shallow tree might capture the broad strokes of heterogeneity (e.g., young vs old, or high risk vs low risk) in a way that practitioners can double-check with domain knowledge. As an example from `grf` documentation:

> Deciding who to assign the program based on a complicated black-box CATE function may be undesirable if policymakers want transparent criteria. Likewise, it may be problematic for participants to learn they were denied a beneficial program solely because a black-box algorithm predicted low benefit. In such settings, we want an interpretable policy, for example, a shallow decision tree that says ‘Alice is assigned treatment because she is under 25 and lives in a disadvantaged neighborhood' see: [https://grf-labs.github.io/grf/articles/policy_learning.html](https://grf-labs.github.io/grf/articles/policy_learning.html).

This nicely illustrates how a policy tree can provide a rationale in human terms.

+**Training and validation for policy trees:** As when **constructing** causal forests and evaluating heterogeneity with RATE/Qini, we must avoid over-fitting. It is tempting to use the same data that suggested heterogeneity to also choose the best splits for the policy tree, but that can lead to optimistic results. The optimal tree is chosen to fit the training data well – if we do not validate it, we might pick a tree that works by chance quirks of the data. Therefore, we use cross-validation to select the tree's complexity (depth) and sample splitting to evaluate its performance, using {{train_proportion_decision_tree}} to train the tree and the remainder to valid it.


```{r, results='asis'}
#| label: fig-example-decision-tree
#| fig-cap: \"Example Decision Tree.\"
#| eval: true
#| include: true
#| echo: false
#| fig-width: 10
#| fig-height: 14
models_binary_batch_example$model_t2_agreeableness_z$combined_plot
```

**Example**: Recall, @fig-example-qini shows reliable treatment heterogeneity for the effect of religious service attendance on agreeableness. @fig-example-decision-tree presents a policy tree analysis in which simple treatment rules are evaluated indicating who we should 'treat' with religious service if we hope to optimise agreeableness. Here we find that three variables predict treatment benefits: weekly hours commuting, weekly hours doing housework, and household income.  Specifically:

- Participants who commute less than about 5.8 hours per week and have a household income above NZD 28,600 experience greater gains in agreeableness when attending religious services. However among those with very low income and lower commuting demands we would not expect added gains for agreeableness from religious service attendance.
- Meanwhile, those with comparable higher commuting hours who household working hours are less than 16 hours per week are not expected to benefit in Agreeableness. However, those who are heavier commuters but and also doing more housework are expected to gain in agreeableness.

This analysis holds both theoretical and practical interest. Rather than picking out a familiar category grouping such as gender or ethnicity, policy trees reveal that effect vary by resource availability.

Overall, policy trees condense the insights from causal forests into actionable guidelines.+We emphasise that deriving these guidelines includes rigorous validation: one part of the data learns the policy, another tests it. This ensures that the simple rules we recommend (e.g. 'treat those with lower time demands; do not treat incomes at a certain level') are supported by evidence rather than being artifacts of noise.

Although estimating heterogeneous treatment effects and deriving actionable policy rules can be both theoretically and practically important, two considerations should temper our enthusiasm.

First, the factors highlighted in policy trees are predictors of treatment-effect variability, but these predictors do not themselves have a causal interpretation. Returning to the example above, we should not infer that intervening to set someone’s travel and housework times to different values would change the variability in response. To understand the causal effects of joint interventions, we would need a different analysis. Decision trees are important because they draw attention to segments of a population likely to benefit, but they do not clarify the effects of changing a population structure.

Second, decisions about prioritising treatment rules are often ethical and political. Even if we set aside uncertainties in the modelling process, few would argue that fairness and justice should be determined by optimisation rules alone.. Even if we set aside uncertainties in the modelling process, few would argue that fairness and justice should be determined by optimisation rules alone. Such questions are typically resolved through democratic processes that involve stakeholder consultations, reflection on social values, a reckoning with historical inequities, and considerations beyond the scope of statistical analyses."

appendix_confusions_cross_lagged_model_deficiencies_text<-"
## Appendix {{appendix_confusions_cross_lagged_model_deficiencies}}: Inadequacy of Cross Lagged Models

Aiken's regression textbook to assert that three conditions must generally be met to establish causality.


Where $X$ represents the exposure or treatment and $Y$ the outcome, Aiken asserts:

1. $X$ and $Y$ must be correlated.
2. The correlation must not be spurious or due to a confounding factor.
3. $X$ must precede $Y$ in time.


The modern causal inference literature, which has advanced considerably over the past two decades in fields such as epidemiology, computer science, economics, and policy research, has shown that these assumptions are inadequate. Consider the following limitations of the traditional veiw.


### Condition 1: It is not true that causation implies correlation


Using Pearl’s graphical models, causality can be defined as $X \rightarrow Y$, where conditioning on a mediator is denoted by $\boxed{M}$. Pearl has shown that in the scenario $X \rightarrow \boxed{M} \rightarrow Y$, causality can exist without a direct association between $X$ and $Y$. This is crucial because, with only two waves of data, any variable measured at Wave 1 could act as a mediator, potentially obscuring the true association between $X$ at time 1 and $Y$ at time 2. To address mediator bias effectively, at least three waves of data are required to disentangle these causal pathways [@vanderweele2020].

### Condition 2: The confounding criterion is insufficient to evaluate causality


This condition essentially restates the idea that an association must be causal to be valid, which, although trivially true, adds no meaningful insight.

The confounding criterion typically applies when treatment $X$ and outcome $Y$ share a common cause, $L$. By adjusting for $L$, we aim to remove the confounding effect. However, consider a scenario where $X$ precedes $Y$ and both $X$ and $Y$ cause $L$, without a direct relationship between $X$ and $Y$. In Pearl's graphical terms, this would be represented as $X \\rightarrow \boxed{L} \\leftarrow Y$. Conditioning on $L$ would induce a spurious association between $X$ and $Y$ that would not exist without conditioning. This demonstrates even when we account for confounding by commong cause, the association between $X$ and $Y$ may be biased.

More fundamental, when including multiple varaibles in our models, we do not automatically obtain consistent causal effect estimates Take the simple case where $L \\rightarrow X \\rightarrow Y$, and suppose that $L \\rightarrow Y$.  To consistently estimate the a causal effect of $X \\rightarrow Y$ we must adjust for $\boxed{L}$.  If the causal relationships we have just described are correct, we may obtain valid causal inference for $X \\rightarrow Y$ by adjusting for $\\boxed{L}$. However the coefficient we obtain for $L$ in our stastical model will not consistently estimate the causal effect of $L \\to Y$ because $X$ is a mediator, such that  $L \\to \boxed{X} \to  Y$.

Inference become even more delicate when we add variables to our model because spurious correlations may be induced. For example, suppose that $L$ is no casually related to $Y$, but causally related to $X$. Suppose further that and unmesaured variable $U$ is related to both $X$ and $Y$ as a common cause.  In this case, there is unmeasured confounding for $X \\to Y$. However, conditioning on $X$ introduces bias for the path for $L$ to $Y$ because $\boxed{X}$ is a collider of $U$ and $L$. In a model that includes $\boxed{X}$ and to will appear that $L$ is associated with $Y$, however were we to simply focus on the unconditional realtionship between $L$ and $Y$ no correlation would be observed. Pearl's proved these points in @pearl1995, yet the remain under-appreciated in many social sciences, including psychological science. However, a consensus in causal inference holds that we should first clarify the causal effect of interest, then assess its identification conditions, then develop an appropriate estimator before we attempt a statistical model.  Practically this means investigators typically require separate models for each causal effect estimate of interest [@mcelreath2020; @westreich2013].


### Condition 3: Temporal precedence is necessary but does not clarify identification


It is true that for $X$ to cause $Y$, $X$ must precede $Y$ in time. However in observational seetings, we must typically collect information on prior states of $Y$ and prior states of $X$ to ensure valid inferences for the causal effects of later states of $X$ on (still) later states of $Y$ . Consider a feedback loop such as $X_0 \rightarrow Y_1 \rightarrow X_2 \rightarrow Y_3$. Here, we imagine that $X$ and $Y$ affect each other over time, (as might occur when threat perceptions affect authoritarian attitudes, which then reshape future threat perceptions...). To correctly estimate the causal effect of $X_t$ on $Y_{t+1}$, we would need to control for both $Y_{t-1}$ and $X_{t-1}$. In regression form, this would appear as:


$$ {\\tt model <- lm(Y_{t+1} \\sim A_t + Y_{t-1} + A_{t-1} + L_{t-1}, ...) }$$

Control for prior exposure and prior outcome is necessary wherever there are unmeasured common causes of both[@vanderweele2020]. Importantly, this model cannot be estimated with only two waves of data; three waves are the minimum required to capture these dynamics. Therefore, the temporal precedence condition, while necessary, is insufficient for drawing reliable causal inferences when data are limited.


### Time series data collected at a four month interval raise in a small student sample are unlikely to yield sufficiently many observations to obtain causal effect estimates


Beyond these conceptual issues, there are practical limitations to causal modelling. For a causal model to be credible, sufficient variation in $X$ over time is necessary. For instance, if we are estimating the causal effect of threat perceptions ($X$) on authoritarianism ($Y$) over a four-month period, we need enough individuals whose threat perceptions have changed to observe any real effects. This variation must also occur across all levels of covariates (e.g., $L$). Without sufficient change, we would not expect to observed causal effects [@vanderweele2020]. However, we cannot rule out causality over longer intervals, which may be required to obtain valid instances of threat triggering.

Notably, experiments are the gold standard for causal inference, and should be preferred when feasible and ethical. We are not given an explanation of why experiments were not used in this thesis. As indicated above, the decision to avoid experiments should be explained.


### Cross-Lagged panel models typically do not afford valid causal effect estimates


Lastly, Cross-Lagged Panel Models (CLPMs), such as those used in Study 4, do not typically offer valid causal contrasts needed to estimate causal effects. The probem is that Cross-Lagged Panel Models lack explicit counterfactual comparisons, which are essential for determining what $Y_{t+1}$ would have been if $X_t$ had taken a different value. Instead, Cross-Lagged Panel Models  only observe $Y_{t+1}$ given the actual value of $X_t$, making it impossible to model hypothetical scenarios. Furthermore, these models typically do not adequately adjust for confounding. As a result, the findings in Study 4 do not provide credible causal inferences.  Gerard notes these problems, but this reader is left in doubt about the scholarly value of the study if the coeffecients reported, because if Gerard's critical observations are right the coefficients are uninterpretable.
"

# yola
# GRF USE -----------------------------------------------------------------

# grf use  ----------------------------------------------------------------


explain_grf <- "
Here, we explain how the `grf` R package (Generalized Random Forests) can be used to estimate causal effects with causal forests.

#### Menu of HTE diagnostics

Investigators may run one, several, or all of the following, depending on the scientific or operational question:
* **RATE AUTOC / RATE Qini (global evidence)** – 'Should we abandon a uniform policy?'
* **Qini curves (budget lens)** – 'If we treat at most $p\\%$, what uplift should we expect?'
* **Policy trees (decision rule)** – 'Which simple, transparent allocation maximises welfare under constraints?'

The tools are complementary, not sequential.  A study may stop after RATE, skip RATE and go straight to a policy tree, or report the full trio; the choice should reflect whether the aim is evidence, optimisation, or communication.

### Step 1: Estimating the Average Treatment Effect (ATE)

The Average Treatment Effect (ATE) is the overall effect of the treatment in the population. It answers the question: on average, how much does the treatment change the outcome compared to not treating? In a randomised trial, the ATE can be estimated simply by the difference in mean outcomes between the treated and control groups. For example, if a randomised intervention is designed to improve a psychological outcome such as well-being, the ATE would be the difference in the average outcome for those who received the intervention versus those who did not.

Formally, we can define the ATE as $E[Y(1) - Y(0)]$, where $Y(1)$ is the outcome if an individual receives the treatment and $Y(0)$ is the outcome if they receive the control. In practice, the `grf` package provides functions like `average_treatment_effect()` that can compute the ATE (often using techniques like doubly robust estimation to improve precision). This initial step gives us a baseline: for instance, we might find that a new therapy improves an outcome by, say, .25 points (1-7 scale) on average.

Why start with the ATE? The ATE tells us if the treatment works on average if everyone were randomised to treatment as compared with everyone not receiving the treatment. Note however that even if the ATE is essentially zero (or unreliable), there may nevertheless be value in pursuing heterogeneity if some portion of the population is benefited or — equally important — if some portion is harmed.

Moreover, even if the ATE is non-zero (e.g. the treatment has a positive overall effect), the next question is whether this effect is homogeneous (similar for everyone) or heterogeneous (different for different individuals). Thus, whether or not we detect reliable Average Treatment Effects we are led to investigate heterogeneous treatment effects.

+Because multiple outcomes were evaluated, we corrected ATE results using {{ate_adjustment}} at α = {{ate_alpha}}.  For all heterogeneity (CATE) tests allow for controlled the false-discovery rate with {{cate_adjustment}} at q = {{cate_alpha}} [@benjamini1995controlling].



### Step 2: Assessing Heterogeneous Treatment Effects (HTE)

Realistically, treatment effects are often heterogeneous, meaning the treatment's magnitude (and direction) of effect can differ across individuals and subgroups. For example, a therapy might work better for participants with a certain personality trait, or an educational intervention might benefit students with lower initial skills more than those with higher skills. Heterogeneous Treatment Effect (HTE) analysis seeks to estimate how the treatment effect varies as a function of individual characteristics. We denote the Conditional Average Treatment Effect (CATE) for an individual with features $x$ as:

$$\\tau(x) = E[Y(1) - Y(0)\\mid X = x]$$

where $X$ represents the covariates (individual characteristics). In plain language, $\\tau(x)$ is the expected treatment effect for an individual with attributes $x$. If $\\tau(x)$ is the same for all $x$, the treatment effect is homogeneous (everyone benefits the same amount). If $\\tau(x)$ varies — for instance, $\\tau(x)$ is higher for some people and lower or even negative for others — then we have treatment effect heterogeneity.

**Standard parametric approaches vs. HTE**: Traditionally, researchers might use regression models with interaction terms to examine heterogeneity. For example, one might include terms like Treatment $\times$ Covariate in a linear regression to see if the effect differs by that covariate. However, standard parametric models impose strong assumptions. Often a simple model assumes a homogeneous effect (one treatment coefficient for all) unless specific interactions are added. Even when interactions are included, the model usually assumes a particular functional form (e.g. linear or additive effects of covariates). This can be very restrictive – real heterogeneity might involve complex, non-linear combinations of attributes that are hard to pre-specify. If we try to include many interaction terms or non-linear effects (such as polynomials or splines), we risk model misspecification (choosing the wrong form) and overfitting in small samples [@Sadique2022]. In short, classical regression approaches require us to guess the right pattern of heterogeneity in advance, and they treat everything outside that guess as noise.

**Example:** imagine a study on a new depression therapy. A simple model might assume the therapy has the same effect on all patients (homogeneous effect). If we suspect younger patients respond differently than older patients, we could add an age interaction. But what if the effect actually depends on a combination of age and baseline symptom severity in a non-linear way? Capturing that with a parametric model would require carefully specifying complex interactions (e.g. Age $\times$ Severity, perhaps non-linearly) and would quickly become impractical with many covariates.

Because of these challenges, we turn to a more flexible, data-driven approach: causal forests. These allow the data to reveal heterogeneity without us having to specify a particular form.

### Step 3: Estimating Individualised Effects with Causal Forests

A causal forest (as implemented in grf) is a machine learning method that extends the idea of random forests to estimate $\\tau(x)$ (the CATE) for each individual. A causal forest is an ensemble of many decision trees that are grown specifically to capture differences in treatment effect across individuals [@grf2024]. In essence, each tree in a causal forest partitions the data based on covariates, aiming to group together individuals with similar treatment responses. By averaging over many such trees (each built on a random subset of data and covariates), the forest produces a robust estimate of the individual treatment effect for each person’s feature profile.

**How causal forests avoid strong assumptions**: Unlike a parametric model, a causal forest model does not assume the treatment effect is constant or linear in the covariates. It can discover non-linear relationships and interactions automatically. For example, a causal forest might find that for younger participants the therapy is very effective, whereas for older participants with high baseline depression the effect is smaller – even if we didn't explicitly program an 'age $\times$ baseline' interaction. The forest algorithm will try splitting the data on different covariates to maximize differences in outcomes between treated and control within each split, effectively letting the data determine where the treatment effect differs. This means we are not forcing a single form of heterogeneity; the model can capture complex patterns (such as a treatment working only at certain combinations of characteristics) that a linear model might miss.

**Causal forest mechanics:** Each tree in a causal forest is constructed in a way that is similar to a standard random forest, but the splitting criterion is tailored to treatment effect estimation. At each split, the algorithm looks for a covariate and a cut-point that best separate the data into two groups with different treatment effects. In other words, it tries to maximize the contrast in outcomes between treated and control in the two child nodes [@grf2024]. This is different from a standard regression tree that would split based on differences in outcome levels; the causal tree splits based on differences in **treatment effects. Once the tree is grown, the treatment effect within each final leaf can be estimated by comparing the average outcomes of treated vs. control units in that leaf. To get a prediction for a new individual with features $x$, we drop that individual down each tree (finding which leaf they land in), and take the leaf's estimated treatment effect. The causal forest averages those estimates across many trees, yielding $\\hat{\tau}(x)$, an estimate of that individual's treatment effect.

Importantly, grf uses an approach called 'orthogonalisation' (or 'Robinson’s transformation') to handle confounding in observational data: it first estimates the outcome without treatment and the propensity of treatment, then focuses the forest on the residuals [@wager2018]. This ensures that the forest primarily learns the difference attributable to treatment, not just baseline outcome differences or propensities. (In a randomised experiment, this step is less critical since treatment assignment is independent of $X$, but it is very useful in observational studies.)

By using a causal forest, we obtain an estimate $\\hat{\\tau}(x_i)$ for each individual $i$ in our sample (often called individual treatment effect or ITE estimates). These estimates tell us who benefits more or less from the treatment according to the model.

### Step 4: Honest Estimation and Out-of-Bag Validation

One concern with flexible models (like forests) is overfitting – they might pick up random noise as if it were a real pattern, especially when trying to estimate many individual effects. The `grf` package addresses this through a technique called honest estimation and by using out-of-bag (OOB) validation.

**Honest trees:** an honest causal tree is built so that the data used to decide where to split the tree is separate from the data used to estimate the treatment effect in each leaf [@wager2018]. In practice, this can be done by randomly splitting the sample into two halves: one half is used to grow the structure of the tree (find the splits based on covariates), and the other half is used to compute the treatment effect estimates within the leaves. Because an observation is either used for splitting or for estimation (but not both) in any given tree, the estimates in each leaf are unbiased by the selection of that leaf. This honest approach guards against a tree that carves the data too finely to chase noisy differences that will not hold up in new data [@wager2018]. Causal forests in `grf` implement honesty by default, meaning each tree in the forest is grown in a way that ensures the treatment effect estimate at the end is an 'out-of-sample' estimate relative to the splits that created the leaf [@grf2024].

Out-of-bag predictions: in a random forest, each tree is typically built on a bootstrap sample (random sampling with replacement) of the data. This means about one-third of the observations are left out (not used) in that tree – these are called 'out-of-bag' observations for that tree. Since they were not used to train that tree, we can use them to get an unbiased prediction for those observations. `grf` uses this idea to produce OOB estimates of $\\tau(x)$ for every training point: each observation’s OOB estimate is the average of predictions from all trees where that observation was not in the training sample. This is like a built-in cross-validation. The OOB estimates are honest in the sense that for each individual, we're predicting their treatment effect using only trees that did not observe that individual's outcome during training [@grf2024]. These OOB estimates can be used to assess model fit and to perform certain tests (with caution, as we discuss later).

By combining honesty and OOB estimation, causal forests avoid the worst of overfitting while still using all the data. In fact, these measures enable the forest to provide valid confidence intervals and variance estimates for the treatment effects. For example, grf can calculate standard errors for $\\hat{\\tau}(x)$ using the variability across trees (with a method that groups trees and compares their predictions) [@grf2024]. The bottom line is that the forest's individual effect estimates are 'honest' (out-of-sample) estimates, increasing our trust that these effects are not just reflecting noise.

### Step 5 Handling Missing Data and Basic Validation

Empirical data often have missing values in some covariates. Unlike many traditional methods that might require dropping observations or imputing values, `grf` can handle missing covariate values directly. `grf` uses a strategy called the Missing Incorporated in Attributes (MIA) splitting rule [@grf2024]. In simple terms, when considering a split on a variable that has some missing values, the algorithm treats 'missing' as its own category: it finds splits that can send missing values one way and non-missing another way. For example, suppose we have a covariate like income where some values are missing. A causal tree could have a split that says 'if Income > 50K go left, if Income <= 50K go right, and if Income is missing, also go right (or go left)' -- essentially handling the missingness within the tree structure. This way, we do not have to drop people with missing income; the forest can still use partial information from other covariates and also possibly learn if 'missingness' itself is informative (perhaps not reporting income correlates with some outcome). By using MIA, the causal forest implicitly handles missing data without a separate imputation step, preserving information and avoiding bias that might come from improper imputation [@grf2024].

Beyond missing data, another core aspect of model validation is ensuring our findings are not artifacts of particular sample splits or tuning choices. Although random forests typically have a few hyperparameters (number of trees, depth, etc.), the defaults in `grf` are often reasonable, and we employ them here. Users can use cross-validation to fine-tune parameters such as minimum leaf size or complexity if needed. For example, one might try different minimum leaf sizes and check which yields the best out-of-sample predictive performance for treatment effects. However, because causal forests average over many trees and use honesty, they are relatively robust and often do not require extensive tuning.


### Step 6 Global evidence with RATE (AUTOC & Qini)

After fitting a causal forest and obtaining individualised effect estimates $\\hat{\tau}(x)$, a crucial question is: do these estimates provide evidence that treatment effects truly vary, or could the apparent heterogeneity be just noise? In other words, we want to test the hypothesis that the treatment effect is actually the same for everyone.

We begin with an approach implemented in the `grf` framework that uses the Rank-Weighted Average Treatment Effect (RATE) metrics as a basis for a hypothesis test [@grf2024]. The idea is to ask: if we were to use our estimated $\\hat{\\tau}(x)$ to prioritise who gets treated, would outcomes improve compared to treating people at random? If there were no heterogeneity, then any rule based on $X$ (including our $\\hat{\\tau}(x)$ estimates) should do no better than random assignment. If there is heterogeneity and we have identified it correctly, then targeting those with higher $\\hat{\\tau}(x)$ should yield better outcomes on average.

Concretely, we can sort individuals by their estimated benefit $\\hat{\tau}(x)$ (from highest to lowest). Suppose we progressively treat people in that order – first the top 10% most likely to benefit, then 20%, and so on. If heterogeneity is meaningful, the people with higher $\\hat{\\tau}(x)$ should indeed have larger actual treatment effects on average, so treating them first gives us a bigger gain than treating a random 10% or 20%. We can summarise this with a Targeting Operator Characteristic (TOC) curve – analogous to an ROC curve in classification – which for each fraction $q$ of the population treated (x-axis) shows the excess treatment effect achieved by targeting based on $\\hat{\tau}$ (y-axis) compared to random treatment [@grf2024]. If there were no heterogeneity, this curve would be flat (no gain from targeting). If there were heterogeneity, the curve would rise – especially at low $q$, where we are focusing on the top predicted responders [refer to @yadlowsky2021evaluating].

The test for heterogeneity can then be based on the area under this TOC curve or other summaries of it. Specifically, grf defines:

- **AUTOC:** the Area Under the TOC Curve. This essentially integrates the benefit of targeting across all possible fractions treated [@grf2024]. If there's no heterogeneity, AUTOC should be zero (no area under the curve, since the curve is flat at zero gain).

- **Qini:** a related metric (named after a concept by Radcliffe, 2007) which is a weighted area under the TOC [@radcliffe2007using]. The Qini index weights larger fractions more heavily (technically, Qini = $\\int_0^1 q \\cdot \\mathrm{TOC}(q),dq$) [@radcliffe2007using]. Intuitively, AUTOC tends to emphasise the extremes (are there a small group of people with very large effects?) whereas Qini gives more weight to broader improvements (moderate effects spread across more people) [@yadlowsky2021evaluating].

Both metrics aggregate the **whole** targeting curve.  A single negative stretch can drag the average below zero, so a reliably negative RATE warns that *no blanket ranking policy should be adopted*, no matter how attractive the extreme tail looks.

To test for heterogeneity, we can check whether these metrics are significantly greater than zero. Under $H_0$ (no heterogeneity), we expect no gain from targeting, so, for example, AUTOC = 0. We can compute an estimate of AUTOC or Qini from the data (using held-out samples to avoid bias) and compute a standard error. Thanks to theoretical results, these metrics satisfy a central limit theorem, so we can do a simple $t$-test. Essentially, we test:

- $H_0$: RATE (e.g., AUTOC or Qini) = 0 (no heterogeneity detectable)
- $H_A$: RATE $> 0$

If the test rejects $H_0$, we have evidence that some individuals benefit more than others, beyond what we would expect by chance [@grf2024]. This is strong evidence of treatment effect heterogeneity. If we fail to reject, it suggests that any differences our forest found might not be statistically reliable – in other words, we cannot be sure the treatment effect isn't basically uniform.

To summarise this step: we use the forest's output to construct a test for heterogeneity. This moves us from simply eyeballing $\\hat{\\tau}(x)$ values (which can be noisy) to a rigorous statistical test of whether tailoring treatment based on $X$ has potential value.

### Step 7 Quantifying policy value with RATE metrics

Beyond hypothesis testing, the RATE framework is valuable for quantifying how much improvement we might gain by individualised treatment policies. RATE stands for Rank-Weighted Average Treatment Effect, which as described, comes from considering a treatment prioritisation rule $S(X)$ that ranks individuals by some score (in our case, the score is the predicted benefit $\\hat{\\tau}(X)$) [@grf2024]. We want to evaluate how good this rule is at targeting treatment to those who benefit the most.

Two key RATE metrics have already been mentioned: AUTOC and Qini. Both summarise the TOC curve (the performance of our targeting rule across the range of possible treated fractions). To interpret them in plain terms:

- AUTOC (Area Under the Targeting Curve): This is the integral of the improvement achieved by targeting, across all possible proportions treated. An equivalent interpretation is that AUTOC is a weighted average of treatment effects where higher weight is placed on the highest-score individuals (since at very low fractions $q$, we’re only treating the top-scoring people). A larger AUTOC means our rule does a very good job at ranking people – the top-ranked have much bigger effects than the average. We refer to this as one type of RATE metric.

- Qini: this is another summary of the targeting curve, but uses a different weighting (in fact, linearly increasing weight by fraction treated) In practice, that means Qini cares more about how well we do when treating larger portions of the population, not just the very top slice. If treatment benefits are spread out, the Qini will capture that better. We can think of Qini as measuring the aggregate gain from using the rule as we expand treatment – it places equal emphasis on high-benefit individuals and those with more modest benefit (because as you increase $q$, the weight $q$ in the integral increases) [@yadlowsky2021evaluating].

Both AUTOC and Qini are numbers (scalars) that we can compute for a given prioritisation rule (like the causal forest's ranking). They can be compared to baseline strategies. For instance, one baseline is a policy that does not use any covariates – e.g. treat a random fraction $q$ of people. That baseline would by definition have a TOC curve of zero (no preferential gain) and thus AUTOC = Qini = 0. Another baseline could be treating everyone (which yields the ATE as the outcome gain, but of course no targeting because everyone is treated.  This latter baseline is the rule that we use here. We are interested in incremental improvements over the ATE by targeting  $\\hat{\\tau}(X)$).

In a policy-setting context, these metrics help answer the question: 'Could we improve outcomes by targeting the treatment to specific individuals instead of a one-size-fits-all approach?' and 'If yes, how large might that improvement be?' For example, suppose the ATE of a job training program is an earnings increase of USD 500 on average. A statistically reliable Qini or AUTOC might reveal that if we only gave the program to the half of people who benefit the most (according to some characteristics), the average impact for those treated could be much higher, say $1000, and essentially zero for those not treated – implying resources can be better allocated. On the other hand, if no heterogeneity were found, we cannot be certain that targeting would change the payoff. Additionally, we might find reliable evidence that **targeting** performs worse than random assignment, and advise caution if the aim is to maximise benefits across the population.


To make this concrete, `grf` provides a function rank_average_treatment_effect() that computes these metrics and even performs statistical tests. An analyst might use it in an applied study to report, for example: 'Using the causal forest’s predicted CATEs as a prioritisation rule, we find a rank-weighted treatment effect (AUTOC) of 0.15 (SE = 0.05), indicating a substantial improvement from targeted treatment over a non-targeted policy [@grf2024]. The Qini coefficient is 0.10 (SE = 0.03). Both are reliably greater than zero, reinforcing the presence of actionable heterogeneity. These numbers would be in the units of the outcome (or outcome difference), and they tell policymakers the potential gain from personalised treatment assignment.

In summary, RATE metrics such as AUTOC and Qini serve a dual purpose: (1) they statistically test for any heterogeneity (as discussed in Step 6) and (2) they quantify how effective our estimated heterogeneity is for improving outcomes through targeting. They help translate complex heterogeneity estimates into a single measure of 'policy value.' If these metrics are small or insignificant, it means even if effects vary, it may not be useful for targeting (perhaps the variation is too minor or too uncertain). If they are large, it suggests real potential to increase impact by focusing the treatment where it works best.

### Step 8 Budget-constrained gains via Qini curves

While single-number metrics (AUTOC, Qini indices) are useful, it is often enlightening to look at the Qini curve or the underlying targeting curve visually. The Qini curve plots the cumulative gain in outcome as we allocate treatment to a larger fraction of the population, ranking by predicted treatment effect [@grf2024]. The x-axis typically is the fraction (or percentage) of the population treated (sorted from highest predicted benefit downwards), and the y-axis is the net gain achieved by that policy compared to treating that fraction at random.

For example, @fig-example-qini presents a Qini curve result in a causal forest analysis investigating the effects of religious service attendance on personality (here 'Agreeableness'). The straight, solid (orange) line indicates the expected gains from treating indiscriminately according to the ATE. The dashed (blue) curve indicates the expected gains from treating using  $\\hat{\\tau}(X)$). The x-axis ('spend') represents the fraction of individuals treated (scaled to a budget), and the y-axis ('gain') represents the improvement in outcome (e.g., additional treatment effect) from targeting those individuals versus random assignment. A steeper, higher dashed curve indicates that the model-based targeting yields much better outcomes for the treated fraction than random selection (solid line). This curve stops (or 'plateaus') after treating approximately 50% of population, because at that point we have assigned treatment to the units predicted to benefit, $\\hat{\\tau_i}(X)> 0)$.


```{r, results='asis'}
#| label: fig-example-qini
#| fig-cap: \"Example Qini Curve.\"
#| eval: true
#| include: true
#| echo: false
#| fig-width: 12
#| fig-height: 12
models_binary_batch_example$model_t2_agreeableness_z$policy_tree
```


In the above illustration, the Qini curve rises sharply at the beginning, indicating that the first several percent of individuals treated (those with the highest predicted $\tau(x)$) yield a large gains to agreeableness. By contrast, the dashed line is roughly a 35-degree line starting at (0,0) – it represents a policy that doesn’t use the heterogeneity (just treating people arbitrarily), which yields, on average, a linear accumulation of the overall ATE. At $x=1.0$ (100% treated), both the model-based policy and the random policy coincide – at that point, everyone is treated, so the total gain is just the ATE. The fact that the Qini curve is above the solid line (forming a 'bulge') indicates that targeting is effective: we achieve more outcome gain for the same treatment fraction [@grf2024]. The area between the solid curve and dashed line up to a certain point corresponds to the RATE metrics discussed (QINI would be the total area between them from 0 to 1).

In our analysis, we report budget-constrained thresholds for treating the top 20% or 50% of recipients. Policymakers often have limits on how many people can be treated (due to cost or other constraints). The Qini curve allows us to zoom in on specific treatment fractions. For instance, if we can only treat 20% of the population, we look at $q = 0.2$ on the x-axis. The y-value of the Qini curve at $0.2$ tells us the expected gain from treating the top 20% (as ranked by the model) compared to treating 20% randomly.

The Qini curve allows us to visualise and explain how the effectiveness of the treatment allocation changes as we expand who gets treated. For an applied researcher, this is very useful. We do more than not that 'heterogeneity exists.'  We are able to advise whether, 'if we have a budget to treat X% of people, here's what outcome we can expect.' For example, suppose an education intervention has an average effect of 5 test score points. The Qini analysis might show that focusing on the top 50% of the population (maybe those with certain demographic profiles) could yield an average effect of .15 SD units for those expected to benefit, meaning we would save resources on those unlikely to gain. At 100% (treating everyone), we're back to average treatment effect, which in this example is about half this amount (0.07 SD units). So the policy insight is: if only 50% can be afforded, target that group to maximise impact.

To summarise, Qini curves allow us to identify and communicate the value of targeted policies at specific coverage levels. They show whether the benefit of the treatment is concentrated in a small segment of the population or more widely distributed. When making policy recommendations, one could use the Qini curve to decide, for instance, 'we should implement the program for the top 50% most likely to benefit; beyond 50%, the returns diminish to roughly the average effect, so including more people isn’t cost-effective.' This level of analysis goes beyond saying 'there is heterogeneity'; instead, we quantifies *how much improvement* is possible and for what percentage of a target population.

**Practical note:** because the Qini curve is derived from the **same validation split** used for RATE, both views sit on equal footing and over-optimism is checked.


+### Step 9 Avoiding over-fitting in policy evaluation (sample splitting for RATE/Qini)

An important statistical caution: when we use the same data to both train a model (such the causal forest) and evaluate its performance (like computing how well targeting does), we can get overly optimistic results. Even though the causal forest provides OOB estimates that are ostensibly out-of-sample for each individual, there is still a subtle form of overfitting possible if we are not careful. The forest was constructed to maximise heterogeneity in the training data, so using those same estimates to evaluate the policy can bias the evaluation upward (we might overstate the value of targeting because we are implicitly reusing the data).

To ensure valid inference, the best practice is to use explicit sample splitting or cross-fitting for the evaluation stage. This means, for example, after training the causal forest on the whole dataset (or a portion of it), we assess the RATE or Qini metrics on a fresh hold-out set that was not used in training. Here we use a separate hold-out set in which we train {{training_proportion}} of the data and use the unseen remainder as the validation set.


Why do we do this? Even though the forest’s OOB predictions are not directly from a model that saw that observation's outcome, there remains correlation – each observation’s prediction is an average from many trees, and while any given tree didn't see that observation, it saw many others including some that are also used in evaluating the policy. There is also the fact that OOB predictions are a form of cross-validation but not a true independent test, especially since the forest structure was influenced by all data. To be rigorous, researchers often do a double sample split: one split to train the forest, and a second split (completely independent) to compute the policy metrics like Qini or evaluate a specific targeting rule. This double-splitting ensures that when we say 'targeting the top 20% yields X improvement,' that claim is verified on data that played no part in determining who was top 20%. Thus, our inference (confidence intervals, p-values for heterogeneity) remains valid and not overly optimistic.

In plain terms, our workflow is as follows:

1. Split the data into two parts: A and B.
2. Use part A to train the causal forest and get $\\hat{\tau}(x)$.
3. Use part B to evaluate heterogeneity: apply the targeting rule in B, compute observed gains, and estimate RATE/Qini metrics on that held-out data.


By doing this, when we report 'p = 0.01 for heterogeneity' or 'Qini = 0.10 at 20% spending', we know these numbers are honest assessments of how the model would perform on new data, not just the data we fit it to.

In short, even with OOB estimates, further sample splitting is used for final evaluation to ensure our conclusions about heterogeneous effects and the benefits of targeting are reliable. This extra step is crucial for rigour: it prevents us from convincing ourselves that a complicated model is useful when in fact it might be explaining noise.

### Step 10 Transparent targeting rules with policy trees

Finally, once we have evidence of heterogeneity and an effective way to target treatment, we might want to translate the complex model into a simple decision rule. Causal forests, while powerful, are 'black box' in nature – they may involve hundreds of trees and intricate splits that are not easy to interpret. For practical decision-making (especially in policy contexts), stakeholders often prefer a simple rule (for example, a checklist or a flowchart) that determines treatment eligibility. This is where policy trees come in.

A policy tree is essentially a simple decision tree that assigns treatment or control based on a few covariates splits, optimised to yield good outcomes. The idea is to summarise the individual treatment effects $\\hat{\\tau}(x)$ (or directly use the data) into a set of if-then rules that approximate the optimal targeting. For instance, a policy tree might look like: 'If age < 25 and baseline severity is high, give treatment; if age ≥ 25 and baseline severity is low, do not treat,' etc. These rules are much easier to communicate than a black-box forest.

`grf` integrates with the `policytree` package to derive an optimal decision tree given the causal forest's estimates [@policytree_package_2024]. Our procedure is as follows: use the forest's estimates or doubly robust scores as input to a decision tree algorithm that maximises the expected outcome (welfare) under that tree policy. In other words, we finds splits that best separate who should be treated vs not to improve the overall result. The result is a shallow tree (often depth 1, 2, or 3) that is much easier to interpret than the full forest. Here we use a decision tree of depth = 2.

**Why use policy trees?** Apart from interpretability, policy trees can enforce fairness or simplicity constraints and avoid overfitting by limiting complexity. A shallow tree might capture the broad strokes of heterogeneity (e.g., young vs old, or high risk vs low risk) in a way that practitioners can double-check with domain knowledge. As an example from `grf` documentation: 'Deciding who to assign the program based on a complicated black-box CATE function may be undesirable if policymakers want transparent criteria. Likewise, it may be problematic for participants to learn they were denied a beneficial program solely because a black-box algorithm predicted low benefit. In such settings, we want an interpretable policy, for example, a shallow decision tree that says ‘Alice is assigned treatment because she is under 25 and lives in a disadvantaged neighborhood' see: [https://grf-labs.github.io/grf/articles/policy_learning.html](https://grf-labs.github.io/grf/articles/policy_learning.html). This nicely illustrates how a policy tree can provide a rationale in human terms.

+**Training and validation for policy trees:** As when **constructing** causal forests and evaluating heterogeneity with RATE/Qini, we must be careful to avoid over-fitting.
It is tempting to use the same data that suggested heterogeneity to also choose the best splits for the policy tree, but that can lead to optimistic results. The optimal tree is chosen to fit the training data well – if we do not validate it, we might pick a tree that works by chance quirks of the data. Therefore, we use cross-validation to select the tree's depth and sample splitting to evaluate its performance, using {{train_proportion_decision_tree}} to train the tree and the remainder to **validate** it.



```{r, results='asis'}
#| label: fig-example-decision-tree
#| fig-cap: \"Example Decision Tree.\"
#| eval: true
#| include: true
#| echo: false
#| fig-width: 10
#| fig-height: 14
models_binary_batch_example$model_t2_agreeableness_z$combined_plot
```

**Example**: Recall, @fig-example-qini shows reliable treatment heterogeneity for the effect of religious service attendance on agreeableness. @fig-example-decision-tree presents a policy tree analysis in which simple treatment rules are evaluated indicating who we should 'treat' with religious service if we hope to optimise agreeableness. Here we find that three variables predict treatment benefits: weekly hours commuting, weekly hours doing housework, and household income.  Specifically:

- Participants who commute less than about 5.8 hours per week and have a household income above NZD 28,600 experience greater gains in agreeableness when attending religious services. However among those with very low income and lower commuting demands we would not expect added gains for agreeableness from religious service attendance.
- Meanwhile, those with comparable higher commuting hours who household working hours are less than 16 hours per week are not expected to benefit in Agreeableness. However, those who are heavier commuters but and also doing more housework are expected to gain in agreeableness.

This analysis holds both theoretical and practical interest. Rather than picking out a familiar category grouping such as gender or ethnicity, policy trees reveal that effect vary by resource availability.

Overall, policy trees condense the insights from causal forests into actionable guidelines. We emphasise that deriving these guidelines includes rigorous validation: we use one part of the data to learn the policy and another to test it. This ensures that the simple rules we recommend (e.g. 'treat those with lower time demands; do not treat incomes at a certain level') are supported by evidence rather than being artifacts of noise.

Although estimating heterogeneous treatment effects and deriving actionable policy rules can be both theoretically and practically important, two considerations should temper our enthusiasm.

First, the factors highlighted in policy trees are predictors of treatment-effect variability, but these predictors do not themselves have a causal interpretation. Returning to the example above, we should not infer that intervening to set someone’s travel and housework times to different values would change the variability in response. To understand the causal effects of joint interventions, we would need a different analysis. Decision trees are important because they highlight segments likely to benefit, but they do not clarify the effects of changing a population’s structure.

Second, decisions about prioritising treatment rules are often ethical and political. Even if we set aside uncertainties in the modelling process, few would argue that fairness and justice should be determined by optimisation rules alone. Such questions are typically resolved through democratic processes that involve stakeholder consultations, reflection on social values, a reckoning with historical inequities, and considerations beyond the scope of statistical analyses.
"



unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.explain.grf_long",
  value = explain_grf_long
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.explain.grf",
  value = explain_grf
)

boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)




#
# appendix_timeline_text
# appendix_baseline_text
# appendix_exposure_text
# appendix_exposure_text
# appendix_outcomes_flourishing_2025_text
# appendix_confounding_lmtp_swig_text
# appendix_confounding_lmtp_dag_text
# appendix_confounding_threewave_x_text
# appendix_confounding_threewave_l_text
# appendix_assumptions_grf_text
# appendix_technical_lmtp_time_vary_text
# appendix_positivity_exposures_5_text
# appendix_positivity_exposures_1_text
# appendix_positivity_exposures_1_binary_text
# appendix_references_text
# appendix_confusions_cross_lagged_model_deficiencies_text

# initialise lists for categories
# unified_db <- boilerplate_add_entry(
#   db = unified_db,
#   path = "appendix.confounding",
#   value = list()
# )
#
# unified_db <- boilerplate_add_entry(
#   db = unified_db,
#   path = "appendix.assumptions",
#   value = list()
# )
#
# unified_db <- boilerplate_add_entry(
#   db = unified_db,
#   path = "appendix.confusions",
#   value = list()
# )

# unified_db <- boilerplate_add_entry(
#   db = unified_db,
#   path = "appendix.positivity",
#   value = list()
# )

# unified_db <- boilerplate_add_entry(
#   db = unified_db,
#   path = "appendix.technical",
#   value = list()
# )
# unified_db <- boilerplate_add_entry(
#   db = unified_db,
#   path = "appendix.outcomes",
#   value = list()
# )

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.confounding.lmtp_swig",
  value = appendix_confounding_lmtp_swig_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.confounding.lmtp_dag",
  value = appendix_confounding_lmtp_dag_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.confounding.threewave_x",
  value = appendix_confounding_threewave_x_text
)
unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.confounding.threewave_l",
  value = appendix_confounding_threewave_l_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.assumptions.assumptions_grf_text",
  value = appendix_assumptions_grf_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.positivity.exposures_5",
  value = appendix_positivity_exposures_5_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.positivity.exposures_1",
  value = appendix_positivity_exposures_1_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.positivity.exposures_1_binary",
  value = appendix_positivity_exposures_1_binary_text
)



unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.references",
  value = appendix_references_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.technical.lmtp_time_vary",
  value = appendix_technical_lmtp_time_vary_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.confusions.cross_lagged_model_deficiencies",
  value = appendix_confusions_cross_lagged_model_deficiencies_text
)


unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.timeline",
  value = appendix_timeline_text
)
unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.baseline",
  value = appendix_baseline_text
)
unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.exposure",
  value = appendix_exposure_text
)

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.outcomes.flourishing_2025",
  value = appendix_outcomes_flourishing_2025_text
)


unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.references",
  value = appendix_references_text
)
unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.outcomes.general",
  value = appendix_outcomes_text
)



# save
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)




appendix_explain_grf_short <- "
## Appendix {{appendix_explain_grf}}. Estimating and Interpreting Heterogeneous Treatment Effects with GRF {#appendix-explain-grf}

Here we explain a heterogeneous‐treatment‐effect (HTE) analysis using causal forests [@grf2024]. In our workflow, we move from the average treatment effect (ATE) to individualised effects, quantify the practical value of targeting, and finish with interpretable decision rules.


### 1 Average Treatment Effect (ATE)

The ATE answers: *'What would happen, on average, if everyone received treatment versus no one?'*

$$
    \\text{ATE}=E[Y(1)-Y(0)].
$$

Using the `grf` package, we estimate the ATE doubly-robustly.  Because we analyse several outcomes, we adjust ATE *p*-values with {{ate_adjustment}} ($\\alpha$ = {{ate_alpha}}) to control the family-wise error rate.




### 2 Do Effects Vary?  Formal Test of Heterogeneity

Define the conditional average treatment effect (CATE)

$$
  \\tau(x)=E[Y(1)-Y(0)\\mid X=x].
$$

If $\\tau(x)$ is constant, effects are homogeneous; otherwise they vary. Classical interaction models impose strong forms; **grf** uses *causal forests* to discover complex, nonlinear heterogeneity [@wager2018].  We assess heterogeneity with RATE *p*-values corrected via {{cate_adjustment}} (q = {{cate_alpha}}), controlling the false-discovery rate [@benjamini1995controlling].




### 3 Causal Forests for Individualised Estimates

A causal forest is an ensemble of 'honest' causal trees that split on covariates to maximise treated–control contrasts. For each unit $i$ we obtain

$$
  \\widehat{\\tau}(x_i)
$$

Strengths are flexibility, orthogonalisation, and per-person estimates.


### 4 Built-in Protection Against Over-fitting

  Honesty (split half/estimate half) plus out-of-bag (OOB) predictions yield unbiased $\\widehat{\\tau}(x)$ and standard errors without manual hyper-tuning.


### 5 Missing Data Handling

**grf** deploys 'Missing Incorporated in Attributes' (MIA): missingness is a valid split, so cases stay in the analysis -- no ad-hoc imputation required.


### 6 Testing for **Actionable** Heterogeneity: the TOC & RATE Metrics

Ranking units by $\\widehat{\\tau}$ defines a **Targeting Operator Characteristic** (TOC) curve: the cumulative gain from treating the top fraction $q$ of predicted responders. Two scalar summaries:

- **RATE AUTOC** – area under the entire TOC; emphasises the very highest responders.
- **RATE Qini** – weighted area with weight $q$; rewards sustained gains across larger coverage [@yadlowsky2021evaluating].

Under $H_0{:}\\tau(x)$ constant, both equal 0.
`grf::rank_average_treatment_effect()` supplies point estimates, standard errors, and $t$-tests.

**Multiplicity control**: We adjust AUTOC and Qini *p*-values with {{cate_adjustment}} (q = {{cate_alpha}}) before declaring actionable heterogeneity.

Here is an **interpretation tip**:

* AUTOC answers *'How sharply can we prioritise?'*
* Qini answers *'How valuable is targeting when budgets are modest but not tiny?'*

### 7 Visualising Policy Value: the Qini Curve

Plotting the Qini curve (cumulative gain vs $q$) reveals where returns plateau. Investigators (and policy audiences) can see at a glance whether benefits concentrate in, say, the top 20 % or persist up to 50 %.


### 8 Valid Inference for RATE / Qini

Although OOB predictions are out-of-sample per tree, they inherit forest-level dependence. We use an explicit **sample split**:

1. **Train set**: fit the causal forest and compute $\\widehat{\\tau}(x)$.
2. **Test set**: compute RATE AUTOC/Qini and run $H_0$ tests.

This second split yields honest policy evaluation and guards against optimistic bias [@grf2024].

### 9 From Black Box to Simple Rules: Policy Trees

Stakeholders value transparent criteria. The **policytree** algorithm takes $\\widehat{\\tau}(x)$ or doubly-robust scores and learns a shallow decision tree that maximises expected welfare [@policytree_package_2024].

*Advantages*: interpretability, the possibility of fairness constraints, and easy communication (e.g., *'treat if age < 25 and baseline severity high'*).

Training mirrors the split above: learn the tree on one fold, evaluate welfare on another.

> **Caveat**  Splits identify predictors of *effect variation*, not causal levers. Changing a covariate in the tree does **not** guarantee an effect on $\\tau(x)$.


### 10 Ethical and Practical Considerations

Statistical optimisation rarely aligns perfectly with equity or political feasibility. Decisions about who *should* receive treatment belong to democratic processes that weigh fairness, cost, and broader social values.


### Putting it together

The sequence—ATE, causal-forest CATEs, RATE/Qini diagnostics, Qini curve, and finally a shallow policy tree—delivers both rigorous evidence and a defensible targeting rule.  Researchers learn **how large** heterogeneity is, **where** targeting pays off under budget constraints, and **which** simple covariate splits capture most of the welfare gain, all while guarding against over-fitting and multiplicity.
"

unified_db <- boilerplate_update_entry(
  db = unified_db,
  path = "appendix.explain.grf_short",
  value = appendix_explain_grf_short
)
boilerplate_save(unified_db, data_path = my_project_path, create_backup = FALSE)




# ------------------------------------------------------
# now test generating text from the database
# ------------------------------------------------------
#unified_db <- boilerplate_import(data_path = my_data_path)

cat(unified_db$methods$analytic_approach$simple_general_approach_cate_short)
unified_db$methods$sensitivity_analysis
unified_db$methods$causal_intervention$lmtp_multi_wave



# example usage of generate_text for methods section
lmtp_methods_text <- boilerplate_generate_text(
  category = "methods",
  sections = c(
    "sample.sample_information",
    "methods.exposure_indicator",
    "causal_intervention.lmtp_multi_wave",
    "sdr_long_explanation",
  #  "causal_intervention.grf",
    "target_population",
    "eligibility.standard",
    "causal_identification_criteria",
    "confounding_control.vanderweele",
    "sensitivity_analysis.evalue",
    "missing_lmtp_time_varying"
    #"missing_data.missing_grf_simple",
  ),
  global_vars = list(
    name_exposure_variable = "Religious Service Attendance",
    n_total = 77,490,
    n_participants = 47,780,
    appendix_assumptions_grf = "Appendix E",
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
    number_exposure_waves = "five",
    time_varying_confounders = "(physical disability, employment status, partner status, and parenting status)",
    flipped_list = c("Neuroticism"),
    appendices_sample = "A",
    appendix_outcomes = "B",
    appendix_positivity = "E",
    appendix_assumptions = "E",
    baseline_wave = "NZAVS time 10, years 2018-2019",
    exposure_waves = "NZAVS time 11, years 2019-2020",
    outcome_wave = "NZAVS time 12, years 2020-2021",
    appendix_ref = "C",
    grf_appendix = "D",
    confidence_level = "95",
    appendix_explain_grf = "G",
    flipped_example = "Neuroticism",
    appendix_analytic_approach = "D",
    protocol_url = "https://osf.io/ce4t9/"
  ),
  db = unified_db
)

# print the generated text
cat(lmtp_methods_text)
unified_db$methods$missing_data$missing_lmtp_time_varying
# results example ---------------------------------------------------------
unified_db$results$grf$interpretation_policy_tree

flipped_list = "fatigue"

results_text <- boilerplate_generate_text(
  category = "results",
  sections = c(
    "grf.interpretation_ominibus_test_negative",
    "grf.interpretation_rate",
    "grf.interpretation_qini",
    "grf.interpretation_policy_tree"
  ),
  global_vars = list(
    exposure_var = "political_conservative",
    flipped_list = flipped_list,
    appendix_cate_validation_grf = "G"
  ),
  db = unified_db
)

cat(results_text)
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
  exposure_waves = "NZAVS time 11, years 2019-2020",
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
  db= unified_db
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
  exposure_waves = "NZAVS time 11, years 2019-2020",
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
  exposure_waves = "NZAVS time 11, years 2019-2020",
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
  exposure_waves = "NZAVS time 11, years 2019-2020",
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


# The following outcomes were inverted: {{flipped_list}}.


general_approach_cate_long <- "Our primary goal was to move beyond average treatment effects (ATE) and investigate whether the intervention's causale effects reliably varied across individuals based on their characteristics. To achieve this, we estimated conditional average treatment effects (CATEs) using causal forests [@grf2024]. Causal forests are a machine learning method adapted specifically for estimating how treatment effects differ across people defined by a set of covariates. These effects are interesting because they help to clarify for whom treatment effects are likely to have their strongest effects, and for whom treatments effects may not work, or be harmful.

For interpretability across different outcomes, we standardised the direction of effects. Some outcomes, like depression scores, are typically interpreted as 'lower is better'. We inverted the scales for such variables so that positive treatment effects consistently indicated improvement (e.g., a reduction in depression). This ensures that larger positive CATE estimates always signify greater benefit from the treatment.

A central challenge when invesitigating individual differences using CATE is ensuring the detected variations are genuine and not just noise or overfitting. Therefore, we rigorously evaluated the causal forest estimates using a {{sample_split}} sample split sample splitting approach. Half the data were used to train the causal forest model (i.e., to identify patterns of differing treatment effects), and the other half (the held-out data) were used exclusively for evaluation.

On the held-out data, we first assessed the calibration of the predictions [@grf2024]. This involved checking if the average of the predicted CATEs accurately reflected the overall average treatment effect (ATE) found in the evaluation sample. More importantly, we tested whether the *predicted variation* in treatment effects (heterogeneous treatment effects, HTE) was reliable. A specific 'differential prediction' test acts as an omnibus indicator ($p$-value) of whether the model successfully captures statistically significant variability in how individuals respond to the treatment [@grf2024]. We also calculated the Rank-Weighted Average Treatment Effect (RATE) [@grf2024; @wager2018]. RATE quantifies the potential real-world value of using the model to target treatment – specifically, it measures the average outcome improvement we would expect if we prioritised treatment for the individuals predicted to benefit most, compared to a simpler strategy like treating everyone or no one based only on the ATE.

To visualise the benefit of prioritisation, we used Qini curves [@grf2024]. These curves compare two scenarios evaluated on the held-out data:
1.  **Uniform Allocation:** Treating (or not treating) individuals based solely on the overall ATE.
2.  **Targeted Allocation:** Prioritising treatment for individuals ranked highest on their predicted CATE from the causal forest.

The Qini curve plots the cumulative gain (or loss) achieved by the targeted approach as we increase the proportion of the sample receiving treatment (starting with those predicted to benefit most). Positive Qini values indicate that targeting treatment based on predicted CATEs yields better overall outcomes than the uniform approach for that proportion treated. This helps determine if a personalised strategy is advantageous, and potentially for what fraction of the population.

Finally, if we detected reliable heterogeneity , we aimed to translate these complex CATE predictions into simpler, actionable rules using policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica]. Policy trees learn simple decision rules (e.g., 'Treat individuals with baseline score > X and age < Y') that optimise treatment allocation based on the estimated CATEs. Policy tree estimation identify defining baseline characteristics that distinguish subgroups with markedly different responses, providing transparent, interpretable guidelines for potentially tailoring treatment in practice.

All heterogeneity analyses, including calibration tests, RATE calculations, Qini curves, and policy trees, were conducted using R, primarily with the `grf` [@grf2024], `policytree` [@policytree_package_2024], and `margot` [@margot2024] packages. The figures presented were generated using `margot`.

Collectively, this multi-stage approach allows us to first estimate individualised effects, rigorously test whether these estimated variations are reliable, evaluate the potential gains from targeting treatment, and finally, derive simple rules to guide personalised intervention strategies if justified by the data.
"


# We standardised effect directions by inverting any outcome where 'lower is better', so positive values consistently denote improvement. The following outcomes were inverted: {{flipped_list}}.

general_approach_cate_short <- "Our primary aim was to look beyond average treatment effects (ATE) and explore whether the intervention’s impact varied systematically across individuals. To that end, we estimated individualised treatment effects, also called conditional average treatment effects (CATEs), using causal forests [@grf2024], a machine learning approach tailored for detecting treatment-effect heterogeneity based on covariates.

A common concern when modelling individual differences using CATE is distinguishing real variation from noise. To address this, we used a {{sample_split}} sample splits. We trained the causal forest on half the data, then tested model predictions exclusively on the remaining half to prevent overfitting.

In the held-out sample, we checked whether the predicted CATEs were well calibrated [@grf2024]. This involved comparing (i) the mean of the predicted effects to the overall ATE in the evaluation sample, and (ii) testing whether the predicted *variation* in effects was reliable. A differential prediction test provided an omnibus $p$-value for whether the model captured genuine heterogeneity in responses. We also calculated the Rank-Weighted Average Treatment Effect (RATE), which estimates how effectively a model-based targeting strategy would outperform a uniform treatment strategy [@grf2024; @wager2018].

To visualise the added value of targeting, we used Qini curves [@grf2024]. These compare:

1.  **Uniform Allocation**: treating or not treating everyone based on the overall ATE.
2.  **Targeted Allocation**: treating individuals ranked highest by their predicted CATE first.

The Qini curve shows the cumulative gain (or loss) of the targeted strategy as we treat larger proportions of the sample. Positive values suggest that prioritising high-CATE individuals outperforms uniform treatment, helping identify whether, and at what coverage level, personalisation is beneficial.

Finally, for practical implementation, we used policy trees [@policytree_package_2024; @athey2021; @athey_2021_policy_tree_econometrica] to derive simple, rule-based recommendations. Policy trees split participants into subgroups defined by key moderators, providing clear 'treat' or 'do not treat' cut-points, evaluated for effectiveness on the held-out data. All heterogeneity analyses—including calibration tests, RATE, Qini curves, and policy trees—were done in R using `grf` [@grf2024], `policytree` [@policytree_package_2024], and `margot` [@margot2024].

Taken together, this multi-stage approach first identifies individualised treatment effects, then tests their reliability, estimates the added value of targeted intervention, and, if justified, provides simple rules for personalising treatment in practice."

interpretation_rate_test <- "To assess the potential practical value of tailoring treatment based on individual predictions, we examined the Rank-Weighted Average Treatment Effect (RATE) and Qini curves [@grf2024; @wager2018]. These predictions are estimates (denoted $\\hat{\\tau}(X_i)$) of the underlying conditional average treatment effect (CATE, $\\tau(X_i)$), which represents the average treatment effect for individuals with specific baseline covariates $X_i$. Where an outcomes ({{flipped_outcomes}}), scales were inverted so higher predicted effects ($\\hat{\\tau}(X_i)$) always indicate greater benefit from treatment. RATE and Qini curves evaluate how well a strategy of prioritising treatment using these CATE predictions $\\hat{\\tau}(X_i)$ performs compared to simpler approaches (e.g., uniform treatment based on the overall average effect, ATE). Specifically, RATE estimates the average gain achieved among the group prioritised by the model [@wager2018]. Qini curves dynamically visualise this potential gain: they plot the cumulative benefit realised as we hypothetically treat an increasing proportion of the population, starting with those individuals having the highest predicted effects $\\hat{\\tau}(X_i)$ [@grf2024]."

interpretation_rate_test_no_flip <- "To assess the potential practical value of tailoring treatment based on individual predictions, we examined the Rank-Weighted Average Treatment Effect (RATE) and Qini curves [@grf2024; @wager2018]. These predictions are estimates (denoted $\\hat{\\tau}(X_i)$) of the underlying conditional average treatment effect (CATE, $\\tau(X_i)$), which represents the average treatment effect for individuals with specific baseline covariates $X_i$. RATE and Qini curves evaluate how well a strategy of prioritising treatment using these CATE predictions $\\hat{\\tau}(X_i)$ performs compared to simpler approaches (e.g., uniform treatment based on the overall average effect, ATE). Specifically, RATE estimates the average gain achieved among the group prioritised by the model [@wager2018]. Qini curves dynamically visualise this potential gain: they plot the cumulative benefit realised as we hypothetically treat an increasing proportion of the population, starting with those individuals having the highest predicted effects $\\hat{\\tau}(X_i)$ [@grf2024]."

appendix_causal_grf <- "
### Exposure Definition

The binary exposure variable, $A$, measured at Wave {{exposure_waves}}, is derived from an underlying measure $A'$ based on a pre-defined threshold, `{{binary_threshold}}`. Individuals are classified into two groups:

1.  **Exposure Group ($A=1$)**: Individuals whose measure $A'$ is greater than the threshold ($A' > {{binary_threshold}}$). We refer to this condition as '{{name_exposure_threshold}}'.
2.  **Control Group ($A=0$)**: Individuals whose measure $A'$ is less than or equal to the threshold ($A' \\le {{binary_threshold}}$). We refer to this condition as '{{name_control_threshold}}'.

### Causal Inference

[...]

We compare two hypothetical scenarios:

1.  **{{name_exposure_threshold}}**: We hypothetically assign everyone the exposure level $A = 1$.
2.  **{{name_control_threshold}}**: We hypothetically assign everyone the exposure level $A = 0$.

Our objective is to estimate the **Conditional Average Treatment Effect (CATE)** [...]

$$ \\tau(x) = E[Y_{\text{end of study}}(1) - Y_{\text{end of study}}(0) | X = x] $$

Here:
-   $Y_{\text{end of study}}(1)$ represents the potential outcome if an individual received exposure level $A=1$.
-   $Y_{\text{end of study}}(0)$ represents the potential outcome if they received exposure level $A=0$.


### Assumptions

Our estimation relies on standard causal assumptions for this setting:

1.  **Conditional Exchangeability [...]**:
    $$ \{ Y_{\text{end of study}}(1), Y_{\text{end of study}}(0) \} \\coprod A \\mid X $$

This implies that, within strata defined by $X$, the exposure assignment is effectively random with respect to the outcome. We assume all relevant common causes of the exposure $A$ and the outcome $Y_{\text{end of study}}$ are included in $X$.

2.  **Consistency:** An individual's observed outcome $Y_{\text{end of study}}$ corresponds to their potential outcome under the exposure level they actually received. If an individual received exposure $A=a$, then their observed outcome is $Y_{\text{end of study}} = Y_{\text{end of study}}(a)$.

3.  **Positivity (Overlap)** For any combination of baseline characteristics $x$ present in the population, there is a non-zero probability of receiving either exposure level. Formally:
    $$ 0 < P(A = {{value_exposure}} \mid X = x) < 1 $$
    This ensures that for any group defined by $X$, we can observe individuals under both the {{name_exposure_threshold}} and {{name_control_threshold}} conditions, allowing for comparison.

(Refer to [{{appendix_assumptions}}](#appendix-assumptions) for further details on these assumptions).

The target population for this analysis is {{name_target_population}}."




### POSSIBLE TEXT

# Summary of Treatment Effect Results
# This section summarises the results from both the Qini curve analysis and the Rank-Weighted Average Treatment Effect (RATE) analysis. These techniques help us understand whether personalised treatments are more effective than a one-size-fits-all approach.
#
# Qini Curve Results
# The Qini curve helps us understand whether focusing on the people predicted to benefit the most actually leads to better overall outcomes than treating everyone (or no one). We start with those who are forecast to benefit the most, then gradually include more people. If the Qini curve remains above the line for treating everyone equally, it implies a targeted approach is likely beneficial.
#
# When interpreting the Qini results, we look at whether the Conditional Average Treatment Effect (CATE) approach reliably outperforms the Average Treatment Effect (ATE) at different spending levels. A positive, statistically significant result suggests that targeting treatments based on individual characteristics is better than treating everyone the same way.
#
# RATE Analysis Results
# The Rank-Weighted Average Treatment Effect (RATE) identifies subgroups of individuals with different responses to treatment. This approach allows us to determine whether certain people will benefit more from an intervention than others, which is crucial for effective resource allocation.
#
# A positive RATE estimate with a confidence interval that doesn't include zero indicates reliable treatment effect heterogeneity with positive effects for targeted subgroups. This suggests that personalising treatment using CATE may lead to better outcomes for certain individuals.
#
# Conversely, a negative RATE estimate with a confidence interval entirely below zero serves as an important caution. It suggests that targeting treatment based on predicted effects would lead to worse outcomes than using the average treatment effect. For these outcomes, a personalised approach is not recommended.
#
# When no statistically significant evidence of treatment effect heterogeneity is detected (the confidence interval crosses zero), we cannot reliably conclude that personalisation would be better than a standard approach.


# measures ----------------------------------------------------------------
str(unified_db, max.level = 1)
unified_db$measures$emp_job_sat <- list(
  name = "Job Satisfaction",
  description = "Job satisfaction was measured with a single item.",
  reference = "[@eisenbarth2022aspects]",
  waves = "1-present",
  keywords = c("employment", "mental health"),
  items = list(
    "How satisfied are you with your current job?"
  )
)

unified_db$measures$emp_job_valued <- list(
  name = "Perceive Gratitude from Organisation",
  description = "Perceved organizational gratitude was measured with a single item.",
  reference = "Developed for the NZAVS",
  waves = "10-present",
  keywords = c("employment", "mental health"),
  items = list(
    "How valued do you feel by your current organization?"
  )
)

unified_db$measures$emp_job_valued_binary <- list(
  name = "Perceive Gratitude from Organisation (binary)",
  description = "Perceved organizational gratitude was measured with a single item.",
  reference = "Developed for the NZAVS",
  waves = "10-present",
  keywords = c("employment", "mental health"),
  items = list(
    "How valued do you feel by your current organization?"
  )
)
unified_db$measures$emp_job_sat

boilerplate::boilerplate_save(unified_db, data_path = my_project_path, create_backup = TRUE)




# SHIFT INTERVENTION ------------------------------------------------------

# We defined $\\dd(A)$ as a hypothetical intervention that increased the value of the {{name_exposure_variable}} by 20% (i.e., multiplied each person's value by 1.2).
