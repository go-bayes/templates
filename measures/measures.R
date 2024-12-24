# # # method boilerplate
# # # joseph.bulbulia@gmail.com
# # # aug 24

library(rlang)
library(here)
library(tidyverse)
# pull_path <- fs::path_expand("/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph Bulbulia/00Bulbulia Pubs/DATA/nzavs-current/r-data/nzavs_data_qs")
# dat <- qs::qread(here::here(pull_path))
# library(margot)
library(glue)
library(rlang)
library(here)
#
# detach("package:margot", unload = TRUE)
# devtools::install_github("go-bayes/margot")
devtools::install_github("go-bayes/boilerplate")
library("boilerplate")

# update
# pak::pak(c(
#   "clarify",
#   "cobalt",
#   "DiagrammeR",
#   "doParallel",
#   "fastDummies",
#   "fs",
#   "ggbeeswarm",
#   "ggplot2",
#   "glmnet",
#   "grf",
#   "gt",
#   "gtsummary",
#   "here",
#   "janitor",
#   "kableExtra",
#   "r-lib/lmtp",
#   "go-bayes/margot",  # assuming 'margot' is available on GitHub under r-lib
#   "MatchIt",
#   "MatchThem",
#   "naniar",
#   "parameters",
#   "policytree",
#   "progressr",
#   "ranger",
#   "skimr",
#   "SuperLearner",
#   "tidyverse",
#   "r-lib/WeightIt",
#   "xgboost",
#   "EValue",
#   "data.table",
#   "maq",
#   "purrr",
#   "patchwork",
#   "labelled",
#   "tidyr"
# ))
#
# library(WeightIt)

# # load necessary libraries
# pacman::p_load(
#   clarify,      # sensitivity analysis for causal inference
#   cobalt,       # covariate balance tables and plots
#   DiagrammeR,   # graph and network visualization
#   doParallel,   # parallel processing with foreach
#   fastDummies,  # fast creation of dummy variables
#   fs,           # cross-platform file system operations
#   ggbeeswarm,   # data visualisation
#   ggplot2,      # data visualisation
#   glmnet,       # lasso and elastic-net regularized models
#   grf,          # generalized random forests
#   gt,           # html tables for data frames
#   gtsummary,    # summary tables for regression models
#   here,         # simple and robust file referencing
#   janitor,      # data cleaning and validation
#   kableExtra,   # advanced table formatting
#   lmtp,         # longitudinal targeted maximum likelihood estimation
#   margot,       # functions for casual inference
#   MatchIt,      # matching methods for causal inference
#   MatchThem,    # matching methods for multiply imputed datasets
#   naniar,       # handling and visualization of missing data
#   parameters,   # parameters and performance metrics
#   policytree,   # causal inference with policy trees
#   progressr,    # progress reporting for R
#   ranger,       # fast implementation of random forests
#   skimr,        # summary statistics for data frames
#   SuperLearner, # ensemble learning
#   tidyverse,    # collection of R packages for data science
#   WeightIt,     # weighting methods for covariate balancing
#   xgboost,      # extreme gradient boosting
#   EValue,       # compute Evalues
#   data.table,   # fast data wrangling
#   maq,          # qini curves
#   purrr,        # data wrangling
#   patchwork,     # multiple plots
#   labelled,
#   tidyr,
#   glue,
#   rlang
# )
# # new function
# read data
# pull_path <- fs::path_expand(
#   "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph Bulbulia/00Bulbulia Pubs/DATA/nzavs-current/r-data/nzavs_data_qs"
# )
# dat <- qs::qread(here::here(pull_path))
# names_dat <- colnames(dat)
# sort(names_dat)



# install margot ----------------------------------------------------------


# detach("package:margot", unload = TRUE)
# devtools::install_github("go-bayes/margot")

# load package
# library(margot)

library(boilerplate)
# set vars. # just an example

baseline_vars <- c(
  "age",
  "agreeableness",
  "alcohol_frequency",
  "alcohol_intensity",
  "belong",
  "born_nz_binary",
  "conscientiousness",
  "education_level_coarsen",
  "employed_binary",
  "eth_cat",
  "extraversion",
  "hlth_disability_binary",
  "honesty_humility",
  "kessler_latent_anxiety",
  "kessler_latent_depression",
  "log_hours_children",
  "log_hours_commute",
  "log_hours_exercise",
  "log_hours_housework",
  "log_household_inc",
  "male_binary",
  "neuroticism",
  "not_heterosexual_binary",
  "nz_dep2018",
  "nzsei_13_l",
  "openness",
  "parent_binary",
  "partner_binary",
  "political_conservative",
  "power_no_control_composite",
  "religion_identification_level",
  "rural_gch_2018_l",
  "sample_frame_opt_in_binary",
  "short_form_health",
  "smoker_binary",
  "support"
)


exposure_var <- "political_orientation"


outcome_vars <- c(
  "belong",
  "bodysat",
  "gratitude",
  "hlth_bmi",
  "hlth_fatigue",
  "hlth_sleep_hours",
  "kessler_latent_anxiety",
  "kessler_latent_depression",
  "lifesat",
  "log_hours_exercise",
  "meaning_purpose",
  "meaning_sense",
  "neighbourhood_community",
  "perfectionism",
  "pwb_standard_living",
  "pwb_your_future_security",
  "pwb_your_health",
  "pwb_your_relationships",
  "rumination",
  "self_control_have_lots",
  "self_control_wish_more_reversed",
  "self_esteem",
  "sexual_satisfaction",
  "short_form_health",
  "smoker_binary",
  "support",
  "vengeful_rumin"
)


here::here()
measures_path <- here::here("measures", "data")
# Example

# baseline variables
baseline_vars <- c("age", "male_binary", "parent_binary")

# exposure variable (intervention)
exposure_var <- "political_conservative"

# outcomes, perhaps defined by domains
outcomes_health <- c("smoker_binary", "hlth_bmi", "log_hours_exercise")
outcomes_psychological <- c("hlth_fatigue", "kessler_latent_anxiety")
outcomes_social <- c("belong", "neighbourhood_community")

# options 1
outcome_vars <- c(outcomes_health, outcomes_psychological, outcomes_social)

# all_vars <- c(baseline_vars, exposure_var, outcome_vars)


# option
all_outcomes <- list(
  health = outcomes_health,
  psychological = outcomes_psychological,
  social = outcomes_social
)

# read bibliography data = object created by `margot_create_bibliography()` function (in this package)
measure_data <- readRDS(here::here(measures_path, "measure_data.rds"))
str(measure_data)

appendix_text_version_1 <- boilerplate::boilerplate_report_measures(
  baseline_vars = baseline_vars,
  exposure_var = exposure_var,
  outcome_vars = outcome_vars,
  measure_data = measure_data
)

# view
cat(appendix_text_version_1)

boilerplate_report_measures


appendix_text_version_2 <- boilerplate_report_variables(
  exposure_var = exposure_var,
  outcome_vars = all_outcomes,
  appendices_measures = "Appendix C",
  measure_data = measure_data
)
cat(appendix_text_version_2)





appendix_text_all_variables <- boilerplate_report_measures(
  all_vars = c("env_sat_nz_environment", outcome_vars),
  measure_data = measure_data
)


cat(appendix_text)


# input bibliography ------------------------------------------------------
# set path



boilerplate_manage_measures(measures_path = measures_path)



222
test_a <- readRDS(here::here("boilerplate", "data", "test_a.rds"))
test_b <- readRDS(here::here("boilerplate", "data", "test_b.rds"))

str(test_a)
str(test_b)


merged_db <- margot_merge_databases(test_a, test_b)

library(cli)



# saveRDS(measure_data, here::here(here::here(
#   "boilerplate", 'data', 'measure_data.rds'
# )))
# [95] "emp_job_valued"                                              "emp_work_life_balance"
# [97] "employed"                                                    "env_1080poison"
# [99] "env_carbon_regs"                                             "env_climate_chg_cause"
# [101] "env_climate_chg_concern"                                     "env_climate_chg_real"
# [103] "env_efficacy_action_belief"                                  "env_efficacy_action_feeling"
# [105] "env_environment_nature"                                      "env_environment_values"
# [107] "env_motorway_spend"                                          "env_native_species"
# [109] "env_pub_trans_subs"                                          "env_routine_made"
# [111] "env_routine_willing"                                         "env_sac_made"
# [113] "env_sac_norms"                                               "env_sac_willing"
# [115] "env_sat_nz_environment"                                      "env_sat_waterways"

# # run manager
# margot::manager_boilerplate_measures(measures_path = measures_path)
#


# test --------------------------------------------------------------------
yola

#
# # test
# measures_path = here::here("boilerplate", 'data')
#
# boilerplate_manage_entries(measures_path = measures_path)
# boilerplate_manage_entries()


# boilerplate -------------------------------------------------------------











2 # test --------------------------------------------------------------------

#
#
# boilerplate_methods <- function(exposure_var, outcome_vars, n_total, baseline_wave, exposure_wave, outcome_wave, baseline_missing_data_proportion, sections_to_include = 'all', appendices_measures = NULL, domains = NULL, measure_data_path = NULL, ...) {
#   all_sections <- c(
#     "sample",
#     "variables",
#     "causal_interventions",
#     "identification_assumptions",
#     "target_population",
#     "eligibility_criteria",
#     "confounding_control",
#     "missing_data",
#     "statistical_estimator",
#     "additional_sections"
#   )
#
#   if (identical(sections_to_include, 'list')) {
#     return(all_sections)
#   }
#
#   cat("Starting boilerplate_methods function\n")
#
#   # initialise an empty list to store all sections
#   methods_sections <- list()
#
#   # capture all additional arguments
#   extra_args <- list(...)
#
#   # define the safe_get function inside boilerplate_methods
#   safe_get <- function(name, default = NULL) {
#     if (name %in% names(extra_args)) {
#       cat(paste("Found", name, "in extra_args\n"))
#       return(extra_args[[name]])
#     } else {
#       cat(paste("Using default value for", name, "\n"))
#       return(default)
#     }
#   }
#
#
#   # Load measure data
#   measure_data <- NULL
#   if (!is.null(measure_data_path)) {
#     if (file.exists(measure_data_path)) {
#       measure_data <- readRDS(measure_data_path)
#     } else {
#       warning(paste("Measure data file not found at:", measure_data_path))
#     }
#   } else {
#     warning("Measure data path not provided. Detailed measure information will not be available.")
#   }
#
#   # define a helper function for error-tolerant execution
#   safe_execute <- function(func_name, args) {
#     cat(paste("Executing", func_name, "\n"))
#     cat("Arguments passed to", func_name, ":\n")
#     print(args)
#     tryCatch({
#       if (exists(func_name, mode = "function")) {
#         func <- get(func_name, mode = "function")
#         # Only pass arguments that the function can accept
#         func_args <- formals(func)
#         valid_args <- args[names(args) %in% names(func_args)]
#         cat("Valid arguments for", func_name, ":\n")
#         print(valid_args)
#         result <- do.call(func, valid_args)
#         cat(paste("Finished", func_name, "\n"))
#         cat("Result of", func_name, ":\n")
#         print(result)
#         return(result)
#       } else {
#         warning(paste("Function", func_name, "not found. Skipping this section."))
#         return(NULL)
#       }
#     }, error = function(e) {
#       warning(paste("Error in", func_name, ":", e$message))
#       return(NULL)
#     })
#   }
#
#   # get eligibility criteria
#   inclusion_criteria <- safe_get("inclusion_criteria", list("No inclusion criteria specified"))
#   exclusion_criteria <- safe_get("exclusion_criteria", list("No exclusion criteria specified"))
#   n_participants <- safe_get("n_participants", "UNDEFINED")
#
#   # get the statistical estimator
#   statistical_estimator <- safe_get("statistical_estimator", list(estimators = "lmtp"))
#   if (is.list(statistical_estimator) && "estimators" %in% names(statistical_estimator)) {
#     statistical_estimator <- statistical_estimator$estimators  # Use all provided estimators
#   }
#
#   sections <- if(identical(sections_to_include, 'all')) all_sections else sections_to_include
#
#   # call sub-functions for each section
#   for (section in sections) {
#     cat(paste("\nProcessing section:", section, "\n"))
#     section_name <- section
#     func_name <- paste0("boilerplate_methods_", section)
#     args <- safe_get(section_name, list())
#
#     # add exposure_var, outcome_vars, n_total, and wave information to args if they're needed
#     args$exposure_var <- exposure_var
#     args$outcome_vars <- outcome_vars  # Add this line to pass outcome_vars
#     args$n_total <- n_total
#     args$baseline_wave <- baseline_wave
#     args$exposure_wave <- exposure_wave
#     args$outcome_wave <- outcome_wave
#     args$baseline_missing_data_proportion <- baseline_missing_data_proportion  # Add this line
#
#     if (section_name == "variables") {
#       args$appendices_measures <- appendices_measures
#       args$domains <- domains
#       args$measure_data <- if (!is.null(measure_data_path) && file.exists(measure_data_path)) readRDS(measure_data_path) else NULL
#     }
#
#     # Special handling for causal_interventions
#     if (section_name == "causal_interventions") {
#       args$causal_interventions <- safe_get("causal_interventions", list(interventions = c("Increase exposure_var", "Do not change exposure_var")))
#       args$contrasts <- safe_get("contrasts", "pairwise")
#       args$null_intervention <- safe_get("null_intervention", NULL)
#     }
#
#     # special handling for eligibility_criteria
#     if (section_name == "eligibility_criteria") {
#       args$inclusion_criteria <- safe_get("inclusion_criteria", "No inclusion criteria specified")
#       args$exclusion_criteria <- safe_get("exclusion_criteria", "No exclusion criteria specified")
#       args$n_participants <- safe_get("n_participants", "UNDEFINED")
#     }
#
#     # special handling for target_population
#     if (section_name == "target_population") {
#       args$statistical_estimator <- if (is.list(statistical_estimator)) statistical_estimator$estimators[1] else statistical_estimator[1]
#       args$baseline_wave <- baseline_wave
#     }
#
#     if (section_name == "missing_data") {
#       args$estimators <- statistical_estimator
#       args$baseline_wave <- baseline_wave
#       args$exposure_wave <- exposure_wave
#       args$outcome_wave <- outcome_wave
#       args$baseline_missing_data_proportion <- baseline_missing_data_proportion
#     }
#
#     # special handling for statistical_estimator
#     if (section_name == "statistical_estimator") {
#       args$estimators <- statistical_estimator
#     }
#
#     # special handling for additional_sections
#     if (section_name == "additional_sections") {
#       additional_sections_args <- safe_get("additional_sections", list())
#       args <- list(
#         sensitivity_analysis = additional_sections_args$sensitivity_analysis,
#         scope_interventions = additional_sections_args$scope_interventions,
#         evidence_change = additional_sections_args$evidence_change
#       )
#     }
#
#     # unlist nested lists if necessary
#     args <- lapply(args, function(arg) if (is.list(arg) && length(arg) == 1) unlist(arg) else arg)
#
#     result <- safe_execute(func_name, args)
#     if (!is.null(result)) {
#       methods_sections[[section_name]] <- result
#       cat(paste("Added result for", section_name, "to methods_sections\n"))
#     } else {
#       cat(paste("No result added for", section_name, "\n"))
#     }
#   }
#   # combine all sections into a single markdown string
#   cat("\nCombining all sections\n")
#   cat("Contents of methods_sections:\n")
#   print(methods_sections)
#   markdown_output <- paste(unlist(methods_sections), collapse = "\n\n")
#
#   cat("Finished boilerplate_methods function\n")
#   cat("Final markdown_output:\n")
#   print(markdown_output)
#   return(markdown_output)
# }
#
#
# boilerplate_methods_variables <- function(exposure_var, outcome_vars, domains = NULL, appendices_measures = NULL, measure_data = NULL, ...) {
#   if (is.null(domains)) {
#     domains <- names(outcome_vars)
#   }
#
#   # Custom function to convert to title case
#   to_title_case <- function(x) {
#     x <- gsub("_", " ", x)
#     paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
#   }
#
#   outcome_vars_list <- ""
#   for (domain in domains) {
#     if (domain %in% names(outcome_vars)) {
#       domain_vars <- outcome_vars[[domain]]
#       domain_list <- ""
#       for (var in domain_vars) {
#         domain_list <- paste0(domain_list, "  - ", var)
#         if (!is.null(measure_data) && var %in% names(measure_data)) {
#           measure_info <- safe_get_measure_info(measure_data, var)
#           if (!is.null(measure_info$item_text)) {
#             domain_list <- paste0(domain_list, "\n    Item text: \"", measure_info$item_text, "\"")
#           }
#           if (!is.null(measure_info$reference)) {
#             domain_list <- paste0(domain_list, "\n    Reference: ", measure_info$reference)
#           }
#         }
#         domain_list <- paste0(domain_list, "\n")
#       }
#       domain_name <- to_title_case(domain)
#       outcome_vars_list <- paste0(outcome_vars_list,
#                                   "\n\n",
#                                   "##### ", domain_name, "\n",
#                                   domain_list)
#     } else {
#       warning(paste("Domain", domain, "not found in outcome_vars."))
#     }
#   }
#
#   appendix_text <- if (!is.null(appendices_measures)) {
#     paste0("\n\nDetailed descriptions of how these variables were measured and operationalized can be found in **Appendix ", appendices_measures, "**.")
#   } else {
#     ""
#   }
#
#   markdown_text <- glue::glue("
# ### Variables
# #### Exposure Variable
# The primary exposure variable in this study is '{exposure_var}'.
# #### Outcome Variables
# The outcome variables examined in this study are:
# {outcome_vars_list}
# {appendix_text}
#   ")
#   return(markdown_text)
# }
#
# # Helper function to safely access measure data
# safe_get_measure_info <- function(measure_data, var_name) {
#   if (is.null(measure_data) || !var_name %in% names(measure_data)) {
#     return(list(item_text = NULL, reference = NULL))
#   }
#   measure_info <- measure_data[[var_name]]
#   list(
#     item_text = if (!is.null(measure_info$item_text)) measure_info$item_text else NULL,
#     reference = if (!is.null(measure_info$reference)) measure_info$reference else NULL
#   )
# }
#
#
# # example
# outcomes_health <- c("smoker_binary", "hlth_bmi", "log_hours_exercise")
# outcomes_psychological <- c("hlth_fatigue", "kessler_latent_anxiety")
# outcomes_social <- c("belong", "neighbourhood_community")
#
#
#
# # Combine all outcomes into a single list
# all_outcomes <- list(
#   health = outcomes_health,
#   psychological = outcomes_psychological,
#   social = outcomes_social
# )
#
# methods_text_all <- boilerplate_methods(
#   exposure_var = "political_conservative",
#   outcome_vars = all_outcomes,
#   n_total = 47000,
#   baseline_wave = "NZAVS time 10, years 2018-2019",
#   exposure_wave = "NZAVS time 11, years 2019-2020",
#   outcome_wave = "NZAVS time 12, years 2020-2021",
#   baseline_missing_data_proportion = 0.15,
#   appendices_measures = "C",
#   measure_data_path = here::here("boilerplate", "data", "measure_data.rds"),
#   causal_interventions = list(interventions = c("Increase exposure_var", "Do not change exposure_var")),
#   contrasts = "null",
#   null_intervention = "Do not change exposure_var",
#   sample = list(appendices = "A-C"),
#   statistical_estimator = list(estimators = c("lmtp", "grf")),
#   inclusion_criteria = c(
#     "Enrolled in the 2018 wave of the New Zealand Attitudes and Values Study (NZAVS time 10).",
#     "Missing covariate data at baseline was permitted, and the data was subjected to imputation methods to reduce bias."
#   ),
#   exclusion_criteria = c(
#     "Did not answer the religious service attendance question at NZAVS time 10 and time 11."
#   ),
#   n_participants = 32451,
#   confounding_control = list(appendix_ref = "B", protocol_url = "https://osf.io/ce4t9/"),
#   additional_sections = list(
#     sensitivity_analysis = list(
#       description = "We use the E-value method to assess sensitivity to unmeasured confounding."
#     ),
#     scope_interventions = list(figure_ref = "@fig-custom-hist"),
#     evidence_change = list(table_ref = "@tbl-custom-transition")
#   )
# )
#
# cat(methods_text_all)

# test_batch <- batch_edit_measures(measure_data, field = "reference", old_value = "nzavs2009",  new_value = "refer to @nzavs2021")


#
# read_version <- readRDS(here::here(measures_path, "measure_data.rds"))
# # check
# read_version
# appendix_text <- boilerplate_measures(
#   baseline_vars,
#   exposure_var = exposure_var,
#   outcome_vars = exposure_var,
#   measure_data = read_version,
#   custom_titles = NULL,
#   print_keywords = FALSE
# )
#
# cat(appendix_text)
#

# manually create datbase if needed


# make initial data (for ease, in the future, use manger_boilerplate_measures())
# measure_data <- list(
#   agreeableness = list(
#     description = "Mini-IPIP6 Agreeableness dimension: (i) I sympathize with others' feelings. (ii) I am not interested in other people's problems. (r) (iii) I feel others' emotions. (iv) I am not really interested in others. (r)",
#     waves = "1-current",
#     reference = "sibley2011"
#   ),
#   age = list(
#     description = "We asked participants' ages in an open-ended question (\"What is your age?\" or \"What is your date of birth\").",
#     waves = "1-current",
#     reference = "nzavs2009"
#   ),
#   belong = list(
#     description = "We assessed felt belongingness with three items adapted from the Sense of Belonging Instrument (Hagerty & Patusky, 1995): (1) \"Know that people in my life accept and value me\"; (2) \"Feel like an outsider\"; (3) \"Know that people around me share my attitudes and beliefs\". Participants responded on a scale from 1 (Very Inaccurate) to 7 (Very Accurate). The second item was reversely coded.",
#     waves = "",
#     reference = "hagerty1995"
#   ),
#   born_nz_binary = list(
#     description = "We asked participants, \"Which country were you born in?\" or \"Where were you born? (please be specific, e.g., which town/city?)\" (waves: 6-15).",
#     waves = "1-2,4-current",
#     reference = "nzavs2009"
#   ),
#   charitable_donations = list(
#     description = "Using one item from Hoverd and Sibley (2010), we asked participants, \"How much money have you donated to charity in the last year?\". To stabilise this indicator, we took the natural log of the response + 1.",
#     waves = "",
#     reference = "hoverd_religious_2010"
#   ),
#   children_num = list(
#     description = "We measured the number of children using one item from Bulbulia et al. (2015). We asked participants, \"How many children have you given birth to, fathered, or adopted?\" or \"How many children have you given birth to, fathered, and/or parented?\" (waves: 12-15).",
#     waves = "1-3, 4-current",
#     reference = "Bulbulia_2015"
#   ),
#   conscientiousness = list(
#     description = "Mini-IPIP6 Conscientiousness dimension: (i) I get chores done right away. (ii) I like order. (iii) I make a mess of things. (r) (iv) I often forget to put things back in their proper place. (r)",
#     waves = "1-current",
#     reference = "sibley2011"
#   ),
#   education_level_coarsen = list(
#     description = "We asked participants, \"What is your highest level of qualification?\". We coded participans highest finished degree according to the New Zealand Qualifications Authority. Ordinal-Rank 0-10 NZREG codes (with overseas school qualifications coded as Level 3, and all other ancillary categories coded as missing)",
#     waves = "1, 4-current",
#     reference = "nzavs2009"
#   ),
#   emp_job_secure = list(
#     description = "Participants indicated their feeling of job security by answering \"How secure do you feel in your current job?\" on a scale from 1 (not secure) to 7 (very secure).",
#     waves = "1-3,4-7,9-current",
#     reference = "nzavs2009"
#   ),
#   employed_binary = list(
#     description = "We asked participants, \"Are you currently employed? (This includes self-employed or casual work)\". * note: This question disappeared in the updated NZAVS Technical documents (Data Dictionary).",
#     waves = "1-3, 4-11",
#     reference = "nzavs2009"
#   ),
#   eth_cat = list(
#     description = "Based on the New Zealand Census, we asked participants, \"Which ethnic group(s) do you belong to?\". The responses were: (1) New Zealand European; (2) Māori; (3) Samoan; (4) Cook Island Māori; (5) Tongan; (6) Niuean; (7) Chinese; (8) Indian; (9) Other such as DUTCH, JAPANESE, TOKELAUAN. Please state:. We coded their answers into four groups: Maori, Pacific, Asian, and Euro (except for Time 3, which used an open-ended measure).",
#     waves = "1-current",
#     reference = "nzavs2009"
#   ),
#   euro = list(
#     description = "Participants were asked \"Which ethnic group do you belong to (NZ census question)?\" or \"Which ethnic group(s) do you belong to? (Open-ended)\" (wave: 3). Europeans were coded as 1, whereas other ethnicities were coded as 0.",
#     waves = "1-current",
#     reference = "nzavs2009"
#   ),
#   extraversion = list(
#     description = "Mini-IPIP6 Extraversion dimension: (i) I am the life of the party. (ii) I don't talk a lot. (r) (iii) I keep in the background. (r) (iv) I talk to a lot of different people at parties.",
#     waves = "1-current",
#     reference = "sibley2011"
#   ),
#   has_siblings = list(
#     description = "\"Do you have siblings?\"",
#     waves = "",
#     reference = "stronge2019onlychild"
#   ),
#   hlth_disability_binary = list(
#     description = "We assessed disability with a one-item indicator adapted from Verbrugge (1997). It asks, \"Do you have a health condition or disability that limits you and that has lasted for 6+ months?\" (1 = Yes, 0 = No).",
#     waves = "",
#     reference = "verbrugge1997"
#   ),
#   hlth_fatigue = list(
#     description = "We assessed subjective fatigue by asking participants, \"During the last 30 days, how often did ... you feel exhausted?\" Responses were collected on an ordinal scale (0 = None of The Time, 1 = A little of The Time, 2 = Some of The Time, 3 = Most of The Time, 4 = All of The Time).",
#     waves = "",
#     reference = "sibley2020"
#   ),
#   honesty_humility = list(
#     description = "Mini-IPIP6 Honesty-Humility dimension: (i) I feel entitled to more of everything. (r) (ii) I deserve more things in life. (r) (iii) I would like to be seen driving around in a very expensive car. (r) (iv) I would get a lot of pleasure from owning expensive luxury goods. (r)",
#     waves = "1-current",
#     reference = "sibley2011"
#   ),
#   income = list(
#     description = "Participants were asked \"Please estimate your total household income (before tax) for the year XXXX\". To stabilise this indicator, we first took the natural log of the response + 1, and then centred and standardised the log-transformed indicator.",
#     waves = "1-3, 4-current",
#     reference = "nzavs2009"
#   ),
#   log_hours_children = list(
#     description = "We measured hours of childcare using one item from Sibley et al. (2011): 'Hours spent … looking after children.' To stabilise this indicator, we took the natural log of the response + 1.",
#     waves = "",
#     reference = "sibley2011"
#   ),
#   log_hours_exercise = list(
#     description = "We measured hours of exercising using one item from Sibley et al. (2011): \"Hours spent … exercising/physical activity\". To stabilise this indicator, we took the natural log of the response + 1.",
#     waves = "",
#     reference = "sibley2011"
#   ),
#   log_hours_housework = list(
#     description = "We measured hours of housework using one item from Sibley et al. (2011): \"Hours spent … housework/cooking\". To stabilise this indicator, we took the natural log of the response + 1.",
#     waves = "",
#     reference = "sibley2011"
#   ),
#   log_hours_work = list(
#     description = "We measured hours of work using one item from Sibley et al. (2011):\"Hours spent … working in paid employment.\" To stabilise this indicator, we took the natural log of the response + 1.",
#     waves = "",
#     reference = "sibley2011"
#   ),
#   male_binary = list(
#     description = "We asked participants' gender in an open-ended question: \"what is your gender?\" or \"Are you male or female?\" (waves: 1-5). Female was coded as 0, Male was coded as 1, and gender diverse coded as 3 (Fraser et al., 2020). (or 0.5 = neither female nor male). Here, we coded all those who responded as Male as 1, and those who did not as 0.",
#     waves = "1-current",
#     reference = "fraser_coding_2020"
#   ),
#   modesty = list(
#     description = "Participants indicated the extent to which they agree with the following four statements from Campbell et al. (2004), and Sibley et al. (2011) (1 = Strongly Disagree to 7 = Strongly Agree): (i) I want people to know that I am an important person of high status, (Waves: 1, 10-14); (ii) I am an ordinary person who is no better than others. (all waves); (iii) I wouldn't want people to treat me as though I were superior to them. (all waves); (iv) I think that I am entitled to more respect than the average person is. (all waves)",
#     waves = "10-14",
#     reference = "campbell2004"
#   ),
#   neuroticism = list(
#     description = "Mini-IPIP6 Neuroticism dimension: (i) I have frequent mood swings. (ii) I am relaxed most of the time. (r) (iii) I get upset easily. (iv) I seldom feel blue. (r)",
#     waves = "1-current",
#     reference = "sibley2011"
#   ),
#   nz_dep2018 = list(
#     description = "We used the NZ Deprivation Index to assign each participant a score based on where they live (Atkinson et al., 2019). This score combines data such as income, home ownership, employment, qualifications, family structure, housing, and access to transport and communication for an area into one deprivation score.",
#     waves = "1-current",
#     reference = "atkinson2019"
#   ),
#   nzsei_13_l = list(
#     description = "We assessed occupational prestige and status using the New Zealand Socio-economic Index 13 (NZSEI-13) (Fahy et al., 2017). This index uses the income, age, and education of a reference group, in this case, the 2013 New Zealand census, to calculate a score for each occupational group. Scores range from 10 (Lowest) to 90 (Highest). This list of index scores for occupational groups was used to assign each participant a NZSEI-13 score based on their occupation.",
#     waves = "8-current",
#     reference = "fahy2017"
#   ),
#   openness = list(
#     description = "Mini-IPIP6 Openness to Experience dimension: (i) I have a vivid imagination. (ii) I have difficulty understanding abstract ideas. (r) (iii) I do not have a good imagination. (r) (iv) I am not interested in abstract ideas. (r)",
#     waves = "1-current",
#     reference = "sibley2011"
#   ),
#   parent_binary = list(
#     description = "We asked participants, \"If you are a parent, what is the birth date of your eldest child?\" or \"If you are a parent, in which year was your eldest child born?\" (waves: 10-current). Parents were coded as 1, while the others were coded as 0.",
#     waves = "5-current",
#     reference = "nzavs2009"
#   ),
#   partner = list(
#     description = "\"What is your relationship status?\" (e.g., single, married, de-facto, civil union, widowed, living together, etc.)",
#     waves = "",
#     reference = "nzavs2009"
#   ),
#   political_conservative = list(
#     description = "We measured participants' political conservative orientation using a single item adapted from Jost (2006): \"Please rate how politically liberal versus conservative you see yourself as being.\" (1 = Extremely Liberal to 7 = Extremely Conservative)",
#     waves = "",
#     reference = "jost_end_2006-1"
#   ),
#   pol_wing = list(
#     description = "We measured participants' political right-wing orientation using a single item adapted from Jost (2006): \"Please rate how politically left-wing versus right-wing you see yourself as being..\" (1 = Extremely left-wing to 7 = Extremely right-wing)",
#     waves = "",
#     reference = "jost_end_2006-1"
#   ),
#   religion_religious = list(
#     description = "Participants were asked to indicate their religion identification (\"Do you identify with a religion and/or spiritual group?\") on a binary response (1 = Yes, 0 = No). We then asked, \"What religion or spiritual group?\" These questions are used in the New Zealand Census",
#     waves = "",
#     reference = "nzavs2009"
#   ),
#   sample_frame_opt_in_binary = list(
#     description = "The New Zealand Attitudes and Values Study allows opt-ins to the study. Because the opt-in population may differ from those sampled randomly from the New Zealand electoral roll; although the opt-in rate is low, we include an indicator (yes/no) for this variable.",
#     waves = "1-current",
#     reference = "nzavs2009"
#   ),
#   support = list(
#     description = "Participants' perceived social support was measured using three items from Cutrona and Russell (1987) and Williams et al. (2000): (1) \"There are people I can depend on to help me if I really need it\"; (2) \"There is no one I can turn to for guidance in times of stress\"; (3) \"I know there are people I can turn to when I need help.\" Participants indicated the extent to which they agreed with those items (1 = Strongly Disagree to 7 = Strongly Agree). The second item was negatively worded, so we reversely recorded the responses to this item.",
#     waves = "",
#     reference = "cutrona1987"
#   ),
#   urban_binary = list(
#     description = "We coded whether they are living in an urban or rural area (1 = Urban, 0 = Rural) based on the addresses provided.",
#     waves = "1-current",
#     reference = "nzavs2009"
#   ),
#   volunteers = list(
#     description = "We measured hours of volunteering using one item from Sibley et al. (2011): \"Hours spent … voluntary/charitable work.\" To stabilise this indicator, we took the natural log of the response + 1.",
#     waves = "",
#     reference = "sibley2011"
#   )
# )





# old --------------------------------------------------------------------


# #### Age (waves: 1-15) ; age

# We asked participants' ages in an open-ended question ("What is your age?" or "What is your date of birth").


# #### Charitable Donations  ; charitable_donations

# Using one item from @hoverd_religious_2010, we asked participants, "How much money have you donated to charity in the last year?". To stabilise this indicator, we took the natural log of the response + 1.


# #### Children Number (waves: 1-3, 4-15);  children_num

# We measured the number of children using one item from @Bulbulia_2015. We asked participants, "How many children have you given birth to, fathered, or adopted. How many children have you given birth to, fathered, or adopted?" or "How many children have you given birth to, fathered, or adopted. How many children have you given birth to, fathered, and/or parented?" (waves: 12-15).


# #### Disability;  hlth_disability_binary

# We assessed disability with a one-item indicator adapted from @verbrugge1997. It asks, "Do you have a health condition or disability that limits you and that has lasted for 6+ months?" (1 = Yes, 0 = No).


# #### Education Attainment (waves: 1, 4-15) ;  education_level_coarsen

# We asked participants, "What is your highest level of qualification?". We coded participans highest finished degree according to the New Zealand Qualifications Authority. Ordinal-Rank 0-10 NZREG codes (with overseas school qualifications coded as Level 3, and all other ancillary categories coded as missing) See:https://www.nzqa.govt.nz/assets/Studying-in-NZ/New-Zealand-Qualification-Framework/requirements-nzqf.pdf

# #### Employment (waves: 1-3, 4-11); employed_binary

# We asked participants, "Are you currently employed? (This includes self-employed or casual work)". \* note: This question disappeared in the updated NZAVS Technical documents (Data Dictionary).

# #### European (all waves);  euro

# Participants were asked "Which ethnic group do you belong to (NZ census question)?" or "Which ethnic group(s) do you belong to? (Open-ended)" (wave: 3). Europeans were coded as 1, whereas other ethnicities were coded as 0.

# #### Ethnicity (all waves); eth_cat

# Based on the New Zealand Census, we asked participants, "Which ethnic group(s) do you belong to?". The responses were: (1) New Zealand European; (2) Māori; (3) Samoan; (4) Cook Island Māori; (5) Tongan; (6) Niuean; (7) Chinese; (8) Indian; (9) Other such as DUTCH, JAPANESE, TOKELAUAN. Please state:. We coded their answers into four groups: Maori, Pacific, Asian, and Euro (except for Time 3, which used an open-ended measure).

# #### Fatigue; hlth_fatigue

# We assessed subjective fatigue by asking participants, "During the last 30 days, how often did ... you feel exhausted?" Responses were collected on an ordinal scale (0 = None of The Time, 1 = A little of The Time, 2 = Some of The Time, 3 = Most of The Time, 4 = All of The Time).  [@sibley2020]

# #### Felt Belongingness ; belong

# We assessed felt belongingness with three items adapted from the Sense of Belonging Instrument [@hagerty1995]: (1) "Know that people in my life accept and value me"; (2) "Feel like an outsider"; (3) "Know that people around me share my attitudes and beliefs". Participants responded on a scale from 1 (Very Inaccurate) to 7 (Very Accurate). The second item was reversely coded.

# #### Honesty-Humility-Modesty Facet (waves: 10-14); modesty

# Participants indicated the extent to which they agree with the following four statements from @campbell2004 , and @sibley2011 (1 = Strongly Disagree to 7 = Strongly Agree)


# i.  I want people to know that I am an important person of high status, (Waves: 1, 10-14)
# ii. I am an ordinary person who is no better than others.  (all waves)
# iii. I wouldn't want people to treat me as though I were superior to them.  (all waves)
# iv. I think that I am entitled to more respect than the average person is.   (all waves)


# #### Has Siblings ; has_siblings

# "Do you have siblings?" [@stronge2019onlychild]


# #### Hours of Childcare; log_hours_children

# We measured hours of exercising using one item from @sibley2011: 'Hours spent … looking after children."

# To stabilise this indicator, we took the natural log of the response + 1.


# #### Hours of Housework; log_hours_housework

# We measured hours of housework using one item from @sibley2011: "Hours spent … housework/cooking"

# To stabilise this indicator, we took the natural log of the response + 1.

# #### Hours of Exercise log_hours_exercise

# We measured hours of exercising using one item from @sibley2011: "Hours spent … exercising/physical activity"

# To stabilise this indicator, we took the natural log of the response + 1.

# #### Hours Volunteering; volunteers

# We measured hours of volunteering using one item from @sibley2011: "Hours spent … voluntary/charitable work."

# To stabilise this indicator, we took the natural log of the response + 1.


# #### Hours of Work log_hours_work

# We measured hours of work using one item from @sibley2011:"Hours spent … working in paid employment."

# To stabilise this indicator, we took the natural log of the response + 1.

# #### Income (waves: 1-3, 4-15); income

# Participants were asked "Please estimate your total household income (before tax) for the year XXXX". To stabilise this indicator, we first took the natural log of the response + 1, and then centred and standardised the log-transformed indicator.



# #### Job Security (waves: 1-3,4-7,9-15);  emp_job_secure

# Participants indicated their feeling of job security by answering "How secure do you feel in your current job?" on a scale from 1 (not secure) to 7 (very secure).


# #### Living in an Urban Area (all waves) urban_binary

# We coded whether they are living in an urban or rural area (1 = Urban, 0 = Rural) based on the addresses provided.

# We coded whether they were living in an urban or rural area (1 = Urban, 0 = Rural) based on the addresses provided.


# #### Male gender (waves: 1-15)  ; male_binary

# We asked participants' gender in an open-ended question: "what is your gender?" or "Are you male or female?" (waves: 1-5). Female was coded as 0, Male was coded as 1, and gender diverse coded as 3 [@fraser_coding_2020]. (or 0.5 = neither female nor male)

# Here, we coded all those who responded as Male as 1, and those who did not as 0.


# #### Mini-IPIP 6 (all waves)

# We measured participants' personalities with the Mini International Personality Item Pool 6 (Mini-IPIP6) [@sibley2011], which consists of six dimensions and each dimension is measured with four items:

# 1.  agreeableness  ; agreeableness

#     i.  I sympathize with others' feelings.
#     ii. I am not interested in other people's problems. (r) ;
#     iii. I feel others' emotions.
#     iv. I am not really interested in others. (r)

# 2.  conscientiousness  ; conscientiousness

#     i.  I get chores done right away.
#     ii. I like order.
#     iii. I make a mess of things. (r)
#     iv. I often forget to put things back in their proper place. (r)

# 3.  extraversion ; extraversion

#     i.  I am the life of the party.
#     ii. I don't talk a lot. (r)
#     iii. I keep in the background. (r)
#     iv. I talk to a lot of different people at parties.

# 4.  honesty-humility ; honesty_humility

#     i.  I feel entitled to more of everything. (r)
#     ii. I deserve more things in life. (r)
#     iii. I would like to be seen driving around in a very expensive car. (r)
#     iv. I would get a lot of pleasure from owning expensive luxury goods. (r)

# 5.  neuroticism ;   neuroticism

#     i.  I have frequent mood swings.
#     ii. I am relaxed most of the time. (r)
#     iii. I get upset easily.
#     iv. I seldom feel blue. (r)

# 6.  openness to experience; openness

#     i.  I have a vivid imagination.
#     ii. I have difficulty understanding abstract ideas. (r)
#     iii. I do not have a good imagination. (r)
#     iv. I am not interested in abstract ideas. (r)

# Each dimension was assessed with four items and participants rated the accuracy of each item as it applies to them from 1 (Very Inaccurate) to 7 (Very Accurate). Items marked with (r) are reverse coded.


# #### NZ-Born (waves: 1-2,4-15);  born_nz_binary

# We asked participants, "Which country were you born in?" or "Where were you born? (please be specific, e.g., which town/city?)" (waves: 6-15).


# #### NZ Deprivation Index (waves: 1-15) ; nz_dep2018

# We used the NZ Deprivation Index to assign each participant a score based on where they live [@atkinson2019]. This score combines data such as income, home ownership, employment, qualifications, family structure, housing, and access to transport and communication for an area into one deprivation score.

# #### Opt-in (all waves) ;  sample_frame_opt_in_binary

# The New Zealand Attitudes and Values Study allows opt-ins to the study. Because the opt-in population may differ from those sampled randomly from the New Zealand electoral roll; although the opt-in rate is low, we include an indicator (yes/no) for this variable.

# #### NZSEI-13 (waves: 8-15)  ; nzsei_13_l

# We assessed occupational prestige and status using the New Zealand Socio-economic Index 13 (NZSEI-13) [@fahy2017]. This index uses the income, age, and education of a reference group, in this case, the 2013 New Zealand census, to calculate a score for each occupational group. Scores range from 10 (Lowest) to 90 (Highest). This list of index scores for occupational groups was used to assign each participant a NZSEI-13 score based on their occupation.

# We asked participants, "If you are a parent, what is the birth date of your eldest child?".

# #### Parent (waves: 5-15) ; parent_binary

# We asked participants, "If you are a parent, what is the birth date of your eldest child?" or "If you are a parent, in which year was your eldest child born?" (waves: 10-15). Parents were coded as 1, while the others were coded as 0. -->


# #### Number of Children (waves: 1-3, 4-15);  children_num

# <We measured the number of children using one item from @Bulbulia_2015. We asked participants, "How many children have you given birth to, fathered, or adopted. How many children have you given birth to, fathered, or adopted?" or ""How many children have you given birth to, fathered, or adopted. How many children have you given birth to, fathered, and/or parented?" (waves: 12-15).


# #### Politically Conservative ; political_conservative

# We measured participants' political conservative orientation using a single item adapted from @jost_end_2006-1.

# "Please rate how politically liberal versus conservative you see yourself as being."

# (1 = Extremely Liberal to 7 = Extremely Conservative)



# #### Politically Right Wing; pol_wing

# We measured participants' political right-wing orientation using a single item adapted from @jost_end_2006-1.

# "Please rate how politically left-wing versus right-wing you see yourself as being.."

# (1 = Extremely left-wing to 7 = Extremely right-wing)


# #### Relationship status; partner

# "What is your relationship status?" (e.g., single, married, de-facto, civil union, widowed, living together, etc.)


# #### Religion Affiliation; religion_religious

# Participants were asked to indicate their religion identification ("Do you identify with a religion and/or spiritual group?") on a binary response (1 = Yes, 0 = No). We then asked, "What religion or spiritual group?" These questions are used in the New Zealand Census


# #### Support; support

# Participants' perceived social support was measured using three items from @cutrona1987 and @williams_cyberostracism_2000:

# (1) "There are people I can depend on to help me if I really need it"; support_help
# (2) "There is no one I can turn to for guidance in times of stress"; support_noguidance_reversed
# (3) "I know there are people I can turn to when I need help." ; support_turnto

# Participants indicated the extent to which they agreed with those items (1 = Strongly Disagree to 7 = Strongly Agree). The second item was negatively worded, so we reversely recorded the responses to this item.

# #### Volunteers; volunteers.

# We asked participants,"Please estimate how many hours you spent doing each of the following things last week" and responded to an item ("voluntary/charitable work") from [@sibley2011].
