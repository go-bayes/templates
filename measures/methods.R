# # # method boilerplate
# # # joseph.bulbulia@gmail.com
# # # aug 24

pull_path <- fs::path_expand("/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph Bulbulia/00Bulbulia Pubs/DATA/nzavs-current/r-data/nzavs_data_qs")
dat <- qs::qread(here::here(pull_path))
# library(margot)
# library(glue)
# library(rlang)

detach("package:margot", unload = TRUE)
devtools::install_github("go-bayes/margot")
devtools::install_github("go-bayes/biolerplate")

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

# load necessary libraries
pacman::p_load(
  clarify,      # sensitivity analysis for causal inference
  cobalt,       # covariate balance tables and plots
  DiagrammeR,   # graph and network visualization
  doParallel,   # parallel processing with foreach
  fastDummies,  # fast creation of dummy variables
  fs,           # cross-platform file system operations
  ggbeeswarm,   # data visualisation
  ggplot2,      # data visualisation
  glmnet,       # lasso and elastic-net regularized models
  grf,          # generalized random forests
  gt,           # html tables for data frames
  gtsummary,    # summary tables for regression models
  here,         # simple and robust file referencing
  janitor,      # data cleaning and validation
  kableExtra,   # advanced table formatting
  lmtp,         # longitudinal targeted maximum likelihood estimation
  margot,       # functions for casual inference
  MatchIt,      # matching methods for causal inference
  MatchThem,    # matching methods for multiply imputed datasets
  naniar,       # handling and visualization of missing data
  parameters,   # parameters and performance metrics
  policytree,   # causal inference with policy trees
  progressr,    # progress reporting for R
  ranger,       # fast implementation of random forests
  skimr,        # summary statistics for data frames
  SuperLearner, # ensemble learning
  tidyverse,    # collection of R packages for data science
  WeightIt,     # weighting methods for covariate balancing
  xgboost,      # extreme gradient boosting
  EValue,       # compute Evalues
  data.table,   # fast data wrangling
  maq,          # qini curves
  purrr,        # data wrangling
  patchwork,     # multiple plots
  labelled,
  tidyr,
  glue
)

# example use
# Define outcomes by domain
outcomes_health <- c("smoker_binary", "hlth_bmi", "log_hours_exercise")
outcomes_psychological <- c("hlth_fatigue", "kessler_latent_anxiety")
outcomes_social <- c("belong", "neighbourhood_community")

exposure_var <- "political_conservative"

baseline_vars <- c("age", "male_binary", "parent_binary", "meaning_sense", "meaning_purpose")
# Combine all outcomes into a single list

all_outcomes <- list(
  health = outcomes_health,
  psychological = outcomes_psychological,
  social = outcomes_social
)

# just baseline vars
baseline_vars_report <- boilerplate::boilerplate_report_measures(outcome_vars = baseline_vars, measure_data = measure_data)
cat(baseline_vars_report)

# measure_data <- readRDS(here::here("boilerplate", "data", "measure_data.rds"))
# if vs code
measure_data <- readRDS(here::here("models", "boilerplate", "data", "measure_data.rds"))


result <- boilerplate_methods(
  exposure_var = exposure_var,
  outcome_vars = all_outcomes,
  n_total = 47000,
  baseline_wave = "NZAVS time 10, years 2018-2019",
  exposure_wave = "NZAVS time 11, years 2019-2020",
  outcome_wave = "NZAVS time 12, years 2020-2021",
  baseline_missing_data_proportion = 0.15,
  appendices_measures = "C",
  measure_data = measure_data,
  causal_interventions = list(interventions = c("Increase exposure_var", "Do not change exposure_var")),
  contrasts = "null",
  null_intervention = "Do not change exposure_var",
  sample = list(appendices = "A-C"),
  statistical_estimator = list(estimators = c("lmtp", "grf")),
  inclusion_criteria = c(
    "Enrolled in the 2018 wave of the New Zealand Attitudes and Values Study (NZAVS time 10).",
    "Missing covariate data at baseline was permitted, and the data was subjected to imputation methods to reduce bias."
  ),
  exclusion_criteria = c(
    "Did not answer the political conservative question at NZAVS time 10 and time 11."
  ),
  n_participants = 32451,
  confounding_control = list(appendix_ref = "B", protocol_url = "https://osf.io/ce4t9/"),
  additional_sections = list(
    sensitivity_analysis = list(
      description = "We use the E-value method to assess sensitivity to unmeasured confounding."
    ),
    scope_interventions = list(figure_ref = "@fig-custom-hist"),
    evidence_change = list(table_ref = "@tbl-custom-transition")
  )
)

cat(result)


result <- boilerplate_methods(
  exposure_var = exposure_var,
  outcome_vars = all_outcomes,
  n_total = 47000,
  baseline_wave = "NZAVS time 10, years 2018-2019",
  exposure_wave = "NZAVS time 11, years 2019-2020",
  outcome_wave = "NZAVS time 12, years 2020-2021",
  baseline_missing_data_proportion = 0.15,
  appendices_measures = "C",
  measure_data = measure_data,
  causal_interventions = list(interventions = c("Increase exposure_var", "Do not change exposure_var")),
  contrasts = "null",
  null_intervention = "Do not change exposure_var",
  sample = list(appendices = "A-C"),
  statistical_estimator = list(estimators = c("lmtp", "grf")),
  inclusion_criteria = c(
    "Enrolled in the 2018 wave of the New Zealand Attitudes and Values Study (NZAVS time 10).",
    "Missing covariate data at baseline was permitted, and the data was subjected to imputation methods to reduce bias."
  ),
  exclusion_criteria = c(
    "Did not answer the political conservative question at NZAVS time 10 and time 11."
  ),
  n_participants = 32451,
  confounding_control = list(appendix_ref = "B", protocol_url = "https://osf.io/ce4t9/"),
  additional_sections = list(
    sensitivity_analysis = list(
      description = "We use the E-value method to assess sensitivity to unmeasured confounding."
    ),
    scope_interventions = list(figure_ref = "@fig-custom-hist"),
    evidence_change = list(table_ref = "@tbl-custom-transition")
  )
)


# boilerplate_methods <- function(exposure_var, outcome_vars, n_total, baseline_wave, exposure_wave, outcome_wave, baseline_missing_data_proportion, sections_to_include = 'all', appendices_measures = NULL, ...) {
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
#     args <- list()
#
#     # Add only necessary arguments for each section
#     if (section_name == "variables") {
#       args <- list(
#         exposure_var = exposure_var,
#         outcome_vars = outcome_vars,
#         measure_data = safe_get("measure_data"),
#         appendices_measures = appendices_measures
#       )
#     } else if (section_name == "causal_interventions") {
#       args <- list(
#         exposure_var = exposure_var,
#         causal_interventions = safe_get("causal_interventions")$interventions,
#         contrasts = safe_get("contrasts", "pairwise"),
#         null_intervention = safe_get("null_intervention", NULL)
#       )
#     } else if (section_name == "target_population") {
#       args <- list(
#         statistical_estimator = if (is.list(statistical_estimator)) statistical_estimator[[1]] else statistical_estimator[1],
#         baseline_wave = baseline_wave
#       )
#     } else if (section_name == "eligibility_criteria") {
#       args <- list(
#         inclusion_criteria = inclusion_criteria,
#         exclusion_criteria = exclusion_criteria,
#         n_participants = n_participants,
#         baseline_wave = baseline_wave
#       )
#     } else if (section_name == "missing_data") {
#       args <- list(
#         estimators = statistical_estimator,
#         baseline_wave = baseline_wave,
#         exposure_wave = exposure_wave,
#         outcome_wave = outcome_wave,
#         baseline_missing_data_proportion = baseline_missing_data_proportion
#       )
#     } else if (section_name == "statistical_estimator") {
#       args <- list(estimators = statistical_estimator)
#     } else if (section_name == "additional_sections") {
#       additional_sections_args <- safe_get("additional_sections", list())
#       args <- list(
#         sensitivity_analysis = additional_sections_args$sensitivity_analysis,
#         scope_interventions = additional_sections_args$scope_interventions,
#         evidence_change = additional_sections_args$evidence_change
#       )
#     } else {
#       # For other sections, pass all common arguments
#       args <- list(
#         exposure_var = exposure_var,
#         outcome_vars = outcome_vars,
#         n_total = n_total,
#         baseline_wave = baseline_wave,
#         exposure_wave = exposure_wave,
#         outcome_wave = outcome_wave
#       )
#     }
#
#     result <- safe_execute(func_name, args)
#     if (!is.null(result)) {
#       methods_sections[[section_name]] <- result
#       cat(paste("Added result for", section_name, "to methods_sections\n"))
#     } else {
#       cat(paste("No result added for", section_name, "\n"))
#     }
#   }
#
#   # combine all sections into a single markdown string
#   cat("\nCombining all sections\n")
#   cat("Contents of methods_sections:\n")
#   print(methods_sections)
#   markdown_output <- paste(unlist(methods_sections), collapse = "\n\n")
#
#   cat("Finished boilerplate_methods function\n")
#   cat("Final markdown_output:\n")
#   cat(markdown_output)
#
#   return(markdown_output)
# }
#
#
#
# boilerplate_measures <- function(exposure_var,
#                                  outcome_vars,
#                                  measure_data,
#                                  appendices_measures = NULL) {
#
#   # Helper function to format a single measure or scale item
#   format_measure <- function(var_name, measure_info) {
#     if (is.null(measure_info)) {
#       warning(paste("No information available for variable:", var_name))
#       return(paste0("##### ", janitor::make_clean_names(var_name, case = "title"), "\n\nNo information available for this variable.\n\n"))
#     }
#
#     title <- janitor::make_clean_names(var_name, case = "title")
#
#     # Add variable type indicator
#     if (endsWith(var_name, "_binary")) {
#       title <- paste0(title, " (Binary)")
#     } else if (endsWith(var_name, "_cat")) {
#       title <- paste0(title, " (Categorical)")
#     }
#
#     description <- trimws(measure_info$description)
#     reference <- measure_info$reference
#
#     # Format the description with reference
#     if (grepl("^string_is\\s+", reference)) {
#       string_content <- sub("^string_is\\s+", "", reference)
#       string_content <- gsub("^[\"']|[\"']$", "", string_content)
#       description_with_ref <- paste0(description, " ", string_content)
#     } else {
#       description_with_ref <- paste0(description, " [@", reference, "]")
#     }
#
#     formatted_text <- paste0("##### ", title, "\n", description_with_ref, "\n")
#
#     # If the measure is a scale item, include its items
#     if ("items" %in% names(measure_info)) {
#       formatted_text <- paste0(formatted_text, "This dimension includes the following items:\n")
#       for (i in seq_along(measure_info$items)) {
#         formatted_text <- paste0(formatted_text, "   ", letters[i], ". ", measure_info$items[i], "\n")
#       }
#     }
#
#     return(formatted_text)
#   }
#
#   # Generate exposure section
#   exposure_section <- paste0(
#     "#### Exposure Indicators\n",
#     format_measure(exposure_var, measure_data[[exposure_var]])
#   )
#
#   # Generate outcome sections by domain
#   outcome_sections <- lapply(names(outcome_vars), function(domain) {
#     domain_vars <- outcome_vars[[domain]]
#     domain_section <- paste0("#### Outcome Domain: ", janitor::make_clean_names(domain, case = "title"), "\n")
#     for (var in domain_vars) {
#       domain_section <- paste0(domain_section, format_measure(var, measure_data[[var]]))
#     }
#     return(domain_section)
#   })
#
#   # Combine all sections
#   full_appendix <- paste0(
#     "### Indicators\n",
#     exposure_section,
#     paste(outcome_sections, collapse = "\n\n")
#   )
#
#   # Add appendix reference if provided
#   if (!is.null(appendices_measures)) {
#     appendix_text <- paste0("\n\nDetailed descriptions of how these variables were measured and operationalized can be found in **Appendix ", appendices_measures, "**.")
#     full_appendix <- paste0(full_appendix, appendix_text)
#   }
#
#   return(full_appendix)
# }
#
# boilerplate_methods_variables <- function(exposure_var,
#                                           outcome_vars,
#                                           measure_data,
#                                           appendices_measures = NULL,
#                                           ...) {
#   # Ignore unused arguments
#   unused_args <- list(...)
#
#   # Call boilerplate_measures with the correct parameters
#   variables_text <- boilerplate_measures(
#     exposure_var = exposure_var,
#     outcome_vars = outcome_vars,
#     measure_data = measure_data,
#     appendices_measures = appendices_measures
#   )
#
#   return(variables_text)
# }
