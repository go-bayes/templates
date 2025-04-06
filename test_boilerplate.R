set.seed(123)
devtools::install_github("go-bayes/boilerplate")
if (!require(boilerplate, quietly = TRUE)) {
  # install devtools if necessary
  if (!require(devtools, quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("go-bayes/boilerplate")
}
# library(cli)
# library(glue)
# library(here)
# library(cli)
# library(utils)
# library(stringr)
# library(janitor)
# # #
# # #
#source(here::here("/Users/joseph/GIT/boilerplate/R", "boilerplate_merege_databases.R"))
#source(here::here("/Users/joseph/GIT/boilerplate/R", "boilerplate_init.R"))
# source(here::here("/Users/joseph/GIT/boilerplate/R", "boilerplate_manage_measures_functions.R"))
# source(here::here("/Users/joseph/GIT/boilerplate/R", "boilerplate_manage_text_functions.R"))
# source(here::here("/Users/joseph/GIT/boilerplate/R", "boilerplate_merge_databases.R"))
# source(here::here("/Users/joseph/GIT/boilerplate/R", "default_databases.R"))
# source(here::here("/Users/joseph/GIT/boilerplate/R", "utilities.R"))
# #
# #
# #
# source(here::here("/Users/joseph/GIT/boilerplate/R", "utility-operations.R"))
# source(here::here("/Users/joseph/GIT/boilerplate/R", "unified-init-functions.R"))
# source(here::here("/Users/joseph/GIT/boilerplate/R", "unified-text-generation-functions.R"))
# source(here::here("/Users/joseph/GIT/boilerplate/R", "unified-import-save-functions.R"))
# source(here::here("/Users/joseph/GIT/boilerplate/R", "unified-category-specific-helpers.R"))
# source(here::here("/Users/joseph/GIT/boilerplate/R", "unified-boilerplate-merge.R"))
# source(here::here("/Users/joseph/GIT/boilerplate/R", "backward-compatibility.R"))
#




#' ## Setup
# library(boilerplate)

# First, initialise all databases with default values
# This creates the directory structure and default databases

my_project_path <- "path/to/your/project/data"

# Initialize databases in your custom location
boilerplate_init(
  categories = c("measures", "methods", "results", "discussion", "appendix", "template"),
  data_path = my_project_path,  # Specify custom path here
  create_dirs = TRUE,
  confirm = FALSE
)


boilerplate_init(
  categories = c("measures", "methods", "results", "discussion", "appendix", "template"),
  create_dirs = TRUE,
  confirm = FALSE
  )

#' ## Working with the Unified Database
# Import all databases at once into a unified structure
unified_db <- boilerplate_import()

# Examine the structure
str(unified_db, max.level = 2)

# Access specific categories using helper functions
methods_db <- boilerplate_methods(unified_db)
measures_db <- boilerplate_measures(unified_db)
results_db <- boilerplate_results(unified_db)

measures_db$anxiety

# Access specific items using dot notation
lmtp_method <- boilerplate_methods(unified_db, "statistical.longitudinal.lmtp")
cat(lmtp_method)

#' ## Generating Text with the Unified Database
# Generate methods text
methods_text <- boilerplate_generate_text(
  category = "methods",
  sections = c("sample", "statistical.longitudinal.lmtp"),
  global_vars = list(
    exposure_var = "political_conservative",
    population = "university students",
    timeframe = "2020-2022"
  ),
  db = unified_db,  # Pass the unified database
  add_headings = TRUE
)

# Print the result
cat(methods_text)
measures_db$psychological$depression
head(measures_db)


# Generate measures text
measures_text <- boilerplate_generate_measures(
  variable_heading = "Outcome Variables",
  variables = c("anxiety", "alcohol_frequency"),
  db = unified_db,  # Pass the unified database
  print_waves = TRUE,
  print_keywords = TRUE,
  label_mappings = c("anxiety" = "Kessler-6")
)

# Print the result
cat(measures_text)



# master_text_path = here::here("/Users/joseph/GIT/templates/databases/methods")
master_measures_path = here::here("/Users/joseph/GIT/templates/databases/measures")


# read
# master_methods_db = margot::here_read("master_methods_db", master_text_path)
# master_methods_db

master_measures_db = margot::here_read("merged_db", master_measures_path)
str(master_measures_db)


measures_text_master <- boilerplate_generate_measures(
  variable_heading = "Outcome Variables",
  variables = c("age", "warm_refugees"),
  db = master_measures_db,  # Pass the unified database
  print_waves = TRUE,
  print_keywords = TRUE,
  label_mappings = c("age" = "Age Open Ended")
)

# Print the result
cat(measures_text)



#' ## Modifying and Saving the Unified Database
# Add a new method
unified_db$methods$statistical$mediation <- "We tested mediation using the approach of {{mediation_approach}} with bootstrapped confidence intervals."

# Add a new measure
unified_db$measures$wellbeing <- list(
  name = "wellbeing scale",
  description = "wellbeing was measured using the WEMWBS scale.",
  reference = "tennant2007",
  waves = "1-3",
  keywords = c("wellbeing", "mental health"),
  items = list(
    "I've been feeling optimistic about the future",
    "I've been feeling useful",
    "I've been feeling relaxed"
  )
)

# Save the updated unified database
boilerplate_save(unified_db)

#' ## Working with Individual Databases
# Import just the methods database
methods_db <- boilerplate_import("methods")

# Modify the methods database
methods_db$new_section <- "This is a new method section."
methods_db$statistical$heterogeneity$new_approach <- "We used a novel approach to heterogeneity detection."

# Save just the methods database
boilerplate_save(methods_db, "methods")

# Generate text using just the methods database
methods_text <- boilerplate_generate_text(
  category = "methods",
  sections = c("new_section", "statistical.heterogeneity.new_approach"),
  db = methods_db,
  add_headings = TRUE
)

# Print the result
cat(methods_text)

#' ## Using Template Variables
# Generate report text with variable substitution
report_text <- boilerplate_generate_text(
  category = "template",
  sections = "journal_article",
  global_vars = list(
    title = "Effect of Political Conservatism on Mental Health Outcomes",
    authors = "Smith, J. & Jones, K.",
    date = "2025-04-01",
    abstract = "This study examines the relationship between political orientation and mental health.",
    introduction = "Political polarization has increased in recent years...",
    methods_sample = "We recruited 500 participants from a national panel.",
    methods_measures = "We used validated scales for all measures.",
    methods_statistical = "We used causal inference techniques.",
    results = "We found significant effects.",
    discussion = "Our findings have important implications."
  ),
  db = unified_db
)

# Print the result
cat(report_text)

#' ## Backward Compatibility
# The old functions still work for backward compatibility
measures_db_old <- boilerplate_manage_measures(action = "list")
methods_db_old <- boilerplate_manage_text(action = "list", category = "methods")

# But you'll see deprecation warnings
# It's recommended to use the new functions instead

#' ## Advanced: Adding Complex Nested Structures
# Let's add a complex nested structure to the methods database
methods_db <- boilerplate_import("methods")

# Add a nested structure for machine learning methods
methods_db$ml <- list(
  default = "We used machine learning techniques for prediction.",
  random_forest = list(
    default = "Random forests were used for classification.",
    standard = "We used the standard implementation of random forests.",
    custom = "We used a custom implementation of random forests with modified parameters."
  ),
  neural_network = list(
    default = "Neural networks were used for classification.",
    cnn = "We used convolutional neural networks for image classification.",
    rnn = "We used recurrent neural networks for sequence prediction."
  ),
  gradient_boosting = "We used gradient boosting machines for regression."
)

# Save the updated methods database
boilerplate_save(methods_db, "methods")

# Generate text from the nested structure
ml_text <- boilerplate_generate_text(
  category = "methods",
  sections = c(
    "ml",
    "ml.random_forest",
    "ml.random_forest.custom",
    "ml.neural_network.cnn",
    "ml.gradient_boosting"
  ),
  db = methods_db,
  add_headings = TRUE
)

# Print the result
cat(ml_text)

#' ## Advanced: Domain-Specific Helper Functions
# For complex workflows, you might want to create domain-specific helper functions

#' Create a standard methods section for a causal analysis paper
#'
#' @param unified_db List. The unified database
#' @param exposure Character. The exposure variable
#' @param outcome Character. The outcome variable
#' @param population Character. The study population
#' @param timeframe Character. The study timeframe
#' @return Character. The generated methods text
create_causal_methods_section <- function(
    unified_db,
    exposure,
    outcome,
    population,
    timeframe
) {
  # Define the sections we need
  sections <- c(
    "sample",
    "causal_assumptions.identification",
    "causal_assumptions.confounding_control",
    "statistical.longitudinal.lmtp"
  )

  # Generate text with all the variables
  methods_text <- boilerplate_generate_text(
    category = "methods",
    sections = sections,
    global_vars = list(
      exposure_var = exposure,
      outcome_var = outcome,
      population = population,
      timeframe = timeframe
    ),
    db = unified_db,
    add_headings = TRUE,
    heading_level = "###"
  )

  return(methods_text)
}

# Use the helper function
methods_section <- create_causal_methods_section(
  unified_db = unified_db,
  exposure = "treatment_exposure",
  outcome = "mental_health_outcome",
  population = "adult community sample",
  timeframe = "2020-2023"
)

# Print the result
cat(methods_section)




# old ---------------------------------------------------------------------


#!/usr/bin/env Rscript

# install from GitHub if not already installed
# if (!require(boilerplate, quietly = TRUE)) {
#   # install devtools if necessary
#   if (!require(devtools, quietly = TRUE)) {
#     install.packages("devtools")
#   }
#   devtools::install_github("go-bayes/boilerplate")
# }

# initialise the default text databases (excluding measures)
boilerplate_init_text(create_dirs = TRUE, confirm = TRUE)

# set the file path (adjust if necessary)
methods_db_path <- "/Users/joseph/GIT/templates/boilerplate/data/methods_db.rds"

# load the database
methods_db <- readRDS(methods_db_path)

# inspect the structure
str(methods_db)

# add a new method entry

# ------------------------------------------------------
# sample section
# ------------------------------------------------------
sample_information_text <- "Data were collected as part of the New Zealand Attitudes and Values Study (NZAVS), an annual longitudinal national probability panel assessing New Zealand residents’ social attitudes, personality, ideology, and health outcomes. The panel began in 2009 and has since expanded to include over fifty researchers, with responses from {{n_total}} participants to date. The study operates independently of political or corporate funding and is based at a university. It employs prize draws to incentivise participation. The NZAVS tends to slightly under-sample males and individuals of Asian descent and to over-sample females and Māori (the Indigenous people of New Zealand). To enhance the representativeness of our sample population estimates for the target population of New Zealand, we apply census-based survey weights that adjust for age, gender, and ethnicity (New Zealand European, Asian, Māori, Pacific) [@sibley2021]. For more information about the NZAVS, visit: [OSF.IO/75SNB](https://doi.org/10.17605/OSF.IO/75SNB). Refer to [Appendix A](#appendix-timeline) for a histogram of daily responses for this cohort."

# Add using name = "sample"
# add sample text as a nested list element
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "sample_nzavs",
  value = sample_information_text,
  db = methods_db,
  confirm = FALSE
)
# add
identification_text <- "This study relies on the following identification assumptions for estimating the causal effect of {{exposure_var}}:

1. **Consistency**: the observed outcome under the observed {{exposure_var}} is equal to the potential outcome under that exposure level. As part of consistency, we assume no interference: the potential outcomes for one individual are not affected by the {{exposure_var}} status of other individuals.

2. **No unmeasured confounding**: all variables that affect both {{exposure_var}} and the outcome have been measured and accounted for in the analysis.

3. **Positivity**: there is a non-zero probability of receiving each level of {{exposure_var}} for every combination of values of {{exposure_var}} and confounders in the population."


# add identification assumptions section
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "identification_assumptions.standard",
  value = identification_text,
  db = methods_db,
  confirm = FALSE
)



methods_text <- boilerplate_generate_text(
  category = "methods",
  sections = c("sample", "identification_assumptions.standard"),
  global_vars = list(
    exposure_var = "perfectionism",
    n_total = "47520"
  ),
  db = methods_db,
  add_headings = TRUE
)

cat(methods_text)

# --------------- Managing Measures ---------------

# First initialise the measures database separately from text databases
boilerplate_init_measures(create_dirs = TRUE, confirm = TRUE)

# Then load the database
measures_db <- boilerplate_manage_measures(action = "list")

# Add a measure with a flat name (no dots)
measures_db <- boilerplate_manage_measures(
  action = "add",
  name = "anxiety_gad7", # use underscore instead of dot
  measure = list(
    description = "Anxiety was measured using the GAD-7 scale.",
    reference = "spitzer2006",
    waves = "1-3",
    keywords = c("anxiety", "mental health", "gad"),
    items = list(
      "Feeling nervous, anxious, or on edge",
      "Not being able to stop or control worrying",
      "Worrying too much about different things",
      "Trouble relaxing"
    )
  ),
  confirm = FALSE,
  db = measures_db
)

# Save the measures database with an explicit file name (required for save action)
boilerplate_manage_measures(
  action = "save",
  db = measures_db,
  file_name = "measures_db.rds",
  confirm = FALSE # explicit file name required for save action
)

# Then reference it with the same flat name
exposure_text <- boilerplate_generate_measures(
  variable_heading = "Exposure Variable",
  variables = "anxiety_gad7", # match the name you used above
  db = measures_db,
  heading_level = 3,
  subheading_level = 4,
  print_waves = TRUE
)

cat("Exposure Text:\n")
cat(exposure_text)
cat("\n\n")

# Add additional measures for later use
measures_db <- boilerplate_manage_measures(
  action = "add",
  name = "depression_phq9",
  measure = list(
    description = "Depression was measured using the PHQ-9 scale.",
    reference = "kroenke2001",
    waves = "1-3",
    keywords = c("depression", "mental health", "phq"),
    items = list(
      "Little interest or pleasure in doing things",
      "Feeling down, depressed, or hopeless",
      "Trouble falling or staying asleep",
      "Feeling tired or having little energy"
    )
  ),
  db = measures_db,
  confirm = FALSE
)

# Save updated measures database
boilerplate_manage_measures(
  action = "save",
  db = measures_db,
  file_name = "measures_db.rds",
  confirm = FALSE
)

# Generate text for outcome variables
psych_text <- boilerplate_generate_measures(
  variable_heading = "Psychological Outcomes",
  variables = c("anxiety_gad7", "depression_phq9"),
  db = measures_db,
  heading_level = 3,
  subheading_level = 4,
  print_waves = TRUE
)

cat("Psychological Outcomes Text:\n")
cat(psych_text)
cat("\n\n")

# ------------- Statistical methods text -------------

# Add statistical methods entry
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "statistical.longitudinal.lmtp",
  value = "Statistical analyses were conducted using {{software}}. We used longitudinal modified treatment policy (LMTP) estimation to account for time-varying confounding.",
  db = methods_db,
  confirm = FALSE
)

# Generate statistical methods text
stats_text <- boilerplate_generate_text(
  category = "methods",
  sections = c("statistical.longitudinal.lmtp"),
  global_vars = list(software = "R version 4.2.0"),
  add_headings = TRUE,
  custom_headings = list("statistical.longitudinal.lmtp" = "LMTP"),
  heading_level = "###",
  db = methods_db
)

cat("Statistical Methods Text:\n")
cat(stats_text)
cat("\n\n")

# ------------- Combine all sections -------------

# Make sure we have a sample text section
sample_text <- boilerplate_generate_text(
  category = "methods",
  sections = c("sample_selection"),
  global_vars = list(
    population = "university students",
    timeframe = "2020-2021",
  ),
  db = methods_db,
  add_headings = TRUE
)

target_population_text <- "The target population for this study comprises New Zealand residents as represented in the {{baseline_wave}} of the New Zealand Attitudes and Values Study (NZAVS) during the years {{baseline_wave}} weighted by New Zealand Census weights for age, gender, and ethnicity (refer to @sibley2021). The NZAVS is a national probability study designed to reflect the broader New Zealand population accurately. Despite its comprehensive scope, the NZAVS has some limitations in its demographic representation. Notably, it tends to under-sample males and individuals of Asian descent while over-sampling females and Māori (the indigenous peoples of New Zealand). To address these disparities and enhance the accuracy of our findings, we apply New Zealand Census survey weights to the sample data."

methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "target_population.nzavs",
  value = target_population_text,
  db = methods_db,
  confirm = FALSE
)
)
# Combine all sections into a complete methods section
methods_section <- paste(
  "## Methods\n\n",
  sample_text, "\n\n",
  "### Variables\n\n",
  exposure_text, "\n",
  "### Outcome Variables\n\n",
  psych_text, "\n\n",
  stats_text,
  sep = ""
)

cat("Complete Methods Section:\n")
cat(methods_section)
cat("\n\n")

# ------------- Appendix Content -------------

# initialise appendix database
boilerplate_init_text(
  categories = "appendix",
  create_dirs = TRUE,
  confirm = TRUE,
  confirm = FALSE
)

# load appendix database
appendix_db <- boilerplate_manage_text(
  category = "appendix",
  action = "list"
)

# add detailed measures documentation to appendix
appendix_db <- boilerplate_manage_text(
  category = "appendix",
  action = "add",
  name = "detailed_measures",
  value = "# Detailed Measures Documentation\n\n## Overview\n\nThis appendix provides comprehensive documentation for all measures used in this study, including full item text, response options, and psychometric properties.\n\n## {{exposure_var}} Measure\n\n{{exposure_details}}\n\n## Outcome Measures\n\n{{outcome_details}}",
  db = appendix_db,
  confirm = FALSE
)

# save appendix database with explicit file name
boilerplate_manage_text(
  category = "appendix",
  action = "save",
  db = appendix_db,
  file_name = "appendix_db.rds"  # explicit file name required for save action,
  confirm = FALSE
)

# generate appendix text with variable substitution
appendix_text <- boilerplate_generate_text(
  category = "appendix",
  sections = c("detailed_measures"),
  global_vars = list(
    exposure_var = "Perfectionism",
    exposure_details = "The perfectionism measure consists of 3 items...",
    outcome_details = "Anxiety was measured using the GAD-7 scale..."
  ),
  db = appendix_db
)

cat("Appendix Text:\n")
cat(appendix_text)
cat("\n\n")

# ------------- Complete Document Workflows -------------

# Add a template to the database
template_db <- boilerplate_manage_text(
  category = "template",
  action = "list"
)

# Add a journal article template
template_db <- boilerplate_manage_text(
  category = "template",
  action = "add",
  name = "journal_article",
  value = "---\ntitle: {{title}}\nauthor: {{authors}}\ndate: {{date}}\n---\n\n# Abstract\n\n{{abstract}}\n\n# Introduction\n\n{{introduction}}\n\n# Methods\n\n{{methods_sample}}\n\n{{methods_measures}}\n\n{{methods_statistical}}\n\n# Results\n\n{{results}}\n\n# Discussion\n\n{{discussion}}",
  db = template_db,
  confirm = FALSE
)

# Save template database
boilerplate_manage_text(
  category = "template",
  action = "save",
  db = template_db,
  file_name = "template_db.rds",
  confirm = FALSE
)

# function to generate a complete document from a template
generate_document <- function(template_name, study_params, section_contents) {
  # get the template
  template_text <- boilerplate_manage_text(
    category = "template",
    action = "get",
    name = template_name,
    db = template_db
  )

  # apply template variables (combining study params and section contents)
  all_vars <- c(study_params, section_contents)

  # replace placeholders in template
  for (var_name in names(all_vars)) {
    placeholder <- paste0("{{", var_name, "}}")
    template_text <- gsub(placeholder, all_vars[[var_name]], template_text, fixed = TRUE)
  }

  return(template_text)
}

# define study parameters
study_params <- list(
  title = "Political Orientation and Social Wellbeing in New Zealand",
  authors = "Jane Smith, John Doe, and Robert Johnson",
  date = format(Sys.Date(), "%B %d, %Y")
)

# define section contents
section_contents <- list(
  abstract = "This study investigates the causal relationship between political orientation and social wellbeing using data from the New Zealand Attitudes and Values Study.",
  introduction = "Understanding the relationship between political beliefs and wellbeing has important implications for social policy and public health...",
  methods_sample = "Participants were recruited from university students during 2020-2021.",
  methods_measures = "Political orientation was measured using a 7-point scale...",
  methods_statistical = "We used the LMTP estimator to address confounding...",
  results = "Our analysis revealed significant effects of political conservatism on social wellbeing...",
  discussion = "These findings suggest that political orientation may causally influence wellbeing through several pathways..."
)

# generate the document
journal_article <- generate_document(
  template_name = "journal_article",
  study_params = study_params,
  section_contents = section_contents
)

cat("Journal Article Preview (first 2500 chars):\n")
cat(substr(journal_article, 1, 2500), "...\n\n")

# ------------- Audience-Specific Reports -------------

# Add audience-specific LMTP descriptions
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "statistical_estimator.lmtp.technical_audience",
  value = "We estimate causal effects using the Longitudinal Modified Treatment Policy (LMTP) estimator within a Targeted Minimum Loss-based Estimation (TMLE) framework. This semi-parametric estimator leverages the efficient influence function (EIF) to achieve double robustness and asymptotic efficiency.",
  db = methods_db,
  confirm = FALSE
)

methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "statistical_estimator.lmtp.applied_audience",
  value = "We estimate causal effects using the LMTP estimator. This approach combines machine learning with causal inference methods to estimate treatment effects while avoiding strict parametric assumptions.",
  db = methods_db,
  confirm = FALSE
)

methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "statistical_estimator.lmtp.general_audience",
  value = "We used advanced statistical methods that account for multiple factors that might influence both {{exposure_var}} and {{outcome_var}}. This method helps us distinguish between mere association and actual causal effects.",
  db = methods_db,
  confirm = FALSE
)

# save methods database with explicit file name
boilerplate_manage_text(
  category = "methods",
  action = "save",
  db = methods_db,
  file_name = "methods_db.rds",  # explicit file name required for save action
  confirm = FALSE
)

# function to generate methods text for different audiences
generate_methods_by_audience <- function(audience = c("technical", "applied", "general")) {
  audience <- match.arg(audience)

  # select appropriate paths based on audience
  lmtp_path <- paste0("statistical_estimator.lmtp.", audience, "_audience")

  # generate text
  boilerplate_generate_text(
    category = "methods",
    sections = c("sample", lmtp_path),
    global_vars = list(
      exposure_var = "political_conservative",
      outcome_var = "social_wellbeing",
      n_total = "47,240"
    ),
    db = methods_db
  )
}

# generate reports for different audiences
technical_report <- generate_methods_by_audience("technical")
applied_report <- generate_methods_by_audience("applied")
general_report <- generate_methods_by_audience("general")

cat("General audience report:\n")
cat(general_report)
cat("\n\n")

cat("Applied audience report:\n")
cat(applied_report)
cat("\n\n")

cat("Technical audience report:\n")
cat(technical_report)
cat("\n\n")

# Optional: Save outputs to files
# write.table(methods_section, "methods_section.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table(journal_article, "journal_article.qmd", row.names = FALSE, col.names = FALSE, quote = FALSE)

cat("Script execution complete.\n")







# tests -------------------------------------------------------------------


#' Generate Formatted Text for Measures
#'
#' This function generates formatted markdown text describing measures in a study.
#' It creates a simple output with customisable heading levels, focusing on presenting
#' measure information in a clean, consistent format.
#'
#' @param variable_heading Character. Heading for the variable section (e.g., "Exposure Variable", "Outcome Variables").
#' @param variables Character vector. Names of the variables to include.
#' @param db List. Measures database typically obtained from boilerplate_manage_measures().
#' @param heading_level Integer. Heading level for the section header (e.g., 2 for ##, 3 for ###). Default is 3.
#' @param subheading_level Integer. Heading level for individual variables (e.g., 3 for ###, 4 for ####). Default is 4.
#' @param print_waves Logical. Whether to include wave information in the output. Default is FALSE.
#' @param print_keywords Logical. Whether to include keyword information in the output. Default is FALSE.
#' @param appendices_measures Character. Optional reference to appendices containing measure details.
#' @param label_mappings Named character vector. Mappings to transform variable names in the output.
#'   For example, c("sdo" = "Social Dominance Orientation", "born_nz_binary" = "Born in NZ").
#'   If a variable name contains any of the keys in this vector, that part will be replaced with the corresponding value.
#' @param quiet Logical. If TRUE, suppresses all CLI alerts. Default is FALSE.
#'
#' @return Character string with formatted text describing the measures.
#'
#' @examples
#' \dontrun{
#' # Load measures database with safety parameters
#' measures_db <- boilerplate_manage_measures(
#'   action = "list",
#'   create_dirs = TRUE,
#'   confirm = TRUE
#' )
#'
#' # Define variable label mappings
#' var_labels <- c(
#'   "sdo" = "Social Dominance Orientation",
#'   "born_nz_binary" = "Born in NZ"
#' )
#'
#' # Generate exposure variable text with custom labels
#' exposure_text <- boilerplate_generate_measures(
#'   variable_heading = "Exposure Variable",
#'   variables = "political_conservative",
#'   db = measures_db,
#'   print_waves = TRUE,
#'   label_mappings = var_labels
#' )
#'
#' # Generate outcome variables text
#' outcome_text <- boilerplate_generate_measures(
#'   variable_heading = "Outcome Variables",
#'   variables = c("anxiety.gad7", "depression.phq9"),
#'   db = measures_db,
#'   appendices_measures = "Appendix A"
#' )
#'
#' # Print the results
#' cat(exposure_text)
#' cat(outcome_text)
#' }
#'
#' @importFrom janitor make_clean_names
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#' @export
boilerplate_generate_measures <- function(
    variable_heading,
    variables,
    db,
    heading_level = 3,
    subheading_level = 4,
    print_waves = FALSE,
    print_keywords = FALSE,
    appendices_measures = NULL,
    label_mappings = NULL,
    quiet = FALSE
) {
  # input validation
  if (!is.character(variable_heading)) {
    if (!quiet) cli_alert_danger("variable_heading must be a character string")
    stop("variable_heading must be a character string")
  }

  if (!is.character(variables)) {
    if (!quiet) cli_alert_danger("variables must be a character vector")
    stop("variables must be a character vector")
  }

  if (!is.list(db)) {
    if (!quiet) cli_alert_danger("db must be a list")
    stop("db must be a list")
  }

  if (!quiet) cli_alert_info("generating formatted text for {length(variables)} {variable_heading}")

  # create heading markers
  heading_marker <- paste(rep("#", heading_level), collapse = "")
  subheading_marker <- paste(rep("#", subheading_level), collapse = "")

  if (!quiet) cli_alert_info("using heading level {heading_level} and subheading level {subheading_level}")

  # initialise output text
  output_text <- paste0(heading_marker, " ", variable_heading, "\n\n")

  # process each variable
  for (var in variables) {
    if (!quiet) cli_alert_info("processing variable: {var}")

    # get measure info
    measure_info <- db[[var]]

    # transform variable name if mapping is provided
    var_display <- if (!is.null(label_mappings)) {
      if (!quiet) cli_alert_info("applying label mappings to {var}")
      transform_label(var, label_mappings, quiet)
    } else {
      var
    }

    if (is.null(measure_info)) {
      # handle missing measures
      if (!quiet) cli_alert_warning("no information available for variable: {var}")
      title <- janitor::make_clean_names(var_display, case = "title")
      var_text <- paste0(subheading_marker, " ", title, "\n\n",
                         "no information available for this variable.\n\n")
    } else {
      # get variable title, applying mapping if provided
      title <- if (!is.null(measure_info$name)) {
        # apply mapping to the name from measure_info
        name_display <- if (!is.null(label_mappings)) {
          if (!quiet) cli_alert_info("applying label mappings to measure name: {measure_info$name}")
          transform_label(measure_info$name, label_mappings, quiet)
        } else {
          measure_info$name
        }
        janitor::make_clean_names(name_display, case = "title")
      } else {
        janitor::make_clean_names(var_display, case = "title")
      }

      # start with variable title
      var_text <- paste0(subheading_marker, " ", title, "\n\n")

      # add items if available
      items <- measure_info$items
      if (!is.null(items) && length(items) > 0) {
        if (!quiet) cli_alert_info("adding {length(items)} items for {var}")
        if (is.list(items)) {
          items_text <- paste(sapply(items, function(item) {
            paste0("*", item, "*")
          }), collapse = "\n")
        } else if (is.character(items)) {
          items_text <- paste(sapply(items, function(item) {
            paste0("*", item, "*")
          }), collapse = "\n")
        }
        var_text <- paste0(var_text, items_text, "\n\n")
      }

      # add description if available
      if (!is.null(measure_info$description)) {
        if (!quiet) cli_alert_info("adding description for {var}")
        var_text <- paste0(var_text, measure_info$description)

        # add reference if available
        if (!is.null(measure_info$reference)) {
          if (!quiet) cli_alert_info("adding reference: {measure_info$reference}")
          var_text <- paste0(var_text, " [@", measure_info$reference, "]")
        }

        var_text <- paste0(var_text, "\n\n")
      }

      # add waves if requested and available
      if (print_waves && !is.null(measure_info$waves)) {
        if (!quiet) cli_alert_info("adding waves information: {measure_info$waves}")
        var_text <- paste0(var_text, "*waves: ", measure_info$waves, "*\n\n")
      }

      # add keywords if requested and available
      if (print_keywords && !is.null(measure_info$keywords)) {
        if (is.character(measure_info$keywords)) {
          if (length(measure_info$keywords) > 1) {
            keywords <- paste(measure_info$keywords, collapse = ", ")
          } else {
            keywords <- measure_info$keywords
          }
          if (!quiet) cli_alert_info("adding keywords: {keywords}")
          var_text <- paste0(var_text, "*keywords: ", keywords, "*\n\n")
        }
      }
    }

    # add to output
    output_text <- paste0(output_text, var_text)
  }

  # add appendix reference if provided
  if (!is.null(appendices_measures)) {
    if (!quiet) cli_alert_info("adding appendix reference: {appendices_measures}")
    output_text <- paste0(
      output_text,
      "detailed descriptions of how these variables were measured and operationalised can be found in **",
      appendices_measures,
      "**.\n\n"
    )
  }

  if (!quiet) cli_alert_success("successfully generated formatted text for {variable_heading}")
  return(output_text)
}



#' Transform a label using provided mappings
#'
#' @param label Character. The original label to transform
#' @param label_mapping Named character vector. Mappings to transform the label
#' @param quiet Logical. If TRUE, suppresses all CLI alerts. Default is FALSE.
#'
#' @return Character. The transformed label
#' @noRd
transform_label <- function(label, label_mapping = NULL, quiet = FALSE) {
  # apply mapping with partial substitutions
  if (!is.null(label_mapping)) {
    for (pattern in names(label_mapping)) {
      if (grepl(pattern, label, fixed = TRUE)) {
        replacement <- label_mapping[[pattern]]
        label <- gsub(pattern, replacement, label, fixed = TRUE)
        if (!quiet) cli_alert_info("mapped label: {pattern} -> {replacement}")
      }
    }
  }
  return(label)
}

# Removed duplicated boilerplate_measures_text function
