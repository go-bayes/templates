# Example script 1: Basic Usage of Boilerplate Text Management
#' @examples
#' library(boilerplate)
#'

#' # 1. Initialize the databases (do this once per project)
boilerplate_init_text()

path_measures_test = '/Users/joseph/GIT/templates/databases/measures'
path_methods_test = '/Users/joseph/GIT/templates/databases/methods'

path_measures_test = '/Users/joseph/GIT/templates/databases/measures'

test_measures_db <- margot::here_read("merged_db", path_measures_test)
str(test_measures_db,  max.level = 2)


test_methods_db <- margot::here_read("methods_db", path_methods_test)
test_2_measures_db <- margot::here_read("merged_db", path_measures_test)


#' # 2. List the available methods text
methods_db_test <- boilerplate_manage_text(category = "methods", action = "list", db = test_methods_db)
str(methods_db_test, max.level = 1)

#'
#' # 3. Add a new method entry
methods_db_test <- boilerplate_manage_text(
  category = "methods",
  action = "add",
  name = "statistical.heterogeneity.machine_learning",
  value = "We examined treatment effect heterogeneity using {{ml_method}} with {{software}}.",
  db = methods_db_test
)

#'
#' # 4. Generate methods text with variables
methods_text <- boilerplate_generate_text(
  category = "methods",
  sections = c(
    "sample",
    "causal_assumptions.identification",
    "statistical.heterogeneity.machine_learning"
  ),
  global_vars = list(
    exposure_var = "cognitive behavioral therapy",
    population = "adolescents with anxiety disorders",
    timeframe = "2018-2022",
    ml_method = "causal forests",
    software = "the 'grf' R package"
  ),
  db = methods_db_test
)

cat(methods_text)

# Example script 2: Working with Measures
#' @examples
#' library(boilerplate)
#'
#' # 1. Add a new measure
test_measures_db <- boilerplate_manage_measures2(
  action = "add",
  name = "depression.phq9",
  measure = list(
    description = "Depression was measured using the PHQ-9 scale.",
    reference = "kroenke2001",
    waves = "10-15",
    keywords = c("depression", "mental health", "phq"),
    items = list(
      "Little interest or pleasure in doing things",
      "Feeling down, depressed, or hopeless",
      "Trouble falling or staying asleep, or sleeping too much",
      "Feeling tired or having little energy"
    )
  ),
  db = test_measures_db
)
test_measures_db$depression$phq9

#'
#' # 2. Get the measure details
#' phq9_measure <- boilerplate_manage_measures2(
#'   action = "get",
#'   name = "depression.phq9",
#'   db = measures_db
#' )
#'
#' str(phq9_measure)
#'
#' # 3. Save the measures database
#' boilerplate_manage_measures2(
#'   action = "save",
#'   db = measures_db,
#'   file_name = "my_measures.rds"
#' )

# Example script 3: Comprehensive Research Workflow
#' @examples
#' library(boilerplate)
#' library(tidyverse)
#'
#' # Define study parameters
study_params <- list(
  exposure_var = "political_conservative",
  outcome_var = "social_wellbeing",
  n_total = "47,940",
  baseline_wave = "NZAVS time 10, years 2018-2019",
  exposure_wave = "NZAVS time 11, years 2019-2020",
  outcome_wave = "NZAVS time 12, years 2020-2021",
  timeframe = "2018-2021",
  population = "New Zealand residents",
  baseline_missing_data_proportion = "15%",
  causal_interventions = c("Increase political conservatism", "Do not change political conservatism"),
  ml_method = "causal forests",
  software = "grf R package",
  flipped_list = c("anxiety", "depression", "fatigue", "rumination")
)
#'

#'
#' methods_db <- boilerplate_manage_text(
#'   category = "methods",
#'   action = "add",
#'   name = "approach.grf_cate_short",
#'   value = "Our primary aim was to look beyond average treatment effects (ATE) and explore whether
#'     the intervention's impact varied systematically across individuals. To that end, we estimated
#'     individualised treatment effects, also called conditional average treatment effects (CATEs),
#'     using causal forests [@grf2024], a machine learning approach tailored for detecting treatment-effect
#'     heterogeneity based on covariates.
#'
#'     We standardised effect directions by inverting any outcome where 'lower is better', so positive
#'     values consistently denote improvement. The following outcomes were inverted: {{flipped_list}}.",
#'   db = methods_db
#' )
#'
#' # Generate comprehensive methods section
#' complete_methods <- boilerplate_generate_text(
#'   category = "methods",
#'   sections = c(
#'     "sample",
#'     "causal_assumptions.identification",
#'     "causal_assumptions.confounding_control",
#'     "statistical.heterogeneity.grf.default",
#'     "approach.grf_cate_short"
#'   ),
#'   global_vars = study_params,
#'   db = methods_db,
#'   add_headings = TRUE,
#'   heading_level = "##",
#'   custom_headings = list(
#'     "sample" = "Study Population",
#'     "causal_assumptions.identification" = "Causal Framework",
#'     "causal_assumptions.confounding_control" = "Confounding Control",
#'     "statistical.heterogeneity.grf.default" = "Heterogeneity Analysis",
#'     "approach.grf_cate_short" = "Analytical Approach"
#'   )
#' )
#'
#' cat(complete_methods)
#'
#' # Save methods to a file
#' write_file(complete_methods, "political_orientation_methods.md")
#'
#' # Generate results section
#' results_text <- boilerplate_results_text(
#'   sections = c("main_effect", "domain.psychological"),
#'   results_data = list(
#'     effect_size = "0.35",
#'     confidence_interval = "95% CI: 0.21, 0.49",
#'     interpretation = "a moderate positive effect",
#'     psych_finding = "reduced anxiety and depression levels"
#'   ),
#'   add_headings = TRUE
#' )
#'
#' cat(results_text)

# Example script 4: Merging Databases from Different Projects
#' @examples
#' library(boilerplate)
#'
#' # Load two separate measure databases
#' project_a_db <- boilerplate_manage_measures2(
#'   action = "list",
#'   measures_path = "project_a/data",
#'   file_name = "measures_a.rds"
#' )
#' #'
#' #' project_b_db <- boilerplate_manage_measures2(
#' #'   action = "list",
#' #'   measures_path = "project_b/data",
#' #'   file_name = "measures_b.rds"
#' #' )
#' #'
#' # Merge the databases with custom names
test_merged_db <- boilerplate_merge_databases(
  db1 = test_measures_db,
  db2 = test_2_measures_db,
  db1_name = "Project A (2020)",
  db2_name = "Project B (2022)"
)

test_merged_db
#'
# Save the merged database
boilerplate_manage_measures2(
  action = "save",
  db = test_merged_db,
  measures_path = "examples/data",
  file_name = "test_merged_db.rds"
)


# simple methods section:

# Simple approach to ordering methods sections with measures in between

library(boilerplate)

# Define your variables
exposure_var <- "political_conservative"
outcome_vars <- list(
  health = c("smoker_binary", "hlth_bmi"),
  psychological = c("kessler_latent_anxiety")
)


# Define study parameters
study_params <- list(
  exposure_var = "political_conservative",
  n_total = "47,940",
  population = "New Zealand residents",
  timeframe = "2018-2021"
)

# 1. Generate the sample section
sample_text <- boilerplate_generate_text(
  category = "methods",
  sections = "sample.nzavs",
  global_vars = study_params,
  db = test_methods_db
)

# 2. Generate the measures section
measures_text <- boilerplate_measures_text(
  exposure_var = exposure_var,
  outcome_vars = outcome_vars,
  measures_db = test_measures_db
)

# 3. Generate the identification assumptions section
identification_text <- boilerplate_generate_text(
  category = "methods",
  sections = "identification_assumptions.standard",
  global_vars = study_params
)

# 4. Generate any remaining sections
confounding_text <- boilerplate_generate_text(
  category = "methods",
  sections = "confounding_control.vanderweele",
  global_vars = study_params
)

# 5. Combine everything in the desired order
complete_methods <- paste(
  "# Methods\n\n",
  sample_text,
  "\n\n",
  measures_text,
  "\n\n",
  identification_text,
  "\n\n",
  confounding_text,
  sep = ""
)

# Print the result
cat(complete_methods)

# Or save it to a file
writeLines(complete_methods, "methods_section.md")



