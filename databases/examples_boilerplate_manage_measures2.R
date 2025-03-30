# Examples for boilerplate_manage_measures2()
library(tidyverse)

# set measures pathes
measures_path <- "/Users/joseph/GIT/templates/databases/measures"

# ------------------------------------------------------
# 1. List all measures from the specified database
# ------------------------------------------------------
measures_db <- boilerplate_manage_measures2(
  action = "list",
  measures_path = "/Users/joseph/GIT/templates/databases/measures",
  file_name = "measures_db.rds"
)

# Check the structure of the database
str(measures_db, max.level = 1, list.len = length(measures_db))  # Just see top-level entries


# example remove
measures_db <- boilerplate_manage_measures2(
  action = "remove",
  name = "hours_children",
  measures_path = "/Users/joseph/GIT/templates/databases/measures",
  file_name = "measures_db.rds"
)

# list
str(measures_db, max.level = 1, list.len = length(measures_db))  # Just see top-level entries


# save with variable removed
margot::here_save(measures_db, "measures_db", here::here(measures_path))

# ------------------------------------------------------
# 2. Add a new measure (top-level)
# ------------------------------------------------------
measures_db_new <- boilerplate_manage_measures2(
  action = "add",
  name = "hours_comp_games",
  measure = list(
    description = "Numerical: open-ended response",
    reference = "nzavs2018",
    waves = "1, 4-current",
    keywords = c("hours"),
    items = list("Hours spent ... playing computer games")
  ),
  db = measures_db
  # measures_path = "/Users/joseph/GIT/templates/databases/measures",
  # file_name = "measures_db.rds"
)

# example save
margot::here_save(measures_db_new, "measures_db", here::here(measures_path))

# ------------------------------------------------------
# 3. Add a nested measure (hierarchical structure)
# ------------------------------------------------------
measures_db <- boilerplate_manage_measures2(
  action = "add",
  name = "psychological.anxiety.social",
  measure = list(
    description = "Social anxiety was measured using a modified version of the Social Interaction Anxiety Scale (SIAS).",
    reference = "mattick1998",
    waves = "11-14",
    keywords = c("anxiety", "social", "mental health", "psychological"),
    items = list(
      "I get nervous if I have to speak with someone in authority.",
      "I have difficulty making eye contact with others.",
      "I find it difficult to mix comfortably with the people I work with."
    )
  ),
  db = measures_db  # Use the database we already loaded
)

# view
(measures_db$psychological$anxiety$social)
# ------------------------------------------------------
# 4. Access and get specific measures
# ------------------------------------------------------
# Get a top-level measure
alcohol_measure <- boilerplate_manage_measures2(
  action = "get",
  name = "alcohol_frequency",
  measures_path = "/Users/joseph/GIT/templates/databases/measures",
  file_name = "measures_db.rds"
)

# Print the measure details
cat(alcohol_measure$description)

# Get a nested measuredescription# Get a nested measure
social_anxiety <- boilerplate_manage_measures2(
  action = "get",
  name = "psychological.anxiety.social",
  db = measures_db
)

print(social_anxiety)

# ------------------------------------------------------
# 5. List all measures in a specific category
# ------------------------------------------------------
psych_measures <- boilerplate_manage_measures2(
  action = "list",
  name = "psychological",
  db = measures_db
)

# Show available subcategories
names(psych_measures)

# Show specific subcategory
names(psych_measures$anxiety)

# ------------------------------------------------------
# 6. Update an existing measure
# ------------------------------------------------------
measures_db <- boilerplate_manage_measures2(
  action = "update",
  name = "psychological.anxiety.social",
  measure = list(
    description = "Frequency of heavy episodic drinking was measured using a single item.",
    reference = "nzavs2018",
    waves = "10-current",
    keywords = c("alcohol", "binge", "drinking", "frequency", "heavy", "episodic"),
    items = list("How often do you consume six or more drinks on one occasion?")
  ),
  db = measures_db
)

# ------------------------------------------------------
# 7. Remove a measure
# ------------------------------------------------------
# Create a copy of the database first to avoid modifying the real one
temp_db <- measures_db

# Remove a measure
measures_db <- boilerplate_manage_measures2(
  action = "remove",
  name = "psychological.anxiety.social",
  db = temp_db
)

# Verify it's gone
"psychological.anxiety.social" %in% names(temp_db)

# ------------------------------------------------------
# 8. Save the database to a new file
# ------------------------------------------------------
# Create a new file in a different location for testing
result <- boilerplate_manage_measures2(
  action = "save",
  db = measures_db,
  measures_path = "/Users/joseph/GIT/templates/databases/measures",
  file_name = "updated_measures.rds"  # Different filename
)

# ------------------------------------------------------
# 9. Batch operations - working with multiple measures
# ------------------------------------------------------
# Let's add several mental health measures at once
mental_health_scales <- list(
  "psychological.depression.phq9" = list(
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

  "psychological.stress.pss" = list(
    description = "Perceived stress was measured using the Perceived Stress Scale.",
    reference = "cohen1983",
    waves = "11-14",
    keywords = c("stress", "perceived", "mental health"),
    items = list(
      "In the last month, how often have you felt that you were unable to control the important things in your life?",
      "In the last month, how often have you felt confident about your ability to handle your personal problems?",
      "In the last month, how often have you felt that things were going your way?"
    )
  )
)


# Add each measure to the database
for (measure_name in names(mental_health_scales)) {
  measures_db <- boilerplate_manage_measures2(
    action = "add",
    name = measure_name,
    measure = mental_health_scales[[measure_name]],
    db = measures_db
  )
}

# Check if they were added
measures_db$psychological$depression$phq9$description
measures_db$psychological$stress$pss$description

# ------------------------------------------------------
# 10. Working with the database and report_measures function
# ------------------------------------------------------
# Create a bibliography of specific measures
measures_bibliography <- boilerplate::boilerplate_report_measures(
  all_vars = c(
    "alcohol_frequency",
    "hours_children",
    "hours_charity"
  ),
  measure_data = measures_db,
  print_keywords = FALSE,
  print_waves = TRUE
)

# Save the bibliography to a markdown file
# writeLines(measures_bibliography, "measures_bibliography.md")

# ------------------------------------------------------
# 11. Working with both measures and methods databases
# ------------------------------------------------------
# Let's load the methods database as well
methods_db <- boilerplate_manage_text(
  category = "methods",
  action = "list",
  text_path = "/Users/joseph/GIT/templates/databases/methods",
  file_name = "methods_db.rds"
)

# Create a complete methods section that references measures
# Define study parameters
n_total = "47,940"
study_params <- list(
  exposure_var = "alcohol_frequency",
  outcome_var = "hours_children",
  n_total = n_total,
  baseline_wave = "NZAVS time 10, years 2018-2019",
  outcome_wave = "NZAVS time 12, years 2020-2021"
)

# Generate methods text
methods_text <- boilerplate_generate_text(
  category = "methods",
  sections = c(
    "sample",
    "identification_assumptions.standard",
    "statistical_estimator.lmtp.short"
  ),
  global_vars = study_params,
  text_path = "/Users/joseph/GIT/templates/databases/methods/",
  db = methods_db
)

# Get the measure details to include in the methods section
exposure_measure <- boilerplate_manage_measures2(
  action = "get",
  name = "alcohol_frequency",
  measures_path = "/Users/joseph/GIT/templates/databases/measures",
  file_name = "merged_db.rds"
)

# Format the exposure measure description
exposure_description <- paste0(
  "## Exposure Variable\n\n",
  "**", "Alcohol Frequency", "**: ",
  exposure_measure$description, " (", exposure_measure$reference, ")\n\n",
  "Item: ", exposure_measure$items[[1]], "\n\n",
  "This measure was collected during waves ", exposure_measure$waves, "."
)

# Combine all text
complete_methods <- paste(
  "# Methods\n\n",
  methods_text, "\n\n",
  exposure_description,
  sep = ""
)

# Save the complete methods section
writeLines(complete_methods, "alcohol_depression_methods.md")
