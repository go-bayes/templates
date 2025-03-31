#' Generate Methods Text from Boilerplate
#'
#' This function generates methods text by retrieving and combining text from
#' the methods database. It's a wrapper around boilerplate_generate_text with
#' methods-specific defaults. Supports arbitrarily nested section paths
#' using dot notation for flexible organization.
#'
#' @param sections Character vector. The methods sections to include (can use dot notation for nesting).
#' @param global_vars List. Variables available to all sections.
#' @param section_vars List. Section-specific variables.
#' @param text_overrides List. Direct text overrides for specific sections.
#' @param db List. Optional methods database to use.
#' @param text_path Character. Path to the directory where text database files are stored.
#' @param warn_missing Logical. Whether to warn about missing template variables.
#' @param add_headings Logical. Whether to add markdown headings to sections. Default is FALSE.
#' @param heading_level Character. The heading level to use (e.g., "###"). Default is "###".
#' @param custom_headings List. Custom headings for specific sections. Names should match section names.
#'
#' @return Character. The combined methods text.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default sections
#' methods_text <- boilerplate_methods_text(
#'   global_vars = list(
#'     exposure_var = "political_conservative",
#'     population = "university students"
#'   )
#' )
#'
#' # Using deeply nested organization with headings
#' methods_text <- boilerplate_methods_text(
#'   sections = c(
#'     "sample",
#'     "causal_assumptions.identification",
#'     "statistical.longitudinal.lmtp",
#'     "statistical.heterogeneity.grf.custom"
#'   ),
#'   global_vars = list(exposure_var = "treatment"),
#'   add_headings = TRUE
#' )
#' }
#'
#' @export
boilerplate_methods_text <- function(
    sections = c(
      "sample",
      "causal_assumptions.identification",
      "causal_assumptions.confounding_control",
      "statistical.longitudinal.lmtp"
    ),
    global_vars = list(),
    section_vars = list(),
    text_overrides = list(),
    db = NULL,
    text_path = NULL,
    warn_missing = TRUE,
    add_headings = FALSE,
    heading_level = "###",
    custom_headings = list()
) {
  boilerplate_generate_text(
    category = "methods",
    sections = sections,
    global_vars = global_vars,
    section_vars = section_vars,
    text_overrides = text_overrides,
    db = db,
    text_path = text_path,
    warn_missing = warn_missing,
    add_headings = add_headings,
    heading_level = heading_level,
    custom_headings = custom_headings
  )
}

#' Generate Results Text from Boilerplate
#'
#' This function generates results text by retrieving and combining text from
#' the results database with appropriate variable substitution. It's a wrapper
#' around boilerplate_generate_text with results-specific defaults.
#' Supports arbitrarily nested section paths using dot notation.
#'
#' @param sections Character vector. The results sections to include (can use dot notation for nesting).
#' @param results_data List. Data from analysis results to use in template variables.
#' @param section_vars List. Section-specific variables if needed beyond results_data.
#' @param text_overrides List. Direct text overrides for specific sections.
#' @param db List. Optional results database to use.
#' @param text_path Character. Path to the directory where text database files are stored.
#' @param warn_missing Logical. Whether to warn about missing template variables.
#' @param add_headings Logical. Whether to add markdown headings to sections. Default is FALSE.
#' @param heading_level Character. The heading level to use (e.g., "###"). Default is "###".
#' @param custom_headings List. Custom headings for specific sections. Names should match section names.
#'
#' @return Character. The combined results text.
#'
#' @examples
#' \dontrun{
#' # Basic usage with analysis results
#' results_text <- boilerplate_results_text(
#'   sections = c("main_effect"),
#'   results_data = list(
#'     effect_size = "0.35",
#'     confidence_interval = "95% CI: 0.21, 0.49",
#'     interpretation = "a moderate positive effect"
#'   )
#' )
#'
#' # Using domain-specific results with nested paths and headings
#' results_text <- boilerplate_results_text(
#'   sections = c("main_effect", "domain.health", "domain.psychological"),
#'   results_data = list(
#'     effect_size = "0.35",
#'     confidence_interval = "95% CI: 0.21, 0.49",
#'     interpretation = "a moderate positive effect",
#'     health_finding = "improved physical outcomes",
#'     psych_finding = "reduced stress levels"
#'   ),
#'   add_headings = TRUE
#' )
#' }
#'
#' @export
boilerplate_results_text <- function(
    sections = c("main_effect"),
    results_data = list(),
    section_vars = list(),
    text_overrides = list(),
    db = NULL,
    text_path = NULL,
    warn_missing = TRUE,
    add_headings = FALSE,
    heading_level = "###",
    custom_headings = list()
) {
  boilerplate_generate_text(
    category = "results",
    sections = sections,
    global_vars = results_data,
    section_vars = section_vars,
    text_overrides = text_overrides,
    db = db,
    text_path = text_path,
    warn_missing = warn_missing,
    add_headings = add_headings,
    heading_level = heading_level,
    custom_headings = custom_headings
  )
}


#' Generate Measures Text for Reports
#'
#' This function generates formatted text describing measures in a study,
#' organized by domains (e.g., health, psychological) and roles (exposure/outcome).
#' It integrates with the boilerplate_manage_measures() function to access measure
#' information from the measures database.
#'
#' @param exposure_var Character string. Name of the exposure variable.
#' @param outcome_vars Named list of character vectors. Names are domains,
#'   values are variable names within each domain.
#' @param measures_db List. Measures database typically obtained from boilerplate_manage_measures().
#' @param appendices_measures Character. Optional reference to appendices containing measure details.
#' @param print_waves Logical. Whether to include wave information in the output.
#' @param print_keywords Logical. Whether to include keyword information in the output.
#' @param heading_level Character. Heading level for section headers (e.g., "##").
#' @param custom_headings List. Optional custom headings for specific domains.
#'   Names should match domain names in outcome_vars.
#' @param db Optional methods database to use for templates. If NULL,
#'   the default methods database is used.
#'
#' @return Character string with formatted text describing the measures.
#'
#' @examples
#' \dontrun{
#' # Get measures database
#' measures_db <- boilerplate_manage_measures(action = "list")
#'
#' # Define exposure and outcome variables
#' exposure_var <- "political_conservative"
#' outcome_vars <- list(
#'   health = c("smoker_binary", "hlth_bmi", "hours_exercise"),
#'   psychological = c("hlth_fatigue", "kessler_latent_anxiety"),
#'   social = c("belong", "neighbourhood_community")
#' )
#'
#' # Generate measures text
#' measures_text <- boilerplate_measures_text(
#'   exposure_var = exposure_var,
#'   outcome_vars = outcome_vars,
#'   measures_db = measures_db,
#'   appendices_measures = "Appendix C",
#'   heading_level = "###"
#' )
#'
#' # Print the result
#' cat(measures_text)
#' }
#'
#' @importFrom janitor make_clean_names
#' @export
boilerplate_measures_text <- function(
    exposure_var,
    outcome_vars,
    measures_db,
    appendices_measures = NULL,
    print_waves = FALSE,
    print_keywords = FALSE,
    heading_level = "##",
    custom_headings = list(),
    db = NULL
) {
  # Only check templates if no database is provided - avoids unnecessary loading
  if (!is.null(db)) {
    # User provided a database, skip template checking to avoid auto-loading
    # This assumes the user has ensured necessary templates exist
  } else {
    # Check if templates exist and create them if needed, but only once
    db <- check_and_create_templates()
  }

  # Format data for exposure variable
  exposure_data <- format_measure_for_template(
    measures_db[[exposure_var]],
    name = exposure_var,
    print_waves = print_waves,
    print_keywords = print_keywords
  )

  # Initialize template data with exposure variable info
  template_data <- list(
    exposure_title = janitor::make_clean_names(exposure_var, case = "title"),
    exposure_formatted = exposure_data
  )

  # Initialize sections
  sections <- c("measures.exposure")

  # Format data for each domain
  for (domain in names(outcome_vars)) {
    # Create variable name for this domain in template
    domain_var_name <- paste0(domain, "_formatted")

    # Format all measures in this domain
    domain_text <- ""
    for (var in outcome_vars[[domain]]) {
      measure_info <- measures_db[[var]]
      if (!is.null(measure_info)) {
        measure_text <- format_measure_for_template(
          measure_info,
          name = var,
          print_waves = print_waves,
          print_keywords = print_keywords
        )
        domain_text <- paste0(domain_text, measure_text)
      }
    }

    # Add formatted domain text to template data
    template_data[[domain_var_name]] <- domain_text

    # Get domain heading
    if (domain %in% names(custom_headings)) {
      heading <- custom_headings[[domain]]
    } else {
      heading <- janitor::make_clean_names(domain, case = "title")
    }

    # Add domain heading to template data
    template_data[[paste0(domain, "_heading")]] <- heading

    # Add section for this domain
    sections <- c(sections, paste0("measures.domains.", domain))
  }

  # Add appendix reference if provided
  if (!is.null(appendices_measures)) {
    template_data[["appendices_measures"]] <- appendices_measures
    sections <- c(sections, "measures.appendix_reference")
  }

  # Generate the text
  measures_text <- boilerplate_generate_text(
    category = "methods",
    sections = sections,
    global_vars = template_data,
    add_headings = FALSE,
    db = db
  )

  return(measures_text)
}

#' Format a Measure for Template Substitution
#'
#' @param measure_info List containing measure information
#' @param name Character. Name of the measure if not in measure_info
#' @param print_waves Logical. Whether to include wave information
#' @param print_keywords Logical. Whether to include keyword information
#'
#' @return Character string with formatted measure information
#' @noRd
format_measure_for_template <- function(measure_info, name = NULL, print_waves = FALSE, print_keywords = FALSE) {
  if (is.null(measure_info)) {
    return(paste0("#### ", janitor::make_clean_names(name, case = "title"), "\n\n",
                  "No information available for this variable.\n\n"))
  }

  # Get measure name
  if (is.null(name)) {
    name <- measure_info$name
  }

  # Format title
  title <- janitor::make_clean_names(name, case = "title")
  formatted_text <- paste0("#### ", title, "\n\n")

  # Format items
  items <- measure_info$items
  if (length(items) > 1) {
    items_text <- paste(sapply(items, function(item) {
      paste0("*", item, "*")
    }), collapse = "\n")
    formatted_text <- paste0(formatted_text, items_text, "\n\n")
  } else if (length(items) == 1) {
    formatted_text <- paste0(formatted_text, "*", items[[1]], "*\n\n")
  }

  # Add description and reference
  if (!is.null(measure_info$description)) {
    formatted_text <- paste0(formatted_text, measure_info$description)

    # Add reference if available
    if (!is.null(measure_info$reference)) {
      formatted_text <- paste0(formatted_text, " [@", measure_info$reference, "]")
    }

    formatted_text <- paste0(formatted_text, "\n\n")
  }

  # Add waves if requested
  if (print_waves && !is.null(measure_info$waves)) {
    formatted_text <- paste0(formatted_text, "*Waves: ", measure_info$waves, "*\n\n")
  }

  # Add keywords if requested
  if (print_keywords && !is.null(measure_info$keywords)) {
    if (is.character(measure_info$keywords)) {
      keywords <- paste(measure_info$keywords, collapse = ", ")
      formatted_text <- paste0(formatted_text, "*Keywords: ", keywords, "*\n\n")
    }
  }

  return(formatted_text)
}

#' Check for and Create Required Templates
#'
#' @param existing_db Optional existing methods database to check and modify
#' @return Returns the database (modified if templates were added), or NULL if no modification needed
#' @noRd
check_and_create_templates <- function(existing_db = NULL) {
  # Get or use the methods database
  if (is.null(existing_db)) {
    # Only load the database once
    db <- boilerplate_manage_text(
      category = "methods",
      action = "list"
    )
  } else {
    db <- existing_db
  }

  # Define required templates
  required_templates <- list(
    "measures.exposure" = "## Variables\n\n### Exposure Variable\n\n{{exposure_formatted}}",
    "measures.outcome_header" = "### Outcome Variables\n\n",
    "measures.domains.health" = "#### {{health_heading}}\n\n{{health_formatted}}\n\n",
    "measures.domains.psychological" = "#### {{psychological_heading}}\n\n{{psychological_formatted}}\n\n",
    "measures.domains.social" = "#### {{social_heading}}\n\n{{social_formatted}}\n\n",
    "measures.appendix_reference" = "Detailed descriptions of how these variables were measured and operationalized can be found in **{{appendices_measures}}**."
  )

  # Check if templates exist and create them if needed
  modified <- FALSE
  for (template_name in names(required_templates)) {
    # Use nested structure for path-based navigation
    path_parts <- strsplit(template_name, "\\.")[[1]]

    # Navigate to the requested folder
    current_db <- db
    found <- TRUE

    for (i in 1:(length(path_parts)-1)) {
      if (!path_parts[i] %in% names(current_db)) {
        found <- FALSE
        break
      }
      current_db <- current_db[[path_parts[i]]]
      if (!is.list(current_db)) {
        found <- FALSE
        break
      }
    }

    # Check if the last part exists
    last_part <- path_parts[length(path_parts)]
    if (found && last_part %in% names(current_db)) {
      # Template exists
      next
    }

    # Template doesn't exist, create it
    db <- boilerplate_manage_text(
      category = "methods",
      action = "add",
      name = template_name,
      value = required_templates[[template_name]],
      db = db
    )
    modified <- TRUE
  }

  # Save the database if modified
  if (modified) {
    boilerplate_manage_text(
      category = "methods",
      action = "save",
      db = db
    )
    # Return the modified database
    return(db)
  } else if (is.null(existing_db)) {
    # Only return the database if we loaded it and the caller doesn't have one
    return(db)
  } else {
    # No modifications and caller already has the database
    return(NULL)
  }
}
