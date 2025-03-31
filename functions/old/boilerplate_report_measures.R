#' Create a Bibliography of Measures
#'
#' This function generates a formatted bibliography of measures used in a study,
#' including details about questions and citations. It supports measures from both
#' the old format and the new hierarchical format.
#'
#' @param all_vars A character vector of all variable names, or `TRUE` to use all variables
#'   from `baseline_vars`, `exposure_var`, and `outcome_vars`. If `NULL`, the function will use the variables provided in `baseline_vars`, `exposure_var`, and `outcome_vars`.
#' @param baseline_vars A character vector of baseline variable names. Ignored if `all_vars` is not `NULL`.
#' @param exposure_var A character string of the exposure variable name. Ignored if `all_vars` is not `NULL`.
#' @param outcome_vars A character vector of outcome variable names. Ignored if `all_vars` is not `NULL`.
#' @param measure_data A list containing information about each measure. The list should include elements such as "description" and "reference" for each variable.
#'   Can be a flat list or a hierarchical list from boilerplate_manage_measures2().
#' @param custom_titles A named list of custom titles for variables (optional). The names should match the variable names in `all_vars`.
#' @param print_keywords Logical, whether to print keywords for each measure (default: `FALSE`).
#' @param print_waves Logical, whether to print wave information for each measure (default: `FALSE`).
#'
#' @return A character string containing the formatted bibliography, suitable for inclusion in a Markdown document.
#'
#' @examples
#' # Example 1: Using separate variable lists
#' baseline_vars <- c("age", "male_binary", "parent_binary")
#' exposure_var <- "political_conservative"
#' outcome_vars <- c("smoker_binary", "hlth_bmi", "log_hours_exercise")
#'
#' # Assuming measure_data is a list with information about each variable
#' appendix_text_1 <- boilerplate_report_measures(
#'   baseline_vars = baseline_vars,
#'   exposure_var = exposure_var,
#'   outcome_vars = outcome_vars,
#'   measure_data = measure_data
#' )
#'
#' # Example using the new hierarchical structure
#' measures_db <- boilerplate_manage_measures2(action = "list")
#' appendix_text_2 <- boilerplate_report_measures(
#'   all_vars = c("psychological.anxiety.generalised", "psychological.depression"),
#'   measure_data = measures_db
#' )
#'
#' @import cli
#' @export
boilerplate_report_measures <- function(all_vars = NULL,
                                        baseline_vars = NULL,
                                        exposure_var = NULL,
                                        outcome_vars = NULL,
                                        measure_data,
                                        custom_titles = NULL,
                                        print_keywords = FALSE,
                                        print_waves = FALSE) {

  # Helper function to find a measure in the hierarchical structure
  find_measure <- function(db, path) {
    if (!grepl("\\.", path)) {
      # Simple path, just return the entry if it exists
      if (path %in% names(db)) {
        return(db[[path]])
      } else {
        return(NULL)
      }
    }

    # For hierarchical paths
    parts <- strsplit(path, "\\.")[[1]]
    current <- db

    for (part in parts) {
      if (part %in% names(current)) {
        current <- current[[part]]
      } else {
        return(NULL)
      }
    }

    return(current)
  }

  # Generate section for variables
  generate_section <- function(vars, section_title) {
    section <- paste0(cli::col_magenta("### ", section_title, "\n\n"))
    for (var in vars) {
      # Find the measure in the hierarchical structure
      measure_info <- find_measure(measure_data, var)
      if (!is.null(measure_info)) {
        section <- paste0(section, format_measure(var, measure_info))
      } else {
        warning(paste("No information available for variable:", var))
      }
    }
    return(section)
  }

  # Input validation and processing
  if (is.null(all_vars) && is.null(baseline_vars) && is.null(exposure_var) && is.null(outcome_vars)) {
    # If no variables are specified, use all top-level variables from measure_data
    all_vars <- names(measure_data)
  } else if (!is.null(all_vars)) {
    if (is.logical(all_vars) && all_vars) {
      all_vars <- unique(c(baseline_vars, exposure_var, outcome_vars))
    } else if (!is.character(all_vars)) {
      stop("all_vars must be either TRUE or a character vector")
    }
  } else {
    # Check if only one of baseline_vars, exposure_var, or outcome_vars is provided
    provided_vars <- c(!is.null(baseline_vars), !is.null(exposure_var), !is.null(outcome_vars))
    if (sum(provided_vars) == 1) {
      if (!is.null(baseline_vars)) {
        all_vars <- baseline_vars
      } else if (!is.null(exposure_var)) {
        all_vars <- exposure_var
      } else {
        all_vars <- outcome_vars
      }
    } else {
      all_vars <- unique(c(baseline_vars, exposure_var, outcome_vars))
    }
  }

  if (!is.list(measure_data)) {
    stop("measure_data must be a list")
  }

  if (!is.null(custom_titles) && (!is.list(custom_titles) || is.null(names(custom_titles)))) {
    stop("custom_titles must be a named list or NULL")
  }

  format_measure <- function(var_name, measure_info) {
    if (is.null(measure_info)) {
      warning(paste("No information available for variable:", var_name))
      return(paste0("#### ", janitor::make_clean_names(var_name, case = "title"), "\n\nNo information available for this variable.\n\n"))
    }

    # Skip if this is a subfolder/category rather than an actual measure
    if (is.list(measure_info) && !any(c("description", "reference", "items") %in% names(measure_info))) {
      return("")
    }

    if (!all(c("description", "reference") %in% names(measure_info))) {
      warning(paste("Measure info for", var_name, "is missing 'description' or 'reference'"))
      missing_fields <- setdiff(c("description", "reference"), names(measure_info))
      for (field in missing_fields) {
        measure_info[[field]] <- "Not provided"
      }
    }

    # Extract the base name (after last dot) for display
    display_name <- if (grepl("\\.", var_name)) {
      tail(strsplit(var_name, "\\.")[[1]], 1)
    } else {
      var_name
    }

    # Use custom title if provided, otherwise use clean name
    if (!is.null(custom_titles) && var_name %in% names(custom_titles)) {
      title <- custom_titles[[var_name]]
    } else {
      title <- janitor::make_clean_names(display_name, case = "title")
    }

    # Add variable type indicator, removing redundancy for binary variables
    if (endsWith(display_name, "_binary")) {
      title <- gsub(" Binary$", "", title)  # Remove " Binary" if it's at the end of the title
      title <- paste0(title, " (Binary)")
    } else if (endsWith(display_name, "_cat")) {
      title <- paste0(title, " (Categorical)")
    }

    # Include wave information if available and print_waves is TRUE
    if (print_waves && "waves" %in% names(measure_info)) {
      title <- paste0(title, " (waves: ", measure_info$waves, ")")
    }

    formatted_text <- paste0(cli::col_cyan("#### ", title, "\n\n"))

    # Print items first
    if ("items" %in% names(measure_info)) {
      if (length(measure_info$items) == 1) {
        formatted_text <- paste0(
          formatted_text,
          cli::col_yellow("*", measure_info$items[1], "*\n\n")
        )
      } else {
        for (item in measure_info$items) {
          formatted_text <- paste0(
            formatted_text,
            cli::col_yellow("*", item, "*\n")
          )
        }
        formatted_text <- paste0(formatted_text, "\n")
      }
    }
    # Combine description and reference
    description <- trimws(measure_info$description)
    reference <- measure_info$reference

    # Check if the reference is a 'string_is' type
    if (grepl("^string_is\\s+", reference)) {
      string_content <- sub("^string_is\\s+", "", reference)
      string_content <- gsub("^[\"']|[\"']$", "", string_content)
      description_with_ref <- paste0(description, ". ", string_content)
      if (!grepl("[.!?]$", description_with_ref)) {
        description_with_ref <- paste0(description_with_ref, ".")
      }
    } else {
      if (substr(description, nchar(description), nchar(description)) %in% c(".", "!", "?")) {
        description_with_ref <- paste0(
          substr(description, 1, nchar(description) - 1),
          " [@",
          reference,
          "]",
          substr(description, nchar(description), nchar(description))
        )
      } else {
        description_with_ref <- paste0(description, " [@", reference, "].")
      }
    }

    formatted_text <- paste0(formatted_text, cli::col_yellow(description_with_ref), "\n\n")

    if (print_keywords && "keywords" %in% names(measure_info) && length(measure_info$keywords) > 0) {
      formatted_text <- paste0(
        formatted_text,
        cli::col_green("Keywords: ", paste(measure_info$keywords, collapse = ", "), "\n\n")
      )
    }

    return(formatted_text)
  }


  # Generate main section with all variables
  main_section <- generate_section(all_vars, "All Measures")

  # Generate keywords section
  keywords_section <- ""
  if (print_keywords) {
    all_keywords <- unique(unlist(lapply(all_vars, function(var) {
      measure_info <- find_measure(measure_data, var)
      if (!is.null(measure_info) && "keywords" %in% names(measure_info)) {
        return(measure_info$keywords)
      }
      return(NULL)
    })))

    if (length(all_keywords) > 0) {
      keywords_section <- paste0(
        cli::col_magenta("### Keywords Used\n\n"),
        cli::col_green(paste(sort(all_keywords), collapse = ", ")),
        "\n\n"
      )
    }
  }

  # Combine all sections
  full_appendix <- paste0(
    cli::col_blue("## Appendix: Measures\n\n"),
    main_section,
    keywords_section
  )

  # Print the colored output to the console
  cat(full_appendix)

  # success message
  cli::cli_alert_success("Finished creating bibliography \U0001F44D")

  # Return the uncoloured markdown text
  return(gsub("\033\\[[0-9;]*m", "", full_appendix))
}
