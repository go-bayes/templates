#' Manage Boilerplate Text
#'
#' This function provides a unified interface for managing boilerplate text
#' across different categories (measures, methods, etc.). It allows listing,
#' adding, updating, removing, and retrieving text entries with template
#' variable substitution. Supports arbitrarily nested hierarchical organization
#' through dot-separated paths.
#'
#' @param category Character. Category of text to manage, e.g., "measures", "methods".
#'   Default options are "measures", "methods", "results", and "discussion".
#' @param action Character. Action to perform: "list", "add", "update", "remove", "get", or "save".
#' @param name Character. Identifier for the text entry (required for add/update/remove/get).
#'   For nested entries, use dot-separated paths (e.g., "methods.statistical.lmtp").
#' @param value Text content for the entry (required for add/update).
#' @param db Optional existing database. If not supplied, the default db is loaded.
#' @param text_path Character. Path to the directory where text database files are stored.
#'   If NULL (default), the function will look in the following locations in order:
#'   1. "boilerplate/data/" subdirectory of the current working directory (via here::here())
#'   2. Package installation directory's "boilerplate/data/" folder
#'   3. "boilerplate/data/" relative to the current working directory
#' @param file_name Character. Name of the file to save or load (without path).
#'   If NULL (default), uses "[category]_db.rds".
#' @param template_vars List. Optional variables for template substitution.
#' @param warn_missing Logical. Whether to warn about missing template variables. Default is TRUE.
#' @param auto_sort Logical. Whether to automatically sort the database alphabetically
#'   after add/update operations. Default is TRUE.
#'
#' @return Depending on action:
#'   - "list": Returns the database or part of it
#'   - "add"/"update"/"remove": Returns the modified database
#'   - "get": Returns a specific text entry, with template variables substituted
#'   - "save": Returns TRUE if successful, FALSE otherwise
#'
#' @examples
#' # List default methods
#' methods_db <- boilerplate_manage_text(category = "methods", action = "list")
#'
#' # Add a new method entry
#' methods_db <- boilerplate_manage_text(
#'   category = "methods",
#'   action = "add",
#'   name = "causal_assumptions",
#'   value = "We made the following assumptions: {{assumptions}}"
#' )
#'
#' # Add a nested entry
#' methods_db <- boilerplate_manage_text(
#'   category = "methods",
#'   action = "add",
#'   name = "statistical.longitudinal.lmtp",
#'   value = "We used the LMTP estimator with {{software}}."
#' )
#'
#' # Get a specific entry with template substitution
#' text <- boilerplate_manage_text(
#'   category = "methods",
#'   action = "get",
#'   name = "causal_assumptions",
#'   db = methods_db,
#'   template_vars = list(assumptions = "no unmeasured confounding")
#' )
#'
#' # Get a nested entry with template substitution
#' text <- boilerplate_manage_text(
#'   category = "methods",
#'   action = "get",
#'   name = "statistical.longitudinal.lmtp",
#'   db = methods_db,
#'   template_vars = list(software = "lmtp R package")
#' )
#'
#' # Update an existing entry
#' methods_db <- boilerplate_manage_text(
#'   category = "methods",
#'   action = "update",
#'   name = "causal_assumptions",
#'   value = "Updated assumptions: {{assumptions}}",
#'   db = methods_db
#' )
#'
#' # Save the database to a specific location
#' boilerplate_manage_text(
#'   category = "methods",
#'   action = "save",
#'   db = methods_db,
#'   text_path = "path/to/data",
#'   file_name = "my_methods.rds"
#' )
#'
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom stringr str_extract_all
#' @importFrom tools toTitleCase
#' @importFrom utils modifyList
#' @export
boilerplate_manage_text <- function(
    category = c("measures", "methods", "results", "discussion"),
    action = c("list", "add", "update", "remove", "get", "save"),
    name = NULL,
    value = NULL,
    db = NULL,
    text_path = NULL,
    file_name = NULL,
    template_vars = list(),
    warn_missing = TRUE,
    auto_sort = TRUE  # Default set to TRUE
) {
  # Validate inputs
  category <- match.arg(category)
  action <- match.arg(action)

  # validate name for actions that require it
  if (action %in% c("add", "update", "remove", "get") && is.null(name)) {
    stop(paste("The", action, "action requires a name parameter."))
  }

  # validate value for actions that require it
  if (action %in% c("add", "update") && is.null(value)) {
    stop(paste("The", action, "action requires a value parameter."))
  }

  # load default database if not provided
  if (is.null(db)) {
    db <- load_text_db(category, text_path, file_name)
  }

  # input validation: ensure db is a list
  if (!is.list(db)) {
    stop("Database must be a list.")
  }

  # handle path-based navigation or simple name
  if (!is.null(name) && grepl("\\.", name)) {
    # We have a nested path
    path_parts <- strsplit(name, "\\.")[[1]]

    if (action == "list") {
      # For list action, navigate to the requested folder
      return(get_nested_folder(db, path_parts))
    } else if (action == "get") {
      # For get action, retrieve and process the text
      return(get_nested_text(db, path_parts, template_vars, warn_missing))
    } else if (action %in% c("add", "update", "remove")) {
      # For modification actions, update the nested structure
      db <- modify_nested_entry(db, path_parts, action, value, auto_sort)

      # Sort the entire database if auto_sort is TRUE
      if (auto_sort && action %in% c("add", "update")) {
        db <- sort_text_db(db)
      }

      return(db)
    }
  } else {
    # handle regular key-based operations
    if (action == "list") {
      if (!is.null(name)) {
        # List a specific top-level entry
        if (!(name %in% names(db))) {
          stop(paste("Text entry", name, "not found."))
        }
        return(db[[name]])
      }
      return(db)
    } else if (action == "add") {
      if (name %in% names(db)) {
        stop("text entry already exists")
      }
      db[[name]] <- value

      # Sort the database if auto_sort is TRUE
      if (auto_sort) {
        db <- db[order(names(db))]
      }
    } else if (action == "update") {
      if (!(name %in% names(db))) {
        stop("text entry does not exist")
      }
      db[[name]] <- value

      # Sort the database if auto_sort is TRUE
      if (auto_sort) {
        db <- db[order(names(db))]
      }
    } else if (action == "remove") {
      if (!(name %in% names(db))) {
        stop("text entry does not exist")
      }
      db[[name]] <- NULL
    } else if (action == "get") {
      if (!(name %in% names(db))) {
        stop("text entry does not exist")
      }
      text <- db[[name]]
      # Apply template substitution
      return(apply_template_vars(text, template_vars, warn_missing))
    } else if (action == "save") {
      return(save_text_db(db, category, text_path, file_name))
    }
  }

  return(db)
}

#' Generate Text from Boilerplate
#'
#' This function generates text by retrieving and combining text from
#' a boilerplate database. It allows for template variable substitution and
#' customization through overrides. Supports arbitrarily nested section paths
#' using dot notation.
#'
#' @param category Character. Category of text to generate.
#' @param sections Character vector. The sections to include (can use dot notation for nesting).
#' @param global_vars List. Variables available to all sections.
#' @param section_vars List. Section-specific variables.
#' @param text_overrides List. Direct text overrides for specific sections.
#' @param db List. Optional database to use.
#' @param text_path Character. Path to the directory where text database files are stored.
#'   If NULL (default), the function will look in the following locations in order:
#'   1. "boilerplate/data/" subdirectory of the current working directory (via here::here())
#'   2. Package installation directory's "boilerplate/data/" folder
#'   3. "boilerplate/data/" relative to the current working directory
#' @param warn_missing Logical. Whether to warn about missing template variables.
#' @param add_headings Logical. Whether to add markdown headings to sections. Default is FALSE.
#' @param heading_level Character. The heading level to use (e.g., "###"). Default is "###".
#' @param custom_headings List. Custom headings for specific sections. Names should match section names.
#'
#' @return Character. The combined text with optional headings.
#'
#' @examples
#' \dontrun{
#' # Basic usage with methods sections
#' methods_text <- boilerplate_generate_text(
#'   category = "methods",
#'   sections = c("sample", "causal_assumptions.identification"),
#'   global_vars = list(
#'     exposure_var = "political_conservative",
#'     population = "university students"
#'   )
#' )
#'
#' # Using deeply nested paths with headings
#' methods_text <- boilerplate_generate_text(
#'   category = "methods",
#'   sections = c(
#'     "sample",
#'     "statistical.longitudinal.lmtp",
#'     "statistical.heterogeneity.grf.custom"
#'   ),
#'   global_vars = list(exposure_var = "treatment"),
#'   text_path = "path/to/project/data",
#'   add_headings = TRUE,
#'   heading_level = "###"
#' )
#' }
#'
#' @importFrom tools toTitleCase
#' @export
boilerplate_generate_text <- function(
    category = c("measures", "methods", "results", "discussion"),
    sections,
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
  # Input validation
  category <- match.arg(category)

  # If category is "methods", use singular form for main heading
  category_title <- ifelse(category == "methods", "Method", tools::toTitleCase(category))

  # Load database if not provided
  if (is.null(db)) {
    db <- boilerplate_manage_text(
      category = category,
      action = "list",
      text_path = text_path
    )
  }

  # Initialize result
  result <- character(0)

  # Process each section for text generation with arbitrary nesting
  for (section in sections) {
    # Determine section title
    section_parts <- strsplit(section, "\\.")[[1]]
    section_name <- section_parts[length(section_parts)]

    # Create heading text
    if (add_headings) {
      # Check if there's a custom heading for this section
      if (section %in% names(custom_headings)) {
        heading_text <- paste0(heading_level, " ", custom_headings[[section]])
      } else {
        # Use the last part of the section path and convert to title case
        heading_text <- paste0(heading_level, " ", tools::toTitleCase(gsub("_", " ", section_name)))
      }
    }

    # Check for text override
    if (section %in% names(text_overrides)) {
      section_text <- text_overrides[[section]]
      if (add_headings) {
        section_text <- paste(heading_text, section_text, sep = "\n\n")
      }
      result <- c(result, section_text)
      next
    }

    # Merge global and section-specific variables
    vars <- global_vars
    if (section %in% names(section_vars)) {
      vars <- c(vars, section_vars[[section]])
    }

    # Attempt to retrieve text
    section_text <- tryCatch({
      boilerplate_manage_text(
        category = category,
        action = "get",
        name = section,
        db = db,
        template_vars = vars,
        warn_missing = warn_missing
      )
    }, error = function(e) {
      warning(paste("Error retrieving section", section, ":", e$message))
      return(NULL)
    })

    if (!is.null(section_text) && is.character(section_text)) {
      if (add_headings) {
        section_text <- paste(heading_text, section_text, sep = "\n\n")
      }
      result <- c(result, section_text)
    }
  }

  # Combine all sections
  return(paste(result, collapse = "\n\n"))
}

# Helper functions for text management

#' Sort Text Database
#'
#' This function recursively sorts a text database alphabetically at each level.
#'
#' @param db A text database (list).
#' @return The same database with all levels sorted alphabetically.
#'
#' @noRd
sort_text_db <- function(db) {
  if (!is.list(db)) {
    return(db)
  }

  # Sort the top level
  sorted_names <- sort(names(db))
  sorted_db <- db[sorted_names]

  # Recursively sort any nested structures
  for (name in sorted_names) {
    if (is.list(sorted_db[[name]])) {
      # This is a folder/category, sort it recursively
      sorted_db[[name]] <- sort_text_db(sorted_db[[name]])
    }
  }

  return(sorted_db)
}

#' Modify a Nested Entry in the Database
#'
#' Recursively navigates a nested list structure to add, update, or remove an entry.
#'
#' @param db List. The database to modify.
#' @param path_parts Character vector. Path components.
#' @param action Character. The action to perform ("add", "update", or "remove").
#' @param value Any. The value to set (for add or update).
#' @param auto_sort Logical. Whether to automatically sort at each level.
#'
#' @return The modified database.
#'
#' @noRd
modify_nested_entry <- function(db, path_parts, action, value = NULL, auto_sort = TRUE) {
  if (length(path_parts) == 0) {
    stop("Empty path")
  }

  current_part <- path_parts[1]
  remaining_parts <- path_parts[-1]

  # When adding, create missing folders as needed
  if (action == "add" && !(current_part %in% names(db))) {
    if (length(remaining_parts) > 0) {
      # Create folder for intermediate path
      db[[current_part]] <- list()
    } else {
      # Add leaf value
      db[[current_part]] <- value

      # Sort after adding if auto_sort is TRUE
      if (auto_sort) {
        db <- db[order(names(db))]
      }

      return(db)
    }
  } else if (action != "add" && !(current_part %in% names(db))) {
    stop(paste("Path component", current_part, "not found"))
  }

  if (length(remaining_parts) == 0) {
    # We've reached the leaf node
    if (action == "add") {
      if (current_part %in% names(db)) {
        stop(paste("Text entry", current_part, "already exists"))
      }
      db[[current_part]] <- value

      # Sort after adding if auto_sort is TRUE
      if (auto_sort) {
        db <- db[order(names(db))]
      }
    } else if (action == "update") {
      db[[current_part]] <- value

      # Sort after updating if auto_sort is TRUE
      if (auto_sort) {
        db <- db[order(names(db))]
      }
    } else if (action == "remove") {
      db[[current_part]] <- NULL
    }
  } else {
    # Continue navigation
    current_item <- db[[current_part]]

    if (!is.list(current_item)) {
      stop(paste("Path component", current_part, "is not a folder"))
    }

    # Recursively modify the nested structure
    db[[current_part]] <- modify_nested_entry(current_item, remaining_parts, action, value, auto_sort)

    # Sort the current level after modifying deeper levels if auto_sort is TRUE
    if (auto_sort && action %in% c("add", "update")) {
      db <- db[order(names(db))]
    }
  }

  return(db)
}

#' Retrieve a Nested Folder from the Database
#'
#' Navigates through a nested list structure to retrieve a specific folder.
#'
#' @param db List. The database to navigate.
#' @param path_parts Character vector. Path components.
#'
#' @return The nested list at the specified path.
#'
#' @noRd
get_nested_folder <- function(db, path_parts) {
  if (length(path_parts) == 0) {
    return(db)
  }

  current_part <- path_parts[1]
  remaining_parts <- path_parts[-1]

  if (!(current_part %in% names(db))) {
    stop(paste("Path component", current_part, "not found"))
  }

  current_item <- db[[current_part]]

  if (!is.list(current_item)) {
    stop(paste("Path component", current_part, "is not a folder"))
  }

  if (length(remaining_parts) == 0) {
    return(current_item)
  } else {
    return(get_nested_folder(current_item, remaining_parts))
  }
}

#' Retrieve Text from a Nested Structure
#'
#' This helper function recursively navigates a nested list structure
#' to retrieve text at any depth of nesting.
#'
#' @param db The database or sublist to search within
#' @param path_parts Character vector of path components
#' @param template_vars Variables for template substitution
#' @param warn_missing Whether to warn about missing template variables
#'
#' @return Retrieved text with template variables substituted
#'
#' @noRd
get_nested_text <- function(db, path_parts, template_vars = list(), warn_missing = TRUE) {
  if (length(path_parts) == 0) {
    stop("Empty path")
  }

  current_part <- path_parts[1]
  remaining_parts <- path_parts[-1]

  if (!(current_part %in% names(db))) {
    stop(paste("Path component", current_part, "not found"))
  }

  current_item <- db[[current_part]]

  if (length(remaining_parts) == 0) {
    # We've reached the leaf node
    if (is.list(current_item)) {
      # If this is a folder, look for a default entry
      if ("default" %in% names(current_item)) {
        current_item <- current_item[["default"]]
      } else {
        stop(paste("Path component", current_part, "is a folder without a default entry"))
      }
    }

    # Apply template substitution and return
    return(apply_template_vars(current_item, template_vars, warn_missing))
  } else {
    # Keep navigating deeper
    if (!is.list(current_item)) {
      stop(paste("Path component", current_part, "is not a folder"))
    }

    return(get_nested_text(current_item, remaining_parts, template_vars, warn_missing))
  }
}

#' Apply Template Variables to Text
#'
#' Substitutes template variables in text using the {{variable_name}} syntax.
#' Uses glue package for robust variable replacement, with a fallback method.
#' Can optionally warn about missing variables.
#'
#' @param text Character. The template text with placeholders.
#' @param template_vars List. Variables to substitute in the template.
#' @param warn_missing Logical. Whether to warn about missing template variables.
#'
#' @return Character. The text with variables substituted.
#'
#' @importFrom glue glue
#' @noRd
apply_template_vars <- function(text, template_vars = list(), warn_missing = TRUE) {
  # Return early for non-character text or empty variables
  if (length(template_vars) == 0 || !is.character(text)) {
    return(text)
  }

  # For vector values, convert to comma-separated strings
  template_vars_processed <- template_vars
  for (var_name in names(template_vars)) {
    var_value <- template_vars[[var_name]]
    if (length(var_value) > 1) {
      template_vars_processed[[var_name]] <- paste(as.character(var_value), collapse = ", ")
    }
  }

  # Prepare the environment for glue
  env <- list2env(template_vars_processed, parent = emptyenv())

  # Try to use glue for substitution
  tryCatch({
    # Convert {{var}} syntax to glue's {var} syntax
    glue_ready_text <- gsub("\\{\\{([^\\}]+)\\}\\}", "{\\1}", text)
    result <- glue::glue(glue_ready_text, .envir = env)
    return(as.character(result))
  }, error = function(e) {
    # Fallback to manual substitution if glue fails
    for (var_name in names(template_vars_processed)) {
      var_value <- template_vars_processed[[var_name]]
      if (is.character(var_value) || is.numeric(var_value)) {
        text <- gsub(
          paste0("\\{\\{", var_name, "\\}\\}"),
          as.character(var_value),
          text,
          fixed = FALSE
        )
      }
    }

    # Issue a warning for unresolved variables if requested
    if (warn_missing) {
      # Look for any remaining {{variable}} patterns
      var_pattern <- "\\{\\{([^\\}]+)\\}\\}"
      if (requireNamespace("stringr", quietly = TRUE)) {
        # Use stringr if available
        remaining_vars <- stringr::str_extract_all(text, var_pattern)
        if (length(remaining_vars[[1]]) > 0) {
          remaining_vars <- unique(gsub("\\{\\{|\\}\\}", "", remaining_vars[[1]]))
          warning(paste("Unresolved template variables:", paste(remaining_vars, collapse = ", ")))
        }
      } else {
        # Fallback to base R
        if (grepl(var_pattern, text)) {
          matches <- gregexpr(var_pattern, text)
          if (matches[[1]][1] != -1) {
            warning("Unresolved template variables present. Consider installing the 'stringr' package for detailed information.")
          }
        }
      }
    }

    return(text)
  })
}

#' Load Text Database from File
#'
#' @param category Character. Category of text to load.
#' @param text_path Character. Path to the directory where text database files are stored.
#'   If NULL, uses default locations.
#' @param file_name Character. Name of the file to load (without path).
#'   If NULL, uses "[category]_db.rds".
#'
#' @return List. The loaded text database or default if not found.
#'
#' @noRd
load_text_db <- function(category, text_path = NULL, file_name = NULL) {
  # determine file path
  file_path <- get_text_file_path(category, text_path, file_name)

  # check if file exists
  if (file.exists(file_path)) {
    tryCatch({
      db <- readRDS(file_path)
      message(paste(category, "text database loaded from", file_path))
      return(db)
    }, error = function(e) {
      warning(paste("Error loading database:", e$message))
      # fall back to defaults if loading fails
    })
  }

  # return built-in defaults if file doesn't exist or couldn't be loaded
  return(get_default_db(category))
}

#' Save Text Database to File
#'
#' @param db List. The database to save.
#' @param category Character. Category of text to save.
#' @param text_path Character. Path to the directory where text database files are stored.
#'   If NULL, uses the "boilerplate/data/" subdirectory of the current working directory.
#' @param file_name Character. Name of the file to save (without path).
#'   If NULL, uses "[category]_db.rds".
#'
#' @return Logical. TRUE if successful, FALSE otherwise.
#'
#' @noRd
save_text_db <- function(db, category, text_path = NULL, file_name = NULL) {
  # determine file path
  file_path <- get_text_file_path(category, text_path, file_name)

  # ensure directory exists
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message(paste("Created directory:", dir_path))
  }

  # check if file exists and warn
  if (file.exists(file_path)) {
    warning(paste("Overwriting existing", category, "database file:", file_path))
  }

  # save database
  tryCatch({
    saveRDS(db, file = file_path)
    message(paste(category, "text database saved to", file_path))
    return(TRUE)
  }, error = function(e) {
    warning(paste("Error saving database:", e$message))
    return(FALSE)
  })
}

#' Get File Path for Text Database
#'
#' Constructs the file path for the text database based on the provided parameters.
#'
#' @param category Character. Category of text.
#' @param text_path Character. Path to the directory where text database files are stored.
#'   If NULL, uses the "boilerplate/data/" subdirectory of the current working directory
#'   via the here::here() function.
#' @param file_name Character. Name of the file (without path).
#'   If NULL, uses "[category]_db.rds".
#'
#' @return Character. The file path for the text database.
#'
#' @noRd
get_text_file_path <- function(category, text_path = NULL, file_name = NULL) {
  # default file name
  if (is.null(file_name)) {
    file_name <- paste0(category, "_db.rds")
  }

  # determine directory path
  if (is.null(text_path)) {
    if (!requireNamespace("here", quietly = TRUE)) {
      stop("Package 'here' is required for default path resolution. Please install it or specify 'text_path' manually.")
    }
    dir_path <- here::here("boilerplate", "data")

    # Create directory if it doesn't exist
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      message(paste("Created directory:", dir_path))
    }
  } else {
    dir_path <- text_path
  }

  # combine directory and file name
  return(file.path(dir_path, file_name))
}

#' Get Default Database for a Category
#'
#' @param category Character. Category to get defaults for.
#'
#' @return List. The default database for the category.
#'
#' @noRd
get_default_db <- function(category) {
  # return built-in defaults based on category
  if (category == "measures") {
    return(get_default_measures_db())
  } else if (category == "methods") {
    return(get_default_methods_db())
  } else if (category == "results") {
    return(get_default_results_db())
  } else if (category == "discussion") {
    return(get_default_discussion_db())
  }

  # Default to empty database for unknown categories
  warning(paste("Unknown category:", category))
  return(list())
}

