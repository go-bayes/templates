#' Enhanced Manage Boilerplate Measures Database with Auto-Sorting
#'
#' A light, enhanced version of boilerplate_manage_text specifically for measures that automatically
#' sorts measures alphabetically after adding or updating them.
#'
#' @param action Character. Action to perform: "list", "add", "update", "remove", "get", or "save".
#' @param name Character. Name of the measure (required for add/update/remove/get).
#'   For nested entries, use a dot-separated path (e.g., "psychological.anxiety").
#' @param measure List. Measure details (required for add/update). Should contain elements
#'   like "description", "reference", "waves", "keywords", and "items".
#' @param db Optional existing measures database. If not supplied, the default db is loaded.
#' @param measures_path Character. Path to the directory where measure database files are stored.
#'   If NULL (default), the function will look in the following locations in order:
#'   1. "boilerplate/data/" subdirectory of the current working directory (via here::here())
#'   2. Package installation directory's "boilerplate/data/" folder
#'   3. "boilerplate/data/" relative to the current working directory
#' @param file_name Character. Name of the file to save or load (without path).
#'   If NULL (default), uses "measures_db.rds".
#' @param auto_sort Logical. Whether to automatically sort the database alphabetically
#'   after add/update operations. Default is TRUE.
#'
#' @return Depending on action:
#'   - "list": Returns the database or part of it
#'   - "add"/"update"/"remove": Returns the modified database
#'   - "get": Returns a specific measure
#'   - "save": Returns TRUE if successful, FALSE otherwise
#'
#' @examples
#' # List all measures
#' measures_db <- boilerplate_manage_measures(action = "list")
#'
#' # Add a new measure with automatic sorting
#' measures_db <- boilerplate_manage_measures(
#'   action = "add",
#'   name = "alcohol_frequency",
#'   measure = list(
#'     description = "Frequency of alcohol consumption was measured using a single item.",
#'     reference = "nzavs2009",
#'     waves = "1-current",
#'     keywords = c("alcohol", "frequency", "consumption"),
#'     items = list("How often do you have a drink containing alcohol?")
#'   )
#' )
#'
#' @importFrom stats setNames
#' @importFrom utils modifyList
#' @importFrom here here
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_danger
#' @export
boilerplate_manage_measures <- function(
    action = c("list", "add", "update", "remove", "get", "save"),
    name = NULL,
    measure = NULL,
    db = NULL,
    measures_path = NULL,
    file_name = NULL,
    auto_sort = TRUE
) {
  # validate inputs
  action <- match.arg(action)

  # validate name for actions that require it
  if (action %in% c("add", "update", "remove", "get") && is.null(name)) {
    stop(paste("The", action, "action requires a name parameter."))
  }

  # validate measure for actions that require it
  if (action %in% c("add", "update") && is.null(measure)) {
    stop(paste("The", action, "action requires a measure parameter."))
  }

  # load default database if not provided
  if (is.null(db)) {
    db <- load_measures_db(measures_path, file_name)
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
      # For get action, retrieve the measure
      return(get_nested_measure(db, path_parts))
    } else if (action %in% c("add", "update", "remove")) {
      # For modification actions, update the nested structure
      db <- modify_nested_measure(db, path_parts, action, measure, auto_sort)

      # Sort the entire database if auto_sort is TRUE
      if (auto_sort && action %in% c("add", "update")) {
        db <- sort_measures_db(db)
      }

      return(db)
    }
  } else {
    # handle regular key-based operations
    if (action == "list") {
      if (!is.null(name)) {
        # List a specific top-level measure
        if (!(name %in% names(db))) {
          stop(paste("Measure", name, "not found."))
        }
        return(db[[name]])
      }
      return(db)
    } else if (action == "add") {
      if (name %in% names(db)) {
        stop("measure already exists")
      }
      db[[name]] <- measure

      # Sort the database if auto_sort is TRUE
      if (auto_sort) {
        db <- db[order(names(db))]
      }
    } else if (action == "update") {
      if (!(name %in% names(db))) {
        stop("measure does not exist")
      }
      db[[name]] <- measure

      # Sort the database if auto_sort is TRUE
      if (auto_sort) {
        db <- db[order(names(db))]
      }
    } else if (action == "remove") {
      if (!(name %in% names(db))) {
        stop("measure does not exist")
      }
      db[[name]] <- NULL
    } else if (action == "get") {
      if (!(name %in% names(db))) {
        stop("measure does not exist")
      }
      return(db[[name]])
    } else if (action == "save") {
      return(save_measures_db(db, measures_path, file_name))
    }
  }

  return(db)
}

#' Modify a Nested Measure in the Database
#'
#' Recursively navigates a nested list structure to add, update, or remove a measure,
#' with optional automatic sorting at each level.
#'
#' @param db List. The database to modify.
#' @param path_parts Character vector. Path components.
#' @param action Character. The action to perform ("add", "update", or "remove").
#' @param measure List. The measure details to set (for add or update).
#' @param auto_sort Logical. Whether to automatically sort at each level.
#'
#' @return The modified database.
#'
#' @noRd
modify_nested_measure <- function(db, path_parts, action, measure = NULL, auto_sort = TRUE) {
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
      db[[current_part]] <- measure

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
        stop(paste("Measure", current_part, "already exists"))
      }
      db[[current_part]] <- measure

      # Sort after adding if auto_sort is TRUE
      if (auto_sort) {
        db <- db[order(names(db))]
      }
    } else if (action == "update") {
      db[[current_part]] <- measure

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
    db[[current_part]] <- modify_nested_measure(
      current_item, remaining_parts, action, measure, auto_sort
    )

    # Sort the current level after modifying deeper levels if auto_sort is TRUE
    if (auto_sort && action %in% c("add", "update")) {
      db <- db[order(names(db))]
    }
  }

  return(db)
}

#' Sort Measures Database
#'
#' This function recursively sorts a measures database alphabetically at each level.
#'
#' @param db A measures database (list).
#' @return The same database with all levels sorted alphabetically.
#'
#' @noRd
sort_measures_db <- function(db) {
  if (!is.list(db)) {
    return(db)
  }

  # Helper function to check if an entry is a measure or a folder
  is_measure <- function(entry) {
    if (!is.list(entry)) return(TRUE)
    # If it has description or items, it's likely a measure
    if (any(c("description", "items", "reference") %in% names(entry))) {
      return(TRUE)
    }
    # Otherwise, it's probably a folder/category
    return(FALSE)
  }

  # Sort the top level first
  sorted_names <- sort(names(db))
  sorted_db <- db[sorted_names]

  # Recursively sort any nested structures
  for (name in sorted_names) {
    if (is.list(sorted_db[[name]]) && !is_measure(sorted_db[[name]])) {
      # This is a category/folder, sort it recursively
      sorted_db[[name]] <- sort_measures_db(sorted_db[[name]])
    }
  }

  return(sorted_db)
}

#' Load Measures Database from File
#'
#' @param measures_path Character. Path to the directory where measure database files are stored.
#'   If NULL, uses default locations.
#' @param file_name Character. Name of the file to load (without path).
#'   If NULL, uses "measures_db.rds".
#'
#' @return List. The loaded measures database or default if not found.
#'
#' @noRd
load_measures_db <- function(measures_path = NULL, file_name = NULL) {
  # determine file path
  file_path <- get_measures_file_path(measures_path, file_name)

  # check if file exists
  if (file.exists(file_path)) {
    tryCatch({
      db <- readRDS(file_path)
      message(paste("Measures database loaded from", file_path))
      return(db)
    }, error = function(e) {
      warning(paste("Error loading database:", e$message))
      # fall back to defaults if loading fails
    })
  }

  # return built-in defaults if file doesn't exist or couldn't be loaded
  return(get_default_measures_db())
}

#' Save Measures Database to File
#'
#' @param db List. The database to save.
#' @param measures_path Character. Path to the directory where measure database files are stored.
#'   If NULL, uses the "boilerplate/data/" subdirectory of the current working directory.
#' @param file_name Character. Name of the file to save (without path).
#'   If NULL, uses "measures_db.rds".
#'
#' @return Logical. TRUE if successful, FALSE otherwise.
#'
#' @noRd
save_measures_db <- function(db, measures_path = NULL, file_name = NULL) {
  # determine file path
  file_path <- get_measures_file_path(measures_path, file_name)

  # ensure directory exists
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message(paste("Created directory:", dir_path))
  }

  # check if file exists and warn
  if (file.exists(file_path)) {
    warning(paste("Overwriting existing measures database file:", file_path))
  }

  # save database
  tryCatch({
    saveRDS(db, file = file_path)
    message(paste("Measures database saved to", file_path))
    return(TRUE)
  }, error = function(e) {
    warning(paste("Error saving database:", e$message))
    return(FALSE)
  })
}

#' Get File Path for Measures Database
#'
#' Constructs the file path for the measures database based on the provided parameters.
#'
#' @param measures_path Character. Path to the directory where measure database files are stored.
#'   If NULL, uses the "boilerplate/data/" subdirectory of the current working directory
#'   via the here::here() function.
#' @param file_name Character. Name of the file (without path).
#'   If NULL, uses "measures_db.rds".
#'
#' @return Character. The file path for the measures database.
#'
#' @noRd
get_measures_file_path <- function(measures_path = NULL, file_name = NULL) {
  # default file name
  if (is.null(file_name)) {
    file_name <- "measures_db.rds"
  }

  # determine directory path
  if (is.null(measures_path)) {
    if (!requireNamespace("here", quietly = TRUE)) {
      stop("Package 'here' is required for default path resolution. Please install it or specify 'measures_path' manually.")
    }
    dir_path <- here::here("boilerplate", "data")

    # Create directory if it doesn't exist
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      message(paste("Created directory:", dir_path))
    }
  } else {
    dir_path <- measures_path
  }

  # combine directory and file name
  return(file.path(dir_path, file_name))
}

#' Retrieve a Nested Measure from a Nested Structure
#'
#' This helper function recursively navigates a nested list structure
#' to retrieve a measure at any depth of nesting.
#'
#' @param db The database or sublist to search within
#' @param path_parts Character vector of path components
#'
#' @return Retrieved measure
#'
#' @noRd
get_nested_measure <- function(db, path_parts) {
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
    return(current_item)
  } else {
    # Keep navigating deeper
    if (!is.list(current_item)) {
      stop(paste("Path component", current_part, "is not a folder"))
    }

    return(get_nested_measure(current_item, remaining_parts))
  }
}

