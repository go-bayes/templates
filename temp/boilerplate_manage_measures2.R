#' Manage Boilerplate Measures Database
#'
#' This function provides a unified interface for managing a database of boilerplate
#' measures. It allows users to list, add, update, remove, and retrieve measure entries.
#' Similar to `boilerplate_manage_text()`, it supports hierarchical organization
#' through dot-separated paths.
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
#'
#' @return Depending on action:
#'   - "list": Returns the database or part of it
#'   - "add"/"update"/"remove": Returns the modified database
#'   - "get": Returns a specific measure
#'   - "save": Returns TRUE if successful, FALSE otherwise
#'
#' @examples
#' # List all measures
#' measures_db <- boilerplate_manage_measures2(action = "list")
#'
#' # Add a new measure
#' measures_db <- boilerplate_manage_measures2(
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
#' # Add a nested measure
#' measures_db <- boilerplate_manage_measures2(
#'   action = "add",
#'   name = "psychological.anxiety.generalised",
#'   measure = list(
#'     description = "Generalised anxiety was measured using the GAD-7 scale.",
#'     reference = "spitzer2006",
#'     waves = "4-15",
#'     keywords = c("anxiety", "mental health", "gad"),
#'     items = list(
#'       "Feeling nervous, anxious, or on edge",
#'       "Not being able to stop or control worrying"
#'     )
#'   )
#' )
#'
#' # Get a specific measure
#' measure <- boilerplate_manage_measures2(
#'   action = "get",
#'   name = "alcohol_frequency"
#' )
#'
#' # Update an existing measure
#' measures_db <- boilerplate_manage_measures2(
#'   action = "update",
#'   name = "alcohol_frequency",
#'   measure = list(
#'     description = "Updated description.",
#'     reference = "nzavs2020",
#'     waves = "1-current",
#'     keywords = c("alcohol", "frequency", "consumption"),
#'     items = list("How often do you consume alcoholic beverages?")
#'   )
#' )
#'
#' # Save the database
#' boilerplate_manage_measures2(
#'   action = "save",
#'   db = measures_db,
#'   measures_path = "path/to/data",
#'   file_name = "my_measures.rds"
#' )
#'
#' # Working with a specific database location
#' measures_db <- boilerplate_manage_measures2(
#'   action = "list",
#'   measures_path = "path/to/project/data"
#' )
#'
#' @export
boilerplate_manage_measures2 <- function(
    action = c("list", "add", "update", "remove", "get", "save"),
    name = NULL,
    measure = NULL,
    db = NULL,
    measures_path = NULL,
    file_name = NULL
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
      return(modify_nested_measure(db, path_parts, action, measure))
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
    } else if (action == "update") {
      if (!(name %in% names(db))) {
        stop("measure does not exist")
      }
      db[[name]] <- measure
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

#' Retrieve a Nested Folder from the Measures Database
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

#' Retrieve a Measure from a Nested Structure
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

#' Modify a Nested Measure in the Database
#'
#' Recursively navigates a nested list structure to add, update, or remove a measure.
#'
#' @param db List. The database to modify.
#' @param path_parts Character vector. Path components.
#' @param action Character. The action to perform ("add", "update", or "remove").
#' @param measure List. The measure details to set (for add or update).
#'
#' @return The modified database.
#'
#' @noRd
modify_nested_measure <- function(db, path_parts, action, measure = NULL) {
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
    } else if (action == "update") {
      db[[current_part]] <- measure
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
    db[[current_part]] <- modify_nested_measure(current_item, remaining_parts, action, measure)
  }

  return(db)
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
#'   If NULL, uses default locations.
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
#'   If NULL, uses default locations.
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
    # use appropriate package function for path resolution if available
    if (requireNamespace("here", quietly = TRUE)) {
      dir_path <- here::here("boilerplate", "data")
    } else {
      # fallback
      data_dir <- system.file("boilerplate", "data", package = "boilerplate", mustWork = FALSE)
      if (dir.exists(data_dir)) {
        dir_path <- data_dir
      } else {
        dir_path <- file.path("boilerplate", "data")
      }
    }
  } else {
    dir_path <- measures_path
  }

  # combine directory and file name
  return(file.path(dir_path, file_name))
}

#' Get Default Measures Database
#'
#' @return List. The default measures database with example entries.
#'
#' @noRd
get_default_measures_db <- function() {
  # return built-in defaults (just a minimal set for example)
  default_db <- list(
    # top-level entries
    alcohol_frequency = list(
      description = "Frequency of alcohol consumption was measured using a single item.",
      reference = "nzavs2009",
      waves = "1-current",
      keywords = c("alcohol", "frequency", "consumption"),
      items = list("How often do you have a drink containing alcohol?")
    ),

    # hierarchical organization example
    psychological = list(
      anxiety = list(
        generalised = list(
          description = "Generalised anxiety was measured using the GAD-7 scale.",
          reference = "spitzer2006",
          waves = "4-15",
          keywords = c("anxiety", "mental health", "gad"),
          items = list(
            "Feeling nervous, anxious, or on edge",
            "Not being able to stop or control worrying"
          )
        )
      ),
      depression = list(
        description = "Depression was measured using the PHQ-9 scale.",
        reference = "kroenke2001",
        waves = "4-15",
        keywords = c("depression", "mental health", "phq"),
        items = list(
          "Little interest or pleasure in doing things",
          "Feeling down, depressed, or hopeless"
        )
      )
    )
  )

  # send a message to warn users they're using the default database
  message("Using default measures database with example entries.")
  message("To create a persistent database, use the 'save' action.")

  return(default_db)
}

#' Migrate Measures from Old Format to New Format
#'
#' Converts a measure database from the old format used by boilerplate_manage_measures()
#' to the new format used by boilerplate_manage_measures2().
#'
#' @param old_db A list representing the old measure database.
#' @param categorize Logical. Whether to attempt automatic categorization (default: FALSE).
#' @param categories Named list. Manual category mapping if categorize is TRUE.
#'
#' @return A list representing the new measure database.
#'
#' @examples
#' \dontrun{
#' # Load old database
#' old_db <- readRDS("old_measures_db.rds")
#'
#' # Simple migration without categorization
#' new_db <- migrate_measures_db(old_db)
#'
#' # Migration with automatic categorization
#' new_db <- migrate_measures_db(old_db, categorize = TRUE)
#'
#' # Migration with manual category mapping
#' categories <- list(
#'   psychological = c("anxiety", "depression", "stress"),
#'   social = c("belonging", "trust", "connection")
#' )
#' new_db <- migrate_measures_db(old_db, categorize = TRUE, categories = categories)
#' }
#'
#' @export
migrate_measures_db <- function(old_db, categorize = FALSE, categories = NULL) {
  if (!is.list(old_db)) {
    stop("Old database must be a list.")
  }

  new_db <- list()

  # simple migration (keeping flat structure)
  if (!categorize) {
    return(old_db)
  }

  # categorized migration
  if (is.null(categories)) {
    # default categories based on common keywords
    categories <- list(
      demographic = c("age", "gender", "ethnicity", "income", "education"),
      psychological = c("anxiety", "depression", "stress", "wellbeing", "mental", "psych"),
      physical = c("health", "exercise", "bmi", "weight", "physical", "fitness"),
      social = c("social", "connect", "belong", "relation", "community", "friend"),
      political = c("polit", "vote", "gov", "conserv", "liberal", "party")
    )
  }

  # helper function to determine category based on measure name and keywords
  get_category <- function(measure_name, measure_info) {
    # check for categories based on keywords
    if (!is.null(measure_info$keywords)) {
      for (cat_name in names(categories)) {
        cat_keywords <- categories[[cat_name]]
        for (keyword in measure_info$keywords) {
          if (any(sapply(cat_keywords, function(k) grepl(k, keyword, ignore.case = TRUE)))) {
            return(cat_name)
          }
        }
      }
    }

    # check for categories based on measure name
    for (cat_name in names(categories)) {
      cat_keywords <- categories[[cat_name]]
      if (any(sapply(cat_keywords, function(k) grepl(k, measure_name, ignore.case = TRUE)))) {
        return(cat_name)
      }
    }

    # default category
    return("other")
  }

  # organize measures by category
  for (measure_name in names(old_db)) {
    measure_info <- old_db[[measure_name]]

    if (categorize) {
      category <- get_category(measure_name, measure_info)

      if (!category %in% names(new_db)) {
        new_db[[category]] <- list()
      }

      new_db[[category]][[measure_name]] <- measure_info
    } else {
      new_db[[measure_name]] <- measure_info
    }
  }

  return(new_db)
}

