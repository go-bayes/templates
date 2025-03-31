#' Initialize Boilerplate Text Databases
#'
#' This function initializes or updates the boilerplate text databases with default values.
#' It's useful for setting up the system initially or adding new default entries.
#'
#' @param categories Character vector. Categories to initialize.
#' @param merge_strategy Character. How to merge with existing databases: "keep_existing", "merge_recursive", or "overwrite_all".
#' @param text_path Character. Path to the directory where text database files are stored.
#'   If NULL (default), the function will look in the following locations in order:
#'   1. "boilerplate/data/" subdirectory of the current working directory (via here::here())
#'   2. Package installation directory's "boilerplate/data/" folder
#'   3. "boilerplate/data/" relative to the current working directory
#' @param overwrite Logical. Whether to overwrite existing entries (deprecated, use merge_strategy instead).
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#' # Initialize all categories with default merge strategy
#' boilerplate_init_text()
#'
#' # Initialize only methods
#' boilerplate_init_text("methods")
#'
#' # Use recursive merging
#' boilerplate_init_text(merge_strategy = "merge_recursive")
#'
#' # Completely overwrite existing databases
#' boilerplate_init_text(merge_strategy = "overwrite_all")
#'
#' # Initialize in a specific directory
#' boilerplate_init_text(text_path = "path/to/project/data")
#' }
#'
#' @export
boilerplate_init_text <- function(
    categories = c("measures", "methods", "results", "discussion"),
    merge_strategy = c("keep_existing", "merge_recursive", "overwrite_all"),
    text_path = NULL,
    overwrite = FALSE
) {
  # Handle input validation
  merge_strategy <- match.arg(merge_strategy)

  # Handle deprecated overwrite parameter
  if (overwrite && merge_strategy == "keep_existing") {
    merge_strategy <- "overwrite_all"
    warning("The 'overwrite' parameter is deprecated. Please use merge_strategy='overwrite_all' instead.")
  }

  # Process each category
  for (category in categories) {
    # Get default database for category
    default_db <- get_default_db(category)

    # Get file path
    file_path <- get_text_file_path(category, text_path)

    # Check if file exists
    if (file.exists(file_path) && merge_strategy != "overwrite_all") {
      # Load existing database
      existing_db <- tryCatch({
        readRDS(file_path)
      }, error = function(e) {
        warning(paste("Error loading existing database:", e$message))
        return(list())
      })

      # Apply selected merge strategy
      if (merge_strategy == "keep_existing") {
        # Only add new keys, never modify existing ones
        merged_db <- utils::modifyList(default_db, existing_db, keep.null = TRUE)
        message(paste("Merged", category, "database (keeping existing entries)"))
      } else if (merge_strategy == "merge_recursive") {
        # Deep recursive merge, combining nested structures
        merged_db <- merge_recursive_lists(default_db, existing_db)
        message(paste("Recursively merged", category, "database"))
      }

      # Save merged database
      save_text_db(merged_db, category, text_path)
    } else {
      # Save default database
      save_text_db(default_db, category, text_path)

      action <- if (merge_strategy == "overwrite_all") "Overwrote" else "Created new"
      message(paste(action, category, "database"))
    }
  }

  message("Initialization complete.")
}

#' Recursively Merge Two Lists
#'
#' This helper function performs a deep recursive merge of two lists,
#' combining nested structures while keeping values from the second list
#' when there are conflicts.
#'
#' @param x First list (defaults)
#' @param y Second list (existing values, takes precedence)
#'
#' @return Merged list
#'
#' @noRd
merge_recursive_lists <- function(x, y) {
  if (!is.list(x) || !is.list(y)) return(y)

  # For each name in x
  for (name in names(x)) {
    if (name %in% names(y)) {
      if (is.list(x[[name]]) && is.list(y[[name]])) {
        # Recursively merge lists
        y[[name]] <- merge_recursive_lists(x[[name]], y[[name]])
      }
      # Otherwise y's value takes precedence
    } else {
      # If not in y, add from x
      y[[name]] <- x[[name]]
    }
  }

  return(y)
}

