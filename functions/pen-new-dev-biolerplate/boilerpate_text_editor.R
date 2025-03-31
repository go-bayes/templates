#' Create an Interactive Text Editor for Boilerplate Text
#'
#' This function provides a hierarchical text editor interface for managing
#' boilerplate text entries. It allows users to navigate, view, add, update, and
#' remove text entries in various categories with support for nested structures.
#'
#' @param category Character. Category of text to manage.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#' # Launch the interactive editor for methods
#' boilerplate_text_editor("methods")
#'
#' # Launch the interactive editor for measures
#' boilerplate_text_editor("measures")
#' }
#'
#' @importFrom cli cli_h1 cli_h2 cli_text cli_code cli_ol cli_li
#' @importFrom cli cli_alert_success cli_alert_info cli_alert_warning cli_alert_danger
#' @export
boilerplate_text_editor <- function(category = c("measures", "methods", "results", "discussion")) {
  category <- match.arg(category)

  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("Package 'cli' is required for the text editor interface.")
  }

  # Load database
  db <- boilerplate_manage_text(category = category, action = "list")

  # Create UI helper functions
  get_input <- function(prompt, allow_empty = FALSE) {
    while (TRUE) {
      input <- trimws(readline(cli::col_cyan(prompt)))
      if (input != "" || allow_empty)
        return(input)
      cli::cli_alert_danger("Input cannot be empty. Please try again.")
    }
  }

  get_multiline_input <- function(prompt) {
    cli::cli_text(cli::col_cyan(prompt))
    cli::cli_text(cli::col_cyan("Enter your text (press Enter twice on an empty line to finish):"))
    lines <- character()
    empty_line_count <- 0
    repeat {
      line <- readline()
      if (line == "") {
        empty_line_count <- empty_line_count + 1
        if (empty_line_count == 2) {
          break
        }
      } else {
        empty_line_count <- 0
      }
      lines <- c(lines, line)
    }
    paste(lines, collapse = "\n")
  }

  display_menu <- function(title, options) {
    cli::cli_h2(title)
    cli::cli_ol(options)
  }

  get_choice <- function(prompt, max_choice, allow_zero = FALSE) {
    while (TRUE) {
      choice <- suppressWarnings(as.integer(get_input(prompt)))
      if (!is.na(choice) &&
          ((allow_zero && choice >= 0 && choice <= max_choice) ||
           (!allow_zero && choice >= 1 && choice <= max_choice))) {
        return(choice)
      }
      if (allow_zero) {
        cli::cli_alert_danger(paste("Invalid choice. Please enter a number between 0 and", max_choice))
      } else {
        cli::cli_alert_danger(paste("Invalid choice. Please enter a number between 1 and", max_choice))
      }
    }
  }

  # Main editor loop
  cli::cli_h1(paste("Boilerplate Text Editor -", toupper(substr(category, 1, 1)), substr(category, 2, nchar(category))))

  # Initialize path stack for navigation
  path_stack <- list(list(name = "root", db = db))
  current_level <- 1

  repeat {
    current_location <- path_stack[[current_level]]
    current_db <- current_location$db

    # Display breadcrumb navigation
    breadcrumb <- if (current_level == 1) {
      "Root"
    } else {
      paste(sapply(path_stack[1:current_level], function(x) x$name), collapse = " > ")
    }

    # Display current entries
    cli::cli_h2(paste("Location:", breadcrumb))
    entries <- names(current_db)

    if (length(entries) == 0) {
      cli::cli_alert_info("No entries found at this location.")
    } else {
      for (i in seq_along(entries)) {
        if (is.list(current_db[[entries[i]]])) {
          # This is a subfolder
          cli::cli_li("{i}. [FOLDER] {entries[i]}")
        } else {
          # This is a text entry
          preview <- current_db[[entries[i]]]
          if (nchar(preview) > 50) {
            preview <- paste0(substr(preview, 1, 47), "...")
          }
          cli::cli_li("{i}. {entries[i]}: {preview}")
        }
      }
    }

    # Display options
    options <- c(
      "Navigate to folder/view entry",
      "Add entry",
      "Update entry",
      "Remove entry",
      "Go up one level",
      "Save changes",
      "Exit"
    )

    display_menu("Options", options)
    choice <- get_choice("Enter your choice: ", length(options))

    if (choice == 1) {
      # Navigate or view
      if (length(entries) == 0) {
        cli::cli_alert_warning("No entries to navigate to or view.")
      } else {
        entry_choice <- get_choice("Enter the number of the entry: ", length(entries))
        entry_name <- entries[entry_choice]
        entry_value <- current_db[[entry_name]]

        if (is.list(entry_value)) {
          # Navigate into subfolder
          path_stack[[current_level + 1]] <- list(name = entry_name, db = entry_value)
          current_level <- current_level + 1
        } else {
          # View text entry
          cli::cli_h2(paste("Entry:", entry_name))
          cli::cli_code(entry_value)
          readline(cli::col_cyan("Press Enter to continue..."))
        }
      }
    } else if (choice == 2) {
      # Add entry
      entry_name <- get_input("Enter name for the new entry: ")

      # Check if entry already exists
      if (entry_name %in% entries) {
        cli::cli_alert_danger("Entry already exists!")
        next
      }

      # Ask if this should be a folder or text entry
      entry_type <- tolower(get_input("Is this a folder (f) or text entry (t)? "))

      if (entry_type == "f") {
        # Create a new subfolder
        current_db[[entry_name]] <- list()
        cli::cli_alert_success("Created new folder: {entry_name}")
      } else {
        # Create a new text entry
        entry_text <- get_multiline_input("Enter text for the new entry: ")
        current_db[[entry_name]] <- entry_text
        cli::cli_alert_success("Added new text entry: {entry_name}")
      }

      # Update the database in the path stack
      path_stack[[current_level]]$db <- current_db

      # If we're in a subfolder, update the parent folders
      if (current_level > 1) {
        for (i in (current_level - 1):1) {
          parent_name <- path_stack[[i + 1]]$name
          path_stack[[i]]$db[[parent_name]] <- path_stack[[i + 1]]$db
        }
      }

      # Update the main database
      db <- path_stack[[1]]$db
    } else if (choice == 3) {
      # Update entry
      if (length(entries) == 0) {
        cli::cli_alert_warning("No entries to update.")
      } else {
        entry_choice <- get_choice("Enter the number of the entry to update: ", length(entries))
        entry_name <- entries[entry_choice]
        entry_value <- current_db[[entry_name]]

        if (is.list(entry_value)) {
          cli::cli_alert_warning("Cannot update a folder directly. Navigate into it first.")
        } else {
          cli::cli_h2(paste("Current text for", entry_name))
          cli::cli_code(entry_value)

          entry_text <- get_multiline_input("Enter new text: ")
          current_db[[entry_name]] <- entry_text

          # Update the database in the path stack
          path_stack[[current_level]]$db <- current_db

          # If we're in a subfolder, update the parent folders
          if (current_level > 1) {
            for (i in (current_level - 1):1) {
              parent_name <- path_stack[[i + 1]]$name
              path_stack[[i]]$db[[parent_name]] <- path_stack[[i + 1]]$db
            }
          }

          # Update the main database
          db <- path_stack[[1]]$db

          cli::cli_alert_success("Updated text entry: {entry_name}")
        }
      }
    } else if (choice == 4) {
      # Remove entry
      if (length(entries) == 0) {
        cli::cli_alert_warning("No entries to remove.")
      } else {
        entry_choice <- get_choice("Enter the number of the entry to remove: ", length(entries))
        entry_name <- entries[entry_choice]

        confirm <- tolower(get_input(paste("Are you sure you want to remove", entry_name, "? (y/n): ")))
        if (confirm == "y") {
          current_db[[entry_name]] <- NULL

          # Update the database in the path stack
          path_stack[[current_level]]$db <- current_db

          # If we're in a subfolder, update the parent folders
          if (current_level > 1) {
            for (i in (current_level - 1):1) {
              parent_name <- path_stack[[i + 1]]$name
              path_stack[[i]]$db[[parent_name]] <- path_stack[[i + 1]]$db
            }
          }

          # Update the main database
          db <- path_stack[[1]]$db

          cli::cli_alert_success("Removed entry: {entry_name}")
        }
      }
    } else if (choice == 5) {
      # Go up one level
      if (current_level == 1) {
        cli::cli_alert_warning("Already at the root level.")
      } else {
        current_level <- current_level - 1
      }
    } else if (choice == 6) {
      # Save changes
      file_path <- get_input("Enter file path to save (or press Enter for default): ", allow_empty = TRUE)

      # Use our refactored function to save the database
      result <- boilerplate_manage_text(
        category = category,
        action = "save",
        db = db,
        text_path = if (file_path == "") NULL else dirname(file_path),
        file_name = if (file_path == "") NULL else basename(file_path)
      )

      if (result) {
        cli::cli_alert_success("Database saved successfully.")
      }
    } else if (choice == 7) {
      # Exit
      confirm_exit <- tolower(get_input("Exit without saving? Changes may be lost. (y/n): "))
      if (confirm_exit == "y") {
        cli::cli_alert_success("Exiting editor.")
        break
      }
    }
  }
}

