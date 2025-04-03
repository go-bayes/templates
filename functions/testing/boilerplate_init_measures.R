boilerplate_init_measures <- function(
    merge_strategy = c("keep_existing", "merge_recursive", "overwrite_all"),
    measures_path = NULL,
    file_name = NULL,
    overwrite = FALSE,
    quiet = FALSE,
    dry_run = FALSE,
    create_dirs = FALSE,
    confirm = TRUE
) {
  # track whether measures were processed or skipped
  processed <- FALSE

  # handle input validation
  merge_strategy <- match.arg(merge_strategy)

  if (!quiet) cli::cli_alert_info("initialising measures database with strategy: {merge_strategy}")
  if (dry_run && !quiet) cli::cli_alert_info("dry run mode: no files will be written")

  # handle deprecated overwrite parameter
  if (overwrite && merge_strategy == "keep_existing") {
    merge_strategy <- "overwrite_all"
    if (!quiet) cli::cli_alert_warning("the 'overwrite' parameter is deprecated. please use merge_strategy='overwrite_all' instead.")
  }

  # set default path if not provided
  if (is.null(measures_path)) {
    if (!requireNamespace("here", quietly = TRUE)) {
      if (!quiet) cli::cli_alert_danger("package 'here' is required for default path resolution")
      stop("Package 'here' is required for default path resolution. Please install it or specify 'measures_path' manually.")
    }
    measures_path <- here::here("boilerplate", "data")
    if (!quiet) cli::cli_alert_info("using default path: {measures_path}")
  }

  # check if directory exists
  dir_exists <- dir.exists(measures_path)

  # handle directory creation
  if (!dir_exists) {
    if (!create_dirs) {
      if (!quiet) cli::cli_alert_danger("directory does not exist: {measures_path}")
      stop("Directory does not exist. Set create_dirs=TRUE to create it or specify an existing directory.")
    }

    # ask for confirmation if needed
    proceed <- TRUE
    if (confirm && !dry_run) {
      proceed <- ask_yes_no(paste0("Directory does not exist: ", measures_path, ". Create it?"))
    }

    if (proceed && !dry_run) {
      dir.create(measures_path, recursive = TRUE)
      if (!quiet) cli::cli_alert_success("created directory: {measures_path}")
    } else if (!proceed) {
      if (!quiet) cli::cli_alert_danger("directory creation cancelled by user")
      stop("Directory creation cancelled by user.")
    } else if (dry_run) {
      if (!quiet) cli::cli_alert_info("would create directory: {measures_path}")
    }
  }

  # default file name
  if (is.null(file_name)) {
    file_name <- "measures_db.rds"
    if (!quiet) cli::cli_alert_info("using default file name: {file_name}")
  }

  # get file path
  file_path <- file.path(measures_path, file_name)
  if (!quiet) cli::cli_alert_info("full file path: {file_path}")

  # get default measures database
  if (!quiet) cli::cli_alert_info("loading default measures database")
  default_db <- get_default_measures_db()

  # check if file exists and apply merge strategy
  file_exists <- file.exists(file_path)
  if (file_exists) {
    action <- if (merge_strategy == "overwrite_all") {
      "overwriting"
    } else {
      "merging with"
    }
    if (!quiet) cli::cli_alert_info("{action} existing measures database file: {file_path}")
  }

  if (file_exists && merge_strategy != "overwrite_all") {
    if (dry_run) {
      if (!quiet) cli::cli_alert_info("would load and merge existing database from {file_path}")
      if (!quiet) cli::cli_alert_success("dry run completed")
      return(invisible(TRUE))
    }

    # ask for confirmation if needed
    proceed <- TRUE
    if (confirm) {
      proceed <- ask_yes_no(paste0("Modify existing file: ", file_path, "?"))
    }

    if (!proceed) {
      if (!quiet) cli::cli_alert_info("measures database update cancelled by user")
      return(invisible(FALSE))
    }

    # load existing database
    if (!quiet) cli::cli_alert_info("loading existing database from {file_path}")
    existing_db <- tryCatch({
      readRDS(file_path)
    }, error = function(e) {
      if (!quiet) cli::cli_alert_danger("error loading existing database: {e$message}")
      return(list())
    })

    # apply selected merge strategy
    if (merge_strategy == "keep_existing") {
      # only add new keys, never modify existing ones
      merged_db <- utils::modifyList(default_db, existing_db, keep.null = TRUE)
      if (!quiet) cli::cli_alert_success("merged measures database (keeping existing entries)")
    } else if (merge_strategy == "merge_recursive") {
      # deep recursive merge, combining nested structures
      merged_db <- merge_recursive_lists(default_db, existing_db)
      if (!quiet) cli::cli_alert_success("recursively merged measures database")
    }

    # save merged database
    if (!quiet) cli::cli_alert_info("saving merged database")
    saveRDS(merged_db, file = file_path)
    if (!quiet) cli::cli_alert_success("updated measures database at: {file_path}")
    processed <- TRUE
  } else {
    if (dry_run) {
      action <- if (file_exists) "would overwrite" else "would create new"
      if (!quiet) cli::cli_alert_info("{action} measures database at: {file_path}")
      if (!quiet) cli::cli_alert_success("dry run completed")
      return(invisible(TRUE))
    }

    # ask for confirmation if needed and file exists
    proceed <- TRUE
    if (confirm && file_exists && merge_strategy == "overwrite_all") {
      proceed <- ask_yes_no(paste0("Overwrite existing file: ", file_path, "?"))
    }

    if (!proceed) {
      if (!quiet) cli::cli_alert_info("measures database creation/overwrite cancelled by user")
      return(invisible(FALSE))
    }

    # save default database
    if (!quiet) cli::cli_alert_info("saving default database")
    saveRDS(default_db, file = file_path)
    action <- if (file_exists) "overwrote" else "created new"
    if (!quiet) cli::cli_alert_success("{action} measures database at: {file_path}")
    processed <- TRUE
  }

  if (!quiet) {
    if (processed) {
      cli::cli_alert_success("measures initialisation complete")
    } else {
      cli::cli_alert_warning("measures initialisation skipped")
    }
  }

  return(invisible(processed))
}
