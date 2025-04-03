boilerplate_init_text <- function(
    categories = c("methods", "results", "discussion", "appendix", "template"),
    merge_strategy = c("keep_existing", "merge_recursive", "overwrite_all"),
    text_path = NULL,
    overwrite = FALSE,
    quiet = FALSE,
    dry_run = FALSE,
    create_dirs = FALSE,
    confirm = TRUE
) {
  # handle input validation
  merge_strategy <- match.arg(merge_strategy)

  # track which categories were successfully processed
  processed_categories <- character(0)
  skipped_categories <- character(0)

  if (!quiet) cli_alert_info("initialising {length(categories)} text databases with strategy: {merge_strategy}")
  if (dry_run && !quiet) cli_alert_info("dry run mode: no files will be written")

  # check if 'measures' is in categories and warn that it should be initialised separately
  if ("measures" %in% categories) {
    if (!quiet) cli_alert_warning("'measures' category should be initialised using boilerplate_init_measures()")
    categories <- setdiff(categories, "measures")
    if (!quiet) cli_alert_info("'measures' category removed from initialisation list")
    skipped_categories <- c(skipped_categories, "measures")
  }

  # handle deprecated overwrite parameter
  if (overwrite && merge_strategy == "keep_existing") {
    merge_strategy <- "overwrite_all"
    if (!quiet) cli_alert_warning("the 'overwrite' parameter is deprecated. please use merge_strategy='overwrite_all' instead.")
  }

  # set default path if not provided
  if (is.null(text_path)) {
    if (!requireNamespace("here", quietly = TRUE)) {
      if (!quiet) cli_alert_danger("package 'here' is required for default path resolution")
      stop("Package 'here' is required for default path resolution. Please install it or specify 'text_path' manually.")
    }
    text_path <- here::here("boilerplate", "data")
    if (!quiet) cli_alert_info("using default path: {text_path}")
  }

  # check if directory exists
  dir_exists <- dir.exists(text_path)

  # handle directory creation
  if (!dir_exists) {
    if (!create_dirs) {
      if (!quiet) cli_alert_danger("directory does not exist: {text_path}")
      stop("Directory does not exist. Set create_dirs=TRUE to create it or specify an existing directory.")
    }

    # ask for confirmation if needed
    proceed <- TRUE
    if (confirm && !dry_run) {
      proceed <- ask_yes_no(paste0("Directory does not exist: ", text_path, ". Create it?"))
    }

    if (proceed && !dry_run) {
      dir.create(text_path, recursive = TRUE)
      if (!quiet) cli_alert_success("created directory: {text_path}")
    } else if (!proceed) {
      if (!quiet) cli_alert_danger("directory creation cancelled by user")
      stop("Directory creation cancelled by user.")
    } else if (dry_run) {
      if (!quiet) cli_alert_info("would create directory: {text_path}")
    }
  }

  # process each category
  for (category in categories) {
    if (!quiet) cli_alert_info("processing category: {category}")

    # get default database for category
    if (!quiet) cli_alert_info("loading default {category} database")
    default_db <- get_default_db(category)

    # get file path
    file_path <- file.path(text_path, paste0(category, "_db.rds"))
    if (!quiet) cli_alert_info("full file path: {file_path}")

    # check if file exists and warn
    file_exists <- file.exists(file_path)
    if (file_exists) {
      action <- if (merge_strategy == "overwrite_all") {
        "overwriting"
      } else {
        "merging with"
      }
      if (!quiet) cli_alert_info("{action} existing {category} database file: {file_path}")
    }

    # handle existing files according to merge strategy
    if (file_exists && merge_strategy != "overwrite_all") {
      if (dry_run) {
        if (!quiet) cli_alert_info("would load and merge existing {category} database from {file_path}")
        processed_categories <- c(processed_categories, category)
        next
      }

      # ask for confirmation if needed
      proceed <- TRUE
      if (confirm) {
        proceed <- ask_yes_no(paste0("Modify existing file: ", file_path, "?"))
      }

      if (!proceed) {
        if (!quiet) cli_alert_info("skipping {category} database update")
        skipped_categories <- c(skipped_categories, category)
        next
      }

      # load existing database
      if (!quiet) cli_alert_info("loading existing {category} database from {file_path}")
      existing_db <- tryCatch({
        readRDS(file_path)
      }, error = function(e) {
        if (!quiet) cli_alert_danger("error loading existing {category} database: {e$message}")
        return(list())
      })

      # apply selected merge strategy
      if (merge_strategy == "keep_existing") {
        # only add new keys, never modify existing ones
        merged_db <- utils::modifyList(default_db, existing_db, keep.null = TRUE)
        if (!quiet) cli_alert_success("merged {category} database (keeping existing entries)")
      } else if (merge_strategy == "merge_recursive") {
        # deep recursive merge, combining nested structures
        merged_db <- merge_recursive_lists(default_db, existing_db)
        if (!quiet) cli_alert_success("recursively merged {category} database")
      }

      # save merged database
      if (!quiet) cli_alert_info("saving merged {category} database")
      saveRDS(merged_db, file = file_path)
      if (!quiet) cli_alert_success("updated {category} database at: {file_path}")
      processed_categories <- c(processed_categories, category)
    } else {
      if (dry_run) {
        action <- if (file_exists) "would overwrite" else "would create new"
        if (!quiet) cli_alert_info("{action} {category} database at: {file_path}")
        processed_categories <- c(processed_categories, category)
        next
      }

      # ask for confirmation if needed and file exists
      proceed <- TRUE
      if (confirm && file_exists && merge_strategy == "overwrite_all") {
        proceed <- ask_yes_no(paste0("Overwrite existing file: ", file_path, "?"))
      }

      if (!proceed) {
        if (!quiet) cli_alert_info("skipping {category} database creation/overwrite")
        skipped_categories <- c(skipped_categories, category)
        next
      }

      # save default database
      if (!quiet) cli_alert_info("saving default {category} database")
      saveRDS(default_db, file = file_path)
      action <- if (file_exists) "overwrote" else "created new"
      if (!quiet) cli_alert_success("{action} {category} database at: {file_path}")
      processed_categories <- c(processed_categories, category)
    }
  }

  if (!quiet) {
    if (dry_run) {
      cli_alert_success("dry run completed for all {length(processed_categories)} categories")
    } else {
      if (length(processed_categories) > 0) {
        cli_alert_success("initialisation complete for {length(processed_categories)} categories: {paste(processed_categories, collapse = ', ')}")
      }

      if (length(skipped_categories) > 0) {
        cli_alert_warning("initialisation skipped for {length(skipped_categories)} categories: {paste(skipped_categories, collapse = ', ')}")
      }

      if (length(processed_categories) == 0) {
        cli_alert_warning("no databases were initialised")
      }
    }
  }

  # return invisible list of processed and skipped categories
  invisible(list(
    processed = processed_categories,
    skipped = skipped_categories
  ))
}
