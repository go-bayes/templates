# Package Documentation and Structure
# Create this as boilerplate-package.R

#' Boilerplate: Generate Methods and Results for Scientific Reports
#'
#' @description
#' The 'boilerplate' package provides tools for managing and generating standardized text
#' for methods and results sections of scientific reports. It handles template variable
#' substitution and supports hierarchical organization of text through dot-separated paths.
#'
#' @details
#' Main functions:
#' \itemize{
#'   \item \code{\link{boilerplate_manage_text}}: Manage general text databases
#'   \item \code{\link{boilerplate_manage_measures}}: Manage measures databases
#'   \item \code{\link{boilerplate_generate_text}}: Generate text from templates with variable substitution
#'   \item \code{\link{boilerplate_methods_text}}: Generate methods text with appropriate defaults
#'   \item \code{\link{boilerplate_results_text}}: Generate results text with appropriate defaults
#'   \item \code{\link{boilerplate_measures_text}}: Generate measures text for reporting
#'   \item \code{\link{boilerplate_init_text}}: Initialize text databases with defaults
#'   \item \code{\link{boilerplate_merge_databases}}: Merge two databases with conflict resolution
#' }
#'
#' @seealso
#' \url{https://github.com/yourusername/boilerplate} for more information and examples.
#'
#' @docType package
#' @name boilerplate-package
NULL

# Directory structure for the package

# boilerplate/
# ├── R/
# │   ├── boilerplate-package.R           # Package documentation
# │   ├── boilerplate_manage_text.R       # Text database management
# │   ├── boilerplate_manage_measures.R   # Measures database management
# │   ├── boilerplate_generate_text.R     # Text generation with templates
# │   ├── specialized_wrappers.R          # Methods, results, measures text generation wrappers
# │   ├── init_and_merge.R                # Database initialization and merging functions
# │   ├── default_databases.R             # Default database content
# │   └── utilities.R                     # Shared utility functions
# ├── data/                               # Example databases
# │   ├── example_methods_db.rda          # Example methods database
# │   └── example_measures_db.rda         # Example measures database
# ├── vignettes/                          # Package vignettes
# │   ├── basic_usage.Rmd                 # Basic usage
# │   └── research_workflow.Rmd           # Complete research workflow example
# ├── DESCRIPTION                         # Package metadata
# ├── NAMESPACE                           # Package exports
# └── README.md                           # Package README

# DESCRIPTION file content
# Title: Boilerplate: Generate Methods and Results for Scientific Reports
# Version: 0.1.0
# Authors@R:
#     person("Your", "Name", , "your.email@example.com", role = c("aut", "cre"),
#            comment = c(ORCID = "YOUR-ORCID-ID"))
# Description: Tools for managing and generating standardized text for methods and
#     results sections of scientific reports. Supports template variable substitution
#     and hierarchical organization of text through dot-separated paths.
# License: MIT + file LICENSE
# Encoding: UTF-8
# Roxygen: list(markdown = TRUE)
# RoxygenNote: 7.2.3
# Imports:
#     glue,
#     here,
#     stringr,
#     tools,
#     janitor,
#     cli
# Suggests:
#     knitr,
#     rmarkdown,
#     testthat (>= 3.0.0)
# VignetteBuilder: knitr
# Config/testthat/edition: 3
