# NZAVS functions
# test data
# reproducibility
set.seed(123)
#detach("package:margot", unload = TRUE)
devtools::install_github("go-bayes/margot")
packageVersion(pkg = 'margot')# set
push_mods <- here::here("/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/00-talks")

path_talk = push_mods
pull_path <- fs::path_expand("/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph Bulbulia/00Bulbulia Pubs/DATA/nzavs-current/r-data/nzavs_data_qs")


# references --------------------------------------------------------------


# read and preprocess data ------------------------------------------------
# import data
dat <- qs::qread(here::here(pull_path))


# read and preprocess data ------------------------------------------------
# import data

# Function to prepare panel data
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggokabeito)
library(cli)
library(margot)
library(patchwork)


# create response timeline ------------------------------------------------


# step 1: prepare NZAVS-specific wave breaks
nzavs_wave_breaks <- list(
  "time 1" = c(as.Date("2009-08-30"), as.Date("2010-10-15")),
  "time 2" = c(as.Date("2010-10-15"), as.Date("2011-10-15")),
  "time 3" = c(as.Date("2011-10-15"), as.Date("2012-10-15")),
  "time 4" = c(as.Date("2012-10-15"), as.Date("2013-10-15")),
  "time 5" = c(as.Date("2013-10-15"), as.Date("2014-10-15")),
  "time 6" = c(as.Date("2014-10-15"), as.Date("2015-10-15")),
  "time 7" = c(as.Date("2015-10-15"), as.Date("2016-10-15")),
  "time 8" = c(as.Date("2016-10-15"), as.Date("2017-10-15")),
  "time 9" = c(as.Date("2017-10-15"), as.Date("2018-10-15")),
  "time 10" = c(as.Date("2018-10-15"), as.Date("2019-10-15")),
  "time 11" = c(as.Date("2019-10-15"), as.Date("2020-10-15")),
  "time 12" = c(as.Date("2020-10-15"), as.Date("2021-10-15")),
  "time 13" = c(as.Date("2021-10-15"), as.Date("2022-10-15")),
  "time 14" = c(as.Date("2022-10-15"), as.Date("2023-10-15"))
)

# step 2: prepare the NZAVS data using the updated generalised function

prepared_data <- prepare_panel_data(
  dat,
  wave_col = "wave",
  tscore_col = "tscore",
  id_col = "id",
  base_date = as.Date("2009-06-30"),
  wave_breaks = nzavs_wave_breaks
)

# view
prepared_data

# step 3: create NZAVS timeline plot using the generalised function
nzavs_timeline <- margot_plot_response_timeline(
  prepared_data$df_timeline,
  n_total_participants = prepared_data$n_total_participants,
  save = TRUE,
  save_path = here::here(push_mods),
  title = "New Zealand Attitudes and Values Study (panel)",
  x_label = paste("NZAVS years", min(lubridate::year(prepared_data$df_timeline$day)),
                  "-", max(lubridate::year(prepared_data$df_timeline$day)),
                  "cohort: daily counts by condition"),
  y_label = "Count of Responses"
)

# Display the plot
nzavs_timeline

# end ---------------------------------------------------------------------




# 1. Muslim warmth after attacks
discontinuity_plot_muslim_warmth <- margot_plot_discontinuity(
  data = dat,
  y_var = "warm_muslims",
  event_dates = c("2019-03-15", "2020-03-26"),
  event_names = c("Christchurch Attack", "COVID-19 Lockdown"),
  start_date = "2012-06-06",
 # title = "Discontinuity Christchurch Attaks & Covid 19",
  y_label = "Muslim Warmth",
  x_label = "NZAVS Time 4 - 14 Cohort (2012-2023)",
  point_alpha = 0.03,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)


# view
discontinuity_plot_muslim_warmth

# 2. Trust in Police before/after COVID-19
discontinuity_plot_trust_police <- margot_plot_discontinuity(
  data = dat,
  y_var = "police_trust",
  event_dates = c("2020-03-26"),
 # title = "Trust in Police: Pre/Post Covid-19",
  event_names = c("COVID-19 Lockdown"),
  y_label = "Trust in Police",
  x_label = "NZAVS Waves",
  point_alpha = 0.03,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)

# view
discontinuity_plot_trust_police



# 3. Trust in Politicians before/after COVID-19
discontinuity_plot_trust_politicians <- margot_plot_discontinuity(
  data = dat,
  y_var = "pol_politician_trust",
  event_dates = c("2020-03-26"),
#  title = "Trust in Politicians: Pre/Post Covid-19",
  event_names = c("COVID-19 Lockdown"),
  y_label = "Trust in Politicians",
  x_label = "NZAVS Waves",
  point_alpha = 0.03,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)

# view
discontinuity_plot_trust_politicians


# 4. Trust in Science before/after COVID-19
discontinuity_plot_trust_science_community <- margot_plot_discontinuity(
  data = dat,
  y_var = "trust_science_high_confidence_scientific_community",
  event_dates = c("2020-03-26"),
 # title = "Trust in Scientific Community: Pre/Post Covid-19",
  event_names = c("COVID-19 Lockdown"),
  start_date = "2019-10-01",
  y_label = "Trust in Scientific Community",
  x_label = "NZAVS Waves",
  point_alpha = 0.03,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)
# view
discontinuity_plot_trust_science_community

sort( colnames(dat) )
discontinuity_plot_trust_science_in_society <- margot_plot_discontinuity(
  data = dat,
  y_var = "trust_science_our_society_places_too_much_emphasis_reversed",
  event_dates = c("2020-03-26"),
 # title = "Trust in Scientific Community: Pre/Post Covid-19",
  event_names = c("COVID-19 Lockdown"),
  start_date = "2019-10-01",
  y_label = "Trust in Scientific Community",
  x_label = "NZAVS Waves",
  point_alpha = 0.03,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)
# view
discontinuity_plot_trust_science_in_society /discontinuity_plot_trust_science_community


# 5. Conspiracy Beliefs before/after COVID-19
discontinuity_plot_conspiracy_beliefs <- margot_plot_discontinuity(
  data = dat,
  y_var = "conspiracy_beliefs",
  event_dates = c("2020-03-26"),
  event_names = c("COVID-19 Lockdown"),
  start_date = "2019-10-01",
  y_label = "Trust in Scientific Community",
  x_label = "NZAVS Waves",
  point_alpha = 0.03,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)
discontinuity_plot_conspiracy_beliefs



# test
test <- margot_plot_slope(
    data = dat,
    y_vars = "conspiracy_beliefs",
    event_dates = c("2020-03-26"),
    event_names = c("COVID-19 Lockdown"),
    y_label = "Trust in Scientific Community",
    x_label = "NZAVS Waves",
  )
test

test_2 <- margot_plot_boxplot(
  data = dat,
  y_vars = "conspiracy_beliefs",
  x_label = "NZAVS Waves",
  x_var = "wave"
)
print( test_2 )

test_3 <- margot_plot_slope_covariate(
  data = dat,
  formula = conspiracy_beliefs ~ wave:hours_work,
  terms = c("wave", "hours_work"),
  #title = "Warmth Towards Muslims by Work Hours Over Time",
  y_label = "Warmth",
  x_label = "Wave",
  color_label = "Work Hours"
)
test_3
# margot::here_save_qs(plotly_muslim_warmth_plot, "plotly_muslim_warmth_plot", push_mods)
# plotly_muslim_warmth_plot


# test functions ----------------------------------------------------------
margot_plot_slope_covariate <- function(data,
                                        formula,
                                        terms,
                                        title = NULL,
                                        y_label = NULL,
                                        x_label = NULL,
                                        y_limits = c(1, 7),
                                        color_label = NULL,
                                        save_path = NULL,
                                        width = 12,
                                        height = 8,
                                        seed = NULL,
                                        ...) {

  cli::cli_h1("Margot Plot ggeffects")

  # Initialize p as NULL
  p <- NULL

  tryCatch({
    cli::cli_alert_info("Preparing model and calculating predicted responses...")

    # Set seed if provided
    if (!is.null(seed)) {
      set.seed(seed)
    }

    # Extract variables from the formula
    all_vars <- all.vars(formula)
    response_var <- all_vars[1]
    predictor_vars <- all_vars[-1]

    # Remove rows with NA or infinite values in response or predictor variables
    clean_data <- data %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(c(response_var, predictor_vars)),
                                  ~!is.na(.) & is.finite(.)))

    # Calculate unique participants and observations after cleaning
    n_participants <- clean_data %>% dplyr::select(id) %>% dplyr::n_distinct()
    n_observations <- nrow(clean_data)

    # Fit the model using clean data
    model <- lm(formula, data = clean_data)

    # Calculate predicted responses
    pred <- ggeffects::predict_response(model, terms = terms, ...)

    cli::cli_alert_success("Predicted responses calculated")

    cli::cli_alert_info("Creating plot...")

    # Generate automatic title if not provided
    if (is.null(title)) {
      predictor_vars_str <- paste(terms, collapse = ", ")
      title <- sprintf("%s by %s\nN = %s participants, %s observations",
                       response_var, predictor_vars_str,
                       format(n_participants, big.mark = ","),
                       format(n_observations, big.mark = ","))
    }

    # Create the ggplot
    p <- plot(pred) +
      ggokabeito::scale_colour_okabe_ito() +
      theme_classic() +
      scale_y_continuous(limits = y_limits) +
      labs(
        title = title,
        y = y_label %||% response_var,
        x = x_label %||% terms[1],
        color = color_label %||% terms[2]
      )

    cli::cli_alert_success("Plot created successfully")

    # Save plot if a save path is provided
    if (!is.null(save_path)) {
      filename <- paste0(
        "ggeffects_plot_",
        response_var,
        "_by_",
        paste(terms, collapse = "_"),
        "_",
        format(Sys.Date(), "%Y%m%d")
      )

      cli::cli_alert_info("Saving plot...")

      ggsave(
        plot = p,
        filename = file.path(save_path, paste0(filename, ".png")),
        width = width,
        height = height,
        units = "in",
        device = 'png',
        dpi = 400
      )

      margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)

      cli::cli_alert_success("Plot saved successfully")
    } else {
      cli::cli_alert_info("No save path provided. Plot not saved.")
    }

    cli::cli_alert_success("Margot plot ggeffects created successfully \U0001F44D")

    # Return the ggplot object
    return(p)

  }, error = function(e) {
    cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
    print(e)
    return(NULL)
  }, warning = function(w) {
    cli::cli_alert_warning("A warning occurred: {conditionMessage(w)}")
    print(w)
  })
}

# margot_plot_slope <- function(data,
#                               y_vars,
#                               event_dates = NULL,
#                               event_names = NULL,
#                               start_date = NULL,
#                               end_date = NULL,
#                               title = NULL,
#                               y_label = NULL,
#                               x_label = NULL,
#                               data_fraction = 1,
#                               seed = NULL,
#                               plot_points = FALSE,
#                               point_alpha = 0.03,
#                               jitter_width = 1,
#                               base_date = as.Date("2009-06-30"),
#                               save_path = NULL,
#                               width = 12,
#                               height = 8,
#                               event_line_color = "darkred",
#                               event_line_alpha = 0.7,
#                               event_line_type = "dashed",
#                               event_line_width = 0.5,
#                               event_label_size = 3,
#                               event_label_color = "darkred",
#                               legend_position = "bottom",
#                               use_title_case = TRUE,
#                               remove_underscores = TRUE,
#                               y_limits = NULL) {
#
#   cli::cli_h1("Margot Plot Slope")
#
#   # Initialize p as NULL
#   p <- NULL
#
#   tryCatch({
#     # Function to transform labels
#     transform_label <- function(label) {
#       if (remove_underscores) {
#         label <- gsub("_", " ", label)
#       }
#       if (use_title_case) {
#         label <- tools::toTitleCase(label)
#       }
#       return(label)
#     }
#
#     cli::cli_alert_info("Preparing data...")
#     # define color palette
#     modified_okabe_ito_colors <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",
#                                    "#D55E00", "#CC79A7", "#000000", "#999999")
#
#     # prepare the data
#     df <- data %>%
#       mutate(year = as.numeric(as.character(wave))) %>%
#       mutate(timeline = base_date + tscore)
#
#     # filter date range if specified
#     if (!is.null(start_date)) {
#       df <- df %>% filter(timeline >= as.Date(start_date))
#     }
#     if (!is.null(end_date)) {
#       df <- df %>% filter(timeline <= as.Date(end_date))
#     }
#
#     # Ensure y_vars is a list
#     if (!is.list(y_vars)) {
#       y_vars <- list(y_vars)
#     }
#
#     # Check if all specified y variables exist in the data
#     missing_vars <- y_vars[!y_vars %in% names(df)]
#     if (length(missing_vars) > 0) {
#       cli::cli_alert_warning("The following variables are not present in the data: {paste(missing_vars, collapse = ', ')}")
#       y_vars <- y_vars[y_vars %in% names(df)]
#       if (length(y_vars) == 0) {
#         cli::cli_alert_danger("No valid y variables remain. Cannot create plot.")
#         return(NULL)
#       }
#     }
#
#     # Remove rows with NA or infinite values in any y_var
#     for (var in y_vars) {
#       df <- df %>% filter(!is.na(!!sym(var)) & is.finite(!!sym(var)))
#     }
#
#     # Calculate unique participants and observations after cleaning
#     n_participants <- df %>% select(id) %>% n_distinct()
#     n_observations <- nrow(df)
#
#     # Sample the data if data_fraction < 1
#     if (data_fraction < 1) {
#       if (!is.null(seed)) {
#         set.seed(seed)
#       }
#       df <- df %>% sample_frac(data_fraction)
#
#       # Recalculate counts after sampling
#       n_participants <- df %>% select(id) %>% n_distinct()
#       n_observations <- nrow(df)
#     }
#
#     cli::cli_alert_success("Data prepared successfully")
#
#     # Warning for plotting points with multiple y variables
#     if (plot_points && length(y_vars) > 1) {
#       cli::cli_alert_warning("Plotting points with multiple y variables may result in a cluttered plot.")
#     }
#
#     cli::cli_alert_info("Creating base plot...")
#
#     # Reshape data for plotting multiple y variables
#     df_long <- df %>%
#       tidyr::pivot_longer(cols = all_of(unlist(y_vars)),
#                           names_to = "variable",
#                           values_to = "value")
#
#     # Determine y-axis limits
#     if (is.null(y_limits)) {
#       y_min <- min(df_long$value, na.rm = TRUE)
#       y_max <- max(df_long$value, na.rm = TRUE)
#       if (y_min >= 1 && y_max <= 7) {
#         y_limits <- c(1, 7)
#       } else {
#         y_limits <- c(y_min, y_max)
#       }
#     }
#
#     # Generate automatic title if not provided
#     if (is.null(title)) {
#       year_range <- range(df$year, na.rm = TRUE)
#       title <- sprintf("Slope Plot for %s\nN = %s participants, %s observations; years %.0f - %.0f",
#                        paste(transform_label(unlist(y_vars)), collapse = ", "),
#                        format(n_participants, big.mark = ","),
#                        format(n_observations, big.mark = ","),
#                        year_range[1], year_range[2])
#     }
#
#     # create the ggplot
#     p <- ggplot(df_long, aes(x = timeline, y = value, color = variable)) +
#       geom_smooth(method = "lm", se = FALSE) +  # add linear trend lines
#       theme_classic() +
#       scale_color_manual(values = modified_okabe_ito_colors,
#                          name = "Variables") +
#       theme(
#         legend.position = legend_position,
#         legend.text = element_text(size = 12),
#         legend.title = element_text(size = 12)
#       ) +
#       labs(
#         title = title,
#         y = transform_label(y_label %||% "Value"),
#         x = transform_label(x_label %||% "Timeline")
#       ) +
#       scale_y_continuous(limits = y_limits)
#
#     # add points if specified
#     if (plot_points) {
#       p <- p + geom_jitter(alpha = point_alpha, width = jitter_width)
#     }
#
#     cli::cli_alert_success("Base plot created")
#
#     if (!is.null(event_dates)) {
#       cli::cli_alert_info("Adding event lines and labels...")
#       # add vertical lines for event dates
#       for (i in seq_along(event_dates)) {
#         event_date <- as.Date(event_dates[i])
#         event_name <- if (!is.null(event_names) && length(event_names) >= i) event_names[i] else paste("Event", i)
#
#         p <- p + geom_vline(xintercept = event_date,
#                             color = event_line_color,
#                             alpha = event_line_alpha,
#                             linetype = event_line_type,
#                             linewidth = event_line_width)
#
#         # calculate y-position for the label to be at the top of the data points
#         y_max <- max(y_limits)
#         y_min <- min(y_limits)
#         y_range <- y_max - y_min
#         label_height <- nchar(event_name) * 0.015 * y_range  # Adjust this multiplier as needed
#         y_position <- y_max
#
#         # calculate x-position slightly to the left of the event line
#         x_offset <- 5  # adjust this value to move labels further left or right
#         x_position <- event_date - x_offset
#
#         # add white rectangle with grey border behind the event label
#         p <- p + annotate("rect",
#                           xmin = x_position - 0.5,
#                           xmax = x_position + 0.5,
#                           ymin = y_position - label_height,
#                           ymax = y_position,
#                           fill = "white",
#                           color = "grey50",
#                           alpha = 0.9,
#                           size = 0.25)
#
#         # add text label for the event
#         p <- p + annotate("text",
#                           x = x_position,
#                           y = y_position,
#                           label = transform_label(event_name),
#                           color = event_label_color,
#                           size = event_label_size,
#                           angle = 90,
#                           vjust = 1,  # Align text to the top of the label box
#                           hjust = 1)  # Align text to the right of the label box
#       }
#       cli::cli_alert_success("Event lines and labels added")
#     }
#
#     cli::cli_alert_info("Saving plot...")
#
#     # save plot if a save path is provided
#     if (!is.null(save_path)) {
#       # generate filename
#       filename <- paste0(
#         "slope_plot_",
#         paste(unlist(y_vars), collapse = "_"),
#         "_",
#         format(Sys.Date(), "%Y%m%d")
#       )
#
#       tryCatch({
#         cli::cli_alert_info("Attempting to print plot...")
#         print(p)
#         cli::cli_alert_success("Plot printed successfully")
#
#         cli::cli_alert_info("Saving plot as PNG...")
#         ggsave(
#           plot = p,
#           filename = file.path(save_path, paste0(filename, ".png")),
#           width = width,
#           height = height,
#           units = "in",
#           device = 'png',
#           dpi = 400
#         )
#         cli::cli_alert_success("Plot saved as PNG successfully")
#
#         cli::cli_alert_info("Saving plot as .qs file...")
#         # from the margot package (go-bayes/margot)
#         margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)
#
#       }, error = function(e) {
#         cli::cli_alert_danger("Error while saving: {conditionMessage(e)}")
#       })
#     } else {
#       cli::cli_alert_info("No save path provided. Plot not saved.")
#     }
#
#     cli::cli_alert_success("Margot plot slope created successfully \U0001F44D")
#
#     # return the ggplot object
#     return(p)
#   }, error = function(e) {
#     cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
#     print(e)
#     return(NULL)
#   }, warning = function(w) {
#     cli::cli_alert_warning("A warning occurred: {conditionMessage(w)}")
#     print(w)
#   })
# }
#' #' Create a Discontinuity Plot for Multiple Events
#' #'
#' #' This function creates a ggplot2 visualisation to show discontinuities in data across multiple events.
#' #' It's particularly useful for visualising changes in trends before and after significant events.
#' #'
#' #' @param data A data frame containing the variables to be plotted.
#' #' @param y_var The name of the y-axis variable in the data frame.
#' #' @param event_dates A vector of dates representing the events.
#' #' @param event_names An optional vector of names for the events. If NULL, events will be labeled "Event 1", "Event 2", etc.
#' #' @param start_date An optional start date for the x-axis.
#' #' @param end_date An optional end date for the x-axis.
#' #' @param title An optional title for the plot.
#' #' @param y_label An optional label for the y-axis.
#' #' @param x_label An optional label for the x-axis.
#' #' @param smoothing_method The method used for smoothing. Default is "gam".
#' #' @param gam_k The number of knots to use if smoothing_method is "gam". Default is 4.
#' #' @param data_fraction The fraction of data to use. Default is 1 (use all data).
#' #' @param seed An optional seed for reproducibility when sampling data.
#' #' @param point_alpha The alpha (transparency) of the data points. Default is 0.03.
#' #' @param jitter_width The width of the jitter for the data points. Default is 1.
#' #' @param base_date The base date for the timeline. Default is "2009-06-30".
#' #' @param save_path An optional path to save the plot.
#' #' @param width The width of the saved plot in inches. Default is 12.
#' #' @param height The height of the saved plot in inches. Default is 8.
#' #' @param event_line_color The color of the event lines. Default is "darkred".
#' #' @param event_line_alpha The alpha of the event lines. Default is 0.7.
#' #' @param event_line_type The type of the event lines. Default is "dashed".
#' #' @param event_line_width The width of the event lines. Default is 0.5.
#' #' @param event_label_size The size of the event labels. Default is 3.
#' #' @param event_label_color The color of the event labels. Default is "darkred".
#' #' @param legend_position The position of the legend. Default is "bottom".
#' #' @param use_title_case Logical, whether to use title case for labels. Default is TRUE.
#' #' @param remove_underscores Logical, whether to remove underscores from labels. Default is TRUE.
#' #'
#' #' @return A ggplot2 object representing the discontinuity plot.
#' #'
#' #' @import ggplot2
#' #' @import dplyr
#' #' @import cli
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' library(dplyr)
#' #' library(ggplot2)
#' #' library(margot)
#' #'
#' #' # Assume that 'dat' is your dataset and that 'path_talk' is defined
#' #' muslim_discontinuity_warmth_plot <- margot_plot_discontinuity(
#' #'   data = dat,
#' #'   y_var = "warm_muslims",
#' #'   event_dates = c("2019-03-15", "2020-03-26"),
#' #'   event_names = c("Christchurch Attack", "COVID-19 Lockdown"),
#' #'   start_date = "2012-06-06",
#' #'   title = "Discontinuity at multiple events (GAM)",
#' #'   y_label = "Muslim Warmth",
#' #'   x_label = "NZAVS Time 4 - 14 Cohort (2012-2023)",
#' #'   point_alpha = 0.05,
#' #'   smoothing_method = "gam",
#' #'   gam_k = 4,
#' #'   data_fraction = .1,
#' #'   seed = 123,
#' #'   save_path = here::here(path_talk)
#' #' )
#' #'
#' #' # Display the plot
#' #' print(muslim_discontinuity_warmth_plot)
#' #' }
#' margot_plot_discontinuity <- function(data,
#'                                       y_var,
#'                                       event_dates,
#'                                       event_names = NULL,
#'                                       start_date = NULL,
#'                                       end_date = NULL,
#'                                       title = NULL,
#'                                       y_label = NULL,
#'                                       x_label = NULL,
#'                                       smoothing_method = "gam",
#'                                       gam_k = 4,
#'                                       data_fraction = 1,
#'                                       seed = NULL,
#'                                       point_alpha = 0.03,
#'                                       jitter_width = 1,
#'                                       base_date = as.Date("2009-06-30"),
#'                                       save_path = NULL,
#'                                       width = 12,
#'                                       height = 8,
#'                                       event_line_color = "darkred",
#'                                       event_line_alpha = 0.7,
#'                                       event_line_type = "dashed",
#'                                       event_line_width = 0.5,
#'                                       event_label_size = 3,
#'                                       event_label_color = "darkred",
#'                                       legend_position = "bottom",
#'                                       use_title_case = TRUE,
#'                                       remove_underscores = TRUE) {
#'
#'   cli::cli_h1("Margot Plot Discontinuity")
#'
#'   # initialise p as NULL
#'   p <- NULL
#'
#'   tryCatch({
#'     # function to transform labels
#'     transform_label <- function(label) {
#'       if (remove_underscores) {
#'         label <- gsub("_", " ", label)
#'       }
#'       if (use_title_case) {
#'         label <- tools::toTitleCase(label)
#'       }
#'       return(label)
#'     }
#'
#'     cli::cli_alert_info("Preparing data...")
#'     # define color palette
#'     modified_okabe_ito_colors <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",
#'                                    "#D55E00", "#CC79A7", "#000000", "#999999")
#'
#'     # prepare the data
#'     df <- data %>%
#'       mutate(year = as.numeric(as.character(wave))) %>%
#'       mutate(timeline = base_date + tscore)
#'
#'     # filter date range if specified
#'     if (!is.null(start_date)) {
#'       df <- df %>% filter(timeline >= as.Date(start_date))
#'     }
#'     if (!is.null(end_date)) {
#'       df <- df %>% filter(timeline <= as.Date(end_date))
#'     }
#'
#'     # remove rows with na or infinite values in y_var
#'     df <- df %>% filter(!is.na(!!sym(y_var)) & is.finite(!!sym(y_var)))
#'
#'     # Calculate unique participants and observations after cleaning
#'     n_participants <- df %>% select(id) %>% n_distinct()
#'     n_observations <- nrow(df)
#'
#'     # Sample the data if data_fraction < 1
#'     if (data_fraction < 1) {
#'       if (!is.null(seed)) {
#'         set.seed(seed)
#'       }
#'       df <- df %>% sample_frac(data_fraction)
#'
#'       # Recalculate counts after sampling
#'       n_participants <- df %>% select(id) %>% n_distinct()
#'       n_observations <- nrow(df)
#'     }
#'
#'     # Generate automatic title if not provided
#'     if (is.null(title)) {
#'       year_range <- range(df$year, na.rm = TRUE)
#'       title <- sprintf("%s Discontinuity Plot\nN = %s participants, %s observations; years %.0f - %.0f",
#'                        transform_label(y_var),
#'                        format(n_participants, big.mark = ","),
#'                        format(n_observations, big.mark = ","),
#'                        year_range[1], year_range[2])
#'     }
#'
#'     # create event condition based on multiple event dates
#'     event_dates <- as.Date(event_dates)
#'     n_discontinuities <- length(event_dates) + 1
#'
#'     df <- df %>%
#'       mutate(event_condition = cut(timeline,
#'                                    breaks = c(as.Date(-Inf), event_dates, as.Date(Inf)),
#'                                    labels = seq_len(n_discontinuities),
#'                                    include.lowest = TRUE))
#'
#'     # recycle colors for the number of discontinuities
#'     recycled_colors <- rep_len(modified_okabe_ito_colors, length.out = n_discontinuities)
#'
#'     cli::cli_alert_success("Data prepared successfully")
#'
#'     cli::cli_alert_info("Creating base plot...")
#'
#'     # create the ggplot
#'     p <- ggplot(df, aes(x = timeline, y = !!sym(y_var), color = event_condition)) +
#'       geom_jitter(alpha = point_alpha, width = jitter_width) +
#'       theme_classic() +
#'       scale_color_manual(values = recycled_colors,
#'                          name = "Event Periods") +
#'       theme(
#'         legend.position = legend_position,
#'         legend.text = element_text(size = 12),
#'         legend.title = element_text(size = 12)
#'       ) +
#'       labs(
#'         title = title,
#'         y = transform_label(y_label %||% y_var),
#'         x = transform_label(x_label %||% "Timeline")
#'       )
#'
#'     cli::cli_alert_success("Base plot created")
#'
#'     cli::cli_alert_info("Adding smoothing...")
#'
#'     # Add smoothing directly without wrapper
#'     p <- p + tryCatch({
#'       if (smoothing_method == "gam") {
#'         geom_smooth(method = "gam",
#'                     formula = y ~ s(x, k = gam_k), se = FALSE)
#'       } else {
#'         geom_smooth(method = smoothing_method,
#'                     formula = y ~ x, se = FALSE)
#'       }
#'     }, error = function(e) {
#'       cli::cli_alert_danger("Failed to add smoothing: {conditionMessage(e)}")
#'       cli::cli_alert_info("Plotting without smoothing...")
#'       geom_blank()
#'     })
#'
#'     cli::cli_alert_success("Smoothing added")
#'
#'     cli::cli_alert_info("Adding event lines and labels...")
#'
#'     # add vertical lines for event dates
#'     for (i in seq_along(event_dates)) {
#'       event_date <- event_dates[i]
#'       event_name <- if (!is.null(event_names) && length(event_names) >= i) event_names[i] else paste("Event", i)
#'
#'       p <- p + geom_vline(xintercept = event_date,
#'                           color = event_line_color,
#'                           alpha = event_line_alpha,
#'                           linetype = event_line_type,
#'                           linewidth = event_line_width)
#'
#'       # calculate y-position for the label to be at the top of the data points
#'       y_max <- max(df[[y_var]], na.rm = TRUE)
#'       y_min <- min(df[[y_var]], na.rm = TRUE)
#'       y_range <- y_max - y_min
#'       label_height <- nchar(event_name) * 0.015 * y_range  # Adjust this multiplier as needed
#'       y_position <- y_max
#'
#'       # calculate x-position slightly to the left of the event line
#'       x_offset <- 5  # adjust this value to move labels further left or right
#'       x_position <- event_date - x_offset
#'
#'       # add white rectangle with grey border behind the event label
#'       p <- p + annotate("rect",
#'                         xmin = x_position - 0.5,
#'                         xmax = x_position + 0.5,
#'                         ymin = y_position - label_height,
#'                         ymax = y_position,
#'                         fill = "white",
#'                         color = "grey50",
#'                         alpha = 0.9,
#'                         linewidth = 0.25)
#'
#'       # add text label for the event
#'       p <- p + annotate("text",
#'                         x = x_position,
#'                         y = y_position,
#'                         label = transform_label(event_name),
#'                         color = event_label_color,
#'                         size = event_label_size,
#'                         angle = 90,
#'                         vjust = 1,  # Align text to the top of the label box
#'                         hjust = 1)  # Align text to the right of the label box
#'     }
#'     cli::cli_alert_success("Event lines and labels added")
#'     cli::cli_alert_info("Saving plot...")
#'
#'     # save plot if a save path is provided
#'     if (!is.null(save_path)) {
#'       # generate filename
#'       filename <- paste0(
#'         "discontinuity_plot_", y_var, "_",
#'         format(Sys.Date(), "%Y%m%d")
#'       )
#'
#'       tryCatch({
#'         cli::cli_alert_info("Attempting to print plot...")
#'         print(p)
#'         cli::cli_alert_success("Plot printed successfully")
#'
#'         cli::cli_alert_info("Saving plot as PNG...")
#'         ggsave(
#'           plot = p,
#'           filename = file.path(save_path, paste0(filename, ".png")),
#'           width = width,
#'           height = height,
#'           units = "in",
#'           device = 'png',
#'           dpi = 400
#'         )
#'         cli::cli_alert_success("Plot saved as PNG successfully")
#'
#'         cli::cli_alert_info("Saving plot as .qs file...")
#'         # from the margot package (go-bayes/margot)
#'         margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)
#'
#'       }, error = function(e) {
#'         cli::cli_alert_danger("Error while saving: {conditionMessage(e)}")
#'       })
#'     } else {
#'       cli::cli_alert_info("No save path provided. Plot not saved.")
#'     }
#'
#'     cli::cli_alert_success("Margot plot discontinuity created successfully \U0001F44D")
#'
#'     # return the ggplot object
#'     return(p)
#'   }, error = function(e) {
#'     cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
#'     print(e)
#'     return(NULL)
#'   }, warning = function(w) {
#'     cli::cli_alert_warning("A warning occurred: {conditionMessage(w)}")
#'     print(w)
#'   })
#' }










# # # To convert the ggplot to plotly and save itr
# muslim_warmth_plotly <- convert_to_plotly(
#   muslim_warmth_plot,
#   save_path = here::here(path_talk)
# )





# 2. rrust in Police before/after COVID-19 (with custom point alpha)
police_trust_plot <- create_discontinuity_graph(
  data = dat,
  y_var = "police_trust",
  event_date = "2020-01-06",
  start_date = "2017-10-01",
  title = "Trust in Police: Pre/Post Covid-19 Attack",
  y_label = "Trust in Police",
  x_label = "NZAVS Time 9 - 14 Cohort (2017-2023)",
  point_alpha = 0.05,  # Custom point alpha
  save_path = here::here(path_talk)
)

# 3. Trust in Politicians before/after COVID-19 (with very low point alpha for high density plots)
politician_trust_plot <- create_discontinuity_graph(
  data = dat,
  y_var = "pol_politician_trust",
  event_date = "2020-01-06",
  start_date = "2017-10-01",
  title = "Trust in Politicians: Pre/Post Covid-19 Attack",
  y_label = "Trust in Politicians",
  x_label = "NZAVS Time 9 - 14 Cohort (2017-2023)",
  point_alpha = 0.05,  # Very low point alpha for high density
  save_path = here::here(path_talk)
)

politician_trust_plot
#
# library(dplyr)
# library(lubridate)
# library(ggplot2)
#
# # Set the base date
# base_date <- as.Date("2009-06-30")
#
#
# #  ensure  data has a 'day' column that correctly represents dates.
#
# df_timeline <- dat %>%
#   mutate(year = as.numeric(as.character(wave))) %>%
#   dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) %>%
#   dplyr:::count(day = floor_date(timeline, "day")) |>
#   mutate(nzavs_wave = factor(
#     case_when(
#       day >= as.Date("2009-08-30") & day < as.Date("2010-10-15") ~ "time 1",
#       day >= as.Date("2010-10-15") & day < as.Date("2011-10-15") ~ "time 2",
#       day >= as.Date("2011-10-15") & day < as.Date("2012-10-15") ~ "time 3",
#       day >= as.Date("2012-10-15") & day < as.Date("2013-10-15") ~ "time 4",
#       day >= as.Date("2013-10-15") & day < as.Date("2014-10-15") ~ "time 5",
#       day >= as.Date("2014-10-15") & day < as.Date("2015-10-15") ~ "time 6",
#       day >= as.Date("2015-10-15") & day < as.Date("2016-10-15") ~ "time 7",
#       day >= as.Date("2016-10-15") & day < as.Date("2017-10-15") ~ "time 8",
#       day >= as.Date("2017-10-15") & day < as.Date("2018-10-15") ~ "time 9",
#       day >= as.Date("2018-10-15") & day < as.Date("2019-10-15") ~ "time 10",
#       day >= as.Date("2019-10-15") & day < as.Date("2020-10-15") ~ "time 11",
#       day >= as.Date("2020-10-15") & day < as.Date("2021-10-15") ~ "time 12",
#       day >= as.Date("2021-10-15") & day < as.Date("2022-10-15") ~ "time 13",
#       day >= as.Date("2022-10-15") & day < as.Date("2023-10-15") ~ "time 14",
#       TRUE ~ NA_character_  # For days outside the defined waves
#     ))
#   ) |>
#   arrange(day, nzavs_wave)
#
# # check
# str(df_timeline)
#
# head(df_timeline)
#
# # check n
#
# # make timeline
# timeline_histgram_2009_2024 <-  ggplot(df_timeline, aes(x = day, y = n, fill = nzavs_wave)) +
#   geom_col() +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
#   labs(
#     title = "New Zealand Attitudes and Values Study (panel)",
#     subtitle = "N = 72,910; years 2012-2022",
#     x = "NZAVS years 2012- 2022 cohort (N = 72,910): daily counts by condition",
#     y = "Count of Responses"
#   ) +
#   theme_classic() +
#   scale_fill_viridis_d() +
#   theme(
#     legend.position = "none",
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 12)
#   )
#
#
# #  plot
# print(timeline_histgram_2009_2024)
#
# # save
# ggsave(
#   timeline_histgram_2009_2024,
#   path = path_talk,
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "timeline_histgram_2009_2024.png",
#   device = 'png',
#   limitsize = FALSE,
#   dpi = 400
# )
#
#
#
# # muslim warmth after attacks ---------------------------------------------
#
# tl <- dat %>%
#   select(warm_muslims, wave,tscore, sample_frame_opt_in,  id) %>%
#   mutate(year = as.numeric(as.character(wave))) |>
#   dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) %>%
#   dplyr::filter(timeline > "2012-06-06") %>%
#   # dplyr:::count(day = floor_date(timeline, "day"))%>%
#   dplyr::mutate(attack_condition = factor(
#     ifelse(timeline < "2019-03-15", 0, 1)
#   )) %>%
#   arrange(timeline, attack_condition)
#
# # check n
# length(unique(tl$id))
#
#
# # discontinuity plot
# muslim_attack_discontinuity_2012_2022_include_opt_in <-
#   ggplot(tl, aes(x = timeline, y = warm_muslims, color = attack_condition)) +
#   geom_jitter(alpha = .03, width = 1) +
#   stat_smooth(method = "gam") +
#   theme(legend.position = "bottom") +
#   labs(
#     title = "Discontinuity at attacks (GAM)",
#     subtitle = "Boost to Warmth increase in the years following the attacks: FULL SAMPLE",
#     y = "Muslim Warmth",
#     x = "NZAVS Time 4 - 14 Cohort (2012-2023), (N = 71,128)"
#   ) +
#   scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 12)
#   )
#
# muslim_attack_discontinuity_2012_2022_include_opt_in
#
# # save
# ggsave(
#   muslim_attack_discontinuity_2012_2022_include_opt_in,
#   # path = here::here("figures"),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename ="muslim_attack_discontinuity_2012_2022_include_opt_in.png",
#   device = 'png',
#   limitsize = TRUE,
#   dpi = 300
# )
#
#
#
# df_covid  <- dat %>%
#   select(conspiracy_beliefs, police_trust, science_trust, pol_politician_trust,
#          alert_level_combined, wave,tscore, sample_frame_opt_in,  id) %>%
#   mutate(year = as.numeric(as.character(wave))) |>
#   dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) %>%
#   dplyr::filter(timeline > "2017-10-01") %>%
#   # dplyr:::count(day = floor_date(timeline, "day"))%>%
#   dplyr::mutate(covid_19_attack = factor(
#     ifelse(timeline < "2020-01-06", 0, 1)
#   )) %>%
#   arrange(timeline, alert_level_combined)
#
# # check n
# length(unique(df_covid$id))
#
#
# # discontinuity plot
# plot_rdd_covid_trust_police <-
#   ggplot(df_covid, aes(x = timeline, y = police_trust, color = covid_19_attack)) +
#   geom_jitter(alpha = .03, width = 1) +
#   stat_smooth(method = "gam", formula = y ~ s(x, k = 4)) +
#   theme(legend.position = "bottom") +
#   labs(
#     title = "Trust in Police: Pre/Post Covid-19 Attack",
#     y = "Trust in Police",
#     x = "NZAVS Time 9 - 14 Cohort (2017-2023), (N = 64287)"
#   ) +
#   scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 12)
#   )
#
# plot_rdd_covid_trust_police
#
# # save
# ggsave(
#   plot_rdd_covid_trust_police,
#   path = path_talk,
#   width = 16,
#   height = 9,
#   units = "in",
#   filename ="plot_rdd_covid_trust_police.png",
#   device = 'png',
#   limitsize = TRUE,
#   dpi = 400
# )
#
#
#
# # discontinuity plot
# plot_rdd_covid_trust_politicians <-
#   ggplot(df_covid, aes(x = timeline, y = pol_politician_trust, color = covid_19_attack)) +
#   geom_jitter(alpha = .03, width = 1) +
#   stat_smooth(method = "gam", formula = y ~ s(x, k = 4)) +
#   theme(legend.position = "bottom") +
#   labs(
#     title = "Trust in Politicians:  Pre/Post Covid-19 Attack",
#     y = "Trust in Politicians",
#     x = "NZAVS Time 9 - 14 Cohort (2017-2023), (N = 64287)"
#   ) +
#   scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 12)
#   )
# plot_rdd_covid_trust_politicians
#
# # save
# ggsave(
#   plot_rdd_covid_trust_politicians,
#   path = path_talk,
#   width = 16,
#   height = 9,
#   units = "in",
#   filename ="plot_rdd_covid_trust_politicians.png",
#   device = 'png',
#   limitsize = TRUE,
#   dpi = 400
# )
#
#
#
# # discontinuity plot
# # science measure started in t11
# df_covid_science  <- dat %>%
#   select(conspiracy_beliefs, police_trust, science_trust, pol_politician_trust,
#          alert_level_combined, wave,tscore, sample_frame_opt_in,  id) %>%
#   mutate(year = as.numeric(as.character(wave))) |>
#   dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) %>%
#   dplyr::filter(timeline > "2019-10-01") %>%
#   # dplyr:::count(day = floor_date(timeline, "day"))%>%
#   dplyr::mutate(covid_19_attack = factor(
#     ifelse(timeline < "2020-01-06", 0, 1)
#   )) %>%
#   arrange(timeline, alert_level_combined)
#
# n_unique(df_covid_science$id)
#
# plot_rdd_covid_trust_science <-
#   ggplot(df_covid_science, aes(x = timeline, y = science_trust, color = covid_19_attack)) +
#   geom_jitter(alpha = .03, width = 1) +
#   stat_smooth(method = "gam", formula = y ~ s(x, k = 3)) +
#   # stat_smooth(method = "lm") +
#   theme(legend.position = "bottom") +
#   labs(
#     title = "Trust in Science: Pre/Post Covid-19 Attacks",
#     y = "Trust in Science",
#     x = "NZAVS Time 11 - 14 Cohort (2012-2023), (N = 55297)"
#   ) +
#   scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 12)
#   )
# plot_rdd_covid_trust_science
#
# # save
# ggsave(
#   plot_rdd_covid_trust_science,
#   path = path_talk,
#   width = 16,
#   height = 9,
#   units = "in",
#   filename ="plot_rdd_covid_trust_science.png",
#   device = 'png',
#   limitsize = TRUE,
#   dpi = 400
# )
#
# # conspiracy beliefs (also started in time 11)
#
#
# plot_rdd_covid_trust_conspiracy <-
#   ggplot(df_covid_science, aes(x = timeline, y = conspiracy_beliefs, color = covid_19_attack)) +
#   geom_jitter(alpha = .03, width = 1) +
#   stat_smooth(method = "gam", formula = y ~ s(x, k = 3)) +
#   theme(legend.position = "bottom") +
#   labs(
#     title = "Conspiracy Beliefs: Pre/Post Covid-19 Attacks",
#     y = "Conspiracy Beliefs",
#     x = "NZAVS Time 11 - 14 Cohort (2012-2023), (N = 55297)"
#   ) +
#   scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 12)
#   )
# plot_rdd_covid_trust_conspiracy
#
# # save
# ggsave(
#   plot_rdd_covid_trust_conspiracy,
#   path = path_talk,
#   width = 16,
#   height = 9,
#   units = "in",
#   filename ="plot_rdd_covid_trust_conspiracy.png",
#   device = 'png',
#   limitsize = TRUE,
#   dpi = 400
# )
