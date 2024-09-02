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


# test --------------------------------------------------------------------

#' Visualize Shifts in Data Distributions with Highlighted Ranges
#'
#' This function creates a histogram that highlights a specified range of values to visualize shifts in data distributions.
#' The highlighted range can indicate areas of interest, such as shifts up or down in the distribution.
#' This visualization is useful for understanding the implications of causal contrasts, such as modified treatment policies.
#' The fill colour of the histogram is dynamically adjusted based on the specified direction of the shift.
#'
#' @param df A dataframe containing the variable of interest.
#' @param col_name The name of the column in `df` to be visualized in the histogram. This should be a numeric variable.
#' @param binwidth The width of the bins for the histogram. Default is 1. Adjust this based on the distribution and scale of your data.
#' @param range_highlight A numeric vector of length 2 specifying the start and end of the range to highlight. If `NULL`, no range is highlighted.
#' @param shift A character string indicating the direction of the shift, either "up" or "down". Default is "up".
#' @param show_avg_line A logical value indicating whether to display a vertical line representing the average value. Default is `TRUE`.
#' @param title An optional custom title for the plot. If NULL, a default title will be generated.
#' @param subtitle An optional custom subtitle for the plot. If NULL, a default subtitle will be generated.
#' @param x_lab An optional label for the x-axis. If NULL, the formatted column name is used.
#' @param y_lab The label for the y-axis. Default is "Count".
#' @param save_path An optional path to save the plot. If NULL, the plot will not be saved.
#' @param width The width of the saved plot in inches. Default is 10.
#' @param height The height of the saved plot in inches. Default is 6.
#'
#' @return A ggplot object representing the histogram with specified highlights.
#'
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_h1
#' @importFrom margot here_save_qs
#' @importFrom stringr str_to_title
#'
#' @export
margot_plot_shift <- function(df, col_name, binwidth = 1, range_highlight = NULL, shift = "up", show_avg_line = TRUE,
                              title = NULL, subtitle = NULL, x_lab = NULL, y_lab = "Count",
                              save_path = NULL, width = 10, height = 6) {


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



# functions ---------------------------------------------------------------
  #' Visualize Shifts in Data Distributions with Highlighted Ranges
  #'
  #' This function creates a histogram that highlights a specified range of values to visualize shifts in data distributions.
  #' The highlighted range can indicate areas of interest, such as shifts up or down in the distribution.
  #' This visualization is useful for understanding the implications of causal contrasts, such as modified treatment policies.
  #' The fill colour of the histogram is dynamically adjusted based on the specified direction of the shift.
  #'
  #' @param df A dataframe containing the variable of interest.
  #' @param col_name The name of the column in `df` to be visualized in the histogram. This should be a numeric variable.
  #' @param binwidth The width of the bins for the histogram. Default is 1. Adjust this based on the distribution and scale of your data.
  #' @param range_highlight A numeric vector of length 2 specifying the start and end of the range to highlight. If `NULL`, no range is highlighted.
  #' @param shift A character string indicating the direction of the shift, either "up" or "down". Default is "up".
  #' @param show_avg_line A logical value indicating whether to display a vertical line representing the average value. Default is `TRUE`.
  #' @param title An optional custom title for the plot. If NULL, a default title will be generated.
  #' @param subtitle An optional custom subtitle for the plot. If NULL, a default subtitle will be generated.
  #' @param x_lab An optional label for the x-axis. If NULL, the formatted column name is used.
  #' @param y_lab The label for the y-axis. Default is "Count".
  #' @param save_path An optional path to save the plot. If NULL, the plot will not be saved.
  #' @param width The width of the saved plot in inches. Default is 10.
  #' @param height The height of the saved plot in inches. Default is 6.
  #'
  #' @return A ggplot object representing the histogram with specified highlights.
  #'
  #' @import ggplot2
  #' @importFrom rlang sym
  #' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_h1
  #' @importFrom margot here_save_qs
  #' @importFrom stringr str_to_title
  #'
  #' @export
  margot_plot_shift <- function(df, col_name, binwidth = 1, range_highlight = NULL, shift = "up", show_avg_line = TRUE,
                                title = NULL, subtitle = NULL, x_lab = NULL, y_lab = "Count",
                                save_path = NULL, width = 10, height = 6) {

    cli::cli_h1("Margot Plot Shift")

    tryCatch({
      # Input validation
      if(!col_name %in% names(df)) {
        cli::cli_alert_danger("Column '{col_name}' not found in the dataframe.")
        return(NULL)
      }
      if(all(is.na(df[[col_name]]))) {
        cli::cli_alert_danger("The specified column contains only NA values.")
        return(NULL)
      }
      if(!shift %in% c("up", "down")) {
        cli::cli_alert_danger("'shift' must be either 'up' or 'down'.")
        return(NULL)
      }

      # Function to convert to title case and remove underscores
      format_label <- function(x) {
        stringr::str_to_title(gsub("_", " ", x))
      }

      # Format column name for labels
      formatted_col_name <- format_label(col_name)

      # Calculate average value for the vertical line
      avg_val <- mean(df[[col_name]], na.rm = TRUE)
      cli::cli_alert_info("Average value of {formatted_col_name}: {round(avg_val, 2)}")

      # Determine the fill colour based on the shift direction
      highlight_color <- if(shift == "up") "gold2" else "dodgerblue"

      # Create a new column for fill colour based on range_highlight
      if (!is.null(range_highlight) && length(range_highlight) == 2) {
        df$fill_color <- ifelse(df[[col_name]] >= range_highlight[1] & df[[col_name]] <= range_highlight[2], highlight_color, "lightgray")
        cli::cli_alert_info("Highlighting range: [{range_highlight[1]}, {range_highlight[2]}]")
      } else {
        df$fill_color <- "lightgray" # Default colour if no range_highlight is provided
        cli::cli_alert_info("No range highlighted")
      }

      # Define subtitle based on the shift direction
      if (is.null(subtitle)) {
        subtitle <- if(shift == "up") {
          "Highlights region shifted up to boundary with grey"
        } else {
          "Highlights region shifted down to boundary with grey"
        }
        if(show_avg_line) {
          subtitle <- paste(subtitle, "\nRed dashed line shows the average value")
        }
      }

      # Create the plot
      p <- ggplot(df, aes(x = !!rlang::sym(col_name), fill = fill_color)) +
        geom_histogram(binwidth = binwidth, alpha = 0.7) +
        scale_fill_identity() +
        labs(title = ifelse(is.null(title), paste("Distribution of", formatted_col_name, "with Shift Intervention"), title),
             subtitle = subtitle,
             x = ifelse(is.null(x_lab), formatted_col_name, x_lab),
             y = y_lab,
             caption = sprintf("N = %d observations", nrow(df))) +
        theme_minimal() +
        theme(text = element_text(size = 12),
              axis.text.x = element_text(angle = 45, hjust = 1))

      # Conditionally add the average value line
      if(show_avg_line) {
        p <- p + geom_vline(xintercept = avg_val, color = "darkred", linetype = "dashed", linewidth = .75)
      }

      # Save plot if a save path is provided
      if (!is.null(save_path)) {
        filename <- paste0(
          "shift_", col_name, "_", shift,
          "_", format(Sys.Date(), "%Y%m%d")
        )

        cli::cli_alert_info("Saving plot...")

        ggsave(
          plot = p,
          filename = file.path(save_path, paste0(filename, ".png")),
          width = width,
          height = height,
          units = "in",
          device = 'png',
          dpi = 300
        )

        margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)

        cli::cli_alert_success("Plot saved successfully as '{filename}' in '{save_path}'")
      } else {
        cli::cli_alert_info("No save path provided. Plot not saved.")
      }

      cli::cli_alert_success("Margot plot shift created successfully \U0001F44D")

      return(p)
    }, error = function(e) {
      cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
      print(e)
      return(NULL)
    })
  }

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

# path: 'timeline_histogram.qs'
# Display the plot
nzavs_timeline

# end ---------------------------------------------------------------------


# discontinuity graphs ----------------------------------------------------
# 1. Muslim warmth after attacks
discontinuity_plot_muslim_warmth <- margot_plot_discontinuity(
  data = dat,
  y_var = "warm_muslims",
  event_dates = c("2019-03-15", "2020-03-26"),
  event_names = c("Christchurch Attack", "COVID-19 Lockdown"),
  start_date = "2012-06-06",
 # title = "Discontinuity Christchurch Attaks & Covid 19",
#  y_label = "Muslim Warmth",
  x_label = "NZAVS Time 4 - 14 Cohort (2012-2023)",
  point_alpha = 0.02,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)
# file: discontinuity_plot_warm_muslims_20240902.qs

# view
discontinuity_plot_muslim_warmth

# 2. Trust in Police before/after COVID-19
discontinuity_plot_trust_police <- margot_plot_discontinuity(
  data = dat,
  y_var = "police_trust",
  event_dates = c("2020-03-26"),
 # title = "Trust in Police: Pre/Post Covid-19",
  event_names = c("COVID-19 Lockdown"),
 # y_label = "Trust in Police",
  x_label = "NZAVS Waves",
  point_alpha = 0.02,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)

# view
# path: discontinuity_plot_police_trust_20240902
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
  point_alpha = 0.02,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)

# path: discontinuity_plot_pol_politician_trust_20240902.qs
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
  point_alpha = 0.02,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)

# path: discontinuity_plot_trust_science_high_confidence_scientific_community_20240902.qs

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
  y_label = "Values Place of Science in Society",
  x_label = "NZAVS Waves",
  point_alpha = 0.02,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)

# path: discontinuity_plot_trust_science_our_society_places_too_much_emphasis_reversed_20240902

# view
discontinuity_plot_trust_science_in_society /discontinuity_plot_trust_science_community

# Free speech
# 5. Free Speech before/after COVID-19
discontinuity_plot_free_speech <- margot_plot_discontinuity(
  data = dat,
  y_var = "free_speech",
  event_dates = c("2020-03-26"),
  event_names = c("COVID-19 Lockdown"),
 # start_date = "2019-10-01",
 # y_label = "",
  x_label = "NZAVS Waves",
  point_alpha = 0.02,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)
# path: discontinuity_plot_free_speech_20240902.qs

# view
discontinuity_plot_free_speech

# 6. Conspiracy Beliefs before/after COVID-19
discontinuity_plot_conspiracy_beliefs <- margot_plot_discontinuity(
  data = dat,
  y_var = "conspiracy_beliefs",
  event_dates = c("2020-03-26"),
  event_names = c("COVID-19 Lockdown"),
  start_date = "2019-10-01",
 # y_label = "Trust in Scientific Community",
  x_label = "NZAVS Waves",
  point_alpha = 0.02,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  event_label_size = 4,
  seed = 123,
  save_path = here::here(push_mods)
)

# path: discontinuity_plot_conspiracy_beliefs_20240902.qs

# view
discontinuity_plot_conspiracy_beliefs



# test
# get warm col names
# Get the original column names
warm_column_names <- names(dat)[grep("^warm", names(dat))]

# Create the list of quoted names
warm_names_list <- as.list(warm_column_names)  # No need for extra quotes

# Use in your function

all <- margot_plot_slope(
  data = dat,
  y_vars = warm_names_list,
  y_label = "Warmth",
  x_label = "NZAVS Waves",
  event_dates = c("2019-03-15"),
  event_names = c("Christchurch Attacks"),
)

all


print( warm_names_list)

non_prototypical  <- margot_plot_slope(
  data = dat,
  y_vars = list("warm_nz_euro", "warm_maori", "warm_pacific"), #"conspiracy_beliefs",
  # event_dates = c("2020-03-26"),
  # event_names = c("COVID-19 Lockdown"),
  # event_dates = c("2019-03-15"),
  # event_names = c("Christchurch Attacks"),
  y_label = "Warmth",
  x_label = "NZAVS Waves",
)
non_prototypical


muslims_and_others <- margot::margot_plot_slope(
  data = dat,
  y_vars = list("warm_muslims", "warm_asians", "warm_indians", "warm_immigrants"), #"conspiracy_beliefs",
  # event_dates = c("2020-03-26"),
  # event_names = c("COVID-19 Lockdown"),
   y_label = "Warmth"#,
 # x_label = "NZAVS Waves",
 # use_facets = FALSE
  )
muslims_and_others

test_2 <- margot_plot_boxplot(
  data = dat,
  y_vars = list("warm_muslims", "warm_asians", "warm_indians", "warm_immigrants"),
  x_label = "NZAVS Waves",
  x_var = "wave"
)
print( test_2 )

test_3 <- margot_plot_slope_covariate(
  data = dat,
  formula = warm_muslims ~ wave:ethnic_cats,
  terms = c("wave", "ethnic_cats"),
  #title = "Warmth Towards Muslims by Work Hours Over Time",
  y_label = "Warmth",
  x_label = "Wave",
  color_label = "Work Hours"
)
test_3

test_4 <- margot_plot_slope_covariate(
  data = dat,
  formula = warm_muslims ~ wave:political_conservative,
  terms = c("wave", "political_conservative"),
  #title = "Warmth Towards Muslims by Work Hours Over Time",
  y_label = "Warmth",
  x_label = "Wave",
  color_label = "Work Hours"
)
test_4
sort(colnames(dat))
dat$political_right_wing
test_5 <- margot_plot_slope_covariate(
  data = dat,
  formula = warm_muslims ~ wave:pol_wing,
  terms = c("wave", "pol_wing"),
  #title = "Warmth Towards Muslims by Work Hours Over Time",
 # y_label = "Warmth",
#  x_label = "Wave",
 # color_label = "Work Hours"
)
test_5

# cool
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

# For multiple waves
test_multi <- margot_plot_histogram(
  data = dat,
  col_names = c("issue_regulate_ai", "warm_immigrants"),
  id_col = "id",
  wave_col = "wave",
  waves = c(2018, 2020, 2021),
  binwidth = 0.5,
  facet_scales = "free"
)

test_multi

# For a single wave
test_single <- margot_plot_histogram_sd(
  data = dat,
  col_names = c("issue_regulate_ai", "warm_immigrants"),
  id_col = "id",
  wave_col = "wave",
  waves = 2019,
  binwidth = 0.5,
  facet_scales = "free"
)

test_single
test <- margot_plot_histogram_sd(
  data = dat,
  col_names = c("issue_regulate_ai"),
  id_col = "id",
  wave_col = "wave",
  waves = c(2018),
  binwidth = 0.5,
  facet_scales = "free",
  save_path = here::here(push_mods),
  color_palette = c("gold2", "dodgerblue")
)
print(test)




# boxplot test ------------------------------------------------------------


dat_2022 <- dat |> filter(wave == 2022, year_measured == 1)


dat_test<- dat |> dplyr::filter(wave %in% c(2018,2020,2021)) |> select("warm_immigrants", "issue_regulate_ai", "id", "wave") |> drop_na()

dat_test

this_works <- margot_plot_boxplot(data = dat, y_vars = c("warm_muslims", "warm_immigrants"), x_var = "wave")

this_works

this_also_works <- margot_plot_boxplot(data = dat_2022, y_vars = c("warm_muslims", "warm_immigrants"), x_var = "wave")

this_also_works

this_works_too <- margot_plot_boxplot(data = dat_2022, y_vars = "warm_muslims", x_var = "wave")

this_works_too
# multiple waves
this_works_exclm <- margot_plot_boxplot(data = dat, y_vars = "warm_muslims", x_var = "wave")

this_works_exclm

sort ( colnames(dat) )

dat_2022 <- dat |> filter(wave == 2022, year_measured == 1) |> select("issue_regulate_ai", "wave")


dat_2022

reg_ai_boxplot <- margot_plot_boxplot(data = dat, y_vars = "issue_regulate_ai", x_var = "wave",
                              show_points = FALSE, point_alpha = .01)
reg_ai_boxplot

library(ggplot2)
hist <- margot_plot_histogram_sd(dat_2022, col_name = "issue_regulate_ai", binwidth = .5)
hist

interactive_hist<- plotly::ggplotly(hist)
interactive_hist
