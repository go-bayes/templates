# NZAVS functions
# test data
# reproducibility
set.seed(123)
#detach("package:margot", unload = TRUE)
devtools::install_github("go-bayes/margot")
library(margot)
packageVersion(pkg = 'margot')# set
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)
library(cli)
push_mods <- here::here("/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/00-talks")

path_talk = push_mods
pull_path <- fs::path_expand("/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph Bulbulia/00Bulbulia Pubs/DATA/nzavs-current/r-data/nzavs_data_qs")

# read and preprocess data ------------------------------------------------
# import data
dat <- qs::qread(here::here(pull_path))

dat <- dat |> mutate(doubts_official_version_truth = conspiracy_beliefs)


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
  width = 12,
  height = 8,
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
  width = 12,
  height = 8,
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
  width = 12,
  height = 8,
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
  width = 12,
  height = 8,
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
  width = 12,
  height = 8,
  save_path = here::here(push_mods)
)

# view
discontinuity_plot_trust_science_in_society

# path: discontinuity_plot_trust_science_our_society_places_too_much_emphasis_reversed_20240902


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
  width = 12,
  height = 8,
  save_path = here::here(push_mods)
)
# path: discontinuity_plot_free_speech_20240902.qs

# view
discontinuity_plot_free_speech

# 6. Conspiracy Beliefs before/after COVID-19
discontinuity_plot_doubts_official_version_truth <- margot_plot_discontinuity(
  data = dat,
  y_var = "doubts_official_version_truth",
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
  width = 12,
  height = 8,
  save_path = here::here(push_mods)
)
discontinuity_plot_conspiracy_beliefs

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

warmth_time <- margot_plot_slope(
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
   y_label = "Warmth",
  width = 12,
  height = 8
 # x_label = "NZAVS Waves",
 # use_facets = FALSE
  )
muslims_and_others

test_2 <- margot_plot_boxplot(
  data = dat,
  y_vars = list("warm_muslims", "warm_asians", "warm_indians", "warm_immigrants"),
  x_label = "NZAVS Waves",
  x_var = "wave"
  width = 12,
  height = 8
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
  width = 12,
  height = 8
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
  width = 12,
  height = 8
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
  color_label = "Work Hours",
  width = 12,
  height = 8
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
  facet_scales = "free",
  width = 12,
  height = 8
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

this_works <- margot_plot_boxplot(data = dat, y_vars = c("warm_muslims", "warm_immigrants"))

this_works

this_also_works <- margot_plot_boxplot(data = dat, y_vars = c("warm_muslims", "warm_immigrants"), coord_flip = TRUE)

this_also_works

this_also_works <- margot_plot_boxplot(data = dat, y_vars = c("warm_muslims", "warm_immigrants"), wave = c("2012", "2022"))
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





# plot individual responses -----------------------------------------------
margot_plot_individual_responses <- function(data,
                                             y_vars,
                                             id_col = "id",
                                             wave_col = "wave",
                                             waves = NULL,
                                             data_fraction = 1,
                                             random_draws = NULL,
                                             title = NULL,
                                             y_label = NULL,
                                             x_label = NULL,
                                             color_palette = NULL,
                                             theme = theme_classic(),
                                             include_timestamp = FALSE,
                                             save_path = NULL,
                                             width = 16,
                                             height = 8,
                                             seed = NULL,
                                             wave_label_angle = 45,
                                             full_response_scale = TRUE,
                                             scale_range = NULL,
                                             prefix = NULL,
                                             jitter_amount = 0.05,
                                             legend_position = "top") {

  cli::cli_h1("Margot Plot Individual Responses")

  # Check for required columns
  required_cols <- c(id_col, wave_col, y_vars)
  if (!all(required_cols %in% colnames(data))) {
    missing_cols <- setdiff(required_cols, colnames(data))
    cli::cli_alert_danger("Missing required columns: {paste(missing_cols, collapse = ', ')}")
    return(NULL)
  }

  # Prepare the data
  cli::cli_alert_info("Preparing data...")

  # Filter waves if specified
  if (!is.null(waves)) {
    data <- data[data[[wave_col]] %in% waves, ]
  }

  # Determine number of waves
  unique_waves <- unique(data[[wave_col]])
  num_waves <- length(unique_waves)

  # Calculate eligibility
  cli::cli_alert_info("Calculating eligibility...")

  # Function to check if an ID meets the eligibility criteria
  check_eligibility <- function(id_data) {
    all(!is.na(id_data[, y_vars])) && nrow(id_data) == num_waves
  }

  # Apply eligibility criteria
  eligible_ids <- data %>%
    group_by(!!sym(id_col)) %>%
    group_map(~ if(check_eligibility(.x)) .y[[id_col]] else NULL) %>%
    unlist()

  data <- data[data[[id_col]] %in% eligible_ids, ]

  # Check if there's any data left after filtering
  if (nrow(data) == 0) {
    cli::cli_alert_danger("No data left after applying eligibility criteria.")
    return(NULL)
  }

  # Sample IDs if data_fraction < 1 or random_draws is specified
  if (data_fraction < 1 || !is.null(random_draws)) {
    cli::cli_alert_info("Sampling IDs...")
    if (!is.null(seed)) set.seed(seed)

    unique_ids <- unique(data[[id_col]])
    n_ids <- length(unique_ids)

    if (!is.null(random_draws)) {
      sample_size <- min(random_draws, n_ids)
    } else {
      sample_size <- max(1, round(n_ids * data_fraction))
    }

    sampled_ids <- sample(unique_ids, size = sample_size)
    data <- data[data[[id_col]] %in% sampled_ids, ]
  }

  cli::cli_alert_info("Pivoting data...")
  df <- tidyr::pivot_longer(data, cols = all_of(y_vars), names_to = "variable", values_to = "value")

  # Remove rows with missing values
  df <- df[!is.na(df$value), ]

  df$variable <- gsub("_", " ", df$variable)
  df$variable <- tools::toTitleCase(df$variable)

  # Determine y-axis limits if full_response_scale is TRUE
  if (full_response_scale) {
    cli::cli_alert_info("Calculating response scale limits...")
    if (is.null(scale_range)) {
      y_limits <- range(df$value, na.rm = TRUE, finite = TRUE)
      if (!all(is.finite(y_limits))) {
        cli::cli_alert_warning("Unable to determine y-axis limits from data. Using default range 0 to 1.")
        y_limits <- c(0, 1)
      }
    } else {
      if (length(scale_range) != 2 || !is.numeric(scale_range) || scale_range[1] >= scale_range[2]) {
        cli::cli_alert_danger("Invalid scale_range. Using default range 0 to 1.")
        y_limits <- c(0, 1)
      } else {
        y_limits <- scale_range
      }
    }
  } else {
    y_limits <- NULL
  }

  # Create the plot
  cli::cli_alert_info("Creating plot...")

  p <- tryCatch({
    ggplot(df, aes(x = !!sym(wave_col), y = value, color = variable, group = interaction(!!sym(id_col), variable))) +
      geom_point(position = position_jitter(height = jitter_amount, width = 0)) +
      geom_line(position = position_jitter(height = jitter_amount, width = 0),
                data = function(d) {
                  d %>%
                    group_by(!!sym(id_col), variable) %>%
                    filter(n() > 1) %>%
                    ungroup()
                }) +
      facet_wrap(as.formula(paste("~", id_col))) +
      theme +
      theme(axis.text.x = element_text(angle = wave_label_angle, hjust = 1),
            legend.position = legend_position) +
      labs(title = title,
           y = y_label %||% "Value",
           x = x_label %||% "Wave",
           color = "Variable")
  }, error = function(e) {
    cli::cli_alert_danger("Error creating plot: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(p)) {
    return(NULL)
  }

  # Apply y-axis limits if full_response_scale is TRUE
  if (full_response_scale && !is.null(y_limits)) {
    y_range <- diff(y_limits)
    p <- p + coord_cartesian(ylim = c(y_limits[1] - y_range * 0.05, y_limits[2] + y_range * 0.05))
  }

  # Apply color palette
  if (is.null(color_palette)) {
    color_palette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",
                       "#D55E00", "#CC79A7", "#000000", "#999999")
  }
  p <- p + scale_color_manual(values = color_palette)

  # Save plot if a save path is provided
  if (!is.null(save_path)) {
    cli::cli_alert_info("Saving plot...")
    tryCatch({
      filename <- "individual_responses_plot"

      if (!is.null(prefix) && nzchar(prefix)) {
        filename <- paste0(prefix, "_", filename)
      }

      filename <- paste0(filename, "_", paste(y_vars, collapse = "_"))

      if (include_timestamp) {
        filename <- paste0(filename, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      }

      full_path_png <- file.path(save_path, paste0(filename, ".png"))
      ggsave(
        plot = p,
        filename = full_path_png,
        width = width,
        height = height,
        units = "in",
        dpi = 300
      )
      cli::cli_alert_success("Plot saved as PNG: {.file {full_path_png}}")

      margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)
      full_path_qs <- file.path(save_path, paste0(filename, ".qs"))
      cli::cli_alert_success("Plot object saved using qs: {.file {full_path_qs}}")

    }, error = function(e) {
      cli::cli_alert_danger("An error occurred while saving the plot: {conditionMessage(e)}")
    })
  } else {
    cli::cli_alert_info("No save path provided. Plot not saved.")
  }

  cli::cli_alert_success("Plot created successfully \U0001F44D")

  # Return the plot object directly
  return(p)
}
plot_individual_warmth_muslims <- margot_plot_individual_responses(data = dat,
                                                                   random_draws = 20,
                                                                   y_vars = "warm_muslims",
                                                                   waves = c(2016:2022),
                                                                   seed = 123)
plot_individual_warmth_muslims



plot_individual_bmi <- margot_plot_individual_responses(
  data = dat,
  random_draws = 50,
  y_vars = "hlth_bmi",
  waves = c(2010:2022),
  seed = 123
)
plot_individual_bmi



plot_individual_religion_identification_level <- margot_plot_individual_responses(
  data = dat,
  random_draws = 100,
  y_vars = "religion_identification_level",
  waves = c(2010:2022),
  seed = 123
)
plot_individual_religion_identification_level
+
