# NZAVS functions
# test data
# reproducibility
set.seed(123)

# set
push_mods <- here::here("/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/00-talks")
pull_path <- fs::path_expand("/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph Bulbulia/00Bulbulia Pubs/DATA/nzavs-current/r-data/nzavs_data_qs")

# read and preprocess data ------------------------------------------------
# import data
dat <- qs::qread(here::here(pull_path))


# read and preprocess data ------------------------------------------------
# import data
dat <- qs::qread(here::here(pull_path))
# Function to prepare panel data
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggokabeito)

# Function to prepare panel data (unchanged)
prepare_panel_data <- function(dat, base_date = as.Date("2009-06-30")) {
  df_timeline <- dat %>%
    mutate(year = as.numeric(as.character(wave))) %>%
    dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) %>%
    dplyr::count(day = floor_date(timeline, "day")) %>%
    mutate(nzavs_wave = factor(
      case_when(
        day >= as.Date("2009-08-30") & day < as.Date("2010-10-15") ~ "time 1",
        day >= as.Date("2010-10-15") & day < as.Date("2011-10-15") ~ "time 2",
        day >= as.Date("2011-10-15") & day < as.Date("2012-10-15") ~ "time 3",
        day >= as.Date("2012-10-15") & day < as.Date("2013-10-15") ~ "time 4",
        day >= as.Date("2013-10-15") & day < as.Date("2014-10-15") ~ "time 5",
        day >= as.Date("2014-10-15") & day < as.Date("2015-10-15") ~ "time 6",
        day >= as.Date("2015-10-15") & day < as.Date("2016-10-15") ~ "time 7",
        day >= as.Date("2016-10-15") & day < as.Date("2017-10-15") ~ "time 8",
        day >= as.Date("2017-10-15") & day < as.Date("2018-10-15") ~ "time 9",
        day >= as.Date("2018-10-15") & day < as.Date("2019-10-15") ~ "time 10",
        day >= as.Date("2019-10-15") & day < as.Date("2020-10-15") ~ "time 11",
        day >= as.Date("2020-10-15") & day < as.Date("2021-10-15") ~ "time 12",
        day >= as.Date("2021-10-15") & day < as.Date("2022-10-15") ~ "time 13",
        day >= as.Date("2022-10-15") & day < as.Date("2023-10-15") ~ "time 14",
        TRUE ~ NA_character_
      )
    )) %>%
    arrange(day, nzavs_wave)
  
  return(df_timeline)
}

# Updated function to create and save timeline histograms
reate_save_timeline_histogram <- function(df_timeline, 
                                          save_path = here::here("output"),
                                          width = 12,
                                          height = 8,
                                          base_filename = "timeline_histogram") {
  
  # Calculate year range
  year_range <- range(year(df_timeline$day), na.rm = TRUE)
  
  # Get the number of unique waves
  n_waves <- length(unique(df_timeline$nzavs_wave))
  
  # Create an extended color palette
  okabe_ito_palette <- palette.colors(palette = "Okabe-Ito")
  extended_palette <- c(
    okabe_ito_palette,
    "#FF9999", "#99FF99", "#9999FF", "#FFFF99", "#FF99FF"  # Add more colors as needed
  )
  
  # Ensure we have enough colors
  if (n_waves > length(extended_palette)) {
    stop("Not enough colors in the extended palette for all waves.")
  }
  
  # Create ggplot
  gg <- ggplot(df_timeline, aes(x = day, y = n, fill = nzavs_wave)) +
    geom_col() +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    labs(
      title = "New Zealand Attitudes and Values Study (panel)",
      subtitle = paste("N =", format(sum(df_timeline$n), big.mark = ","), 
                       "; years", year_range[1], "-", year_range[2]),
      x = paste("NZAVS years", year_range[1], "-", year_range[2], 
                "cohort: daily counts by condition"),
      y = "Count of Responses"
    ) +
    theme_classic() +
    scale_fill_manual(values = extended_palette[1:n_waves]) +  # Use extended palette
    theme(
      legend.position = "none",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12)
    )
  
  # Save ggplot
  ggsave(
    plot = gg,
    filename = file.path(save_path, paste0(base_filename, "_ggplot.png")),
    width = width,
    height = height,
    units = "in",
    dpi = 400
  )
  
  # Create and save plotly
  p <- ggplotly(gg)
  htmlwidgets::saveWidget(p, file.path(save_path, paste0(base_filename, "_plotly.html")))
  
  # Return both plots
  list(ggplot = gg, plotly = p)
}


dat <- qs::qread(here::here(pull_path))
df_timeline <- prepare_panel_data(dat)
nzavs_timeline <- create_save_timeline_histogram(df_timeline,  save_path = here::here(push_mods))

nzavs_timeline$ggplot
nzavs_timeline$plotly

  
  
# print(results$ggplot)
# print(results$plotly)
# example usage
# impport data

results <- create_timeline_histogram(dat, 
                                     end_date = "2023-10-15", 
                                     wave_interval = "1 Year")

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
