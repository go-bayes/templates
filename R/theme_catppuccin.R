# theme_catppuccin.R
# catppuccin mocha ggplot2 theme and palettes
# source this file to use: source("~/GIT/templates/R/theme_catppuccin.R")

library(ggplot2)

# catppuccin mocha palette
ctp_mocha <- list(
 rosewater = "#f5e0dc",
 flamingo = "#f2cdcd",
 pink = "#f5c2e7",
 mauve = "#cba6f7",
 red = "#f38ba8",
 maroon = "#eba0ac",
 peach = "#fab387",
 yellow = "#f9e2af",
 green = "#a6e3a1",
 teal = "#94e2d5",
 sky = "#89dceb",
 sapphire = "#74c7ec",
 blue = "#89b4fa",
 lavender = "#b4befe",
 text = "#cdd6f4",
 subtext1 = "#bac2de",
 subtext0 = "#a6adc8",
 overlay2 = "#9399b2",
 overlay1 = "#7f849c",
 overlay0 = "#6c7086",
 surface2 = "#585b70",
 surface1 = "#45475a",
 surface0 = "#313244",
 base = "#1e1e2e",
 mantle = "#181825",
 crust = "#11111b"
)

# discrete colour palette (accent colours)
ctp_palette <- c(
 ctp_mocha$green,
 ctp_mocha$yellow,
 ctp_mocha$red,
 ctp_mocha$blue,
 ctp_mocha$peach,
 ctp_mocha$mauve,
 ctp_mocha$teal,
 ctp_mocha$pink,
 ctp_mocha$sapphire,
 ctp_mocha$flamingo,
 ctp_mocha$lavender,
 ctp_mocha$maroon
)

# ggplot2 theme for dark catppuccin slides
theme_catppuccin <- function(base_size = 14, base_family = "") {
 ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
   ggplot2::theme(
     # background
     plot.background = ggplot2::element_rect(fill = ctp_mocha$base, colour = NA),
     panel.background = ggplot2::element_rect(fill = ctp_mocha$base, colour = NA),

     # grid
     panel.grid.major = ggplot2::element_line(colour = ctp_mocha$surface0, linewidth = 0.3),
     panel.grid.minor = ggplot2::element_blank(),

     # axes
     axis.text = ggplot2::element_text(colour = ctp_mocha$text),
     axis.title = ggplot2::element_text(colour = ctp_mocha$subtext1),
     axis.ticks = ggplot2::element_line(colour = ctp_mocha$surface1),

     # titles
     plot.title = ggplot2::element_text(colour = ctp_mocha$lavender, face = "bold"),
     plot.subtitle = ggplot2::element_text(colour = ctp_mocha$mauve),
     plot.caption = ggplot2::element_text(colour = ctp_mocha$overlay1),

     # legend
     legend.background = ggplot2::element_rect(fill = ctp_mocha$base, colour = NA),
     legend.key = ggplot2::element_rect(fill = ctp_mocha$base, colour = NA),
     legend.text = ggplot2::element_text(colour = ctp_mocha$text),
     legend.title = ggplot2::element_text(colour = ctp_mocha$peach),

     # facets
     strip.background = ggplot2::element_rect(fill = ctp_mocha$surface0, colour = NA),
     strip.text = ggplot2::element_text(colour = ctp_mocha$text),

     # border
     panel.border = ggplot2::element_blank()
   )
}

# light theme variant (for non-dark presentations)
theme_catppuccin_light <- function(base_size = 14, base_family = "") {
 # catppuccin latte palette
 latte_base <- "#eff1f5"
 latte_text <- "#4c4f69"
 latte_surface0 <- "#ccd0da"
 latte_surface1 <- "#bcc0cc"
 latte_overlay1 <- "#8c8fa1"
 latte_lavender <- "#7287fd"
 latte_mauve <- "#8839ef"
 latte_peach <- "#fe640b"

 ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
   ggplot2::theme(
     plot.background = ggplot2::element_rect(fill = latte_base, colour = NA),
     panel.background = ggplot2::element_rect(fill = latte_base, colour = NA),
     panel.grid.major = ggplot2::element_line(colour = latte_surface0, linewidth = 0.3),
     panel.grid.minor = ggplot2::element_blank(),
     axis.text = ggplot2::element_text(colour = latte_text),
     axis.title = ggplot2::element_text(colour = latte_text),
     plot.title = ggplot2::element_text(colour = latte_lavender, face = "bold"),
     plot.subtitle = ggplot2::element_text(colour = latte_mauve),
     plot.caption = ggplot2::element_text(colour = latte_overlay1),
     legend.background = ggplot2::element_rect(fill = latte_base, colour = NA),
     legend.key = ggplot2::element_rect(fill = latte_base, colour = NA),
     legend.text = ggplot2::element_text(colour = latte_text),
     legend.title = ggplot2::element_text(colour = latte_peach),
     strip.background = ggplot2::element_rect(fill = latte_surface0, colour = NA),
     strip.text = ggplot2::element_text(colour = latte_text),
     panel.border = ggplot2::element_blank()
   )
}

# colour scale for discrete variables
scale_colour_ctp <- function(...) {
 ggplot2::scale_colour_manual(values = ctp_palette, ...)
}

scale_color_ctp <- scale_colour_ctp

# fill scale for discrete variables
scale_fill_ctp <- function(...) {
 ggplot2::scale_fill_manual(values = ctp_palette, ...)
}

# example usage:
# source("~/GIT/templates/R/theme_catppuccin.R")
#
# ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
#   geom_point(size = 3) +
#   scale_colour_ctp() +
#   theme_catppuccin() +
#   labs(title = "fuel efficiency", colour = "cylinders")
