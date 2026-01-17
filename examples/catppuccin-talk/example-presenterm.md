---
title: Catppuccin Theme Example
sub_title: A cosy presentation template
author: Joseph A. Bulbulia
theme:
  name: catppuccin-mocha
---

<!-- speaker_note: welcome everyone. this talk demonstrates the catppuccin mocha theme for presenterm. -->

# overview

this example demonstrates the **catppuccin mocha** theme for presenterm presentations.

- warm, pastel colours
- easy on the eyes
- runs in the terminal

<!-- speaker_note: speaker notes appear here. run with publish-speaker-notes and a second terminal with listen-speaker-notes -->

<!-- end_slide -->

# code example

```r
# fit a simple linear model
library(ggplot2)

model <- lm(mpg ~ wt + hp, data = mtcars)
summary(model)
```

<!-- speaker_note: this slide demonstrates syntax highlighting for R code. -->

<!-- end_slide -->

# ggplot2: scatter plot

```r +exec +image
library(ggplot2)

# catppuccin mocha colours
ctp_base <- "#1e1e2e"
ctp_text <- "#cdd6f4"
ctp_mauve <- "#cba6f7"
ctp_surface0 <- "#313244"

ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_colour_manual(
    values = c("4" = "#a6e3a1", "6" = "#f9e2af", "8" = "#f38ba8"),
    name = "cylinders"
  ) +
  labs(title = "fuel efficiency by weight", x = "weight (1000 lbs)", y = "mpg") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = ctp_base, colour = NA),
    panel.background = element_rect(fill = ctp_base, colour = NA),
    panel.grid.major = element_line(colour = ctp_surface0),
    panel.grid.minor = element_blank(),
    text = element_text(colour = ctp_text),
    axis.text = element_text(colour = ctp_text),
    plot.title = element_text(colour = ctp_mauve),
    legend.background = element_rect(fill = ctp_base),
    legend.text = element_text(colour = ctp_text)
  )
```

<!-- speaker_note: press ctrl+e to execute this code and see the plot. -->

<!-- end_slide -->

# ggplot2: bar chart

```r +exec +image
library(ggplot2)

ctp_base <- "#1e1e2e"
ctp_text <- "#cdd6f4"
ctp_mauve <- "#cba6f7"
ctp_surface0 <- "#313244"
ctp_palette <- c("#a6e3a1", "#f9e2af", "#f38ba8", "#89b4fa", "#fab387", "#cba6f7", "#94e2d5")

ggplot(mpg, aes(x = class, fill = class)) +
  geom_bar() +
  scale_fill_manual(values = ctp_palette) +
  labs(title = "vehicle classes in mpg dataset", x = "class", y = "count") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = ctp_base, colour = NA),
    panel.background = element_rect(fill = ctp_base, colour = NA),
    panel.grid.major = element_line(colour = ctp_surface0),
    panel.grid.minor = element_blank(),
    text = element_text(colour = ctp_text),
    axis.text = element_text(colour = ctp_text),
    plot.title = element_text(colour = ctp_mauve),
    legend.position = "none"
  )
```

<!-- speaker_note: bar chart using catppuccin palette colours. -->

<!-- end_slide -->

# ggplot2: boxplot

```r +exec +image
library(ggplot2)

ctp_base <- "#1e1e2e"
ctp_text <- "#cdd6f4"
ctp_mauve <- "#cba6f7"
ctp_surface0 <- "#313244"
ctp_palette <- c("#a6e3a1", "#f9e2af", "#f38ba8")

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(alpha = 0.8, colour = ctp_text) +
  scale_fill_manual(values = ctp_palette) +
  labs(title = "sepal length by species", x = "species", y = "sepal length (cm)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = ctp_base, colour = NA),
    panel.background = element_rect(fill = ctp_base, colour = NA),
    panel.grid.major = element_line(colour = ctp_surface0),
    panel.grid.minor = element_blank(),
    text = element_text(colour = ctp_text),
    axis.text = element_text(colour = ctp_text),
    plot.title = element_text(colour = ctp_mauve),
    legend.position = "none"
  )
```

<!-- speaker_note: boxplot with catppuccin fill colours. -->

<!-- end_slide -->

# maths support

OLS estimator:

```latex +render
\[ \hat{\beta} = (X'X)^{-1}X'y \]
```

conditional average treatment effect:

```latex +render
\[ \tau_i(x) = E[Y_i(1) - Y_i(0) \mid X_i = x] \]
```

<!--
speaker_note: |
  the first equation is the OLS estimator
  the second is the conditional average treatment effect (CATE)
  presenterm uses typst for maths rendering
-->

<!-- end_slide -->

# emphasis styles

- **bold text** stands out
- *italic text* for emphasis
- `inline code` in monospace

> blockquotes work too

<!-- speaker_note: catppuccin theme handles emphasis styles automatically. -->

<!-- end_slide -->

# tables

| outcome     | estimate | 95% CI         |
|-------------|----------|----------------|
| wellbeing   | 0.23     | [0.12, 0.34]   |
| cooperation | 0.18     | [0.08, 0.28]   |
| meaning     | 0.31     | [0.19, 0.43]   |

<!-- speaker_note: markdown tables render nicely in the terminal. -->

<!-- end_slide -->

# causal diagram

note: tikz diagrams need pre-rendering for presenterm.

```
    Confounder
      /    \
     v      v
Exposure → Outcome
```

alternative: use ASCII art or pre-rendered PNG.

<!-- speaker_note: tikz diagrams would need to be saved as images and included. -->

<!-- end_slide -->

# links

- presenterm: https://github.com/mfontanini/presenterm
- catppuccin: https://github.com/catppuccin/catppuccin
- quarto: https://quarto.org

<!-- speaker_note: links display as plain text in terminal. -->

<!-- end_slide -->

# thank you

questions?

<!--
speaker_note: |
  remember to:
  thank the organisers
  mention collaborators
  point to the github repo for code
-->
