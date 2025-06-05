# an r script to generate a hex sticker for the boilerplate package.

# --- 1. installation ---
# install required packages if you don't have them already.
if (!requireNamespace("hexSticker", quietly = TRUE)) {
  install.packages("hexSticker")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}


# --- 2. create the central graphic (the "subplot") ---
# we'll use ggplot2 to create the metal plate with its engraved text.

library(ggplot2)

# define the colours from our recipe.
plate_colour <- "#bdc3c7" # a light, metallic silver
engraved_text_colour <- "#7f8c8d" # a medium grey

# we use theme_void() to make it a clean graphic with no axes or background. s
# create a plot object for the metal plate.
metal_plate_plot <- ggplot() +
  # draw the silver rectangle for the plate.
  geom_rect(aes(xmin = 0, xmax = 10, ymin = 3, ymax = 7), fill = plate_colour) +
  # add the engraved placeholder text.
  geom_text(
    aes(x = 5, y = 5, label = "{{...}}"),
    family = "sans",
    size = 30, # <-- increase this number to make the text larger.
    colour = engraved_text_colour
  ) +
  # remove all plot elements for a clean image.
  theme_void()

# --- 3. generate the hex sticker ---
# now we feed our subplot into the hexsticker function.

library(hexSticker)

# define the colours for the hexagon itself.
background_colour <- "#34495e" # dark blue-grey
border_colour <- "#2c3e50" # a slightly darker border
# text_colour <- "#FFFFFF" # white
text_colour <- "#bdc3c7" # white

sticker(
  # subplot is our ggplot graphic.
  subplot = metal_plate_plot,
  s_width = 3.4,      # width of the subplot.
  s_height = 1.0,     # height of the subplot.
  s_x = 1.0,          # horizontal position of the subplot.
  s_y = 1.0,          # vertical position of the subplot.

  # package is the main text at the bottom.
  package = "boilerplate",
  p_family = "sans",  # font family for package name.
  p_size = 24,        # font size.
  p_color = text_colour,
  p_y = 0.3,          # vertical position of package name.

  # h_fill and h_color are for the hexagon itself.
  h_fill = background_colour,
  h_color = border_colour,

  # define the output filename.
  # it's good practice to save it in `man/figures/`.
  filename = "man/figures/logo-new.png"
)



# third version -----------------------------------------------------------

  # an r script to generate a hex sticker for the boilerplate package.

  # --- 1. installation ---
  # install required packages if you don't have them already.
  if (!requireNamespace("hexSticker", quietly = TRUE)) {
    install.packages("hexSticker")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }


  # --- 2. create the central graphic (the "subplot") ---
  # we'll use ggplot2 to create the metal plate with its engraved text.

  library(ggplot2)

  # define the colours from our recipe.
  plate_colour <- "#bdc3c7" # a light, metallic silver
  engraved_text_colour <- "#7f8c8d" # a medium grey

  # create a plot object for the metal plate.
  # we use theme_void() to make it a clean graphic with no axes or background.
  metal_plate_plot <- ggplot() +
    # draw the silver rectangle for the plate.
    geom_rect(aes(xmin = 0, xmax = 10, ymin = 3, ymax = 7), fill = plate_colour) +
    # add the engraved placeholder text.
    geom_text(
      aes(x = 5, y = 5, label = "{{...}}"),
      family = "sans", # use a generic sans-serif font.
      size = 30,
      colour = engraved_text_colour
    ) +
    # remove all plot elements for a clean image.
    theme_void()


  # --- 3. generate the hex sticker ---
  # now we feed our subplot into the hexsticker function.

  library(hexSticker)

  # define the colours for the hexagon itself.
  # these have been changed for a white background.
  # background_colour <- "#FFFFFF" # set to white.
  background_colour <- "#2c3e50"  # set to white.

  border_colour <- "#D3D3D3" # set to a light grey for contrast.
  text_colour <- "#2c3e50" # set to a dark colour to be visible.

  sticker(
    # subplot is our ggplot graphic.
    subplot = metal_plate_plot,
    s_width = 1.4,      # width of the subplot.
    s_height = 1.0,     # height of the subplot.
    s_x = 1.0,          # horizontal position of the subplot.
    s_y = 1.0,          # vertical position of the subplot.

    # package is the main text at the bottom.
    package = "boilerplate",
    p_family = "sans",  # font family for package name.
    p_size = 24,        # font size.
    p_color = text_colour,
    p_y = 0.3,          # vertical position of package name.

    # h_fill and h_color are for the hexagon itself.
    h_fill = background_colour,
    h_color = border_colour,

    # define the output filename.
    # it's good practice to save it in `man/figures/`.
    filename = "man/figures/logo_white.png" # changed filename to avoid overwriting.
  )



# version 4 ---------------------------------------------------------------

  # an r script to generate a hex sticker for the boilerplate package.
  # this version has a dark hexagon fill and a transparent background.

  # --- 1. installation ---
  # install required packages if you don't have them already.
  if (!requireNamespace("hexSticker", quietly = TRUE)) {
    install.packages("hexSticker")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }


  # --- 2. create the central graphic (the "subplot") ---
  # we'll use ggplot2 to create the metal plate with its engraved text.

  library(ggplot2)

  # define the colours for the subplot.
  plate_colour <- "#bdc3c7" # a light, metallic silver
  engraved_text_colour <- "#7f8c8d" # a medium grey

  # create a plot object for the metal plate.
  metal_plate_plot <- ggplot() +
    # draw the silver rectangle for the plate.
    geom_rect(aes(xmin = 0, xmax = 10, ymin = 3, ymax = 7), fill = plate_colour) +
    # add the engraved placeholder text.
    geom_text(
      aes(x = 5, y = 5, label = "{{...}}"),
      family = "sans", # use a generic sans-serif font.
      size = 30,
      colour = engraved_text_colour
    ) +
    # remove all plot elements for a clean image.
    theme_void()


  # --- 3. generate the hex sticker ---
  # now we feed our subplot into the hexsticker function.

  library(hexSticker)

  # define the colours for the dark hexagon.
  background_colour <- "#34495e" # dark blue-grey
  border_colour <- "#2c3e50" # a slightly darker border
  text_colour <- "#FFFFFF" # white

  sticker(
    # subplot is our ggplot graphic.
    subplot = metal_plate_plot,
    s_width = 1.4,      # width of the subplot.
    s_height = 1.0,     # height of the subplot.
    s_x = 1.0,          # horizontal position of the subplot.
    s_y = 1.0,          # vertical position of the subplot.

    # package is the main text at the bottom.
    package = "boilerplate",
    p_family = "sans",  # font family for package name.
    p_size = 24,        # font size.
    p_color = text_colour,
    p_y = 0.3,          # vertical position of package name.

    # h_fill and h_color are for the hexagon itself.
    h_fill = background_colour,
    h_color = border_colour,

    # define the output filename.
    filename = "man/figures/logo.png"
  )

