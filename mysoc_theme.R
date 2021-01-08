library(ggplot2)
library(dplyr)
library(showtext)
library(magick)
library(here)
library(magrittr)

# odd r function to get the directory of the current file
theme_dir <- getSrcDirectory(function(dummy) {
  dummy
})

# improve anti-aliasing on windows
showtext_auto()
trace(grDevices:::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)

# get png fucntion
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

# get fonts for mysociety and societyworks
font_add(family = "sspro", regular = file.path(theme_dir, "font", "SourceSansPro-Regular.ttf"))
font_add(family = "sspro_bold", regular = file.path(theme_dir, "font", "SourceSansPro-Bold.ttf"))
font_add(family = "lato", regular = file.path(theme_dir, "font", "Lato-Regular.ttf"))
font_add(family = "lato_bold", regular = file.path(theme_dir, "font", "Lato-Black.ttf"))

# core theme colours

mysoc_blue_dark <- "#207cba"
mysoc_blue <- "#4FADED"
mysoc_green <- "#62B356"
mysoc_red <- "#E04B4B"
mysoc_purple <- "#A94CA6"
mysoc_orange <- "#F4A140"
mysoc_yellow <- "#FFD836"
mysoc_dark_grey <- "#6C6B68"
mysoc_black <- "#333333"
sworks_blue <- "#0ba7d1"
sworks_orange <- "#fed876"
sworks_green <- "#0BD198"
sworks_pink <- "#e65376"


# these need replacing
sworks_2_point <- c(
  sworks_blue,
  sworks_pink
)

sworks_3_point <- c(
  sworks_blue,
  sworks_green,
  sworks_pink
)
sworks_4_point <- c(
  sworks_blue,
  sworks_green,
  sworks_pink,
  sworks_orange
)

# mysoc dark blue + d2 category 10 colours
mysoc_palette <- c(
  mysoc_blue_dark, "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
)

mysoc_colour <- function() {
  function(n) {
    return(mysoc_palette[1:n])
  }
}

sworks_colour <- function() {
  function(n) {
    if (n == 2) {
      return(sworks_2_point)
    }
    if (n == 3) {
      return(sworks_3_point)
    }
    if (n == 4) {
      return(sworks_4_point)
    }
  }
}

# scale to use to get correct colours on a discreet range (coloured lines, bars, etc)
mysoc_discreet_scale <- function() {
  ggplot2::discrete_scale("colour", "branded", mysoc_colour())
}

sworks_discreet_scale <- function() {
  ggplot2::discrete_scale("colour", "branded", sworks_colour())
}

# add elements related to brand logo to theme so can be overridden
# by stacking themes
register_theme_elements(
  brand.logo = "mysociety-logo.jpg",
  brand.size = "400",
  brand.offset = "+100+50",
  brand.gravity = "southwest",
  element_tree = list(
    brand.logo = el_def("character", "text"),
    brand.size = el_def("character", "text"),
    brand.offset = el_def("character", "text"),
    brand.gravity = el_def("character", "text")
  )
)

# rough font sizes (matches other document)
heading_pt <- 30
subheading_pt <- 16
standard_pt <- 14
ratio <- 2.215 # scale up fonts to still match at higher resolutions


# mysociety styling that matches altair formatting
mysoc_theme <- function(legend.position = "bottom",
                        header_colour = "black",
                        font_family_title = "sspro_bold",
                        font_family = "sspro",
                        header_size = heading_pt * ratio,
                        axis_size = subheading_pt * ratio,
                        label_size = standard_pt * ratio) {
  theme_classic() +
    theme(
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = 0.1, color = "lightgrey")
    ) +
    theme(
      text = element_text(family = font_family, size = label_size),
      axis.text = element_text(size = label_size),
      axis.title = element_text(size = axis_size, face = "bold", family = font_family_title, colour = header_colour, ),
      plot.subtitle = element_text(size = axis_size, face = "bold", family = font_family_title, colour = header_colour),
      axis.ticks = element_blank(),
      axis.title.y = element_text(angle = 0, vjust = 1),
      axis.line = element_line(colour = "black", size = 0.1)
    ) +

    theme(
      plot.title.position = "plot",
      plot.title = element_text(
        colour = header_colour,
        family = font_family_title,
        size = header_size
      )
    ) +
    theme(
      brand.logo = "mysociety-logo.jpg",
      brand.size = "300",
      brand.offset = "+40+0",
      brand.gravity = "southwest"
    ) +
    theme(line = element_line(size = 2)) +
    theme(
      legend.position = legend.position,
      legend.margin = margin(c(0, 0, 0, 0)),
      legend.box.margin = margin(-10, 95, -5, -5),
      legend.text.align = 0,
      legend.justification = c(1, 0)
    ) +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
}


# theme variant for societyworks
sworks_theme <- function(legend.position = "bottom",
                         header_colour = mysoc_black,
                         font_family_title = "lato_bold",
                         font_family = "lato",
                         header_size = default_header_size) {
  new_theme <- mysoc_theme(legend.position, header_colour, font_family_title, font_family, header_size)
  new_theme <- new_theme + theme(brand.logo = "sworks.png", brand.offset = "+80+10")
}

# change the default sizes of points and labels to match scaling elsewhere
update_geom_defaults("point", list(size = 1))
update_geom_defaults("label", list(size = standard_pt))

# override ylas function to push it to subtitle instead
# puts label above y axis rather than to left of it
ylab <- function(text) {
  ggplot2::labs(subtitle = text, y = "")
}


# load a logo and add it at the position specified by the theme
add_logo <- function(filename, theme) {
  plot <- image_read(filename)
  logo_raw <- image_read(file.path(theme_dir, "logos", theme$brand.logo))
  logo <- logo_raw %>%
    image_scale(theme$brand.size) %>%
    image_transparent("white")

  final_plot <- plot %>% image_composite(logo, gravity = theme$brand.gravity, offset = theme$brand.offset)
  image_write(final_plot, filename)
}

# save image to file and then reload to preview within rstudio
save_and_show <- function(plot, filename, width = 10, height = 5, unit = "cm", dpi = 576, no_logo = FALSE) {
  plot + ggsave(filename, type = "cairo", width = width, height = height, unit= unit, dpi = dpi)
  if (no_logo == FALSE) {
    add_logo(filename, plot$theme)
  }
  img <- magick::image_read(filename)
  plot(img)
}
