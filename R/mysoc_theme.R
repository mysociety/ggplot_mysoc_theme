
# improve anti-aliasing on windows
#' @export
init_antialias <- function() {
  showtext::showtext_auto()
  trace(grDevices:::png, quote({
    if (missing(type) && missing(antialias)) {
      type <- "cairo-png"
      antialias <- "subpixel"
    }
  }), print = FALSE)
}

init_fonts <- function() {
  # get fonts for mysociety and societyworks
  sysfonts::font_add(family = "sspro", regular = system.file("font", "SourceSansPro-Regular.ttf", package = "ggplotmysoc"))
  sysfonts::font_add(family = "sspro_bold", regular = system.file("font", "SourceSansPro-Bold.ttf", package = "ggplotmysoc"))
  sysfonts::font_add(family = "lato", regular = system.file("font", "Lato-Regular.ttf", package = "ggplotmysoc"))
  sysfonts::font_add(family = "lato_bold", regular = system.file("font", "Lato-Black.ttf", package = "ggplotmysoc"))
}

#' @export
init_theme <- function() {
  init_antialias()
  init_fonts()

  # add elements related to brand logo to theme so can be overridden
  # by stacking themes
  ggplot2::register_theme_elements(
    brand.logo = "mysociety-logo.jpg",
    brand.size = "400",
    brand.offset = "+100+50",
    brand.gravity = "southwest",
    element_tree = list(
      brand.logo = ggplot2::el_def("character", "text"),
      brand.size = ggplot2::el_def("character", "text"),
      brand.offset = ggplot2::el_def("character", "text"),
      brand.gravity = ggplot2::el_def("character", "text")
    )
  )

  # change the default sizes of points and labels to match scaling elsewhere
  ggplot2::update_geom_defaults("point", list(size = 1))
  ggplot2::update_geom_defaults("label", list(size = standard_pt))
}

# get png fucntion
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}


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

mysoc_palette_yellow <- "#ffe269"
mysoc_palette_orange <- "#f4a140"
mysoc_palette_berry <- "#e02653"
mysoc_palette_purple <- "#a94ca6"
mysoc_palette_blue <- "#4faded"
mysoc_palette_dark_blue <- "#0a4166"

mysoc_mono_blue_l20 <- "#acd8f6"
mysoc_mono_blue <- "#4faded"
mysoc_mono_blue_d20 <- "#147cc2"
mysoc_mono_blue_d30 <- "#0f5e94"
mysoc_mono_blue_d40 <- "#0a4166"
mysoc_mono_blue_d50 <- "#062337"

sworks_palette_yellow <- "#fed876"
sworks_palette_berry <- "#e02653"
sworks_palette_blue <- "#0ba7d1"
sworks_palette_dark_blue <- "#065a70"

sworks_mono_blue_l30 <- "#7ddef8"
sworks_mono_blue <- "#oba7d1"
sworks_mono_blue_d15 <- "#076d88"
sworks_mono_blue_d30 <- "#033340"

# mysoc colours + d2 category 10 colours
mysoc_palette <- c(
  mysoc_blue_dark, mysoc_palette_blue, mysoc_palette_purple, mysoc_palette_berry, mysoc_palette_orange,
  mysoc_palette_yellow, "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
)

mysoc_contrast_palette <- c(
  mysoc_blue_dark, mysoc_palette_orange, mysoc_palette_purple, mysoc_palette_berry, mysoc_palette_blue, mysoc_palette_purple, 
  mysoc_palette_yellow, "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
)


sworks_palette <- c(
  sworks_palette_yellow, sworks_palette_berry, sworks_palette_blue, sworks_palette_dark_blue, "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
)

mysoc_mono_palette <- c(mysoc_mono_blue_d50,mysoc_mono_blue_d40, mysoc_mono_blue_d30, mysoc_mono_blue_d20, mysoc_mono_blue, mysoc_mono_blue_l20 )

sworks_mono_palette <- c(sworks_mono_blue_d30, sworks_mono_blue_d15, sworks_mono_blue, sworks_mono_blue_l30 )

list_to_func <- function(colour_list){
  function(n) {
    return(list[1:n])
  }
}

mysoc_colour <- function() {
  function(n) {
    return(mysoc_palette[1:n])
  }
}

sworks_colour <- function() {
  function(n) {
    return(sworks_palette[1:n])
  }
}

# scale to use to get correct colours on a discreet range (coloured lines, bars, etc)
mysoc_discreet_scale <- function(mono = TRUE, contrast=FALSE) {
  if (mono == TRUE){
    if (contrast == TRUE){
      palette = mysoc_contrast_palette
    } else {
      palette = mysoc_palette
    }
  } else {
    palette = mysoc_mono_palette
  }
  ggplot2::discrete_scale("colour", "branded", list_to_func(palette))
}

sworks_discreet_scale <- function(mono = TRUE) {
  if (mono == TRUE){
    palette = sworks_palette
  } else {
    palette = sworks_mono_palette
  }
  ggplot2::discrete_scale("colour", "branded", list_to_func(palette))
}

mysoc_discreet_fill <- function(mono = TRUE, contrast=FALSE) {
  if (mono == TRUE){
    if (contrast == TRUE){
      palette = mysoc_contrast_palette
    } else {
      palette = mysoc_palette
    }
  } else {
    palette = mysoc_mono_palette
  }
  scale_fill_manual(values=palette)
}


# rough font sizes (matches other document)
heading_pt <- 30
subheading_pt <- 16
standard_pt <- 14
ratio <- 2.215 # scale up fonts to still match at higher resolutions

# mysociety styling that matches altair formatting
#' @export
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
    theme(legend.position = legend.position) +
    theme(line = element_line(size = 2)) +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
}


# theme variant for societyworks
#' @export
sworks_theme <- function(legend.position = "bottom",
                         header_colour = mysoc_black,
                         font_family_title = "lato_bold",
                         font_family = "lato",
                         header_size = heading_pt * ratio) {
  new_theme <- mysoc_theme(legend.position, header_colour, font_family_title, font_family, header_size)
  new_theme <- new_theme + theme(brand.logo = "societyworks.png", brand.offset = "+80+10")
}

# override ylas function to push it to subtitle instead
# puts label above y axis rather than to left of it
#' @export
ylab <- function(text) {
  ggplot2::labs(subtitle = text, y = "")
}


# load a logo and add it at the position specified by the theme
#' @export
add_logo <- function(filename, theme) {
  plot <- magick::image_read(filename)
  logo_raw <- magick::image_read(system.file("logos", theme$brand.logo, package = "ggplotmysoc"))
  logo <- logo_raw %>%
    magick::image_scale(theme$brand.size) %>%
    magick::image_transparent("white")

  final_plot <- plot %>% magick::image_composite(logo, gravity = theme$brand.gravity, offset = theme$brand.offset)
  magick::image_write(final_plot, filename)
}

# save image to file and then reload to preview within rstudio
#' @export
save_and_show <- function(plot, filename, width = 10, height = 5, unit = "cm", dpi = 576, no_logo = FALSE) {
  plot + ggsave(filename, type = "cairo", width = width, height = height, unit = unit, dpi = dpi)
  if (no_logo == FALSE) {
    add_logo(filename, plot$theme)
  }
  img <- magick::image_read(filename)
  plot(img)
}
