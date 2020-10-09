# functions to create customized ggplot theme

library(ggplot2)
library(viridis)
library(RColorBrewer)
library(stringr)

myGGplotTheme <- function(font = "Helvetica",
                          panel_grid_major_color = "grey90",
                          panel_background_color = "grey98",
                          strip_background_color = "#2c3e50",
                          x_text_angle = FALSE,
                          size_theme = NULL,
                          axis_title_size = 10, axis_text_size = 7,
                          legend_title_size = 10, legend_text_size = 8,
                          strip_text_size = 9, title_size = 12, 
                          ...) {
  ####### Function Description ########
  # customized ggplot theme
  # 
  # inputs:
  # - font = font family for ggplot text
  # - panel_grid_major_color = color of panel grid major axes
  # - panel_background_color = color for plot background
  # - strip_background_color = color for strip background (for facet_grid/wrap)
  # - x_text_angle = logical; whether or not to angle x text at 45 degrees
  # - size_theme = "small", "normal", "large", "xlarge"; default sizes for
  #     plot text and titles; if NULL, defaults to values specified by 
  #     axis_title_size, axis_text_size, legend_title_size, legend_text_size,
  #     strip_text_size, title_size
  # - axis_title_size = font size of axis title; ignored if size_theme not NULL
  # - axis_text_size = font size of axis text; ignored if size_theme not NULL
  # - legend_title_size = font size of legend title; ignored if size_theme not
  #     NULL
  # - legend_text_size = font size of legend text/key; ignored if size_theme not
  #     NULL
  # - strip_text_size = font size of strip text; ignored if size_theme not NULL
  # - title_size = font size of plot title; ignored if size_theme not NULL
  # - ... = other arguments to pass to ggplot2::theme()
  #
  # output:
  # - ggplot theme object
  #
  # example usage:
  # ggplot(iris) + aes(x = Sepal.Length, y = Sepal.Width) +
  #   geom_point() + myGGplotTheme()
  ####### 
  
  if (!is.null(size_theme)) {
    if (size_theme == "small") {
      axis_title_size <- 10
      axis_text_size <- 7
      legend_title_size <- 10
      legend_text_size <- 8
      strip_text_size <- 9
      title_size <- 12
    } else if (size_theme == "medium") {
      axis_title_size <- 14
      axis_text_size <- 10
      legend_title_size <- 14
      legend_text_size <- 10
      strip_text_size <- 12
      title_size <- 16
    } else if (str_detect(size_theme, "large")) {
      num_x <- str_count(size_theme, "x")
      axis_title_size <- 18 + num_x * 2
      axis_text_size <- 14 + num_x * 2
      legend_title_size <- 18 + num_x * 2
      legend_text_size <- 14 + num_x * 2
      strip_text_size <- 16 + num_x * 2
      title_size <- 20 + num_x * 2
    } else {
      stop("size_theme must be one of 'small', 'medium', 'large', 'xlarge', or NULL.")
    }
  }
  
  my_theme <- theme(
    axis.title = element_text(family = font, 
                              size = axis_title_size, 
                              face = "bold"),
    axis.text = element_text(family = font, size = axis_text_size),
    axis.line = element_line(size = 1, color = "black"),
    axis.ticks = element_line(size = rel(1), colour = "black"),
    axis.text.x = element_text(angle = ifelse(x_text_angle, 45, 0),
                               hjust = ifelse(x_text_angle, 1, 0.5)),
    panel.grid.major = element_line(colour = panel_grid_major_color,
                                    size = rel(0.5)),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = panel_background_color),
    strip.background = element_rect(fill = strip_background_color,
                                    color = strip_background_color),
    strip.text = element_text(color = "white",
                              face = "bold",
                              size = strip_text_size),
    legend.key = element_rect(fill = "grey98"),
    legend.text = element_text(family = font, size = legend_text_size),
    legend.title = element_text(family = font, 
                                face = "bold",
                                size = legend_title_size),
    plot.title = element_text(family = font, face = "bold",
                              size = title_size),
    ...
  )
  
  return(my_theme)
}

myGGplotMapTheme <- function(...) {
  ####### Function Description ########
  # customized ggplot theme for maps
  # 
  # inputs:
  # - ... = other arguments to pass to ggplot2::theme()
  #
  # output:
  # - ggplot theme object
  ####### 
  
  my_theme <- theme_void() + theme(...)
  return(my_theme)
}


myGGplotColor <- function(color, viridis = F, option = "plasma", 
                          drop = T, ...) {
  ####### Function Description ########
  # customized ggplot color scheme
  # 
  # inputs:
  # - color = color vector (should match aes(color = ) argument)
  # - viridis = T/F logical; whether or not to use viridis color scheme if using
  #     discrete color scheme
  # - option = argument indicating viridis color palette name
  # - drop = logical; whether or not to drop factors with no observations
  # - ... = other arguments to pass to scale_color_manual() or 
  #   scale_colour_viridis()
  #
  # output:
  # - ggplot color theme object
  #
  # example usage:
  # ggplot(iris) + aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
  #   geom_point() + myGGplotTheme() + myGGplotColor(color = iris$Species)
  #######
  
  discrete <- is.factor(color)
  
  if (discrete) {
    if (nlevels(color) <= 8 & viridis == FALSE) {
      my_palette <- brewer.pal(n = 8, name = "Dark2")
      # reorder colors
      my_palette[2] <- my_palette[1]
      my_palette[1] <- "#FF9300"
      my_color <- scale_color_manual(values = my_palette, drop = drop, ...)
    } else {
      my_color <- scale_colour_viridis(
        discrete = discrete, option = option,
        begin = 0, end = 0.95, drop = drop, ...
      )
    }
  } else {
    my_color <- scale_colour_viridis(
      discrete = discrete, option = option,
      begin = 0, end = 0.95, ...
    )
  }
  
  return(my_color)
}


myGGplotFill <- function(fill, viridis = F, option = "plasma", 
                         drop = T, ...) {
  ####### Function Description ########
  # customized ggplot fill scheme
  # 
  # inputs:
  # - fill = fill vector (should match aes(fill = ) argument)
  # - viridis = T/F logical; whether or not to use viridis color scheme if using
  #     discrete color scheme
  # - option = argument indicating viridis color palette name
  # - drop = logical; whether or not to drop factors with no observations
  # - ... = other arguments to pass to scale_fill_manual() or 
  #   scale_fill_viridis()
  #
  # output:
  # - ggplot fill theme object
  #
  # example usage:
  # ggplot(iris) + aes(x = Sepal.Length, fill = Species) +
  #   geom_density() + myGGplotTheme() + myGGplotFill(fill = iris$Species)
  #######
  
  discrete <- is.factor(fill)
  
  if (discrete) {
    if (nlevels(fill) <= 8 & viridis == FALSE) {
      my_palette <- brewer.pal(n = 8, name = "Dark2")
      # reorder colors
      my_palette[2] <- my_palette[1]
      my_palette[1] <- "#FF9300"
      my_fill <- scale_fill_manual(values = my_palette, drop = drop, ...)
    } else {
      my_fill <- scale_fill_viridis(
        discrete = discrete, option = option,
        begin = 0, end = 0.95, drop = drop, ...
      )
    }
  } else {
    my_fill <- scale_fill_viridis(
      discrete = discrete, option = option,
      begin = 0, end = 0.95, ...
    )
  }
  
  return(my_fill)
}
