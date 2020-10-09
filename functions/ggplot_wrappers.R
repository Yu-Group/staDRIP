# convenient wrapper functions around custom ggplot theme

library(tidyverse)

source("./ggplot_themes.R")

plotBarplot <- function(data, x.str, fill.str = NULL, fill = "#6FBBE3", 
                        show.plot = F, ...) {
  ####### Function Description ########
  # plot nice barplots using custom ggplot theme
  # 
  # inputs:
  # - data = data.frame for plotting
  # - x.str = string; name of variable to plot frequencies for
  # - fill.str = string (optional); variable name to use as color for plotting
  # - fill = barplot fill color
  # - show.plot = logical; whether or not to print plot
  # ... = other geom_bar() arguments
  #
  # output:
  # - ggplot object
  #
  # example usage:
  # df <- data.frame(x = rep(letters[1:3], 2), y = rep(LETTERS[1:2], 3))
  # plotBar(data = df, x.str = "x")
  # plotBar(df, x.str = "x", fill.str = "y")
  ####### 
  
  if (is.null(x.str)) {
    stop("Must specify x.str argument.")
  }
  
  if (is.null(fill.str)) {
    data <- data %>%
      rename(x = x.str)
    
    plt <- ggplot(data) +
      aes(x = x) +
      labs(y = "Frequency", x = x.str) +
      geom_bar(position = "dodge", stat = "count", color = "grey98",
               fill = fill, ...) +
      myGGplotTheme()
  } else {
    data <- data %>%
      rename(x = x.str, fill = fill.str)
    
    plt <- ggplot(data) +
      aes(x = x, fill = fill) +
      labs(y = "Frequency", x = x.str, fill = fill.str) +
      geom_bar(position = "dodge", stat = "count", color = "grey98", ...) +
      myGGplotTheme() +
      myGGplotFill(fill = data$fill)
  }
  
  if (show.plot) {
    print(plt)
  }
  
  return(plt)
}

plotBoxplot <- function(data, x.str = NULL, y.str = NULL, fill.str = NULL,
                        horizontal = FALSE, show.plot = F, ...) {
  ####### Function Description ########
  # plot nice boxplots using custom ggplot theme
  # 
  # inputs:
  # - data = data.frame for plotting
  # - x.str = string; name of variable for plotting; if NULL, plot boxplot
  #   of all data
  # - y.str = string (optional); name of factor variable for plotting;
  # - fill.str = string (optional); variable name to use as fill for plotting
  # - horizontal = logical; whether boxplots are horizontal or vertical
  # - show.plot = logical; whether or not to print plot
  # ... = other geom_boxplot() arguments
  #
  # output:
  # - ggplot object
  #
  # example usage:
  # ## plot boxplot of all data in data frame
  # plotBoxplot(as.data.frame(matrix(rnorm(1000), nrow = 100)))
  # ## plot boxplot of single column in data frame
  # plotBoxplot(iris, x.str = "Sepal.Width")
  # plotBoxplot(iris, x.str = "Sepal.Width", y.str = "Species")
  # iris2 <- data.frame(iris, z = rep(letters[1:2], length.out = nrow(iris)))
  # plotBoxplot(iris2, x.str = "Sepal.Width", y.str = "Species", fill.str = "z")
  #######
  
  if (is.null(fill.str)) {
    if (is.null(x.str) & is.null(y.str)) {  # plot all data
      x.str <- "data"
      data <- gather(data, key = "variable", value = "data")
    } else if (is.null(x.str)) {  # plot all data
      x.str <- "data"
      data <- gather(data, key = "variable", value = "data", -y.str)
    }  
    
    if (is.null(y.str)) {
      data <- data %>%
        rename(x = x.str)
      plt <- ggplot(data) +
        aes(x = "", y = x) +
        labs(y = x.str, x = "") +
        geom_boxplot(...) +
        myGGplotTheme()
    } else {
      data <- data %>%
        rename(x = x.str, y = y.str)
      plt <- ggplot(data) +
        aes(x = y, y = x, group = y) +
        labs(y = x.str, x = y.str) +
        geom_boxplot(...) +
        myGGplotTheme()
    }
    
  } else {
    if (is.null(x.str) & is.null(y.str)) { # plot all data
      x.str <- "data"
      data <- gather(data, key = "variable", value = "data", -fill.str)
    } else if (is.null(x.str)) {  # plot all data
      x.str <- "data"
      data <- gather(data, key = "variable", value = "data", -fill.str, -y.str)
    }
    
    if (is.null(y.str)) {
      data <- data %>%
        rename(x = x.str, fill = fill.str)
      plt <- ggplot(data) +
        aes(x = "", y = x, fill = fill) +
        labs(y = x.str, x = "", fill = fill.str) +
        geom_boxplot(...) +
        myGGplotTheme() +
        myGGplotFill(fill = data$fill)
    } else {
      data <- data %>%
        rename(x = x.str, y = y.str, fill = fill.str)
      plt <- ggplot(data) +
        aes(x = y, y = x, fill = fill, group = interaction(y, fill)) +
        labs(y = x.str, x = y.str, fill = fill.str) +
        geom_boxplot(...) +
        myGGplotTheme() + 
        myGGplotFill(fill = data$fill)
    }
  }
  
  if (horizontal) {
    plt <- plt + coord_flip()
  }
  
  if (show.plot) {
    print(plt)
  }
  
  return(plt)
}

plotDensity <- function(data, x.str = NULL, fill.str = NULL, fill = "#6FBBE3",
                        alpha = 0.4, show.plot = F, ...) {
  ####### Function Description ########
  # plot nice density plots using custom ggplot theme
  # 
  # inputs:
  # - data = data.frame for plotting
  # - x.str = string; name of variable for plotting; if NULL, plot density
  #   of all data
  # - fill.str = string (optional); variable name to use as fill for plotting
  # - fill = density fill color
  # - alpha = density alpha value
  # - show.plot = logical; whether or not to print plot
  # ... = other geom_density() arguments
  # 
  # output:
  # - ggplot object
  #
  # example usage:
  # ## plot distribution of all data in data frame
  # plotDensity(as.data.frame(rnorm(1000), nrow = 100))
  # ## plot distribution of a single column in data frame
  # plotDensity(iris, x.str = "Sepal.Width")
  # plotDensity(iris, x.str = "Sepal.Width", fill.str = "Species")
  #######
  
  if (is.null(fill.str)) {
    if (is.null(x.str)) { # plot all data if x.str is not provided
      x.str <- "data"
      data <- gather(data, key = "variable", value = "data")
    }
    
    data <- data %>%
      rename(x = x.str)
    
    plt <- ggplot(data) +
      aes(x = x) +
      labs(y = "Density", x = x.str) +
      geom_density(fill = fill, alpha = alpha, color = "black", ...) +
      myGGplotTheme()
  } else {
    if (is.null(x.str)) { # plot all data if x.str is not provided
      x.str <- "data"
      data <- gather(data, key = "variable", value = "data", -fill.str)
    }
    
    data <- data %>%
      rename(x = x.str, fill = fill.str)
    
    plt <- ggplot(data) +
      aes(x = x, fill = fill) +
      labs(y = "Density", x = x.str, fill = fill.str) +
      geom_density(color = "black", alpha = alpha, ...) +
      myGGplotTheme() +
      myGGplotFill(fill = data$fill)
  }
  
  if (show.plot) {
    print(plt)
  }
  
  return(plt)
}

plotHistogram <- function(data, x.str = NULL, fill.str = NULL, fill = "#6FBBE3",
                          bins = 12, show.plot = F, ...) {
  ####### Function Description ########
  # plot nice histograms using custom ggplot theme
  # 
  # inputs:
  # - data = data.frame for plotting
  # - x.str = string; name of variable for plotting; if NULL, plot histogram
  #   of all data
  # - fill.str = string (optional); variable name to use as color for plotting
  # - fill = histogram fill color
  # - bins = number of bins for geom_histogram()
  # - show.plot = logical; whether or not to print plot
  # ... = other geom_histogram() arguments
  # 
  # output:
  # - ggplot object
  #
  # example usage:
  # ## plot distribution of all data in data frame
  # plotHistogram(as.data.frame(rnorm(1000), nrow = 100))
  # ## plot distribution of a single column in data frame
  # plotHistogram(iris, x.str = "Sepal.Width")
  # plotHistogram(iris, x.str = "Sepal.Width", fill.str = "Species")
  #######
  
  if (is.null(fill.str)) {
    if (is.null(x.str)) { # plot all data if x.str is not provided
      x.str <- "data"
      data <- gather(data, key = "variable", value = "data")
    }
    
    data <- data %>%
      rename(x = x.str)
    
    plt <- ggplot(data) +
      aes(x = x) +
      labs(y = "Frequency", x = x.str) +
      geom_histogram(fill = fill, color = "grey98", bins = bins, ...) +
      myGGplotTheme()
  } else {
    if (is.null(x.str)) { # plot all data if x.str is not provided
      x.str <- "data"
      data <- gather(data, key = "variable", value = "data", -fill.str)
    }
    
    data <- data %>%
      rename(x = x.str, fill = fill.str)
    
    plt <- ggplot(data) +
      aes(x = x, fill = fill) +
      labs(y = "Frequency", x = x.str, fill = fill.str) +
      geom_histogram(color = "grey98", bins = bins, ...) +
      myGGplotTheme() +
      myGGplotFill(fill = data$fill)
  }
  
  if (show.plot) {
    print(plt)
  }
  
  return(plt)
}

plotLine <- function(data, x.str, y.str, color.str = NULL, show.plot = F, ...) {
  ####### Function Description ########
  # plot nice line plots using custom ggplot theme
  # 
  # inputs:
  # - data = data.frame for plotting
  # - x.str = string; name of variable for plotting on x axis
  # - y.str = string; name of variable for plotting on y axis
  # - color.str = string (optional); variable name to use as color for plotting
  # - show.plot = logical; whether or not to print plot
  # ... = other geom_line() arguments
  # 
  # output:
  # - ggplot object
  #
  # example usage:
  # df <- data.frame(time = 1:5, value = 5:9)
  # plotLine(df, x.str = "time", y.str = "value")
  # df2 <- data.frame(time = rep(1:5, 2), value = 1:10,
  #                   group = rep(letters[1:2], each = 5))
  # plotLine(df2, x.str = "time", y.str = "value", color.str = "group")
  ####### 
  
  if (is.null(x.str) | is.null(y.str)) {
    stop("Must specify x.str and y.str argument.")
  }
  
  if (is.null(color.str)) {
    data <- data %>%
      rename(x = x.str, y = y.str)
    
    plt <- ggplot(data) +
      aes(x = x, y = y) +
      labs(y = y.str, x = x.str) +
      geom_line(...) + 
      myGGplotTheme()
    
  } else {
    data <- data %>%
      rename(x = x.str, y = y.str, color = color.str)
    
    plt <- ggplot(data) +
      aes(x = x, y = y, color = color, group = color) +
      labs(y = y.str, x = x.str, color = color.str) +
      geom_line(...) +
      myGGplotTheme() +
      myGGplotColor(color = data$color)
  }
  
  if (show.plot) {
    print(plt)
  }
  
  return(plt)
}

plotScatter <- function(data, x.str, y.str, color.str = NULL,
                        show.plot = F, ...) {
  ####### Function Description ########
  # plot nice scatter plots using custom ggplot theme
  # 
  # inputs:
  # - data = data.frame for plotting
  # - x.str = string; name of variable for plotting on x axis
  # - y.str = string; name of variable for plotting on y axis
  # - color.str = string (optional); variable name to use as color for plotting
  # - show.plot = logical; whether or not to print plot
  # ... = other geom_point() arguments
  # 
  # output:
  # - ggplot object
  #
  # example usage:
  # plotScatter(iris, x.str = "Sepal.Width", y.str = "Sepal.Length")
  # plotScatter(iris, x.str = "Sepal.Width", y.str = "Sepal.Length",
  #             color.str = "Species")
  #######
  
  if (is.null(x.str) | is.null(y.str)) {
    stop("Must specify x.str and y.str argument.")
  }
  
  if (is.null(color.str)) {
    data <- data %>%
      rename(x = x.str, y = y.str)
    
    plt <- ggplot(data) +
      aes(x = x, y = y) +
      labs(y = y.str, x = x.str) +
      geom_point(...) + 
      myGGplotTheme()
    
  } else {
    data <- data %>%
      rename(x = x.str, y = y.str, color = color.str)
    
    plt <- ggplot(data) +
      aes(x = x, y = y, color = color) +
      labs(y = y.str, x = x.str, color = color.str) +
      geom_point(...) +
      myGGplotTheme() +
      myGGplotColor(color = data$color)
  }
  
  if (show.plot) {
    print(plt)
  }
  
  return(plt)
}

