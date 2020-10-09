# functions to create basic eda plots

library(tidyverse)
library(irlba)
library(GGally)
library(cowplot)
library(dendextend)
library(leaflet)

source("./ggplot_themes.R")

plotPairs <- function(data, columns, color = NULL, color2 = NULL, 
                      color.label = "", color2.label = "",
                      manual.color = NULL, manual.color2 = NULL,
                      columnLabels = colnames(data[columns]), title = "",
                      size = .5, alpha = .5, cor.text.size = 3.5, subsample = 1,
                      show.upper = T, drop = F, show.plot = F, ...) {
  ####### Function Description ########
  # function to plot nice pair plots of variables
  #
  # inputs:
  # - data = data frame or data matrix
  # - columns = vector of column indices to plot in pair plots
  # - color = vector of class labels to use as colors for lower ggplot panels
  # - color2 = vector of class labels to use as colors for upper ggplot panels
  # - color.label = string for color legend title
  # - color2.label = string for color2 legend title
  # - manual.color = vector of manual colors, corresponding to color argument
  # - manual.color2 = vector of manual colors, corresponding to color2 argument
  # - columnLabels = label names to be displayed on strips
  # - size = point size for geom_point
  # - alpha = alpha for geom_point
  # - cor.text.size = size of correlation text
  # - show.upper = logical; whether or not to show plot in upper panels
  # - subsample = proportion of points to sample and plot
  # - title = string; title for ggplot
  # - drop = logical; whether or not to drop factors with no observations
  # - show.plot = logical; whether or not to show plot
  # ... = additional arguments to pass to myGGplotTheme()
  #
  # outputs: a ggpairs object
  #
  # example usage:
  # plotPairs(data = iris, columns = 1:ncol(iris), color = iris$Species)
  ####### 
  
  # adding labels for colors
  plt_df <- as.data.frame(data)
  if (!is.null(color)) {
    plt_df <- plt_df %>%
      mutate(color = color)
  }
  if (!is.null(color2)) {
    plt_df <- plt_df %>%
      mutate(color2 = color2)
  }
  
  # subsample points
  if (subsample != 1) {
    plt_df <- sample_frac(plt_df, size = subsample, replace = F)
  }
  
  # check if show correlations in upper panel
  if (show.upper) {
    uplt <- GGally::wrap("cor", size = cor.text.size)
  } else {
    uplt <- "blank"
  }
  
  if (is.null(color) & is.null(color2)) {  # no colors
    plt <- ggpairs(
      data = plt_df,
      columns = columns,
      diag = list(continuous = GGally::wrap("densityDiag", alpha = .5)),
      lower = list(continuous = GGally::wrap("points", 
                                             size = size, alpha = alpha)),
      upper = list(continuous = uplt),
      title = title,
      columnLabels = columnLabels
    ) + myGGplotTheme(...)
    
  } else if (is.null(color2)) {  # one color
    
    # grab subplot for legend
    if (length(columns) == 1) {
      if (is.factor(color)) {
        legend <- c(1, 1)
      } else {
        legend <- NULL  
      }
    } else {
      legend_plt_df <- data.frame(color = color, 
                                  legend_x = rep(1, length(color)))
      legend_plt <- ggplot(legend_plt_df) +
        aes(x = legend_x, y = legend_x, color = color) +
        geom_point() +
        labs(color = color.label)
      if (is.null(manual.color)) {
        legend_plt <- legend_plt +
          myGGplotColor(color = color, drop = drop)
      } else {
        legend_plt <- legend_plt +
          scale_color_manual(values = manual.color, drop = drop)
      }
      legend <- grab_legend(legend_plt)
    }
    
    if (is.factor(color)) {
      upper_ls <- list(continuous = uplt)
    } else {
      if (show.upper) {
        upper_ls <- list(continuous = GGally::wrap("points", 
                                                   size = size, alpha = alpha))
      } else {
        upper_ls <- list(continuous = "blank")
      }
    }
    
    plt <- ggpairs(
      data = plt_df,
      columns = columns,
      mapping = aes(color = color),
      diag = list(continuous = GGally::wrap("densityDiag", alpha = .5)),
      lower = list(continuous = GGally::wrap("points", 
                                             size = size, alpha = alpha)),
      upper = upper_ls,
      title = title,
      legend = legend,
      columnLabels = columnLabels
    ) 
    
    # change color palette for all panels
    for (i in 1:plt$nrow) {
      for (j in 1:plt$ncol) {
        if (is.null(manual.color)) {
          plt[i, j] <- plt[i, j] +
            myGGplotColor(color = color, drop = drop) +
            myGGplotFill(fill = color, drop = drop)
        } else {
          plt[i, j] <- plt[i, j] +
            scale_color_manual(values = manual.color, drop = drop) +
            scale_fill_manual(values = manual.color, drop = drop)
        }
      }
    }
    
    plt <- plt + 
      labs(color = color.label, fill = color.label) +
      myGGplotTheme(...)
    
  } else {
    # make lower scatter plots and color by color for the ggpairs plots
    lowerContinuous <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      data <- data %>%
        rename(x = x_str, y = y_str)
      p <- ggplot(data) +
        aes(x = x, y = y, color = color) +
        geom_point(size = size, alpha = alpha)
      return(p)
    }
    
    # make upper scatter plots and color by color2 for the ggpairs plots
    upperContinuous <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      data <- data %>%
        rename(x = x_str, y = y_str)
      p <- ggplot(data) +
        aes(x = x, y = y, color = color2) +
        geom_point(size = size, alpha = alpha)
      return(p)
    }
    
    # make upper boxplots and color by color2 for the ggpairs plots
    upperCombo <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      data <- data %>%
        rename(x = x_str, y = y_str)
      if (is.factor(data$x)) {
        p <- ggplot(data) +
          aes(x = x, y = y, fill = color2) +
          geom_boxplot()
      } else {
        p <- ggplot(data) +
          aes(x = y, y = x, fill = color2) +
          geom_boxplot() +
          coord_flip()
      }
      return(p)
    }
    
    # make upper bar plots and color by color2 for the ggpairs plots
    upperDiscrete <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      data <- data %>%
        rename(x = x_str, y = y_str)
      p <- ggplot(data) +
        aes(x = x, fill = color2) +
        facet_grid(y ~.) +
        geom_bar()
      return(p)
    }
    
    # number of color factors
    nfactors <- is.factor(color) + is.factor(color2)
    
    if (length(columns) == 1) {  # in this case, plot density
      if (nfactors == 0) {
        plt <- ggpairs(data = plt_df, columns = columns, 
                       title = title, legend = c(1, 1), 
                       columnLabels = columnLabels) + 
          labs(color = color.label, fill = color.label) +
          myGGplotTheme(...)
      } else {
        if (is.factor(color)) {
          plt_df$plt_color <- color
        } else {
          plt_df$plt_color <- color2
        }
        plt <- ggpairs(
          data = plt_df,
          columns = columns,
          mapping = aes(color = plt_color),
          diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5)),
          title = title,
          legend = c(1, 1),
          columnLabels = columnLabels
        ) + 
          labs(color = color.label, fill = color.label) +
          myGGplotTheme(...)
        
        if (is.null(manual.color)) {
          plt[1, 1] <- plt[1, 1] +
            myGGplotColor(color = plt_df$plt_color, drop = drop) +
            myGGplotFill(fill = plt_df$plt_color, drop = drop)
        } else {
          plt[1, 1] <- plt[1, 1] +
            scale_color_manual(values = manual.color, drop = drop) +
            scale_fill_manual(values = manual.color, drop = drop)
        }
      }
      
    } else {
      
      # grab color and color2 legends
      legend_plt_df <- plt_df %>%
        mutate(legend_x = data[, columns[1]], legend_y = data[, columns[2]])
      legend_plt1 <- ggplot(legend_plt_df) +
        geom_point(aes(x = legend_x, y = legend_y, color = color)) +
        labs(color = color.label, fill = color.label) +
        theme(legend.position = "bottom")
      legend_plt2 <- ggplot(legend_plt_df) +
        geom_point(aes(x = legend_x, y = legend_y, color = color2)) +
        labs(color = color2.label, fill = color2.label) +
        theme(legend.position = "bottom")
      if (is.null(manual.color)) {
        legend_plt1 <- legend_plt1 +
          myGGplotColor(color = color, drop = drop)
      } else {
        legend_plt1 <- legend_plt1 +
          scale_color_manual(values = manual.color, drop = drop)
      }
      if (is.null(manual.color2)) {
        legend_plt2 <- legend_plt2 +
          myGGplotColor(color = color2, option = "D", viridis = T, drop = drop)
      } else {
        legend_plt2 <- legend_plt2 +
          scale_color_manual(values = manual.color2, drop = drop)
      }
      legend1 <- grab_legend(legend_plt1)
      legend2 <- grab_legend(legend_plt2)
      
      # make ggpairs
      if (nfactors == 0) {
        plt <- ggpairs(
          data = plt_df, 
          columns = columns,
          mapping = aes(color = color),
          diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5)),
          lower = list(continuous = GGally::wrap("points", 
                                                 size = size, alpha = alpha)),
          upper = list(continuous = upperContinuous),
          title = title,
          columnLabels = columnLabels
        )
      } else if (nfactors == 2) {
        plt <- ggpairs(
          data = plt_df,
          columns = columns,
          mapping = aes(color = color),
          diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5)),
          lower = list(continuous = GGally::wrap("points", 
                                                 size = size, alpha = alpha),
                       combo = "box_no_facet"),
          upper = list(continuous = upperContinuous,
                       combo = upperCombo,
                       discrete = upperDiscrete),
          title = title,
          columnLabels = columnLabels
        )
      } else {
        if (is.factor(color)) {
          plt_df$plt_color <- color
        } else {
          plt_df$plt_color <- color2
        }
        
        plt <- ggpairs(
          data = plt_df,
          columns = columns,
          mapping = aes(color = plt_color),
          diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5)),
          lower = list(continuous = lowerContinuous),
          upper = list(continuous = upperContinuous),
          title = title,
          columnLabels = columnLabels
        )
      }
      
      # change color scheme in all panels
      for (i in 1:plt$nrow) {
        for (j in 1:plt$ncol) {
          plt_fill <- plt[i, j]$labels$fill
          plt_col <- plt[i, j]$labels$colour
          if (!is.null(plt_fill)) {
            if (plt_fill %in% names(plt_df)) {
              if (all(as.character(plt_df[, plt_fill]) == 
                      as.character(color))) {
                if (is.null(manual.color)) {
                  plt[i, j] <- plt[i, j] +
                    myGGplotFill(fill = color, drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    scale_fill_manual(values = manual.color, drop = drop)
                }
              } else {
                if (is.null(manual.color2)) {
                  plt[i, j] <- plt[i, j] +
                    myGGplotFill(fill = color2, option = "D", viridis = T,
                                 drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    scale_fill_manual(values = manual.color2, drop = drop)
                }
              }
            }
          }
          
          if (!is.null(plt_col)) {
            if (plt_col %in% names(plt_df)) {
              ptr <- FALSE
              if (is.numeric(plt_df[, plt_col]) & is.numeric(color)) {
                ptr <- all(plt_df[, plt_col] == color)
              } else if (is.factor(plt_df[, plt_col]) & is.factor(color)) {
                ptr <- all(as.character(plt_df[, plt_col]) == 
                             as.character(color))
              }
              if (ptr) {
                if (is.null(manual.color)) {
                  plt[i, j] <- plt[i, j] +
                    myGGplotColor(color = color, drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    scale_color_manual(values = manual.color, drop = drop)
                }
              } else {
                if (is.null(manual.color2)) {
                  plt[i, j] <- plt[i, j] +
                    myGGplotColor(color = color2, option = "D", viridis = T,
                                  drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    scale_color_manual(values = manual.color2, drop = drop)
                }
              }
            }
          }
        }
      }
    }
    
    if (length(columns) != 1) {
      plt <- plot_grid(ggmatrix_gtable(plt + myGGplotTheme(...)),
                       legend1, legend2, 
                       nrow = 3, rel_heights = c(10, 1, 1))
    }
    
  }
  
  if (show.plot) {
    print(plt)
  }
  
  return(plt)
}

plotPCA <- function(X, pca.out, 
                    npcs, pcs, 
                    color = NULL, color2 = NULL, 
                    color.label = "", color2.label = "",
                    manual.color = NULL, manual.color2 = NULL,
                    size = .5, alpha = 1, subsample = 1,
                    show.var = T, center = T, scale = F,
                    title = "", show.plot = F, save = F, path = NULL, ...) {
  ####### Function Description ########
  # function to plot PCA pairs plot
  #
  # inputs: (must specify either npcs or pcs, and either X or pca.out)
  # - X = data matrix or data.frame (must specify either X or pca.out)
  # - pca.out = output of plotPCA(); to avoid computing svds (i.e. pca loadings
  #   and scores) again if computed previously
  # - npcs = number of top pcs to plot (must specify either npcs or pcs)
  # - pcs = vector of which pcs to show (optional; only needed if npcs missing)
  # - color = vector of class labels to use as colors for lower ggplot panels
  # - color2 = (optional) vector of secondary class labels to use as colors for
  #   upper ggplot panels
  # - color.label = string for color legend title
  # - color2.label = string for color2 legend title
  # - manual.color = vector of manual colors, corresponding to color argument
  # - manual.color2 = vector of manual colors, corresponding to color2 argument
  # - size = point size for geom_point
  # - alpha = alpha for geom_point
  # - subsample = proportion of points to sample and plot
  # - show.var = logical; show proportion of variance explained on axes labels
  # - center = logical; whether or not to center data for pca
  # - scale = logical; whether or not to scale data for pca
  # - title = string; title for ggplot
  # - show.plot = logical; whether or not to show plot
  # - save = logical; whether or not to save plot
  # - path = string ending in .rds; filename to save plot
  # - ... = additional arguments to pass to myGGplotTheme()
  #
  # outputs: list of 4
  # - plot = pca pair plots
  # - scores = PCA scores
  # - loadings = PCA loadings
  # - var.explained = proportion of variance explained
  # 
  # example usage:
  # out <- plotPCA(X = iris[, -5], npcs = 3, color = iris$Species)
  # out$plot
  # iris2 <- data.frame(iris, z = rep(letters[1:2], length.out = nrow(iris)))
  # out <- plotPCA(X = iris2[, -c(5, 6)], npcs = 3, 
  #                color = iris2$Species, color2 = iris2$z)
  # out$plot
  ####### 
  
  if (!missing(X)) {
    X <- scale(as.matrix(X), center = center, scale = scale)
    
    if (sum(is.na(X)) > 0) { # error checking
      stop("NAs found in X")
    }
  }
  
  if (!missing(npcs)) { # which pcs to plot
    pcs <- 1:npcs
  } else {
    npcs <- length(pcs)
  }
  
  max_pcs <- max(pcs) # maximum pc
  
  # compute svd of X
  if (!missing(X)) {
    if (max_pcs / min(nrow(X), ncol(X)) > .25) { # full svd
      X_svd <- svd(X)
    } else { # only compute top singular vectors
      X_svd <- X %>% irlba(nu = max_pcs, nv = max_pcs)
    }
  } else {
    X_svd <- list(
      u = pca.out$scores,
      v = pca.out$loadings,
      var_explained = pca.out$var.explained
    )
  }
  
  # compute and show proportion of variance
  if (show.var) { 
    if (!missing(X)) {
      total_var <- norm(X, "F")^2
      var_explained <- X_svd$d^2 / total_var
    } else {
      var_explained <- X_svd$var_explained
    }
    var_explained_str <- paste0(" (", round(var_explained, 3), ")")
  } else {
    var_explained <- NA
    var_explained_str <- rep("", times = max_pcs)
  }
  
  # initializing data frame for plotting
  plt_df <- data.frame(X_svd$u)
  
  # adding labels for colors
  if (!is.null(color) & !is.null(color2)) {
    plt_df <- plt_df %>%
      mutate(
        color = color,
        color2 = color2
      )
  } else if (!is.null(color)) {
    plt_df <- plt_df %>%
      mutate(color = color)
  }
  
  # subsample points
  if (subsample != 1) {
    plt_df <- sample_frac(plt_df, size = subsample, replace = F)
  }
  
  if (npcs == 2) {  # ggplot object instead of ggpairs
    
    if (!is.null(color)) {  # plot with color
      plt <- ggplot(plt_df) +
        aes(x = X1, y = X2, color = color) +
        geom_point(alpha = alpha, size = size) +
        labs(
          x = paste0("PC1", var_explained_str[1]),
          y = paste0("PC2", var_explained_str[2]),
          color = color.label, title = title
        ) +
        myGGplotTheme(...)
      
      if (is.null(manual.color)) {
        plt <- plt + myGGplotColor(color = plt_df$color)
      } else {
        plt <- plt + scale_color_manual(values = manual.color)
      }
    } else {  # plot without color
      plt <- ggplot(plt_df) +
        aes(x = X1, y = X2) +
        geom_point(alpha = alpha, size = size) +
        labs(
          x = paste0("PC1", var_explained_str[1]),
          y = paste0("PC2", var_explained_str[2]),
          title = title
        ) +
        myGGplotTheme(...)
    }
  } else {
    plt <- plotPairs(data = plt_df, columns = pcs, title = title, 
                     size = size, alpha = alpha, show.upper = F, drop = F, 
                     color = color, color2 = color2, 
                     color.label = color.label, color2.label = color2.label,
                     manual.color = manual.color, manual.color2 = manual.color2,
                     columnLabels = paste0("PC", pcs, var_explained_str[pcs]),
                     ...)
  }
  
  if (show.plot) {
    print(plt)
  }
  
  if (save) { # save figure to file
    if (!is.null(path)) {
      saveRDS(plt, paste0("./", path))
    } else {
      saveRDS(plt, paste0("./", title, ".rds"))
    }
  }
  
  return(list(
    plot = plt, scores = X_svd$u, loadings = X_svd$v,
    var.explained = var_explained
  ))
}

plotHeatmap <- function(X, y.labels = rownames(X), x.labels = colnames(X),
                        text.size = 0, theme = "default", position = "identity",
                        viridis = T, option = "C", 
                        col_quantile = F, n_quantiles = 5,
                        manual.fill = NULL, show.plot = F, ...) {
  ####### Function Description ########
  # plot nice heatmap of X
  #
  # inputs:
  # - X = data matrix or data frame
  # - y.labels = column names of X
  # - x.labels = row names of X
  # - text.size = numeric; size of text on heatmap; no text if text.size = 0
  # - theme = "default" or "blank"
  # - position = "identity" or "ordered"
  # - viridis = logical; whether or not to use viridis color scheme
  # - option = viridis option argument
  # - col_quantile = logical; whether or not to use quantile color scale
  # - n_quantiles = number of quantiles for color scheme; only used if
  #     col_quantile = T
  # - manual.fill = "temperature" or vector of colors for the color scale
  #     (optional)
  # - show.plot = logical; whether or not to print plot
  # - ... = additional arguments to pass to myGGplotTheme()
  #
  # outputs: a ggplot object
  # 
  # example usage:
  # df <- as.data.frame(matrix(1:100, nrow = 10, byrow = 10))
  # plotHeatmap(df,  position = "identity")
  #######
  
  # convert to long df to plot
  X_long <- as.data.frame(X) %>%
    setNames(x.labels) %>%
    mutate(y = factor(y.labels, levels = y.labels)) %>%
    gather(data = ., -y, key = "x", value = "fill") %>%
    mutate(x = factor(x, levels = x.labels))
  
  # base plot
  plt <- ggplot(X_long) +
    aes(x = x, y = y, fill = fill) +
    geom_tile() 
  
  # add text if specified
  if (text.size > 0) {
    plt <- plt +
      geom_text(aes(x = x, y = y, label = fill), 
                color = "black", size = text.size)
  }
  
  # add theme
  if (identical(theme, "default")) {
    plt <- plt + myGGplotTheme(...)
  } else if (identical(theme, "blank")) {
    plt <- plt + myGGplotMapTheme()
  } else {
    stop("Unknown theme argument.")
  }
  
  # add color
  if (!col_quantile | is.factor(X_long$fill)) {
    if (is.null(manual.fill)) {
      plt <- plt +
        myGGplotFill(fill = fill, viridis = viridis, option = option)
    } else {
      if (manual.fill == "temperature") {
        plt <- plt + 
          scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                               midpoint = median(X_long$fill),
                               limit = c(min(X_long$fill), max(X_long$fill)))
      } else {
        plt <- plt +
          scale_fill_manual(values = manual.fill)
      }
    }
  } else {
    if (!is.null(manual.fill)) {
      if (manual.fill == "temperature") {
        heat_pal <- c("blue", "white", "red")
      } else {
        heat_pal <- manual.fill
      }
    } else {
      heat_pal <- viridis(n = n_quantiles, option = option)
    }
    probs <- seq(0, 1, length = length(heat_pal))
    quantiles <- quantile(X_long$fill, probs, na.rm = T)
    heat_pal_values <- (quantiles - min(quantiles)) /
      (max(quantiles) - min(quantiles))
    plt <- plt + 
      scale_fill_gradientn(values = heat_pal_values,
                           colors  = heat_pal)
  }
  
  # image position
  if (identical(position, "identity")) {
    plt <- plt +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0), limits = rev(levels(X_long$y)))
  } else if (identical(position, "ordered")) {
    plt <- plt + 
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0))
  } else {
    stop("Unknown position argument.")
  }
  
  if (show.plot) {
    print(plt)
  }
  
  return(plt)
}

plotHclust <- function(data, y = NULL,
                       dist.metric = "euclidean", linkage = "ward.D",
                       show.y.text = T, text.size = 0.5, show.plt = F,
                       title = NULL, manual.color = NULL, 
                       save = F, save.filename = NULL) {
  ####### Function Description ########
  # perform hierarchical clustering and plot resulting dendrogram
  # 
  # inputs:
  # - data = data matrix or data frame
  # - y = class labels or outcome (optional)
  # - dist.metric = distance metric (see stats::dist)
  # - linkage = type of linkage for hierarchical clustering (see stats::hclust)
  # - show.y.text = logical; whether or not to print y text or just the color
  # - text.size = size of text for leaves
  # - title = string for plot title name
  # - manual.color = vector of manual colors for leaf text labels
  # - show.plt = logical; whether or not to show plot
  # - save = logical; whether or not to save plot as rds file
  # - save.filename = string ending in .rds; where to save file
  # 
  # output: list of 2
  # - hclust = output of hclust()
  # - dend = hierarchical clustering dendrogram
  #
  # example usage:
  # out <- plotHclust(data = iris[, -5], y = iris$Species, show.plt = T)
  # plot(out$dend)
  ####### 

  data <- as.matrix(data)
  
  if (sum(is.na(data)) > 0) {
    stop("NAs found in data")
  }
  
  # distance matrix
  Dmat <- dist(data, method = dist.metric)
  
  # hierarchical clustering
  hclust_out <- hclust(Dmat, method = linkage)
  hclust_dend <- as.dendrogram(hclust_out)
  
  if (!is.null(y)) {  # annotate tree leaves
    
    if (is.factor(y)) {  # categorical y
      if (is.null(manual.color)) {
        my_colors <- 1:length(unique(y))
      } else {
        my_colors <- manual.color
      }
      labels_colors(hclust_dend) <- my_colors[y][order.dendrogram(hclust_dend)]
    } else {  # continuous y
      if (is.null(manual.color)) {
        my_colors <- colorNumeric(palette = "viridis", domain = c(min(y), max(y)))
      } else {
        my_colors <- manual.color
      }
      labels_colors(hclust_dend) <- my_colors(y)[order.dendrogram(hclust_dend)]
    }
    
    labels(hclust_dend) <- as.character(y)[order.dendrogram(hclust_dend)]
    dend_colors <- labels_colors(hclust_dend)
    if (!show.y.text) {
      labels(hclust_dend) <- "------"
    }
    
    hclust_dend <- hang.dendrogram(hclust_dend, hang_height = 0.1)
    hclust_dend <- assign_values_to_leaves_nodePar(hclust_dend, 
                                                   text.size, "lab.cex")
  }
  
  if (is.null(title)) {
    title <- paste0(
      "Hierarchical Clustering: \n",
      linkage, " Linkage, ", dist.metric, " Distance"
    )
  }

  if (show.plt) {
    plot(hclust_dend, main = title, horiz = FALSE)
    if (!is.null(y)) {
      if (is.factor(y)) {
        legend("topright", legend = levels(y), col = my_colors,
               lwd = 1, bty = "n", cex = 0.8)
      }
    }
  }
  
  if (save) { # save figure to file
    if (!is.null(save.filename)) {
      saveRDS(hclust_dend, paste0("./", save.filename))
    } else {
      saveRDS(hclust_dend, paste0("./", title, ".rds"))
    }
  }
  
  return(list(hclust = hclust_out, dend = hclust_dend, 
              dend_colors = dend_colors))
}
