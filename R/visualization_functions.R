#' Render a scatter plot
#'
#' @param labels A list with sample names, that should be of the same length as x and y.
#' @param x Numeric values mapped to the x-axis. In case of a nested list, a dropdown menu will be provided in the interactive mode.
#' @param y Numeric values mapped to the y-axis. In case of a nested list, a dropdown menu will be provided in the interactive mode.
#' @param colour_by A factor that will be mapped to colours. In case of a nested list, a dropdown menu will be provided in the interactive mode.
#'
#' @return A list with 1. the plotly object & 2. the data frame used in the plot
#' @export
.scatter_plot <- function(labels = NULL, x, y, colour_by = NULL, checkbox = FALSE, selected_label = NULL){
  # create data.frame for plot (fill colour_by & labels with dummy data if NULL)
  dummy_values <- FALSE
  plot_list <- list()
  if (!is.null(labels)) plot_list <- rlist::list.append(plot_list, labels) else plot_list <- rlist::list.append(plot_list, c(1:length(x[[1]])))
  plot_list <- rlist::list.append(plot_list, c(x, y))
  if (!is.null(colour_by)) {
    plot_list <- rlist::list.append(plot_list, colour_by)
  } else {
    plot_list <- rlist::list.append(plot_list, c(1:length(x[[1]])))
    dummy_values <- TRUE
  }
  plot_df <- do.call("data.frame", plot_list)

  # assign variables
  labels <- plot_df[[1]]
  x_value <- plot_df[[2]]
  y_value <- plot_df[[3]]
  colour_by <- plot_df[[4]]

  x_title <- names(plot_df[2])
  y_title <- names(plot_df[3])

  if(dummy_values) {
    colour_by <- NULL
  }
  # if(is.null(labels)) {
  #   labels <- NULL
  # }
  if(is.null(checkbox)) {
    checkbox <- FALSE
  }

  # find point for annotation
  a <- NULL
  if (!is.null(labels)) {
    if (checkbox) {
      colour_by <- NULL
      point_index <- match(selected_label, plot_df[[1]])
      point <- plot_df[point_index,]
      a <- list(
        x = point[,2],
        y = point[,3],
        text = point[,1],
        xref = "x",
        yref = "y",
        showarrow = T,
        arrowhead = 7,
        arrowcolor = "red",
        ax = 20,
        ay = -40
      )
    }
  }

  # plotly
  p <- plotly::plot_ly(plot_df,
                       x = x_value,
                       y = y_value,
                       color = colour_by,
                       text = labels
  )
  p <- plotly::add_markers(p)
  p <- plotly::layout(p,
                      xaxis = list(title = x_title),
                      yaxis = list(title = y_title),
                      annotations = a)
  return(p)
}

#' Function to create a dataframe for plotly_barplot.

#' @param group_by A list with factorial values, by which observations can optionally be grouped.
#' @param x (Optional) A named list with the observations for the barplot.
#'
#' @return An object of class \code{list} containig the dataframe 'df', the vector 'x' with values for the x-axis, the vector 'y' with values for the y-axis, the vector 'names' (can be NULL), the boolean value 'showlegend'.
#' @export
create_barplot_df <- function(group_by, x = NULL){
  if(is.null(x)){
    tab <- table(group_by)
    df <- as.data.frame(tab)
    x <- df[2]
    y <- df[1]
    names <- NULL
    showlegend <- F
    return(list("df" = df, "x" = x, "y" = y, "names" = names, "showlegend" = showlegend))
  } else {
    tab <- table(group_by, x)
    ptab <- prop.table(tab, margin = 1)
    df <- as.data.frame(ptab)
    x <- df[3]
    y <- df[1]
    names <- df[2]
    showlegend <- T
    return(list("df" = df, "x" = x, "y" = y, "names" = names, "showlegend" = showlegend))
  }
}

#' Render a bar plot with plotly.
#'
#' @param ... these arguments are of either the form value or tag = value and should be valid for the 'plotly::plot_ly()' method.
#' @param showlegend (Optional) Boolean value that describes if the legend should be shown.
#' @param title_x (Optional) A title that describes the observations.
#' @param title_group_by (Optional) A title that describes the grouping factor.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_barplot <- function(..., showlegend = NULL, title_x = NULL, title_group_by = NULL){
  p <- plotly::plot_ly(..., type = "bar", orientation = "h", opacity = 0.7) %>%
    plotly::layout(xaxis = list(title = title_x, showline = T),
                   yaxis = list(title = title_group_by, showline = T, showticklabels = T),
                   barmode = 'stack',
                   showlegend = showlegend)
  p
}

#' Render a box plot with plotly.
#'
#' @param df A dataframe containing the data fo the boxplot.
#' @param ... these arguments are of either the form value or tag = value and should be valid for the 'plotly::plot_ly()'
#' @param title_x A title that describes the observations.
#' @param title_group_by A title that describes the grouping factor.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_boxplot <- function(df, ..., title_x = NULL, title_group_by = NULL){
  # if (is.null(group_by)) {
  #   y <- NULL
  # } else {
  #   y <- group_by[[1]]
  # }
  # x <- x[[1]]

  plotly::plot_ly(..., type = "box") %>%
    plotly::layout(xaxis = list(title = title_x, showline = T),
                   yaxis = list(title = title_group_by, showline = T, showticklabels = T),
                   showlegend = F)
}

