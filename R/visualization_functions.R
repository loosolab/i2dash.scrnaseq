#' Renders a scatter plot
#'
#' @param df A dataframe containing the data for the boxplot
#' @param labels A list with sample names, which should be of the same length as x and y.
#' @param colour_by A list containing factorial values that will be used for colouring. In case of a named list, a dropdown menu will be provided in the interactive mode. Note: The length of all vectors in case of a named list should be of the same length as x and y.
#' @param checkbox A boolean value as indicator for colouring by labels.
#' @param selected_label The label (character) selected by the user.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_scatterplot <- function(df, labels = NULL, colour_by = NULL, expression = NULL, checkbox = FALSE, expr_checkbox = FALSE, selected_label = NULL){

  if(is.null(checkbox)) checkbox <- FALSE
  if(is.null(expr_checkbox)) expr_checkbox <- FALSE

  # assign variables
  if(!is.null(labels)) labels <- df[[1]]
  x_value <- df[[2]]
  y_value <- df[[3]]
  if(!is.null(expression) & expr_checkbox) colour_by <- df[[4]]
  if(!is.null(colour_by)) colour_by <- df[[4]]
  x_title <- names(df[2])
  y_title <- names(df[3])

  # find point for annotation
  a <- NULL
  if (checkbox) {
    colour_by <- NULL
    point_index <- match(selected_label, df[[1]])
    point <- df[point_index,]
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

  # plotly
  plotly::plot_ly(df, x = x_value, y = y_value, color = colour_by, text = labels) %>%
    plotly::add_markers() %>%
    plotly::layout(xaxis = list(title = x_title),
                   yaxis = list(title = y_title),
                   annotations = a)
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

#' Function to create a dataframe for plotly_violinplot.
#'
#' @param y A list with the observations for the violinplot.
#' @param group_by (Optional) A list with factorial values, by which observations can optionally be grouped.
#' @param title_y (Optional) The title of the y-axis that describes the observations.
#' @param title_group_by (Optional) The title of the x-axis that describes the grouping factor.
#'
#' @return An object of class \code{list} containig the dataframe 'df', the vector 'x' with values for the x-axis, the vector 'y' with values for the y-axis, the vector 'split', the boolean value 'showlegend', the character string "title_y", the character string "title_group_by".
#' @export
create_violinplot_df <- function(y, group_by = NULL, title_y = NULL, title_group_by = NULL){
  # create data_frame
  if(is.null(group_by)){
    df <- data.frame(y)
  } else {
    df <- data.frame(y, group_by)
  }

  # manage the titles of axis
  if(is.null(title_y)) title_y <- names(df[1]) else title_y <- title_y
  if(is.null(title_group_by) & !is.null(group_by)) title_group_by <- names(df[2]) else title_group_by <- title_group_by

  # set variables in dependence of 'group_by'
  if (is.null(group_by)) {
    x <- NULL
    showlegend <- F
  } else {
    x <- df[[2]]
    showlegend <- T
  }

  return(list("df"=df, "x"=x, "y"=df[[1]], "split"=x, "showlegend"=showlegend, "title_y"=title_y, "title_group_by"=title_group_by))
}

#' Render a vertical violin plot with plotly.
#'
#' @param ... these arguments are of either the form value or tag = value and should be valid for the 'plotly::plot_ly()' method.
#' @param showlegend Boolean value that describes if the legend should be shown.
#' @param title_y (Optional) The title of the y-axis that describes the observations.
#' @param title_group_by (Optional) The title of the x-axis that describes the grouping factor.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_violinplot <- function(..., showlegend = F, title_y = NULL, title_group_by = NULL){
  plotly::plot_ly(..., type = 'violin',
                  box = list(visible = T),
                  meanline = list(visible = T)) %>%
    plotly::layout(xaxis = list(title = title_group_by, showline = T),
                   yaxis = list(title = title_y, showline = T),
                   showlegend = showlegend)
}

