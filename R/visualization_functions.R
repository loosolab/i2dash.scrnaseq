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

#' Render a bar plot
#'
#' @param cluster Values for the membership to clusters. In case of a nested list, a dropdown menu will be provided in the interactive mode.
#' @param x Numeric values mapped to the x-axis. In case of a nested list, a dropdown menu will be provided in the interactive mode.
#'
#' @return A list with 1. the plotly object & 2. the data frame used in the plot
#' @export
.bar_plot <- function(cluster, x = NULL){

  #if x = NULL -> plot for "Number of cells"
  #if x != NULL -> plot for "Number of fractions"
  if(is.null(x)){
    tab <- table(cluster)
    tab_df <- as.data.frame(tab)

    # plotly
    title <- "Number of cells"
    p <- plotly::plot_ly(tab_df, x = tab_df[[2]], y = tab_df[[1]],
                         name = names(tab_df[1]),
                         type = "bar", orientation = "h", opacity = 0.7)
    p <- plotly::layout(p,
                        xaxis = list(title=title, showline = T),
                        yaxis = list(title="Cluster", showline = T, showticklabels = T),
                        showlegend = F
    )
    return(list("plot" = p, "df" = tab_df))
  } else {
    # create data.frame for plot
    tab <- table(cluster[[1]],x[[1]])
    ptab <- prop.table(tab,margin = 1)
    ptab_df <- as.data.frame.matrix(ptab)

    # plotly
    title <- "Fraction of cells"
    p <- plotly::plot_ly(ptab_df, type = "bar", orientation = "h", opacity = 0.7)
    for(i in 1:length(names(ptab_df))){
      p <- plotly::add_trace(p, x = ptab_df[[i]], y = row.names(ptab_df), name = names(ptab_df[i]))
    }
    p <- plotly::layout(p,
                        xaxis = list(title=title, showline = T),
                        yaxis = list(title="Cluster", showline = T, showticklabels = T),
                        barmode = 'stack',
                        showlegend = T
    )
    return(list("plot" = p, "df" = ptab_df))
  }

}

#' Render a box plot with plotly.
#'
#' @param x Numeric observations for the boxplot.
#' @param group_by A factor, by which observations can optionally be grouped.
#' @param title A title that describes the observations.
#' @param group_by_title A title that describes the grouping factor.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_boxplot <- function(x, group_by = NULL, title = "", group_by_title = NULL){
  plotly::plot_ly(x = x, y = group_by, type = "box", name = title) %>%
    plotly::layout(xaxis = list(title = title, showline = T),
                   yaxis = list(title = group_by_title, showline = T, showticklabels = T),
                   showlegend = T)
}

#' Render a vertical violin plot with plotly.
#'
#' @param df A dataframe containing the data for the violinplot.
#' @param group_by A factor, by which observations can be grouped. In case of a named list, a dropdown menu will be provided in the interactive mode.
#' @param title_y A title of the y-axis that describes the observations. In case of a named list this parameter is not needed because the names of the list will be used as title of the y axis.
#' @param title_group_by A title of the x-axis that describes the grouping factor. In case of a named list this parameter is not needed because the names of the list will be used as title of the x axis.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_violinplot <- function(df, group_by = NULL, title_y = NULL, title_group_by = NULL){
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
  y <- df[[1]]

  plotly::plot_ly(x = x, y = y, split = x, type = 'violin',
                  box = list(visible = T),
                  meanline = list(visible = T)) %>%
    plotly::layout(xaxis = list(title = title_group_by, showline = T),
                   yaxis = list(title = title_y, showline = T),
                   showlegend = showlegend)
}

