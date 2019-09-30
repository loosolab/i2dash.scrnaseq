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

#' Render a bar plot with plotly.
#'
#' @param ... these arguments are of either the form value or tag = value and should be valid for the 'plotly::plot_ly()' method.
#' @param showlegend (Optional) Boolean value that describes if the legend should be shown.
#' @param title_x (Optional) A title that describes the observations.
#' @param title_group_by (Optional) A title that describes the grouping factor.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_barplot <- function(..., showlegend = NULL, x_group_by_title = NULL, y_group_by_title = NULL){
  p <- plotly::plot_ly(..., type = "bar", orientation = "h", opacity = 0.7) %>%
    plotly::layout(xaxis = list(title = x_group_by_title, showline = T),
                   yaxis = list(title = y_group_by_title, showline = T, showticklabels = T),
                   barmode = 'stack',
                   showlegend = showlegend)
  p
}

#' Render a boxplot with plotly.
#'
#' @param x Numeric observations for the boxplot.
#' @param group_by A factor, by which observations can optionally be grouped.
#' @param x_title A title that describes the observations.
#' @param group_by_title A title that describes the grouping factor.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_boxplot <- function(x, group_by = NULL, x_title = NULL, group_by_title = NULL){
  plotly::plot_ly(x = x, y = group_by, type = "box", colors = "Set1",  color = group_by) %>%
    plotly::layout(xaxis = list(title = x_title, showline = T),
                   yaxis = list(title = group_by_title, showline = T, showticklabels = T))
}

#' Render a vertical violin plot with plotly.
#'
#' @param y Numeric observations.
#' @param group_by A factor, by which observations can optionally be grouped.
#' @param y_title A title that describes the observations.
#' @param group_by_title A title that describes the grouping factor.
#'
#' @return An object of class \code{plotly}.
#' @export
 plotly_violinplot <- function(y, group_by = NULL, y_title = NULL, group_by_title = NULL){
      plotly::plot_ly(colors = "Set1", x = group_by, y = y, color = group_by, type = "violin", box = list(visible = T), meanline = list(visible = T), points = "all", jitter = 0) %>%
      plotly::layout(
        xaxis = list(title = group_by_title),
        yaxis = list(title = y_title)
      )
}

#' Render a heatmap with ComplexHeatmap.
#'
#' @param ... further optional and valid arguments, that are the supported arguments in ComplexHeatmap.
#' @param legend_title An optional title of the legend.
#' @return An object of class \code{Heatmap}.
#' @export
ComplexHeatmap_heatmap <- function(..., legend_title = NULL){
  if (!requireNamespace("ComplexHeatmap", quietly = TRUE)){
    stop("Package ComplexHeatmap is needed to use this function. Please install it.")
  }
  ComplexHeatmap::Heatmap(...,
                          row_title_side = "right",
                          row_names_side = "left",
                          row_dend_side = "right",
                          column_names_side = "top",
                          heatmap_legend_param = list(
                            color_bar = "continuous",
                            title = legend_title
                          ))

}












