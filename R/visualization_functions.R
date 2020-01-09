#' Creates a plotly scatterplot
#'
#' @param df A dataframe containing the data for the boxplot
#' @param labels A list with sample names, which should be of the same length as x and y.
#' @param colour_by A list containing factorial values that will be used for colouring. In case of a named list, a dropdown menu will be provided in the interactive mode. Note: The length of all vectors in case of a named list should be of the same length as x and y.
#' @param checkbox A boolean value as indicator for colouring by labels.
#' @param selected_label The label (character) selected by the user.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_scatterplot <- function(..., y_title = NULL, x_title = NULL){
  plotly::plot_ly(...) %>%
    plotly::layout(xaxis = list(title = x_title),
                   yaxis = list(title = y_title)
    )
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
#' @param ... Additional arguments passed on to \code{plotly::plot_ly}.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_violinplot <- function(y, group_by = NULL, y_title = NULL, group_by_title = NULL, ...){
  plotly::plot_ly(x = group_by, y = y, color = group_by, type = "violin", box = list(visible = T), meanline = list(visible = T), points = "all", jitter = 0, ...) %>%
    plotly::layout(
      xaxis = list(title = group_by_title),
      yaxis = list(title = y_title))
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

