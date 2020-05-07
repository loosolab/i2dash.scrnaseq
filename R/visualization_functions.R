#' Render a scatterplot with plotly.
#'
#' @param ... Additional arguments passed on to \code{plotly::plot_ly}.
#' @param y_title The title of the x-axis.
#' @param x_title The title of the y-axis.
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
#' @param ... Additional arguments passed on to \code{plotly::plot_ly}.
#' @param showlegend (Optional) Boolean value that describes if the legend should be shown.
#' @param x_group_by_title (Optional) A title that describes the grouping factor along the x-axis.
#' @param y_group_by_title (Optional) A title that describes the grouping factor along the y-axis.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_barplot <- function(..., showlegend = NULL, x_group_by_title = NULL, y_group_by_title = NULL){
  plotly::plot_ly(..., type = "bar", orientation = "h", opacity = 0.7) %>%
    plotly::layout(xaxis = list(title = x_group_by_title, showline = T),
                   yaxis = list(title = y_group_by_title, showline = T, showticklabels = T),
                   barmode = 'stack',
                   showlegend = showlegend)
}

#' Render a boxplot with plotly.
#'
#' @param x Numeric observations for the boxplot.
#' @param group_by A factor, by which observations can optionally be grouped.
#' @param x_title A title that describes the observations.
#' @param group_by_title A title that describes the grouping factor.
#' @param ... Additional arguments passed on to \code{plotly::plot_ly}.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_boxplot <- function(x, group_by = NULL, x_title = NULL, group_by_title = NULL, ...){
  plotly::plot_ly(x = x, y = group_by, type = "box", color = group_by, ...) %>%
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

#' Render a heatmap with ComplexHeatmap.
#'
#' @param ... Additional arguments passed on to \code{ComplexHeatmap::Heatmap}.
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

#' Render a bubbleplot with plotly.
#'
#' @param x Numeric observations mapped to the x-axis.
#' @param y Numeric observations mapped to the y-axis.
#' @param size Numeric values defining the size of the dots.
#' @param x_title The title of the x-axis.
#' @param y_title The title of the y-axis.
#' @param ... these arguments are of either the form \code{value} or \code{tag = value} and should be valid for the \code{plotly::plot_ly()} method.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_bubbleplot <- function(x, y, size, x_title = NULL, y_title = NULL, ...){
  plotly::plot_ly(x = x, y = y, size = size, type = 'scatter', mode = 'markers', marker = list( oparcity = 0.5), ...) %>%
    plotly::layout(xaxis = list(title = x_title, showgrid = FALSE),
                   yaxis = list(title = y_title, showgrid = FALSE)
    )
}

