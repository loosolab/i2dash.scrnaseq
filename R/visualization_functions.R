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
