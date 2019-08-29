#' Render a vertical violin plot with plotly.
#'
#' @param df A data.frame containing in the first column numerical values for the y axis and optionally factor in the second column for grouping the observations along the x axis.
#'
#' @return An object of class \code{plotly}.
#' @export
plotly_violinplot <- function(df){
  if(ncol(df) > 1){
    df %>%
      plotly::plot_ly(colors = "Set1", x = df[,2], y = df[,1], color = df[,2], type = "violin", box = list(visible = T), meanline = list(visible = T), points = "all", jitter = 0) %>%
      plotly::layout(
        xaxis = list(title = colnames(df)[2]),
        yaxis = list(title = colnames(df)[1])
      )
  } else {
    df %>% plotly::plot_ly(colors = "Set1", x = NULL, y = df[,1], color = NULL, type = "violin", box = list(visible = T), meanline = list(visible = T), points = "all", jitter = 0) %>%
      plotly::layout(
        xaxis = list(title = NULL),
        yaxis = list(title = colnames(df)[1])
      )
  }
}

