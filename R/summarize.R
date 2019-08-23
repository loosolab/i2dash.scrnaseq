#' Summarize samples or features of objects containing data from single-cell experiments.
#'
#' @param object A SingleCellExperiment or Seurat object.
#' @param columns The metadata columns to summarize.
#' @param FUNS A named vector indicating summary functions.
#'
#' @return A \code{data.frame} with one row for each \code{columns} and one column for each \code{FUNS}, containing the desired summary. An additional column \code{group} is added which indicates whether values from \emph{Samples} or \emph{Features} were summarized.
#' @name summarize
NULL
#> NULL

#' @export
#' @rdname summarize
setGeneric("summarize_samples", function(object, ...) standardGeneric("summarize_samples"))

#' @export
#' @rdname summarize
setGeneric("summarize_features", function(object, ...) standardGeneric("summarize_features"))


setMethod("summarize_samples",
          signature = signature(object = "SingleCellExperiment"),
          definition = function(object, ...) {
            summarize_data_(data = SummarizedExperiment::colData(object), ...) %>%
              dplyr::mutate(group = "Samples")
          })

setMethod("summarize_features",
          signature = signature(object = "SingleCellExperiment"),
          definition = function(object, ...) {
            summarize_data_(data = SummarizedExperiment::rowData(object), ...) %>%
              dplyr::mutate(group = "Features")
          })

setMethod("summarize_samples",
          signature = signature(object = "Seurat"),
          definition = function(object, ...) {
            summarize_data_(data = SummarizedExperiment::colData(object), ...) %>%
              dplyr::mutate(group = "Samples")
          })

setMethod("summarize_features",
          signature = signature(object = "Seurat"),
          definition = function(object, ...) {
            summarize_data_(data = SummarizedExperiment::rowData(object), ...) %>%
              dplyr::mutate(group = "Features")
          })


#' Summarizes columns of \code{data} using \code{FUNS}.
#'
#' @param data A data.frame.
#' @param columns The columns of \code{data} that are summarized.
#' @param FUNS A named vector of summary functions
#'
#' @return A \code{data.frame} that contains summary statistics.
summarize_data_ <- function(data, columns, FUNS = c("mean" = "mean", "median" = "median")) {
  separate_regex <- paste0("_(?=[",paste0(FUNS, collapse = "|"),"])")

  if(!assertive.properties::has_names(FUNS)) {
    names(FUNS) <- FUNS
  }

  if(length(columns) > 1) {
    data %>%
      as.data.frame() %>%
      dplyr::select(.dots = columns) %>%
      dplyr::summarise_if(is.numeric, .funs = FUNS) %>%
      tidyr::gather() %>%
      dplyr::mutate(key = correct_name(key, columns)) %>%
      tidyr::separate(col = key, into = c("variable", "stat"), sep = separate_regex) %>%
      tidyr::spread(key = stat, value = value) %>%
      dplyr::mutate(variable = columns)
  } else {
    data %>%
      as.data.frame() %>%
      dplyr::select(.dots = columns) %>%
      dplyr::summarise_if(is.numeric, .funs = FUNS) -> stats_table
    cbind("Variable" = columns, stats_table)
  }
}

#' Helper function to correct column names
#' See https://github.com/tidyverse/dplyr/issues/1902 for additional information.
#'
#' @param key The key.
#' @param prefix The prefix.
#'
#' @return The corrected column names.
correct_name <- function(key, prefix) {
  if(length(prefix) > 1) return(key)
  return(paste(prefix, key, sep = "_"))
}
