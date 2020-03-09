#' @name summarize
#' @rdname summarize
#' @export
setMethod("summarize_samples",
          signature = signature(object = "SingleCellExperiment"),
          definition = function(object, group_by = NULL, ...) {
            if(!is.null(group_by)) group_by <- SummarizedExperiment::colData(object)[[group_by]]
            summarize_data_(data = SummarizedExperiment::colData(object), group_by = group_by, ...) %>%
              kableExtra::kable_styling(bootstrap_options = "striped", full_width = F) %>%
              kableExtra::footnote(general = "Summarized values from samples.",
                       general_title = "Note: ",
                       footnote_as_chunk = T, title_format = c("bold", "underline")
              )
          })

#' @name summarize
#' @rdname summarize
#' @export
setMethod("summarize_features",
          signature = signature(object = "SingleCellExperiment"),
          definition = function(object, group_by = NULL, ...) {
            if(!is.null(group_by)) group_by <- SummarizedExperiment::rowData(object)[[group_by]]
            summarize_data_(data = SummarizedExperiment::rowData(object), group_by = group_by, ...) %>%
              kableExtra::kable_styling(bootstrap_options = "striped", full_width = F) %>%
              kableExtra::footnote(general = "Summarized values from features.",
                                   general_title = "Note: ",
                                   footnote_as_chunk = T, title_format = c("bold", "underline")
              )
          })

#' @name summarize
#' @rdname summarize
#' @export
setMethod("summarize_samples",
          signature = signature(object = "Seurat"),
          definition = function(object, group_by = NULL, ...) {
            if(!is.null(group_by)) group_by <- object@meta.data[[group_by]]
            summarize_data_(data = object@meta.data, group_by = group_by, ...) %>%
              kableExtra::kable_styling(bootstrap_options = "striped", full_width = F) %>%
              kableExtra::footnote(general = "Summarized values from samples.",
                                   general_title = "Note: ",
                                   footnote_as_chunk = T, title_format = c("bold", "underline")
              )
          })

#' @name summarize
#' @rdname summarize
#' @export
setMethod("summarize_features",
          signature = signature(object = "Seurat"),
          definition = function(object, assay = "RNA", group_by = NULL, ...) {
            if(!is.null(group_by)) group_by <- object@assays[[assay]]@meta.features[[group_by]]
            summarize_data_(data = object@assays[[assay]]@meta.features, group_by = group_by, ...) %>%
              kableExtra::kable_styling(bootstrap_options = "striped", full_width = F) %>%
              kableExtra::footnote(general = "Summarized values from features.",
                                   general_title = "Note: ",
                                   footnote_as_chunk = T, title_format = c("bold", "underline")
              )
          })


#' Summarizes columns of \code{data} using \code{FUNS} and can be grouped by \code{group_by}.
#'
#' @param data A data.frame.
#' @param columns The columns of \code{data} that are summarized.
#' @param FUNS A named vector of summary functions
#'
#' @return A \code{data.frame} that contains summary statistics.
summarize_data_ <- function(data, columns, FUNS, group_by = NULL) {
  if(is.null(group_by)){
    df <- df_without_grouping(data = data, columns = columns, FUNS = FUNS)
    return(df)
  } else {
    df_list <- list()
    for (func in FUNS){
      df_list[[func]] <- df_per_func(data = data, columns = columns, func = func, group_by = group_by)
    }
    df <- do.call("cbind", df_list)

    #
    # prepare a header vector for kableExtra::add_header_above
    #
    level_nr <- length(unique(group_by))
    FUNS_nr <- length(FUNS)
    header_vector <- c(1, rep(level_nr, FUNS_nr))
    names(header_vector) <- c(" ", FUNS)

    kableExtra::kable(df) %>%
      kableExtra::kable_styling("striped") %>%
      kableExtra::add_header_above(header_vector) -> kb
    return(kb)
  }
}

#' Function to create a data.frame containing one column for each provided function (rownames = selected colData)
#' @param data A data.frame.
#' @param columns The columns of \code{data} that are summarized.
#' @param func A character of one summary function.
#' @param group_by A vector of the same length as the number of rows in data, which is used for grouping.
#'
#' @return data.frame
df_without_grouping <- function(data, columns, FUNS){
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
      dplyr::mutate(variable = columns) -> df
  } else {
    data %>%
      as.data.frame() %>%
      dplyr::select(.dots = columns) %>%
      dplyr::summarise_if(is.numeric, .funs = FUNS) %>%
      cbind("variable" = columns) -> df
  }

  df %>%
    tibble::column_to_rownames("variable") %>%
    round(3) %>%
    kableExtra::kable() %>%
    kableExtra::kable_styling("striped")
}


#' Function to create a data.frame per function: rownames = selected colData | column for each level of group_by
#' @param data A data.frame.
#' @param columns The columns of \code{data} that are summarized.
#' @param func A character of one summary function.
#' @param group_by A vector of the same length as the number of rows in data, which is used for grouping.
#'
#' @return data.frame
df_per_func <- function(data, columns, func, group_by){
  data %>%
    as.data.frame() %>%
    dplyr::select(.dots = columns) %>%
    dplyr::mutate(grouping = group_by) %>%
    dplyr::group_by(grouping) %>%
    dplyr::summarise_if(is.numeric, .funs = func) %>%
    tibble::column_to_rownames("grouping") %>%
    t() %>%
    round(3) -> df_per_fun
  rownames(df_per_fun) <- columns
  return(df_per_fun)
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
