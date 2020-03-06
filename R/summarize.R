#' Summarizes columns of \code{data} using \code{FUNS} and can be grouped by \code{group_by}.
#'
#' @param data A data.frame.
#' @param columns The columns of \code{data} that are summarized.
#' @param FUNS A named vector of summary functions
#'
#' @return A \code{data.frame} that contains summary statistics.
summarize_data_ <- function(data, columns, FUNS, group_by = NULL) {
  if(is.null(group_by)){
    df_without_grouping(data = data, columns = columns, FUNS = FUNS) %>%
      return(.)
  } else {
    lapply(FUNS, function(func) df_per_func(data = data, columns = columns, func = func, group_by = group_by)) %>%
      do.call("cbind", .) %>%
      return(.)
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
  df %<>%
    tibble::column_to_rownames("variable") %>%
    round(3)
  return(df)
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


#' @rdname summarize_metadata
#' @return A string containing markdown code for the rendered component
setMethod("summarize_metadata",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, data, columns = colnames(data), FUNS = c("min", "max", "mean", "median"), group_by = NULL, caption = NULL, description = NULL, title = NULL) {

            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))
            data <- as.data.frame(data)
            if(!all(sapply(data[columns], is.numeric))) stop("The provided columns of data should be numeric.")
            df <- summarize_data_(data, columns, FUNS, group_by)

            # Create component environment
            env <- new.env()
            env$df <- df
            env$group_by <- group_by
            env$FUNS <- FUNS
            env$footnote_title <- caption
            env$footnote_text <- description

            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            print(title)
            expanded_component <- knitr::knit_expand(file = system.file("templates", "summarize_metadata.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })


#' @rdname summarize_metadata
#' @export
setMethod("summarize_metadata",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, from = c("colData", "rowData"), columns = NULL, group_by = NULL, caption = "Table: ", description = NULL, ...) {

            from <- match.arg(from)
            data <- switch(from,
                           "colData" = SummarizedExperiment::colData(object),
                           "rowData" = SummarizedExperiment::rowData(object))

            if(is.null(columns)){
              data %>%
                as.data.frame %>%
                dplyr::select_if(is.numeric) -> cols
              columns <- colnames(cols)
            }
            assertive.sets::assert_is_subset(columns, colnames(data))

            if(!is.null(group_by)){
              assertive.sets::assert_is_subset(group_by, colnames(data))
              group_by <- switch(from,
                             "colData" = SummarizedExperiment::colData(object)[[group_by]],
                             "rowData" = SummarizedExperiment::rowData(object)[[group_by]])
            }
            if(is.null(description)){
              description <- switch(from,
                           "colData" = "Summarized values from samples.",
                           "rowData" = "Summarized values from features.")
            }

            summarize_metadata(
              dashboard = dashboard,
              data = data,
              columns = columns,
              group_by = group_by,
              footnote_title = footnote,
              footnote_text = description,
              ...
            )
          })

#' @rdname summarize_metadata
#' @export
setMethod("summarize_metadata",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, from = c("meta.data", "meta.features"), assay = "RNA", columns = NULL, group_by = NULL, caption = "Table", description = NULL, ...) {

            from <- match.arg(from)
            data <- switch(from,
                           "meta.data" = object@meta.data,
                           "meta.features" = object[[assay]]@meta.features)

            if(is.null(columns)){
              data %>%
                as.data.frame %>%
                dplyr::select_if(is.numeric) -> cols
              columns <- colnames(cols)
            }
            assertive.sets::assert_is_subset(columns, colnames(data))

            if(!is.null(group_by)){
              assertive.sets::assert_is_subset(group_by, colnames(data))
              group_by <- switch(from,
                                 "meta.data" = object@meta.data[[group_by]],
                                 "meta.features" = object[[assay]]@meta.features[[group_by]])
            }
            if(is.null(description)){
              description <- switch(from,
                                      "meta.data" = "Summarized values from samples.",
                                      "meta.features" = "Summarized values from features.")
            }

            summarize_metadata(
              dashboard = dashboard,
              data = data,
              columns = columns,
              group_by = group_by,
              footnote_title = footnote,
              footnote_text = description,
              ...
            )
          })


