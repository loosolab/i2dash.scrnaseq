#' Summarizes columns of \code{data} by \code{discarded} and is grouped by \code{columns}.
#'
#' @param data A data.frame.
#' @param columns The columns of \code{data} containing experiemntal factors.
#' @param discarded A column of \code{data} contatining logical values.
#' @param colnames A vector of length three containing the column names of the final table.
#' @param table_title Title of the table.
#' @param footnote_title Title of the footnote.
#' @param footnote_text Description for the table shown as footnote.
#'
#' @return A list with a \code{data.frame} and a HTML table that contains the number of kept and disarded samples per column.
.summarize_discarded <- function(data, discarded, columns, colnames, table_title, footnote_title, footnote_text){
  library(kableExtra)
  library(dplyr)
  library(tidyr)
  table <- data.frame(condition = "total", "FALSE" = as.numeric(summary(data[[discarded]])[2]), "TRUE" = as.numeric(summary(data[[discarded]])[3]))
  colnames(table) <- c("condition", "FALSE", "TRUE") # column names are changed for further use
  rows <- c(1) # a vector to save the rownumber (row per factorial level) of each condition.

  # Calculate the number of kept and discarded cells per condition and bind it to the DataFrame
  for (column in columns){
    data %>%
      as.data.frame() %>%
      group_by(!!! rlang::syms(discarded), !!! rlang::syms(column)) %>%
      summarise(n_samples = dplyr::n()) %>%
      pivot_wider(names_from = discarded, values_from = n_samples) %>%
      replace(., is.na(.), 0) %>%
      dplyr::rename("condition" = 1) %>%
      as.data.frame() -> stat
    stat$condition <- as.factor(stat$condition)
    table <- rbind(table, stat)
    rows <- c(rows, nrow(stat))
  }
  # Change column names
  colnames(table) <- colnames
  end_rows <- cumsum(rows) # get the rownumber indicating the end of a condition
  # Create html table with the package kableExtra
  kable(table, caption = table_title) %>%
    kable_styling(bootstrap_options = c("striped", "hover")) %>%
    kableExtra::footnote(general = footnote_text,
                         general_title = footnote_title,
                         footnote_as_chunk = T,
                         title_format = c("bold", "underline")) -> kable

  # Create groupings in the table by conditions
  for (index in 1:length(columns)){
    kable %<>%
      pack_rows(columns[index], end_rows[index]+1, end_rows[index+1])
  }
  return(list("kable" = kable, "df" = table))
}


#' @rdname summarize_discarded
#' @return A string containing markdown code for the rendered component
setMethod("summarize_discarded",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, data, columns, discarded, footnote_title = NULL, footnote_text = NULL, table_title = "Number of kept / discarded cells per condition.", colnames = c("condition", "kept", "discarded"), title = NULL) {

            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))
            data <- as.data.frame(data)
            if(!all(sapply(data[columns], is.factor))) stop("The provided columns of data should contain factors.")
            if(length(colnames)!=3) stop("The vector colnames should be of length 3.")
            if(!is.logical(data[[discarded]])) stop("The column provided by discarded should contain logical values.")
            outlist <- .summarize_discarded(data, discarded, columns, colnames, table_title, footnote_title, footnote_text)

            # Create component environment
            env <- new.env()
            env$df <- outlist$df
            env$kable <- outlist$kable

            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "summarize_discarded.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })


#' @rdname summarize_discarded
#' @export
setMethod("summarize_discarded",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, discarded, from = c("colData", "rowData"), columns = NULL, ...) {

            from <- match.arg(from)
            data <- switch(from,
                           "colData" = SummarizedExperiment::colData(object),
                           "rowData" = SummarizedExperiment::rowData(object))

            if(is.null(columns)){
              data %>%
                as.data.frame() %>%
                dplyr::select_if(is.factor) -> cols
              columns <- colnames(cols)
            }
            assertive.sets::assert_is_subset(columns, colnames(data))
            assertive.sets::assert_is_subset(discarded, colnames(data))


            summarize_discarded(
              dashboard = dashboard,
              data = data,
              columns = columns,
              discarded = discarded,
              ...
            )
          })

#' @rdname summarize_discarded
#' @export
setMethod("summarize_discarded",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, discarded, from = c("colData", "rowData"), columns = NULL, assay = "RNA", ...) {

            from <- match.arg(from)
            data <- switch(from,
                           "meta.data" = object@meta.data,
                           "meta.features" = object[[assay]]@meta.features)

            if(is.null(columns)){
              data %>%
                as.data.frame() %>%
                dplyr::select_if(is.factor) -> cols
              columns <- colnames(cols)
            }
            assertive.sets::assert_is_subset(columns, colnames(data))
            assertive.sets::assert_is_subset(discarded, colnames(data))


            summarize_discarded(
              dashboard = dashboard,
              data = data,
              columns = columns,
              discarded = discarded,
              ...
            )
          })


