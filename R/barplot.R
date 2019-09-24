#' Renders a component containing a horizontal barplot.
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param y_group_by A data.frame (matrix) with columns containing grouping factors for the vertical axis.
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @param x_group_by Optionally provide a data.frame (matrix) with columns containing grouping factors for the horizontal axis. The result is a barplot grouped by the levels in \code{x_group_by} and shows the relative number of its observations.
#' @param use A character specifying where to obtain the data from. One of \code{"colData"} or \code{"rowData"}.
#' @param title The title of the components junk.
#' @param y_group_by_title The title of the y-axis.
#' @param x_group_by_title The title of the x-axis.
#'
#' @name barplot
#' @rdname barplot
#' @exportMethod barplot
setGeneric("barplot", function(dashboard, object, ...) standardGeneric("barplot"))

#' @rdname barplot
#' @return A string containing markdown code for the rendered component
setMethod("barplot",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, y_group_by, x_group_by = NULL, title = NULL, x_group_by_title = NULL, y_group_by_title = NULL) {
            # Create random env i
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            assertive.types::assert_is_any_of(y_group_by, c("data.frame", "matrix"))
            if(is.null(colnames(y_group_by))) colnames(y_group_by) <- paste0("V", 1:ncol(y_group_by))

            if(!is.null(x_group_by)){
              assertive.types::assert_is_any_of(x_group_by, c("data.frame", "matrix"))
              if(is.null(colnames(x_group_by))) colnames(x_group_by) <- paste0("V", 1:ncol(x_group_by))
              if(nrow(y_group_by) != nrow(x_group_by)) stop("The numbers of rows in 'x_group_by' and 'y_group_by' are not equal.")
            }

            # Create component environment
            env <- new.env()

            env$y_group_by <- y_group_by
            env$y_group_by_selection <- length(y_group_by) > 1

            env$x_group_by <- x_group_by
            env$x_group_by_selection <- length(x_group_by) > 1

            env$x_group_by_title <- x_group_by_title
            env$y_group_by_title <- y_group_by_title

            # save environment report
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "barplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })


#' @rdname barplot
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("barplot",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, use = "colData", y_group_by = NULL, x_group_by = NULL, ...) {
            if(use == "colData") {
              if(!is.null(y_group_by)) {
                assertive.sets::assert_is_subset(y_group_by, colnames(SummarizedExperiment::colData(object)))
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!y_group_by) -> y_group_by
              } else {
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() -> y_group_by
              }
              if(!is.null(x_group_by)) {
                assertive.sets::assert_is_subset(x_group_by, colnames(SummarizedExperiment::colData(object)))
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!x_group_by) -> x_group_by
              }
            } else if (use == "rowData") {
              if(!is.null(y_group_by)) {
                assertive.sets::assert_is_subset(y_group_by, colnames(SummarizedExperiment::rowData(object)))
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!y_group_by) -> y_group_by
              } else {
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() -> y_group_by
              }
              if(!is.null(x_group_by)) {
                assertive.sets::assert_is_subset(x_group_by, colnames(SummarizedExperiment::rowData(object)))
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!x_group_by) -> x_group_by
              }
            }
            barplot(dashboard,
                    y_group_by = y_group_by,
                    x_group_by = x_group_by,
                    ...)
          })


