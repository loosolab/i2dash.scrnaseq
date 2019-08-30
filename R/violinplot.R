#' Renders a component containing a vertical violin plot
#'
#' @param report An object of class \linkS4class{i2dash::i2dashboard}.
#' @param y A data.frame (matrix) containing numeric observations for the vertical axis, or a character vector indicating column names of \code{colData(object)}, \code{rowData(object)}.
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @param group_by An optional data.frame (matrix) with columns containing grouping factors for the horizontal axis.
#' @param use A character specifying where to obtain the data from. One of \code{"colData"} or \code{"rowData"}.
#' @param title The title of the component.
#' @param y_title The title of the y-axis.
#' @param group_by_title The title of the x-axis.
#'
#' @name violinplot
#' @rdname violinplot
#' @exportMethod violinplot
setGeneric("violinplot", function(report, object, ...) standardGeneric("violinplot"))

#' @rdname violinplot
#' @return A string containing markdown code for the rendered component
setMethod("violinplot",
          signature = signature(report = "i2dashboard", object = "missing"),
          function(report, y, group_by = NULL,  title = NULL, y_title = NULL, group_by_title = NULL) {
            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            assertive.types::assert_is_any_of(y, c("data.frame", "matrix"))
            if(is.null(colnames(y))) colnames(y) <- paste0("V", 1:ncol(y))

            if(!is.null(group_by)){
              assertive.types::assert_is_any_of(group_by, c("data.frame", "matrix"))
              if(is.null(colnames(group_by))) colnames(group_by) <- paste0("V", 1:ncol(group_by))
              if(nrow(y) != nrow(group_by)) stop("The numbers of rows in 'y' and 'group_by' are not equal.")
            }

            # Create component environment
            env <- new.env()

            env$y <- y
            env$y_selection <- length(y) > 1

            env$group_by <- group_by
            env$group_by_selection <- length(group_by) > 1

            env$y_title <- y_title
            env$group_by_title <- group_by_title

            # save environment report
            saveRDS(env, file = file.path(report@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "violinplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })

#' @rdname violinplot
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("violinplot",
          signature = signature(report = "i2dashboard", object = "SingleCellExperiment"),
          function(report, object, use = "colData", y = NULL, group_by = NULL,  title = NULL, y_title = NULL, group_by_title = NULL) {

            if(use == "colData") {
              if(!is.null(y)) {
                assertive.sets::assert_is_subset(y, colnames(SummarizedExperiment::colData(object)))
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!y) -> y
              } else {
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() -> y
              }
              if(!is.null(group_by)) {
                assertive.sets::assert_is_subset(group_by, colnames(SummarizedExperiment::colData(object)))
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!group_by) -> group_by
              }
            } else if (use == "rowData") {
              if(!is.null(y)) {
                assertive.sets::assert_is_subset(y, colnames(SummarizedExperiment::rowData(object)))
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!y) -> y
              } else {
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() -> y
              }
              if(!is.null(group_by)) {
                assertive.sets::assert_is_subset(group_by, colnames(SummarizedExperiment::rowData(object)))
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!group_by) -> group_by
              }
            }
            violinplot(report,
                       y = y,
                       group_by = group_by,
                       title = title,
                       y_title = y_title,
                       group_by_title = group_by_title)
          })

