#' Renders a component containing a vertical violin plot
#'
#' @name violinplot
#' @rdname violinplot
#' @exportMethod violinplot
setGeneric("violinplot", function(object, sc_object, ...) standardGeneric("violinplot"))

#'
#' @rdname violinplot
#' @param object An object of class \linkS4class{i2dash::i2dashboard}.
#' @param y A data.frame or matrix with columns containing numeric observations for the vertical violinplot (y-axis).
#' @param group_by (Optional) A data.frame or matrix with columns conatining factors for grouping the observations.
#' @param title (Optional) The title of the components junk.
#'
#' @return A string containing markdown code for the rendered component
setMethod("violinplot",
          signature = signature(object = "i2dashboard", sc_object = "missing"),
          function(object, y, group_by = NULL,  title = NULL) {
            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            assertive.types::assert_is_any_of(y, c("data.frame", "matrix"))
            if(is.null(colnames(y))) stop("'y' does not have colnames.")
            if(!is.null(group_by)){
              assertive.types::assert_is_any_of(group_by, c("data.frame", "matrix"))
              if(is.null(colnames(group_by))) stop("'group_by' does not have colnames.")
              if(nrow(y) != nrow(group_by)) stop("The numbers of rows in 'y' and 'group_by' are not equal.")
            }

            # Create component environment
            env <- new.env()

            env$y <- y
            env$y_selection <- length(y) > 1

            env$group_by <- group_by
            env$group_by_selection <- length(group_by) > 1

            # save environment object
            saveRDS(env, file = file.path(object@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "violinplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })

#' @rdname violinplot
#'
#' @param sc_object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @param use A character specifying where to obtain the data from. Valid input: "colData" or "rowData".
#' @param y A character vector with column names to use for observations.
#' @param group_by A character vector with column names to use for grouping the observations.
#'
#' @inheritParams violinplot,i2dashboard,missing-method
#'
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("violinplot",
          signature = signature(object = "i2dashboard", sc_object = "SingleCellExperiment"),
          function(object, sc_object, use = "colData", y = NULL, group_by = NULL,  title = NULL) {

            if(use == "colData"){
              #
              # definte y data.frame
              #
              if(!is.null(y)){
                assertive.sets::assert_is_subset(y, colnames(SummarizedExperiment::colData(sc_object)))
                SummarizedExperiment::colData(sc_object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!y) -> y
              } else {
                SummarizedExperiment::colData(sc_object) %>%
                  as.data.frame() -> y
              }
              #
              # definte group_by data.frame
              #
              if(!is.null(group_by)){
                assertive.sets::assert_is_subset(group_by, colnames(SummarizedExperiment::colData(sc_object)))
                SummarizedExperiment::colData(sc_object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!group_by) -> group_by
              }
            } else if (use == "rowData"){
              #
              # definte y data.frame
              #
              if(!is.null(y)){
                assertive.sets::assert_is_subset(y, colnames(SummarizedExperiment::rowData(sc_object)))
                SummarizedExperiment::rowData(sc_object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!y) -> y
              } else {
                SummarizedExperiment::rowData(sc_object) %>%
                  as.data.frame() -> y
              }
              #
              # definte group_by data.frame
              #
              if(!is.null(group_by)){
                assertive.sets::assert_is_subset(group_by, colnames(SummarizedExperiment::rowData(sc_object)))
                SummarizedExperiment::rowData(sc_object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!group_by) -> group_by
              }
            }
            violinplot(object,
                       y = y,
                       group_by = group_by,
                       title = title)
          })

