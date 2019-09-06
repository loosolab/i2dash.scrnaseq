#' Renders a component containing a scatterplot with optional selection options
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param x A data.frame (matrix) containing columns with numeric values that will be mapped to the x-axis.
#' @param y A data.frame (matrix) containing columns with numeric values that will be mapped to the y-axis.
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @param use A character specifying where to obtain the data from. One of \code{"colData"}, \code{"rowData"}, \code{"reducedDim"}.
#' @param reducedDim A character vector indicating the reduced dimension to use from \code{"reducedDim"}
#' @param colour_by An optional data.frame (matrix) containing columns with numeric or factorial values that will be used for colouring.
#' @param labels An optional vector with sample names. A dropdown menu for colouring by label will be provided.
#' @param exprs_values An optional data.frame (matrix) containing expression data of features of interest in rows and samples in columns.
#' @param title The title of the components junk.
#' @param x_title An optional title of the x-axis. If not provided the column names from \code{x} are used instead.
#' @param y_title An optional title of the y-axis. If not provided the column names from \code{y}  are used instead.
#'
#' @name scatterplot
#' @rdname scatterplot
#' @exportMethod scatterplot
setGeneric("scatterplot", function(dashboard, object, ...) standardGeneric("scatterplot"))

#' @rdname scatterplot
#' @return A string containing markdown code for the rendered component
setMethod("scatterplot",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, x, y, colour_by = NULL, labels = NULL, exprs_values = NULL, title = NULL, x_title = NULL, y_title = NULL) {
            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            assertive.types::assert_is_any_of(x, c("data.frame", "matrix"))
            assertive.types::assert_is_any_of(y, c("data.frame", "matrix"))
            x %<>%
              as.data.frame() %>%
              dplyr::select_if(function(col) is.integer(col) | is.numeric(col))
            y %<>%
              as.data.frame() %>%
              dplyr::select_if(function(col) is.integer(col) | is.numeric(col))

            if(is.null(colnames(x))) colnames(x) <- paste0("X_", 1:ncol(x))
            if(is.null(colnames(y))) colnames(y) <- paste0("Y_", 1:ncol(y))
            if(nrow(x) != nrow(y)) stop("The number of rows in 'x' and 'y' are is equal.")

            colouring <- list("No colour" = 0)

            if(!is.null(colour_by)){
              assertive.types::assert_is_any_of(colour_by, c("data.frame", "matrix"))
              colour_by %<>%
                as.data.frame() %>%
                dplyr::select_if(function(col) is.integer(col) | is.numeric(col) | is.factor(col))
              if(is.null(colnames(colour_by))) colnames(colour_by) <- paste0("Color_by_", 1:ncol(colour_by))
              if(nrow(x) != nrow(colour_by)) stop("The number of rows in 'x' and 'colour_by' is not equal.")
              colouring["Colour by metadata"] <- 1
            }

            if(!is.null(labels)){
              assertive.types::assert_is_any_of(labels, c("character", "numeric", "integer"))
              if(nrow(x) != length(labels)) stop("The number of rows in 'x' and the length of 'labels' is not equal.")
              colouring["Colour by label"] <- 2
            }

            if(!is.null(exprs_values)){
              assertive.types::assert_is_any_of(exprs_values, c("data.frame", "matrix"))
              if(is.null(rownames(exprs_values))) rownames(exprs_values) <- paste0("feature_", 1:nrow(exprs_values))
              if(nrow(x) != ncol(exprs_values)) stop("The number of rows in 'x' and columns in 'exprs_values' is not equal.")
              colouring["Colour by expression"] <- 3
            }

            # Create component environment
            env <- new.env()

            env$x <- x
            env$x_selection <- length(x) > 1

            env$y <- y
            env$y_selection <- length(y) > 1

            env$colour_by <- colour_by
            env$colour_by_selection <- length(colour_by) > 1

            env$labels <- labels
            env$exprs_values <- exprs_values
            env$colouring <- colouring
            env$x_title <- x_title
            env$y_title <- y_title

            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "scatterplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })


#' @rdname scatterplot
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("scatterplot",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, use = "colData", x = NULL, y = NULL, colour_by = NULL, reduced_dim = NULL, ...) {
            #
            # use colData
            #
            if(use == "colData") {
              labels <- rownames(SummarizedExperiment::colData(object))
              #
              # create data.frame for y
              #
              if(!is.null(y)) {
                assertive.sets::assert_is_subset(y, colnames(SummarizedExperiment::colData(object)))
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!y) -> y
              } else {
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() -> y
              }
              #
              # create data.frame for x
              #
              if(!is.null(x)) {
                assertive.sets::assert_is_subset(x, colnames(SummarizedExperiment::colData(object)))
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!x) -> x
              } else {
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() -> x
              }
              #
              # create data.frame for colour_by
              #
              if(!is.null(colour_by)) {
                assertive.sets::assert_is_subset(colour_by, colnames(SummarizedExperiment::colData(object)))
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!colour_by) -> colour_by
              } else {
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() -> colour_by
              }
            #
            # use rowData
            #
            } else if (use == "rowData") {
              labels <- rownames(SummarizedExperiment::rowData(object))
              #
              # create data.frame for y
              #
              if(!is.null(y)) {
                assertive.sets::assert_is_subset(y, colnames(SummarizedExperiment::rowData(object)))
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!y) -> y
              } else {
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() -> y
              }
              #
              # create data.frame for x
              #
              if(!is.null(x)) {
                assertive.sets::assert_is_subset(x, colnames(SummarizedExperiment::rowData(object)))
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!x) -> x
              } else {
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() -> x
              }
              #
              # create data.frame for colour_by
              #
              if(!is.null(colour_by)) {
                assertive.sets::assert_is_subset(colour_by, colnames(SummarizedExperiment::rowData(object)))
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!colour_by) -> colour_by
              } else {
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() -> colour_by
              }
            #
            # use reducedDim
            #
            } else if (use == "reducedDim"){
              labels <- rownames(SingleCellExperiment::reducedDim(object))
              #
              # create data.frames for x, y
              #
              # To Do: in statical mode the first column is used for x and y. This is useless.
              if(!is.null(reduced_dim)) {
                assertive.sets::assert_is_subset(reduced_dim, SingleCellExperiment::reducedDimNames(object))
                SingleCellExperiment::reducedDim(object, reduced_dim) %>%
                  as.data.frame() -> x -> y
              } else {
                SingleCellExperiment::reducedDim(object) %>%
                  as.data.frame() -> x -> y
              }
              #
              # create data.frame for colour_by
              #
              if(!is.null(colour_by)) {
                assertive.sets::assert_is_subset(colour_by, colnames(SummarizedExperiment::colData(object)))
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!colour_by) -> colour_by
              } else {
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() -> colour_by
              }
            }

            scatterplot(dashboard,
                        x = x,
                        y = y,
                        labels = labels,
                        colour_by = colour_by,
                        ...)
          })


SingleCellExperiment::colData(sce) %>%
  as.data.frame() %>%
  dplyr::select(1)
