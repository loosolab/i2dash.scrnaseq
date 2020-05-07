#'
#' @rdname bubbleplot
#' @return A string containing markdown code for the rendered component
setMethod("bubbleplot",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, x, y, size, colour_by = NULL, labels = NULL, title = NULL, x_title = NULL, y_title = NULL, plot_title = NULL) {
            # handle single numeric vector:
            if(is.numeric(x)) x <- data.frame("X" <- x)
            if(is.numeric(y)) y <- data.frame("Y" <- y)
            if(is.character(colour_by) | is.numeric(colour_by) | is.factor(colour_by)) colour_by <- data.frame("Colour_by" <- colour_by)
            if(is.numeric(size) | is.factor(size)) size <- data.frame("size" <- as.numeric(size))

            # Validate input
            assertive.types::assert_is_any_of(x, c("data.frame", "matrix"))
            assertive.types::assert_is_any_of(y, c("data.frame", "matrix"))
            assertive.types::assert_is_any_of(size, c("data.frame", "matrix"))

            # select columns only containing numeric or integer values
            x %<>%
              as.data.frame() %>%
              dplyr::select_if(function(col) is.integer(col) | is.numeric(col))
            y %<>%
              as.data.frame() %>%
              dplyr::select_if(function(col) is.integer(col) | is.numeric(col))
            size %<>%
              as.data.frame() %>%
              dplyr::mutate_if(is.character, as.factor) %>%
              dplyr::mutate_if(is.logical, as.factor) %>%
              dplyr::mutate_if(is.factor, as.numeric) %>%
              dplyr::select_if(function(col) is.integer(col) | is.numeric(col))

            # provide column names
            if(is.null(colnames(x))) colnames(x) <- paste0("X_", 1:ncol(x))
            if(is.null(colnames(y))) colnames(y) <- paste0("Y_", 1:ncol(y))
            if(is.null(colnames(size))) colnames(size) <- paste0("Size_", 1:ncol(size))

            # check correct dimensions
            if(nrow(x) != nrow(y)) stop("The number of rows in 'x' and 'y' is not equal.")
            if(nrow(x) != nrow(size)) stop("The number of rows in 'x' and 'size' is not equal.")

            # check optional parameters
            if(!is.null(colour_by)){
              assertive.types::assert_is_any_of(colour_by, c("data.frame", "matrix"))
              colour_by %<>%
                as.data.frame() %>%
                dplyr::select_if(function(col) is.integer(col) | is.numeric(col) | is.factor(col))
              if(is.null(colnames(colour_by))) colnames(colour_by) <- paste0("Color_", 1:ncol(colour_by))
              if(nrow(x) != nrow(colour_by)) stop("The number of rows in 'x' and 'colour_by' is not equal.")
            }

            # Columns are swapped in case of equal column names to prevent visualization of the same column (always the first one) on both axes.
            if(colnames(x)[1] == colnames(y)[1] & colnames(x)[2] == colnames(y)[2]) {
              if(ncol(y) > 2) {
                y <-  y[, c(2, 1, c(3:ncol(y)))]
              } else {
                y <-  y[, c(2, 1)]
              }
            }

            if(!is.null(labels)) assertive.types::assert_is_any_of(labels, "vector")
            if(!is.null(title)) assertive.types::is_character(title)
            if(!is.null(x_title)) assertive.types::is_character(x_title)
            if(!is.null(y_title)) assertive.types::is_character(y_title)

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Create component environment
            env <- new.env()
            env$x <- x
            env$y <- y
            env$size <- size
            env$colour_by <- colour_by
            env$labels <- labels
            env$x_title <- x_title
            env$y_title <- y_title
            env$plot_title <- plot_title

            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "bubbleplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })


#' @rdname bubbleplot
#' @export
setMethod("bubbleplot",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, from = c("colData", "rowData", "reducedDim"), x = NULL, y = NULL, size = NULL, colour_by = NULL, use_dimred = NULL, ...) {

            from <- match.arg(from)

            # Prevent misuse of reducedDim mode
            if(from == "reducedDim") {
              if(is.null(use_dimred)) stop("use_dimred cannot be NULL when using data from reducedDim.")
              x <- NULL
              y <- NULL
              colour_by <- NULL
            }

            data <- switch(from,
                           "colData" = SummarizedExperiment::colData(object),
                           "rowData" = SummarizedExperiment::rowData(object),
                           "reducedDim" = SingleCellExperiment::reducedDim(object, use_dimred))

            labels <- rownames(data)

            # create data.frame for y
            if(!is.null(y)) {
              assertive.sets::assert_is_subset(y, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!y) -> y
            } else {
              data %>%
                as.data.frame() -> y
            }

            # create data.frame for x
            if(!is.null(x)) {
              assertive.sets::assert_is_subset(x, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!x) -> x
            } else {
              data %>%
                as.data.frame() -> x
            }

            # create data.frame for size
            if(!is.null(size)) {
              assertive.sets::assert_is_subset(size, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!size) -> size
            } else {
              data %>%
                as.data.frame() -> size
            }

            # create data.frame for colour_by
            if(!is.null(colour_by)) {
              assertive.sets::assert_is_subset(colour_by, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!colour_by) -> colour_by
            } else {
              data %>%
                as.data.frame() -> colour_by
            }

            bubbleplot(
              dashboard,
              x = x,
              y = y,
              size = size,
              labels = labels,
              colour_by = colour_by,
              ...
            )
          })

#' @rdname bubbleplot
#' @export
setMethod("bubbleplot",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, from = c("meta.data", "meta.features", "embedding"), x = NULL, y = NULL, size = NULL, colour_by = NULL, use_dimred = NULL, assay = "RNA", slot = NULL, subset_row = NULL, ...) {

            from <- match.arg(from)

            # Prevent misuse of embedding mode
            if(from == "embedding") {
              if(is.null(use_dimred)) stop("reduction cannot be NULL when using data from embedding")
              x <- NULL
              y <- NULL
              Size <- NULL
              colour_by <- NULL
            }

            data <- switch(from,
                           "meta.data" = object@meta.data,
                           "meta.features" = object[[assay]]@meta.features,
                           "embedding" = Seurat::Embeddings(object, reduction = use_dimred))

            labels <- rownames(data)

            # create data.frame for y
            if(!is.null(y)) {
              assertive.sets::assert_is_subset(y, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!y) -> y
            } else {
              data %>%
                as.data.frame() -> y
            }

            # create data.frame for x
            if(!is.null(x)) {
              assertive.sets::assert_is_subset(x, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!x) -> x
            } else {
              data %>%
                as.data.frame() -> x
            }

            # create data.frame for size
            if(!is.null(size)) {
              assertive.sets::assert_is_subset(size, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!size) -> size
            } else {
              data %>%
                as.data.frame() -> size
            }

            # create data.frame for colour_by
            if(!is.null(colour_by)) {
              assertive.sets::assert_is_subset(colour_by, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!colour_by) -> colour_by
            } else {
              data %>%
                as.data.frame() -> colour_by
            }

            bubbleplot(
              dashboard,
              x = x,
              y = y,
              size = size,
              labels = labels,
              colour_by = colour_by,
              ...
            )
          })
