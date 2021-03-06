#' @rdname scatterplot
#' @return A string containing markdown code for the rendered component
setMethod("scatterplot",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, x, y, colour_by = NULL, labels = NULL, exprs_values = NULL, title = NULL, x_title = NULL, y_title = NULL, plot_title = NULL, source = "A", transmitter = NULL) {
            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            # create valid names
            assertive.types::assert_is_character(source)
            source %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names -> source
            if(!is.null(transmitter)){
              assertive.types::assert_is_character(transmitter)
              transmitter %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names -> transmitter
            }

            # handle single numeric vector:
            if(is.numeric(x)) x <- data.frame("X" <- x)
            if(is.numeric(y)) y <- data.frame("Y" <- y)
            if(is.character(colour_by) | is.numeric(colour_by) | is.factor(colour_by)) colour_by <- data.frame("Colour_by" <- colour_by)

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
            if(nrow(x) != nrow(y)) stop("The number of rows in 'x' and 'y' should be equal.")

            # Columns are swapped in case of equal column names to prevent visualization of the same column (always the first one) on both axes.
            if(colnames(x)[1] == colnames(y)[1] & colnames(x)[2] == colnames(y)[2]) {
              if(ncol(y) > 2) {
                y <-  y[, c(2, 1, c(3:ncol(y)))]
              } else {
                y <-  y[, c(2, 1)]
              }
            }

            colouring <- list("No colour" = 0)

            if(!is.null(colour_by)){
              assertive.types::assert_is_any_of(colour_by, c("data.frame", "matrix"))
              colour_by %<>%
                as.data.frame() %>%
                dplyr::mutate_if(is.character, as.factor) %>%
                dplyr::mutate_if(is.logical, as.factor) %>%
                dplyr::select_if(function(col) is.integer(col) | is.numeric(col) | is.factor(col))

              if(is.null(colnames(colour_by))) colnames(colour_by) <- paste0("Colour_by_", 1:ncol(colour_by))
              if(nrow(x) != nrow(colour_by)) stop("The number of rows in 'x' and 'colour_by' is not equal.")
              colouring["Colour by metadata"] <- 1
            }

            if(!is.null(labels)){
              assertive.types::assert_is_any_of(labels, c("character", "numeric", "integer"))
              if(nrow(x) != length(labels)) stop("The number of rows in 'x' and the length of 'labels' is not equal.")
              colouring["Colour by label"] <- 2
            }

            if(!is.null(exprs_values)){
              exprs_values <- as.matrix(exprs_values)
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
            env$plot_title <- plot_title

            env$source <- source # the id used in plotly's source argument of the component
            env$transmitter <- transmitter # the id of an existing transmitter to obtain the data from

            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "scatterplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })


#' @rdname scatterplot
#' @export
setMethod("scatterplot",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, from = c("colData", "rowData", "reducedDim"), x = NULL, y = NULL, colour_by = NULL, use_dimred = NULL, exprs_values = NULL, subset_row = NULL, ...) {

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

            # data.frame from Assay for colouring by expression
            if(from != "rowData"){
              if(!is.null(exprs_values)){
                assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))
                exprs_values <- SummarizedExperiment::assay(object, i = exprs_values)
                if(!is.null(subset_row)) {
                  exprs_values <- exprs_values[subset_row, ]
                }
              }
            }

            scatterplot(dashboard,
                        x = x,
                        y = y,
                        labels = labels,
                        colour_by = colour_by,
                        exprs_values = exprs_values,
                        ...)
          })

#' @rdname scatterplot
#' @export
setMethod("scatterplot",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, from = c("meta.data", "meta.features", "embedding"), x = NULL, y = NULL, colour_by = NULL, use_dimred = NULL, assay = "RNA", slot = NULL, subset_row = NULL, ...) {

            from <- match.arg(from)

            # Prevent misuse of embedding mode
            if(from == "embedding") {
              if(is.null(use_dimred)) stop("reduction cannot be NULL when using data from embedding")
              x <- NULL
              y <- NULL
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

            # data.frame from Assay for colouring by expression
            if(from != "rowData"){
              if(!is.null(slot)){
                assertive.sets::assert_is_subset(assay, names(object@assays))
                assay_obj <- Seurat::GetAssay(object = object, assay = assay)
                exprs_values <- Seurat::GetAssayData(object = assay_obj, slot = slot)
                if(!is.null(subset_row)) {
                  exprs_values <- exprs_values[subset_row, ]
                }
              }
            }

            scatterplot(dashboard,
                        x = x,
                        y = y,
                        labels = labels,
                        colour_by = colour_by,
                        exprs_values = exprs_values,
                        ...)
          })
