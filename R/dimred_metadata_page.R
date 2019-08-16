#' Add a dimension reduction with metadata page
#'
#' This function renders adds a page with two linked components to the \code{report} object: The first component is a scatterplot, showing samples in along two-dimensional coordinates from \code{use_dimred}. The second component is a table, that shows metadata from \code{metadata}. The scatterplot is colored by feature expression values provided in \code{expression} for the selected feature in the table.
#'
#' @name add_dimred_metadata_page
#' @rdname add_dimred_metadata_page
#' @exportMethod add_dimred_metadata_page
setGeneric("add_dimred_metadata_page", function(report, object, ...) standardGeneric("add_dimred_metadata_page"))

#' @rdname add_dimred_metadata_page
#' @param report An object of class \linkS4class{i2dash::i2dashboard}.
#' @param use_dimred A data.frame or matrix containing coordinates of the reduced dimensions.
#' @param exprs_values A data.frame or matrix containing expression data of features of interest (features in rows and samples in columns). Rownames are used as feature names.
#' @param metadata A data.frame or matrix along rows of \code{exprs_values} containing metadata data for the features of interest.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_dimred_metadata_page",
          signature = signature(report = "i2dashboard", object = "missing"),
          function(report, use_dimred, exprs_values, metadata, title = "Marker gene expression", menu = NULL) {

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Input validation
            assertive.types::assert_is_any_of(use_dimred, c("data.frame", "matrix"))
            assertive.types::assert_is_any_of(exprs_values, c("data.frame", "matrix"))
            assertive.types::assert_is_any_of(metadata, c("data.frame", "matrix"))

            if(ncol(use_dimred) < 2 ) stop("'use_dimred' should contain at least two columns.")

            if(is.null(colnames(metadata))) stop("'metadata' should contain colnames.")

            # Create component environment
            env <- new.env()

            env$reduced_dim <- data.frame("x" = use_dimred[,1], "y" = use_dimred[,2])
            env$expression <- round(exprs_values,3)
            env$metadata <- metadata

            # save environment object
            saveRDS(env, file = file.path(report@datadir, paste0(env_id, ".rds")))

            expanded_components <- list()
            timestamp <- Sys.time()

            # fill list "expanded_components" with components
            scatterplot_component <- knitr::knit_expand(file = system.file("templates", "dimred_metadata_dimred.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
            expanded_components <- append(expanded_components, scatterplot_component)

            table_component<- knitr::knit_expand(file = system.file("templates", "dimred_metadata_table.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
            expanded_components <- append(expanded_components, table_component)

            report@pages[["dimred_metadata_page"]] <- list(title = title, layout = "2x2_grid", menu = menu, components = expanded_components, max_components = 2)
            return(report)
          })

#' @rdname add_dimred_metadata_page
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @inheritParams add_dimred_metadata_page,i2dashboard,missing-method
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_dimred_metadata_page",
          signature = signature(report = "i2dashboard", object = "SingleCellExperiment"),
          function(report, object, use_dimred, exprs_values, metadata, genes, title = "Marker gene expression", menu = NULL) {

            # validate and extract metadata
            if(!is.character(metadata) & !is.list(metadata)) stop("'metadata' should be a character or a list.")
            if(!all(metadata %in% names(SummarizedExperiment::rowData(object)))) stop("'rowData' slot of the SingleCellExperiment object does not contain the column names from 'metadata'.")
            metadata <- as.data.frame(SummarizedExperiment::rowData(sce)[metadata])

            # validate input and extract dimension reduction
            if(!is.character(use_dimred) | length(use_dimred) > 1) stop("'use_dimred' should be a character of length 1.")
            if(!use_dimred %in% SingleCellExperiment::reducedDimNames(object)) stop("'reducedDim' slot of the SingleCellExperiment object does not contain the name provided in 'use_dimred'.")
            use_dimred <- SingleCellExperiment::reducedDim(object, use_dimred)

            # validate input and extract expression
            if(!is.character(exprs_values) | length(exprs_values) > 1) stop("'exprs_values' should be a character of length 1.")
            if(!exprs_values %in% SummarizedExperiment::assayNames(object)) stop("'assay' slot of the SingleCellExperiment object does not contain the name provided in 'expression'.")
            exprs_values <- SummarizedExperiment::assay(object, exprs_values)

            # validate genes of interest and subset expression and metadata

            if(!is.character(genes) & !is.list(genes)) stop("'genes' should be a character or a list.")
            if(!all(genes %in% rownames(exprs_values))){
              false_genes <- which(!genes %in% rownames(exprs_values))
              print("The row names do not contain the following genes:")
              print(genes[false_genes])
              stop("'genes' contains invalid names.")
            }
            exprs_values <- exprs_values[genes,]
            metadata <- metadata[genes,]

            report <- add_dimred_metadata_page(report = report,
                                               use_dimred = use_dimred,
                                               exprs_values = exprs_values,
                                               metadata = metadata,
                                               title = title,
                                               menu = menu)
            return(report)
          })

#' @rdname add_dimred_metadata_page
#' @param object A valid \linkS4class{Seurat::Seurat} object.
#' @param assay A character of length 1 representing the name of a \linkS4class{Seurat::Assay} object present in the slot \code{assays} of the \linkS4class{Seurat::Seurat} object.
#' @param assay_slot (Default value: "data") A character of length 1 representing the name of a slot in the \linkS4class{Seurat::Assay} object provided in \code{assay}.
#' @inheritParams add_dimred_metadata_page,i2dashboard,missing-method
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_dimred_metadata_page",
          signature = signature(report = "i2dashboard", object = "Seurat"),
          function(report, object, use_dimred, exprs_values, metadata, genes, assay, assay_slot = "data", title = "Marker gene expression", menu = NULL) {

            # validate and extract metadata
            if(!is.character(metadata) & !is.list(metadata)) stop("'metadata' should be a character or a list.")
            if(!all(metadata %in% names(object@meta.data))) stop("'meta.data' slot of the Seurat object does not contain the column names from 'metadata'.")
            metadata <- object@meta.data[metadata]

            # validate input and extract use_dimred
            if(!is.character(use_dimred) | length(use_dimred) > 1) stop("'use_dimred' should be a character of length 1.")
            if(!use_dimred %in% names(object@reductions)) stop("'reductions' slot of the Seurat object does not contain the name provided in 'use_dimred'.")
            use_dimred <- Seurat::Embeddings(object, reduction = use_dimred)[,1:2]

            # validate input and extract exprs_values
            if(!is.character(assay) | length(assay) > 1) stop("'assay' should be a character of length 1.")
            if(!is.character(assay_slot) | length(assay_slot) > 1) stop("'assay_slot' should be a character of length 1.")
            if(!assay %in% names(object@assays)) stop(paste0("The 'assay' slot of the Seurat object does not contain an Assay object with the name '", assay,"'."))
            assay_obj <- Seurat::GetAssay(object = object, assay = assay)
            if(!assay_slot %in% slotNames(assay_obj)) stop(paste0("A slot with the name '", assay_slot, "' is not present in the Assay object '", assay, "'of the Seurat object."))
            exprs_values <- Seurat::GetAssayData(object = assay_obj, slot = assay_slot)
            if(nrow(use_dimred) != ncol(exprs_values)) stop("The number of rows in 'use_dimred' should be equal to the number of columns of the 'assay_slot'")
            exprs_values <- as.matrix(exprs_values) # convert dgCMatrix to regular matrix

            # validate genes of interest and subset expression

            if(!is.character(genes) & !is.list(genes)) stop("'genes' should be a character or a list.")
            if(!all(genes %in% rownames(exprs_values))){
              false_genes <- which(!genes %in% rownames(exprs_values))
              print("The row names do not contain the following genes:")
              print(genes[false_genes])
              stop("'genes' contains invalid names.")
            }
            exprs_values <- exprs_values[genes,]
            metadata <- metadata[genes,]

            report <- add_dimred_metadata_page(report = report,
                                               use_dimred = use_dimred,
                                               exprs_values = exprs_values,
                                               metadata = metadata,
                                               title = title,
                                               menu = menu)
            return(report)
          })
