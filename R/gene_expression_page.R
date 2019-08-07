setGeneric("add_gene_expression_page", function(object, sc_object, ...) standardGeneric("add_gene_expression_page"))


#' Renders a page with two linked components. The first component is a scatterplot, showing samples in along coordinates from \code{reduced_dim}. The second component is a violin plot, that shows expression values from \code{expression} by groups defined in \code{grouping}.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param reduced_dim A data.frame or matrix containing coordinates of the reduced dimensions. Rownames are used as labels.
#' @param expression A data.frame or matrix containing expression data of features of interest in rows and samples in columns.
#' @param metadata A data.frame with sample metadata or a factor. Should have the same number of rows/length as \code{reduced_dim}.
#' @param grouping A character of length 1 identical to one of the column names in \code{metadata} used for expression grouping.
#' @param title The title of the page.
#' @param labels (Optional) A vector with labels. Should be of the same length as the rownumber of 'reduced_dim'.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#' @param sidebar (Optional) The page layout (see below).
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_gene_expression_page",
          signature = signature(object = "i2dashboard", sc_object = "missing"),
          function(object, reduced_dim, expression, metadata, grouping, title = NULL, labels = NULL, menu = NULL, sidebar = NULL) {

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            if(!is.data.frame(reduced_dim) & !is.matrix(reduced_dim)) stop("'reduced_dim' should should be of class 'data.frame' or 'matrix'.")
            if(ncol(reduced_dim) < 2 ) stop("'reduced_dim' should contain at least two columns.")
            if(!is.data.frame(expression) & !is.matrix(expression)) stop("'expression' should be of class 'data.frame' or 'matrix'.")
            if(!is.data.frame(metadata) & !is.matrix(metadata) & !is.factor(metadata)) stop("'metadata' should be of class 'data.frame' or 'matrix' or a factor.")
            if(!is.factor(metadata)){
              if(is.null(colnames(metadata))) stop("'metadata' should contain colnames.")
              if(nrow(metadata) != nrow(reduced_dim)) stop("'metadata' and 'reduced_dim' should contain the same number of rows.")
              if(!grouping %in% colnames(metadata)) stop("'metadata' does not contain a column with this name.")
            } else {
              if(length(metadata) != nrow(reduced_dim)) stop("'metadata' should have the same length as the number of rows of 'reduced_dim'.")
            }
            if(!is.null(title) & !is.character(title)) stop("'title' should be a character vector.")
            if(!is.null(labels) & length(labels) != nrow(reduced_dim)) stop("The length of the vector 'labels' should be equal to the number of rows in 'reduced_dim'.")

            # Create component environment
            env <- new.env()

            env$reduced_dim <- reduced_dim[, 1:2]
            env$expression <- expression
            env$metadata <- metadata
            env$grouping <- grouping
            env$labels <- labels
            if(!is.factor(metadata)) env$multiple_meta <- ncol(metadata) > 1 else env$multiple_meta <- FALSE

            # save environment object
            saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

            expanded_components <- list()
            timestamp <- Sys.time()

            # fill list "expanded_components" with components
            scatterplot_component <- knitr::knit_expand(file = system.file("templates", "gene_expression_dimred.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
            expanded_components <- append(expanded_components, scatterplot_component)

            violinplot_component<- knitr::knit_expand(file = system.file("templates", "gene_expression_violin_table.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
            expanded_components <- append(expanded_components, violinplot_component)

            # Expand component
            timestamp <- Sys.time()

            object@pages[["gene_expression_page"]] <- list(title = title, layout = "2x2_grid", menu = menu, components = expanded_components, max_components = 2, sidebar = sidebar)
            return(object)
          })

#' Renders a page with two linked components. The first component is a scatterplot, showing samples in along coordinates from \code{reduced_dim}. The second component is a violin plot, that shows expression values from \code{expression} by groups defined in \code{grouping}.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param sc_object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object
#' @param reduced_dim A character of length 1 representing the name of a dimension reduction of \code{reducedDim}.
#' @param expression A character of length 1 representing the name of an \code{assay}.
#' @param metadata A character or list representing the names of  columns of \code{colData}.
#' @param grouping A character of length 1 representing the name of columns of \code{colData} and used for expression grouping.
#' @param genes (Optional) A character or list of genes of interest that are present in row names.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#' @param sidebar (Optional) The page layout (see below).
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_gene_expression_page",
          signature = signature(object = "i2dashboard", sc_object = "SingleCellExperiment"),
          function(object, sc_object, reduced_dim, expression, metadata, grouping, genes = NULL, title = NULL, menu = NULL, sidebar = NULL) {

            # validate and extract metadata
            if(!is.character(metadata) & !is.list(metadata)) stop("'metadata' should be a character or a list.")
            if(!all(metadata %in% names(colData(sc_object)))) stop("'colData' slot of the SingleCellExperiment object does not contain the column names from 'metadata'.")
            metadata <- as.data.frame(sc_object@colData[metadata])
            # metadata <- sapply(metadata, extract_colData, x = sc_object)
            # metadata <- as.data.frame(metadata)

            # validate input and extract dimension reduction
            if(!is.character(reduced_dim) | length(reduced_dim) > 1) stop("'reduced_dim' should be a character of length 1.")
            if(!reduced_dim %in% reducedDimNames(sc_object)) stop("'reducedDim' slot of the SingleCellExperiment object does not contain the name provided in 'reduced_dim'.")
            reduced_dim <- reducedDim(sc_object, reduced_dim)

            # validate input and extract expression
            if(!is.character(expression) | length(expression) > 1) stop("'expression' should be a character of length 1.")
            if(!expression %in% assayNames(sc_object)) stop("'assay' slot of the SingleCellExperiment object does not contain the name provided in 'expression'.")
            expression <- assay(sc_object, expression)

            # validate genes of intereset and subset expression
            if(!is.null(genes)){
              if(!is.character(genes) & !is.list(genes)) stop("'genes' should be a character or a list.")
              if(!all(genes %in% rownames(expression))){
                false_genes <- which(!genes %in% rownames(expression))
                print("The row names do not contain the following genes:")
                print(genes[false_genes])
                stop("'genes' contains invalid names.")
              }
              expression <- expression[genes,]
            } else {
              stop("Please provide a vector with genes/ features of interest. The names should match with the row names of the 'assay' slot of the SingleCellExperiment object.")
            }

            # validate grouping
            if(!is.character(grouping) | length(grouping) > 1) stop("'grouping' should be a character of length 1.")

            labels <- colnames(sc_object)

            object <- add_gene_expression_page(object = object,
                                               reduced_dim = reduced_dim,
                                               expression = expression,
                                               metadata = metadata,
                                               grouping = grouping,
                                               title = title,
                                               labels = labels,
                                               menu = menu,
                                               sidebar = sidebar)
            return(object)
          })

#' Renders a page with two linked components. The first component is a scatterplot, showing samples in along coordinates from \code{reduced_dim}. The second component is a violin plot, that shows expression values from \code{expression} by groups defined in \code{grouping}.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param sc_object A valid \linkS4class{Seurat::Seurat} object
#' @param reduced_dim A character of length 1 representing the name of a \linkS4class{Seurat::DimReduc} object present in the slot \code{reductions} of the \linkS4class{Seurat::Seurat} object.
#' @param metadata A character or list representing the names of columns in the slot \code{meta.data} in the \linkS4class{Seurat::Seurat} object.
#' @param grouping A character of length 1 representing the name of a column in the slot \code{meta.data} in the \linkS4class{Seurat::Seurat} and used for expression grouping.
#' @param assay A character of length 1 representing the name of a \linkS4class{Seurat::Assay} object present in the slot \code{assays} of the \linkS4class{Seurat::Seurat} object.
#' @param assay_slot (Default value: "data") A character of length 1 representing the name of a slot in the \linkS4class{Seurat::Assay} object provided in \code{assay}.
#' @param genes A character or list of genes of interest that are present in row names.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#' @param sidebar (Optional) The page layout (see below).
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_gene_expression_page",
          signature = signature(object = "i2dashboard", sc_object = "Seurat"),
          function(object, sc_object, reduced_dim, metadata, grouping, assay, assay_slot = "data", genes = NULL, title = NULL, menu = NULL, sidebar = NULL) {

            # validate and extract metadata
            if(!is.character(metadata) & !is.list(metadata)) stop("'metadata' should be a character or a list.")
            if(!all(metadata %in% names(sc_object@meta.data))) stop("'meta.data' slot of the Seurat object does not contain the column names from 'metadata'.")
            metadata <- sc_object@meta.data[metadata]

            # validate input and extract dimension reduction
            if(!is.character(reduced_dim) | length(reduced_dim) > 1) stop("'reduced_dim' should be a character of length 1.")
            if(!reduced_dim %in% names(sc_object@reductions)) stop("'reductions' slot of the Seurat object does not contain the name provided in 'reduced_dim'.")
            reduced_dim <- Seurat::Embeddings(sc_object, reduction = reduced_dim)[,1:2]

            # validate input and extract expression
            if(!is.character(assay) | length(assay) > 1) stop("'assay' should be a character of length 1.")
            if(!is.character(assay_slot) | length(assay_slot) > 1) stop("'assay_slot' should be a character of length 1.")
            if(!assay %in% names(sc_object@assays)) stop(paste0("The 'assay' slot of the Seurat object does not contain an Assay object with the name '", assay,"'."))
            assay_obj <- Seurat::GetAssay(object = sc_object, assay = assay)
            if(!assay_slot %in% slotNames(assay_obj)) stop(paste0("A slot with the name '", assay_slot, "' is not present in the Assay object '", assay, "'of the Seurat object."))
            expression <- Seurat::GetAssayData(object = assay_obj, slot = assay_slot)
            if(nrow(reduced_dim) != ncol(expression)) stop("The number of rows in 'reduced_dim' should be equal to the number of columns of the 'assay_slot'")
            expression <- as.matrix(expression) # covert dgCMatrix to regular matrix

            # validate genes of intereset and subset expression
            if(!is.null(genes)){
              if(!is.character(genes) & !is.list(genes)) stop("'genes' should be a character or a list.")
              if(!all(genes %in% rownames(expression))){
                false_genes <- which(!genes %in% rownames(expression))
                print("The row names do not contain the following genes:")
                print(genes[false_genes])
                stop("'genes' contains invalid names.")
              }
              expression <- expression[genes,]
            } else {
              stop("Please provide a vector with genes/ features of interest. The names should match with the row names of the provided 'assay_slot'.")
            }

            # validate grouping
            if(!is.character(grouping) | length(grouping) > 1) stop("'grouping' should be a character of length 1.")

            labels <- colnames(expression)

            object <- add_gene_expression_page(object = object,
                                               reduced_dim = reduced_dim,
                                               expression = expression,
                                               metadata = metadata,
                                               grouping = grouping,
                                               title = title,
                                               labels = labels,
                                               menu = menu,
                                               sidebar = sidebar)
            return(object)
          })
