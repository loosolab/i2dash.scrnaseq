#' Add a gene expression page.
#'
#' This function adds a page with two linked components to the \code{report} object: A scatterplot, showing samples in along two-dimensional coordinates, and a violin plot, showing feature expression values by groups defined in \code{group_by}.
#'
#' @name add_gene_expression_page
#' @rdname add_gene_expression_page
#' @exportMethod add_gene_expression_page
setGeneric("add_gene_expression_page", function(report, object, ...) standardGeneric("add_gene_expression_page"))

#' @rdname add_gene_expression_page
#' @param report An object of class \linkS4class{i2dash::i2dashboard}.
#' @param use_dimred A data.frame or matrix containing coordinates of the reduced dimensions.
#' @param exprs_values A data.frame or matrix containing expression data of features of interest in rows and samples in columns.
#' @param group_by A factor or data.frame along rows of \code{use_dimred} that is used for grouping expression values in the violin plot.
#' @param labels A vector with optional sample labels that are used instead of \code{rownames(use_dimred)}.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_gene_expression_page",
          signature = signature(report = "i2dashboard", object = "missing"),
          function(report, use_dimred, exprs_values, group_by = NULL, labels = rownames(use_dimred), title = "Gene expression", menu = NULL) {

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Input validation
            assertive.types::assert_is_any_of(use_dimred, c("data.frame", "matrix"))
            assertive.types::assert_is_any_of(exprs_values, c("data.frame", "matrix"))

            if(ncol(use_dimred) < 2 ) stop("'use_dimred' should contain at least two columns.")

            if(!is.null(group_by)) assertive.types::assert_is_any_of(group_by, c("factor", "data.frame"))

            # Prepare expression data
            exprs_values %>%
              as.data.frame() %>%
              tibble::rownames_to_column(var = "feature") %>%
              dplyr::mutate(feature = paste0("feature_", feature)) %>%
              tibble::column_to_rownames(var = "feature") %>%
              t() -> e

            # Prepare grouping
            if(is.null(group_by)) group_by <- factor(rep(1, nrow(use_dimred)))
            if(is.factor(group_by)) {
              group_by <- data.frame(default = group_by)
            } else {
              group_by %<>% dplyr::select_if(is.factor)
            }

            # Create plot data
            data.frame(x = use_dimred[, 1], y = use_dimred[, 2], label = labels) %>%
              cbind(e, group_by) %>%
              tidyr::gather(key = "feature", value = "expression", dplyr::starts_with("feature_")) %>%
              tidyr::gather(key = "group_by", value = "level", -x, -y, -label, -feature, -expression) -> data

            # Create component environment
            env <- new.env()
            env$data <- data
            env$group_filter <- colnames(group_by)[1]
            saveRDS(env, file = file.path(report@datadir, paste0(env_id, ".rds")))

            expanded_components <- list()
            timestamp <- Sys.time()

            # fill list "expanded_components" with components
            scatterplot_component <- knitr::knit_expand(file = system.file("templates", "gene_expression_dimred.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
            expanded_components <- append(expanded_components, scatterplot_component)

            violinplot_component<- knitr::knit_expand(file = system.file("templates", "gene_expression_violin_table.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
            expanded_components <- append(expanded_components, violinplot_component)

            # Expand component
            timestamp <- Sys.time()

            report@pages[["gene_expression_page"]] <- list(title = title, layout = "2x2_grid", menu = menu, components = expanded_components, max_components = 2, sidebar = NULL)
            return(report)
          })

#' @rdname add_gene_expression_page
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @inheritParams add_gene_expression_page,i2dashboard,missing-method
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_gene_expression_page",
          signature = signature(report = "i2dashboard", object = "SingleCellExperiment"),
          function(report, object, use_dimred, expression, metadata, grouping, genes, title = NULL, menu = NULL, sidebar = NULL) {

            # validate and extract metadata
            if(!is.character(metadata) & !is.list(metadata)) stop("'metadata' should be a character or a list.")
            if(!all(metadata %in% names(colData(object)))) stop("'colData' slot of the SingleCellExperiment object does not contain the column names from 'metadata'.")
            metadata <- as.data.frame(object@colData[metadata])

            # validate input and extract dimension reduction
            if(!is.character(use_dimred) | length(use_dimred) > 1) stop("'use_dimred' should be a character of length 1.")
            if(!use_dimred %in% reducedDimNames(object)) stop("'reducedDim' slot of the SingleCellExperiment object does not contain the name provided in 'use_dimred'.")
            use_dimred <- reducedDim(object, use_dimred)

            # validate input and extract expression
            if(!is.character(expression) | length(expression) > 1) stop("'expression' should be a character of length 1.")
            if(!expression %in% assayNames(object)) stop("'assay' slot of the SingleCellExperiment object does not contain the name provided in 'expression'.")
            expression <- assay(object, expression)

            # validate genes of interest and subset expression
            if(!is.null(genes)){
              if(!is.character(genes) & !is.list(genes)) stop("'genes' should be a character or a list.")
              if(!all(genes %in% rownames(expression))){
                false_genes <- which(!genes %in% rownames(expression))
                print("The row names do not contain the following genes:")
                print(genes[false_genes])
                stop("'genes' contains invalid names.")
              }
              expression <- expression[genes,]
            }

            # validate grouping
            if(!is.character(grouping) | length(grouping) > 1) stop("'grouping' should be a character of length 1.")

            labels <- colnames(object)

            report <- add_gene_expression_page(report = report,
                                               use_dimred = use_dimred,
                                               expression = expression,
                                               metadata = metadata,
                                               grouping = grouping,
                                               title = title,
                                               labels = labels,
                                               menu = menu,
                                               sidebar = sidebar)
            return(report)
          })

#' Renders a page with two linked components. The first component is a scatterplot, showing samples in along coordinates from \code{reduced_dim}. The second component is a violin plot, that shows expression values from \code{expression} by groups defined in \code{grouping}.
#'
#' @param report An object of class \linkS4class{i2dash::i2dashboard}.
#' @param object A valid \linkS4class{Seurat::Seurat} object.
#' @param reduced_dim A character of length 1 representing the name of a \linkS4class{Seurat::DimReduc} object present in the slot \code{reductions} of the \linkS4class{Seurat::Seurat} object.
#' @param metadata A character or list representing the names of columns in the slot \code{meta.data} in the \linkS4class{Seurat::Seurat} object.
#' @param grouping A character of length 1 representing the name of a column in the slot \code{meta.data} in the \linkS4class{Seurat::Seurat} and used for expression grouping.
#' @param assay A character of length 1 representing the name of a \linkS4class{Seurat::Assay} object present in the slot \code{assays} of the \linkS4class{Seurat::Seurat} object.
#' @param assay_slot (Default value: "data") A character of length 1 representing the name of a slot in the \linkS4class{Seurat::Assay} object provided in \code{assay}.
#' @param genes A character or list of genes of interest that are present in row names of the provided \code{'assay_slot'}.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#' @param sidebar (Optional) The page layout (see below).
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_gene_expression_page",
          signature = signature(report = "i2dashboard", object = "Seurat"),
          function(report, object, reduced_dim, metadata, grouping, assay, assay_slot = "data", genes = NULL, title = NULL, menu = NULL, sidebar = NULL) {

            # validate and extract metadata
            if(!is.character(metadata) & !is.list(metadata)) stop("'metadata' should be a character or a list.")
            if(!all(metadata %in% names(object@meta.data))) stop("'meta.data' slot of the Seurat object does not contain the column names from 'metadata'.")
            metadata <- object@meta.data[metadata]

            # validate input and extract dimension reduction
            if(!is.character(reduced_dim) | length(reduced_dim) > 1) stop("'reduced_dim' should be a character of length 1.")
            if(!reduced_dim %in% names(object@reductions)) stop("'reductions' slot of the Seurat object does not contain the name provided in 'reduced_dim'.")
            reduced_dim <- Seurat::Embeddings(object, reduction = reduced_dim)[,1:2]

            # validate input and extract expression
            if(!is.character(assay) | length(assay) > 1) stop("'assay' should be a character of length 1.")
            if(!is.character(assay_slot) | length(assay_slot) > 1) stop("'assay_slot' should be a character of length 1.")
            if(!assay %in% names(object@assays)) stop(paste0("The 'assay' slot of the Seurat object does not contain an Assay object with the name '", assay,"'."))
            assay_obj <- Seurat::GetAssay(object = object, assay = assay)
            if(!assay_slot %in% slotNames(assay_obj)) stop(paste0("A slot with the name '", assay_slot, "' is not present in the Assay object '", assay, "'of the Seurat object."))
            expression <- Seurat::GetAssayData(object = assay_obj, slot = assay_slot)
            if(nrow(reduced_dim) != ncol(expression)) stop("The number of rows in 'reduced_dim' should be equal to the number of columns of the 'assay_slot'")
            expression <- as.matrix(expression) # convert dgCMatrix to regular matrix

            # validate genes of interest and subset expression
            if(!is.null(genes)){
              if(!is.character(genes) & !is.list(genes)) stop("'genes' should be a character or a list.")
              if(!all(genes %in% rownames(expression))){
                false_genes <- which(!genes %in% rownames(expression))
                print("The row names do not contain the following genes:")
                print(genes[false_genes])
                stop("'genes' contains invalid names.")
              }
              expression <- expression[genes,]
            }

            # validate grouping
            if(!is.character(grouping) | length(grouping) > 1) stop("'grouping' should be a character of length 1.")

            labels <- colnames(expression)

            report <- add_gene_expression_page(report = report,
                                               reduced_dim = reduced_dim,
                                               expression = expression,
                                               metadata = metadata,
                                               grouping = grouping,
                                               title = title,
                                               labels = labels,
                                               menu = menu,
                                               sidebar = sidebar)
            return(report)
          })
