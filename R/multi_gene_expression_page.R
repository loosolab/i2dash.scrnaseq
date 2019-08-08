setGeneric("add_gene_grid_page", function(object, sc_object, ...) standardGeneric("add_gene_grid_page"))

#' Renders a page, where the user first can select the dimension reduction from \code{reduced_dim} (if a list is given). Then several features from \code{expression} can be selected and also the number of columns to render scatter plots along coordinates from \code{reduced_dim} for each selected feature and coloured by its expression values from \code{expression}.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param reduced_dim A named list of data.frames or matrices or a single data.frame or matrix containing coordinates of the reduced dimensions.
#' @param expression A data.frame or matrix containing expression data of features of interest in rows and samples in columns.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_gene_grid_page",
          signature = signature(object = "i2dashboard", sc_object = "missing"),
          function(object, reduced_dim, expression, title = NULL, menu = NULL) {

            # validate if interactive mode is used
            if(!object@interactive) warning("This page can only be used with the shiny based interactive mode. Consider setting the 'interactive' argument during creation of the 'i2dashboard' object to 'TRUE'.")

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # set in list
            if(!is.list(reduced_dim)) reduced_dim <- list(reduced_dim)
            # if no names provided, set names
            if(is.null(names(reduced_dim))) {
              varname <- c('reduced_dim')
              names(reduced_dim) <- unlist(mapply(function(x,y) paste(x, seq(1,y), sep="_"), varname, length(reduced_dim)))
            }
            # validate input
            if(!is.data.frame(expression) & !is.matrix(expression)) stop("'expression' should be of class 'data.frame' or 'matrix'.")
            l <- c(ncol(expression))
            for(df in reduced_dim){
              if(!is.data.frame(df) & !is.matrix(df)) stop("'reduced_dim' should be of class 'data.frame' or 'matrix'.")
              if(!all(sapply(df, is.numeric))) stop("The data.frame in 'reduced_dim' should contain numerical values only.")
              if(ncol(df) < 2 ) stop("'reduced_dim' should contain at least two columns.")
              l <- c(l, nrow(df))
            }
            if(length(unique(l)) > 1) stop("The number of columns of 'expression' should be equal to the number of rows in 'reduced_dim'.")
            if(!is.null(title) & !is.character(title)) stop("'title' should be a character vector.")

            # Create component environment
            env <- new.env()
            env$selection_reduced_dim <- length(reduced_dim) > 1
            env$reduced_dim <- reduced_dim
            env$expression <- expression

            # save environment object
            saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

            expanded_components <- list()
            timestamp <- Sys.time()

            # fill list "expanded_components" with components
            multi_gene_expr_component <- knitr::knit_expand(file = system.file("templates", "multi_gene_expression.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)

            # Expand component
            timestamp <- Sys.time()

            object@pages[["gene_grid_page"]] <- list(title = title, layout = "empty", menu = menu, components = multi_gene_expr_component, max_components = 1, sidebar = NULL)
            return(object)
          })

#' Renders a page, where the user first can select the dimension reduction from \code{reduced_dim} (if a list is given). Then several features from \code{expression} can be selected and also the number of columns to render scatter plots along coordinates from \code{reduced_dim} for each selected feature and coloured by its expression values from \code{expression}.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param sc_object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object
#' @param reduced_dim A list or character representing the name of a dimension reduction of \code{reducedDim} in the SingleCellObject.
#' @param expression A character of length 1 representing the name of an \code{assay}.
#' @param genes A character or list of genes of interest that are present in row names of the \code{reduced_dim}.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_gene_grid_page",
          signature = signature(object = "i2dashboard", sc_object = "SingleCellExperiment"),
          function(object, sc_object, reduced_dim, expression, genes, title = NULL, menu = NULL) {

            # validate input and extract dimension reduction
            if(!is.character(reduced_dim) & !is.list(reduced_dim)) stop("'reduced_dim' should be a character or a list")
            reduced_dim_list <- list()
            for(reddim in reduced_dim){
              if(!reddim %in% reducedDimNames(sc_object)) stop(paste0("'reducedDim' slot of the SingleCellExperiment object does not contain the name ", reddim))
              reduced_dim_list[[reddim]] <-reducedDim(sce, reddim)
            }
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
            }

            object <- add_gene_grid_page(object = object,
                                         reduced_dim = reduced_dim_list,
                                         expression = expression,
                                         title = title,
                                         menu = menu)
            return(object)

          })

#' Renders a page, where the user first can select the dimension reduction from \code{reduced_dim} (if a list is given). Then several features from \code{expression} can be selected and also the number of columns to render scatter plots along coordinates from \code{reduced_dim} for each selected feature and coloured by its expression values from \code{expression}.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param sc_object A valid \linkS4class{Seurat::Seurat} object
#' @param reduced_dim A list or character representing the name of a \linkS4class{Seurat::DimReduc} object present in the slot \code{reductions} of the \linkS4class{Seurat::Seurat} object.
#' @param assay A character of length 1 representing the name of a \linkS4class{Seurat::Assay} object present in the slot \code{assays} of the \linkS4class{Seurat::Seurat} object.
#' @param assay_slot (Default value: "data") A character of length 1 representing the name of a slot in the \linkS4class{Seurat::Assay} object provided in \code{assay}.
#' @param genes A character or list of genes of interest that are present in row names of the provided \code{'assay_slot'}.

#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_gene_grid_page",
          signature = signature(object = "i2dashboard", sc_object = "Seurat"),
          function(object, sc_object, reduced_dim, assay, assay_slot = "data", genes, title = NULL, menu = NULL) {

            # validate input and extract dimension reduction
            if(!is.character(reduced_dim) & !is.list(reduced_dim)) stop("'reduced_dim' should be a character or a list")
            reduced_dim_list <- list()
            nrows <- c()
            for(reddim in reduced_dim){
              if(!reddim %in% names(sc_object@reductions)) stop(paste0("'reductions' slot of the Seurat object does not contain the name ", reddim))
              reduced_dim_list[[reddim]] <- Seurat::Embeddings(sc_object, reduction = reddim)[,1:2]
              nrows <- c(nrows, nrow(reduced_dim_list[[reddim]]))
            }
            # validate input and extract expression
            if(!is.character(assay) | length(assay) > 1) stop("'assay' should be a character of length 1.")
            if(!is.character(assay_slot) | length(assay_slot) > 1) stop("'assay_slot' should be a character of length 1.")
            if(!assay %in% names(sc_object@assays)) stop(paste0("The 'assay' slot of the Seurat object does not contain an Assay object with the name '", assay,"'."))
            assay_obj <- Seurat::GetAssay(object = sc_object, assay = assay)
            if(!assay_slot %in% slotNames(assay_obj)) stop(paste0("A slot with the name '", assay_slot, "' is not present in the Assay object '", assay, "'of the Seurat object."))
            expression <- Seurat::GetAssayData(object = assay_obj, slot = assay_slot)
            if(!all(nrows == nrows[[1]])) stop("The number of rows in 'reduced_dim' should be equal to the number of columns of the 'assay_slot'")
            #if(nrow(reduced_dim) != ncol(expression)) stop("The number of rows in 'reduced_dim' should be equal to the number of columns of the 'assay_slot'")
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

            object <- add_gene_grid_page(object = object,
                                         reduced_dim = reduced_dim_list,
                                         expression = expression,
                                         title = title,
                                         menu = menu)
            return(object)

          })
