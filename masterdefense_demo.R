library(i2dash)
library(i2dash.scrnaseq)
library(SingleCellExperiment)
library(DropletUtils)
library(plotly)
library(scater)
library(scran)

#########################################
# Load Data
#########################################

# Read the SingleCellExperiment
sce_full <- scRNAseq::ZilionisLungData("mouse")
sce_full <- sce_full[, colData(sce_full)$Used==TRUE]
# make uique sample names
colnames(sce_full) <- paste0(colnames(sce_full),"_", colData(sce_full)$Library)
# convert to factors
as_factor <- c("Library", "Animal", "Run", "Tissue", "Library prep batch", "Most likely Immgen cell type", "Major cell type", "Minor subset")
colData(sce_full)[as_factor] <- lapply(colData(sce_full)[as_factor], as.factor)
colnames(colData(sce_full))[11] <- c("Major.cell.type")
# optional downsampling for better performance
sce <- sce_full[, sample(ncol(sce_full), 2000)]
sce

# Cell-level QC
per.cell     <- perCellQCMetrics(sce, subsets = list(Mito = grep("mt-", rownames(sce))))
colData(sce) <- cbind(colData(sce), per.cell)

# Detect outliers
qc.stats     <- quickPerCellQC(sce, percent_subsets="subsets_Mito_percent")
colData(sce) <- cbind(colData(sce), qc.stats)


#########################################
# Create i2dashboard
#########################################
dashboard <- i2dashboard(
  title       = "scRNA-seq",
  author      = "Arsenij Ustjanzew",
  interactive = TRUE,
  theme       = "yeti",
  datadir     = "datadir_demo"
)

#########################################
# Intro page
#########################################
dashboard %<>% i2dash::add_page(
  page   = "intro",
  title  = "Introduction",
  layout = "2x2_grid"
) %>%
  i2dash::add_component(
    component = "descr.md",
    page      = "intro"
  ) %>%
  i2dash::add_component(
    component = "descr2.md",
    page      = "intro"
  )

#########################################
# Quality controll
#########################################
dashboard <- i2dash::add_page(
  dashboard,
  page   = "qc",
  title  = "Quality Control",
  layout = "storyboard"
)

#########################################
# Quality controll - Plot of Rank / total UMI
# Calculate barcode ranks
bcrank <- barcodeRanks(counts(sce))

# Function for horizontal lines
hline <- function(y = 0, color = "blue") {
  list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = y, y1 = y, line = list(color = color))
}

# plot the data
droplet <- plot_ly(as.data.frame(bcrank), x = ~rank, y = ~total) %>%
  plotly::layout(
    xaxis = list(type = "log", title = "Rank"),
    yaxis = list(type = "log", title = "Total UMI count"),
    shapes = list(
      hline(metadata(bcrank)$inflection, "darkgreen"),
      hline(metadata(bcrank)$knee, "dodgerblue")
    )
  )

dashboard <- i2dash::add_component(
  dashboard = dashboard,
  component = droplet,
  page      = "qc",
  title     = "Total UMI count for each barcode in the dataset, plotted against its rank in decreasing order of total counts (Blue line = inflection; Green line = knee)."
)

#########################################
# Quality controll - scatterplot
dashboard %<>% i2dash::add_component(
  component = i2dash.scrnaseq::scatterplot,
  object = sce,
  from = "colData",
  y = "sum",
  x = "detected",
  colour_by = c("Tissue", "Animal", "Library"),
  page      = "qc",
  title     = "Ration between detected genes and total count. Each point represents a cell that can be coloured by tissue, library, or biological replicate."
)

#########################################
# Quality controll - quality metrics
dashboard %<>% i2dash::add_component(
  component = i2dash.scrnaseq::violinplot,
  object = sce,
  from = "colData",
  y = c("sum", "detected", "subsets_Mito_percent"),
  group_by = c("Tissue", "Library", "Animal"),
  page      = "qc",
  title     = "Key quality metrics (total counts per cell, proportion of mitochondrial reads, number of detected features) grouped by condition."
)

#########################################
# Quality controll - Cell filtering
cell_filtering <- plot_ly(
  as.data.frame(colData(sce)),
  x = ~detected,
  y = ~subsets_Mito_percent,
  color = ~discard,
  type = "scatter",
  colors = c("TRUE" = "red", "FALSE" = "blue")
) %>%
  layout(
    yaxis = list(title = "Mitochondrial transcripts [%]"),
    xaxis = list(title = "Detected genes", type = "log")
  )

dashboard %<>% i2dash::add_component(
  component = cell_filtering,
  page      = "qc",
  title     = "Scatterplot of reads mapped to mitochondrial transcripts (%) against detected genes, coloured by discarded cells (red). Each point represents a cell."
)

#########################################
# Quality controll - Table

dashboard %<>% i2dash::add_page(
  page   = "summary",
  title  = "Summary",
  layout = "2x2_grid"
)

dashboard <- i2dash::add_component(
  dashboard = dashboard,
  component = i2dash.scrnaseq::summarize_metadata,
  object    = sce,
  from      = "colData",
  columns  = c("sum", "detected", "subsets_Mito_percent"),
  group_by  = "Tissue",
  page      = "summary",
  title     = "Metadata statistic"
)
library(kableExtra)
CellFilterStat <- t(apply(colData(sce)[c("low_n_features", "high_subsets_Mito_percent", "discard")], 2, summary))
colnames(CellFilterStat) <- c("reason", "kept", "discarded")
summary3 <- kable(CellFilterStat[,2:3], caption = "Number of kept / discarded cells per reason for filtering")%>%
  kable_styling(bootstrap_options = c("striped", "hover"))

dashboard %<>% i2dash::add_component(
  component = summary3,
  page      = "summary",
  title     = "Cell filtering"
)

#########################################
# Filtering
#########################################

sce2         <- sce[,!colData(sce)$discard]
keep_feature <- nexprs(sce, byrow=TRUE) > 0
sce2         <- sce2[keep_feature,]

#########################################
# Normalization
#########################################
sce2 <- logNormCounts(sce2)

# Get highly variable genes
gene.var <- modelGeneVar(sce2) # the warning message can be ignored
hvg      <- getTopHVGs(gene.var, prop=0.1)

#########################################
# Metadata page
#########################################

dashboard %<>% i2dash.scrnaseq::add_dimred_sample_page(
  object           = sce2,
  use_dimred       = "SPRING",
  sample_metadata  = c("Major.cell.type", "subsets_Mito_percent", "Tissue", "Animal", "Library"),
  group_by         = "Major.cell.type",
  show_group_sizes = TRUE,
  show_silhouette  = TRUE,
  page             = "dimred_sample_page1",
  title            = "Cell metadata 1",
  menu             = "Cell metadata"
)

# Page for metadata exploration with show_group_sizes = FALSE and show_silhouette = FALSE (see right image below)
dashboard %<>% i2dash.scrnaseq::add_dimred_sample_page(
  object           = sce2,
  use_dimred       = "SPRING",
  sample_metadata  = c("Major.cell.type", "Tissue", "sum", "detected", "Animal", "Library"),
  group_by         = "Major.cell.type",
  show_group_sizes = FALSE,
  show_silhouette  = FALSE,
  page             = "dimred_sample_page2",
  title            = "Cell metadata 2",
  menu             = "Cell metadata"
)

#########################################
# Gene explorer page
#########################################
dashboard %<>% i2dash.scrnaseq::add_feature_expression_page(
  object       = sce2,
  use_dimred   = "SPRING",
  exprs_values = "logcounts",
  group_by     = c("Major.cell.type", "Tissue", "Animal", "Library"),
  subset_row   = hvg[1:100],
  title        = "Gene explorer"
)

#########################################
# Marker genes page
#########################################
markers <- findMarkers(sce2, colData(sce2)$Major.cell.type)
marker_list <- lapply(markers, function(x){rownames(x)[1:10]})
marker_names <- unique(unlist(marker_list, recursive = F, use.names = F))

dashboard %<>% i2dash::add_page(
  page   = "marker",
  title  = "Marker genes",
  layout = "default"
) %>% i2dash::add_component(
  page = "marker",
  component = i2dash.scrnaseq::heatmap,
  object = sce2,
  exprs_values = "logcounts",
  subset_row = marker_names,
  split_by = c("Major.cell.type", "Tissue", "Animal", "Library"),
  aggregate_by = c("Major.cell.type", "Tissue", "Animal", "Library"),
  title = "Expression of marker genes"
)

#########################################
# Feature grid page
#########################################

dashboard %<>% i2dash.scrnaseq::add_feature_grid_page(
  object       = sce2,
  use_dimred   = "SPRING",
  exprs_values = "logcounts",
  subset_row   = hvg[1:100]
)

assemble(dashboard, file = "MyDashboard.Rmd", exclude="default")
