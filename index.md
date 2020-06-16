# **i2dash.scRNAseq**: An i2dash extension for single-cell RNA-sequencing data

![](man/figures/i2dash_header.png)

## Abstract

The rapid development of NGS technologies enables the investigation of the transcriptome of thousands of cells in a single experiment. scRNA-seq allows to identify rare cell types and populations, to uncover gene regulatory networks, and to track the trajectories of distinct cell lineages in development. The extension **i2dash.scrnaseq** enables an enhanced user interactivity and contains simple but effective tools for the creation of an i2dashboard with focus on scRNA-seq data visualization and exploration. Read further information of the core i2dash package [here](https://loosolab.github.io/i2dash/).

### Features of **i2dash.scrnaseq**:

- Usage of common single-cell classes "SingleCellExperiment" & "Seurat" for fast creation of components
- Useful and flexible plots with extended interactivity through Shiny widgets
- Pre-defined pages with focus on different aspects of scRNA-seq data
- Linked plots, which recalculate themselfes based on specific selections

## Installation:

```r
install.packages("BiocManager")
BiocManager::install(c("i2dash", "i2dash.scrnaseq"))
```

## Where to start

Based on three common use-cases for single-cell data analysis you can explore the possibilities and features of the **i2dash.scrnaseq** extension. Note: The use-cases show an increasing degree of difficulty and complexity.

### Use-cases:

1. [**Quality metrics**](articles/Quality_metrics.html)

   This use-case shows how to build a SingleCellExperiment object from a count-matrix, calculate quality metrics and create a simple dashboard to visualize different quality aspects of an scRNA-seq dataset. [Demo](http://mpibn-mampok.134.176.27.161.xip.io/use-case-1/i2dash/)

2. [**Single-cell data explorer**](articles/sc_data_explorer.html)

   The second use-case demonstrates how to build a dashboard, which enables the exploration of gene expression values and metadata coupled with reduced dimension plots. Mainly native tools and pre-defined pages from the i2dash.scrnaseq package are used. [Demo](http://mpibn-mampok.134.176.27.161.xip.io/use-case-2/i2dash/)

3. [**Visualized parameter selection for experts**](articles/vis_parameter_selection.html)

   The third use-case provides tools of i2dash.scrnaseq, which are useful during the process of data analysis but are not reasonable for the final report. The pre-defined pages enable the selection of features, the inspection of the normalisazion and the selection of optimal paramters for a dimensionality reduction. [Demo](http://mpibn-mampok.134.176.27.161.xip.io/use-case-3/i2dash/)

### Further reading:

- [**Documentation**](articles/Documentation.html) This page gives you an overview over all features containing in the **i2dash.scrnaseq** package.

- [**Developer guide**](articles/Developer_guide.html) This tutorial explains how to build a customized componentor a pre-defined page from scratch and how to link the components of your page together.

## How to cite
Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum.

## License
This project is licensed under the MIT license.


