**References:**

Rapolas Z. et al. (2019). *Single-Cell Transcriptomics of Human and Mouse Lung Cancers Reveals Conserved Myeloid Populations across Individuals and Species.* Immunity 50, 1317-1334.

**Abstract:**

Tumor-infiltrating myeloid cells (TIMs) comprise monocytes, macrophages, dendritic cells, and neutrophils, and have emerged as key regulators of cancer growth. These cells can diversify into a spectrum of states, which might promote or limit tumor outgrowth but remain poorly understood. Here, we used single-cell RNA sequencing (scRNA-seq) to map TIMs in non-small-cell lung cancer patients. We uncovered 25 TIM states, most of which were reproducibly found across patients. To facilitate translational research of these populations, we also profiled TIMs in mice. In comparing TIMs across species, we identified a near-complete congruence of population structures among dendritic cells and monocytes; conserved neutrophil subsets; and species differences among macrophages. By contrast, myeloid cell population structures in patients' blood showed limited overlap with those of TIMs. This study determines the lung TIM landscape and sets the stage for future investigations into the potential of TIMs as immunotherapy targets.

---

**scRNA-seq of CD45-positive cells from lungs of 2 healthy mice and 2 lung tumor-bearing mice were used.**

Data obtained from [here](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE127465)

A sample of the data set with 2000 cells was used for better performance. Full data contained 15939 cells:

```
class: SingleCellExperiment 
dim: 28205 2000 
metadata(0):
assays(1): counts
rownames(28205): 0610007P14Rik 0610009B22Rik ... mt-Nd5 mt-Nd6
rowData names(0):
colnames(2000): bc0517_h_1_1 bc0723_t_1_4 ... bc0010_t_1_4 bc0405_t_2_4
colData names(12): Library Barcode ... Major.cell.type Minor subset
reducedDimNames(1): SPRING
altExpNames(0):
```
