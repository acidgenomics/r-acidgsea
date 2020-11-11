# AcidGSEA

Perform parameterized gene set enrichment analysis (GSEA) on multiple differential expression contrasts.

[AcidGSEA][] currently extends the functionality of [fgsea][].

## Installation

### [R][] method

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install.packages(
    pkgs = "AcidGSEA",
    repos = c(
        "https://r.acidgenomics.com",
        BiocManager::repositories()
    )
)
```

[acidgsea]: https://acidgsea.acidgenomics.com/
[fgsea]: https://bioconductor.org/packages/fgsea/
[r]: https://www.r-project.org
