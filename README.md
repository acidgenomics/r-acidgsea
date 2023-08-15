# AcidGSEA

[![Install with Bioconda](https://img.shields.io/badge/install%20with-bioconda-brightgreen.svg)](http://bioconda.github.io/recipes/r-acidgsea/README.html) ![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)

Perform parameterized gene set enrichment analysis (GSEA) on multiple
differential expression contrasts. Currently extends the functionality of
[fgsea][].

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
    ),
    dependencies = TRUE
)
```

### [Conda][] method

Configure [Conda][] to use the [Bioconda][] channels.

```sh
# Don't install recipe into base environment.
name='r-acidgsea'
conda create --name="$name" "$name"
conda activate "$name"
R
```

[bioconda]: https://bioconda.github.io/
[conda]: https://conda.io/
[fgsea]: https://bioconductor.org/packages/fgsea/
[r]: https://www.r-project.org
