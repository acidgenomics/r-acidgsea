# AcidGSEA

Perform parameterized gene set enrichment analysis (GSEA) on multiple differential expression contrasts.

Currently extends the functionality of [fgsea][].

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

### [Docker][] method

```sh
image="acidgenomics/r-acidgsea"
workdir="/mnt/work"
docker pull "$image"
docker run -it \
    --volume="${PWD}:${workdir}" \
    --workdir="$workdir" \
    "$image" \
    R
```

[docker]: https://www.docker.com/
[fgsea]: https://bioconductor.org/packages/fgsea/
[r]: https://www.r-project.org
