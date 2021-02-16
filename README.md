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

[acidgsea]: https://acidgsea.acidgenomics.com/
[docker]: https://www.docker.com/
[fgsea]: https://bioconductor.org/packages/fgsea/
[r]: https://www.r-project.org
