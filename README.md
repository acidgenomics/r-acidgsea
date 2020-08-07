# acidgsea

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/acidgsea.svg?branch=master)](https://travis-ci.com/acidgenomics/acidgsea)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/fa5hpl1hbf4memee/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/acidgsea/branch/master)

Perform parameterized gene set enrichment analysis (GSEA) on multiple differential expression contrasts.

[acidgsea][] currently extends the functionality of [fgsea][].

## Installation

### [R][] method

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
Sys.setenv(R_REMOTES_UPGRADE = "always")
# Set `GITHUB_PAT` in `~/.Renviron` if you get a rate limit error.
remotes::install_github("acidgenomics/acidgsea")
```

### [Docker][] method

```sh
image="acidgenomics/r-rnaseq"
workdir="/mnt/work"
docker pull "$image"
docker run -it \
    --volume="${PWD}:${workdir}" \
    --workdir="$workdir" \
    "$image" \
    R
```

[acidgsea]: https://acidgsea.acidgenomics.com/
[biocmanager]: https://cran.r-project.org/package=BiocManager
[bioconductor]: https://bioconductor.org/
[docker]: https://www.docker.com/
[fgsea]: https://bioconductor.org/packages/fgsea/
[r]: https://www.r-project.org
