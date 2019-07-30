# pfgsea

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/pfgsea.svg?branch=master)](https://travis-ci.com/acidgenomics/pfgsea)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/fa5hpl1hbf4memee/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/pfgsea/branch/master)

Parameterized Fast GSEA

[pfgsea][] extends the functionality of [fgsea][]. The package is designed to quickly perform multiple GSEA comparisons on RNA-seq differential expression results.

## Installation

### [R][] method

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
Sys.setenv(R_REMOTES_UPGRADE = "always")
# Set `GITHUB_PAT` in `~/.Renviron` if you get a rate limit error.
remotes::install_github("acidgenomics/pfgsea")
```

Here's how to update to the latest version on GitHub:

```r
Sys.setenv(R_REMOTES_UPGRADE = "always")
remotes::update_packages()
```

Always check that your Bioconductor installation is valid before proceeding.

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
BiocManager::valid()
```

[BiocManager]: https://cran.r-project.org/package=BiocManager
[Bioconductor]: https://bioconductor.org/
[fgsea]: https://bioconductor.org/packages/fgsea/
[pfgsea]: https://pfgsea.acidgenomics.com/
[R]: https://www.r-project.org
