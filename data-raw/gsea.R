library(usethis)
library(pryr)

data(deseq, package = "DESeqAnalysis")

## Ensure that we're using Ensembl 99 in working example, to avoid current
## issue with archives for biomaRt being unavailable during server migration,
## which applies until March 24, 2020.
stopifnot(identical(metadata(rowRanges(deseq@data))[["ensemblRelease"]], 99L))

rankedList <- RankedList(deseq)

## RankedList of length 2
## names(2): condition_B_vs_A treatment_D_vs_C

## Just using hallmark in minimal example.
gmtFiles <- file.path(
    "~",
    "msigdb",
    "7.0",
    "msigdb_v7.0_GMTs",
    "h.all.v7.0.symbols.gmt"
)
stopifnot(all(file.exists(gmtFiles)))
names(gmtFiles) <- "h"

gsea <- pfgsea(
    rankedList = rankedList,
    gmtFiles = gmtFiles
)
validObject(gsea)

object_size(gsea)
## 113 kB

use_data(gsea)
