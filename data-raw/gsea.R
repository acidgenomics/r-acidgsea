## Example *Homo sapiens* GSEA analysis.
## Updated 2020-03-17.

library(usethis)
library(pryr)
library(basejump)  # 0.12.5
library(DESeq2)    # 1.26.0

## Restrict to 1 MB.
## Use `pryr::object_size()` instead of `utils::object.size()`.
limit <- structure(1e6, class = "object_size")

gr <- makeGRangesFromEnsembl(organism = "Homo sapiens", release = 99L)
## Subset to include 10k genes, as minimal example.
gr <- head(gr, n = 10000L)

dds <- makeExampleDESeqDataSet(n = length(gr), m = 12L)
rowRanges(dds) <- gr
design(dds)
dds <- DESeq(dds)

res <- results(dds)
g2s <- Gene2Symbol(gr, format = "unmodified")

rankedList <- RankedList(object = res, gene2symbol = g2s)
object_size(rankedList)
## 729 kB
stopifnot(object_size(rankedList) < limit)

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

## Check the object size.
lapply(coerceToList(gsea), object_size)
object_size(gsea)
lapply(metadata(gsea), object_size)
stopifnot(object_size(gsea) < limit)

use_data(gsea, overwrite = TRUE, compress = "xz")
