## Example *Homo sapiens* GSEA analysis.
## Updated 2020-03-17.

library(usethis)
library(pryr)
library(basejump)       # 0.12.5
library(DESeq2)         # 1.26.0
library(DESeqAnalysis)  # 0.2.19

## Restrict to 1 MB.
## Use `pryr::object_size()` instead of `utils::object.size()`.
limit <- structure(1e6, class = "object_size")

gr <- makeGRangesFromEnsembl(organism = "Homo sapiens", release = 99L)
## Subset to include 5k genes, as minimal example.
gr <- head(gr, n = 5000L)

## DESeqDataSet
dds <- makeExampleDESeqDataSet(n = length(gr), m = 12L)
dds$treatment <- as.factor(x = rep(c("C", "D"), each = 3L))
design(dds) <- ~ condition + treatment + condition:treatment
rowRanges(dds) <- gr
dds <- DESeq(dds)
dds$condition
##  [1] A A A A A A B B B B B B
## Levels: A B
dds$treatment
##  [1] C C C D D D C C C D D D
## Levels: C D

## DESeqTransform
dt <- varianceStabilizingTransformation(dds)

## DESeqResults
contrasts <- list(
    "condition_B_vs_A" = c(
        factor = "condition",
        numerator = "B",
        denominator = "A"
    ),
    "treatment_D_vs_C" = c(
        factor = "treatment",
        numerator = "D",
        denominator = "C"
    )
)
res <- mapply(
    FUN = DESeq2::results,
    contrast = contrasts,
    MoreArgs = list(object = dds),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
)

## DESeqAnalysis
deseq <- DESeqAnalysis(
    data = dds,
    transform = dt,
    results = res,
    lfcShrink = NULL
)

rankedList <- RankedList(deseq)
object_size(rankedList)
## 448 kB
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
