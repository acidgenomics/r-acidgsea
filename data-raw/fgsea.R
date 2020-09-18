## Example *Homo sapiens* GSEA analysis.
## Updated 2020-09-18.

library(usethis)
library(pryr)
library(basejump)       # 0.12.14
library(DESeq2)         # 1.28.1
library(DESeqAnalysis)  # 0.3.6

## Restrict to 1 MB.
## Use `pryr::object_size()` instead of `utils::object.size()`.
limit <- structure(1e6, class = "object_size")

gr <- makeGRangesFromEnsembl(organism = "Homo sapiens", release = 100L)
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
geneSetFiles <- system.file(
    "extdata",
    "msigdb",
    "7.0",
    "msigdb_v7.0_GMTs",
    "h.all.v7.0.symbols.gmt",
    package = "acidgsea",
    mustWork = TRUE
)
names(geneSetFiles) <- "h"

fgsea <- FGSEAList(
    object = rankedList,
    geneSetFiles = geneSetFiles
)
validObject(fgsea)

## Check the object size.
lapply(coerceToList(fgsea), object_size)
object_size(fgsea)
lapply(metadata(fgsea), object_size)
stopifnot(object_size(fgsea) < limit)

use_data(deseq, fgsea, overwrite = TRUE, compress = "xz")
