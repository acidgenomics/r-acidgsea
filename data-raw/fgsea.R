## Example *Homo sapiens* GSEA analysis.
## Updated 2020-09-23.

library(usethis)
library(basejump)       # 0.12.14
library(DESeq2)         # 1.28.1
library(DESeqAnalysis)  # 0.3.6

## Restrict to 2 MB.
limit <- structure(2e6, class = "object_size")

gr <- makeGRangesFromEnsembl(organism = "Homo sapiens", release = 100L)
gr <- head(gr, n = 500L)
gr <- droplevels(gr)
object.size(gr)
## 420560 bytes
mcols(gr) <- mcols(gr)[c("geneID", "geneName")]
object.size(gr)
## 219080 bytes

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
object.size(dds)
## 573464 bytes

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
object.size(deseq)
## 1196328 bytes

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
    object = deseq,
    geneSetFiles = geneSetFiles
)
validObject(fgsea)
stopifnot(object.size(fgsea) < limit)

use_data(fgsea, overwrite = TRUE, compress = "xz")
