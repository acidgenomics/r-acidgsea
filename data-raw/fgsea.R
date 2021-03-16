suppressPackageStartupMessages({
    library(usethis)
    library(goalie)
    library(basejump)       # 0.14.17
    library(DESeq2)         # 1.30.1
    library(DESeqAnalysis)  # 0.4.0
})

## Restrict to 3 MB.
limit <- structure(3e6, class = "object_size")

gr <- makeGRangesFromEnsembl(
    organism = "Homo sapiens",
    level = "genes",
    release = 100L,
    ignoreVersion = TRUE
)
gr <- as(gr, "GRanges")
gr <- gr[sort(names(gr))]
gr <- head(gr, n = 1000L)
gr <- droplevels(gr)
mcols(gr) <- mcols(gr)[c("geneId", "geneName")]
object.size(gr)
## 337624 bytes

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
## 939848 bytes

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
## 1965104 bytes

## Just using hallmark in minimal example.
geneSetFiles <- prepareGeneSetFiles(
    dir = system.file(
        "extdata",
        "msigdb",
        "7.0",
        "msigdb_v7.0_GMTs",
        package = "AcidGSEA",
        mustWork = TRUE
    ),
    pattern = "*.all.*.symbols.gmt"
)

fgsea <- FGSEAList(
    object = deseq,
    geneSetFiles = geneSetFiles,
    alphaThreshold = 0.99
)
validObject(fgsea)
assert(
    object.size(fgsea) < limit,
    hasRows(fgsea[[1L]][[1L]]),
    is(metadata(fgsea)[["deseq"]], "DESeqAnalysis")
)

use_data(fgsea, overwrite = TRUE, compress = "xz")
