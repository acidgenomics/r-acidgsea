## Minimal mouse DESeqDataSet to for testing human ortholog matching.
## Updated 2022-05-24.
## nolint start
suppressPackageStartupMessages({
    library(basejump)
})
## nolint end
data(deseq, package = "DESeqAnalysis")
object <- deseq
rm(deseq)
limit <- structure(1e6L, class = "object_size")
rr <- makeGRangesFromEnsembl(
    organism = "Mus musculus",
    release = 99L
)
rr <- rr[seq_len(object@data)]
rr <- droplevels2(rr)
## Ensure that the names match, otherwise we'll hit a validity check failure.
names(rr) <- rownames(object@data)
rowRanges(object@data) <- rr
rowRanges(object@transform) <- rr
stopifnot(
    object.size(object) < limit,
    validObject(object)
)
mm_deseq <- object # nolint
saveData(mm_deseq, dir = "~", compress = "xz")
