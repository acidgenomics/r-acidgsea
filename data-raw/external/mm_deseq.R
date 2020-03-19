## Minimal mouse DESeqDataSet to for testing human ortholog matching.
## Updated 2020-03-16.

library(pryr)
library(basejump)  # 0.12.4

data(deseq, package = "DESeqAnalysis")
object <- deseq
rm(deseq)

limit <- structure(1e6, class = "object_size")

rr <- makeGRangesFromEnsembl(
    organism = "Mus musculus",
    release = 99L
)
rr <- rr[1L:nrow(object@data)]
rr <- droplevels(rr)
## Ensure that the names match, otherwise we'll hit a validity check failure.
names(rr) <- rownames(object@data)
rowRanges(object@data) <- rr
rowRanges(object@transform) <- rr

stopifnot(object_size(object) < limit)
object_size(object)
# 905 kB

validObject(object)
object

mm_deseq <- object
saveData(mm_deseq, dir = "~", compress = "xz")
