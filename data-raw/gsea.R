library(pryr)
library(usethis)

dataset <- file.path(
    "~",
    "draco",
    "datasets",
    "2018_12_EZH2_CPI1205_LNCaP_CSS_RNAseq_Genewiz"
)
stopifnot(dir.exists(dataset))

## FGSEAList
gsea <- readRDS(file.path(dataset, "rds", "2019-02-04", "gsea.rds"))
validObject(gsea)

object_size(gsea)
## 23.5 MB

gsea <- gsea["h"]

object_size(gsea)
## 3.83 MB

## nolint start
##
## gsea[["h"]] <- gsea[["h"]]["dmso_r1881_vs_etoh"]
##
## Error in .wrap_in_length_one_list_like_object(value, names(x)[[i]], x) :
##   failed to coerce 'list(value)' to a FGSEAList object of length
##   1
## Calls: [[<- ... .replace_list_element -> .wrap_in_length_one_list_like_object
## Backtrace:
##     █
##  1. ├─methods:::`[[<-`(...)
##  2. └─S4Vectors:::`[[<-`(...)
##  3.   ├─S4Vectors::setListElement(x, i, value)
##  4.   └─S4Vectors::setListElement(x, i, value)
##  5.     └─S4Vectors:::.replace_list_element(x, i2, value)
##  6.       └─S4Vectors:::.wrap_in_length_one_list_like_object(...)
##
## https://stat.ethz.ch/pipermail/bioc-devel/2017-December/012465.html
##
## nolint end

gsea@listData[["h"]] <- gsea[["h"]]["dmso_r1881_vs_etoh"]

## validObject(gsea)
## Error in validObject(gsea) :
##   invalid class "FGSEAList" object: identical(names(object), names(metadata(object)[["gmtFiles"]])) is not TRUE.
##
## identical(names(object[[1L]]), names(metadata(object)[["rankedList"]])) is not TRUE.

## Fix gmtFiles metadata.
metadata(gsea)[["gmtFiles"]] <- metadata(gsea)[["gmtFiles"]]["h"]

## validObject(gsea)
## Error in validObject(gsea) :
##   invalid class "FGSEAList" object: identical(names(object[[1L]]), names(metadata(object)[["rankedList"]])) is not TRUE.

metadata(gsea)[["rankedList"]] <-
    metadata(gsea)[["rankedList"]]["dmso_r1881_vs_etoh"]

## Convert the example gmtFile path to relative path, so we can run on CI.
metadata(gsea)[["gmtFiles"]][["h"]] <-
    sub(
        pattern = "/home/[a-z.]+/",
        replacement = "~/",
        x = metadata(gsea)[["gmtFiles"]][["h"]]
    )

validObject(gsea)

object_size(gsea)
## 2.66 MB

use_data(gsea)
