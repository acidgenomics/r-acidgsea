#' Extract and/or slot `DESeqAnalysis` object.
#'
#' @name DESeqAnalysis
#' @note Updated 2020-09-23.
#'
#' @examples
#' data(fgsea)
#' deseq <- DESeqAnalysis(fgsea)
#' class(deseq)
NULL



## Updated 2020-09-23.
`DESeqAnalysis,FGSEAList` <-  # nolint
    function(object) {
        validObject(object)
        deseq <- metadata(object)[["deseq"]]
        if (!is(deseq, "DESeqAnalysis")) {
            stop(paste(
                "FGSEAList does not contain DESeqAnalysis.",
                "Slot using 'DESeqAnalysis<-' function."
            ))
        }
        validObject(deseq)
        deseq
    }



#' @rdname DESeqAnalysis
#' @export
setMethod(
    f = "DESeqAnalysis",
    signature = signature("FGSEAList"),
    definition = `DESeqAnalysis,FGSEAList`
)



## Updated 2020-09-23.
`DESeqAnalysis<-,FGSEAList,DESeqAnalysis` <-  # nolint
    function(object, value) {
        metadata(object)[["deseq"]] <- value
        object
    }



#' @rdname DESeqAnalysis
#' @export
setReplaceMethod(
    f = "DESeqAnalysis",
    signature = signature(
        object = "FGSEAList",
        value = "DESeqAnalysis"
    ),
    definition = `DESeqAnalysis<-,FGSEAList,DESeqAnalysis`
)
