## Updated 2020-09-23.
`DESeqAnalysis,FGSEAList` <-  # nolint
    function(object) {
        deseq <- metadata(object)[["deseq"]]
        if (!is(deseq, "DESeqAnalysis")) {
            stop(paste(
                "FGSEAList does not contain DESeqAnalysis.",
                "Update using 'updateObject' function."
            ))
        }
        deseq
    }



## Updated 2020-09-23.
`DESeqAnalysis<-,FGSEAList,DESeqAnalysis` <-  # nolint
    function(object, value) {
        metadata(object)[["deseq"]] <- value
        object
    }
