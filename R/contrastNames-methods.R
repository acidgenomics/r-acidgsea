#' @name contrastNames
#' @inherit bioverbs::contrastNames
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(gsea)
#' contrastNames(gsea)
NULL



#' @rdname contrastNames
#' @name contrastNames
#' @importFrom bioverbs contrastNames
#' @usage contrastNames(object, ...)
#' @export
NULL

#' @rdname contrastNames
#' @name contrastNames<-
#' @importFrom bioverbs contrastNames<-
#' @usage contrastNames(object, ...) <- value
#' @export
NULL




contrastNames.FGSEAList <-  # nolint
    function(object) {
        names(object[[1L]])
    }



#' @rdname contrastNames
#' @export
setMethod(
    f = "contrastNames",
    signature = signature("FGSEAList"),
    definition = contrastNames.FGSEAList
)



`contrastNames<-.FGSEAList,character` <-  # nolint
    function(object, value) {
        ## Loop across the pathway list and update the contrast names.
        listData <- slot(object, "listData")
        assert(
            isCharacter(value),
            areSameLength(names(listData[[1L]]), value)
        )
        listData <- lapply(X = listData, FUN = `names<-`, value = value)
        slot(object, "listData") <- listData
        ## Ensure the ranked list names stored in `metadata()` are updated.
        names(metadata(object)[["rankedList"]]) <- value
        validObject(object)
        object
    }



#' @rdname contrastNames
#' @export
setMethod(
    f = "contrastNames<-",
    signature = signature(
        object = "FGSEAList",
        value = "character"
    ),
    definition = `contrastNames<-.FGSEAList,character`
)
