#' @name contrastNames
#' @inherit AcidGenerics::contrastNames
#' @note Updated 2022-04-27.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(fgsea)
#'
#' ## FgseaList ====
#' object <- fgsea
#' contrastNames(object)
NULL



## Updated 2019-07-24.
`contrastNames,FgseaList` <- # nolint
    function(object) {
        names(object[[1L]])
    }



## Updated 2019-07-24.
`contrastNames<-,FgseaList,character` <- # nolint
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
    f = "contrastNames",
    signature = signature(object = "FgseaList"),
    definition = `contrastNames,FgseaList`
)

#' @rdname contrastNames
#' @export
setMethod(
    f = "contrastNames<-",
    signature = signature(
        object = "FgseaList",
        value = "character"
    ),
    definition = `contrastNames<-,FgseaList,character`
)
