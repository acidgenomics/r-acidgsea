#' Pathway names
#'
#' @name pathwayNames
#' @inheritParams params
#'
#' @examples
#' data(gsea)
#' pathwayNames(gsea)
NULL



pathwayNames.FGSEAList <-  # nolint
    function(object) {
        names(object)
    }



#' @rdname pathwayNames
#' @export
setMethod(
    f = "pathwayNames",
    signature = signature("FGSEAList"),
    definition = pathwayNames.FGSEAList
)



`pathwayNames<-.FGSEAList,character` <-  # nolint
    function(object, value) {
        assert(
            isCharacter(value),
            areSameLength(names(object), value)
        )
        names(object) <- value
        names(metadata(object)[["gmtFiles"]]) <- value
        validObject(object)
        object
    }



#' @rdname pathwayNames
#' @export
setMethod(
    f = "pathwayNames<-",
    signature = signature(
        object = "FGSEAList",
        value = "character"
    ),
    definition = `pathwayNames<-.FGSEAList,character`
)
