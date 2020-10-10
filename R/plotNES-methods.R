#' @name plotNES
#' @inherit AcidGenerics::plotNES
#' @note Updated 2020-10-01.
#'
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @seealso Inspired by example in Stephen Turner's
#' [DESeq to fgsea guide](https://stephenturner.github.io/deseq-to-fgsea/).
#'
#' @examples
#' data(fgsea)
#' plotNES(
#'     object = fgsea,
#'     contrast = "condition_B_vs_A",
#'     collection = "h"
#' )
NULL



#' @rdname plotNES
#' @name plotNES
#' @importFrom AcidGenerics plotNES
#' @usage plotNES(object, ...)
#' @export
NULL



## Updated 2020-09-22.
`plotNES,FGSEAList` <-  # nolint
    function(
        object,
        contrast,
        collection,
        fill,
        flip
    ) {
        validObject(object)
        assert(
            isString(contrast),
            isString(collection),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE),
            isFlag(flip)
        )
        data <- as_tibble(results(
            object = object,
            contrast = contrast,
            collection = collection
        ))
        alpha <- alphaThreshold(object)
        assert(isSubset(c("nes", "padj", "pathway"), colnames(data)))
        data[["sig"]] <- data[["padj"]] < alpha
        p <- ggplot(
            data = data,
            mapping = aes(
                x = reorder(!!sym("pathway"), !!sym("nes")),
                y = !!sym("nes")
            )) +
            geom_col(
                mapping = aes(fill = !!sym("sig"))
            ) +
            labs(
                x = "pathway",
                y = "normalized enrichment score",
                fill = paste0("padj < ", alpha),
                title = collection
            )
        ## Fill.
        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }
        ## Flip.
        if (isTRUE(flip)) {
            p <- p + coord_flip()
        }
        p
    }

formals(`plotNES,FGSEAList`)[c("fill", "flip")] <-
    formalsList[c("fill.discrete", "flip")]



#' @rdname plotNES
#' @export
setMethod(
    f = "plotNES",
    signature = signature("FGSEAList"),
    definition = `plotNES,FGSEAList`
)
