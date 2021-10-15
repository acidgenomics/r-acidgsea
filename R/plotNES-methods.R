#' @name plotNES
#' @inherit AcidGenerics::plotNES
#' @note Updated 2021-09-24.
#'
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @seealso Inspired by example in Stephen Turner's
#' [DESeq to fgsea guide](https://stephenturner.github.io/deseq-to-fgsea/).
#'
#' @examples
#' ## FGSEAList ====
#' data(fgsea)
#' plotNES(
#'     object = fgsea,
#'     contrast = "condition_B_vs_A",
#'     collection = "h"
#' )
NULL



## Updated 2021-09-24.
`plotNES,FGSEAList` <-  # nolint
    function(
        object,
        contrast,
        collection,
        flip = getOption(
            x = "acid.flip",
            default = TRUE
        )
    ) {
        validObject(object)
        assert(
            isString(contrast),
            isString(collection),
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
        p <- p + autoDiscreteFillScale()
        ## Flip.
        if (isTRUE(flip)) {
            p <- p + coord_flip()
        }
        p
    }



#' @rdname plotNES
#' @export
setMethod(
    f = "plotNES",
    signature = signature(object = "FGSEAList"),
    definition = `plotNES,FGSEAList`
)
