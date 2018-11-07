# Refer to rlang/dplyr documentation on tidyeval/quasiquotation usage here.



#' Top Table
#' @name topTable
#' @inheritParams params
#' @return `data.frame`.
NULL



#' @describeIn topTable Return a top table of the most postiively associated
#'   processes, arranged by `NES`. Must pass the `alpha` level cutoff.
#' @export
topTable <- function(
    results,
    alpha,
    n = 10L
) {
    assert_is_data.frame(results)
    assertIsAlpha(alpha)
    assert_is_a_number(n)
    cols <- syms(setdiff(colnames(results), c("leadingEdge", "nMoreExtreme")))
    results %>%
        as_tibble() %>%
        arrange(desc(!!sym("NES")), !!sym("padj")) %>%
        filter(!!sym("padj") < !!alpha) %>%
        head(n = n) %>%
        select(!!!cols)
}



#' @describeIn topTable Parameterized. Supports looping across multiple DEG
#'   results, and adds a Markdown header for each contrast.
#' @export
topTables <- function(
    resultsList,
    alpha,
    n = 10L,
    headerLevel = 3L
) {
    assert_is_list(resultsList)
    assertIsAlpha(alpha)
    assert_is_a_number(n)
    assertIsHeaderLevel(headerLevel)
    invisible(mapply(
        text = names(resultsList),
        results = resultsList,
        FUN = function(text, results) {
            markdownHeader(text = text, level = headerLevel, asis = TRUE)
            print(kable(
                x = topTable(results = results, alpha = alpha, n = n),
                digits = 3L
            ))
        },
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    ))
}
