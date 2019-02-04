.filterResults <- function(data, alpha) {
    assert(is(data, "data.table"), isAlpha(alpha))
    data <- data %>%
        as_tibble() %>%
        filter(!!sym("padj") < !!alpha) %>%
        arrange(!!sym("padj"), desc(!!sym("NES"))) %>%
        # Prioritize the first columns.
        select(!!!syms(c("pathway", "padj", "NES")), everything())
    if (!hasRows(data)) {
        message(paste("No significant pathways at alpha < ", alpha, "."))
    }
    data
}
