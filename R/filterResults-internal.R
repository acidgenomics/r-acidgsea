.filterResults <- function(data, alpha) {
    assert(
        is(data, "data.table"),
        isAlpha(alpha)
    )
    data %>%
        as_tibble() %>%
        filter(!!sym("padj") < !!alpha) %>%
        arrange(!!sym("padj"), desc(!!sym("NES"))) %>%
        # Prioritize the first columns.
        select(!!!syms(c("pathway", "padj", "NES")), everything())
}
