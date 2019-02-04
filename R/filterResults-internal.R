.filterResults <- function(data, alpha) {
    assert(
        is(data, "data.table"),
        isAlpha(alpha)
    )
    data %>%
        as_tibble() %>%
        filter(!!sym("padj") < !!alpha) %>%
        arrange(desc(!!sym("NES")), !!sym("padj")) %>%
        # Prioritize the first columns.
        select(!!!syms(c("pathway", "NES", "padj")), everything())
}
