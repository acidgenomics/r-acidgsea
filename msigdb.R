file.copy(
    from = file.path("inst", "extdata", "msigdb"),
    to = "~",
    overwrite = FALSE,
    recursive = TRUE
)
stopifnot(file.exists(
    file.path(
        "~",
        "msigdb",
        "msigdb_v6.2_GMTs",
        "h.all.v6.2.symbols.gmt"
    )
))
