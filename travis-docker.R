file.copy(
    from = file.path("", "pfgsea", "inst", "extdata", "msigdb"),
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

rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")
BiocCheck::BiocCheck(`quit-with-status` = TRUE)

lintr::lint_package()
