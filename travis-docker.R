# Copy example MSigDb files to `$HOME`.
file.copy(
    from = system.file(
        "extdata", "msigdb",
        package = "pfgsea",
        mustWork = TRUE
    ),
    to = "~",
    overwrite = FALSE,
    recursive = TRUE
)

rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")
BiocCheck::BiocCheck(`quit-with-status` = TRUE)
lintr::lint_package()
