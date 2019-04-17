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
