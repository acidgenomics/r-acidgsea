if (!isTRUE(goalie::hasInternet())) {
    warning("No Internet connection detected.")
    return(invisible(NULL))
}
dir.create("cache", showWarnings = FALSE)
files <- c(
    "h.all.v7.0.symbols.gmt",
    "mm_deseq.rds"
)
mapply(
    FUN = function(remoteDir, file, envir) {
        destfile <- file.path("cache", file)
        if (!file.exists(destfile)) {
            utils::download.file(
                url = paste(remoteDir, file, sep = "/"),
                destfile = destfile
            )
        }
    },
    file = files,
    MoreArgs = list(
        remoteDir = AcidGSEATestsURL,
        envir = environment()
    )
)
if (!isTRUE(dir.exists(file.path("~", "msigdb")))) {
    message(sprintf("Copying test data to '%s'.", "~/msigdb"))
    file.copy(
        from = normalizePath(
            file.path("..", "..", "inst", "extdata", "msigdb")
        ),
        to = "~",
        overwrite = FALSE,
        recursive = TRUE
    )
    stopifnot(file.exists(
        file.path(
            "~",
            "msigdb",
            "7.0",
            "msigdb_v7.0_GMTs",
            "h.all.v7.0.symbols.gmt"
        )
    ))
}
