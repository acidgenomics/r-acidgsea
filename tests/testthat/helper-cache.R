dir.create("cache", showWarnings = FALSE)
files <- "mm_deseq.rds"
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
        remoteDir = pfgseaTestsURL,
        envir = environment()
    )
)
