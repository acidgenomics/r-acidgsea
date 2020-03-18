## Render multiple fast GSEA reports.
## Updated 2020-03-18.

library(rmarkdown)

templateFile <- "fgsea.Rmd"
stopifnot(file.exists(templateFile))

## Load the FGSEAList objects.
datasets <- c(
    name1 = "fgsealist1",
    name2 = "fgsealist2"
)
objectFiles <- file.path(
    "rds",
    Sys.Date(),
    paste0(datasets, ".rds")
)
names(objectFiles) <- names(datasets)
stopifnot(all(file.exists(objectFiles)))

outputDir <- file.path("results", Sys.Date(), "fgsea")

invisible(mapply(
    name = names(objectFiles),
    file = objectFiles,
    FUN = function(name, file) {
        message(sprintf(
            "Rendering '%s'\nFile: %s",
            name, file
        ))
        render(
            params = list(
                title = paste("GSEA:", name),
                object = file,
                name = name,
                output_dir = outputDir
            ),
            input = templateFile,
            output_format = "html_document",
            output_file = paste0(name, ".html"),
            output_dir = outputDir,
            clean = TRUE,
            envir = new.env()
        )
    },
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
))
