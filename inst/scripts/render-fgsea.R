## Render multiple fast GSEA reports.
## Updated 2022-05-24.
## nolint start
suppressPackageStartupMessages({
    library(rmarkdown)
})
## nolint end
templateFile <- "fgsea.Rmd"
stopifnot(file.exists(templateFile))
## Load the FGSEAList objects.
datasets <- c(
    "name1" = "fgsealist1",
    "name2" = "fgsealist2"
)
objectFiles <- file.path(
    "rds",
    Sys.Date(),
    paste0(datasets, ".rds")
)
names(objectFiles) <- names(datasets)
stopifnot(all(file.exists(objectFiles)))
outputDir <- file.path("results", Sys.Date(), "fgsea")
invisible(Map(
    name = names(objectFiles),
    file = objectFiles,
    f = function(name, file) {
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
    }
))
