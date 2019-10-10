## Render multiple GSEA reports.
## Updated 2019-08-28.

library(rmarkdown)

templateFile <- "pfgsea.Rmd"
stopifnot(file.exists(templateFile))

## Load the FGSEAList objects.
datasets <- c("pbmc_pfgsea", "bm_pfgsea")
objectFiles <- file.path(
    "rds",
    Sys.Date(),
    paste0(datasets, ".rds")
)
names(objectFiles) <- datasets
stopifnot(all(file.exists(objectFiles)))

outputDir <- file.path("results", Sys.Date(), "gsea")

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
                fgsealist_file = file,
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
