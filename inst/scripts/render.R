# Render multiple GSEA reports.
# Last modified 2019-06-12.

library(rmarkdown)

datasets <- c("object1", "object2")
objectFiles <- file.path(
    "data",
    "YYYY-MM-DD",
    paste0(datasets, ".rds")
)
names(objectFiles) <- datasets
stopifnot(all(file.exists(objectFiles)))

outputDir <- file.path("results", Sys.Date(), "gsea")

invisible(mapply(
    name = names(objectFiles),
    file = objectFiles,
    FUN = function(name, file) {
        message(paste0(
            "Rendering ", name, "\n",
            "File: ", file
        ))
        render(
            params = list(
                title = paste("GSEA:", name),
                object_file = file,
                output_dir = outputDir
            ),
            input = "gsea.Rmd",
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

