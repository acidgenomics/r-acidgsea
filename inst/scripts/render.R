# Render multiple GSEA reports.
# Modified 2019-06-26.

library(rmarkdown)
templateFile <- "pfgsea.Rmd"

# Load the FGSEAList objects.
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
        message(paste0(
            "Rendering ", name, "\n",
            "File: ", file
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
