library(rmarkdown)

datasets <- c("object1", "object2")
object_files <- file.path(
    "data",
    "YYYY-MM-DD",
    paste0(datasets, ".rds")
)
names(object_files) <- datasets
stopifnot(all(file.exists(object_files)))

output_dir <- file.path("results", Sys.Date(), "gsea")

invisible(mapply(
    name = names(object_files),
    file = object_files,
    FUN = function(name, file) {
        message(paste0(
            "Rendering ", name, "\n",
            "File: ", file
        ))
        render(
            params = list(
                title = paste("GSEA:", name),
                object_file = file,
                output_dir = output_dir
            ),
            input = "gsea.Rmd",
            output_format = "html_document",
            output_file = paste0(name, ".html"),
            output_dir = output_dir,
            clean = TRUE,
            envir = new.env()
        )
    },
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
))

