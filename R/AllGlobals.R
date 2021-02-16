## Updated 2021-02-12.
.bplapply <-
    {
        if (isInstalled("BiocParallel")) {
            x <- quote(BiocParallel::bplapply)
        } else {
            x <- quote(lapply)
        }
        x
    }



#' RankedList values
#'
#' @note Updated 2020-10-10.
#' @noRd
.rankedListValue <- c("stat", "log2FoldChange", "padj")



#' Package version
#'
#' @note Updated 2020-10-10.
#' @noRd
.version <- packageVersion(packageName())



#' AcidGSEA test data URL
#'
#' @keywords internal
#' @export
#'
#' @examples
#' AcidGSEATestsURL
AcidGSEATestsURL <-  # nolint
    paste0(
        "https://tests.acidgenomics.com/AcidGSEA/",
        "v", .version$major, ".", .version$minor  # nolint
    )
