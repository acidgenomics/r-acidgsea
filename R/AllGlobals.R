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



## Updated 2021-03-04.
.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



#' RankedList values
#'
#' @note Updated 2020-10-10.
#' @noRd
.rankedListValue <- c("stat", "log2FoldChange", "padj")



#' AcidGSEA test data URL
#'
#' @keywords internal
#' @export
#'
#' @examples
#' AcidGSEATestsURL
AcidGSEATestsURL <-  # nolint
    paste0(
        "https://r.acidgenomics.com/testdata/acidgsea/",
        "v", .pkgVersion$major, ".", .pkgVersion$minor  # nolint
    )
