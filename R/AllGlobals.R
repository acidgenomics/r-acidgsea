## Updated 2021-03-04.
.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



## FIXME Take this out
## Updated 2021-02-12.
.bplapply <- {
    if (isInstalled("BiocParallel")) {
        x <- quote(BiocParallel::bplapply)
    } else {
        x <- quote(lapply)
    }
    x
}



#' Supported key types for RankedList
#'
#' @note Updated 2023-03-01.
#' @noRd
.keyType <- quote(c(
    "geneName",
    "ensemblGeneId",
    "ncbiGeneId"
))



#' AcidGSEA test data URL
#'
#' @keywords internal
#' @export
#'
#' @examples
#' AcidGSEATestsURL
AcidGSEATestsURL <- # nolint
    paste0(
        "https://r.acidgenomics.com/testdata/acidgsea/",
        "v", .pkgVersion$major, ".", .pkgVersion$minor # nolint
    )
