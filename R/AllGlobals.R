## Updated 2021-03-04.
.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



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
#' @note Updated 2022-05-25.
#' @noRd
.keyType <- quote(c(
    "geneName",
    "geneId",
    "entrezId",
    "ensemblId",
    "rowname"
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
