## Updated 2021-03-04.
.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



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
#' AcidGseaTestsUrl
AcidGseaTestsUrl <- # nolint
    "https://r.acidgenomics.com/testdata/acidgsea"
