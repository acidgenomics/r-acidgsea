globalVariables(".")



.version <- packageVersion(packageName())

#' acidgsea test data URL
#' @keywords internal
#' @export
#' @examples
#' acidgseaTestsURL
acidgseaTestsURL <- paste0(
    "https://tests.acidgenomics.com/acidgsea/",
    "v", .version$major, ".", .version$minor  # nolint
)
