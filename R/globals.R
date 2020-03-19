globalVariables(".")



.version <- packageVersion("acidgsea")

#' acidgsea test data URL
#' @keywords internal
#' @export
#' @examples
#' acidgseaTestsURL
acidgseaTestsURL <- paste0(
    "http://tests.acidgenomics.com/acidgsea/",
    "v", .version$major, ".", .version$minor  # nolint
)
