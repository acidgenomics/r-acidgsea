globalVariables(".")



.version <- packageVersion("pfgsea")

#' pfgsea test data URL
#' @keywords internal
#' @export
#' @examples
#' pfgseaTestsURL
pfgseaTestsURL <- paste0(
    "http://tests.acidgenomics.com/pfgsea/",
    "v", .version$major, ".", .version$minor  # nolint
)
