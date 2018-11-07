#' pfgsea
#'
#' Perform parameterized gene set enrichment analysis on multiple differential
#' expression contrasts.
#'
#' @aliases NULL
#'
#' @importFrom assertive.base assert_are_identical
#' @importFrom assertive.files assert_all_are_existing_files
#' @importFrom assertive.properties assert_has_names
#' @importFrom assertive.types assert_is_a_number assert_is_a_string
#'   assert_is_character assert_is_data.frame assert_is_list assert_is_numeric
#' @importFrom basejump assertIsAlpha assertIsHeaderLevel markdownHeader
#'   theme_paperwhite
#' @importFrom dplyr arrange desc filter select
#' @importFrom fgsea fgsea gmtPathways plotEnrichment plotGseaTable
#' @importFrom ggplot2 labs
#' @importFrom knitr kable
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! sym syms
#' @importFrom tibble as_tibble
#' @importFrom utils head tail
"_PACKAGE"
