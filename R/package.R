#' pfgsea
#'
#' Perform parameterized gene set enrichment analysis on multiple differential
#' expression contrasts.
#'
#' @aliases NULL
#'
#' @importFrom basejump as_tibble markdownHeader theme_paperwhite
#' @importFrom dplyr arrange desc filter select
#' @importFrom fgsea fgsea gmtPathways plotEnrichment plotGseaTable
#' @importFrom ggplot2 labs
#' @importFrom goalie assert containsAlpha containsHeaderLevel hasNames isAFile
#'   isFile
#' @importFrom knitr kable
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! sym syms
#' @importFrom utils head tail
"_PACKAGE"
