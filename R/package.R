#' pfgsea
#'
#' Perform **p**arameterized **fast** **g**ene **s**et **e**nrichment
#' **a**nalysis on multiple differential expression contrasts.
#'
#' @aliases NULL
#' @keywords internal
#'
#' @importFrom basejump as_tibble markdownHeader theme_paperwhite
#' @importFrom dplyr arrange desc filter select
#' @importFrom fgsea fgsea gmtPathways plotEnrichment plotGseaTable
#' @importFrom ggplot2 labs
#' @importFrom goalie assert containsAlpha containsHeaderLevel hasNames isAFile
#'   isCharacter isFile isInt
#' @importFrom knitr kable
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! sym syms
#' @importFrom utils head tail
"_PACKAGE"
