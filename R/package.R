#' pfgsea
#'
#' Perform **p**arameterized **fast** **g**ene **s**et **e**nrichment
#' **a**nalysis on multiple differential expression contrasts.
#'
#' @aliases NULL
#' @keywords internal
#'
#' @importMethodsFrom DESeqAnalysis coerce
#'
#' @importFrom basejump Gene2Symbol as_tibble markdownHeader theme_paperwhite
#' @importFrom dplyr arrange desc distinct filter group_by left_join select
#'   summarize
#' @importFrom fgsea fgsea gmtPathways plotEnrichment plotGseaTable
#' @importFrom ggplot2 labs
#' @importFrom goalie assert containsAlpha containsHeaderLevel hasNames isAFile
#'   isCharacter isFile isInt
#' @importFrom knitr kable
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! := sym syms
#' @importFrom tibble deframe
#' @importFrom utils head tail
"_PACKAGE"
