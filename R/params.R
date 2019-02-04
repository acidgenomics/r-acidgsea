#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param alpha `numeric(1)`.
#'   Alpha level.
#' @param geneSet `character(1)`.
#'   Gene set name. Can obtain using `names()` on `FGSEAList` object.
#' @param gmtFile `character(1)`.
#'   MSigDB GMT file path.
#' @param gmtFiles `character`.
#'   MSigDB GMT file paths.
#' @param headerLevel `integer(1)`.
#'   Markdown header level.
#' @param n `integer(1)`.
#'   Number of significant processes to include.
#'   Bidirectional; will include `n` up- and down-regulated processes.
#' @param object Object.
#' @param pathways `character`.
#'   Pathways.
#' @param results `data.frame`/`data.table`.
#'   Unmodified `fgsea::fgsea` return.
#' @param resultsList `list`.
#'   Results list.
#' @param stats `numeric`.
#'   Gene stats. Test statistic is recommended by default, but shrunken log2
#'   fold changes are also acceptable. Don't input unshrunken LFC values without
#'   standard error correction.
#' @param statsList `list`.
#'   Gene stats list.
#' @param statsList `list`.
#'   Gene stats list.
#' @param theme `theme`/`gg`.
#'   ggplot2 theme.
#' @param ... Additional arguments.
NULL
