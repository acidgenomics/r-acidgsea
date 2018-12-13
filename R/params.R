#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param alpha `scalar numeric`.
#'   Alpha level.
#' @param gmtFile `string`.
#'   MSigDB GMT file path.
#' @param gmtFiles `character`.
#'   MSigDB GMT file paths.
#' @param headerLevel `scalar integer`.
#'   Markdown header level.
#' @param n `scalar integer`.
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
#' @param ... Additional arguments.
NULL
