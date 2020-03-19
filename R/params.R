#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param DESeqAnalysis `DESeqAnalysis`.
#'   Corresponding DESeq2 data used to perform GSEA.
#' @param alpha `numeric(1)`.
#'   Alpha level.
#' @param collection `character(1)` or `integer(1)`.
#'   Gene set collection name or position, corresponding to values defined in
#'   [`names()`][base::names].
#'   Typically refers to `h` (hallmark), `c1`-`c7` collections from MSigDb.
#'   Can obtain using `collectionNames()` on `FGSEAList` object.
#' @param contrast `character(1)` or `integer(1)`.
#'   DESeqResults contrast.
#' @param contrastSamples `logical(1)`.
#'   Only visualize the samples defined in the contrast.
#' @param gmtFile `character(1)`.
#'   MSigDB GMT file path.
#' @param gmtFiles `character`.
#'   MSigDB GMT file paths.
#' @param headerLevel `integer(1)`.
#'   Markdown header level.
#' @param leadingEdge `logical(1)`.
#'   Visualize only the leading edge genes returned by GSEA. If `FALSE`, plot
#'   all genes in the gene set, which requires the external gene set file that
#'   was originally used to run the analysis. This file is referenced internally
#'   inside the object at `metadata(object)[["gmtFiles"]]`.
#' @param n `integer(1)`.
#'   Number of significant processes to include.
#'   Bidirectional; will include `n` up- and down-regulated processes.
#' @param nesThreshold `numeric(1)` or `NULL`.
#'   NES cutoff threshold.
#'   If left `NULL`, no cutoff will be applied.
#' @param object Object.
#' @param pathways `character`.
#'   Pathways.
#' @param results `data.frame`/`data.table`.
#'   Unmodified `fgsea::fgsea` return.
#' @param resultsList `list`.
#'   Results list.
#' @param set `character(1)`.
#'   Gene set name, in a defined `collection`.
#'   For example, `"HALLMARK_ADIPOGENESIS"`.
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
#' @param value Value to assign.
#' @param ... Additional arguments.
NULL
