#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param DESeqAnalysis `DESeqAnalysis`.
#'   Corresponding DESeq2 data used to perform GSEA.
#' @param collection `character(1)`.
#'   Gene set collection name.
#'   Typically refers to `h` (hallmark), `c1`-`c7` collections from MSigDb.
#'   Can obtain using `collectionNames()` on `FGSEAList` object.
#' @param contrast `character(1)`.
#'   Contrast name.
#' @param contrastSamples `logical(1)`.
#'   Only visualize the samples defined in the contrast.
#' @param geneSetFiles `character`.
#'   Gene set file paths (i.e. GMT files).
#'   MSigDB files are recommended by default.
#' @param headerLevel `integer(1)`.
#'   Markdown header level.
#' @param keyType `character(1).
#'   Gene identifier format:
#'   - `"entrezId"`: Entrez identifiers (e.g. `7157`).
#'   - `"geneName"`: Gene names (a.k.a. symbols; e.g. `"TP53"`).
#' @param leadingEdge `logical(1)`.
#'   Visualize only the leading edge genes returned by GSEA.
#'   If `FALSE`, plot all genes in the gene set.
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
#' @param value `character(1)`.
#'   Value type to use for GSEA ranked list.
#'
#'   Currently supported:
#'
#'   1. `stat`: Wald test statistic. This column is returned by `results()`
#'      but is removed in `DESeq2::lfcShrink()` return, currently.
#'   2. `log2FoldChange`: Shrunken log2 fold change. Note that this option
#'      requires `DESeq2::lfcShrink()` return to be slotted.
#'   3. `padj`: Adjusted *P* value. This don't provide directional ranks, but
#'      is offered as a legacy option. Not generally recommended.
#' @param ... Additional arguments.
NULL
