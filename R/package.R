#' AcidGSEA
#'
#' Perform parameterized fast gene set enrichment analysis (GSEA) on multiple
#' differential expression contrasts.
#'
#' @aliases NULL
#' @keywords internal
#'
#' @importClassesFrom DESeqAnalysis DESeqAnalysis
#' @importMethodsFrom DESeqAnalysis coerce plotCounts
#'
#' @importFrom AcidPlots !! acid_theme_light autoDiscreteColorScale
#'   autoDiscreteFillScale plotUpset matchLabels sym
#' @importFrom DESeqAnalysis DESeqAnalysis contrastName contrastSamples
#' @importFrom basejump DataFrame Gene2Symbol IntegerList SimpleList
#'   SplitDataFrameList abort alert alertInfo alertWarning as_tibble camelCase
#'   complete.cases dl export head import initDir lapply leftJoin makeNames
#'   mapGenesToRownames mapHumanOrthologs markdownHeader mcols mcols<- melt
#'   metadata metadata<- methodFunction na.omit order packageName packageVersion
#'   realpath requireNamespaces reorder rowRanges rowRanges<- selectIf
#'   session_info showHeader showSlotInfo split standardizeCall tail
#'   toInlineString txt ul unique unlist unsplit
#' @importFrom fgsea calcGseaStat fgsea
#' @importFrom ggplot2 aes coord_flip geom_boxplot geom_col geom_hline
#'   geom_jitter geom_line geom_point geom_segment geom_violin ggplot labs
#' @importFrom goalie allAreAtomic allAreFiles areDisjointSets areSameLength
#'   areSetEqual assert bapply hasColnames hasLength hasNames hasNoDuplicates
#'   hasRownames hasRows isAFile isAll isAlpha isAny isCharacter isFile isFlag
#'   isHeaderLevel isInstalled isInt isNumber isNonNegative isScalarInteger
#'   isString isSubset validate
#' @importFrom methods as is new setAs setGeneric setMethod setValidity show
#'   slot slot<- validObject
"_PACKAGE"



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
#' @param value Value to assign.
#' @param ... Additional arguments.
NULL
