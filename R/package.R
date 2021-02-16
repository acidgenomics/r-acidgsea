#' AcidGSEA
#'
#' Perform parameterized fast gene set enrichment analysis (GSEA) on multiple
#' differential expression contrasts.
#'
#' @aliases NULL
#' @keywords internal

#' @importClassesFrom DESeqAnalysis DESeqAnalysis
#' @importMethodsFrom DESeqAnalysis coerce plotCounts
#'
#' @importFrom AcidPlots acid_theme_light plotUpset
#' @importFrom BiocParallel bplapply bpparam
#' @importFrom DESeqAnalysis DESeqAnalysis contrastName contrastSamples
#' @importFrom IRanges SplitDataFrameList unsplit
#' @importFrom S4Vectors DataFrame SimpleList complete.cases head lapply mcols
#'   mcols<- metadata metadata<- na.omit order split tail unique
#' @importFrom SummarizedExperiment rowRanges rowRanges<-
#' @importFrom basejump Gene2Symbol aggregateRows alert alertInfo alertWarning
#'   as_tibble camelCase dl export formalsList import initDir leftJoin makeNames
#'   mapGenesToRownames markdownHeader matchArgsToDoCall matchHumanOrthologs
#'   melt selectIf showHeader showSlotInfo standardizeCall txt ul
#' @importFrom fgsea calcGseaStat fgsea
#' @importFrom ggplot2 aes coord_flip geom_boxplot geom_col geom_hline
#'   geom_jitter geom_line geom_point geom_segment geom_violin ggplot labs
#' @importFrom goalie allAreAtomic allAreFiles areDisjointSets areSameLength
#'   areSetEqual assert bapply hasColnames hasLength hasNames hasNoDuplicates
#'   hasRownames hasRows isAFile isAll isAlpha isAny isCharacter isFile isFlag
#'   isGGScale isHeaderLevel isInt isNumber isNonNegative isScalarInteger
#'   isString isSubset validate
#' @importFrom knitr kable
#' @importFrom methods as is new setAs setGeneric setMethod setValidity show
#'   slot slot<- validObject
#' @importFrom rlang !! sym
#' @importFrom sessioninfo session_info
#' @importFrom stats reorder
#' @importFrom utils packageName packageVersion
"_PACKAGE"
