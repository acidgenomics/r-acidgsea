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
#' @importFrom AcidPlots !! acid_theme_light plotUpset sym
#' @importFrom DESeqAnalysis DESeqAnalysis contrastName contrastSamples
#' @importFrom basejump DataFrame Gene2Symbol SimpleList SplitDataFrameList
#'   aggregateRows alert alertInfo alertWarning as_tibble camelCase
#'   complete.cases dl export formalsList head import initDir lapply leftJoin
#'   makeNames mapGenesToRownames mapHumanOrthologs markdownHeader mcols mcols<-
#'   melt metadata metadata<- na.omit order packageName packageVersion
#'   requireNamespaces reorder rowRanges rowRanges<- selectIf session_info
#'   showHeader showSlotInfo split standardizeCall tail txt ul unique unsplit
#' @importFrom fgsea calcGseaStat fgsea
#' @importFrom ggplot2 aes coord_flip geom_boxplot geom_col geom_hline
#'   geom_jitter geom_line geom_point geom_segment geom_violin ggplot labs
#' @importFrom goalie allAreAtomic allAreFiles areDisjointSets areSameLength
#'   areSetEqual assert bapply hasColnames hasLength hasNames hasNoDuplicates
#'   hasRownames hasRows isAFile isAll isAlpha isAny isCharacter isFile isFlag
#'   isGGScale isHeaderLevel isInstalled isInt isNumber isNonNegative
#'   isScalarInteger isString isSubset validate
#' @importFrom methods as is new setAs setGeneric setMethod setValidity show
#'   slot slot<- validObject
"_PACKAGE"
