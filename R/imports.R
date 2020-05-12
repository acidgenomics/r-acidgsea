#' @importClassesFrom DESeqAnalysis DESeqAnalysis
#' @importMethodsFrom DESeqAnalysis coerce plotCounts
#'
#' @importFrom BiocParallel bplapply bpparam
#' @importFrom DESeqAnalysis DESeqAnalysis contrastName contrastSamples
#' @importFrom IRanges SplitDataFrameList unsplit
#' @importFrom S4Vectors DataFrame SimpleList complete.cases head lapply mcols
#'   mcols<- metadata metadata<- na.omit order split tail unique
#' @importFrom SummarizedExperiment rowRanges rowRanges<-
#' @importFrom UpSetR fromList
#' @importFrom acidplots acid_theme_light plotUpset
#' @importFrom basejump Gene2Symbol alphaThreshold as_tibble export import
#'   initDir leftJoin makeNames mapGenesToRownames markdownHeader
#'   matchArgsToDoCall matchHumanOrthologs selectIf showHeader showSlotInfo
#'   standardizeCall
#' @importFrom cli cli_alert cli_alert_info cli_alert_warning cli_dl cli_text
#'   cli_ul
#' @importFrom fgsea fgsea gmtPathways plotEnrichment plotGseaTable
#' @importFrom ggplot2 labs
#' @importFrom goalie allAreAtomic areDisjointSets areSameLength assert bapply
#'   hasColnames hasLength hasNames hasNoDuplicates hasRownames hasRows isAFile
#'   isAll isAlpha isAny isCharacter isFile isFlag isHeaderLevel isInt isNumber
#'   isScalar isScalarInteger isString isSubset validate
#' @importFrom knitr kable
#' @importFrom methods as is new setAs setGeneric setMethod setValidity show
#'   slot slot<- validObject
#' @importFrom sessioninfo session_info
#' @importFrom utils globalVariables
NULL
