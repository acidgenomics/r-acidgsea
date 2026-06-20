#' AcidGSEA
#'
#' Perform parameterized fast gene set enrichment analysis (GSEA) on multiple
#' differential expression contrasts.
#'
#' @aliases NULL
#' @keywords internal
#'
"_PACKAGE"


## S4 classes ==================================================================

#' @importClassesFrom DESeqAnalysis DESeqAnalysis
NULL


# S4 generics and methods ======================================================

#' @importFrom AcidGenerics GeneToSymbol alphaThreshold alphaThreshold<-
#' @importFrom AcidGenerics as.DESeqDataSet as.DataFrame camelCase
#' @importFrom AcidGenerics collectionNames collectionNames<-
#' @importFrom AcidGenerics contrastName contrastNames contrastNames<-
#' @importFrom AcidGenerics contrastSamples convertToHuman enrichedGeneSets
#' @importFrom AcidGenerics export geneSet geneSetNames geneSetResults import
#' @importFrom AcidGenerics leadingEdge leftJoin makeNames mapGenesToRownames
#' @importFrom AcidGenerics markdownTables melt nesThreshold nesThreshold<-
#' @importFrom AcidGenerics plotEnrichedGeneSets plotEnrichedUpset
#' @importFrom AcidGenerics plotGeneSet plotHeatmap plotLfc plotNes plotUpset
#' @importFrom AcidGenerics results selectIf showHeader snakeCase
#' @importFrom AcidGenerics stripGeneVersions
#' @importFrom BiocGenerics %in% combine lapply order organism sort unique
#' @importFrom BiocGenerics unlist unsplit updateObject
#' @importFrom GenomeInfoDb genome seqnames
#' @importFrom S4Vectors complete.cases decode head mcols mcols<- metadata
#' @importFrom S4Vectors metadata<- na.omit split tail
#' @importFrom SummarizedExperiment rowRanges rowRanges<-
#' @importFrom fgsea calcGseaStat fgsea
#' @importFrom methods coerce show
NULL

#' @importMethodsFrom AcidBase showHeader
#' @importMethodsFrom AcidExperiment GeneToSymbol export mapGenesToRownames
#' @importMethodsFrom AcidGenomes stripGeneVersions
#' @importMethodsFrom AcidPlots plotHeatmap plotUpset
#' @importMethodsFrom AcidPlyr leftJoin melt selectIf
#' @importMethodsFrom DESeqAnalysis as.DESeqDataSet coerce contrastName
#' @importMethodsFrom DESeqAnalysis contrastSamples plotCounts results
#' @importMethodsFrom pipette as.DataFrame export import
#' @importMethodsFrom syntactic camelCase makeNames snakeCase
NULL


## S3 generics =================================================================

#' @importFrom stats reorder
NULL


## Standard functions ==========================================================

#' @importFrom AcidBase basenameSansExt initDir methodFunction realpath
#' @importFrom AcidBase showSlotInfo standardizeCall
#' @importFrom AcidCLI abort alert alertInfo alertWarning dl toInlineString
#' @importFrom AcidCLI txt ul
#' @importFrom AcidGenomes mapHumanOrthologs
#' @importFrom AcidMarkdown markdownHeader
#' @importFrom AcidPlots .data acid_theme_light acid_scale_color_discrete
#' @importFrom AcidPlots acid_scale_fill_discrete matchLabels
#' @importFrom DESeqAnalysis DESeqAnalysis
#' @importFrom IRanges IntegerList SplitDataFrameList
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom ggplot2 aes coord_flip geom_boxplot geom_col geom_hline
#' @importFrom ggplot2 geom_jitter geom_line geom_point geom_segment
#' @importFrom ggplot2 geom_violin ggplot labs scale_alpha_identity
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom goalie allAreAtomic allAreFiles allAreMatchingRegex
#' @importFrom goalie areDisjointSets areIntersectingSets areSameLength
#' @importFrom goalie areSetEqual assert bapply hasColnames hasDuplicates
#' @importFrom goalie hasLength hasNames hasNoDuplicates hasRownames hasRows
#' @importFrom goalie isADir isAFile isAll isAlpha isAny isCharacter isFile
#' @importFrom goalie isFlag isHeaderLevel isInRange isInstalled isInt
#' @importFrom goalie isMatchingRegex isNumber isNonNegative isOrganism
#' @importFrom goalie isScalar isScalarInteger isString isSubset
#' @importFrom goalie requireNamespaces validate
#' @importFrom methods as is new setAs setGeneric setMethod setValidity slot
#' @importFrom methods slot<- validObject
#' @importFrom parallel mclapply
#' @importFrom utils packageName packageVersion sessionInfo
NULL
