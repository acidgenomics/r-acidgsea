## S4 classes ==================================================================

#' @importClassesFrom DESeqAnalysis DESeqAnalysis
NULL



# S4 generics and methods ======================================================

#' @importFrom AcidGenerics Gene2Symbol alphaThreshold alphaThreshold<-
#' as.DESeqDataSet as.DataFrame camelCase collectionNames collectionNames<-
#' contrastName contrastNames contrastNames<- contrastSamples convertToHuman
#' enrichedGeneSets geneSet geneSetNames geneSetResults leadingEdge leftJoin
#' makeNames mapGenesToRownames markdownTables melt nesThreshold nesThreshold<-
#' plotEnrichedGeneSets plotEnrichedUpset plotGeneSet plotHeatmap plotLFC
#' plotNES plotUpset results selectIf showHeader snakeCase stripGeneVersions
#' @importFrom BiocGenerics %in% combine lapply order organism sort unique
#' unlist unsplit updateObject
#' @importFrom GenomeInfoDb genome seqnames
#' @importFrom S4Vectors complete.cases decode head mcols mcols<- metadata
#' metadata<- na.omit split tail
#' @importFrom SummarizedExperiment rowRanges rowRanges<-
#' @importFrom fgsea calcGseaStat fgsea
#' @importFrom methods coerce show
#' @importFrom pipette export import
#'
#' @importMethodsFrom AcidBase showHeader
#' @importMethodsFrom AcidExperiment Gene2Symbol export mapGenesToRownames
#' @importMethodsFrom AcidGenomes stripGeneVersions
#' @importMethodsFrom AcidPlots plotHeatmap plotUpset
#' @importMethodsFrom AcidPlyr leftJoin melt selectIf
#' @importMethodsFrom DESeqAnalysis as.DESeqDataSet coerce contrastName
#' contrastSamples plotCounts results
#' @importMethodsFrom pipette as.DataFrame export import
#' @importMethodsFrom syntactic camelCase makeNames snakeCase
NULL



## S3 generics =================================================================

#' @importFrom stats reorder
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase basenameSansExt initDir methodFunction realpath
#' requireNamespaces showSlotInfo standardizeCall
#' @importFrom AcidCLI abort alert alertInfo alertWarning dl toInlineString
#' txt ul
#' @importFrom AcidGenomes mapHumanOrthologs
#' @importFrom AcidMarkdown markdownHeader
#' @importFrom AcidPlots !! acid_theme_light autoDiscreteColorScale
#' autoDiscreteFillScale matchLabels sym
#' @importFrom DESeqAnalysis DESeqAnalysis
#' @importFrom IRanges IntegerList SplitDataFrameList
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom ggplot2 aes coord_flip geom_boxplot geom_col geom_hline
#' geom_jitter geom_line geom_point geom_segment geom_violin ggplot labs
#' scale_alpha_identity scale_shape_manual
#' @importFrom goalie allAreAtomic allAreFiles allAreMatchingRegex
#' areDisjointSets areIntersectingSets areSameLength areSetEqual assert bapply
#' hasColnames hasLength hasNames hasNoDuplicates hasRownames hasRows isADir
#' isAFile isAll isAlpha isAny isBiocParallelParam isCharacter isFile isFlag
#' isHeaderLevel isInRange isInstalled isInt isMatchingRegex isNumber
#' isNonNegative isOrganism isScalar isScalarInteger isString isSubset validate
#' @importFrom methods as is new setAs setGeneric setMethod setValidity slot
#' slot<- validObject
#' @importFrom utils packageName packageVersion sessionInfo
NULL
