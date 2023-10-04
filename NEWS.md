# Release notes

## AcidGSEA 0.9.0 (2023-10-04)

Major changes:

- Now enforcing strict camel case for all function names.
- Renamed primary `FGSEAList` class to `FgseaList`.
- Removed BiocParallel in favor of parallel.

## AcidGSEA 0.8.9 (2023-08-15)

Minor changes:

- Now requiring R 4.3 / Bioconductor 3.17.
- Reworked internal ggplot2 code to use data pronouns instead of rlang
  syntactic sugar.
- Updated NAMESPACE related to AcidPlots.

## AcidGSEA 0.8.8 (2023-04-28)

Major changes:

- Reworked handling of NCBI (Entrez) gene identifiers.
- Now classing on `DFrame` instead of `DataFrame` virtual class, where
  applicable.
- Also classing on `GRanges` directly instead of `GenomicRanges` virtual class,
  where applicable.

## AcidGSEA 0.8.7 (2023-02-09)

Minor changes:

- Updated dependencies to Bioconductor 3.16.
- Migrated `requireNamespaces` import from AcidBase to goalie.

## AcidGSEA 0.8.6 (2022-10-25)

Major changes:

- Hardened package dependency requirements.
- `export`: Restricting methods, following conventions now defined in pipette
  0.10.0 update.
- Using `hasDuplicates` internally from goalie.

## AcidGSEA 0.8.5 (2022-08-31)

Major changes:

- `RankedList` Relaxed the internal thresholds for acceptable gene identifier
  mapping thresholds. Previous cutoffs were a bit too strict and could result
  in errors with acceptable datasets.
- `export`: Simplified S4 method to class with `con` and `format` arguments
  defined as required `character` and `missingOrNULL`, respectively. Improved
  the documentation to denote that user should specify a directory as `con`, and
  either `"csv"` or `"tsv"` as the desired output `format`.

Minor changes:

- Updated roxygen2 documentation.
- Resolved any Markdown lints using `markdownlint`.

## AcidGSEA 0.8.4 (2022-08-17)

Major changes:

- `FGSEAList`: Simplified input to use recommended defaults for fgsea
  passthrough. Note that fgsea now uses `fgseaMultilevel` by default in
  Bioconductor 3.15. Object class checks have been updated to reflect this
  breaking change.

Minor changes:

- `RankedList`: Fix for `ensemblId` handling as input.
- Resaved example `fgsea` object.

## AcidGSEA 0.8.3 (2022-06-09)

Minor changes:

- Improved support for Windows CI checks, using `tempdir2` and `unlink2`.

## AcidGSEA 0.8.2 (2022-05-26)

Major changes:

- `RankedList`: Improved `keyType` support, adding back `"geneId"`, and
  `"ensemblId"`. Simplified internal code for unlisting nested Entrez identifier
  keys and ensuring they map 1:1 by oldest identifier.
- `RankedList`: Function now looks inside `geneId` and checks against `provider`
  metadata defined in `rowRanges`, in the event that `ensemblId` or `ensemblId`
  `mcols` are not defined, but the key is in Ensembl or Entrez format.
- `RankedList`: In the event the user requests `keyType` of `"geneId"` or
  `"ensemblId"`, we now also call `stripGeneVersions` to sanitize the
  gene identifiers to remove gene versions.

## AcidGSEA 0.8.1 (2022-05-24)

Major changes:

- Reworked `topTables` as `markdownTables`, following updated conventions used
  in other Acid Genomics packages, such as AcidMarkdown and DESeqAnalysis.

Minor changes:

- Updated lintr checks and testthat unit tests.

## AcidGSEA 0.8.0 (2022-05-06)

Major changes:

- Package now requires R 4.2 / Bioconductor 3.15.
- Compatibility fixes for breaking changes in pipette package.

## AcidGSEA 0.7.0 (2022-04-27)

Major changes:

- Split out basejump dependencies into individual packages.
- Added support for multiple contrasts: `plotNES`, `plotGeneSet`, `results`.
- `export` method now confirms to BiocIO approach, using `con` argument.
- `RankedList`: Tightned up Ensembl reference genome filtering, checking for
  gene identifiers from primary chromosomes, to avoid unwanted averaging of
  values from haplotype scaffold gene identifiers. Also added support for
  `proteinCodingOnly`, to enable analysis of protein coding genes only, which
  is disabled by default.
- Reworked our internal Gene2Symbol handling for plot functions.

Minor changes:

- `FGSEAList` and `RankedList`: Reworked default formals for `keyType`
  and `value`.
- Added basejump as a dependency to R Markdown template.
- Relaxed gene-to-symbol class checks for `RankedList` S4 class.
- Improved rich text formatting for CLI messages, where applicable.
- Added an additional assert check for gene identifier overlap prior to
  internal `fgsea` handoff.
- `plotLFC`: Improved labeling of axes.
- `prepareGeneSetFiles`: Reworked file name detection for MSigDb.
- Improved internal code for returning results for all contrasts.

## AcidGSEA 0.6.4 (2021-07-21)

Minor changes:

- `RankedList`: Improved handling of duplicate Entrez gene identifiers, such
  as is the case in the bcbioRNASeq F1000 example dataset.
- Reorganized and consolidated S4 method exports.

## AcidGSEA 0.6.3 (2021-06-29)

Minor changes:

- `RankedList`: Improved column name sanitization and handling for input
  `DESeqResults` object, avoiding conflicts with `Gene2Symbol` object.
- Improved unit tests to no longer rely on gene sets saved in `~/msigdb`.

## AcidGSEA 0.6.2 (2021-03-16)

Minor changes:

- Updated basejump dependencies.
- Resaved example `fgsea` object (`FGSEAList`).
- Updated unit tests.

## AcidGSEA 0.6.1 (2021-03-04)

New functions:

- `prepareGeneSetFiles`: MSigDb utility function for easy matching of default
  GMT gene files to use for GSEA. Matches the symbol files by default.

Major changes:

- `RankedList` and `FGSEAList` method support has been improved for minimal
  `DESeqResults` and `DataFrame` objects. These approaches are intended
  primarily for analysis of files from collaborators where we don't necessarily
  have the complete set of DESeq2 analysis files, and therefore cannot
  construct a `DESeqAnalysis` object.

Minor changes:

- `RankedList` now supports `keyType`, which defaults to gene name (a.k.a.
  symbol), but now also supports original gene identifier. This alternative
  option is useful when working with the clusterProfiler package and is used
  in the bcbioRNASeq template.

## AcidGSEA 0.6.0 (2021-02-17)

Major changes:

- Reworked and simplified NAMESPACE and package dependencies, following
  basejump v0.14 release series update.
- Removed BiocParallel as a required import.

## AcidGSEA 0.5.0 (2020-10-10)

Major changes:

- Renamed package to AcidGSEA from acidgsea.

Minor changes:

- Migrated generics to AcidGenerics: `geneSetNames`, `plotLFC`, `plotNES`.

## acidgsea 0.4.0 (2020-09-26)

Major changes:

- `FGSEAList`: Reworked primary method to use `DESeqAnalysis` instead of
  `RankedList` as input. The `RankedList` object is now calculated automatically
  internally.
- Now slotting gene sets inside the `FGSEAList` object.
- `gmtFiles` argument has been renamed to `geneSetFiles`.

New functions:

- `geneSetResults`: Extract gene set expression values (from `DESeqResults`).
- `leadingEdge`: New accessor that returns leading edge genes from GSEA run.
- `results`: Extract GSEA results from object (from `fgsea` return).

## acidgsea 0.3.2 (2020-08-05)

Major changes:

- Simplified internal handling of alpha (now consistently referfed to as
  "alphaThreshold") and nesThreshold.
- Added `alphaThreshold` and `nesThreshold` S4 generic support.
- These name chanages better match the syntax used in DESeqAnalysis v0.3.

## acidgsea 0.3.1 (2020-07-24)

New functions:

- `plotGeneSet`: Split out internal gene set plotting code used previously in
  `plotEnrichedGeneSets`. This is a modified variant of `fgsea::plotEnrichment`
  that allows for additional color customization.

Major changes:

- Updated minimum R dependency to 4.0.
- `plotEnrichedGeneSets`: Now calls new `plotGeneSet` function internally.

Minor changes:

- Removed UpSetR dependency, based on update to AcidPlots package.
- Removed `plotCounts` method, in favor of `plotHeatmap` usage.

## acidgsea 0.3.0 (2020-05-24)

Major changes:

- `FGSEAList`: Converted function to an S4 generic. Previously the main arguemnt
  was named `rankedList`, but this has been renamed to `object`. A `RankedList`
  object can still be passed in as the first object in the function call. This
  change was necessary for adding GSEA support in pointillism single-cell
  RNA-seq analysis toolkit.

Minor changes:

- `RankedList`: Added support for `DataFrame` class, which is used inside
  pointillism package update.
- `RankedList`: Added `gene2symbol` argument support for `matrix` model.
  Using this for cluster matrix in pointillism for scRNA-seq using output
  from edgeR (or DESeq2) for per-cluster marker analysis.

## acidgsea 0.2.1 (2020-05-12)

Minor changes:

- Split out `setValidity` from `setClass` for S4 class definitions.
- `updateObject`: Added support for `...` and `verbose` arguments defined in the
  S4 generic.

## acidgsea 0.2.0 (2020-03-18)

Major changes:

- Renamed package from "pfgsea" to "acidgsea". Additional GSEA methods including
  the Broad GSEA Preranked algorithm and support for Jean Fan's liger package
  are planned for a future update, so it no longer makes sense to pin the
  package name specifically to fgsea.
- Reworked internally enriched gene set filtering and handoff.
  Affected functions: `enrichedGeneSets`, `plotEnrichedGeneSets`,
  and `plotEnrichedUpset`.

Minor changes:

- `convertToHuman`: Updated working example to use Ensembl 99 release, as
  archived releases are currently unavailable via biomaRt until March 24, due
  to Ensembl server migration. See Ensembl website for details.
- Updated minimal example `gsea` object.
- Updated unit tests and working examples to reflect package name change.

## pfgsea 0.1.17 (2020-01-27)

Minor changes:

- Now using cli package for interactive messages.

## pfgsea 0.1.16 (2020-01-20)

Minor changes:

- NAMESPACE update required for changes in basejump v0.12.

## pfgsea 0.1.15 (2019-11-20)

New functions:

- `plotCounts`, `plotHeatmap`: Added initial `FGSEAList` method support.

Major changes:

- `plotGSEATable`: Made defunct, since this visualization doesn't render very
  reliably in R Markdown output. Consider using a heatmap approach instead.

## pfgsea 0.1.14 (2019-10-11)

Minor changes:

- `export`: Updated internal code to handle `leadingEdge` as `list` class.
- Updated R Markdown template to use "object" as primary param.

## pfgsea 0.1.13 (2019-08-28)

Major changes:

- Reworked internal code to no longer depend on dplyr.
- Improved internal handling using `DataFrame` rather than `data.table`.

## pfgsea 0.1.12 (2019-07-30)

Minor changes:

- Updated basejump dependency versions.
- Updated working examples and improved documentation.

## pfgsea 0.1.11 (2019-07-24)

## pfgsea 0.1.10 (2019-07-17)

Minor changes:

- Updated basejump dependency.
- Improved Travis CI docker configuration.

## pfgsea 0.1.9 (2019-06-26)

New functions:

- `convertToHuman`: Utility that maps orthologs onto human genes, so we can
  perform GSEA. Added `map` argument support, to speed up multiple calls.

## pfgsea 0.1.8 (2019-06-12)

New functions:

- Initial method support for `convertToHuman`, which allows for easy ortholog
  conversion to HUGO (HGNC) identifiers.

Minor changes:

- Improved `render.R` script for looping multiple `DESeqAnalysis` objects.
- Improved dockerized Travis CI checks.
- Improved code coverage checks for `export` method.

## pfgsea 0.1.7 (2019-04-30)

Major changes:

- Reworked language to use "collection" consistently instead of mixing/matching
  "geneSet" and "pathway", which isn't appropriate. Relevant function arguments
  have been updated to reflect this change.
- Now exporting `collectionNames` instead of `pathwayNames`.
- `enriched` and `plotEnrichedUpset` have been updated to return per collection
  (e.g. hallmark gene set), which makes more sense. We don't want to visualize
  overlap across collections, since these are often completely different
  classes (e.g. c1-c7 will never have overlap).

## pfgsea 0.1.6 (2019-04-29)

New functions:

- `pathwayNames`, `contrastNames`: Accessor functions for pathway (e.g. MSigDb)
  and differential expression contrast names.
- `enriched` method support for FGSEAList class. Useful for returning a list
  directionally significant pathways, per contrast. Primarily intended for use
  at the moment with `plotEnrichedUpset`.
- `plotEnrichedUpset`: Utility function that makes it easy to compare enrichment
  groups across contrasts, via an UpSet plot.

Major changes:

- Added `combine` method support for `FGSEAList`, allowing us to easily combine
  GSEA results from multiple datasets. Primarily intended for downstream
  visualization using an UpSet plot (see `plotEnrichedUpset`).

Minor changes:

- Improved `show` method support for `FGSEAList`, showing `pathwayNames` and
  `contrastNames` more clearly.

## pfgsea 0.1.5 (2019-04-25)

Minor changes:

- S4 generic reexport documentation fixes.

## pfgsea 0.1.4 (2019-04-23)

Minor changes:

- Switch to importing graphics code from [AcidPlots][] package instead of now
  defunct minimalism package. Default ggplot2 theme has been renamed from
  `theme_paperwhite` to `acid_theme_light`.

## pfgsea 0.1.3 (2019-04-17)

Minor changes:

- Updated Travis CI configuration to use `rnaseq` Docker image.
- Miscellaneous documentation improvements.

## pfgsea 0.1.2 (2019-04-10)

Major changes:

- Deleted `list` to `FGSEAList` coercion method.

Minor changes:

- `pfgsea`: Renamed `bpparam` argument to `BPPARAM`, to match Bioconductor.
  Also now reexporting `BiocParallel::bpparam`.
- Added `gsea` minimal working example dataset, for unit testing.
- Added documentation working examples, using `gsea` data.
- Updated NAMESPACE for `theme_paperwhite`. Now reexporting this function.

## pfgsea 0.1.1 (2019-04-01)

Minor changes:

- NAMESPACE updates to reflect changes in [basejump][] package.
- Now importing `theme_paperwhite` from minimalism package.

## pfgsea 0.1.0 (2019-03-23)

- Initial stable release.
- Moved code to [Acid Genomics][].

## pfgsea 0.0.7 (2019-03-18)

Minor changes:

- Updated basejump and DESeqAnalysis dependencies.

## pfgsea 0.0.6 (2019-03-11)

Minor changes:

- Improved parameterized object passthrough in R Markdown template.
- Added Travis CI and AppVeyor CI coverage.

## pfgsea 0.0.5 (2019-02-04)

Major changes:

- Now exporting `FGSEAList` and `RankedList` S4 classes.
- Reworked `plotEnrichment()`, `plotGSEATable()`, and `topTables()` to use
  `FGSEAList` object.
- Renamed `statsList()` to `RankedList()`, and switched to S4 method approach.
- Updated R Markdown template to reflect these function reworkings.

## pfgsea 0.0.4 (2019-01-15)

Minor changes:

- Documentation fixes and other code cleanup to pass lintr checks.

## pfgsea 0.0.3 (2018-12-17)

New functions:

- statsList: Return preranked GSEA stats list.

Major changes:

- Added FGSEA R Markdown template.

## pfgsea 0.0.2 (2018-12-14)

Major changes:

- Switched to using goalie for assert checks instead of assertive.

Minor changes:

- Added README.
- Set up lintr checks.
- Set up pkgdown website.
- Miscellaneous documentation improvements.

## pfgsea 0.0.1 (2018-11-26)

Initial release.

[Acid Genomics]: https://acidgenomics.com/
[AcidPlots]: https://r.acidgenomics.com/packages/acidplots/
[basejump]: https://r.acidgenomics.com/packages/basejump/
