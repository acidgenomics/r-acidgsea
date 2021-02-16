## AcidGSEA 0.6.0 (2021-02-16)

### Major changes

- Reworked and simplified NAMESPACE and package dependencies, following
  basejump v0.14 release series update.

## AcidGSEA 0.5.0 (2020-10-10)

### Major changes

- Renamed package to AcidGSEA from acidgsea.

### Minor changes

- Migrated generics to AcidGenerics: `geneSetNames`, `plotLFC`, `plotNES`.

## acidgsea 0.4.0 (2020-09-26)

### Major changes

- `FGSEAList`: Reworked primary method to use `DESeqAnalysis` instead of
  `RankedList` as input. The `RankedList` object is now calculated automatically
  internally.
- Now slotting gene sets inside the `FGSEAList` object.
- `gmtFiles` argument has been renamed to `geneSetFiles`.

### New functions

- `geneSetResults`: Extract gene set expression values (from `DESeqResults`).
- `leadingEdge`: New accessor that returns leading edge genes from GSEA run.
- `results`: Extract GSEA results from object (from `fgsea` return).

## acidgsea 0.3.2 (2020-08-05)

### Major changes

- Simplified internal handling of alpha (now consistently referfed to as
  "alphaThreshold") and nesThreshold.
- Added `alphaThreshold` and `nesThreshold` S4 generic support.
- These name chanages better match the syntax used in DESeqAnalysis v0.3.

## acidgsea 0.3.1 (2020-07-24)

### New functions

- `plotGeneSet`: Split out internal gene set plotting code used previously in
  `plotEnrichedGeneSets`. This is a modified variant of `fgsea::plotEnrichment`
  that allows for additional color customization.

### Major changes

- Updated minimum R dependency to 4.0.
- `plotEnrichedGeneSets`: Now calls new `plotGeneSet` function internally.

### Minor changes

- Removed UpSetR dependency, based on update to AcidPlots package.
- Removed `plotCounts` method, in favor of `plotHeatmap` usage.

## acidgsea 0.3.0 (2020-05-24)

### Major changes

- `FGSEAList`: Converted function to an S4 generic. Previously the main arguemnt
  was named `rankedList`, but this has been renamed to `object`. A `RankedList`
  object can still be passed in as the first object in the function call. This
  change was necessary for adding GSEA support in pointillism single-cell
  RNA-seq analysis toolkit.

### Minor changes

- `RankedList`: Added support for `DataFrame` class, which is used inside
  pointillism package update.
- `RankedList`: Added `gene2symbol` argument support for `matrix` model.
  Using this for cluster matrix in pointillism for scRNA-seq using output
  from edgeR (or DESeq2) for per-cluster marker analysis.

## acidgsea 0.2.1 (2020-05-12)

### Minor changes

- Split out `setValidity` from `setClass` for S4 class definitions.
- `updateObject`: Added support for `...` and `verbose` arguments defined in the
  S4 generic.

## acidgsea 0.2.0 (2020-03-18)

### Major changes

- Renamed package from "pfgsea" to "acidgsea". Additional GSEA methods including
  the Broad GSEA Preranked algorithm and support for Jean Fan's liger package
  are planned for a future update, so it no longer makes sense to pin the
  package name specifically to fgsea.
- Reworked internally enriched gene set filtering and handoff.
  Affected functions: `enrichedGeneSets`, `plotEnrichedGeneSets`,
  and `plotEnrichedUpset`.

### Minor changes

- `convertToHuman`: Updated working example to use Ensembl 99 release, as
  archived releases are currently unavailable via biomaRt until March 24, due
  to Ensembl server migration. See Ensembl website for details.
- Updated minimal example `gsea` object.
- Updated unit tests and working examples to reflect package name change.

## pfgsea 0.1.17 (2020-01-27)

### Minor changes

- Now using cli package for interactive messages.

## pfgsea 0.1.16 (2020-01-20)

### Minor changes

- NAMESPACE update required for changes in basejump v0.12.

## pfgsea 0.1.15 (2019-11-20)

### New functions

- `plotCounts`, `plotHeatmap`: Added initial `FGSEAList` method support.

### Major changes

- `plotGSEATable`: Made defunct, since this visualization doesn't render very
  reliably in R Markdown output. Consider using a heatmap approach instead.

## pfgsea 0.1.14 (2019-10-11)

### Minor changes

- `export`: Updated internal code to handle `leadingEdge` as `list` class.
- Updated R Markdown template to use "object" as primary param.

## pfgsea 0.1.13 (2019-08-28)

### Major changes

- Reworked internal code to no longer depend on dplyr.
- Improved internal handling using `DataFrame` rather than `data.table`.

## pfgsea 0.1.12 (2019-07-30)

### Minor changes

- Updated basejump dependency versions.
- Updated working examples and improved documentation.

## pfgsea 0.1.11 (2019-07-24)

## pfgsea 0.1.10 (2019-07-17)

### Minor changes

- Updated basejump dependency.
- Improved Travis CI docker configuration.

## pfgsea 0.1.9 (2019-06-26)

### New functions

- `convertToHuman`: Utility that maps orthologs onto human genes, so we can
  perform GSEA. Added `map` argument support, to speed up multiple calls.

## pfgsea 0.1.8 (2019-06-12)

### New functions

- Initial method support for `convertToHuman`, which allows for easy ortholog
  conversion to HUGO (HGNC) identifiers.

### Minor changes

- Improved `render.R` script for looping multiple `DESeqAnalysis` objects.
- Improved dockerized Travis CI checks.
- Improved code coverage checks for `export` method.

## pfgsea 0.1.7 (2019-04-30)

### Major changes

- Reworked language to use "collection" consistently instead of mixing/matching
  "geneSet" and "pathway", which isn't appropriate. Relevant function arguments
  have been updated to reflect this change.
- Now exporting `collectionNames` instead of `pathwayNames`.
- `enriched` and `plotEnrichedUpset` have been updated to return per collection
  (e.g. hallmark gene set), which makes more sense. We don't want to visualize
  overlap across collections, since these are often completely different
  classes (e.g. c1-c7 will never have overlap).

## pfgsea 0.1.6 (2019-04-29)

### New functions

- `pathwayNames`, `contrastNames`: Accessor functions for pathway (e.g. MSigDb)
  and differential expression contrast names.
- `enriched` method support for FGSEAList class. Useful for returning a list
  directionally significant pathways, per contrast. Primarily intended for use
  at the moment with `plotEnrichedUpset`.
- `plotEnrichedUpset`: Utility function that makes it easy to compare enrichment
  groups across contrasts, via an UpSet plot.

### Major changes

- Added `combine` method support for `FGSEAList`, allowing us to easily combine
  GSEA results from multiple datasets. Primarily intended for downstream
  visualization using an UpSet plot (see `plotEnrichedUpset`).

### Minor changes

- Improved `show` method support for `FGSEAList`, showing `pathwayNames` and
  `contrastNames` more clearly.

## pfgsea 0.1.5 (2019-04-25)

### Minor changes

- S4 generic reexport documentation fixes.

## pfgsea 0.1.4 (2019-04-23)

### Minor changes

- Switch to importing graphics code from [AcidPlots][] package instead of now
  defunct minimalism package. Default ggplot2 theme has been renamed from
  `theme_paperwhite` to `acid_theme_light`.

## pfgsea 0.1.3 (2019-04-17)

### Minor changes

- Updated Travis CI configuration to use `rnaseq` Docker image.
- Miscellaneous documentation improvements.

## pfgsea 0.1.2 (2019-04-10)

### Major changes

- Deleted `list` to `FGSEAList` coercion method.

### Minor changes

- `pfgsea`: Renamed `bpparam` argument to `BPPARAM`, to match Bioconductor.
  Also now reexporting `BiocParallel::bpparam`.
- Added `gsea` minimal working example dataset, for unit testing.
- Added documentation working examples, using `gsea` data.
- Updated NAMESPACE for `theme_paperwhite`. Now reexporting this function.

## pfgsea 0.1.1 (2019-04-01)

### Minor changes

- NAMESPACE updates to reflect changes in [basejump][] package.
- Now importing `theme_paperwhite` from minimalism package.

## pfgsea 0.1.0 (2019-03-23)

- Initial stable release.
- Moved code to [Acid Genomics][].

## pfgsea 0.0.7 (2019-03-18)

### Minor changes

- Updated basejump and DESeqAnalysis dependencies.

## pfgsea 0.0.6 (2019-03-11)

### Minor changes

- Improved parameterized object passthrough in R Markdown template.
- Added Travis CI and AppVeyor CI coverage.

## pfgsea 0.0.5 (2019-02-04)

### Major changes

- Now exporting `FGSEAList` and `RankedList` S4 classes.
- Reworked `plotEnrichment()`, `plotGSEATable()`, and `topTables()` to use
  `FGSEAList` object.
- Renamed `statsList()` to `RankedList()`, and switched to S4 method approach.
- Updated R Markdown template to reflect these function reworkings.

## pfgsea 0.0.4 (2019-01-15)

### Minor changes

- Documentation fixes and other code cleanup to pass lintr checks.

## pfgsea 0.0.3 (2018-12-17)

### New functions

- statsList: Return preranked GSEA stats list.

### Major changes

- Added FGSEA R Markdown template.

## pfgsea 0.0.2 (2018-12-14)

### Major changes

- Switched to using goalie for assert checks instead of assertive.

### Minor changes

- Added README.
- Set up lintr checks.
- Set up pkgdown website.
- Miscellaneous documentation improvements.

## pfgsea 0.0.1 (2018-11-26)

Initial release.

[Acid Genomics]: https://acidgenomics.com/
[AcidPlots]: https://AcidPlots.acidgenomics.com/
[basejump]: https://basejump.acidgenomics.com/
