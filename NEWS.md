## pfgsea 0.1.10 (2019-07-17)




## pfgsea 0.1.9 (2019-06-26)

### New functions

- `convertToHuman`: Utility that maps orthologs onto human genes, so we can
  perform GSEA.

## pfgsea 0.1.8 (2019-06-12)




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

- Switch to importing graphics code from [acidplots][] package instead of now
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
[acidplots]: https://acidplots.acidgenomics.com/
[basejump]: https://basejump.acidgenomics.com/
