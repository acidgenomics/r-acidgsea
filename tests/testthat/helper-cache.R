lst <- AcidDevTools::cacheTestFiles(
    pkg = .pkgName,
    files = c(
        "h.all.v7.0.symbols.gmt",
        "mm_deseq.rds"
    )
)
cacheDir <- lst[["cacheDir"]]
rm(lst)
