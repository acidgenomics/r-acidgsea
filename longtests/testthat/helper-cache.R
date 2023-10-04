lst <- AcidDevTools::cacheTestFiles(
    pkg = .pkgName,
    files = "mm_deseq.rds"
)
cacheDir <- lst[["cacheDir"]]
rm(lst)
