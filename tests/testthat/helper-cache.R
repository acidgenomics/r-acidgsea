lst <- AcidDevTools::cacheTestFiles(
    pkg = .pkgName,
    files = "h.all.v7.0.symbols.gmt"
)
cacheDir <- lst[["cacheDir"]]
rm(lst)
