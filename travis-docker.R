source("msigdb.R")

rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")
BiocCheck::BiocCheck(`quit-with-status` = TRUE)

lintr::lint_package()
