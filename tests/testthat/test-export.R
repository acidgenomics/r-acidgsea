context("export : FGSEAList")

testdir <- file.path(tempdir(), "example")

## FIXME Need to add support for this.
test_that("New 'con' BiocIO approach, instead of deprecated 'dir'", {
})

test_that("FGSEAList", {
    unlink(testdir, recursive = TRUE)
    object <- fgsea
    out <- export(
        object = object,
        dir = testdir
    )
    prefix <- file.path(testdir, "object")
    expect_identical(
        object = out,
        expected = list(
            "condition_B_vs_A" = list(
                "h" = realpath(file.path(
                    prefix,
                    "condition_B_vs_A",
                    "h.csv"
                ))
            ),
            "treatment_D_vs_C" = list(
                "h" = realpath(file.path(
                    prefix,
                    "treatment_D_vs_C",
                    "h.csv"
                ))
            )
        )
    )
    expect_identical(
        object = sort(list.files(prefix)),
        expected = c(
            "condition_B_vs_A",
            "treatment_D_vs_C"
        )
    )
    expect_true(all(file.exists(
        file.path(
            prefix,
            c(
                "condition_B_vs_A",
                "treatment_D_vs_C"
            ),
            "h.csv"
        )
    )))
    unlink(testdir, recursive = TRUE)
})

test_that("name argument", {
    files <- export(
        object = fgsea,
        name = "XXX",
        dir = ".",
        geneSetResults = FALSE
    )
    expect_identical(
        object = files,
        expected = list(
            "condition_B_vs_A" = list(
                "h" = realpath(file.path(
                    "XXX",
                    "condition_B_vs_A",
                    "h.csv"
                ))
            ),
            "treatment_D_vs_C" = list(
                "h" = realpath(file.path(
                    "XXX",
                    "treatment_D_vs_C",
                    "h.csv"
                ))
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})
