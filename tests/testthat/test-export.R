context("export : FGSEAList")

testdir <- file.path(tempdir(), "example")

test_that("New 'con' BiocIO approach, instead of deprecated 'dir'", {
    unlink(testdir, recursive = TRUE)
    object <- fgsea
    out <- export(
        object = object,
        con = testdir
    )
    prefix <- realpath(testdir)
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

test_that("Deprecated : 'dir' argument, no 'name'", {
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
