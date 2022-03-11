## FIXME Need to check that leadingEdge column is exported correctly
## and contains toString formatted output.



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
    df <- import(file.path(prefix, "condition_B_vs_A", "h.csv"))
    l <- as.list(df[1L, ])
    expect_identical(
        object = l[c("pathway", "leadingEdge")],
        expected = list(
            "pathway" = "HALLMARK_G2M_CHECKPOINT",
            "leadingEdge" = toString(c(
                "E2F2",
                "ARID4A",
                "UPF1",
                "MNAT1",
                "PAFAH1B1",
                "CUL1",
                "POLQ",
                "TACC3",
                "DBF4",
                "HMGB3",
                "CDC27",
                "FOXN3",
                "SLC12A2",
                "POLA2",
                "LIG3",
                "CUL3"
            ))
        )
    )
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
