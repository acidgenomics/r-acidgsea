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
                "h_all_v7_0_symbols" = realpath(file.path(
                    prefix,
                    "condition_B_vs_A",
                    "h_all_v7_0_symbols.csv"
                ))
            ),
            "treatment_D_vs_C" = list(
                "h_all_v7_0_symbols" = realpath(file.path(
                    prefix,
                    "treatment_D_vs_C",
                    "h_all_v7_0_symbols.csv"
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
            "h_all_v7_0_symbols.csv"
        )
    )))
    df <- import(con = file.path(
        prefix,
        "condition_B_vs_A",
        "h_all_v7_0_symbols.csv"
    ))
    l <- as.list(df[1L, ])
    expect_identical(
        object = l[c("pathway", "leadingEdge")],
        expected = list(
            "pathway" = "HALLMARK_G2M_CHECKPOINT",
            "leadingEdge" = toString(c(
                "DBF4",
                "UPF1",
                "CDC27",
                "POLQ",
                "ARID4A",
                "PAFAH1B1",
                "MNAT1"
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
                "h_all_v7_0_symbols" = realpath(file.path(
                    prefix,
                    "condition_B_vs_A",
                    "h_all_v7_0_symbols.csv"
                ))
            ),
            "treatment_D_vs_C" = list(
                "h_all_v7_0_symbols" = realpath(file.path(
                    prefix,
                    "treatment_D_vs_C",
                    "h_all_v7_0_symbols.csv"
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
            "h_all_v7_0_symbols.csv"
        )
    )))
    unlink(testdir, recursive = TRUE)
})
