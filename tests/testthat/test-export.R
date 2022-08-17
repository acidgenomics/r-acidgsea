test_that("New 'con' BiocIO approach, instead of deprecated 'dir'", {
    testdir <- tempdir2()
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
            "pathway" = "HALLMARK_ADIPOGENESIS",
            "leadingEdge" = toString(c(
                "PHLDB1",
                "IFNGR1",
                "MYLK",
                "RETSAT"
            ))
        )
    )
    unlink2(testdir)
})

test_that("Deprecated : 'dir' argument, no 'name'", {
    testdir <- tempdir2()
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
    unlink2(testdir)
})
