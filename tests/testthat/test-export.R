context("export")

test_that("FGSEAList", {
    files <- export(
        object = fgsea,
        dir = "example",
        geneSetResults = FALSE
    )
    expect_identical(
        object = files,
        expected = list(
            "condition_B_vs_A" = list(
                "h" = realpath(file.path(
                    "example",
                    "fgsea",
                    "condition_B_vs_A",
                    "h.csv"
                ))
            ),
            "treatment_D_vs_C" = list(
                "h" = realpath(file.path(
                    "example",
                    "fgsea",
                    "treatment_D_vs_C",
                    "h.csv"
                ))
            )
        )
    )
    expect_identical(sort(list.files("example")), "fgsea")
    expect_identical(
        object = sort(list.files(file.path("example", "fgsea"))),
        expected = c(
            "condition_B_vs_A",
            "treatment_D_vs_C"
        )
    )
    expect_true(all(file.exists(
        file.path(
            "example",
            "fgsea",
            c(
                "condition_B_vs_A",
                "treatment_D_vs_C"
            ),
            "h.csv"
        )
    )))
    unlink("example", recursive = TRUE)
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
