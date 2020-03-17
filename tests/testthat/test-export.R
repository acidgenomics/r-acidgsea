context("export")

test_that("FGSEAList", {
    files <- export(object = gsea, dir = "example")
    expect_identical(
        object = files,
        expected = list(
            h = list(
                "condition_B_vs_A" = realpath(file.path(
                    "example",
                    "gsea",
                    "condition_B_vs_A",
                    "h.csv"
                )),
                "treatment_D_vs_C" = realpath(file.path(
                    "example",
                    "gsea",
                    "treatment_D_vs_C",
                    "h.csv"
                ))
            )
        )
    )
    expect_identical(sort(list.files("example")), "gsea")
    expect_identical(
        object = sort(list.files(file.path("example", "gsea"))),
        expected = c(
            "condition_B_vs_A",
            "treatment_D_vs_C"
        )
    )
    expect_true(all(file.exists(
        file.path(
            "example",
            "gsea",
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
    files <- export(object = gsea, name = "XXX", dir = ".")
    expect_identical(
        object = files,
        expected = list(
            h = list(
                "condition_B_vs_A" = realpath(file.path(
                    "XXX",
                    "condition_B_vs_A",
                    "h.csv"
                )),
                "treatment_D_vs_C" = realpath(file.path(
                    "XXX",
                    "treatment_D_vs_C",
                    "h.csv"
                ))
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})
