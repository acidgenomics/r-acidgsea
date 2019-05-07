context("export")

test_that("FGSEAList", {
    files <- export(object = gsea, dir = "example")
    expect_identical(
        object = files,
        expected = list(
            h = list(
                dmso_r1881_vs_etoh = realpath(file.path(
                    "example", "gsea", "dmso_r1881_vs_etoh", "h.csv"
                ))
            )
        )
    )
    expect_identical(sort(list.files("example")), "gsea")
    expect_identical(
        object = sort(list.files(file.path("example", "gsea"))),
        expected = "dmso_r1881_vs_etoh"
    )
    expect_true(file.exists(
        file.path("example", "gsea", "dmso_r1881_vs_etoh", "h.csv")
    ))
    unlink("example", recursive = TRUE)
})

test_that("name argument", {
    files <- export(object = gsea, name = "XXX", dir = ".")
    expect_identical(
        object = files,
        expected = list(
            h = list(
                dmso_r1881_vs_etoh = realpath(file.path(
                    "XXX", "dmso_r1881_vs_etoh", "h.csv"
                ))
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})
