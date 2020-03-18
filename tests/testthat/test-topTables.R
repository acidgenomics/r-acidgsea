context("topTables")

test_that("FGSEAList", {
    output <- capture.output(
        topTables(fgsea, collection = "h")
    )
    expect_identical(
        object = output,
        expected = c(
            "",
            "",
            "### dmso_r1881_vs_etoh",
            "",
            "",
            "",
            "#### Upregulated",
            "",
            "",
            "",
            "|pathway                            |  padj|   NES| size|",
            "|:----------------------------------|-----:|-----:|----:|",
            "|HALLMARK_E2F_TARGETS               | 0.005| 3.111|  196|",
            "|HALLMARK_MYC_TARGETS_V1            | 0.005| 2.814|  197|",
            "|HALLMARK_ANDROGEN_RESPONSE         | 0.005| 2.743|   97|",
            "|HALLMARK_G2M_CHECKPOINT            | 0.005| 2.723|  193|",
            "|HALLMARK_MYC_TARGETS_V2            | 0.005| 2.452|   58|",
            "|HALLMARK_MTORC1_SIGNALING          | 0.005| 2.375|  197|",
            "|HALLMARK_DNA_REPAIR                | 0.005| 2.229|  140|",
            "|HALLMARK_UNFOLDED_PROTEIN_RESPONSE | 0.005| 1.929|  107|",
            "|HALLMARK_MITOTIC_SPINDLE           | 0.005| 1.875|  197|",
            "|HALLMARK_ESTROGEN_RESPONSE_LATE    | 0.005| 1.875|  192|",
            "",
            "",
            "#### Downregulated",
            "",
            "",
            "",
            "|pathway                            |  padj|    NES| size|",
            "|:----------------------------------|-----:|------:|----:|",
            "|HALLMARK_INTERFERON_ALPHA_RESPONSE | 0.012| -1.549|   91|",
            "|HALLMARK_MYOGENESIS                | 0.014| -1.830|  181|",
            "|HALLMARK_HEDGEHOG_SIGNALING        | 0.014| -1.778|   34|",
            "|HALLMARK_HEME_METABOLISM           | 0.014| -1.515|  187|",
            "|HALLMARK_COAGULATION               | 0.019| -1.499|  118|",
            "|HALLMARK_UV_RESPONSE_DN            | 0.021| -1.554|  137|",
            "|HALLMARK_COMPLEMENT                | 0.022| -1.485|  180|",
            "|HALLMARK_APICAL_JUNCTION           | 0.022| -1.427|  187|"
        )
    )
})
