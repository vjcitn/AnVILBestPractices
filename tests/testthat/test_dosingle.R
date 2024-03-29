

test_that("do_SingleR works", {
 p3k = TENxPBMCData::TENxPBMCData("pbmc3k")
 rownames(p3k) = make.names(rowData(p3k)$Symbol, unique=TRUE)
 pred3k = do_SingleR(p3k)
 expect_true(all(c("input", "preds") %in% names(pred3k)))
})

