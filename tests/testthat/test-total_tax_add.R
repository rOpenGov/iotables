test_tax <- total_tax_add (data_table = iotable_get(), 
               tax_names = c("net_tax_products",  "net_tax_production"),
               total_tax_name = "total_tax")

test_that("total_tax_add() adds together tax rows into a single row", {
  expect_true("total_tax" %in% as.character(unlist(test_tax[,1])))
})
