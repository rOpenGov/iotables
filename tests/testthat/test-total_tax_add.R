
test_tax <- total_tax_add (data_table = iotable_get(), 
               tax_names = c("net_tax_products",  "net_tax_production"),
               total_tax_name = "total_tax")

test_that("total tax row is returned", {
  expect_true("total_tax" %in% test_tax[,1])
})
