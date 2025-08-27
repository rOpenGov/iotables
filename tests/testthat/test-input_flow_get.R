test_that("input_flow_get returns rows up to total if present", {
  dt <- iotable_get(labelling = "iotables")
  x  <- input_flow_get(dt, households = TRUE, empty_remove = FALSE)
  
  # Should include at least one industry row
  expect_gt(nrow(x), 1)
  # Key column preserved
  expect_identical(names(x)[1], names(dt)[1])
})

test_that("households toggle adds/removes the household column", {
  dt <- iotable_get(labelling = "iotables")
  
  x_on  <- input_flow_get(dt, households = TRUE,  empty_remove = FALSE)
  x_off <- input_flow_get(dt, households = FALSE, empty_remove = FALSE)
  
  expect_true("final_consumption_households" %in% names(x_on))
  expect_false("final_consumption_households" %in% names(x_off))
})

test_that("empty_remove argument currently has no effect (documented)", {
  # This test documents current behavior (bug) so later changes will make it fail.
  dt <- iotable_get()
  x1 <- input_flow_get(dt, empty_remove = FALSE, households = FALSE)
  x2 <- input_flow_get(dt, empty_remove = TRUE,  households = FALSE)
  expect_equal(dim(x1), dim(x2))
})
