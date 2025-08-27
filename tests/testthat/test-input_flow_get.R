input_flow_table <- input_flow_get(iotable_get(labelling = "iotables"),
  households = TRUE
)

test_that("input_flow_get() returns the expected data from the Eurostat Manual p461", {
  expect_equal(as.numeric(input_flow_table[1, 2]), 1131)
  expect_equal(
    as.numeric(input_flow_table[3, "final_consumption_households"]),
    3457
  )
})

test_that("In input_flow_get() the households are included or excluded as requested", {
  expect_equal("final_consumption_households" %in%
    names(input_flow_get(iotable_get(labelling = "iotables"),
      households = FALSE
    )), FALSE)
  expect_equal("final_consumption_households" %in%
    names(input_flow_table), TRUE)
})
