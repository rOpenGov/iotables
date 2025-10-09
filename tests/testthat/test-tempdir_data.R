test_that("tempdir_data() issues a deprecation warning", {
  # Load mockery
  library(mockery)

  # Define harmless mocks that return dummy data
  mock_get <- mock(data.frame(values = 1))
  mock_clean <- mock(NULL)

  # Mock both eurostat functions safely
  stub(tempdir_data, "eurostat::get_eurostat", mock_get)
  stub(tempdir_data, "eurostat::clean_eurostat_cache", mock_clean)

  # Expect a deprecation warning mentioning iotables_download()
  expect_warning(
    tempdir_data("naio_10_cp1700", force_download = FALSE),
    regexp = "deprecated.*iotables_download",
    fixed = FALSE
  )
})
