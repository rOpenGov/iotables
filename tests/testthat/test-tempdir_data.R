test_that("tempdir_data() emits deprecation warning", {
  fake_download <- data.frame(a = 1)
  with_mock(
    `eurostat::get_eurostat` = function(...) fake_download,
    expect_warning(tempdir_data("naio_10_cp1700"), "deprecated")
  )
})
