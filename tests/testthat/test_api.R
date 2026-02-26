test_that("getRvcData successfully downloads data (Live Test)", {
  # These skips ensure CRAN doesn't fail your package if the NOAA server is down
  skip_on_cran()
  skip_if_offline()
  
  # Check if the server is alive right now; if not, skip the test
  server <- 'https://ncrmp-data-entry.fisheries.noaa.gov/rvc_analysis20/'
  skip_if(httr::http_error(server), message = "NOAA server is currently unreachable.")
  
  # Download a small, specific dataset to test the pipeline
  expect_message(
    res <- getRvcData(years = 2012, regions = "FLA KEYS"),
    "downloading"
  )
  
  # Verify the structure of the returned object
  expect_type(res, "list")
  expect_named(res, c("sample_data", "stratum_data", "taxonomic_data"))
  
  # Verify the contents are dataframes with actual rows
  expect_s3_class(res$sample_data, "data.frame")
  expect_s3_class(res$stratum_data, "data.frame")
  expect_s3_class(res$taxonomic_data, "data.frame")
  expect_gt(nrow(res$sample_data), 0)
})

test_that("API handles invalid combinations gracefully", {
  skip_on_cran()
  skip_if_offline()
  
  server <- 'https://ncrmp-data-entry.fisheries.noaa.gov/rvc_analysis20/'
  skip_if(httr::http_error(server))
  
  # Asking for a year that doesn't exist should throw an error or warning
  expect_error(
    getSampleData(years = 1800, regions = "FLA KEYS"),
    "The following combinations of region/year are not available"
  )
})