## Tests for api functions

check_api = function(){
  exists = !httr::http_error('https://ncrmp-data-entry.fisheries.noaa.gov/rvc_analysis20/')
  if(!exists){
    skip("API unavailable")
  }
}

context("getBenthicData function")

test_that('returns a data.frame with appropriate columns', {
  check_api()

  bd = getBenthicData(years = 2011, regions = "FLA KEYS")
  expect_true(is.data.frame(bd))
  expect_true(all(c("REGION", "YEAR", "PRIMARY_SAMPLE_UNIT", "STATION_NR",
                    "DEPTH", "MAX_HARD_RELIEF", "MAX_SOFT_RELIEF",
                    "AVG_HARD_RELIEF", "HARD_REL_PCT_0", "HARD_REL_PCT_1",
                    "HARD_REL_PCT_2", "HARD_REL_PCT_3", "HARD_REL_PCT_4",
                    "PCT_SAND", "PCT_HARD_BOTTOM", "PCT_RUBBLE", "PCT_CORAL",
                    "PCT_OCTO", "PCT_SPONGE") %in% names(bd)))
})

context('getStratumData function')

test_that('returns a data.frame with appropriate columns', {
  check_api()

  sd = getStratumData(years = 2012, regions = "DRY TORT")
  expect_true(is.data.frame(sd))
  expect_true(all(c('REGION', 'YEAR', 'PROT', 'STRAT', 'NTOT', 'GRID_SIZE')
                  %in% names(sd)))
})

context('getSampleData function')

test_that('returns a data.frame with appropriate columns', {
  check_api()

  sd = getSampleData(years = 2012, regions = "FLA KEYS")
  expect_true(is.data.frame(sd))
  expect_true(all(c('PRIMARY_SAMPLE_UNIT', 'YEAR', 'MONTH',
                    'DAY', 'STATION_NR', 'LAT_DEGREES',
                    'LON_DEGREES', 'DEPTH', 'UNDERWATER_VISIBILITY',
                    'MAPGRID_NR', 'HABITAT_CD', 'ZONE_NR', 'SUBREGION_NR',
                    'MPA_NR', 'SPECIES_NR', 'SPECIES_CD', 'LEN', 'NUM',
                    'TIME_SEEN', 'PROT', 'STRAT', 'REGION') %in% names(sd)))
})
