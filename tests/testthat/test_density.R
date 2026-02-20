# Load your test data just like you do in your other tests
load('../test_data.Rdata')

test_that("ssu_density output exactly matches baseline", {
  # 1. Isolate the input data
  ssu_input = subset(fk2012, SPECIES_CD == "ANI VIRG" & 
                             PRIMARY_SAMPLE_UNIT == "005U" & 
                             STATION_NR == 2)
  
  # 2. Run the current base R function
  res = ssu_density(ssu_input)
  
  # 3. Take the snapshot! 
  # style = "json2" is great because it strictly checks column names, values, and data types
  expect_snapshot_value(res, style = "json2") 
})

test_that("psu_density output exactly matches baseline", {
  # Get PSU input using your existing dataset
  p_input = subset(fk2012, SPECIES_CD == "LUT GRIS" & 
                           PRIMARY_SAMPLE_UNIT == "005U")
  
  # Note: psu_density takes the output of ssu_density
  ssu_res = ssu_density(p_input)
  res = psu_density(ssu_res)
  
  expect_snapshot_value(res, style = "json2") 
})

test_that("strat_density output exactly matches baseline", {
  # 1. Grab a subset of data
  strat_input = subset(fk2012, SPECIES_CD == "MYC BONA" & STRAT == "FDLR" & PROT == 0)
  
  # 2. Run the pipeline
  ssu_res = ssu_density(strat_input)
  psu_res = psu_density(ssu_res)
  res = strat_density(psu_res, ntot2012)
  
  # 3. Snapshot the printed output!
  expect_snapshot(res) 
})

test_that("domain_density output exactly matches baseline", {
  # 1. Grab a subset of data
  domain_input = subset(fk2012, SPECIES_CD == "EPI MORI")
  
  # 2. Run the pipeline
  sdens = strat_density(psu_density(ssu_density(domain_input)), ntot2012)
  ddens = domain_density(sdens, ntot2012)
  
  # 3. Snapshot!
  expect_snapshot(ddens) 
})