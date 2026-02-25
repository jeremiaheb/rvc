load('../test_data.Rdata')

test_that("strat_length_frequency output exactly matches baseline", {
  strat = subset(fk2012, SPECIES_CD == 'EPI MORI' & STRAT == 'FSLR' & PROT == 0)
  
  res = strat_length_frequency(strat, ntot2012)
  expect_snapshot(res)
})

test_that("domain_length_frequency output exactly matches baseline", {
  domain = subset(fk2012, SPECIES_CD == 'LUT GRIS')
  
  res = domain_length_frequency(strat_length_frequency(domain, ntot2012), ntot2012)
  expect_snapshot(res)
})