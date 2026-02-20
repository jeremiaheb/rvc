load("../test_data.Rdata")

test_that("psu_abundance output exactly matches baseline", {
  p_input = subset(fk2012, SPECIES_CD == "LUT GRIS" & PRIMARY_SAMPLE_UNIT == "005U")
  pdens = psu_density(ssu_density(p_input))
  
  res = psu_abundance(pdens)
  expect_snapshot(res)
})

test_that("strat_abundance output exactly matches baseline", {
  strat = subset(fk2012, SPECIES_CD == "MYC BONA" & STRAT == "FDLR" & PROT == 0)
  pdens = psu_density(ssu_density(strat))
  
  res = strat_abundance(pdens, ntot2012)
  expect_snapshot(res)
})

test_that("domain_abundance output exactly matches baseline", {
  domain = subset(fk2012, SPECIES_CD == "ANI VIRG")
  sdens = strat_density(psu_density(ssu_density(domain)), ntot2012)
  
  res = domain_abundance(sdens, ntot2012)
  expect_snapshot(res)
})