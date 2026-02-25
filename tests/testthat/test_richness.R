load('../test_data.Rdata')

test_that("ssu_richness output exactly matches baseline", {
  ssu = subset(fk2012, PRIMARY_SAMPLE_UNIT == PRIMARY_SAMPLE_UNIT[1] & STATION_NR == 1)
  res = ssu_richness(ssu)
  expect_snapshot(res)
})

test_that("psu_richness output exactly matches baseline", {
  psu = subset(fk2012, PRIMARY_SAMPLE_UNIT == PRIMARY_SAMPLE_UNIT[1])
  res = psu_richness(ssu_richness(psu))
  expect_snapshot(res)
})

test_that("strat_richness output exactly matches baseline", {
  strat = subset(fk2012, STRAT == STRAT[1] & PROT == 0)
  res = strat_richness(psu_richness(ssu_richness(strat)), ntot2012)
  expect_snapshot(res)
})

test_that("domain_richness output exactly matches baseline", {
  # We use the full dataset here as the original test did
  res = domain_richness(strat_richness(psu_richness(ssu_richness(fk2012)), ntot2012), ntot2012)
  expect_snapshot(res)
})


# context("SSU richness")

# load('../test_data.Rdata')
# ssu = subset(fk2012, PRIMARY_SAMPLE_UNIT == PRIMARY_SAMPLE_UNIT[1] & STATION_NR == 1)
# ssr = ssu_richness(ssu)

# test_that('returns a data.frame',
#           expect_true(is.data.frame(ssr))
#           )
# test_that('has richness column',
#           expect_true('richness' %in% names(ssr)))

# test_that('returns correct richness',
#           expect_equal(ssr$richness, 14))

# context("PSU richness")

# psu = subset(fk2012, PRIMARY_SAMPLE_UNIT == PRIMARY_SAMPLE_UNIT[1])
# psr = psu_richness(ssu_richness(psu))

# test_that('returns a data.frame', expect_true(is.data.frame(psr)))
# test_that('has richness/var column', expect_true(all(c('var','richness') %in% names(psr))))
# test_that('returns correct richness', expect_equal(psr$richness, 23.5))
# test_that('returns correct variance', expect_equal(psr$var, 180.5))

# context("STRAT richness")

# strat = subset(fk2012, STRAT == STRAT[1] & PROT == 0)
# str = strat_richness(psu_richness(ssu_richness(strat)), ntot = ntot2012)

# test_that('returns a data.frame', expect_true(is.data.frame(str)))
# test_that('returns a richness, var, n, nm, N, NM column',
#           expect_true(all(c('richness', 'var', 'n', 'nm', 'N', 'NM') %in% names(str))))
# test_that('returns correct richness', expect_equal(signif(str$richness), 26.3750))
# test_that('returns correct variance', expect_equal(signif(str$var, 4),0.5125))

# context("DOMAIN richness")
# dr = domain_richness(strat_richness(psu_richness(ssu_richness(fk2012)), ntot2012), ntot2012)

# test_that('returns a data.frame', expect_true(is.data.frame(dr)))
# test_that('returns a richness, var, n, nm, N, and NM column',
#           expect_true(all(c('richness', 'var', 'n', 'nm', 'N', 'NM') %in% names(dr))))
# test_that('returns correct richness',
#           expect_equal(signif(dr$richness), 28.5211))
# test_that('returns correct variance',
#           expect_equal(signif(dr$var), 0.110390))
