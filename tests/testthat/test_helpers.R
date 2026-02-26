load('../test_data.Rdata')

test_that(".aggBy returns correct grouping columns for each level", {
  
  # SSU level includes everything down to the station
  expect_equal(
    rvc:::.aggBy("ssu"),
    c("YEAR", "REGION", "STRAT", "PROT", "PRIMARY_SAMPLE_UNIT", "STATION_NR", "SPECIES_CD")
  )
  
  # PSU level drops the station number
  expect_equal(
    rvc:::.aggBy("psu"),
    c("YEAR", "REGION", "STRAT", "PROT", "PRIMARY_SAMPLE_UNIT", "SPECIES_CD")
  )
  
  # Stratum level drops the PSU
  expect_equal(
    rvc:::.aggBy("strat"),
    c("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD")
  )
  
  # Domain level drops the Stratum and Protected status
  expect_equal(
    rvc:::.aggBy("domain"),
    c("YEAR", "REGION", "SPECIES_CD")
  )
  
  # Safety check: ensure it throws an error if given a bad input
  expect_error(rvc:::.aggBy("invalid_level"))
})

test_that(".wrapFunction correctly renames, applies function, and restores name", {
  
  dummy_data = data.frame(id = 1:3, my_var = c(1, 2, 3))
  
  dummy_math_func = function(df, multiplier) {
    df$target_var = df$target_var * multiplier
    return(df)
  }
  
  # Pass arguments positionally so we don't trip over parameter names
  res = rvc:::.wrapFunction(dummy_data, "my_var", "target_var", dummy_math_func, multiplier = 10)
  
  expect_equal(res$my_var, c(10, 20, 30))
  expect_false("target_var" %in% names(res))
})

test_that(".getWeight output matches baseline and weights sum to 1", {
  # 1. Create realistic strata-level data exactly as the package does
  sample_data = subset(fk2012, SPECIES_CD == "ANI VIRG")
  sdens = strat_density(psu_density(ssu_density(sample_data)), ntot2012)
  
  # 2. Apply the weighting function
  res = rvc:::.getWeight(sdens, ntot2012)
  
  # 3. Snapshot the resulting data frame
  expect_snapshot(res)
  
  # 4. Property Test: Verify that the weighting factors (wh) sum to exactly 1
  # (Since sdens has one row per stratum, summing the wh column gives the total domain weight)
  expect_equal(sum(res$wh), 1, tolerance = 1e-6)
})

test_that(".with_strata output exactly matches baseline", {
  # Filter sample data so it only contains a few strata
  sample_subset = subset(fk2012, STRAT == "FSLR")
  
  res = rvc:::.with_strata(ntot2012, sample_subset)
  expect_snapshot(res)
})

test_that(".wrapperProto correctly filters data and routes to function", {
  
  # 1. Build the 'x' list object expected by .wrapperProto
  rvc_data_list = list(
    sample_data = fk2012,
    stratum_data = ntot2012,
    taxonomic_data = taxonomic_data
  )
  
  # 2. Define a dummy math function to intercept the routed data
  # This just returns the number of rows left in the data frames after filtering
  dummy_fun = function(samp, strat, ...) {
    return(data.frame(
      sampled_rows = nrow(samp),
      stratum_rows = nrow(strat)
    ))
  }
  
  # 3. Test the base case: No length bins, merged protected
  # We'll filter for a single species and a specific year
  res = rvc:::.wrapperProto(
    x = rvc_data_list,
    species = "EPI MORI",
    length_bins = NULL,
    merge_protected = TRUE,
    wrapper = "dummy_wrapper",
    fun = dummy_fun,
    years = 2012 # Passed via ... to the filters
  )
  
  expect_snapshot(res)
})

test_that(".apply_sample_filters output exactly matches baseline", {
  
  # Apply a complex combination of filters through the ... argument
  res = rvc:::.apply_sample_filters(
    x = fk2012, 
    species = c("EPI MORI", "MYC BONA"),
    years = 2012,
    is_protected = FALSE,
    len_geq = 20
  )
  
  expect_snapshot(res)
})

test_that(".apply_stratum_filters output exactly matches baseline", {
  
  # 1. Create a mocked sample dataset that has already been filtered
  sample_sub = subset(fk2012, SPECIES_CD == "EPI MORI" & YEAR == 2012 & PROT == 0)
  
  # 2. Run the stratum filters
  res = rvc:::.apply_stratum_filters(
    stratum_data = ntot2012, 
    sample_data = sample_sub,
    years = 2012,
    is_protected = FALSE
  )
  
  expect_snapshot(res)
})

test_that(".getSpecies_cd output exactly matches baseline", {
  # Test with species code
  expect_snapshot(rvc:::.getSpecies_cd("Myc BONA", taxonomic_data))
  
  # Test with common name
  expect_snapshot(rvc:::.getSpecies_cd("Black Grouper", taxonomic_data))
  
  # Test with scientific name
  expect_snapshot(rvc:::.getSpecies_cd("Mycteroperca bonaci", taxonomic_data))
})

test_that(".funByLen output exactly matches baseline for numeric bins and lookups", {
  
  rvc_data_list = list(
    sample_data = fk2012,
    stratum_data = ntot2012,
    taxonomic_data = taxonomic_data
  )
  
  # A dummy callback that simulates a wrapper function like rvc_density
  dummy_cb = function(x, species, len_lt = NULL, len_geq = NULL, ...) {
    data.frame(
      SPECIES_CD = species,
      len_lt = ifelse(is.null(len_lt), NA_real_, len_lt),
      len_geq = ifelse(is.null(len_geq), NA_real_, len_geq)
    )
  }
  
  # 1. Test Base Case: Numeric vector of length bins
  res_numeric = rvc:::.funByLen(
    x = rvc_data_list, 
    species = "EPI MORI", 
    length_bins = c(20, 30), 
    cb = dummy_cb
  )
  expect_snapshot(res_numeric)
  
  # 2. Test Recursive/Lookup Case: "LC" (Length at Capture)
  res_lc = rvc:::.funByLen(
    x = rvc_data_list, 
    species = "EPI MORI", 
    length_bins = "LC", 
    cb = dummy_cb
  )
  expect_snapshot(res_lc)
})

test_that(".funByProt output exactly matches baseline", {
  dummy_x = list(stratum_data = data.frame(PROT = c(0, 1)))
  
  # The spy now checks if 'years' was passed through the ... argument
  dummy_cb = function(x, species, length_bins, status, years = NULL, ...) {
    data.frame(
      SPECIES_CD = species,
      status_evaluated = status,
      year_filtered = ifelse(is.null(years), "DROPPED", years)
    )
  }
  
  # We pass years = 2012 into the ...
  res = rvc:::.funByProt(dummy_x, "EPI MORI", NULL, dummy_cb, years = 2012)
  
  expect_snapshot(res)
})

test_that(".getGrowthParameters output and messages exactly match baseline", {
  
  # Create the dummy x list containing taxonomic data
  dummy_x = list(taxonomic_data = taxonomic_data)
  
  # Test 1: NULL parameters (Should capture the message and return taxonomic data)
  expect_snapshot(rvc:::.getGrowthParameters(dummy_x, "EPI MORI", NULL))
  
  # Test 2: Standard list with lowercase 'a' and 'b' (Should rename and capitalize)
  dummy_list = list(a = 0.01, b = 3.0, extra_var = 5)
  expect_snapshot(rvc:::.getGrowthParameters(dummy_x, "EPI MORI", dummy_list))
  
  # Test 3: Dataframe with a mix of cases and common names
  dummy_df = data.frame(
    species_cd = c("EPI MORI", "Black Grouper"), # Mix of code and common name
    a = c(0.01, 0.02),
    b = c(3.0, 3.1)
  )
  expect_snapshot(rvc:::.getGrowthParameters(dummy_x, "EPI MORI", dummy_df))
})

test_that(".checkSpeciesMatch catches missing parameters and filters sample_data", {
  
  # 1. Create a dummy x list with 3 species
  dummy_x = list(
    sample_data = data.frame(SPECIES_CD = c("EPI MORI", "MYC BONA", "LUT GRIS")),
    taxonomic_data = taxonomic_data
  )
  
  # 2. Create a dummy growth parameter table where MYC BONA is missing WLEN_A
  dummy_gp = data.frame(
    SPECIES_CD = c("EPI MORI", "MYC BONA"),
    WLEN_A = c(0.01, NA), 
    WLEN_B = c(3.0, 3.1)
  )
  
  # Test 1: Dataframe with missing parameters (Should print a warning and drop MYC BONA)
  expect_snapshot(rvc:::.checkSpeciesMatch(dummy_x, c("EPI MORI", "MYC BONA"), dummy_gp))
  
  # Test 2: Standard list parameters (Should bypass the check and return x untouched)
  expect_snapshot(rvc:::.checkSpeciesMatch(dummy_x, "EPI MORI", list(WLEN_A = 0.01, WLEN_B = 3.0)))
})