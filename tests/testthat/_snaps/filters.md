# species_filter output exactly matches baseline

    Code
      species_filter(species_data, "EPI MORI")
    Output
        SPECIES_CD
      1   EPI MORI

---

    Code
      species_filter(species_data, c("EPI MORI", "MYC BONA"))
    Output
        SPECIES_CD
      1   EPI MORI
      2   MYC BONA

---

    Code
      species_filter(species_data)
    Output
        SPECIES_CD
      1   EPI MORI
      2   MYC BONA

# strata_filter output exactly matches baseline

    Code
      strata_filter(stratum_data, "FSLR")
    Output
        STRAT
      1  FSLR

---

    Code
      strata_filter(stratum_data, c("FSLR", "FDLR"))
    Output
        STRAT
      1  FDLR
      2  FSLR

---

    Code
      strata_filter(stratum_data)
    Output
        STRAT
      1  FDLR
      2  FSLR

# protected_filter output exactly matches baseline

    Code
      protected_filter(prot_data)
    Output
        PROT
      1    1
      2    0
      3    2

---

    Code
      protected_filter(prot_data, 0)
    Output
        PROT
      1    0

---

    Code
      protected_filter(prot_data, c(0, 1))
    Output
        PROT
      1    1
      2    0

---

    Code
      protected_filter(prot_data, is_protected = TRUE)
    Output
        PROT
      1    1
      2    2

---

    Code
      protected_filter(prot_data, is_protected = FALSE)
    Output
        PROT
      1    0

# year_filter output exactly matches baseline

    Code
      year_filter(year_data)
    Output
        YEAR
      1 2000
      2 2001

---

    Code
      year_filter(year_data, years = 2001)
    Output
        YEAR
      1 2001

# region_filter output exactly matches baseline

    Code
      region_filter(region_data)
    Output
          REGION
      1 FLA KEYS
      2     DRTO

---

    Code
      region_filter(region_data, region = "FLA KEYS")
    Output
          REGION
      1 FLA KEYS

# length_filter output exactly matches baseline

    Code
      length_filter(length_data)
    Output
        LEN NUM
      1   0   1
      2   1   2
      3   2   3
      4   3   4

---

    Code
      length_filter(length_data, len_geq = 2)
    Output
        LEN NUM
      1   0   0
      2   1   0
      3   2   3
      4   3   4

---

    Code
      length_filter(length_data, len_lt = 3)
    Output
        LEN NUM
      1   0   1
      2   1   2
      3   2   3
      4   3   0

---

    Code
      length_filter(length_data, len_lt = 3, len_geq = 2)
    Output
        LEN NUM
      1   0   0
      2   1   0
      3   2   3
      4   3   0

# count_filter output exactly matches baseline

    Code
      count_filter(count_data)
    Output
         NUM
      1 0.00
      2 0.25
      3 1.00
      4 3.00

---

    Code
      count_filter(count_data, cnt_geq = 2e-06)
    Output
         NUM
      1 0.25
      2 1.00
      3 3.00

---

    Code
      count_filter(count_data, cnt_lt = 1)
    Output
         NUM
      1 0.00
      2 0.25

---

    Code
      count_filter(count_data, cnt_geq = 2e-06, cnt_lt = 3)
    Output
         NUM
      1 0.25
      2 1.00

---

    Code
      count_filter(count_data, when_present = TRUE)
    Output
         NUM
      1 0.25
      2 1.00
      3 3.00

