# getDomainLbar handles Base Case exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD     Lbar   vbar_L   n  nm STAGE_LEVEL
      1 2012 FLA KEYS   EPI MORI 39.23787 3.762196 416 803           2

# getDomainLbar handles Length Bins exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD     Lbar   vbar_L   n  nm STAGE_LEVEL length_class
      1 2012 FLA KEYS   EPI MORI 36.57054 2.632088 416 803           2         < 60
      2 2012 FLA KEYS   EPI MORI 66.80859 8.877297 416 803           2        >= 60
      3 2012 FLA KEYS   EPI MORI 39.23787 3.762196 416 803           2          all

# getDomainLbar handles Split Protected Status exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD     Lbar   vbar_L   n  nm STAGE_LEVEL
      1 2012 FLA KEYS   EPI MORI 39.04978 4.172985 302 587           2
      2 2012 FLA KEYS   EPI MORI 43.05452 5.611855 114 216           2
        protected_status
      1                0
      2                1

# getDomainLbar handles Kitchen Sink Filtering exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD     Lbar     vbar_L   n  nm STAGE_LEVEL length_class
      1 2012 FLA KEYS   OCY CHRY 14.04378 0.20857151 302 587           2         < 23
      2 2012 FLA KEYS   OCY CHRY 28.30589 0.41596779 302 587           2        >= 23
      3 2012 FLA KEYS   OCY CHRY 16.35813 0.79372991 302 587           2          all
      4 2012 FLA KEYS   OCY CHRY 15.45827 0.28304167 114 216           2         < 23
      5 2012 FLA KEYS   OCY CHRY 26.35105 0.08717523 114 216           2        >= 23
      6 2012 FLA KEYS   OCY CHRY 17.49594 0.36057956 114 216           2          all
        protected_status
      1                0
      2                0
      3                0
      4                1
      5                1
      6                1

# Lbar wrappers correctly calculate mixed designs across multiple years

    Code
      res_domain
    Output
        YEAR   REGION SPECIES_CD     Lbar    vbar_L   n  nm STAGE_LEVEL
      1 2012 FLA KEYS   EPI MORI 39.00928  5.461113 302 587           2
      2 2022 FLA KEYS   EPI MORI 33.26651  1.827975 464  NA           1
      3 2012 FLA KEYS   EPI MORI 43.09759  4.887005 114 216           2
      4 2022 FLA KEYS   EPI MORI 41.98497 14.601848 184  NA           1
        protected_status
      1                0
      2                0
      3                1
      4                1

