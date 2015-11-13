# Reef Visual Census statistical package in R #

## Description ##
The rvc package is designed to compute summary statistics, such as: fish density, frequency of occurrence, and length frequencies for Reef Visual Census data, which can then be output to csv files or turned into graphs.

----
## Installation ##

### Using devtools (preferred) ###
Type the following into R
```
install.packages('devtools')
devtools::install_github('harryganz/rvc')
```
You can skip the first line if you already have devtools installed.

### Using base R ###
1. Download the compressed source folder named "rvc\_&lt;version&gt;.tar.gz" from the root directory of this project.
2. Type the following into R
```
install.packages('<path_to_file>/rvc_<version>.tar.gz', repos = NULL, type = "source")
require(rvc)
```

----
## Example Workflows ##

### Basic Workflow ###
```
library(rvc)

## Download desired Years/Regions data from server
keys11_14 = getRvcData(years = 2011:2014, regions = c("FLA KEYS", "DRTO"))

## Take a look at object structure
str(keys11_14, 1) # 3 data.frames

## Make a list of species
## You can use full scientific names, common names, or
## species codes (first 3 letters of genus, and first 4 of species)
## Only scientific names are case-sensitive
spcs = c("Epinephelus morio", "Black Grouper", "STE VARI")

## Calculate statistics for entire sampling domain
ddens = getDomainDensity(keys11_14, species = spcs)
dabun = getDomainAbundance(keys11_14, species = spcs)
docc = getDomainOccurrence(keys11_14, species = spcs)

## Stratum Level Estimates for Red Grouper in 2011 in
## The Florida Keys
sdens = getStratumDensity(keys11_14, species = "EPI MORI",
                         years = 2011, regions = "FLA KEYS")
```

### Biomass Estimates

Biomass per secondary sampling unit (~177m<sup>2</sup>), and total biomass for the sampling domain can be calculated using allometric growth parameters. Currently, this feature can only be used one species at a time. The equation used to calculate weight at length is:
\\[ W(g) = \\alpha L(mm)^\\beta \\]

This means that the \\(\\alpha\\) and \\(\\beta\\) coefficients used must be for conversion from millimeters to grams, even though the final output in kilograms.
```
require(rvc)

dt2012 = getRvcData(years = 2012, regions = "DRTO")

## Growth parameters for Black Grouper
## MUST BE in a list with named elements 'a' and 'b'
## where 'a' is the linear allometric growth coefficient and
## 'b' is the expontential coefficient
## W(g) = a*L(mm)^b
gp = list(a = 4.27e-6, b = 3.2051)

## Calculate biomass per SSU
dbiom = getDomainBiomass(dt2012, species = "Black Grouper",
                         growth_parameters = gp)
## Calculate total biomass for the sampling domain
dtbiom = getDomainTotalBiomass(dt2012, species = "MYC BONA",
                               growth_parameters = gp)
```
<strong>NOTE:</strong> Currently, provided growth parameters are applied to all species, so biomass estimates can only be calculated for one species at a time.
### Using Filters ###
Filtering data can be used to select a subset of the data.
```
require(rvc)

## Get Data from the Florida Keys in 2011-2012
fk11_12 = getRvcData(years = 2011:2012, regions = "FLA KEYS")

## Display occurrence for hogfish both inside and outside
## protected areas separately
lm_prot = getDomainOccurrence(fk11_12, species = "LAC MAXI",
                              merge_protected = FALSE)
## Yellowtail occurrence only in protected areas
yt_prot = getDomainOccurrence(fk11_12, species = "Ocyurus chrysurus", is_protected = TRUE)
## Red grouper density when present
rg_present = getDomainDensity(fk11_12, species = "Red Grouper", when_present = TRUE)
## Only select data from 2011
mut_2011 = getDomainAbundance(fk11_12, species = "Lut anal", years = 2011)
```

Available Filters:
1. merge_protected: A boolean indicating whether
protected areas should be combined in calculating the
statistic (TRUE), the default, or should be calculated
separately (FALSE).
2. strata: character vector of strata codes to keep.
3. status: character vector of protected statuses to keep.
4. is_protected: A boolean indicating whether only samples from protected areas should be kept (TRUE) or only unprotected areas (FALSE).
5. years: A numeric vector of years to keep.
6. regions: A character vector of region codes to keep. DRTO - Dry Tortugas, FLA KEYS: Florida Keys, SEFCRI - Southeast Peninsular Florida.
7. when_present: A boolean indicating whether only samples in which individuals are seen should be kept (TRUE) or not (FALSE).

### Length Classes and Length Bins ###
Any statistic in the rvc package can be calculated for length classes or in length bins by providing the length_bins option. Using length bins can make calculating statistics significantly slower, so it is not recommended for use with more than a few species at a time. This may change in later versions of this package.
```
require(rvc)

## Get 2013 SEFCRI data from server
sf2013 = getRvcData(years = 2013, region = "SEFCRI")

## Calculate length frequencies for Mangrove Snapper
## for lengths from 0-100cm by 1 cm bins
lglf = getDomainLengthFrequency(sf2013, species = "Lut gris",
                                length_bins = seq(0,100,1))

## Abundance of yellowtail with 26cm cutpoint
ytabun = getDomainAbundance(sf2013, species = "OCY CHRY",
                            length_bins = 26)

## White Grunt abundance from 0-50cm in 1cm bins
## This calculation may take a few minutes
hp2013 = getDomainAbundance(sf2013, species = "HAE PLUM",
                            seq(0,50,1))

```

<script src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>
