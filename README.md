> **As of February 2026 the main server housing the raw text files were moved**
> 
> **Please reinstall this package if you are unable to sucessfully run getRvcData() function**
> 
> **Run devtools::install_github('jeremiaheb/rvc') to update to latest package version 1.3.0**


# Reef Visual Census statistical package in R #

## Description ##
The rvc package is designed to compute summary statistics, such as: fish density, frequency of occurrence, and length frequencies for Reef Visual Census data, which can then be output to csv files or turned into graphs.

----
## Installation ##

### Using devtools (preferred) ###
Type the following into R
```
install.packages('devtools')
devtools::install_github('jeremiaheb/rvc')
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
keys11_14 = getRvcData(years = 2011:2014, regions = c("FLA KEYS", "DRY TORT"))

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

Biomass per secondary sampling unit (~177m<sup>2</sup>), and total biomass for the sampling domain can be calculated using allometric growth parameters. The equation used to calculate weight at length is: </br>
W(g) = a\*L(mm)^b

This means that the 'a' and 'b' coefficients used must be for conversion from millimeters to grams, even though the final output is in kilograms (see function help files for more information).
```
require(rvc)

dt2012 = getRvcData(years = 2012, regions = "DRY TORT")

## Calculate biomass per SSU (177m^2) and Total Biomass (per domain)
## for Black and Red Grouper Using data from server
groupers = c("EPI MORI", "MYC BONA")
dbiom = getDomainBiomass(dt2012, species = groupers)
dtbiom = getDomainTotalBiomass(dt2012, species = groupers)
## To take a look at the growth_parameters used
gp1 = subset(dt2012$taxonomic_data, SPECIES_CD %in% toupper(groupers),
  select = c("SPECIES_CD", "WLEN_A", "WLEN_B"))

## Using the rfishbase package, enter custom lookup table for
## Black and Red grouper and recalculate growth parameters
## NOTE: The weighting methods used here are not sufficient where records are
## doubtful or growth parameters vary by location
require(rfishbase)
require(plyr)
## Get length-weight parameters from fishbase
lw = length_weight(c("Epinephelus morio", "Mycteroperca bonaci"))
## Create a table of parameters
## Note the unit conversion of 'a' from cm->g to mm->g by
## Dividing 'a' by 10^b
gp2 = ddply(lw, c("sciname"), summarize,
  WLEN_A = exp(weighted.mean(log(a/10^b), Number)), # Weighted geometric mean
  WLEN_B = weighted.mean(b, Number)) # Weighted arithmetic mean
## Generate species codes from scientific names
gp2$SPECIES_CD = toupper(paste(substr(gp2$sciname, 1, 3),
sapply(strsplit(gp2$sciname, " "), function(x){substr(x[[2]],1,4)}), sep = " "))
## Use custom table in biomass functions
dbiom2 = getDomainBiomass(dt2012, species = groupers, growth_parameters = gp2)
dtbiom2 = getDomainTotalBiomass(dt2012, species = groupers,
   growth_parameters = gp2)

## If you would like to apply the same growth_parameters to multiple species
## or are only using one species, you can use a list instead of a lookup table
## In this case we can get the biomass of Cocoa Damselfish
## by inputing the growth parameters as a list
gp3 = list(WLEN_A = 2.346e-5, WLEN_B = 2.98)
dbiom3 = getDomainBiomass(dt2012, species = "STE VARI",
 growth_parameters = gp3)
dtbiom3 = getDomainTotalBiomass(dt2012, species = "STE VARI",
 growth_parameters = gp3)

```
### Using Filters and Grouping Species ###
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
yt_prot = getDomainOccurrence(fk11_12, species = "Ocyurus chrysurus",
 is_protected = TRUE)
## Red grouper density when present
rg_present = getDomainDensity(fk11_12, species = "Red Grouper",
 when_present = TRUE)
## Only select data from 2011
mut_2011 = getDomainAbundance(fk11_12, species = "Lut anal", years = 2011)
```

The group parameter can be used to calculate a statistic per group instead of 
per species. This can be useful in calculating biomass or abundance per genus
or family, or for a trophic grouping. 
```
require(rvc)

## Get data for the Dry Tortugas from 2000 - 2006
dt2000_8 = getRvcData(years = 2000:2006, region = "DRY TORT")

## Create a lookup table for snapper/grouper families
lookup = subset(dt2000_8$taxonomic_data, FAMILY %in% c("Lutjanidae", "Serranidae"),
  select = c("SPECIES_CD", "FAMILY"))

## Calculate snapper/grouper abundance 
sngpAbun = getDomainAbundance(dt2000_8, species = lookup$SPECIES_CD, group = lookup)

## You can also create your own lookup tables, with species 
## names in the first column and group names in the second
lookup2 = data.frame(species = c("Lachnolaimus maximus", "Epinephelus morio"),
group = rep("Custom Grouping",2))

sngpBiom = getDomainBiomass(dt2000_8, species = lookup2$species, group = lookup2)
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
6. regions: A character vector of region codes to keep. DRY TORT - Dry Tortugas, FLA KEYS: Florida Keys, SEFCRI - Southeast Peninsular Florida.
7. when_present: A boolean indicating whether only samples in which individuals are seen should be kept (TRUE) or not (FALSE).
8. group: A lookup table (data.frame) - the first column of which is a list of species codes, scientific names, or common names, and the second column of which is a list of names by which to group the species (e.g. Family names, trophic groups, etc). If this option is used instead of the statistic being calculated per species, it will be calculated per group.

### Length Classes and Length Bins ###
Any statistic in the rvc package can be calculated for length classes or in length bins by providing the length_bins option. Be aware that using length bins can make calculating statistics significantly slower.

You have three options in calculating length specific statistics:

1. You can use keywords "lc" and "lm" to calculate statistics above and below the minimum length-at-caputure and median length-at-maturity, respectively, assuming the values are available in the database.
2. You can pass in your own number or sequence of numbers which will serve as breakpoints, in cm, above, below, and between which the statistic will be calculated for all species in the species argument combined. This is the most useful when binning data.
3. You can pass in a custom lookup table with species names and breakpoints (in cm) for each species. The lookup table will be used to calculate the statistic above and below the breakpoint for each species separately.

**NOTE:** Only option 2 can be used in the getDomainLengthFrequency and getStratumLengthFreqency
functions.

**WARNING**: The variance associated with data in small length bins could be
large and/or misleading.
```
require(rvc)

## Get 2013 SEFCRI data from server
sf2013 = getRvcData(years = 2013, region = "SEFCRI")

## Calculate length frequencies for Mangrove Snapper
## for lengths from 0-100cm by 1 cm bins
lglf = getDomainLengthFrequency(sf2013, species = "Lut gris",
                                length_bins = seq(0,100,1))

## Abundance of yellowtail above/below length at maturity (from database)
ytabun = getDomainAbundance(sf2013, species = "OCY CHRY", length_bins = "lm")

## Abundance of yellowtail with 26cm cutpoint
ytabun2 = getDomainAbundance(sf2013, species = "OCY CHRY",
                            length_bins = 26)
## Create a custom lookup table and use it to set length classes
## for Mutton and Mangrove Snapper when calculating densities
lb = data.frame(SPECIES_CD = c("LUT ANAL", "LUT GRIS"), CUT = c(40, 30))
snappers2013 = getDomainDensity(sf2013, c("LUT ANAL", "LUT GRIS"),
  length_bins = lb)

## White Grunt abundance from 0-50cm in 1cm bins
## This calculation may take a few minutes
hp2013 = getDomainAbundance(sf2013, species = "HAE PLUM",
                            seq(0,50,1))
```

## Just Getting the Data ##

If you would like to get the sample data or benthic data directly from the server
and save it as a data.frame you can do that as well.
The data is divided into four parts:

1. Sample Data: Diver collected counts and geographic information relaiting to the
fish survey.
2. Stratum data: Meta information on the number of primary sample units per stratum
and the size of each primary sample unit.
3. Benthic Data: Data collected by diver on the vertical relief, and composition
of the benthos at each secondary sampling unit.
4. Taxonomic Data: Taxonomic and life history information for species observed in
the Reef Visual Census.

The functions to access this data are shown in the example below:
```
## Get taxonomic data for all species as a data.frame
taxonomic_data = getTaxonomicData()

## Get sample data for a range of years and regions
keys00_14 = getSampleData(years = 2000:2014, regions = c("DRY TORT", "FLA KEYS"))

## Get stratum info for a range of years and regions
ntot00_14 = getStratumData(years = 2000:2014, regions = c("DRY TORT", "FLA KEYS"))

## Get benthic data for the Florida Keys in 2011
benthic11 = getBenthicData(years = 2011, regions = "FLA KEYS")
```

## List of Functions ##

* getSampleData - Retrieves sample data from server as data.frame.
* getStratumData - Retrieves stratum information from server as data.frame.
* getBenthicData - Retrieves benthic data from server as data.frame.
* getTaxonomicData - Retrieves taxonomic and life history data from server as data.frame.
* getRvcData - Retrieves sample, stratum, and taxonomic data from server as list.
* getStratumDensity - Calculates average density per secondary sampling unit for each stratum.
* getDomainDensity - Calculates average density per secondary sampling unit for each sampling domain.
* getStratumOccurrence - Calculates average occurrence per secondary sampling unit for each stratum.
* getDomainOccurrence - Calculates average occurrence per secondary sampling unit for each sampling domain.
* getStratumAbundance - Calculates total number of individuals per stratum.
* getDomainAbundance - Calculates total number of individuals per sampling domain.
* getStratumBiomass - Calculates average biomass per secondary sampling for each stratum.
* getDomainBiomass - Calculates average biomass per secondary sampling unit for each sampling domain.
* getStratumTotalBiomass - Calculates total biomass per stratum.
* getDomainTotalBiomass - Calculates total biomass per sampling domain.
* getStratumLengthFreqency - Calculates length frequency per stratum.
* getDomainLengthFrequency - Calculates length frequency per sampling domain.
