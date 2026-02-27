# rvc 2.0.0 (2026-02-27)

## Major Changes

-   **Defunct `dplyr` Syntax Removed:** Completely replaced the defunct `group_by_(.dots = ...)` standard evaluation syntax with modern tidy evaluation. This resolves the fatal errors introduced in `dplyr` 1.2.0 and ensures the package is fully compatible with the current `tidyverse` ecosystem.
-   **Dependency Modernization:** Removed the retired `plyr` package from the dependencies. All internal data merging and binding operations now use highly optimized `dplyr` functions.
-   **Pipeline Refactor:** All top-level calculation wrappers (Density, Occurrence, Biomass, Abundance, Length Frequency, and Lbar) have been rewritten using modern `magrittr` (`%>%`) pipelines. This makes the internal routing significantly faster, safer, and easier to maintain.

## Bug Fixes

-   **Filter Retention (`.funByProt`):** Fixed a silent bug where user-provided filters (such as `years = ...` or `regions = ...`) were completely ignored and dropped when splitting calculations by protected status.
-   **API URL Generation:** Fixed a bug in `.getData` where URL combinations could be mismatched due to base R vector recycling. It now uses explicit `expand.grid()` logic to guarantee accurate API calls.
-   **Numeric Length Bins:** Fixed an issue in `.funByLen` where passing numeric length bins could trigger a base R coercion warning due to a missing `is.character()` check.
-   **Safe Subsetting:** Fixed edge-case dimension dropping in `.checkSpeciesMatch` when validating growth parameters.

## Under the Hood

-   **Integration Testing:** Introduced a rigorous, automated snapshot testing suite (`testthat`). Every mathematical helper, routing function, and top-level wrapper is now tested against a permanent baseline to guarantee backward compatibility and mathematical accuracy during future updates.
-   **Mixed-Design Validation:** Added complex multi-year, mixed sampling design (1-stage and 2-stage) datasets to the test suite to ensure the underlying variance engines adapt dynamically to different RVC survey architectures.
-   **API Memory Cleanup:** Added `on.exit(unlink())` statements to the `.download_csv` helper to ensure temporary files and unzipped CSVs are automatically deleted from the user's hard drive after an API call completes.