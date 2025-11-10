# airportr 0.2.0

## Major Improvements

* Modernized CI/CD infrastructure - replaced Travis CI with GitHub Actions
* Significantly expanded test coverage (3 tests → 59 tests)
* Updated airport data from OpenFlights (as of 2025-11-09)
* Increased minimum R version requirement to R >= 4.0.0
* Updated roxygen2 to 7.3.2

## Code Quality

* Refactored repetitive code patterns into helper functions
* Removed redundant `data()` calls throughout codebase
* Extracted magic numbers into named constants for clarity
* Improved error handling and input validation
* Added comprehensive input validation for coordinate functions

## Infrastructure

* Added GitHub Actions workflows for R-CMD-check, test coverage, and pkgdown deployment
* Moved data preparation script to standard `data-raw/` directory
* Updated package documentation to modern roxygen2 standards
* Fixed author role in DESCRIPTION (added "aut")
* Updated README with new CI badges

# airportr 0.1.4

* Added more new airports
* Added a `pkgdown` documentation website

# airportr 0.1.3

* Added a `NEWS.md` file to track changes to the package.
* Added 514 new airports including 78 in the United States, 61 in China, 37 in Australia, and 34 in Greenland of all places. 
* Added ability to limit airport lookups by any ISO country code in addition to country name. Thanks to @danim25 for the suggestion. 
