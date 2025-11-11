## Resubmission - v.0.2.2

This is a critical bug fix release addressing a reverse dependency failure:

* Restored explicit `data()` calls in all exported functions
* Fixes "object 'airports' not found" error when functions are called via `::` from other packages
* Resolves CRAN check failures in the 'footprint' package

Previous fixes (0.2.1):
* Fixed ODbL license URL (changed from 1.0/ to 1-0/ to avoid 301 redirect)
* Fixed OpenFlights data URL (changed from data.html to data#airport to fix 404)

## Update - v.0.2.0

This is a major modernization release with significant improvements:

* Modernized CI/CD infrastructure (replaced Travis CI with GitHub Actions)
* Significantly expanded test coverage (3 tests → 84 tests, 92.78% coverage)
* Updated airport data from OpenFlights (as of 2025-11-09)
* Refactored codebase with improved error handling and input validation
* Updated minimum R version requirement to R >= 4.0.0 (breaking change)
* Updated roxygen2 to 7.3.2 and modernized package documentation

## Test environments
* macOS (latest), R 4.5.2
* Windows (latest), R 4.5.2
* Ubuntu (latest), R-devel, R-release, R-oldrel-1
* GitHub Actions: R-CMD-check workflow

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Reverse dependencies

There are no reverse dependencies.

---

## Previous releases

### Update - v.0.1.3

* Adds some minor new functionality requested by users
* Updates package data with newer information

