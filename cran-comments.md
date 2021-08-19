## Test environments
* local R installation, R 4.0.2
## Test environments
* R-hub windows-x86_64-devel (r-devel)
* R-hub ubuntu-gcc-release (r-release)
* R-hub fedora-clang-devel (r-devel)

## R CMD check results
On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
checking CRAN incoming feasibility ... NOTE
Maintainer: 'Kate Finucane <kate.finucane@ucdconnect.ie>'

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  metabolomic (27:7)

  Metabolomic (2:50)
CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-05-20 as requires 'gWidgets'
  Which is orphaned and soon to be archived.

0 errors | 0 warnings | 1 note x

## Explanations

* The word "metabolomic" (or "Metabolomic" in uppercase) is not a misspelling. 

* This is an update to the archived MetSizeR package which restores its functionality on a Shiny framework as the previous GUI used relied on a dependency which is no longer maintained.
