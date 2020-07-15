## infer 0.5.3

This release fixes failing CRAN checks related to long-double support. This
submission is a resubmission following an automated pretest failure due to
a warning resulting from the newly released sf package.

The previous submission was also a resubmission following an automated pretest 
failure related to visual testing failures on the most recent R-devel version.

## Test environments
* local OS X install, R 4.0.2
* ubuntu 16.04 (on travis-ci), oldrel, release, devel
* win-builder (devel)
* rhub: debian-gcc-devel-nold

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

We checked five reverse dependencies, two of which are on CRAN, with the
remaining on bioconductor, and found no new issues.
