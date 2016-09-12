
dateSampler
===========

[![Travis-CI Build Status](https://travis-ci.org/mdlincoln/dateSampler.svg?branch=master)](https://travis-ci.org/mdlincoln/dateSampler) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mdlincoln/dateSampler?branch=master&svg=true)](https://ci.appveyor.com/project/mdlincoln/dateSampler)

Generate randomized dates within separately-specified ranges of years, months, and days.

Data generated from archival sources often contains fragmentary dates, in which many records may be precise to the day, but some will be missing components such as the precise day, the month, or even the exact year.

\[Multiple imputation\]\[mi\] can be used to simulate the effect of these missing values when producing models based on such fragmentary data. Unlike assinging categorical or numeric values, however, generating random dates under certain restrictions can be difficult. dateSampler generates randomized dates that abide to restrictions on the year, month, and day components of a given date.

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("mdlincoln/dateSampler")
```

Usage
-----

Say you have a record that falls within January of 1875:

``` r
set.seed(101)
library(dateSampler)
sample_date(year_min = 1875, year_max = 1875, month_min = 1, month_max = 1, n = 5)
```

    ## [1] "1875-01-12" "1875-01-02" "1875-01-23" "1875-01-21" "1875-01-08"

However, it may also be that you know the year and the day of the record, but not the month. If this day happens to place an implicit restriction on the month (e.g. `31` rules out September, April, June, November, and February as a possible month), `sample_date` similarly restricts its results, regenerating a new date if its initial pass returns an illegal combination:

``` r
sample_date(year_min = 1875, year_max = 1875, day_min = 31, day_max = 31, n = 5)
```

    ## Regenerating 1 illegal date...
    ## Regenerating 1 illegal date...
    ## Regenerating 1 illegal date...
    ## Regenerating 1 illegal date...

    ## [1] "1875-12-31" "1875-08-31" "1875-05-31" "1875-08-31" "1875-07-31"

(Messages may be suppressed by passing `quiet = TRUE`)

You may also set ranges for each of these components

``` r
sample_date(year_min = 1875, year_max = 1892, month_min = 1, month_max = 5, n = 5)
```

    ## [1] "1883-01-29" "1885-04-25" "1889-05-03" "1879-02-13" "1882-04-13"

### Expanding data frames

TBD

------------------------------------------------------------------------

[Matthew Lincoln](http://matthewlincoln.net) | Getty Research Institute
