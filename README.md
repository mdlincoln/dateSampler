
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

You may also set ranges for each of these components:

``` r
sample_date(year_min = 1875, year_max = 1892, month_min = 1, month_max = 5, n = 5)
```

    ## [1] "1883-01-29" "1885-04-25" "1889-05-03" "1879-02-13" "1882-04-13"

If you pass a range that can only produce illegal dates, however, `sample_date` will throw an error:

``` r
sample_date(year_min = 1875, month_min = 2, month_max = 2, day_min = 30, n = 5)
```

    ## Error in check_args(year_min, year_max, month_min, month_max, day_min, : The following ranges cannot return any valid dates:
    ## Year: 1875-1875
    ## Month: 2-2
    ## Day: 30-31

### Expanding data frames

`sample_date_df` is a convenience function that generates replicates of a given data frame and appends new sampled dates. New dates are sampled rowwise, allowing you to specify different date component restrictions for each row. This can be useful when used as part of a modeling pipeline in which you need to use multiple imputation.

``` r
head(sample_date_df(iris, year_min = 1850:1999, n = 5), n = 10)
```

    ##     Sepal.Length Sepal.Width Petal.Length Petal.Width Species replicate
    ## 1            5.1         3.5          1.4         0.2  setosa         1
    ## 1.1          5.1         3.5          1.4         0.2  setosa         2
    ## 1.2          5.1         3.5          1.4         0.2  setosa         3
    ## 1.3          5.1         3.5          1.4         0.2  setosa         4
    ## 1.4          5.1         3.5          1.4         0.2  setosa         5
    ## 2            4.9         3.0          1.4         0.2  setosa         1
    ## 2.1          4.9         3.0          1.4         0.2  setosa         2
    ## 2.2          4.9         3.0          1.4         0.2  setosa         3
    ## 2.3          4.9         3.0          1.4         0.2  setosa         4
    ## 2.4          4.9         3.0          1.4         0.2  setosa         5
    ##     sampled_date
    ## 1     1850-08-17
    ## 1.1   1850-06-29
    ## 1.2   1850-04-07
    ## 1.3   1850-03-26
    ## 1.4   1850-02-01
    ## 2     1851-12-13
    ## 2.1   1851-06-08
    ## 2.2   1851-06-03
    ## 2.3   1851-10-29
    ## 2.4   1851-04-24

**TBD**: non-standard evaluation of column names to make `sample_date_df` extra pipe-friendly

Custom predicates
-----------------

A custom predicate function can also be supplied to `sample_date` (and `sample_date_df`, if passed inside a `list()`) in order to intorduce more complex restrictions on the dates returned. The predicate function must take a year, month, and day value, and return `FALSE` if the supplied date is invalid, prompting `sample_date` to generate a new date.

For example, if you wished to produce dates that were only Mondays, you could produce a predicate function like so:

``` r
is_monday <- function(y, m, d) {
  lubridate::wday(lubridate::ymd(paste(y, m, d, sep = "-"), quiet = TRUE)) == 2
}

sample_date(1850, n = 5, .p = is_monday, quiet = TRUE)
```

    ## [1] "1850-12-09" "1850-09-16" "1850-06-17" "1850-11-11" "1850-04-22"

Note that the more restrictive the predicate function, the more times `sample_date` may be forced to resample new dates.

------------------------------------------------------------------------

[Matthew Lincoln](http://matthewlincoln.net) | Getty Research Institute
