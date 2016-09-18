library(lubridate)
library(dplyr)

set.seed(101)

# Generate a range of dates, and then randomly sprinkle NAs

# Date generation code adapted from Dirk Eddelbuettel:
# http://stackoverflow.com/a/14721124/3547541
gen_date <- function(i, sd, ed) {
  dt <- difftime(ed, sd, units = "days")
  ev <- runif(i, 0, dt)
  sd + ev
}

# Salt vector with NAs
salt_na <- function(x, m) {
  i <- sample.int(length(x), size = m, replace = FALSE)
  x[i] <- NA
  x
}

size <- 25

dates <- data.frame(core = gen_date(size, sd = ymd("2000-05-07"), ed = ymd("2001-08-30")))
dates_na <- dates %>%
  mutate(
    added = core + sample(1:800, size),
    ye = year(core),
    yl = year(added),
    me = month(core),
    ml = month(added),
    de = mday(core),
    dl = mday(added)) %>%
  mutate_each(funs(salt_na(., 10)), me:dl) %>%
  select(-core, -added) %>%
  mutate_each(funs(as.integer), everything())

devtools::use_data(dates_na, overwrite = TRUE)
