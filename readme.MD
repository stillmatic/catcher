# cacheR - Drop In Caching for R Functions

*Author:* Chris Hua / `chua@wharton.upenn.edu`

[![Build Status](https://travis-ci.org/stillmatic/cacheR.svg?branch=master)](https://travis-ci.org/stillmatic/cacheR)

Easily speed up repeated expensive operations by saving the results on disk and loading from disk on subsequent runs, with many quality-of-life enhancements for data scientists.

## Installation:

Install from this repository:

```r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("stillmatic/cacheR")
```

## Why cacheR

This package is very light but includes a number of important helpers: advanced management of the cache and when to use the cache. It does not require use of databases or any other dependencies. Its core functions are easy to understand and can easily replace existing code without breaking old functionality. This package additionally takes care of all necessary hashing to store files. 

Existing R caching packages are either too complicated, e.g. [R.cache](https://github.com/HenrikBengtsson/R.cache), or too barebones, e.g. [simpleRCache](https://github.com/cannin/simpleRCache). This package attempts to create a sensible middle ground, with a number of tools essential for data scientists built in. For example, we offer first-class handling of stale cached data and notation of when data was loaded. This is essentially a hashtable held solely on disk and 'instantiated' as your cache directory.

This package was inspired by some of the work I did on [Rbnb](https://medium.com/airbnb-engineering/using-r-packages-and-education-to-scale-data-science-at-airbnb-906faa58e12d) while at Airbnb Data Science this summer. Particular thanks are due to Ricardo Bion and Jason Goodman for their earlier work on this problem.

## Usage

The main function is `cache_op()`:

```r
cache_op("read.csv", 
  "https://cdn.rawgit.com/Keno/8573181/raw/7e97f56f521d1f49b966e04457687e87da1b062b/gistfile1.txt", 
  header = T)
```

This is equivalent to:

```r
read.csv("https://cdn.rawgit.com/Keno/8573181/raw/7e97f56f521d1f49b966e04457687e87da1b062b/gistfile1.txt", 
  header = T)
```

You can also use this library to create wrapped versions of your own functions. Take for example a hypothetical function `sql` that connects to your SQL database:

```r
sql <- function(query, user = Sys.getEnv("db_auth_user"), pass = Sys.getEnv("db_auth_pass")) {
  # ...
}

sql("SELECT * FROM dim_users LIMIT 100")
```

You can wrap this function easily with the following:

```r
sql_c <- function(query, ...) {
  cacheR::cache_op("sql", query, ...)
}

sql_c("SELECT * FROM dim_users LIMIT 100")
```

Easily reproducible proof of concept:

```r
sin_c <- function(query, ...) { 
  cacheR::cache_op("sin", query, ...)
}

sin_c(pi)
# reading from cache created at 2016-09-03 14:03:32; 0.05 days old.
# [1] 1.224606e-16
```

## Options

The function `cache_op()` takes a few additional arguments which customize the behavior of the caching.

* `use_cache` - self explanatory; sometimes you want to bypass the cache and neither load the file from cache nor save the query results to cache. Default is TRUE, i.e. use the caching functionality.
* `overwrite` - this is useful when the file exists in the cache; ie has been cached before, but you want to update the cached value with the newest version.
* `max_lifetime` - the max number of days old a cached file can be. Default is 30 days. If the cached version is older than this, then the function will rerun and overwrite with the new data.

When wrapping your own custom functions with caches, you can modify these parameters however you want. Let's say you have some function `get_ga()` that gathers Google Analytics data for the last week, so you want to redownload the data if it's been more than 7 days since the last update. Then, you would initiate it as:

```r
get_ga_c <- function(query, ...) { 
  cacheR::cache_op("get_ga", max_lifetime = 7, query, ...)
}

# or

get_ga_c <- function(query, max_lifetime = 7, ...) {
  cacheR::cache_op("get_ga", max_lifetime = max_lifetime, query, ...)
}
```
