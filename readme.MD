# cacheR - Drop In Caching for R Functions

*Author:* Chris Hua / paste("chua", "wharton.upenn.edu", sep = "@")

Easily speed up repeated expensive operations by saving the results on disk and loading from disk on subsequent runs. Written in base R to ensure cross-platform compatibility.

## Installation:

Install from this repository:

```r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("stillmatic/cacheR")
```

## Usage

The main function is `cache_op`:

```r
cache_op("read.csv", "https://cdn.rawgit.com/Keno/8573181/raw/7e97f56f521d1f49b966e04457687e87da1b062b/gistfile1.txt", header = T)
```

This is equivalent to:

```r
read.csv("https://cdn.rawgit.com/Keno/8573181/raw/7e97f56f521d1f49b966e04457687e87da1b062b/gistfile1.txt", header = T)
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
  cache_op("sql", query, ...)
}

sql_c("SELECT * FROM dim_users LIMIT 100")
```

Easily reproducible proof of concept:

```r
sin_c <- function(query, ...) { cache_op("sin", query, ...)}

sin_c(pi)
# reading from cache created at 2016-09-03 14:03:32; 0.05 days old.
# [1] 1.224606e-16
```

## Why cacheR

This library is very light and "just works". It does not require use of databases or any other dependencies. Its core functions are easy to understand and can easily replace existing code without breaking old functionality. This package additionally takes care of all necessary hashing.

There is no setup necessary but further customizability is on the roadmap.
