#' Wrapper to add caching to functions
#'
#' @param fun function name (string)
#' @param quer first argument to fun
#' @param use_cache should we use cache this time
#' @param overwrite if cached file exists, should the function run and overwrite?
#' @param max_lifetime max acceptable age of a cached object, in days
#' @param ... other parameters passed to fun
#'
#' @return typical fun, query, ellipsis result
#' @export
#'
#' @examples
#' cache_op("sin", pi)
#' cache_op("read.csv",
#' "http://www.sample-videos.com/csv/Sample-Spreadsheet-10-rows.csv",
#' header = TRUE)
cache_op <- function(fun, quer,
                     use_cache = TRUE,
                     overwrite = FALSE,
                     max_lifetime = 30,
                     ...) {
  # match string to actual function
  fun2 <- match_fun(fun)
  # run and immediately return the data
  if (!use_cache) {
    return(fun2(quer, ...))
  }
  # load cached version
  key <- hash_query(paste0(fun, quer, ..., collapse = ""))
  if (exists_in_cache(key, max_lifetime) && !overwrite) {
    dat <- read_from_cache(key)
  } else {
    dat <- fun2(quer, ...)
    save_to_cache(dat, key)
  }
  return(dat)
}

#' Get info about the cache
#'
#' Returns table with hashes, size of files (in MB), when each file was last modified, and the age (in days) of each file.
#' If the cache folder is empty, or doesn't exist (i.e. not created yet), this returns an empty dataframe.
#'
#' @param summary_only Should we only print summary info?
#'
#' @return info about files in the cache
#' @export
#'
#' @examples
#' cache_info(FALSE)
cache_info <- function(summary_only = FALSE) {
  info <- file.info(dir(get_cache_dir(), full.names = T))
  rownames(info) <- dir(get_cache_dir())
  info$size <- round(info$size / (1000 * 1000), 2) # return MB
  info$age <- round(difftime(Sys.time(), info$mtime, units = "days"), 2)
  summ_str <- paste("Cache has", nrow(info),
                     "files with a total of", sum(info$size), "MB.")
  write(summ_str, file = stderr())
  if (summary_only) {
    return(invisible(summ_str))
  }
  return(info[c("size", "mtime", "age")])
}
