#' Wrapper to add caching to functions
#'
#' @param fun function name (string)
#' @param query first argument to fun
#' @param use_cache should we use cache this time
#' @param overwrite if cached file exists, should the function run and overwrite?
#' @param max_lifetime max acceptable age of a cached object
#' @param ... other parameters passed to fun
#'
#' @return typical fun, query, ellipsis result
#' @export
#'
#' @examples
#' cache_op("sin", pi)
#' # note: this below only works on windows; just an example of additional params
#' cache_op("read.csv", "https://cdn.rawgit.com/Keno/8573181/raw/7e97f56f521d1f49b966e04457687e87da1b062b/gistfile1.txt", header = T)
cache_op <- function(fun, query,
                     use_cache = T,
                     overwrite = F,
                     max_lifetime = 30,
                     ...) {
  # separate function name and function
  fun2 <- match.fun(fun)

  # run and immediately return the data
  if(!use_cache) {
    return(fun2(query, ...))
  }

  key <- hash_query(paste0(fun, query, ...))
  # load cached version
  if(exists_in_cache(key, max_lifetime) && !overwrite) {
    dat <- read_from_cache(key)
    return(dat)
  } else {
    dat <- fun2(query, ...)
    save_to_cache(dat, key)
    return(dat)
  }
}

#' Get info about the cache
#'
#' Returns table with hashes, size of files (in MB), when each file was last modified, and the age in days of each file.
#'
#' @param summary_only Should we only print summary info?
#'
#' @return info about files in the cache
#' @export
#'
#' @examples
#' cache_info(F)
cache_info <- function(summary_only = F) {
  info <- file.info(dir(get_cache_dir(), full.names = T))
  rownames(info) <- dir(get_cache_dir())
  info$size <- round(info$size / (1000 * 1000), 2) # return MB
  info$age <- round(difftime(Sys.time(), info$mtime, units = "days"), 2)
  summ_str <- paste("Cache has", nrow(info),
                     "files with a total of", sum(info$size), "MB.")
  write(summ_str, file = stderr())
  if(summary_only) {
    return(invisible(summ_str))
  }
  return(info[c("size", "mtime", "age")])
}


#' Delete your entire cache
#'
#' @param really set this to TRUE if you actually want to delete
#'
#' @return an empty cache folder
#' @export
#'
#' @examples
#' \dontrun{delete_cache(T)}
delete_cache <- function(really = F) {
  if(really) {
    write("deleting cache for good!", file = stderr())
    file.remove(dir(get_cache_dir(), full.names = T))
  }
  invisible(NULL)
}
