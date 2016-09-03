#' Wrapper to add caching to functions
#'
#' @param fun function name (string)
#' @param query first argument to fun
#' @param use_cache should we use cache this time
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
                     ...) {
  fun2 <- match.fun(fun)
  # run and immediately return the data
  if(!use_cache) {
    return(fun2(query, ...))
  }

  key <- hash_query(paste0(fun, query, ...))
  # load cached version
  if(exists_in_cache(key)) {
    dat <- read_from_cache(key)
    return(dat)
  } else {
    dat <- fun2(query, ...)
    save_to_cache(dat, key)
    return(dat)
  }
}
