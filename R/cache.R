#' Get cache directory
#'
#' @return location of the cache
#' @export
#'
#' @examples
#' get_cache_dir()
get_cache_dir <- function() {
  path <- file.path("~", "ds_cache")
  if(!dir.exists(path)) {
    dir.create(path)
  }
  return(path)
}

#' Hash an arbitrary string
#'
#' Very light wrapper
#'
#' @param query to hash
#'
#' @return sha1 hash
#' @export
#'
#' @examples
#' hash_query("sin1234")
hash_query <- function(query) {
  return(digest::sha1(query))
}

#' Check if given key exists in cache
#'
#' @param key hashed representation of function and args
#'
#' @return boolean
#' @export
#'
#' @examples
#' exists_in_cache("de245179163e5245a56484e7207bf3a3469c358b")
exists_in_cache <- function(key) {
  return(file.exists(file.path(get_cache_dir(), key)))
}

#' Save R object to cache
#'
#' @param df arbitary R object
#' @param key hashed representation of df's generating query
#'
#' @return invisibly returns df
#' @export
#'
#' @examples
#' \dontrun{save_to_cache(ggplot2::diamonds, "6904e1dbc962a5df3040efd454ade0d564be27ce")}
save_to_cache <- function(df, key) {
  path <- file.path(get_cache_dir(), key)
  write(paste0("saving to cache at ", Sys.time()), file = stdout())
  saveRDS(df, path)
  return(invisible(df))  # TODO not sure if we need this
}

#' Read file from cache
#'
#' @param key hashed representation of object to look up
#'
#' @return cached file
#' @export
#'
#' @examples
#' \dontrun{read_from_cache("6904e1dbc962a5df3040efd454ade0d564be27ce")}
read_from_cache <- function(key) {
  path <- file.path(get_cache_dir(), key)
  # TODO: fix path time
  write(paste0("reading from cache created at ", Sys.time()), file = stdout())
  return(readRDS(path))
}
