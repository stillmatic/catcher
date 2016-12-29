#' Get cache directory
#'
#' @importFrom rappdirs user_cache_dir
#' @return location of the cache
#' @export
#'
#' @examples
#' get_cache_dir()
get_cache_dir <- function() {
  path <- rappdirs::user_cache_dir("catcher")
  dir.create(path, showWarnings = F, recursive = T)
  return(path)
}

#' Hash an arbitrary string
#'
#' @importFrom digest sha1
#' @param quer string to hash
#'
#' @return sha1 hash
#'
#' @examples
#' \dontrun{hash_query("sin1234")}
hash_query <- function(quer) {
  return(digest::sha1(quer))
}

#' Check if given key exists in cache
#'
#' @param key hashed representation of function and args
#' @param max_lifetime max age of cached object (days)
#'
#' @return boolean
#'
#' @examples
#' \dontrun{exists_in_cache("de245179163e5245a56484e7207bf3a3469c358b")}
exists_in_cache <- function(key, max_lifetime = 30) {
  file_path <- file_path(get_cache_dir(), key)
  if (file.exists(file_path) && !missing(max_lifetime)) {
    age <- difftime(Sys.time(), file.info(file_path)$mtime, units = "days")
    if (age >= max_lifetime) return(FALSE)
  }
  return(file.exists(file_path))
}

#' Save R object to cache
#'
#' @param df arbitary R object
#' @param key hashed representation of df's generating query
#'
#' @return invisibly returns df
#'
#' @examples
#' \dontrun{save_to_cache(ggplot2::diamonds, "6904e1dbc962a5df3040efd454ade0d564be27ce")}
save_to_cache <- function(df, key) {
  path <- file_path(get_cache_dir(), key)
  write(paste0("saving to cache at ", Sys.time()), file = stderr())
  saveRDS(df, path)
  return(invisible(df))
}

#' Read file from cache.
#'
#' Also includes when the file was created and how old it is.
#'
#' @param key hashed representation of object to look up
#'
#' @return cached file
#'
#' @examples
#' \dontrun{read_from_cache("6904e1dbc962a5df3040efd454ade0d564be27ce")}
read_from_cache <- function(key) {
  path <- file_path(get_cache_dir(), key)
  mtime <- file.info(path)$mtime
  age <- round(difftime(Sys.time(), mtime, units = "days"), 2)
  out_str <- paste0("reading from cache created at ", mtime, "; ",
                    age, " days old.")
  write(out_str, file = stderr())
  return(readRDS(path))
}

# for compatibility with rappdirs
file_path <- function(...) {
  normalizePath(do.call("file.path", as.list(c(...))), mustWork = FALSE)
}

#' Smart matching of functions
#'
#' @importFrom assertthat assert_that
#' @param fun function name, as a string
#' @return the desired function
#'
#' @examples
#' catcher:::match_fun("digest::digest")
#' catcher:::match_fun("sum")
match_fun <- function(fun) {
  fun2 <- NULL
  fun <- as.character(fun)
  assertthat::assert_that(assertthat::is.string(fun))
  if (grepl(":::", fun)) {
    # if in form package:::function
    args <- unlist(strsplit(fun, ":::"))
    fun2 <- utils::getFromNamespace(args[2], args[1])
  } else if (grepl("::", fun)) {
    # in form, package::function
    args <- unlist(strsplit(fun, "::"))
    fun2 <- utils::getFromNamespace(args[2], args[1])
  } else {
    fun2 <- tryCatch({
      get(fun, mode = "function", envir = parent.frame(2))
    }, error = function(err) {
      stop(paste0("Invalid function, ", fun, ", provided."))
    })
  }
  fun2
}
