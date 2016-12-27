#' Delete your entire cache
#'
#' @param really set this to TRUE if you actually want to delete
#'
#' @return an empty cache folder
#' @export
#'
#' @examples
#' \dontrun{delete_cache(TRUE)}
delete_cache <- function(really = FALSE) {
  if(really) {
    write("deleting cache for good!", file = stderr())
    file.remove(dir(get_cache_dir(), full.names = T))
  }
  invisible(NULL)
}
