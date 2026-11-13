#' Index files in a directory tree
#'
#' Searches one or more directories recursively and returns a data frame with
#' one row per match: its name and its absolute folder path. It can also add
#' a content-based hash id.
#'
#' @param path Character vector of directories to search. Relative paths are
#'   resolved against the working directory.
#' @param filter Regular expression to match file names, as in
#'   [list.files()]. This is a regex, not a wildcard pattern: use
#'   `"\\.csv$"` or `glob2rx("*.csv")` rather than `"*.csv"`.
#' @param add.id Logical. If `TRUE`, adds an `id` column with the MD5 hash of
#'   each file's contents. Identical files get the same id. Each file is read
#'   in full, so this can be slow for many or large files.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{file}{File (or folder) name.}
#'     \item{path}{Absolute path of the containing folder, with `/` as separator.}
#'     \item{id}{Only if `add.id = TRUE`: MD5 hash of the file contents; `NA`
#'       for folders.}
#'   }
#'   Folders that match `filter` are included as rows. If nothing matches,
#'   a data frame with zero rows is returned.
#'
#' @examples
#' \dontrun{
#' sfsindex("data", "\\.csv$")
#' sfsindex(c("data/raw", "data/processed"), "\\.xlsx?$", add.id = TRUE)
#' }
#'
#' @md
#' @import digest
#' @export
sfsindex <- function(path, filter, add.id = FALSE){

  flist = list.files(path, pattern = filter, recursive = TRUE, include.dirs = TRUE,
                     full.names = TRUE)
  flist = normalizePath(flist, winslash = "/", mustWork = FALSE)

  findex = data.frame(
    file = basename(flist),
    path = dirname(flist),
    stringsAsFactors = FALSE
  )

  if (add.id) {
    # hash of the file contents: identical files get the same id
    # directories (from include.dirs = TRUE) have no contents and get NA
    findex$id = vapply(
      flist,
      function(f) {
        if (dir.exists(f)) NA_character_ else digest::digest(f, algo = "md5", file = TRUE)
      },
      character(1),
      USE.NAMES = FALSE
    )
  }

  findex
}
