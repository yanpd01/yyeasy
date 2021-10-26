#' Easy get
#'
#' @rdname get
#'
#' @param filename file path
#' @param lower All lowercase
#'
#' @examples
#' get_ext("123.txt")
#' @export
get_ext <- function(filename, lower = TRUE) {
    ext <- tools::file_ext(filename)
    if (lower == TRUE) ext <- tolower(ext)
    ext
}
