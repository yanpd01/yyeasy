
#' Max min standardization
#'
#' @rdname std
#' @param x Numbers.
#'
#' @return Standardized data
#' @export
#'
#' @examples
#' std_max(1:10)
#' std_min(1:10)
std_max <- function(x){
    ((x-min(x))/(max(x)-min(x)))
}

#' @rdname std
#' @export
std_min <- function(x){
    ((max(x)-x)/(max(x)-min(x)))
}




