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


#' @rdname get
#' @param df your data
#' @param level significance level
#' @param na Whether to display negative numbers as NA.
#'
#' @return Convert significance numbers to ***.
#'
#' @examples
#' get_sig(c(-1,0.00001, 0.0001, 0.001, 0.01, 0.04), level = 5, na = FALSE)
#' get_sig(c(-1,0.00001, 0.0001, 0.001, 0.01, 0.04), level = 5, na = TRUE)
#' @export
get_sig <- function(df ,level=3, na = FALSE) {
    ## get address
    add_0 <- df >= 0.05
    add_N <- df < 0
    add_1 <- df < 0.05   & df >= 0
    add_2 <- df < 0.01   & df >= 0
    add_3 <- df < 0.001  & df >= 0
    add_4 <- df < 0.0001 & df >= 0
    ##
    df[add_N] <- ""
    if (na == TRUE) df[add_N] <- NA

    df[add_0] <- ''
    df[add_1] <- '*'
    if (level == 1) return(df)
    df[add_2] <- '**'
    if (level == 2) return(df)
    df[add_3] <- '***'
    if (level == 3) return(df)
    df[add_4] <- '****'
    if (level == 4) return(df)
    df
}
