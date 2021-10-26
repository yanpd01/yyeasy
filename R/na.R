#' Remove all na cols, or rows
#'
#' @rdname na
#' @param df your data
#' @param margin 1 means rm all NA rows, 2 means rm all NA cols,
#' 0 means all rows and cols.
#'
#' @return new data
#'
#' @examples
#' df <- 1:20 %>% matrix(nrow = 5)
#' df[2, 4] <- NA
#' df[5, 2] <- NA
#' df[1, ] <- NA
#' df[, 3] <- NA
#' rownames(df) <- letters[1:5]
#' colnames(df) <- letters[6:9]
#' rm_NAs(df, margin = 1)
#' rm_NAs(df, margin = 2)
#' rm_NAs(df, margin = 0)
#' @export
rm_NAs <- function(df, margin = 0) {
    if (!margin %in% c(0:2)) {
          stop("margin should be one of c(0, 1, 2)")
      }
    if (margin == 0) {
        df <- df[, colSums(is.na(df)) != nrow(df)]
        df <- df[rowSums(is.na(df)) != ncol(df), ]
        return(df)
    } else if (margin == 1) {
        df <- df[rowSums(is.na(df)) != ncol(df), ]
        return(df)
    } else if (margin == 2) {
        df <- df[, colSums(is.na(df)) != nrow(df)]
        return(df)
    }
}