#' yybind
#'
#' Easy bind
#' @rdname yybind
#' @param x the first df
#' @param y the second df
#' @param by item should be the rownames
#'
#' @return cbind df
#'
#' @examples
#' ?cbind
#' ?dplyr::bind_cols
#' @export
bind_cols_identical <- function(x, y, by) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    if (!missing(by)) {
        x <- tibble::remove_rownames(x)
        y <- tibble::remove_rownames(y)
        x <- tibble::column_to_rownames(x, by)
        y <- tibble::column_to_rownames(y, by)
    } else {
        if (!tibble::has_rownames(x) | !tibble::has_rownames(y)) {
            stop(
                "if the input data doesn't have rownames,
                 You should specify a column as ROWNAMES
                 using 'by'='your_id'. ")
        }
    }
    if (identical(rownames(x), rownames(y))) {
        df <- dplyr::bind_cols(x, y)
    } else {
        y <- y[rownames(x),]
        df <- dplyr::bind_cols(x, y)
    }
    if (!identical(rownames(x), rownames(y))) {
        warning(
            "Tables don't have same rownames,
            Tables don't have same rownames,
            Tables don't have same rownames.")
    }
    return(df)
}
