#' Easily transpose tibble and data.frame and data.table
#'
#' Function from data.table.
#' @rdname transpose
#' @param x an tibble or data.table
#' @param colnames_to_columm The name of the first column in the result containing the colnames of the input.
#' @param column_to_colnames The name or number of a column in the input to use as colnames of the output.
#' @param ... Others params from [data.table::transpose()].
#'
#' @return a transpose tibble
#'
#' @examples
#' tibble::as_tibble(mtcars, rownames = "row_id") %>% t()
#' tibble::as_tibble(mtcars, rownames = "row_id") %>%
#'     t() %>%
#'     t()
#' tibble::as_tibble(mtcars, rownames = "row_id") %>%
#'     data.table::as.data.table() %>%
#'     t()
#'
#' @export
`t.tbl_df` <-
    function(x,
             colnames_to_columm = "col_id",
             column_to_colnames = 1,
             ...) {
        out <- data.table::transpose(
            x,
            keep.names = colnames_to_columm,
            make.names = column_to_colnames,
            ...
        )
        return(tibble::as_tibble(out))
    }


#' @rdname transpose
#'
#' @return a transpose data.table
#' @export
`t.data.table` <-
    function(x,
             colnames_to_columm = "col_id",
             column_to_colnames = 1,
             ...) {
        out <- data.table::transpose(
            x,
            keep.names = colnames_to_columm,
            make.names = column_to_colnames,
            ...
        )
        return(out)
    }

#### Because the original `t` has only one `x` parameter, you need to redeclare s3.

#' @rdname transpose
#' @export
t <- function(x, colnames_to_columm, column_to_colnames, ...) {
    UseMethod("t")
}
