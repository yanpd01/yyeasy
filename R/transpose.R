#' Easily transpose tibble and data.frame and data.table
#'
#' Function from data.table.
#' @rdname transpose
#' @param x an tibble or data.table
#' @export
tt <- function(x, colnames_to_columm, column_to_colnames, ...) {
    UseMethod("tt")
}

#' @rdname transpose
#' @export
tt.default <- function(x, colnames_to_columm, column_to_colnames, ...) {
    return(t(x))
}


#' @rdname transpose
#' @param colnames_to_columm The name of the first column in the result containing the colnames of the input.
#' @param column_to_colnames The name or number of a column in the input to use as colnames of the output.
#' @param ... Others params from [data.table::transpose()].
#'
#' @return a transpose tibble
#'
#' @examples
#' tibble::as_tibble(mtcars, rownames = "row_id") %>% tt()
#' tibble::as_tibble(mtcars, rownames = "row_id") %>%
#'     tt() %>%
#'     tt()
#' tibble::as_tibble(mtcars, rownames = "row_id") %>%
#'     data.table::as.data.table() %>%
#'     tt()
#'
#' @export
`tt.tbl_df` <-
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
`tt.data.table` <-
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
