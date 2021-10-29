#' Easily read files
#'
#' Function from data.table, readxl.
#' @rdname yyread
#'
#' @param filename filename
#' @param rownames Logical; whether to use the first column as the row names.
#' @param header Logical; whether to use the first row as the col names.
#' @param excel Logical; whether to read Excel files.
#' It is strongly recommended to use TSV or CSV files rather than Excel files
#' @param sheet Excel option; either a string (the name of a sheet),
#' or an integer (the position of the sheet).
#' @param col_types Excel option; NULL to guess;
#' A character vector containing these options:
#' "skip", "guess", "logical", "numeric", "date", "text" or "list".
#' If exactly one col_type is specified, it will be recycled.
#' The content of a cell in a skipped column is never read
#' and that column will not appear in the data frame output.
#' @param ... Others values from ?readxl::read_excel and ?data.table::fread.
#'
#' @examples
#' \dontrun{
#' yyread("a1.tsv")
#' yyread("a1.xls", excel = TRUE)
#' }
#' @export
yyread <- function(filename,
                   rownames = FALSE,
                   header = TRUE,
                   excel = FALSE,
                   sheet = NULL,
                   col_types = NULL,
                   ...) {
    if (excel) {
        tmp <- readxl::read_excel(filename,
            sheet = sheet,
            col_names = header,
            col_types = col_types,
            ...
        )
    } else {
        tmp <- data.table::fread(filename, header = header, ...)
    }
    df <- as.data.frame(tmp)
    if (rownames) {
        df <- df %>%
            tibble::column_to_rownames(var = colnames(df)[1])
    }
    return(df)
}


#' Easily write files
#'
#' Function from data.table.
#' @rdname yywrite
#'
#' @inheritParams data.table::fwrite
#' @param sep The separator between columns.
#' Default "," for "csv" file; "\\t" otherswise.
#' @param eol Default "\\n", you can also use "\\r\\n" and "\\r".
#' @param ... others values from ?data.table::fwrite.
#'
#' @examples
#' \dontrun{
#' a1 <- data.frame(matrix(1:9, 3))
#' yywrite(a1, "a1.tsv")
#' yywrite(a1, "a1.csv")
#' rownames(a1) <- letters[1:3]
#' yywrite(a1, "a1_name.tsv", row.names = TRUE)
#' yywrite(a1, "a1_name_no_colname.tsv", row.names = TRUE, col.names = FALSE)
#' }
#' @export
yywrite <-
    function(x,
             file = "",
             row.names = FALSE,
             col.names = TRUE,
             sep,
             eol = c("\n", "\r\n", "\r"),
             ...) {
        if (missing(sep)) {
            if (get_ext(file) == "csv") {
                sep <- ","
            } else {
                sep <- "\t"
            }
        }
        eol <- match.arg(eol)
        data.table::fwrite(
            x = x,
            file = file,
            row.names = row.names,
            col.names = col.names,
            sep = sep,
            eol = eol,
            ...
        )
    }
