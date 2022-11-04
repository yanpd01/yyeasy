#' Easily read files
#'
#' Function from data.table, readxl.
#' @rdname yyread
#'
#' @param filename filename
#' @param rownames Logical; whether to use the first column as the row names.
#' @param header Logical; whether to use the first row as the col names.
#' @param method c("vroom", "fread", "excel"). The function used to read the file, the default is "vroom".
#' It is strongly recommended to use TSV or CSV files rather than Excel files
#' @param sheet Excel option; either a string (the name of a sheet),
#' or an integer (the position of the sheet).
#' @param col_types Excel option; NULL to guess;
#' A character vector containing these options:
#' "skip", "guess", "logical", "numeric", "date", "text" or "list".
#' If exactly one col_type is specified, it will be recycled.
#' The content of a cell in a skipped column is never read
#' @param encoding Default is "unknown". Other possible options are "UTF-8" and "Latin-1".
#' and that column will not appear in the data frame output.
#' @param ... Others params from [vroom::vroom()], [data.table::fread()] or [readxl::read_excel()].
#' Just like "num_threads" in vroom; "nThread" in data.table.
#'
#' @examples
#' \dontrun{
#' yyread("a1.tsv")
#' yyread("a1.xls", method = "excel")
#' }
#' @export
yyread <- function(filename,
                   rownames = FALSE,
                   header = TRUE,
                   method = c("vroom", "fread", "excel"),
                   sheet = NULL,
                   col_types = NULL,
                   encoding = "unknown",
                   ...) {
    method <- match.arg(method)
    if (method == "vroom") {
        tmp <- vroom::vroom(
            filename,
            col_names = header,
            col_types = col_types,
            ...
        )
    } else if (method == "fread") {
        tmp <- data.table::fread(
            filename,
            header = header,
            encoding = encoding,
            ...
        ) %>% tibble::as_tibble()
    } else {
        tmp <- readxl::read_excel(
            filename,
            sheet = sheet,
            col_names = header,
            col_types = col_types,
            ...
        )
    }
    # df <- as.data.frame(tmp)
    df < tmp
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
#' @param rownames.id A string: the name of a new column. Existing rownames are
#' transferred into this column and the row.names attribute is deleted.
#' @param method c("vroom", "fwrite"). The function used to write file, the default is "vroom".
#' @param sep The separator between columns.
#' Default "," for "csv" file; "\\t" otherswise.
#' @param eol Default "\\n", you can also use "\\r\\n" and "\\r".
#' @param ... others params from [vroom::vroom_write()] or [data.table::fwrite()].
#' Just like "num_threads" in vroom; "nThread" in data.table.
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
yywrite <- function(x,
                    file = "",
                    rownames.id,
                    col.names = TRUE,
                    method = c("vroom", "write"),
                    sep,
                    eol = c("\n", "\r\n", "\r"),
                    ...) {
    method <- match.arg(method)
    if (missing(sep)) {
        if (get_ext(file) == "csv") {
            sep <- ","
        } else {
            sep <- "\t"
        }
    }
    eol <- match.arg(eol)
    if (method == "vroom") {
        if (!missing(rownames.id)) x <- tibble::as_tibble(x, rownames = rownames.id)
        vroom::vroom_write(
            x = x,
            file = file,
            col_names = col.names,
            delim = sep,
            eol = eol,
            ...
        )
    } else {
        if (!missing(rownames.id)) x <- tibble::rownames_to_column(as.data.frame(x), var = rownames.id)
        data.table::fwrite(
            x = x,
            file = file,
            col.names = col.names,
            sep = sep,
            eol = eol,
            ...
        )
    }
}
