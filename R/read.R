#' Read Write Save
#'
#' Easy read, easy write, easy save.
#' Function from data.table, readxl and ggplot.
#' @rdname read
#'
#' @inheritParams readxl::read_excel
#' @param filename filename
#' @param support_excel TRUE or FALSE
#' @param excel_col_names TRUE to use the first row as column names,
#' FALSE to get default names, or a character vector giving a name for each column.
#' If user provides col_types as a vector, col_names can have one entry per column,
#' i.e. have the same length as col_types, or one entry per unskipped column.
#' @param excel_col_types Either NULL to guess all from the spreadsheet
#' or a character vector containing one entry per column from these options:
#' "skip", "guess", "logical", "numeric", "date", "text" or "list".
#' If exactly one col_type is specified, it will be recycled.
#' The content of a cell in a skipped column is never read
#' and that column will not appear in the data frame output.
#' A list cell loads a column as a list of length 1 vectors,
#' which are typed using the type guessing logic from col_types = NULL,
#' but on a cell-by-cell basis.
#' @param ... others values from ?readxl::read_excel, ?data.table::fread and ?ggsave.
#'
#' @examples
#' ?data.table::fread
#' ?readxl::read_excel
#' @export
yyread <- function(filename,
                   support_excel = TRUE,
                   excel_col_names = TRUE,
                   excel_col_types = NULL,
                   ...) {
    if (support_excel) {
        excel <- c("xls", "xlsx", "xlsm", "xltx", "xltm")
        if (get_ext(filename) %in% excel) {
            tb <- readxl::read_excel(filename,
                                     col_names = excel_col_names,
                                     col_types = excel_col_types,
                                     ...)
        } else {
            tmp <- data.table::fread(filename, ...)
            tb <- tibble::as_tibble(tmp)
        }
    } else {
        tmp <- data.table::fread(filename, ...)
        tb <- tibble::as_tibble(tmp)
    }
    return(tb)
}


#' @rdname read
#'
#' @param x df, matrix
#' @param file Output file name. "" indicates output to the console.
#' @param sep The separator between columns.
#' Default "," for "csv" file; "\\t" otherswise.
#' @param eol Default "\\n", you can also use "\\r\\n" and "\\r".
#'
#' @examples
#' ?data.table::fwrite
#' @export
yywrite <-
    function(x,
             file = "",
             sep,
             eol = c("\n", "\r\n", "\r")) {
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
            sep = sep,
            eol = eol
        )
    }


#' @rdname read
#'
#' @inheritParams ggplot2::ggsave
#' @param compression the type of compression to be used.
#'
#' @examples
#' ?ggplot2::ggsave
#' @export
yysave <- function(plot = last_plot(),
                   filename,
                   path = "plot_dir",
                   width = 8,
                   height = 7.5,
                   units = "cm",
                   dpi = 1200,
                   compression = "lzw",
                   ...) {
    if (!dir.exists(path))
        dir.create(path)
    if (get_ext(filename) %in% c("tiff", "tif")) {
        ggsave(
            filename = filename,
            plot = plot,
            path = path,
            device = "tiff",
            width = width,
            height = height,
            units = units,
            dpi = dpi,
            compression = compression,
            ...
        )
    } else if (get_ext(filename) == "pdf") {
        ggsave(
            filename = filename,
            plot = plot,
            path = path,
            width = width,
            height = height,
            units = units,
            device = grDevices::cairo_pdf,
            ...
        )
    } else {
        ggsave(
            filename = filename,
            plot = plot,
            path = path,
            width = width,
            height = height,
            units = units,
            dpi = dpi,
            ...
        )
    }
}
