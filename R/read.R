#' Read Write Save
#'
#' Easy read, easy write, easy save.
#' Function from data.table, readxl and ggplot.
#' @rdname read
#'
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
#' @param rownames logical; TRUE means use the first column as rownames,
#' FALSE (default) means no rownames.
#' @param ... others values from ?readxl::read_excel, ?data.table::fread.
#'
#' @examples
#' ?data.table::fread
#' ?readxl::read_excel
#' @export
yyread <- function(filename,
                   rownames = FALSE,
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
    if (rownames)
        tb <- tb %>% tibble::column_to_rownames(var = colnames(tb)[1])
    return(tb)
}


#' @rdname read
#'
#' @param x df, matrix
#' @param file Output file name. "" indicates output to the console.
#' @param sep The separator between columns.
#' Default "," for "csv" file; "\\t" otherswise.
#' @param eol Default "\\n", you can also use "\\r\\n" and "\\r".
#' @param rownames Logical value, whether to save the rownames.
#'
#' @examples
#' ?data.table::fwrite
#' @export
yywrite <-
    function(x,
             file = "",
             sep,
             eol = c("\n", "\r\n", "\r"),
             rownames = FALSE) {
        if (missing(sep)) {
            if (get_ext(file) == "csv") {
                sep <- ","
            } else {
                sep <- "\t"
            }
        }
        if (rownames) {
            x <- as.data.frame(x)
            x <- tibble::rownames_to_column(x)
        }
        eol <- match.arg(eol)
        data.table::fwrite(
            x = x,
            file = file,
            sep = sep,
            eol = eol
        )
    }




#' yyplot
#'
#' Save ggplot object or create plot device.
#' @rdname yyplot
#'
#' @inheritParams ggplot2::ggsave
#' @param compression the type of compression to be used.
#'
#' @examples
#' ?ggplot2::ggsave
#' @export
yysave <- function(plot = last_plot(),
                   filename = "Rplot%03d.tif",
                   width = 8,
                   height = 6,
                   path = "./plot_out",
                   units = c("cm", "in"),
                   dpi = 600,
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

#' @rdname yyplot
#'
#' @param ... others values from svglite::svglite, grDevices::cairo_pdf,
#' grDevices::tiff, grDevices::jpeg, grDevices::png.
#' @param verbose logical, whether to display prompts.
#'
#' @examples
#' \dontrun{
#' ## build device
#' yydev("tmp001.tif")
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point()
#' dev.off()
#'
#' ## Export the ggplot image directly.
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point()
#' yyexport("tmp002.pdf")
#' }
#'
yydev <- function(
    filename = "Rplot%03d.tif",
    width = 8,
    height = 6,
    path = "./plot_out",
    dpi = 600,
    verbose = TRUE,
    ...) {
    ## exist
    if (!dir.exists(path))
        dir.create(path)
    ## path
    ext <- get_ext(filename)
    fpath <- paste0(path, "/", filename)
    ## if

    if (ext %in% c("svg")) {
        svglite::svglite(filename = fpath,
                         width = width/2.54, height = height/2.54, ...)
    } else if (ext %in% c("pdf")) {
        grDevices::cairo_pdf(filename = fpath,
                             width = width/2.54, height = height/2.54, ...)
    } else if (ext %in% c("tiff", "tif")) {
        grDevices::tiff(filename = fpath,
                        width = width, height = height,
                        units = "cm", res = dpi, compression = "lzw", ...)
    } else if (ext %in% c("jpg", "jpeg")) {
        grDevices::jpeg(filename = fpath,
                        width = width, height = height,
                        units = "cm", res = dpi, ...)
    } else if (ext %in% c("png")) {
        grDevices::png(filename = fpath,
                       width = width, height = height,
                       units = "cm", res = dpi, ...)
    } else {
        stop("yyexport supports only svg, pdf, tif, jpg and png")
    }
    if (verbose)
    print("Don't forget to type dev.off().")
    invisible()
}



#' @rdname yyplot
#' @export
yyexport <- function(plot = last_plot(),
                     filename = "Rplot%03d.tif",
                     width = 8,
                     height = 6,
                     path = "./plot_out",
                     dpi = 600,
                     ...) {
    yydev(
        filename = filename,
        width = width,
        height = height,
        path = path,
        dpi = dpi,
        verbose = FALSE,
        ...

    )
    grid::grid.draw(plot)
    grDevices::dev.off()
    invisible()
}





