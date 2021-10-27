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




#' yyplot
#'
#' Save ggplot object or create plot device.
#' @rdname yyplot
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to last plot displayed.
#' @param width,height Plot size in cm. Defaults to the 8 x 6 cm.
#' @param path Path of the directory to save plot to.
#' @param dpi Plot resolution.
#' @param ... others values from svglite::svglite, grDevices::cairo_pdf,
#' grDevices::tiff, grDevices::jpeg, grDevices::png.
#' @param verbose logical, whether to display prompts.
#'
#' @examples
#' \dontrun{
#' ## build device
#' yydev("tmp001.tif")
#' ggplot(mtcars, aes(mpg, wt)) +
#'     geom_point()
#' dev.off()
#'
#' ## Export the ggplot image directly.
#' ggplot(mtcars, aes(mpg, wt)) +
#'     geom_point()
#' yyexport(filename = "tmp003.pdf")
#' }
#' @export
yydev <- function(filename = "Rplot%03d.tif",
                  width = 8,
                  height = 6,
                  path = "./plot_out",
                  dpi = 600,
                  verbose = TRUE,
                  ...) {
    ## exist
    if (!dir.exists(path)) {
          dir.create(path)
      }
    ## path
    ext <- get_ext(filename)
    fpath <- paste0(path, "/", filename)
    ## if

    if (ext %in% c("svg")) {
        svglite::svglite(
            filename = fpath,
            width = width / 2.54, height = height / 2.54, ...
        )
    } else if (ext %in% c("pdf")) {
        grDevices::cairo_pdf(
            filename = fpath,
            width = width / 2.54, height = height / 2.54, ...
        )
    } else if (ext %in% c("tiff", "tif")) {
        grDevices::tiff(
            filename = fpath,
            width = width, height = height,
            units = "cm", res = dpi, compression = "lzw", ...
        )
    } else if (ext %in% c("jpg", "jpeg")) {
        grDevices::jpeg(
            filename = fpath,
            width = width, height = height,
            units = "cm", res = dpi, ...
        )
    } else if (ext %in% c("png")) {
        grDevices::png(
            filename = fpath,
            width = width, height = height,
            units = "cm", res = dpi, ...
        )
    } else {
        stop("yyexport supports only svg, pdf, tif, jpg and png")
    }
    if (verbose) {
          print("Don't forget to type dev.off().")
      }
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