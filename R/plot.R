

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
#' @param compression tiff option, the type of compression to be used. c("none", "rle", "lzw", "jpeg", "zip", "lzw+p", "zip+p").
#' @param fix_text_size svglite option,
#' if TRUE each string will have the textLength CSS property set to the width
#' calculated by systemfonts and lengthAdjust='spacingAndGlyphs'.
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
                  width = 12,
                  height = 12,
                  path = ".",
                  dpi = 600,
                  verbose = TRUE,
                  compression = "lzw",
                  fix_text_size = FALSE,
                  ...) {
    ## path
    fpath <- paste0(path, "/", filename)
    dir_path <- dirname(fpath)
    if (!dir.exists(dir_path)) {
        dir.create(dir_path)
    }
    ## if
    ext <- get_ext(filename)
    if (ext %in% c("svg")) {
        svglite::svglite(
            filename = fpath,
            width = width / 2.54, height = height / 2.54,
            fix_text_size = fix_text_size, ...
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
            units = "cm", res = dpi,
            compression = compression, ...
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
        message("Don't forget to type dev.off().")
    }
    message(paste0("Image will be saved to '", fpath, "'."))
    invisible()
}



#' @rdname yyplot
#' @export
yyexport <- function(plot = last_plot(),
                     filename = "Rplot%03d.tif",
                     width = 12,
                     height = 12,
                     path = ".",
                     dpi = 600,
                     compression = "lzw",
                     fix_text_size = FALSE,
                     ...) {
    yydev(
        filename = filename,
        width = width,
        height = height,
        path = path,
        dpi = dpi,
        verbose = FALSE,
        compression = compression,
        fix_text_size = fix_text_size,
        ...
    )
    # grid::grid.draw(plot)
    print(plot)
    grDevices::dev.off()
    message(paste("The image size is", width, "*", height, "cm.", sep = " "))
    invisible()
}

#' @rdname yyplot
#' @export
yysave <- yyexport