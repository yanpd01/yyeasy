

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
    print(paste0("Image will be saved to '", fpath, "'"))
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

#' @rdname yyplot
#' @export
yysave <- yyexport