#' ggTheme
#'
#' Custom ggTheme
#' @rdname ggtheme
#'
#' @param font_size font_size
#' @param font  font: sans = Arial, serif = Times New Roman, any other fonts
#' @param line_size line_size
#'
#' @importFrom ggplot2 %+replace% ggsave last_plot
#' element_blank element_line element_rect element_text
#' theme theme_bw theme_classic
#'
#' @export
theme_bw2 <- function(font_size = 10,
                      font = "sans",
                      line_size = 0.5) {
    ## 四周型主题
    theme_bw() %+replace%
        theme(
            text = element_text(size = font_size, family = font),
            # legend
            legend.title = element_text(size = font_size + 1),
            legend.key = element_blank(),
            legend.background = element_blank(),
            legend.text = element_text(size = font_size),
            # axis
            axis.text = element_text(size = font_size, color = "black"),
            axis.title = element_text(size = font_size + 1, vjust = 1),
            axis.line = element_blank(),
            # panel
            panel.border = element_blank(),
            panel.background = element_rect(size = (line_size * 2), color = "black", fill = "white"),
            panel.grid = element_blank(),
            # strip
            strip.background = element_rect(fill = "grey85", color = "grey20"),
            # plot
            plot.background = element_blank(),
            complete = TRUE
        )
}


#' @rdname ggtheme
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, disp, color = cyl)) +
#'     geom_point()
#' p + theme_bw2(font_size = 15, line_size = 2)
#' p + theme_classic2(font_size = 15, line_size = 2)
#' @export
theme_classic2 <- function(font_size = 10,
                           font = "sans",
                           line_size = 0.5) {
    ## 常规xy轴主题
    theme_bw2(font_size = font_size, font = font, line_size = line_size) %+replace%
        theme(
            ## axis
            axis.line = element_line(size = line_size),
            ## panel
            panel.background = element_rect(color = NA, fill = "white"),
            complete = TRUE
        )
}