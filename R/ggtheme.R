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
theme_bw2 <- function(
    font_size = 10,
    font = "sans",
    line_size = 0.5
) {
    ## 四周型主题
    theme_bw() %+replace%
        theme(
            text = element_text(size = font_size, family = font),
            # legend
            legend.title = element_text(size = font_size + 1),
            legend.key = element_blank(),
            legend.background = element_blank(),
            legend.text = element_text(size = font_size),
            ## axis
            axis.text = element_text(size = font_size, color = "black"),
            axis.title = element_text(size = font_size + 1, vjust = 1),
            axis.line = element_blank(),
            ## panel
            panel.border = element_rect(
                size = (line_size * 2),
                colour = "black",
                fill = NA
            ),
            panel.grid = element_blank()
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
theme_classic2 <- function(
    font_size = 10,
    font = "sans",
    line_size = 0.5
) {
    ## 常规xy轴主题
    theme_classic() %+replace%
        theme(
            text = element_text(size = font_size, family = font),
            ## legend
            legend.title = element_text(size = font_size + 1),
            legend.key = element_blank(),
            legend.background = element_blank(),
            legend.text = element_text(size = font_size),
            ## axis
            axis.text = element_text(size = font_size, colour = "black"),
            axis.title = element_text(size = font_size + 1, vjust = 1),
            axis.line = element_line(size = line_size)
        )
}
