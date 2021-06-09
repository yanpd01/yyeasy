#' Rarefy
#'
#' Rarefy the otu/asv table, with "Observed features" and "Shannon" indexs.
#' Plot the rearrarefaction curve.
#' @rdname rarefy
#'
#' @param x the features(otu/asv) table,
#' The sample should be in column,
#' the features(otu/asv) should be in row.
#' @param step steps
#' @param rep replace
#' @param depth max sequencing depth
#'
#' @return rarefy lists with "Observed features" and "Shannon" indexs
#'
#' @export
rarefy_table <- function(x, step = 10, rep = 5, depth) {
    ## warning stop
    if (nrow(x) > ncol(x)) {
        warning(
            "
        The sample should be in row,
        the features(otu/asv) should be in columu;
        The sample needs to be less than the features(otu/asv) "
        )
    }
    ## missing
    if (missing(depth)) {
        depth <- min(rowSums(x)) * .95
    }
    ## start
    nx <- nrow(x)
    step_num <- ceiling(seq(0, depth, depth / step))
    ## rare_raw
    rare_all_sobs <- data.frame()
    rare_all_shannon <- data.frame()
    ## for loop
    for (m in 1:rep) {
        rare_sobs <- as.data.frame(matrix(ncol = nx))
        rare_shannon <- as.data.frame(matrix(ncol = nx))
        for (k in 1:(step + 1)) {
            r_x <- vegan::rrarefy(x, step_num[k])
            ## different methods
            rare_sobs[k, ] <- rowSums(r_x > 0)
            rare_shannon[k, ] <- vegan::diversity(r_x,
                                                  index = "shannon",
                                                  MARGIN = 1,
                                                  base = 2
            )
        }
        ## colnames
        colnames(rare_sobs) <- rownames(x)
        colnames(rare_shannon) <- rownames(x)
        ## depth
        rare_sobs$depth <- step_num
        rare_shannon$depth <- step_num
        ## rbind
        rare_all_sobs <- rbind(rare_all_sobs, rare_sobs)
        rare_all_shannon <- rbind(rare_all_shannon, rare_shannon)
    }
    ## gather
    sobs <- tidyr::gather(rare_all_sobs, key = "id", value = "value", -depth)
    shannon <- tidyr::gather(rare_all_shannon,
                             key = "id",
                             value = "value",
                             -depth
    )
    ## return
    return(list(`Observed features` = sobs, Shannon = shannon))
}


#' @rdname rarefy
#'
#' @param df  one of result in rarefy_table,or other table
#' @param group group
#' @param ylab "Observed features", "Shannon"
#' @param errorbar TRUE, FALSE
#' @param legend TRUE, FALSE
#' @param font "sans" = Arial, "serif" = Times New Roman ,any fonts
#' @param font_size size_name of  the df
#' @param depth_name depth_name of  the df
#' @param value_name value_name of  the df
#' @param id_name id_name of the df
#'
#' @examples
#' data(its)
#' its_t <- t(its)
#' rt <- rarefy_table(its_t, step = 10, rep = 6)
#' rarefy_plot(rt[[1]])
#' rarefy_plot(rt[[2]], ylab = "Shannon")
#' @export
rarefy_plot <- function(
    df,
    group,
    ylab = "Observed features",
    errorbar = FALSE,
    legend = TRUE,
    font = "sans",
    font_size = 10,
    id_name = "id",
    depth_name = "depth",
    value_name = "value"
) {
    ## warning stop
    if (!font %in% c("sans", "serif")) {
        warning(" 'sans'=Arial, 'serif'=Times New Roman ")
    }
    x <- as.data.frame(df)
    ## missing
    if (missing(group)) {
        group <- data.frame(x[id_name], x[id_name])
    }
    if (length(unique(group[[1]])) < length(unique(group[[2]]))) {
        group <- group[, c(2, 1)]
    }
    colnames(group) <- c(id_name, "group")
    ## data
    rare_plot <- dplyr::right_join(x, group, id_name)
    ## ggplot
    p <- ggplot(
        rare_plot,
        aes_string(x = depth_name, y = value_name, color = "group")
    ) +
        stat_summary(
            fun = "mean",
            geom = "line",
            size = 0.35
        ) +
        labs(x = "Sequencing Depth", y = ylab) +
        # theme
        theme_classic() +
        theme(axis.line = element_line(size = 0.5)) +
        theme(text = element_text(size = font_size, family = font)) +
        theme(
            legend.position = c(.9, .5),
            ## legend 系列
            legend.key = element_blank(),
            ## 去除图列图例数值部分的背景
            legend.background = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = font_size)
        ) +
        theme(
            axis.text = element_text(size = font_size, colour = "black"),
            axis.title = element_text(size = font_size + .5, vjust = 1)
        ) +
        scale_x_continuous(expand = c(0.01, 0)) +
        scale_y_continuous(expand = c(0.01, 0))

    if (errorbar == TRUE) {
        p <- p + stat_summary(
            aes_string(width = (max(depth_name) - min(depth_name)) / 50),
            geom = "errorbar",
            fun.data = "mean_cl_normal",
            size = 0.3
        )
    }
    if (legend == FALSE) {
        p <- p + theme(legend.position = "none")
    }
    p
}
