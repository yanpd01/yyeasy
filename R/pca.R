#' PCA PCoa NMDS
#'
#' Plot the PCA PCoa NMDS.
#' @rdname pca
#' @param df df
#' @param group group
#' @param scale TRUE of FALSE. Scale species to unit variance
#' (like correlations).
#' @param axis select the axis to plot
#' @param label TRUE of FALSE. Whether to draw a label
#' @param legend TRUE of FALSE. Whether to draw a legend
#' @param point_size point size
#' @param font sans = Arial, serif = Times New Roman
#' @param font_size font size
#' @importFrom ggplot2 ggplot aes aes_string  stat_summary
#' element_blank element_line element_rect element_text
#' geom_hline geom_point geom_vline scale_x_continuous scale_y_continuous
#' theme theme_bw theme_classic %+% %+replace% .pt
#' labs xlab ylab
#'
#' @export
pca_plot <- function(df,
                     group,
                     scale = FALSE,
                     axis = c(1, 2),
                     label = TRUE,
                     legend = FALSE,
                     point_size = 2,
                     font = "sans",
                     font_size = 10) {
    ## df
    df <- as.data.frame(df)
    id <- rownames(df)
    ## group
    if (missing(group)) {
        group <- data.frame(id = id, group = id)
        warning("Missing group, use id as group.")
    }
    colnames(group) <- c("id", "group")
    rownames(group) <- group$id
    group <- group[id, ]
    ## axis
    axis_x <- axis[1]
    axis_y <- axis[2]
    ## pca
    pca <- vegan::rda(df, scale = scale)
    pca_sum <- summary(pca, scaling = 1)
    pca_im <- pca_sum$cont$importance[2, ] * 100
    pca_data <- pca_sum$sites %>%
        as.data.frame() %>%
        cbind(group)
    ## ggplot
    p <- ggplot(pca_data) +
        aes_string(x = paste0("PC", axis_x), y = paste0("PC", axis_y)) +
        geom_point(size = point_size)
    if (label) {
        p <- p +
            ggrepel::geom_text_repel(
                aes(label = id),
                size = font_size / .pt, family = font
            )
    }
    p <- p +
        labs(
            x = paste0("PC", axis_x, " ", sprintf("%0.2f", pca_im[axis_x]), "%"),
            y = paste0("PC", axis_y, " ", sprintf("%0.2f", pca_im[axis_y]), "%")
        ) +
        theme_bw2(font_size = font_size, font = font)
    p <- inner_hvline(p)
    if (!legend) {
        p <- p + theme(legend.position = "none")
    }
    return(tibble::lst(pca_data, pca_im, p))
}


#' @rdname pca
#' @param dist dist
#' @param ... Other params from ?ape::pcoa and ?vegan::metaMDS
#' @export
pcoa_plot <- function(dist,
                      group,
                      axis = c(1, 2),
                      label = TRUE,
                      legend = FALSE,
                      point_size = 2,
                      font = "sans",
                      font_size = 10,
                      ...) {
    ## id
    id <- attributes(dist)$Labels
    ## group
    if (missing(group)) {
        group <- data.frame(id = id, group = id)
        warning("Missing group, use id as group.")
    }
    colnames(group) <- c("id", "group")
    rownames(group) <- group$id
    group <- group[id, ]
    ## axis
    axis_x <- axis[1]
    axis_y <- axis[2]
    ## pcoa
    pcoa <- ape::pcoa(dist, ...)
    pcoa_data <- pcoa$vectors %>%
        as.data.frame() %>%
        cbind(group)
    pcoa_im <-
        pcoa$values$Eigenvalues / sum(pcoa$values$Eigenvalues) * 100
    names(pcoa_im) <- colnames(pcoa$vectors)
    ## ggplot
    p <- ggplot(data = pcoa_data) +
        aes_string(x = paste0("Axis.", axis_x), y = paste0("Axis.", axis_y)) +
        geom_point(size = point_size)
    if (label) {
        p <- p +
            ggrepel::geom_text_repel(
                aes(label = id),
                size = font_size / .pt, family = font
            )
    }
    p <- p +
        labs(
            x = paste0("PCoA", axis_x, " ", sprintf("%0.2f", pcoa_im[axis_x]), "%"),
            y = paste0("PCoA", axis_y, " ", sprintf("%0.2f", pcoa_im[axis_y]), "%")
        ) +
        theme_bw2(font_size = font_size, font = font)
    p <- inner_hvline(p)
    if (!legend) {
        p <- p + theme(legend.position = "none")
    }
    return(tibble::lst(pcoa_data, pcoa_im, p))
}


#' @rdname pca
#' @param dist dist.
#' @examples
#' data(its)
#' otu_t <- t(its)
#' pca_plot(otu_t)
#' bray_dist <- vegan::vegdist(otu_t)
#' pcoa_plot(bray_dist)
#' nmds_plot(bray_dist)
#' @export
nmds_plot <- function(dist,
                      group,
                      label = TRUE,
                      legend = FALSE,
                      point_size = 2,
                      font = "sans",
                      font_size = 10,
                      ...) {
    ## prepare
    id <- attr(dist, "Labels")
    ## group
    if (missing(group)) {
        group <- data.frame(id = id, group = id)
        warning("Missing group, use id as group.")
    }
    colnames(group) <- c("id", "group")
    rownames(group) <- group$id
    group <- group[id, ]
    ## NMDS
    nmds <- vegan::metaMDS(dist, ...)
    nmds_data <- nmds$points %>%
        as.data.frame() %>%
        cbind(group)
    nmds_stress <- nmds$stress
    ## ggplot
    p <- ggplot(data = nmds_data) +
        aes_string(x = "MDS1", y = "MDS2") +
        geom_point(size = point_size) +
        labs(
            x = "NMDS1", y = "NMDS2",
            subtitle = paste0("Stress: ", round(nmds_stress, 6))
        ) +
        theme_bw2(font_size = font_size, font = font)
    p <- inner_hvline(p)
    if (label) {
        p <- p + ggrepel::geom_text_repel(
            aes(label = id),
            size = font_size / .pt, family = font
        )
    }
    if (!legend) p <- p + theme(legend.position = "none")

    return(tibble::lst(nmds_data, nmds_stress, p))
}

inner_hvline <- function(p) {
    p + geom_vline(xintercept = 0, linetype = 2, color = "grey0", alpha = 0.5) +
        geom_hline(yintercept = 0, linetype = 2, color = "grey0", alpha = 0.5)
}