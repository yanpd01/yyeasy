#' PCA PCoa NMDS
#'
#' Plot the PCA PCoa NMDS.
#' @rdname pca
#'
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
#' @param returns return plot or axis
#'
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
                     font_size = 10,
                     returns = c("plot", "data")) {
    ## stop warning
    if (!font %in% c("sans", "serif")) {
        warning(" 'sans'=Arial, 'serif'=Times New Roman ")
    }
    if (ncol(df) < nrow(df)) {
        warning("
          The sample should be in row,
          and the Species should be in column")
    }
    ## missing
    if (missing(group)) {
        group <- data.frame(id = rownames(df), group = rownames(df))
    }
    if (length(unique(group[[1]])) < length(unique(group[[2]]))) {
        group <- group[, c(2, 1)]
    }
    colnames(group) <- c("id", "group")
    ## pca
    pca <- vegan::rda(df, scale = scale)
    pca_sum <- summary(pca, scaling = 1)
    ## return first
    returns <- match.arg(returns)
    if (returns == "data") {
        pca_axis <- as.data.frame(pca_sum$sites) %>%
            tibble::rownames_to_column(var = "id")
        pca_importance <-  pca_sum$cont$importance[2,]
        return(tibble::lst(pca_axis, pca_importance))
    }
    ## pca plot
    pca_im <- pca_sum$cont$importance[2, axis] * 100
    pca_xy <- pca_sum$sites[, axis] %>% as.data.frame()
    colnames(pca_xy) <- c("pcx", "pcy")
    pca_out <- pca_xy %>%
        tibble::rownames_to_column(var = "id") %>%
        dplyr::right_join(group, "id")
    ## ggplot
    p <- ggplot(
        pca_out,
        aes_string(x = "pcx", y = "pcy", color = "group")) +
        geom_point(size = point_size)
    if (label == TRUE) {
        p <- p + ggrepel::geom_text_repel(
            aes_string(label = "id"),
            show.legend = FALSE,
            size = font_size / .pt,
            family = font
        )
    }
    p <- p +
        xlab(paste0(names(pca_im[1]), " ", sprintf("%0.2f", pca_im[1]), "%")) +
        ylab(paste0(names(pca_im[2]), " ", sprintf("%0.2f", pca_im[2]), "%")) +
        geom_vline(
            xintercept = 0,
            linetype = 2,
            color = "grey0",
            alpha = 0.5
        ) +
        geom_hline(
            yintercept = 0,
            linetype = 2,
            color = "grey0",
            alpha = 0.5
        ) +
        # theme
        theme_bw() +
        theme(
            panel.border = element_rect(
                size = 1,
                colour = "black",
                fill = NA
            ),
            axis.line = element_blank()
        ) +
        theme(text = element_text(size = font_size, family = font)) +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = font_size)) +
        theme(
            axis.text = element_text(size = font_size, color = "black"),
            axis.title = element_text(size = font_size + .5, vjust = 1),
            panel.grid = element_blank()
        )

    if (legend == FALSE) {
        p <- p + theme(legend.position = "none")
    }
    p
}


#' @rdname pca
#'
#' @param dist dist
#'
#' @export
pcoa_plot <- function(dist,
                      group,
                      axis = c(1, 2),
                      label = TRUE,
                      legend = FALSE,
                      point_size = 2,
                      font = "sans",
                      font_size = 10,
                      returns = c("plot", "data")) {
    ## prepare
    id <- attributes(dist)$Labels
    ## warning stop
    if (!font %in% c("sans", "serif")) {
        warning(" 'sans'=Arial, 'serif'=Times New Roman ")
    }
    ## missing
    if (missing(group)) {
        group <- data.frame(id = id, group = id)
    }
    if (length(unique(group[[1]])) < length(unique(group[[2]]))) {
        group <- group[, c(2, 1)]
    }
    colnames(group) <- c("id", "group")
    ## pcoa
    pcoa <- ape::pcoa(dist)
    pcoa_xy <- pcoa$vectors[, axis] %>% as.data.frame()
    pcoa_im <-
        pcoa$values$Eigenvalues / sum(pcoa$values$Eigenvalues) * 100
    colnames(pcoa_xy) <- c("pcoax", "pcoay")
    pcoa_out <- pcoa_xy %>%
        tibble::rownames_to_column(var = "id") %>%
        dplyr::right_join(group, "id")
    ##return first
    returns <- match.arg(returns)
    if (returns == "data") {
        pcoa_axis <- as.data.frame(pcoa$vectors) %>%
            tibble::rownames_to_column(var = "id")
        pcoa_importance <- pcoa_im/100
        return(tibble::lst(pcoa_axis, pcoa_importance))
    }

    ## ggplot
    p <- ggplot(data = pcoa_out,
                aes_string(x = "pcoax", y = "pcoay", color = "group")) +
        geom_point(size = point_size)
    if (label == TRUE) {
        p <- p + ggrepel::geom_text_repel(
            aes(label = id),
            show.legend = FALSE,
            size = font_size / .pt,
            family = font
        )
    }
    p <- p +
        xlab(paste0("PCoA", axis[1], " ", sprintf("%0.2f", pcoa_im[axis[1]]), "%")) +
        ylab(paste0("PCoA", axis[2], " ", sprintf("%0.2f", pcoa_im[axis[2]]), "%")) +
        geom_vline(
            xintercept = 0,
            linetype = 2,
            color = "grey0",
            alpha = 0.5
        ) +
        geom_hline(
            yintercept = 0,
            linetype = 2,
            color = "grey0",
            alpha = 0.5
        ) +
        # theme
        theme_bw() +
        theme(
            panel.border = element_rect(
                size = 1,
                colour = "black",
                fill = NA
            ),
            axis.line = element_blank()
        ) +
        theme(text = element_text(size = font_size, family = font)) +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = font_size)) +
        theme(
            axis.text = element_text(size = font_size, color = "black"),
            axis.title = element_text(size = font_size + .5, vjust = 1),
            panel.grid = element_blank()
        )

    if (legend == FALSE) {
        p <- p + theme(legend.position = "none")
    }
    p
}


#' @rdname pca
#'
#' @param dist_or_df Community, otu , species data, Alternatively,
#' dissimilarities either as a dist structure .
#' @param veddist_method Dissimilarity index, partial match to
#' "manhattan", "euclidean", "canberra", "clark", "bray", "kulczynski",
#' "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup",
#' "binomial", "chao", "cao", "mahalanobis", "chisq" or "chord".
#' @param autotransform Use simple heuristics for possible data transformation
#' of typical community data (see below). If you do not have community data,
#' you should probably set autotransform = FALSE.
#' @param trace FALSE or unmber,
#' 	Trace the function; trace = 2 or higher will be more voluminous.
#'
#' @examples
#' data(its)
#' otu_t <- t(its)
#' pca_plot(otu_t)
#' tree <- ape::rtree(nrow(its), rooted = TRUE, tip.label = rownames(its))
#' bray_dist <- vegan::vegdist(otu_t)
#' pcoa_plot(bray_dist)
#' nmds_plot(bray_dist)
#' nmds_plot(otu_t)
#'
#' @export
nmds_plot <- function(dist_or_df,
                      group,
                      veddist_method = "bray",
                      autotransform = FALSE,
                      trace = FALSE,
                      label = TRUE,
                      legend = FALSE,
                      point_size = 2,
                      font = "sans",
                      font_size = 10,
                      returns = c("plot", "data")) {
    ## prepare
    if ("dist" %in% class(dist_or_df)) {
        id <- attr(dist_or_df, "Labels")
    } else {
        id <- rownames(dist_or_df)
    }
    ## warning stop
    if (!font %in% c("sans", "serif")) {
        warning(" 'sans'=Arial, 'serif'=Times New Roman ")
    }
    ## missing
    if (missing(group)) {
        group <- data.frame(id = id, group = id)
    }
    if (length(unique(group[[1]])) < length(unique(group[[2]]))) {
        group <- group[, c(2, 1)]
    }
    colnames(group) <- c("id", "group")
    ## NMDS
    nmds <- vegan::metaMDS(
        dist_or_df,
        trace = trace,
        distance = veddist_method,
        autotransform = autotransform
    )
    nmds_xy <- nmds$points %>%
        as.data.frame() %>%
        tibble::rownames_to_column("id")
    nmds_plot <- dplyr::right_join(nmds_xy, group, by = "id")
    nmds_stress <- (nmds$stress %>% round(6))
    ##return first
    returns <- match.arg(returns)
    if (returns == "data") {
        nmds_axis <- nmds_xy
        nmds_stress <- nmds$stress
        return(tibble::lst(nmds_axis, nmds_stress))
    }

    ## ggplot
    p <- ggplot(data = nmds_plot,
                aes_string(x = "MDS1", y = "MDS2", color = "group")) +
        geom_point(size = point_size)
    if (label == TRUE) {
        p <- p + ggrepel::geom_text_repel(
            aes(label = id),
            show.legend = FALSE,
            size = font_size / .pt,
            family = font
        )
    }
    p <- p +
        labs(
            x = "NMDS1",
            y = "NMDS2",
            subtitle = paste0("Stress: ", nmds_stress)
        ) +
        geom_vline(
            xintercept = 0,
            linetype = 2,
            color = "grey0",
            alpha = 0.5
        ) +
        geom_hline(
            yintercept = 0,
            linetype = 2,
            color = "grey0",
            alpha = 0.5
        ) +

        # theme
        theme_bw() +
        theme(
            panel.border = element_rect(
                size = 1,
                colour = "black",
                fill = NA
            ),
            axis.line = element_blank()
        ) +
        theme(text = element_text(size = font_size, family = font)) +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = font_size)) +
        theme(
            axis.text = element_text(size = font_size, color = "black"),
            axis.title = element_text(size = font_size + .5, vjust = 1),
            panel.grid = element_blank(),
            plot.subtitle = element_text(size = font_size + .5)
        )
    if (legend == FALSE) {
        p <- p + theme(legend.position = "none")
    }
    p
}
