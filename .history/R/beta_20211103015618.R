#' beta
#'
#' compute some dists.
#' Plot the PCA PCoa NMDS.
#' @rdname beta
#'
#' @param otu_t otu table after t(),
#' The sample should be in row,
#' the features(otu/asv) should be in column;
#' @param tree phylogene tree
#'
#' @return beta dist
#'
#' @export
beta_dist <- function(otu, tree) {
    if (missing(tree)) {
        tmp_phy <-
            phyloseq::phyloseq(phyloseq::otu_table(otu, taxa_are_rows = TRUE))
    } else {
        tmp_phy <-
            phyloseq::phyloseq(
                phyloseq::otu_table(otu, taxa_are_rows = TRUE),
                phyloseq::phy_tree(tree)
            )
    }
    tmp_dists1 <- list(
        Bray_Curtis        = distance(tmp_phy, "bray"),
        Jaccard            = distance(tmp_phy, "jaccard", binary = TRUE),
    )
    if (missing(tree)) {
        return(tmp_dists1)
    }
    tmp_dists2 <- list(
        Unweighted_UniFrac = distance(tmp_phy, "uunifrac"),
        Weighted_UniFrac = distance(tmp_phy, "wunifrac")
    )
    return(c(tmp_dists1, tmp_dists2))
}