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
    tmp_phy <- 
    phyloseq::phyloseq(
            phyloseq::otu_table(otu_t, taxa_are_rows = FALSE),
            phyloseq::phy_tree(tree)
    tmp_dists <- list(
        Unweighted_UniFrac = distance(tmp_phy, "uunifrac"),
        Weighted_UniFrac = distance(tmp_phy, "wunifrac"),
        Bray_Curtis = distance(tmp_phy, "bray"),
        Jaccard = distance(tmp_phy, "jaccard", binary = TRUE),
        JSD = distance(tmp_phy, "jsd"),
        Euclidean = distance(tmp_phy, "euclidean")
    )
}