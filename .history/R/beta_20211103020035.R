#' beta
#'
#' compute some dists.
#' Plot the PCA PCoa NMDS.
#' @rdname beta
#'
#' @param otu otu table.
#' @param tree phylogene tree
#'
#' @return beta dist
#' @examples
#' data(its)
#' tree <- ape::rtree(nrow(its), rooted = TRUE, tip.label = rownames(its))
#' dists <- beta_dist(its_t, tree)
#' pcoa_plot(dists[[1]])
#' nmds_plot(dists[[3]])
#' nmds_plot(its_t)
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
        Bray_Curtis = distance(tmp_phy, "bray"),
        Jaccard     = distance(tmp_phy, "jaccard", binary = TRUE),
    )
    if (missing(tree)) {
        return(tmp_dists1)
    }
    tmp_dists2 <- list(
        Unweighted_UniFrac = distance(tmp_phy, "uunifrac"),
        Weighted_UniFrac   = distance(tmp_phy, "wunifrac")
    )
    return(c(tmp_dists1, tmp_dists2))
}