#' alpha
#'
#' compute some indexs.
#' @rdname alpha
#' @param otu otu table
#' @param tree tree
#' @return alpha dist
#' @examples
#' data(its)
#' indexs <- alpha_index(its)
#' indexs
#' @export
alpha_index <- function(otu, tree) {
    df <- otu
    tdf <- t(df)
    Shannon <- vegan::diversity(df, index = "shannon", MARGIN = 2, base = 2)
    Simpson <- vegan::diversity(df, index = "simpson", MARGIN = 2, base = exp(1))
    Richness <- vegan::specnumber(df, MARGIN = 2)
    index <- as.data.frame(cbind(Shannon, Simpson, Richness))
    obs_chao_ace <- t(vegan::estimateR(tdf))
    obs_chao_ace <- obs_chao_ace[rownames(index), ]
    index$Chao <- obs_chao_ace[, 2]
    index$Ace <- obs_chao_ace[, 4]
    index$Pielou_e <- Shannon / log(Richness, 2)
    index$Goods_coverage <- 1 - colSums(df == 1) / colSums(df)
    ## missing
    if (!missing(tree)) {
        index$PD_whole_tree <- picante::pd(tdf, tree, include.root = T)[, 1]
    }
    index <- tibble::rownames_to_column(index, "id")
    return(index)
}