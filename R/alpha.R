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
    # index <- tibble::rownames_to_column(index, "id")
    return(index)
}


#' rarefy
#'
#' Prepare data for rerafy curves.
#' @rdname rarefy
#' @param phy phyloseq
#' @param step steps
#' @param rep times of repetition
#' @inheritParams phyloseq::estimate_richness
#'
#' @return rarefy table
#' @examples
#' library(phyloseq)
#' data(its)
#' phy <- phyloseq(otu_table(its, taxa_are_rows = TRUE))
#' rare <- rarefy_otu(phy)
#' x_min <- group_by(rare, id) %>%
#'     summarise(across(Depth, max)) %>%
#'     .[, "Depth"] %>%
#'     min()
#' ggplot(rare, aes(Depth, Observed, group = id, color = id)) +
#'     geom_vline(xintercept = x_min, linetype = "dashed", color = "grey60") +
#'     stat_summary(fun = "mean", geom = "smooth", size = .7) +
#'     stat_summary(
#'         fun.data = "mean_cl_normal", geom = "errorbar",
#'         size = .5, aes(width = max(Depth) / 30)) +
#'     theme_bw2(15) +
#'     theme(legend.justification = c(1, 0), legend.position = c(1, 0))
#' @export
rarefy_otu <- function(phy, step = 10, rep = 3, measures = c("Observed", "Shannon")) {
    otu <- phyloseq::otu_table(phy)
    sums <- phyloseq::sample_sums(otu)
    step_all <- floor(seq(1, max(sums), length.out = step))[-step]
    df_out3 <-
        foreach::foreach(step_now = step_all, .combine = rbind) %do% {
                message("The Depth is ", step_now)
            foreach::foreach(i = seq_len(rep), .combine = rbind) %do% {
                df_out <- phyloseq::prune_samples(sums >= step_now, otu) %>%
                    phyloseq::rarefy_even_depth(sample.size = step_now, replace = FALSE, verbose = FALSE, trimOTUs = FALSE) %>%
                    phyloseq::estimate_richness(measures = measures)
                df_out$Depth <- step_now
                df_out <- tibble::rownames_to_column(df_out, "id")
                df_out
            }
        }
    df_out4 <- phyloseq::estimate_richness(otu, measures = measures)
    df_out4$Depth <- phyloseq::sample_sums(otu)
    df_out4 <- tibble::rownames_to_column(df_out4, "id")
    return(rbind(df_out3, df_out4))
}

#' repeat run
#'
#' @rdname repeat
#' @param exp A task enclosed in quotes ready to run.
#' @param times times of repetition.
#' @param ... Other arguments from foreach::foreach
#' @inheritParams foreach::foreach
#'
#' @return repeat results
#' @examples
#' rep_run('sum(1:10)')
#' 'runif(10)' %>% rep_run(5, .combine = rbind)
#' @export
rep_run <- function(exp, times = 3, .combine = list, ...) {
foreach::foreach(i = seq_len(times), .combine = .combine, ...) %do%
    eval(parse(text = exp))
}