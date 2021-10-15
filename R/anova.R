#' anova_cca/rda_simple_effect
#'
#' @param data comm data
#' @param env env data
#' @param fun "RDA" or "CCA"
#' @param permutations permutation test form.
#'
#' @return The effect of each factor alone.
#' @examples
#' data(dune)
#' data(dune.env)
#' anova_ca_simple_effect(dune, dune.env, "RDA", 499)
#'
#' @export
anova_ca_simple_effect <-
    function(data,
             env,
             fun = c("RDA", "CCA"),
             permutations = permute::how(nperm = 999)
    ) {
        data <- as.data.frame(data)
        env <- as.data.frame(env)
        out_df <- data.frame()
        fun <- match.arg(fun)
        for (cn in colnames(env)) {
            fo <- stats::as.formula(paste0("data ~ ", cn))
            if (fun == "RDA") {
                rda_cca <- vegan::rda(fo, data = env)
            } else {
                rda_cca <- vegan::cca(fo, data = env)
            }
            tmp_df <-
                rda_cca %>%
                vegan::anova.cca(by = "terms", permutations = permutations) %>%
                as.data.frame()
            tmp_df <- tmp_df[1,] %>%
                tibble::rownames_to_column(paste0(fun, "~vars"))
            out_df <- rbind(out_df, tmp_df)
        }
        out_df <- out_df[order(out_df$`Pr(>F)`),]
        return(out_df)
    }
