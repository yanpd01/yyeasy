#' corr p
#' Simplified digital display.
#' @rdname corr
#' @param df your data
#' @param p_value p_value table
#' @param level significance level
#' @param p_adj_method ?stats::p.adjust.methods
#' @param cut Numbers less than this value are omitted.
#' @return short cor R.
#' @examples
#' get_corr(c(0.1, 0.5, 0.6), cut = 0.2)
#' get_corr_sig(c(0.1, 0.5, 0.6), c(0.1, 0.04, 0.001))
#' @export
#'
get_corr <- function(df, cut = 0.3) {
    ## trans
    df <- as.data.frame(df)
    dim_0 <- dim(df)
    df <- as.numeric(as.matrix(df))
    df[is.na(df)] <- 0
    ## get address
    add_0 <- abs(df) < cut
    ## show number
    df_new <- sprintf("%.2f", df)
    df_new[add_0] <- ""
    ## dim
    if (dim_0[2] != 1) dim(df_new) <- dim_0
    return(df_new)
}


#' @rdname corr
#' @return paste r and *.
#' @param sep Separator of r and *.
#' @param rm_no_sig Logical value, whether to remove non-significant values.
#' @param rm_1 Logical value, whether to remove self-correlation values.
#' @export
get_corr_sig <- function(
    df,
    p_value,
    p_adj_method = "none",
    sep = "\n",
    rm_no_sig = TRUE,
    rm_1 = TRUE,
    level = 3
    ) {
    df <- as.data.frame(df)
    p_value <- as.data.frame(p_value)
    # dim
    dim_0 <- dim(df)
    dim_1 <- dim(p_value)
    if (!identical(dim_0, dim_1)) {
          stop("The datas need to have the same dimensions")
      }
    # paste
    df_cor <- get_corr(df, cut = 0)
    df_p <- get_sig(p_value, level = level, p_adj_method = p_adj_method)
    sites_n1 <- df_p == ""
    sites_n2 <- df == 1
    df_out <- paste0(df_cor, sep, df_p)
    if (dim_0[2] != 1) dim(df_out) <- dim_0
    if (rm_no_sig) df_out[sites_n1] <- ""
    if (rm_1)      df_out[sites_n2] <- ""
    return(df_out)
}