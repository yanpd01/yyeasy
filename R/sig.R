#' significance
#' Get simplified correlation coefficients and significance stars
#' @rdname sig
#' @param p_value p_value table
#' @param level significance level
#' @param p_adj_method ?stats::p.adjust.methods
#'
#' @return Convert significance numbers to ***.
#'
#' @examples
#' get_sig(c(0.6, 0.00001, 0.0001, 0.001, 0.01, 0.04), level = 5)
#' @export
get_sig <- function(p_value, level = 3, p_adj_method = "none") {
    ## trans
    df <- as.data.frame(p_value)
    dim_0 <- dim(df)
    df <- as.numeric(as.matrix(df))
    df <- stats::p.adjust(df, method = p_adj_method)
    if (dim_0[2] != 1) dim(df) <- dim_0
    df[is.na(df)] <- 1
    df[df < 0] <- 1
    ## get address
    add_0 <- df > 0.05
    add_1 <- df <= 0.05 & df > 0
    add_2 <- df <= 0.01 & df > 0
    add_3 <- df <= 0.001 & df > 0
    add_4 <- df <= 0.0001 & df > 0
    ## *****
    df[add_0] <- ""
    df[add_1] <- "*"
    if (level == 1) {
        return(df)
    }
    df[add_2] <- "**"
    if (level == 2) {
        return(df)
    }
    df[add_3] <- "***"
    if (level == 3) {
        return(df)
    }
    df[add_4] <- "****"
    if (level == 4) {
        return(df)
    }
    df
}



#' significance
#' Get difference analysis results and significance labeling.
#' The core source of this function comes from \code{rstatix} and \code{agricolae}.
#' @rdname sig
#' @param formula a formula of the form x ~ group where x is a numeric variable
#' giving the data values and group is a factor with one or multiple levels
#' giving the corresponding groups. For example, formula = TP53 ~ cancer_group.
#' @param data a data.frame containing the variables in the formula.
#' @param test the name of the statistical test that is applied to the values.
#' "wilcox_test", "t_test", "dunn_test", "tukey_hsd" comes from rstatix,
#' "HSD.test", "duncan.test", "LSD.test", "SNK.test" comes from agricolae.
#' @param p.adjust.method method to adjust p values for multiple comparisons.
#' This parameter only works with the test method from rstatix.
#' @param alpha Significant level
#' @param quantile_function Quantile function to compute the confidence interval. Default is \code{qt} which means the \eqn{t} distribution and \code{qnorm} means \eqn{norm} distribution.
#' @return Difference analysis results and significance labeling.
#'
#' @examples
#' sig_label(disp ~ gear, mtcars)
#' @export
sig_label <- function(formula, data,
                      test = c(
                          "wilcox_test", "t_test", "dunn_test", "tukey_hsd",
                          "HSD.test", "duncan.test", "LSD.test", "SNK.test"
                      ),
                      p.adjust.method = "none",
                      alpha = 0.05,
                      quantile_function = "qt") {
    fm <- formula(formula)
    test <- match.arg(test)
    qt_fun <- eval(parse(text = paste0("stats::", quantile_function)))
    t0 <- stats::aggregate(fm, data, mean) %>% tibble::column_to_rownames(colnames(.)[1])
    t0_sd <- stats::aggregate(fm, data, sd) %>% tibble::column_to_rownames(colnames(.)[1])
    id <- rownames(t0)
    id_y <- deparse(fm[[2]])
    id_x <- deparse(fm[[3]])
    id_n <- as.numeric(table(data[, id_x])[id])
    if (test %in% c("wilcox_test", "t_test", "dunn_test", "tukey_hsd")) {
        test_fun <- eval(parse(text = paste0("rstatix::", test)))
        t1 <-
            test_fun(data, fm, p.adjust.method = p.adjust.method) %>%
            dplyr::select(group1, group2, p.adj)
        t1_mirror <- dplyr::select(t1, group2, group1, p.adj)
        colnames(t1_mirror) <- colnames(t1)
        t3 <- rbind(t1, t1_mirror)
        t4 <- tidyr::pivot_wider(
            t3,
            id_cols = "group1",
            names_from = "group2",
            values_from = "p.adj"
        ) %>%
            tibble::column_to_rownames("group1") %>%
            .[id, id] %>%
            as.matrix()
        diag(t4) <- 1
        df_out <- agricolae::orderPvalue(id, t0[id, ], alpha, t4) %>% .[id, ]
        # colnames(df_out)[1] <- id_y
    } else {
        test_fun <- eval(parse(text = paste0("agricolae::", test)))
        df_out <- test_fun(stats::aov(fm, data), id_x, alpha = alpha)$groups[id, ]
    }

    t7 <- data.frame(
        y_term = id_y,
        x_term = id_x,
        x_level = id,
        label = df_out[, 2],
        mean = df_out[, 1],
        n = id_n,
        sd = t0_sd[id, ],
        row.names = id
    ) %>% mutate(
        se = sd / sqrt(n),
        ci = sd / sqrt(n) * qt_fun(1 - alpha / 2, n - 1),
    )
    message(
        "Difference test method: ", test,
        "\nAlpha: ", alpha,
        "\nP.adj: ", p.adjust.method
    )
    return(t7)
}
