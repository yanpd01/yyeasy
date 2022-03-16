
#' Max min standardization
#'
#' @rdname std
#' @param x Numbers.
#'
#' @return Standardized data
#' @export
#'
#' @examples
#' std_max(1:10)
#' std_min(1:10)
std_max <- function(x) {
    ((x - min(x)) / (max(x) - min(x)))
}

#' @rdname std
#' @export
std_min <- function(x) {
    ((max(x) - x) / (max(x) - min(x)))
}



#' coef standardization
#'
#' @rdname std
#' @param model the result of lm function.
#'
#' @return Standardized coefficients and p_value.
#' @export
#' @import stats
#'
#' @examples
#' set.seed(1)
#' a1 <- runif(100)
#' a2 <- a1*runif(100, .9,1.2)*100
#' a3 <- runif(100, .9,1.2)*10
#' md1 <- lm(a1 ~ a2 +a3)
#' coef(md1)
#' coef_std(md1)
coef_std <- function(model) {
    md <- model
    md_df <- md$model
    cf <- coef(md)
    if (names(cf)[1] == "(Intercept)") cf <- cf[-1]
    cf_std <- coef(md)
    cf_std[names(cf)] <- cf * sapply(md_df[, names(cf), drop = FALSE], sd) / sd(md_df[, 1])
    p_value <- round(summary.lm(md)$coefficients[, "Pr(>|t|)"], 5)
    return(
        data.frame(
            original = coef(md),
            standardized = cf_std,
            p_value = p_value ,
            sig = get_sig(p_value)
        )
    )
}
