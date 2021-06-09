#' Var tools
#'
#' Move the var to a new specified list.
#' @param ls ls(); ls(pattern = "aa")
#'
#' @return new list
#'
#' @examples
#' a01 <- 1
#' a02 <- 2
#' a_all <- yylst(ls(pattern = "a0"))
#'
#' @export
yylst <- function(ls) {
    out_list <- list()
    for ( i in ls) {
        i_wd <- get(i, envir = globalenv())
        out_list <- c(list(i_wd), out_list)
        names(out_list)[1] <- i
        o_wd <- out_list[[i]]
        if (identical(i_wd, o_wd))
            rm(list = i, envir = globalenv() )
    }
    return(out_list)
}

#' Rename var
#'
#' @param old_var_name The name of the old variable.
#' @param new_var_name The name of the new variable.
#'
#' @return no return
#'
#' @examples
#' a=1
#' rename_var("a", "c")
#' c
#'
#' @export
rename_var <- function(old_var_name, new_var_name) {
    env <- globalenv()
    wd <- get(old_var_name, envir = env)
    env[[new_var_name]] <- wd
    rm(list = old_var_name, envir = env)
    invisible()
}
