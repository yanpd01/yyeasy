#' Var tools
#'
#' Move the var to a new specified list.
#'
#' @param ... vars.
#' @param names character, the names of vars. eg: ls(pattern = "aa").
#' @param rm logical, whether to delete the variable.
#'
#' @return new list
#'
#' @examples
#' a01 <- 1
#' a02 <- 2
#' b03 <- 1
#' all <- lst_sum(b03, names=ls(pattern = "a0"))
#'
#' @export
lst_sum <- function(..., names = NULL, rm = TRUE) {
    dots <- match.call(expand.dots = FALSE)$...
    if (length(dots) &&
        !all(vapply(dots, function(x) is.symbol(x), NA, USE.NAMES = FALSE)))
        stop("... can`t be character strings")
    name <- vapply(dots, as.character, "")
    all <- c(name, names)
    out_list <- list()
    all <- unique(all)
    for (i in rev(all)) {
        i_wd <- get(i, envir = globalenv())
        out_list <- c(list(i_wd), out_list)
        names(out_list)[1] <- i
        o_wd <- out_list[[i]]
        if (rm && identical(i_wd, o_wd))
            rm(list = i, envir = globalenv())
    }
    return(out_list)
}

#' Rename var
#'
#' @param ... The old variable and new variable. Only two items are allowed.
#'
#' @return no return
#'
#' @examples
#' a=1
#' rename_var(a, c)
#' c
#'
#' @export
rename_var <- function(...) {
    dots <- match.call(expand.dots = FALSE)$...
    if (length(dots) != 2 ||
        !all(vapply(dots, function(x) is.symbol(x), NA, USE.NAMES = FALSE)))
        stop("... can`t be character string,
             and only two items are allowed.")
    names <- vapply(dots, as.character, "")
    old_name <- names[1]
    new_name <- names[2]
    env <- globalenv()
    wd <- get(old_name, envir = env)
    rm(list = old_name, envir = env)
    env[[new_name]] <- wd
    invisible()
}
