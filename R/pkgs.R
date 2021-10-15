# for attach  auto library ----------------------------------------------------
run_install <-  function (package, update =  FALSE, ...) {

    if (!requireNamespace(package, quietly = TRUE)) {
        BiocManager::install(package, update = update)
    }
    invisible()
}

run_library <- function(pkg,
                        character.only = TRUE,
                        quietly = TRUE,
                        warn.conflicts = FALSE
                        ) {
    if (!requireNamespace(pkg)) return(FALSE)
    loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
    do.call(
        "library",
        list(pkg,
             lib.loc = loc,
             character.only = character.only,
             quietly = quietly,
             warn.conflicts = warn.conflicts)
    )
    return(TRUE)
}

run_unload <- function(pkg) {
    try_res <- try(
        detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE),
        silent = TRUE
    )
    if ("try-error" %in% class(try_res)) return(FALSE)
    return(TRUE)
}

.onAttach <- function(...) {
    core <- c("vegan",
              "tibble", "dplyr", "ggplot2",
              "tidyr", "stringr",
              "phyloseq"
    )
    packageStartupMessage("The following packages will be attached.\n",
                          paste0(core, collapse = ", "),
                          ".\n"
    )
    tmp0 <- paste0("package:", core) %in% search()
    needed <- core[!tmp0]
    if (length(needed) == 0)
        return(invisible())
    suppressMessages(lapply(needed, run_library))
    invisible()
}

#' yyload
#'
#' library packages.
#'
#' @rdname load
#' @param ... names of pkgs, without the quotes.
#' @param names names of pkgs, with the quotes.
#' @param show.conflict Whether to display conflict information.
#'
#' @export
yyload <- function(..., names = NULL, show.conflict = TRUE) {
    dots <- match.call(expand.dots = FALSE)$...
    if (length(dots) &&
        !all(vapply(dots, function(x)
            is.symbol(x), NA, USE.NAMES = FALSE)))
        stop("... can`t be character strings")
    name <- vapply(dots, as.character, "")
    all <- c(name, names)
    # suppressWarnings(lapply(all, run_install))
    # suppressMessages(lapply(all, run_install))
    suppressMessages(suppressWarnings(lapply(all, run_install)))
    suppressMessages(pkg_lst <- sapply(all, run_library))
    names(pkg_lst) <- all
    if (show.conflict) print(conflicted::conflict_scout())
    packageStartupMessage("\nThe load status:")
    print(pkg_lst)
}




#' @param uninstall Logical value, whether to uninstall the installation package.
#'
#' @rdname load
#' @export
#' @examples
#' yyload(ggrepel)
#' yyunload(ggrepel)
yyunload <- function(..., names = NULL, uninstall = FALSE) {
    dots <- match.call(expand.dots = FALSE)$...
    if (length(dots) &&
        !all(vapply(dots, function(x)
            is.symbol(x), NA, USE.NAMES = FALSE)))
        stop("... can`t be character strings")
    name <- vapply(dots, as.character, "")
    all <- c(name, names)
    pkg_lst <- sapply(all, run_unload)
    packageStartupMessage("The unload status:")
    print(pkg_lst)
    if (uninstall) {
        packageStartupMessage("\nThe uninstall status:")
        invisible(lapply(all, utils::remove.packages))
    }
}
