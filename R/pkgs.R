## install  ----------------------------------------------------

#' @rdname load
#' @export
#' @inheritParams BiocManager::install
#' @inheritParams utils::install.packages
yyinstall <- function(...,
                      names = NULL,
                      dependencies = NA,
                      update = FALSE,
                      version = BiocManager::version()) {
    dots <- match.call(expand.dots = FALSE)$...
    name <- vapply(dots, as.character, "")
    all <- c(name, names)
    tmp_id <- sapply(all, requireNamespace, quietly = TRUE)
    tmp_pkgs <- all[!tmp_id]
    if (length(tmp_pkgs)) {
        BiocManager::install(
            tmp_pkgs,
            dependencies = dependencies,
            update = update,
            version = version
        )
    }
    pkg_lst <- sapply(all, requireNamespace, quietly = TRUE)
    packageStartupMessage("\nThe install status:")
    return(pkg_lst)
}

#' @export
#' @rdname load
yyuninstall <- function(..., names = NULL) {
    dots <- match.call(expand.dots = FALSE)$...
    name <- vapply(dots, as.character, "")
    all <- c(name, names)
    utils::remove.packages(all)
}

## auto library ------------------------------------------------
run_library <- function(pkg,
                        character.only = TRUE,
                        quietly = TRUE,
                        warn.conflicts = FALSE) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        return(FALSE)
    }
    loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
    do.call(
        "library",
        list(pkg,
            lib.loc = loc,
            character.only = character.only,
            quietly = quietly,
            warn.conflicts = warn.conflicts
        )
    )
    return(TRUE)
}

run_unload <- function(pkg) {
    try_res <- try(
        detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE),
        silent = TRUE
    )
    if ("try-error" %in% class(try_res)) {
        return(FALSE)
    }
    return(TRUE)
}

.onAttach <- function(...) {
    core <- c(
        # "vegan",
        "tibble",
        "dplyr",
        "ggplot2",
        "tidyr",
        "stringr"
        # "phyloseq"
    )
    packageStartupMessage(
        "The following packages will be attached.\n",
        paste0(core, collapse = ", "),
        ".\n"
    )
    tmp0 <- paste0("package:", core) %in% search()
    needed <- core[!tmp0]
    if (length(needed) == 0) {
          return(invisible())
      }
    suppressMessages(lapply(needed, run_library))
    invisible()
}

## yyload ----------------------------------------------------
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
    name <- vapply(dots, as.character, "")
    all <- c(name, names)
    tmp_id <- sapply(all, requireNamespace, quietly = TRUE)
    tmp_pkgs <- all[!tmp_id]
    if (length((tmp_pkgs))) {
        if (utils::askYesNo(paste(
            "Whether to install:",
            paste(tmp_pkgs, collapse = ", ")
        ))
        ) {
            BiocManager::install(tmp_pkgs, update = FALSE)
        }
    }
    suppressMessages(pkg_lst <- sapply(all, run_library))
    if (show.conflict) print(conflicted::conflict_scout())
    packageStartupMessage("\nThe load status:")
    print(pkg_lst)
}


#' @rdname load
#' @export
#' @examples
#' \dontrun{
#' yyinstall(ggtext, "ggsci")
#' yyuninstall(ggtext, "ggsci")
#'
#' ## restart R
#' yyload(ggtext, "ggsci")
#' yyunload(ggtext, "ggsci")
#' }
yyunload <- function(..., names = NULL) {
    dots <- match.call(expand.dots = FALSE)$...
    name <- vapply(dots, as.character, "")
    all <- c(name, names)
    pkg_lst <- sapply(all, run_unload)
    packageStartupMessage("The unload status:")
    print(pkg_lst)
}