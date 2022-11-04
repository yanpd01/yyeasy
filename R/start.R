.onAttach <- function(...) {
    core <- c(
        # "vegan",
        "magrittr",
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
