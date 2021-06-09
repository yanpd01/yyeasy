## for reexports ---------------------------------------------------------------
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' @importFrom magrittr %T>%
#' @export
magrittr::`%T>%`

#' @importFrom magrittr %$%
#' @export
magrittr::`%$%`

#' @importFrom treeio read.newick
#' @export
treeio::read.newick


## for depends and attach ------------------------------------------------------
#' @importFrom tidyverse tidyverse_conflicts
#' @importFrom extrafont fonts
#' @importFrom vegan rda
#' @importFrom phyloseq phyloseq
NULL



## for attach  auto library ----------------------------------------------------
# run_library <- function(pkg) {
#     loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
#     do.call(
#         "library",
#         list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = TRUE)
#     )
# }
#
# .onAttach <- function(...) {
#     core <- c("extrafont", "vegan", "tidyverse")
#     tmp0 <- paste0("package:", core) %in% search()
#     needed <- core[!tmp0]
#     if (length(needed) == 0)
#         return(invisible())
#     lapply(needed, run_library)
#     invisible()
# }

