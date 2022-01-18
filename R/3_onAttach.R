#' @import data.table ggplot2
#' @importFrom magrittr %>%
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "spltidy ",
    # utils::packageDescription("spltidy")$Version,
    "\n",
    "https://docs.sykdomspulsen.no/spltidy"
  ))
}
