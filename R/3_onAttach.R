#' @import data.table ggplot2
#' @importFrom magrittr %>%
.onAttach <- function(libname, pkgname) {
  version <- tryCatch(
    utils::packageDescription("spltidy", fields = "Version"),
    warning = function(w){
      1
    }
  )

  packageStartupMessage(paste0(
    "spltidy ",
    version,
    "\n",
    "https://docs.sykdomspulsen.no/spltidy"
  ))
}

dummy_function <- function(){
  spltime::isoyearweek_to_isoweek_c("2021-01")
}

