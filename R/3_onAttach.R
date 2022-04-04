#' @import data.table ggplot2
#' @importFrom magrittr %>%
.onAttach <- function(libname, pkgname) {
    version <- tryCatch(
      utils::packageDescription("splalert", fields = "Version"),
      warning = function(w){
        1
      }
    )
  
  packageStartupMessage(paste0(
    "splalert ",
    version,
    "\n",
    "https://docs.sykdomspulsen.no/splalert"
  ))
}
