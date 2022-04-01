#' pivot_wider
#' @param data tbl object
#' @param ... dots
#' @method pivot_wider data.table
#' @export
#' @importFrom tidyr pivot_wider
pivot_wider.data.table <- function(
  data,
  ...
){
  data <- as.data.frame(data)
  retval <- tidyr::pivot_wider(
    data = data,
    ...
  ) %>%
    as.data.table()
  return(retval)

}


