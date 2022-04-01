#' collect
#' @param data tbl object
#' @param id_cols id_cols
#' @param names_from names_from
#' @param names_prefix names_prefix
#' @param names_sep names_sep
#' @param names_glue names_glue
#' @param names_sort names_sort
#' @param names_repair names_repair
#' @param values_from values_from
#' @param values_fill values_fill
#' @param values_fn values_fn
#' @param ... dots
#' @method pivot_wider data.table
#' @export
#' @importFrom tidyr pivot_wider
pivot_wider.data.table <- function(
  data,
  id_cols = NULL,
  names_from = name,
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_repair = "check_unique",
  values_from = value,
  values_fill = NULL,
  values_fn = NULL,
  ...
){

  data <- as.data.frame(data)
  retval <- NextMethod() %>%
    as.data.table()
  return(retval)
}


