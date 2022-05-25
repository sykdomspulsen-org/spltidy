
format_nor <- function(x, digits = 0, sig = NULL, break_with_four_digits = T) {
  retval <- vector("character", length = length(x))
  index_not_na <- !is.na(x)
  retval[!index_not_na] <- "IK"

  if(!is.null(sig)) {
    retval[index_not_na] <- formatC(signif(x[index_not_na], digits = sig), big.mark = " ", decimal.mark = ",", format = "f", digits = digits)
  } else {
    retval[index_not_na] <- formatC(x[index_not_na], big.mark = " ", decimal.mark = ",", format = "f", digits = digits)
  }
  index <- which(x[index_not_na] >= 1000 & x[index_not_na] < 10000)
  if (length(index) > 0 & break_with_four_digits == F) retval[index_not_na][index] <- stringr::str_remove(retval[index_not_na][index], " ")
  return(retval)
}

format_nor_perc_0 <- function(x){
  retval <- paste0(format_nor(x, digits = 0), " %")
  retval[retval=="IK %"] <- "IK"
  return(retval)
}
