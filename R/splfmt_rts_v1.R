formats <- list()
formats$splfmt_rts_v1_mini <- list()
formats$splfmt_rts_v1_mini$unified <- list()
formats$splfmt_rts_v1_mini$unified$granularity_time <- list(
  NA_allowed = FALSE,
  NA_class = NA_character_,
  values_allowed = c("day", "isoweek", "isoyear"),
  class = "character"
)

formats$splfmt_rts_v1_mini$unified$granularity_geo <- list(
  NA_allowed = FALSE,
  NA_class = NA_character_,
  values_allowed = unique(spldata::norway_locations_names()$granularity_geo),
  class = "character"
)

formats$splfmt_rts_v1_mini$unified$country_iso3 <- list(
  NA_allowed = FALSE,
  NA_class = NA_character_,
  values_allowed = c("nor", "den", "swe", "fin"),
  class = "character"
)

formats$splfmt_rts_v1_mini$unified$location_code <- list(
  NA_allowed = FALSE,
  NA_class = NA_character_,
  values_allowed = NULL,
  class = "character"
)

formats$splfmt_rts_v1_mini$unified$border <- list(
  NA_allowed = FALSE,
  NA_class = NA_integer_,
  values_allowed = 2020,
  class = "integer"
)

formats$splfmt_rts_v1_mini$unified$age <- list(
  NA_allowed = TRUE,
  NA_class = NA_character_,
  values_allowed = NULL,
  class = "character"
)

formats$splfmt_rts_v1_mini$unified$sex <- list(
  NA_allowed = TRUE,
  NA_class = NA_character_,
  values_allowed = NULL,
  class = "character"
)

formats$splfmt_rts_v1_mini$unified$isoyear <- list(
  NA_allowed = FALSE,
  NA_class = NA_integer_,
  values_allowed = NULL,
  class = "integer"
)

formats$splfmt_rts_v1_mini$unified$isoyearweek <- list(
  NA_allowed = FALSE,
  NA_class = NA_character_,
  values_allowed = NULL,
  class = "character"
)

formats$splfmt_rts_v1_mini$unified$date <- list(
  NA_allowed = FALSE,
  NA_class = as.Date(NA),
  values_allowed = NULL,
  class = "Date"
)

#' Test data generator
#' @param fmt Format (splfmt_rts_v1_mini)
#' @examples
#' test_data_generator("splfmt_rts_v1_mini")
#' @export
test_data_generator <- function(fmt = "splfmt_rts_v1_mini"){
  stopifnot(fmt %in% c("splfmt_rts_v1_mini"))

  if(fmt == "splfmt_rts_v1_mini"){
    d <- data.table(location_code = spldata::norway_locations_names()[granularity_geo=="county"]$location_code)
    d[, granularity_time := "isoweek"]
    d[, isoyearweek := "2022-03"]
    d[, deaths_n := stats::rpois(.N, 5)]
  }
  return(d)
}


"[.splfmt_rts_v1_mini" = function (x, ...){
  # original call
  modified_call <- orig_call <- sys.calls()[[sys.nframe()-1]]
  healing_calls <- list()

  # smart-assignment
  # try to find which part uses :=
  first_call <- lapply(orig_call, function(x){if(length(x)>1)deparse(x[[1]])})
  i <- as.numeric(which(first_call==":="))
  if(length(i) == 1){
    # smart-assignment for time ----
    # identify if a time variable is mentioned
    lhs <- unlist(lapply(orig_call[[i]][[2]], function(x){deparse(x)}))
    time_vars <- c("isoyear", "isoyearweek", "date")
    time_vars_with_quotes <- c(time_vars, paste0("\"",time_vars,"\""))
    time_var_modified_index <- which(lhs %in% time_vars_with_quotes)

    if(length(time_var_modified_index)>1){
      warning("Multiple time variables specified. Smart-assignment disabled.")
    } else if(length(time_var_modified_index)==1){
      modified_time <- TRUE
      # one date thing is modified
      # find out which type
      time_var_modified <- stringr::str_replace_all(lhs[time_var_modified_index],"\"","")

      if(length(lhs)==1){
        # only one thing on the left
        # need to turn this call into a "multiple assignment" call
        modified_call[[i]][[2]] <- substitute(c(x, "x_modified_timevar_97531"), list(x = deparse(orig_call[[i]][[2]])))
        modified_call[[i]][[3]] <- substitute(.(x, y), list(x = orig_call[[i]][[3]], y = time_var_modified))
      } else {
        # multiple things on the left
        # just need to add x_modified_timevar_97531 to the right most of the multiple assignments
        modified_call[[i]][[2]][[length(lhs) + 1]] <- "x_modified_timevar_97531"
        modified_call[[i]][[3]][[length(lhs) + 1]] <- time_var_modified
      }

      if(time_var_modified=="isoyear"){
        healing_calls[[length(healing_calls)+1]] <- glue::glue(
          '{orig_call[[2]]}[!is.na(x_modified_timevar_97531),c("granularity_time", "isoyearweek", "date") := .("isoyear", spltime::isoyear_to_last_isoyearweek_c(isoyear), spltime::isoyear_to_last_date(isoyear))]'
        )
      } else if(time_var_modified=="isoyearweek"){
        healing_calls[[length(healing_calls)+1]] <- glue::glue(
          '{orig_call[[2]]}[!is.na(x_modified_timevar_97531),c("granularity_time", "isoyear", "date") := .("isoweek", spltime::isoyearweek_to_isoyear_n(isoyearweek), spltime::isoyearweek_to_last_date(isoyearweek))]'
        )
      } else if(time_var_modified=="date"){
        healing_calls[[length(healing_calls)+1]] <- glue::glue(
          '{orig_call[[2]]}[!is.na(x_modified_timevar_97531),c("granularity_time", "isoyear", "isoyearweek") := .("day", spltime::date_to_isoyear_n(date), spltime::date_to_isoyearweek_c(date))]'
        )
      }
      healing_calls[[length(healing_calls)+1]] <- glue::glue(
        '{orig_call[[2]]}[, x_modified_timevar_97531 := NULL]'
      )
    }

    # smart-assignment for geo ----
    # our smart-assignment code always starts off with orig_call = modified_code
    orig_call <- modified_call
    # identify if a geo variable is mentioned
    lhs <- unlist(lapply(orig_call[[i]][[2]], function(x){deparse(x)}))
    geo_vars <- c("granularity_geo", "location_code", "iso3")
    geo_vars_with_quotes <- c(geo_vars, paste0("\"",geo_vars,"\""))
    geo_var_modified_index <- which(lhs %in% geo_vars_with_quotes)

    if(length(geo_var_modified_index)>1){
      warning("Multiple geo variables specified. Smart-assignment disabled.")
    } else if(length(geo_var_modified_index)==1){
      modified_geo <- TRUE
      # one date thing is modified
      # find out which type
      geo_var_modified <- stringr::str_replace_all(lhs[geo_var_modified_index],"\"","")

      if(length(lhs)==1){
        # only one thing on the left
        # need to turn this call into a "multiple assignment" call
        modified_call[[i]][[2]] <- substitute(c(x, "x_modified_geovar_97531"), list(x = deparse(orig_call[[i]][[2]])))
        modified_call[[i]][[3]] <- substitute(.(x, y), list(x = orig_call[[i]][[3]], y = geo_var_modified))
      } else {
        # multiple things on the left
        # just need to add x_modified_geovar_97531 to the right most of the multiple assignments
        modified_call[[i]][[2]][[length(lhs) + 1]] <- "x_modified_geovar_97531"
        modified_call[[i]][[3]][[length(lhs) + 1]] <- geo_var_modified
      }

      if(geo_var_modified=="location_code"){
        healing_calls[[length(healing_calls)+1]] <- glue::glue(
          '{orig_call[[2]]}[!is.na(x_modified_geovar_97531),c("granularity_geo", "country_iso3") := .(spldata::location_code_to_granularity_geo(location_code), spldata::location_code_to_iso3(location_code))]'
        )
      }
      healing_calls[[length(healing_calls)+1]] <- glue::glue(
        '{orig_call[[2]]}[, x_modified_geovar_97531 := NULL]'
      )
    }
  }
  # print(orig_call)
  # print(modified_call)
  # print(healing_calls)

  old_class <- attr(x, "class")
  on.exit(setattr(x, "class", old_class))
  setattr(x, "class", class(x)[-1])

  #print(modified_call)
  eval(parse(text = deparse(modified_call)), envir = parent.frame(1:2))
  lapply(healing_calls, function(x){eval(parse(text = x), envir = parent.frame(1:2))})

  return(invisible(x))
}


create_unified_columns <- function (x, ...) {
  UseMethod("create_unified_columns", x)
}

heal <- function (x, ...) {
  UseMethod("heal", x)
}

create_unified_columns.splfmt_rts_v1_mini <- function(x){
  fmt <- attr(x, "format_unified")
  for(i in names(fmt)){
    if(!i %in% names(x)){
      x[,(i) := fmt[[i]]$NA_class]
    }
  }
  setcolorder(x, names(fmt))
}


heal.splfmt_rts_v1_mini <- function(x){
  create_unified_columns(x)

  # granularity_time = mandatory

  # granularity_geo = interpolate from location_code
  d[is.na(granularity_geo) & !is.na(location_code), location_code := location_code]

  # country_iso3
  d[is.na(country_iso3) & !is.na(location_code), location_code := location_code]

  d[granularity_time=="isoyear" & !is.na(isoyear) & (is.na(isoyearweek) | is.na(date)), isoyear := isoyear]
  d[granularity_time=="isoweek" & !is.na(isoyearweek) & (is.na(isoyear) | is.na(date)), isoyearweek := isoyearweek]
  d[granularity_time=="day" & !is.na(date) & (is.na(isoyear) | is.na(isoyearweek)), date := date]

  return(invisible(x))
}


#' Set as splfmt_rts_v1_mini
#' @param x The data.table to be converted to splfmt_rts_v1_mini
#' @examples
#' d <- test_data_generator()
#' set_splfmt_rts_v1_mini(d)
#' d[1,isoyearweek := "2021-01"]
#' d
#' d[2,isoyear := 2019]
#' d
#' d[4:5,date := as.Date("2020-01-01")]
#' d
#' d[10,c("isoyear","isoyearweek") := .(2021,"2021-01")]
#' d
#' d[10,c("location_code") := .("norge")]
#' d
#' @export
set_splfmt_rts_v1_mini <- function(x){
  if(!is.data.table(x)){
    stop("x must be data.table. Run setDT('x').")
  }

  fmt <- formats$splfmt_rts_v1_mini$unified
  setattr(x, "format_unified", fmt)
  setattr(x, "class", unique(c("splfmt_rts_v1_mini", class(x))))

  create_unified_columns.splfmt_rts_v1_mini(x)
  heal.splfmt_rts_v1_mini(x)

  return(invisible(x))
}


validate <- function(x){
  new_hash <- digest::digest(attr(x, ".internal.selfref"),"md5")
  old_hash <- attr(x, "hash")

  # data hasnt changed since last validation
  if(identical(new_hash, old_hash)){
    #return(invisible())
  }
  status <- attr(x, "status")
  if(is.null(status)) status <- list()

  fmt <- attr(x, "format_unified")
  for(i in names(fmt)){
    fmt_i <- fmt[[i]]
    status_i <- list()
    status_i$errors <- "\U274C Errors:"

    # variable doesn't exist
    if(!i %in% names(x)){
      status_i$errors <- paste0(status_i$errors,"\n- Variable doesn't exist")
    } else {
      # check for NAs allowed
      if(fmt_i$NA_allowed==FALSE & sum(is.na(x[[i]])) > 0){
        status_i$errors <- paste0(status_i$errors,"\n- NA exists (but not allowed)")
      }
      # if(!is.null())

      if(status_i$errors=="\U274C Errors:") status_i$errors <- "\U2705 No errors"
    }
    status[[i]] <- status_i
  }

  setattr(x, "status", status)
  setattr(x, "hash", new_hash)
}

summary.splfmt_rts_v1_mini <- function(object, ...){
  validate(object)
  status <- attr(object, "status")

  for(i in names(status)){
    status_i <- status[[i]]

    cat("\n", crayon::underline(i), "\n", sep="")
    cat(status_i$errors, "\n", sep="")
  }
}


hash_structure <- function(x){
  # Take in the data table
  # data <- data$cases
  # data <- data$vax

  d <- copy(data)


  extra_cols <- 5:ncol(d)

  d1 <- copy(d)
  d1[, (extra_cols[1]) := NULL]
  # d1[, cases_testdate_n := NULL]

  d2 <- copy(d)
  d2[, (extra_cols[2]) := NULL]
  # d2[, cases_testdate_n_sum0_13 := NULL]

  l1 <- list()

  l1$d1 <- d1
  l1$d2 <- d2

  # a vector of all the column names
  q <- 1
  q1 <- list()


  for (d in l1) {
    # d <- l1$d1
    colnames_all <- colnames(d)


    # one vector of the standard column names, and one for all the other column names
    standard_cols <- c("granularity_time", "granularity_geo", "age", "sex")
    color_col_names <- colnames_all[!(colnames_all %in% standard_cols)]

    # make a list that consists of lists with all the values from the to-be-colored columns
    color_cols <- list()
    j <- 1

    color_cols[[1]] <- d[[5]]

    # remove the column that will be colored
    d <- subset(d, select = standard_cols)

    # Adding colors to the rows that exist
    # blue - if it exist and has values
    # yellow - if it exist and is NA
    t1 <- dplyr::case_when(
      !is.na(color_cols[[1]]) ~ "blue",
      TRUE ~ "yellow"
    )
    d <- cbind(d, t1)
    setnames(d, "t1", color_col_names[1])

    # Removes duplicates
    d <- unique(d)

    # Make a data table of all possible combinations of the variables
    ages <- unique(d$age)
    sex <- unique(d$sex)
    gran_geo <- unique(d$granularity_geo)
    gran_time <- unique(d$granularity_time)

    d_1 <- CJ(granularity_time = gran_time, granularity_geo = gran_geo, age = ages, sex = sex)

    # Removing all the rows in d from d_1
    d_1 <- d_1[!d, on = .(granularity_time, granularity_geo, age, sex)]

    # if d_1 has any rows, they should be red, so they get filled with NA
    if (nrow(d_1) != 0) {
      d <- rbind(d, d_1, fill = TRUE)
    }

    # set order to make it easier to look over
    setorder(d, granularity_time, granularity_geo, age, sex)

    # name <- paste0("q_", q)
    name <- names(d)[5]

    d <- unique(d)

    q <- q + 1

    # d <- dcast(d, granularity_time + age + sex + get(name) ~ granularity_geo, value.var = get(name))

    q1[[name]] <- d
  }

}
