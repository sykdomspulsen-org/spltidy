formats <- list()
formats$splfmt_rts_data_v1 <- list()
formats$splfmt_rts_data_v1$unified <- list()
formats$splfmt_rts_data_v1$unified$granularity_time <- list(
  NA_allowed = FALSE,
  NA_class = NA_character_,
  values_allowed = c("day", "isoweek", "isoyear"),
  class = "character"
)

formats$splfmt_rts_data_v1$unified$granularity_geo <- list(
  NA_allowed = FALSE,
  NA_class = NA_character_,
  values_allowed = unique(spldata::norway_locations_names()$granularity_geo),
  class = "character"
)

formats$splfmt_rts_data_v1$unified$country_iso3 <- list(
  NA_allowed = FALSE,
  NA_class = NA_character_,
  values_allowed = c("nor", "den", "swe", "fin"),
  class = "character"
)

formats$splfmt_rts_data_v1$unified$location_code <- list(
  NA_allowed = FALSE,
  NA_class = NA_character_,
  values_allowed = NULL,
  class = "character"
)

formats$splfmt_rts_data_v1$unified$border <- list(
  NA_allowed = FALSE,
  NA_class = NA_integer_,
  values_allowed = 2020,
  class = "integer"
)

formats$splfmt_rts_data_v1$unified$age <- list(
  NA_allowed = TRUE,
  NA_class = NA_character_,
  values_allowed = NULL,
  class = "character"
)

formats$splfmt_rts_data_v1$unified$sex <- list(
  NA_allowed = TRUE,
  NA_class = NA_character_,
  values_allowed = NULL,
  class = "character"
)

formats$splfmt_rts_data_v1$unified$isoyear <- list(
  NA_allowed = TRUE,
  NA_class = NA_integer_,
  values_allowed = NULL,
  class = "integer"
)

formats$splfmt_rts_data_v1$unified$isoweek <- list(
  NA_allowed = TRUE,
  NA_class = NA_integer_,
  values_allowed = NULL,
  class = "integer"
)

formats$splfmt_rts_data_v1$unified$isoyearweek <- list(
  NA_allowed = FALSE,
  NA_class = NA_character_,
  values_allowed = NULL,
  class = "character"
)

formats$splfmt_rts_data_v1$unified$season <- list(
  NA_allowed = TRUE,
  NA_class = NA_character_,
  values_allowed = NULL,
  class = "character"
)

formats$splfmt_rts_data_v1$unified$seasonweek <- list(
  NA_allowed = TRUE,
  NA_class = NA_real_,
  values_allowed = NULL,
  class = "numeric"
)

formats$splfmt_rts_data_v1$unified$calyear <- list(
  NA_allowed = TRUE,
  NA_class = NA_integer_,
  values_allowed = NULL,
  class = "integer"
)

formats$splfmt_rts_data_v1$unified$calmonth <- list(
  NA_allowed = TRUE,
  NA_class = NA_integer_,
  values_allowed = NULL,
  class = "integer"
)

formats$splfmt_rts_data_v1$unified$calyearmonth <- list(
  NA_allowed = TRUE,
  NA_class = NA_character_,
  values_allowed = NULL,
  class = "character"
)

formats$splfmt_rts_data_v1$unified$date <- list(
  NA_allowed = FALSE,
  NA_class = as.Date(NA),
  values_allowed = NULL,
  class = "Date"
)

#' Remove class splfmt_rts_data_*
#' @param x data.table
#' @examples
#' x <- spltidy::generate_test_data() %>%
#'   spltidy::set_splfmt_rts_data_v1()
#' class(x)
#' spltidy::remove_class_splfmt_rts_data(x)
#' class(x)
#' @family splfmt_rts_data
#' @export
remove_class_splfmt_rts_data <- function(x) {
  classes <- class(x)
  classes <- classes[!stringr::str_detect(classes, "^splfmt_rts_data_")]
  setattr(x, "class", classes)
  return(invisible(x))
}


#' Generate test data
#'
#' @description
#' Generates some test data
#'
#' @param fmt Data format (\code{splfmt_rts_data_v1})
#' @examples
#' spltidy::generate_test_data("splfmt_rts_data_v1")
#' @export
generate_test_data <- function(fmt = "splfmt_rts_data_v1") {
  stopifnot(fmt %in% c("splfmt_rts_data_v1"))

  if (fmt == "splfmt_rts_data_v1") {
    d1 <- data.table(location_code = spldata::norway_locations_names()[granularity_geo == "county"]$location_code)
    d1[, granularity_time := "isoweek"]
    d1[, isoyearweek := "2022-03"]
    d1[, deaths_n := stats::rpois(.N, 5)]

    d2 <- copy(d1)
    d2[, isoyear := 2022]
    d2[, age := "total"]
    d2[, sex := "total"]

    d3 <- copy(d1)
    d3[, isoyear := 2022]
    d3[, age := "000-005"]
    d3[, sex := "total"]

    d <- rbind(d1, d2, d3, fill = T)
  }
  return(d)
}

#' @export
print.splfmt_rts_data_v1 <- function(x, ...) {
  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-faq.html#ok-thanks.-what-was-so-difficult-about-the-result-of-dti-col-value-being-returned-invisibly
  if (!data.table::shouldPrint(x)) {
    return(invisible(x))
  }
  # this function is recursive, so that
  dots <- list(...)
  print_dots_before_row <- 999999999999999
  if (nrow(x) == 0) {
    cat(glue::glue("splfmt_rts_data_v1 with {ncol(x)} columns and 0 rows"))
    cat(names(x))
    return(invisible(x))
  } else if (nrow(x) > 100) {
    row_numbers <- c(1:nrow(x))[c(1:5, (nrow(x) - 4):nrow(x))]
    to_print <- x[c(1:5, (.N - 4):.N)]
    print_dots_before_row <- 6
  } else {
    row_numbers <- 1:nrow(x)
    to_print <- copy(x)
  }

  # unified vs context
  format_unified <- attr(to_print, "format_unified")
  variable_types <- rep("[context]", ncol(to_print))
  for (i in seq_along(variable_types)) {
    pos <- which(names(format_unified) %in% names(to_print))
    # if(length(pos))
  }
  names(to_print)
  names(format_unified)
  variable_types <- dplyr::case_when(
    names(to_print) %in% names(format_unified) ~ "[unified]",
    TRUE ~ "[context]"
  )

  # classes
  variable_classes <- paste0("<", unlist(lapply(to_print, class)), ">")
  variable_names <- names(to_print)
  row_numbers <- formatC(row_numbers, width = max(nchar(row_numbers))) %>%
    paste0(":", sep = "")
  row_number_spacing <- formatC("", width = max(nchar(row_numbers)))

  # round numeric so it isnt too big
  vars_to_round <- variable_names[variable_classes=="<numeric>"]
  for(i in vars_to_round){
    to_print[,(i) := round(get(i), 4)]
  }

  # missing
  na_percent <- rep("X", ncol(to_print))
  na_percent_red <- rep(FALSE, ncol(to_print))
  for(i in seq_along(na_percent)){
    var = names(x)[i]
    val <- x[,.(val = mean(is.na(get(var))))]$val
    na_percent[i] <- glue::glue("NA={splstyle::format_nor_perc_0(val*100)}")
    if(val>0){
      na_percent_red[i] <- TRUE
    }
  }

  width_char <- apply(to_print, 2, nchar, keepNA = F) %>%
    rbind(nchar(variable_types)) %>%
    rbind(nchar(variable_classes)) %>%
    rbind(nchar(na_percent)) %>%
    rbind(nchar(variable_names)) %>%
    apply(2, max)

  max_width <- getOption("width") - 5
  cum_width <- nchar(row_number_spacing) + cumsum(width_char + 3)
  breaks <- floor(cum_width / max_width)

  if (!"recursive" %in% names(dots)) {
    # if this is the first time the function is run, then it just acts
    # as the brain, and determines how many times the function needs to be called
    for (i in unique(breaks)) {
      print(x[, names(breaks)[breaks == i], with = F], recursive = TRUE)
      cat("\n")
    }
    return(invisible(x))
  } else {
    # we are in the recursive level, so we actually print things!
    for (i in -3:nrow(to_print)) {
      if (i == print_dots_before_row) {
        for (j in seq_len(max(cum_width))) cat(".")
        cat("\n")
      }

      if (i <= 0) {
        cat(row_number_spacing)
      } else {
        cat(row_numbers[i])
      }
      for (j in 1:ncol(to_print)) {
        cat("   ")

        if (i == -3) {
          cat(formatC(variable_types[j], width = width_char[j]))
        } else if (i == -2) {
          cat(formatC(variable_classes[j], width = width_char[j]))
        } else if (i == -1) {
          if(na_percent_red[j] == TRUE){
            cat(crayon::red(formatC(na_percent[j], width = width_char[j])))
          } else {
            cat(formatC(na_percent[j], width = width_char[j]))
          }
        } else if (i == 0) {
          cat(crayon::bold(formatC(variable_names[j], width = width_char[j])))
        } else {
          cat(formatC(as.character(to_print[i][[j]]), width = width_char[j]))
        }
      }
      cat("\n")
    }
    return(invisible(x))
  }
}

#' @method [ splfmt_rts_data_v1
#' @export
"[.splfmt_rts_data_v1" <- function(x, ...) {
  # original call
  modified_call <- orig_call <- sys.calls()[[sys.nframe() - 1]]
  healing_calls <- list()
  # print(orig_call)

  # variable_in_sys_call <- orig_call[[2]]
  # if(!variable_in_sys_call %in% ls(parent.frame(1:2))){
  #   stop(glue::glue("{variable_in_sys_call} is not in parent.frame(1:2)"))
  # }
  # if(!is.data.table(get(variable_in_sys_call, envir = parent.frame(1:2)))){
  #   stop(glue::glue("{variable_in_sys_call} is not data.table"))
  #   x$.internal.selfref
  # }

  # smart-assignment
  # try to find which part uses :=
  first_call <- lapply(orig_call, function(x) {
    if (length(x) > 1) deparse(x[[1]])
  })
  i <- as.numeric(which(first_call == ":="))
  if (length(i) == 0) {
    # no assignment
    remove_class_splfmt_rts_data(x)
    on.exit(set_splfmt_rts_data_v1(x, create_unified_columns = FALSE, heal = FALSE))

    y <- eval(parse(text = deparse(modified_call)), envir = parent.frame(1:2))
    set_splfmt_rts_data_v1(y, create_unified_columns = FALSE, heal = FALSE)
    return(invisible(y))
  } else if (length(i) == 1) {
    # smart-assignment for time ----
    # identify_data_structure if a time variable is mentioned
    lhs <- unlist(lapply(orig_call[[i]][[2]], function(x) {
      deparse(x)
    }))
    time_vars <- c("isoyear", "isoyearweek", "date")
    time_vars_with_quotes <- c(time_vars, paste0("\"", time_vars, "\""))
    time_var_modified_index <- which(lhs %in% time_vars_with_quotes)

    if (length(time_var_modified_index) > 1) {
      warning("Multiple time variables specified. Smart-assignment disabled.")
    } else if (length(time_var_modified_index) == 1) {
      modified_time <- TRUE
      # one date thing is modified
      # find out which type
      time_var_modified <- stringr::str_replace_all(lhs[time_var_modified_index], "\"", "")

      if (length(lhs) == 1) {
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

      if (time_var_modified == "isoyear") {
        healing_options <- list(
          "granularity_time" = "\"isoyear\"",
          "isoweek" = "spltime::isoyear_to_last_isoweek_n(isoyear)",
          "isoyearweek" = "spltime::isoyear_to_last_isoyearweek_c(isoyear)",
          "season" = "NA_character_",
          "seasonweek" = "NA_real_",
          "calyear" = "NA_integer_",
          "calmonth" = "NA_integer_",
          "calyearmonth" = "NA_character_",
          "date" = "spltime::isoyear_to_last_date(isoyear)"
        )
      } else if (time_var_modified == "isoyearweek") {
        healing_options <- list(
          "granularity_time" = "\"isoweek\"",
          "isoyear" = "spltime::isoyearweek_to_isoyear_n(isoyearweek)",
          "isoweek" = "spltime::isoyearweek_to_isoweek_n(isoyearweek)",
          "season" = "spltime::isoyearweek_to_season_c(isoyearweek)",
          "seasonweek" = "spltime::isoyearweek_to_seasonweek_n(isoyearweek)",
          "calyear" = "NA_integer_",
          "calmonth" = "NA_integer_",
          "calyearmonth" = "NA_character_",
          "date" = "spltime::isoyearweek_to_last_date(isoyearweek)"
        )
      } else if (time_var_modified == "date") {
        healing_options <- list(
          "granularity_time" = "\"day\"",
          "isoyear" = "spltime::date_to_isoyear_n(date)",
          "isoweek" = "spltime::date_to_isoweek_n(date)",
          "isoyearweek" = "spltime::date_to_isoyearweek_c(date)",
          "season" = "spltime::date_to_season_c(date)",
          "seasonweek" = "spltime::date_to_seasonweek_n(date)",
          "calyear" = "spltime::date_to_calyear_n(date)",
          "calmonth" = "spltime::date_to_calmonth_n(date)",
          "calyearmonth" = "spltime::date_to_calyearmonth_c(date)"
        )
      } else {
        healing_options <- NULL
      }

      if (!is.null(healing_options)) {
        healing_options <- healing_options[names(healing_options) %in% names(x)]
        if (length(healing_options) > 0) {
          healing_calls[[length(healing_calls) + 1]] <- glue::glue(
            '{orig_call[[2]]}[!is.na(x_modified_timevar_97531),
            c("{paste0(names(healing_options), collapse="\\",\\"")}")
            :=
            .({paste0(healing_options, collapse=",")})
            ]'
          )
        }
      }

      healing_calls[[length(healing_calls) + 1]] <- glue::glue(
        "{orig_call[[2]]}[, x_modified_timevar_97531 := NULL]"
      )
    }

    # smart-assignment for geo ----
    # our smart-assignment code always starts off with orig_call = modified_code
    orig_call <- modified_call
    # identify_data_structure if a geo variable is mentioned
    lhs <- unlist(lapply(orig_call[[i]][[2]], function(x) {
      deparse(x)
    }))
    geo_vars <- c("granularity_geo", "location_code", "country_iso3")
    geo_vars_with_quotes <- c(geo_vars, paste0("\"", geo_vars, "\""))
    geo_var_modified_index <- which(lhs %in% geo_vars_with_quotes)

    if (length(geo_var_modified_index) > 1) {
      warning("Multiple geo variables specified. Smart-assignment disabled.")
    } else if (length(geo_var_modified_index) == 1) {
      modified_geo <- TRUE
      # one date thing is modified
      # find out which type
      geo_var_modified <- stringr::str_replace_all(lhs[geo_var_modified_index], "\"", "")

      if (length(lhs) == 1) {
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

      if (geo_var_modified == "location_code") {
        healing_options <- list(
          "granularity_geo" = "spldata::location_code_to_granularity_geo(location_code)",
          "country_iso3" = "spldata::location_code_to_iso3(location_code)"
        )
      } else {
        healing_options <- NULL
      }

      if (!is.null(healing_options)) {
        healing_options <- healing_options[names(healing_options) %in% names(x)]
        if (length(healing_options) > 0) {
          healing_calls[[length(healing_calls) + 1]] <- glue::glue(
            '{orig_call[[2]]}[!is.na(x_modified_geovar_97531),
            c("{paste0(names(healing_options), collapse="\\",\\"")}")
            :=
            .({paste0(healing_options, collapse=",")})
            ]'
          )
        }
      }

      healing_calls[[length(healing_calls) + 1]] <- glue::glue(
        "{orig_call[[2]]}[, x_modified_geovar_97531 := NULL]"
      )
    }
    # print(orig_call)
    # print(modified_call)
    # print(healing_calls)

    remove_class_splfmt_rts_data(x)
    on.exit(set_splfmt_rts_data_v1(x, create_unified_columns = FALSE, heal = FALSE))

    eval(parse(text = deparse(modified_call)), envir = parent.frame(1:2))
    for (i in seq_along(healing_calls)) {
      eval(parse(text = healing_calls[[i]]), envir = parent.frame(1:2))
    }

    return(invisible(x))
  }
}

#' Impute missing values
#'
#' @description
#' Tries to impute missing values by performing smart assignment on all columns that are missing data.
#' E.g. if \code{location_code='norge'} then we know that \code{granularity_geo='nation'}.
#'
#' @section splfmt_rts_data_v1:
#' The **columns in bold** will be used to impute the listed columns.
#'
#' **location_code**:
#' - granularity_geo
#' - country_iso3
#'
#' **isoyear** (when `granularity_time=="isoyear"`):
#' - isoweek
#' - isoyearweek
#' - season
#' - seasonweek
#' - calyear
#' - calmonth
#' - calyearmonth
#' - date
#'
#' **isoyearweek** (when `granularity_time=="isoweek"`):
#' - isoyear
#' - isoweek
#' - season
#' - seasonweek
#' - calyear
#' - calmonth
#' - calyearmonth
#' - date
#'
#' **date** (when `granularity_time=="day"`):
#' - isoyear
#' - isoweek
#' - isoyearweek
#' - season
#' - seasonweek
#' - calyear
#' - calmonth
#' - calyearmonth
#'
#' With regards to the time columns, `granularity_time` takes precedence over everything.
#' If `granularity_time` is missing, then we try to impute `granularity_time` by seeing if
#' there is only one time column with non-missing data. Due to the multitude of time columns,
#' `granularity_time` is an extremely important column and should always be kept with valid values.
#'
#' @param x An object of type \code{\link{splfmt_rts_data_v1}}
#' @param ... Not used.
#' @family splfmt_rts_data
#' @export
heal <- function(x, ...) {
  UseMethod("heal", x)
}

#' @method heal splfmt_rts_data_v1
#' @export
heal.splfmt_rts_data_v1 <- function(x, ...) {
  assert_classes.splfmt_rts_data_v1(x)

  # making sure that granularity_time is taken care of
  # if granularity_time doesn't exist, then make it exist
  # and try to imput it straight away
  # granularity_time is a special case because it is very
  # difficult to identify_data_structure which of the time-variables
  # takes precedence over the others (without using granularity_time)
  if(!"granularity_time" %in% names(x)){
    x[, granularity_time := NA_character_]
    on.exit(x[, granularity_time := NULL])
  }
  time_vars <- c(
    "isoyear",
    "isoweek",
    "isoyearweek",
    "season",
    "seasonweek",
    "calyear",
    "calmonth",
    "calyearmonth",
    "date"
  )
  time_vars <- time_vars[time_vars %in% names(x)]
  time_vars_to_loop_through <- time_vars[time_vars %in% c("isoyear","isoyearweek","date")]
  for(i in time_vars_to_loop_through){
    other_time_vars <- time_vars[time_vars != i]

    if(i=="isoyearweek"){
      time_var_as_granularity_geo <- "isoweek"
    } else if(i=="date"){
      time_var_as_granularity_geo <- "day"
    } else {
      time_var_as_granularity_geo <- i
    }
    if(length(other_time_vars)>=1){
      txt <- glue::glue(
        '
            x[!is.na({i}) & is.na({paste0(other_time_vars, collapse=") & is.na(")}), granularity_time := "{time_var_as_granularity_geo}"]
            '
      )
    } else {
      txt <- glue::glue(
        '
            x[!is.na({i}), granularity_time := "{time_var_as_granularity_geo}"]
            '
      )
    }
    eval(parse(text = txt))
  }

  # granularity_time = mandatory
  imputing_vars_geo <- list(
    "location_code" = c("granularity_geo", "country_iso3")
  )

  imputing_vars_time <- list(
    "isoyear" = c(
      "isoweek",
      "isoyearweek",
      "season",
      "seasonweek",
      "calyear",
      "calmonth",
      "calyearmonth",
      "date"
    ),
    "isoyearweek" = c(
      "isoyear",
      "isoweek",
      "season",
      "seasonweek",
      "calyear",
      "calmonth",
      "calyearmonth",
      "date"
    ),
    "date" = c(
      "isoyear",
      "isoweek",
      "isoyearweek",
      "season",
      "seasonweek",
      "calyear",
      "calmonth",
      "calyearmonth"
    )
  )

  for(type in c("geo", "time")){
    if(type=="geo"){
      imputing_vars <- imputing_vars_geo
    } else if(type=="time"){
      imputing_vars <- imputing_vars_time
    } else {
      stop("")
    }

    for (i in seq_along(imputing_vars)) {
      imputed_from <- names(imputing_vars)[i]
      to_be_imputed <- imputing_vars[[i]]
      to_be_imputed <- to_be_imputed[to_be_imputed %in% names(x)]

      if(type=="geo"){
        extra_restriction <- ''
      } else if(type=="time"){
        if(imputed_from=="isoyearweek"){
          time_var_as_granularity_time <- "isoweek"
        } else if(imputed_from=="date"){
          time_var_as_granularity_time <- "day"
        } else {
          time_var_as_granularity_time <- imputed_from
        }
        extra_restriction <- glue::glue('granularity_time==\"{time_var_as_granularity_time}" &')
      } else {
        stop("")
      }

      if (imputed_from %in% names(x) & length(to_be_imputed) > 0) {
        txt <- glue::glue(
          '
          x[{extra_restriction} !is.na({imputed_from}) & (is.na({paste0(to_be_imputed, collapse=")|is.na(")})), {imputed_from} := {imputed_from}]
          '
        )
        eval(parse(text = txt))
      }
    }
  }

  # allows us to print
  data.table::shouldPrint(x)

  return(invisible(x))
}

#' Create unified columns
#'
#' @description
#' Creates the pre-specified unified columns for some data formats.
#'
#' @section splfmt_rts_data_v1:
#' \code{splfmt_rts_data_v1} contains 16 unified columns:
#' - granularity_time
#' - granularity_geo
#' - country_iso3
#' - location_code
#' - border
#' - age
#' - sex
#' - isoyear
#' - isoweek
#' - isoyearweek
#' - season
#' - seasonweek
#' - calyear
#' - calmonth
#' - calyearmonth
#' - date
#'
#' For more details see the vignette:
#' \code{vignette("splfmt_rts_data_v1", package = "spltidy")}
#'
#' @param x An object of type \code{\link{splfmt_rts_data_v1}}
#' @param ... Arguments passed to or from other methods
#' @family splfmt_rts_data
#' @export
create_unified_columns <- function(x, ...) {
  UseMethod("create_unified_columns", x)
}

#' @method create_unified_columns splfmt_rts_data_v1
#' @export
create_unified_columns.splfmt_rts_data_v1 <- function(x, ...) {
  fmt <- attr(x, "format_unified")
  for (i in names(fmt)) {
    if (!i %in% names(x)) {
      # create empty columns
      x[, (i) := fmt[[i]]$NA_class]
    }
  }
  setcolorder(x, names(fmt))

  # heal it
  heal.splfmt_rts_data_v1(x)

  # allows us to print
  data.table::shouldPrint(x)

  return(invisible(x))
}

assert_classes <- function(x, ...) {
  UseMethod("assert_classes", x)
}

assert_classes.splfmt_rts_data_v1 <- function(x) {
  fmt <- attr(x, "format_unified")
  classes_real <- lapply(x, class)
  classes_wanted <- lapply(fmt, function(x) {
    x$class
  })
  # just take the ones that are intersected
  classes_wanted <- classes_wanted[names(classes_wanted) %in% names(classes_real)]
  classes_real <- classes_real[names(classes_real) %in% names(classes_wanted)]
  for (i in names(classes_real)) {
    if (classes_real[[i]] != classes_wanted[[i]]) {
      # force class
      if (classes_wanted[[i]] == "Date") {
        x[, (i) := as.Date(get(i))]
      } else {
        x[, (i) := methods::as(get(i), classes_wanted[[i]])]
      }
    }
  }

  # allows us to print
  data.table::shouldPrint(x)

  return(invisible(x))
}

#' Convert data.table to splfmt_rts_data_v1
#'
#' @description
#' \code{set_splfmt_rts_data_v1} converts a \code{data.table} to \code{splfmt_rts_data_v1} by reference.
#' \code{splfmt_rts_data_v1} creates a new \code{splfmt_rts_data_v1} (not by reference) from either a \code{data.table} or \code{data.frame}.
#'
#' @section Smart assignment:
#' \code{splfmt_rts_data_v1} contains the smart assignment feature for time and geography.
#'
#' When the **variables in bold** are assigned using `:=`, the listed variables will be automatically imputed.
#'
#' **location_code**:
#' - granularity_geo
#' - country_iso3
#'
#' **isoyear**:
#' - granularity_time
#' - isoweek
#' - isoyearweek
#' - season
#' - seasonweek
#' - calyear
#' - calmonth
#' - calyearmonth
#' - date
#'
#' **isoyearweek**:
#' - granularity_time
#' - isoyear
#' - isoweek
#' - season
#' - seasonweek
#' - calyear
#' - calmonth
#' - calyearmonth
#' - date
#'
#' **date**:
#' - granularity_time
#' - isoyear
#' - isoweek
#' - isoyearweek
#' - season
#' - seasonweek
#' - calyear
#' - calmonth
#' - calyearmonth
#'
#' @section Unified columns:
#' \code{splfmt_rts_data_v1} contains 16 unified columns:
#' - granularity_time
#' - granularity_geo
#' - country_iso3
#' - location_code
#' - border
#' - age
#' - sex
#' - isoyear
#' - isoweek
#' - isoyearweek
#' - season
#' - seasonweek
#' - calyear
#' - calmonth
#' - calyearmonth
#' - date
#'
#' @details
#' For more details see the vignette:
#' \code{vignette("splfmt_rts_data_v1", package = "spltidy")}
#'
#' @return An extended \code{data.table}, which has been modified by reference and returned (invisibly).
#'
#' @param x The data.table to be converted to splfmt_rts_data_v1
#' @param create_unified_columns Do you want it to \code{\link{create_unified_columns}}?
#' @param heal Do you want to \code{\link{heal}} on creation?
#' @examples
#' # Create some fake data as data.table
#' d <- spltidy::generate_test_data(fmt = "splfmt_rts_data_v1")
#' d <- d[1:5]
#'
#' # convert to splfmt_rts_data_v1 by reference
#' spltidy::set_splfmt_rts_data_v1(d, create_unified_columns = TRUE)
#'
#' #
#' d[1, isoyearweek := "2021-01"]
#' d
#' d[2, isoyear := 2019]
#' d
#' d[3, date := as.Date("2020-01-01")]
#' d
#' d[4, c("isoyear", "isoyearweek") := .(2021, "2021-01")]
#' d
#' d[5, c("location_code") := .("norge")]
#' d
#'
#' # Investigating the data structure of one column inside a dataset
#' spltidy::generate_test_data() %>%
#'   spltidy::set_splfmt_rts_data_v1() %>%
#'   spltidy::identify_data_structure("deaths_n") %>%
#'   plot()
#' # Investigating the data structure via summary
#' spltidy::generate_test_data() %>%
#'   spltidy::set_splfmt_rts_data_v1() %>%
#'   summary()
#' @family splfmt_rts_data
#' @export
set_splfmt_rts_data_v1 <- function(x, create_unified_columns = TRUE, heal = TRUE) {
  if (!is.data.table(x)) {
    stop("x must be data.table. Run setDT('x').")
  }

  fmt <- formats$splfmt_rts_data_v1$unified
  setattr(x, "format_unified", fmt)
  setattr(x, "class", unique(c("splfmt_rts_data_v1", class(x))))

  if (create_unified_columns) {
    create_unified_columns.splfmt_rts_data_v1(x)
  }

  if (heal) {
    heal.splfmt_rts_data_v1(x)
  }

  return(invisible(x))
}

#' @rdname set_splfmt_rts_data_v1
#' @export
splfmt_rts_data_v1 <- function(x, create_unified_columns = TRUE, heal = TRUE) {
  y <- copy(x)
  set_splfmt_rts_data_v1(
    y,
    create_unified_columns,
    heal
  )

  return(y)
}

validate <- function(x) {
  new_hash <- digest::digest(attr(x, ".internal.selfref"), "md5")
  old_hash <- attr(x, "hash")

  # data hasnt changed since last validation
  if (identical(new_hash, old_hash)) {
    # return(invisible())
  }
  status <- attr(x, "status")
  if (is.null(status)) status <- list()

  fmt <- attr(x, "format_unified")
  for (i in names(fmt)) {
    fmt_i <- fmt[[i]]
    status_i <- list()
    status_i$errors <- "\U274C Errors:"

    # variable doesn't exist
    if (!i %in% names(x)) {
      status_i$errors <- paste0(status_i$errors, "\n- Variable doesn't exist")
    } else {
      # check for NAs allowed
      if (fmt_i$NA_allowed == FALSE & sum(is.na(x[[i]])) > 0) {
        status_i$errors <- paste0(status_i$errors, "\n- NA exists (not allowed)")
      }
      # if(!is.null())

      if (status_i$errors == "\U274C Errors:") status_i$errors <- "\U2705 No errors"
    }
    status[[i]] <- status_i
  }

  setattr(x, "status", status)
  setattr(x, "hash", new_hash)
}

#' @method summary splfmt_rts_data_v1
#' @export
summary.splfmt_rts_data_v1 <- function(object, ...) {
  # validate
  validate(object)
  status <- attr(object, "status")

  for (i in names(status)) {
    status_i <- status[[i]]

    cat("\n", crayon::underline(i), "\n", sep = "")
    cat(status_i$errors, "\n", sep = "")
  }

  # details
  for(i in seq_len(ncol(object))){
    var <- names(object)[i]
    details <- ""
    if(
      var %in% c(
        "granularity_time",
        "granularity_geo",
        "country_iso3",
        # "location_code",
        "border",
        "age",
        "sex",

        "isoyear",
        #"isoweek",
        #"isoyearweek",
        "season"
      ) |
      stringr::str_detect(var, "_tag$") |
      stringr::str_detect(var, "_status$")
    ){
      details <- object[, .(n=.N), keyby=.(get(var))] %>%
        remove_class_splfmt_rts_data()
      setnames(details, "get", "val")
      details[is.na(val), val := "<NA>"]

      # manually specify some ordering requirements
      levels <- sort(details$val)
      extra_levels <- c(
        "nation",
        "county",
        "notmainlandcounty",
        "missingcounty",
        "municip",
        "notmainlandmunicip",
        "missingmunicip",
        "wardoslo",
        "wardbergen",
        "wardstavanger",
        "wardtrondheim",
        "extrawardoslo",
        "missingwardbergen",
        "missingwardoslo",
        "missingwardstavanger",
        "missingwardtrondheim",
        "baregion",
        "region",
        "faregion"
      )
      reordered_levels <- unique(c(extra_levels, levels))
      reordered_levels <- reordered_levels[reordered_levels %in% levels]
      details[, val := factor(val, levels = reordered_levels)]
      setorder(details, val)

      # create display (n + padding)
      details[, len := stringr::str_length(val)]
      details[, max_len := max(len)]
      details[, val := stringr::str_pad(val, max_len, side = "right")]

      details[, n := splstyle::format_nor(n)]
      details[, len := stringr::str_length(n)]
      details[, max_len := max(len)]
      details[, n := stringr::str_pad(n, max_len, side = "left")]

      details[, display := paste0(val, " (n = ",n ,")")]
      details <- details$display

      for(j in seq_along(details)) details[j] <- paste0("\n\t- ",paste0(details[j], collapse = ""))
      details <- paste0(details, collapse = "")
      details <- paste0(":", details)
    }
    cat(names(object)[i], " (", class(object[[i]]),")", details, "\n", sep = "")
  }
  cat("\n")
}

#' Hash the data structure of a dataset for a given column
#'
#' @description
#' Reduces the data structure of a column inside a dataset into something that describes
#'
#' @param x An object
#' @param col Column name to hash
#' @param ... Arguments passed to or from other methods
#' @examples
#' spltidy::generate_test_data() %>%
#'   spltidy::set_splfmt_rts_data_v1() %>%
#'   spltidy::identify_data_structure("deaths_n") %>%
#'   plot()
#' @family splfmt_rts_data
#' @export
identify_data_structure <- function(x, col, ...) {
  UseMethod("identify_data_structure", x)
}

identify_data_structure_internal <- function(summarized, col) {
  # we expect a data.table with columns:
  # - granularity_time
  # - granularity_geo
  # - age
  # - sex
  # - num_valid
  # - num_na

  skeleton <- CJ(
    granularity_time = c("isoyear", "isoweek", "date"),
    granularity_geo = unique(spldata::norway_locations_names()$granularity_geo),
    age = unique(summarized$age),
    sex = unique(summarized$sex)
  )
  skeleton[
    summarized,
    on = c("granularity_time", "granularity_geo", "age", "sex"),
    c("num_valid", "num_na") := .(num_valid, num_na)
  ]
  skeleton[is.na(num_valid), num_valid := 0]
  skeleton[is.na(num_na), num_na := 0]

  skeleton[, category := dplyr::case_when(
    num_valid == 0 & num_na == 0 ~ "structurally_missing",
    num_valid == 0 & num_na > 0 ~ "only_na",
    num_valid > 0 & num_na == 0 ~ "only_data",
    num_valid > 0 & num_na > 0 ~ "data_and_na",
  )]
  skeleton[is.na(age), age := "missing"]
  skeleton[is.na(sex), sex := "missing"]

  skeleton[, num_valid := NULL]
  skeleton[, num_na := NULL]
  skeleton[, granularity_geo := factor(granularity_geo, levels = unique(spldata::norway_locations_names()$granularity_geo))]

  # check if can merge together age groups
  skeleton_wide <- dcast.data.table(
    skeleton,
    granularity_time + granularity_geo + sex ~ age,
    value.var = "category"
  )

  equality <- diag(ncol(skeleton_wide)-3)
  colnames(equality) <- names(skeleton_wide)[4:ncol(skeleton_wide)]
  rownames(equality) <- names(skeleton_wide)[4:ncol(skeleton_wide)]
  for (i in 4:ncol(skeleton_wide)) {
    for (j in 4:ncol(skeleton_wide)) {
      if (sum(skeleton_wide[[i]] != skeleton_wide[[j]]) == 0) equality[i - 3, j - 3] <- 1
    }
  }
  while (nrow(equality) > 0) {
    if (sum(equality[1, ]) > 1) {
      names_to_sum <- colnames(equality)[equality[1, ] == 1]
      end_name <- paste0(names_to_sum, collapse = ",")
      skeleton_wide[, (end_name) := get(names_to_sum[1])]
      for (i in names_to_sum) {
        # delete the data in the skeleton
        skeleton_wide[, (i) := NULL]
        # delete the data in the equality matrix
        equality <- equality[-which(rownames(equality) == i), , drop = FALSE]
      }
    } else {
      equality <- equality[-1, , drop = FALSE]
    }
  }

  skeleton <- melt.data.table(
    skeleton_wide,
    id.vars = c("granularity_time", "granularity_geo", "sex"),
    variable.factor = FALSE,
    variable.name = "age",
    value.name = "category"
  )

  skeleton_wide <- dcast.data.table(
    skeleton,
    granularity_time + age + sex ~ granularity_geo,
    value.var = "category"
  )

  # delete columns that are just structurally_missing and furthest to the right
  for (i in rev(names(skeleton_wide))) {
    if (i == "municip") {
      break()
    } else if (sum(skeleton_wide[[i]] != "structurally_missing", na.rm = T) == 0) {
      skeleton_wide[, (i) := NULL]
    } else {
      break()
    }
  }

  skeleton_long <- melt.data.table(
    skeleton_wide,
    id.vars = c("granularity_time", "age", "sex"),
    variable.factor = FALSE,
    variable.name = "granularity_geo",
    value.name = "category"
  )

  setattr(skeleton_long, "class", unique(c("splfmt_rts_data_structure_hash_v1", class(skeleton_long))))

  return(invisible(skeleton_long))
}

#' @method identify_data_structure splfmt_rts_data_v1
#' @export
identify_data_structure.splfmt_rts_data_v1 <- function(x, col, ...) {
  # col <-
  # Take in the data table
  # data <- data$cases
  # data <- data$vax

  summarized <- x[, .(
    num_valid = sum(!is.na(get(col))),
    num_na = sum(is.na(get(col)))
  ),
  keyby = .(
    granularity_time,
    granularity_geo,
    age,
    sex
  )
  ]

  identify_data_structure_internal(
    summarized,
    var
  )
}

#' @export
"identify_data_structure.tbl_Microsoft SQL Server" <- function(x, col, ...) {
  # col <-
  # Take in the data table
  # data <- data$cases
  # data <- data$vax

  summarized <- x %>%
    dplyr::rename(col = !!col) %>%
    dplyr::group_by(
      granularity_time,
      granularity_geo,
      age,
      sex
    ) %>%
    dplyr::summarize(
      num_total = n(),
      num_na = sum(as.numeric(is.na(col)))
    ) %>%
    dplyr::mutate(
      num_valid = num_total - num_na
    ) %>%
    dplyr::select(-num_total) %>%
    dplyr::collect() %>%
    as.data.table()

  identify_data_structure_internal(
    summarized,
    col
  )
}

#' @method identify_data_structure tbl_Pool
#' @export
"identify_data_structure.tbl_Pool" <- function(x, col, ...) {
  # col <-
  # Take in the data table
  # data <- data$cases
  # data <- data$vax

  summarized <- x %>%
    dplyr::rename(col = !!col) %>%
    dplyr::group_by(
      granularity_time,
      granularity_geo,
      age,
      sex
    ) %>%
    dplyr::summarize(
      num_total = n(),
      num_na = sum(as.numeric(is.na(col)))
    ) %>%
    dplyr::mutate(
      num_valid = num_total - num_na
    ) %>%
    dplyr::select(-num_total) %>%
    dplyr::collect() %>%
    as.data.table()

  identify_data_structure_internal(
    summarized,
    col
  )
}

#' @method plot splfmt_rts_data_structure_hash_v1
#' @export
plot.splfmt_rts_data_structure_hash_v1 <- function(x, y, ...) {
  # x <- generate_test_data() %>%
  #   set_splfmt_rts_data_v1() %>%
  #   identify_data_structure("deaths_n")

  pd <- copy(x)
  pd[, granularity_geo := factor(granularity_geo, levels = unique(spldata::norway_locations_names()$granularity_geo))]
  pd[, category := factor(category, levels = c("structurally_missing", "only_na", "data_and_na", "only_data"))]

  pd[, age := paste0("age=", age)]
  pd[, sex := paste0("sex=", sex)]

  q <- ggplot(pd, aes(x = granularity_geo, y = age, fill = category))
  q <- q + geom_tile(color = "black")
  q <- q + facet_grid(sex ~ granularity_time)
  # q <- q + facet_wrap(~granularity_time, scales = "free")
  q <- q + splstyle::scale_fill_fhi(NULL, drop = F, direction = -1)
  q <- q + scale_x_discrete(NULL)
  q <- q + scale_y_discrete(NULL)
  q <- q + splstyle::set_x_axis_vertical()
  q <- q + theme(legend.position = "bottom", legend.direction = "horizontal")
  q
}



#' Unique time series
#'
#' @description
#' Attempts to identify the unique time series that exist in this dataset.
#'
#' A time series is defined as a unique combination of:
#' - granularity_time
#' - granularity_geo
#' - country_iso3
#' - location_code
#' - border
#' - age
#' - sex
#' - *_id
#' - *_tag
#'
#' @param x An object of type \code{\link{splfmt_rts_data_v1}}
#' @param set_time_series_id If TRUE, then `x` will have a new column called 'time_series_id'
#' @param ... Not used.
#' @family splfmt_rts_data
#' @export
unique_time_series <- function(x, set_time_series_id = FALSE, ...) {
  UseMethod("unique_time_series", x)
}

#' @method unique_time_series splfmt_rts_data_v1
#' @export
unique_time_series.splfmt_rts_data_v1 <- function(x, set_time_series_id = FALSE, ...) {
  ids <- unique(
    c(
      "granularity_time",
      "granularity_geo",
      "country_iso3",
      "location_code",
      "border",
      "age",
      "sex",
      stringr::str_subset(names(x), c("_id$")),
      stringr::str_subset(names(x), c("_tag$"))
    )
  )
  ids <- ids[ids %in% names(x)]
  retval <- x[, ids, with=F] %>%
    unique() %>%
    remove_class_splfmt_rts_data()

  # return the old stuff
  if("time_series_id" %in% names(retval)){
    return(retval)
  }

  retval[, time_series_id := 1:.N]

  if(set_time_series_id){
    x[retval, on = ids, time_series_id := time_series_id]
  }

  # allows us to print
  data.table::shouldPrint(x)

  return(retval)
}

#' Expand time to
#'
#' @description
#' Attempts to expand the dataset to include more time
#'
#' A time series is defined as a unique combination of:
#' - granularity_time
#' - granularity_geo
#' - country_iso3
#' - location_code
#' - border
#' - age
#' - sex
#' - *_id
#' - *_tag
#'
#' @param x An object of type \code{\link{splfmt_rts_data_v1}}
#' @param max_isoyear Maximum isoyear
#' @param max_isoyearweek Maximum isoyearweek
#' @param max_date Maximum date
#' @param ... Not used.
#' @family splfmt_rts_data
#' @export
expand_time_to <- function(x, max_isoyear = NULL, max_isoyearweek = NULL, max_date = NULL, ...) {
  UseMethod("expand_time_to", x)
}

#' @method expand_time_to splfmt_rts_data_v1
#' @export
expand_time_to.splfmt_rts_data_v1 <- function(x, max_isoyear = NULL, max_isoyearweek = NULL, max_date = NULL, ...) {
  if(is.null(max_isoyear) & is.null(max_isoyearweek) & is.null(max_date)){
    stop("At least one of max_isoyear, max_isoyearweek, max_date must be used")
  }
  d1 <- d2 <- d3 <- NULL
  if(!is.null(max_isoyear)){
    d1 <- expand_time_to_max_isoyear.splfmt_rts_data_v1(x, max_isoyear = max_isoyear)
  }
  if(!is.null(max_isoyearweek)){
    d2 <- expand_time_to_max_isoyearweek.splfmt_rts_data_v1(x, max_isoyearweek = max_isoyearweek)
  }
  if(!is.null(max_date)){
    d3 <- expand_time_to_max_date.splfmt_rts_data_v1(x, max_date = max_date)
  }
  retval <- rbindlist(list(d1,d2,d3), fill = T)

  # allows us to print
  data.table::shouldPrint(retval)

  return(retval)
}

expand_time_to_max_isoyear <- function(x, max_isoyear = NULL, ...) {
  UseMethod("expand_time_to_max_isoyear", x)
}

expand_time_to_max_isoyear.splfmt_rts_data_v1 <- function(x, max_isoyear = NULL, ...) {
  d <- copy(x[granularity_time=="isoyear"])
  if(nrow(d) == 0) return(d)

  if(!"time_series_id" %in% names(d)){
    on.exit(d[, time_series_id := NULL])
    flag_to_remove_time_series_id <- TRUE
  } else {
    flag_to_remove_time_series_id <- FALSE
  }
  ids <- unique_time_series(d, set_time_series_id = TRUE)

  max_vals <- d[, .(max_isoyear = max(isoyear, na.rm=T)), by=.(time_series_id)]
  ids[max_vals, on="time_series_id", max_current_isoyear := max_isoyear]
  ids[, max_isoyear := max_isoyear]

  retval <- vector("list", length = nrow(ids))
  for(i in seq_along(retval)){
    if(ids$max_current_isoyear[i] >= ids$max_isoyear[i]){
      break()
    }
    new_isoyears <- c((ids$max_current_isoyear[i]+1):ids$max_isoyear[i])
    retval[[i]] <- copy(ids[rep(i,length(new_isoyears))])
    retval[[i]][, isoyear := new_isoyears]
  }

  retval <- rbindlist(retval)

  x <- rbindlist(list(d, retval), fill = T)
  spltidy::set_splfmt_rts_data_v1(x)
  setorder(x, time_series_id, date)

  if(flag_to_remove_time_series_id) x[, time_series_id := NULL]
  if("max_current_isoyear" %in% names(x)) x[, max_current_isoyear := NULL]
  if("max_isoyear" %in% names(x)) x[, max_isoyear := NULL]

  # allows us to print
  data.table::shouldPrint(x)

  return(x)
}

expand_time_to_max_isoyearweek <- function(x, max_isoyearweek = NULL, ...) {
  UseMethod("expand_time_to_max_isoyearweek", x)
}

expand_time_to_max_isoyearweek.splfmt_rts_data_v1 <- function(x, max_isoyearweek = NULL, ...) {
  d <- copy(x[granularity_time=="isoweek"])
  if(nrow(d) == 0) return(NULL)

  if(!"time_series_id" %in% names(d)){
    on.exit(d[, time_series_id := NULL])
    flag_to_remove_time_series_id <- TRUE
  } else {
    flag_to_remove_time_series_id <- FALSE
  }
  ids <- unique_time_series(d, set_time_series_id = TRUE)

  max_vals <- d[, .(max_isoyearweek = max(isoyearweek, na.rm=T)), by=.(time_series_id)]
  ids[max_vals, on="time_series_id", max_current_isoyearweek := max_isoyearweek]
  ids[, max_isoyearweek := max_isoyearweek]

  retval <- vector("list", length = nrow(ids))
  for(i in seq_along(retval)){
    if(ids$max_current_isoyearweek[i] >= ids$max_isoyearweek[i]){
      break()
    }
    index_min <- which(spltime::dates_by_isoyearweek$isoyearweek == ids$max_current_isoyearweek[i])+1
    index_max <- which(spltime::dates_by_isoyearweek$isoyearweek == ids$max_isoyearweek[i])
    new_isoyearweeks <- spltime::dates_by_isoyearweek$isoyearweek[index_min:index_max]
    retval[[i]] <- copy(ids[rep(i,length(new_isoyearweeks))])
    retval[[i]][, isoyearweek := new_isoyearweeks]
  }

  retval <- rbindlist(retval)

  x <- rbindlist(list(d, retval), fill = T)
  spltidy::set_splfmt_rts_data_v1(x)
  setorder(x, time_series_id, date)

  if(flag_to_remove_time_series_id) x[, time_series_id := NULL]
  if("max_current_isoyearweek" %in% names(x)) x[, max_current_isoyearweek := NULL]
  if("max_isoyearweek" %in% names(x)) x[, max_isoyearweek := NULL]

  # allows us to print
  data.table::shouldPrint(x)

  return(x)
}

expand_time_to_max_date <- function(x, max_date = NULL, ...) {
  UseMethod("expand_time_to_max_date", x)
}

expand_time_to_max_date.splfmt_rts_data_v1 <- function(x, max_date = NULL, ...) {
  d <- copy(x[granularity_time=="day"])
  if(nrow(d) == 0) return(NULL)

  if(!"time_series_id" %in% names(d)){
    on.exit(d[, time_series_id := NULL])
    flag_to_remove_time_series_id <- TRUE
  } else {
    flag_to_remove_time_series_id <- FALSE
  }
  ids <- unique_time_series(d, set_time_series_id = TRUE)

  max_vals <- d[, .(max_date = max(date, na.rm=T)), by=.(time_series_id)]
  ids[max_vals, on="time_series_id", max_current_date := max_date]
  ids[, max_date := max_date]

  retval <- vector("list", length = nrow(ids))
  for(i in seq_along(retval)){
    if(ids$max_current_date[i] >= ids$max_date[i]){
      break()
    }
    new_dates <- seq.Date(as.Date(ids$max_current_date[i])+1, as.Date(ids$max_date[i]), by = 1)
    retval[[i]] <- copy(ids[rep(i,length(new_dates))])
    retval[[i]][, date := new_dates]
  }

  retval <- rbindlist(retval)

  x <- rbindlist(list(d, retval), fill = T)
  spltidy::set_splfmt_rts_data_v1(x)
  setorder(x, time_series_id, date)

  if(flag_to_remove_time_series_id) x[, time_series_id := NULL]
  if("max_current_date" %in% names(x)) x[, max_current_date := NULL]
  if("max_date" %in% names(x)) x[, max_date := NULL]

  # allows us to print
  data.table::shouldPrint(x)

  return(x)
}










