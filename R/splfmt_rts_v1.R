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

#' remove class splfmt_rts_data_*
#' @param x data.table
#' @export
remove_class_splfmt_rts_data <- function(x) {
  classes <- class(x)
  classes <- classes[!stringr::str_detect(classes, "^splfmt_rts_data_")]
  setattr(x, "class", classes)
  return(invisible(x))
}


#' Test data generator
#' @param fmt Format (splfmt_rts_data_v1)
#' @examples
#' test_data_generator("splfmt_rts_data_v1")
#' @export
test_data_generator <- function(fmt = "splfmt_rts_data_v1") {
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

#' print
#' @param x x
#' @param ... dots
#' @method print splfmt_rts_data_v1
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
  variable_classes <- paste0("<", unlist(lapply(to_print, class)), ">")
  variable_names <- names(to_print)
  row_numbers <- formatC(row_numbers, width = max(nchar(row_numbers))) %>%
    paste0(":", sep = "")
  row_number_spacing <- formatC("", width = max(nchar(row_numbers)))

  width_char <- apply(to_print, 2, nchar, keepNA = F) %>%
    rbind(nchar(variable_types)) %>%
    rbind(nchar(variable_classes)) %>%
    rbind(nchar(variable_names)) %>%
    apply(2, max)

  max_width <- getOption("width")
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
    for (i in -2:nrow(to_print)) {
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

        if (i == -2) {
          cat(formatC(variable_types[j], width = width_char[j]))
        } else if (i == -1) {
          cat(formatC(variable_classes[j], width = width_char[j]))
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

#' assignment
#' @param x x
#' @param ... dots
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
    on.exit(set_splfmt_rts_data_v1(x, create_unified_variables = FALSE, heal = FALSE))

    y <- eval(parse(text = deparse(modified_call)), envir = parent.frame(1:2))
    set_splfmt_rts_data_v1(y, create_unified_variables = FALSE, heal = FALSE)
    return(invisible(y))
  } else if (length(i) == 1) {
    # smart-assignment for time ----
    # identify if a time variable is mentioned
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
    # identify if a geo variable is mentioned
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
    on.exit(set_splfmt_rts_data_v1(x, create_unified_variables = FALSE, heal = FALSE))

    eval(parse(text = deparse(modified_call)), envir = parent.frame(1:2))
    for (i in seq_along(healing_calls)) {
      eval(parse(text = healing_calls[[i]]), envir = parent.frame(1:2))
    }

    return(invisible(x))
  }
}



#' heal generic
#'
#' @param x An object
#' @param ... Arguments passed to or from other methods
#' @export
heal <- function(x, ...) {
  UseMethod("heal", x)
}

#' Heal
#' @param x x
#' @param ... Arguments passed to or from other methods
#' @method heal splfmt_rts_data_v1
#' @export
heal.splfmt_rts_data_v1 <- function(x, ...) {
  assert_classes.splfmt_rts_data_v1(x)

  # granularity_time = mandatory
  imputing_vars <- list(
    "location_code" = c("granularity_geo", "country_iso3"),
    "isoyear" = c(
      "granularity_time",
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
      "granularity_time",
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
      "granularity_time",
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

  for (i in seq_along(imputing_vars)) {
    imputed_from <- names(imputing_vars)[i]
    to_be_imputed <- imputing_vars[[i]]

    to_be_imputed <- to_be_imputed[to_be_imputed %in% names(x)]
    if (imputed_from %in% names(x) & length(to_be_imputed) > 0) {
      txt <- glue::glue(
        '
        x[!is.na({imputed_from}) & (is.na({paste0(to_be_imputed, collapse=")|is.na(")})), {imputed_from} := {imputed_from}]
        '
      )
      eval(parse(text = txt))
    }
  }

  # allows us to print
  data.table::shouldPrint(x)

  return(invisible(x))
}

#' create_unified_columns generic
#'
#' @param x An object
#' @param ... Arguments passed to or from other methods
#' @export
create_unified_columns <- function(x, ...) {
  UseMethod("create_unified_columns", x)
}

#' create_unified_columns
#' @param x x
#' @param ... Arguments passed to or from other methods
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

#' Set as splfmt_rts_data_v1
#' @param x The data.table to be converted to splfmt_rts_data_v1
#' @param create_unified_variables Do you want it to create all unified variables?
#' @param heal Do you want to heal on creation?
#' @examples
#' d <- test_data_generator()
#' set_splfmt_rts_data_v1(d, create_unified_variables = TRUE)
#' d[1, isoyearweek := "2021-01"]
#' d
#' d[2, isoyear := 2019]
#' d
#' d[4:5, date := as.Date("2020-01-01")]
#' d
#' d[10, c("isoyear", "isoyearweek") := .(2021, "2021-01")]
#' d
#' d[10, c("location_code") := .("norge")]
#' d
#' @export
set_splfmt_rts_data_v1 <- function(x, create_unified_variables = TRUE, heal = TRUE) {
  if (!is.data.table(x)) {
    stop("x must be data.table. Run setDT('x').")
  }

  fmt <- formats$splfmt_rts_data_v1$unified
  setattr(x, "format_unified", fmt)
  setattr(x, "class", unique(c("splfmt_rts_data_v1", class(x))))

  if (create_unified_variables) {
    create_unified_columns.splfmt_rts_data_v1(x)
  }

  if (heal) {
    heal.splfmt_rts_data_v1(x)
  }

  return(invisible(x))
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

summary.splfmt_rts_data_v1 <- function(object, ...) {
  validate(object)
  status <- attr(object, "status")

  for (i in names(status)) {
    status_i <- status[[i]]

    cat("\n", crayon::underline(i), "\n", sep = "")
    cat(status_i$errors, "\n", sep = "")
  }
}

#' heal generic
#'
#' @param x An object
#' @param var variable to hash
#' @param ... Arguments passed to or from other methods
#' @export
hash_data_structure <- function(x, var, ...) {
  UseMethod("hash_data_structure", x)
}

hash_data_structure_internal <- function(summarized, var) {
  # we expect a data.table with columns:
  # - granularity_time
  # - granularity_geo
  # - age
  # - sex
  # - num_valid
  # - num_na

  skeleton <- CJ(
    granularity_time = c("isoyear", "isoweek", "day"),
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

#' Hash
#' @param x x
#' @param var variable to hash
#' @param ... Arguments passed to or from other methods
#' @method hash_data_structure splfmt_rts_data_v1
#' @export
hash_data_structure.splfmt_rts_data_v1 <- function(x, var, ...) {
  # var <-
  # Take in the data table
  # data <- data$cases
  # data <- data$vax

  summarized <- x[, .(
    num_valid = sum(!is.na(get(var))),
    num_na = sum(is.na(get(var)))
  ),
  keyby = .(
    granularity_time,
    granularity_geo,
    age,
    sex
  )
  ]

  hash_data_structure_internal(
    summarized,
    var
  )
}

#' Hash
#' @param x x
#' @param var variable to hash
#' @param ... Arguments passed to or from other methods
#' @export
"hash_data_structure.tbl_Microsoft SQL Server" <- function(x, var, ...) {
  # var <-
  # Take in the data table
  # data <- data$cases
  # data <- data$vax

  summarized <- x %>%
    dplyr::rename(var = !!var) %>%
    dplyr::group_by(
      granularity_time,
      granularity_geo,
      age,
      sex
    ) %>%
    dplyr::summarize(
      num_total = n(),
      num_na = sum(as.numeric(is.na(var)))
    ) %>%
    dplyr::mutate(
      num_valid = num_total - num_na
    ) %>%
    dplyr::select(-num_total) %>%
    dplyr::collect() %>%
    as.data.table()

  hash_data_structure_internal(
    summarized,
    var
  )
}

#' Hash
#' @param x x
#' @param var variable to hash
#' @param ... Arguments passed to or from other methods
#' @method hash_data_structure Schema_v8
#' @export
hash_data_structure.Schema_v8 <- function(x, var, ...) {
  hash_data_structure(x$tbl(), var)
}

#' print
#' @param x x
#' @param y x
#' @param ... dots
#' @examples
#' x <- spltidy::test_data_generator() %>%
#'   spltidy::set_splfmt_rts_data_v1() %>%
#'   spltidy::hash_data_structure("deaths_n") %>%
#'   plot
#' @method plot splfmt_rts_data_structure_hash_v1
#' @export
plot.splfmt_rts_data_structure_hash_v1 <- function(x, y, ...) {
  # x <- test_data_generator() %>%
  #   set_splfmt_rts_data_v1() %>%
  #   hash_data_structure("deaths_n")

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
