## code to prepare `DATASET` dataset goes here
library(data.table)
library(magrittr)
d <- data.table::fread("data-raw/covid19_hospital.csv")
d[, date:=as.Date(date)]
d[, date_of_publishing:=NULL]
d[, year:=NULL]
d[, week:=NULL]
d[, yrwk:=NULL]
d[, x:=NULL]
spltidy::set_splfmt_rts_data_v1(d)
setnames(
  d,
  c(
    "n_icu",
    "n_hospital_main_cause"
  ),
  c(
    "icu_with_positive_pcr_n",
    "hospitalization_with_covid19_as_primary_cause_n"
  )
)

week <- d[,.(
  icu_with_positive_pcr_n = sum(icu_with_positive_pcr_n),
  hospitalization_with_covid19_as_primary_cause_n = sum(hospitalization_with_covid19_as_primary_cause_n)
  ),
  keyby=.(
    location_code,
    border,
    age,
    sex,
    isoyearweek
  )] %>%
  spltidy::create_unified_columns()

norway_covid19_icu_and_hospitalization <- rbind(d, week)
spltidy::set_splfmt_rts_data_v1(norway_covid19_icu_and_hospitalization)

usethis::use_data(norway_covid19_icu_and_hospitalization, overwrite = TRUE)
