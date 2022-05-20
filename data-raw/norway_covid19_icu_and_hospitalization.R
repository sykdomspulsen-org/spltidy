## code to prepare `DATASET` dataset goes here
library(data.table)
library(magrittr)

# import data
# daily, nation
d <- data.table::fread("data-raw/covid19_hospital.csv")
d[, date:=as.Date(date)]
d[, date_of_publishing:=NULL]
d[, year:=NULL]
d[, week:=NULL]
d[, yrwk:=NULL]
d[, x:=NULL]

# set to splfmt
spltidy::set_splfmt_rts_data_v1(d)

# change variable names
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

d


# weekly aggregation
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

week


# put daily and weekly together
norway_covid19_icu_and_hospitalization <- rbind(d, week)

# set to splfmt
spltidy::set_splfmt_rts_data_v1(norway_covid19_icu_and_hospitalization)

# save the data into data folder in .rda format
usethis::use_data(norway_covid19_icu_and_hospitalization, overwrite = TRUE)


# ?spltidy::norway_covid19_icu_and_hospitalization
