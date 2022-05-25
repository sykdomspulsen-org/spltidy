# spltidy 2022.5.25

- Dataset covid19_msis_cases_by_time_location renamed to norway_covid19_cases_by_time_location.
- In splfmt_rts_v1, the granularity_time for "an ongoing event" was changed from event_\*_9999_01_01 to event_\*_9999_09_09. This was done because isoyear for 9999-01-01 is 9998 (which is confusing), while isoyear for 9999-09-09 is 9999 (which makes sense).
- In splfmt_rts_v1, the missing value for sex and age was changed to "missing" instead of NA_character_. This was chosen because NA_character_ requires special manipulation functions (is.na) which makes post-processing of data less efficient for the end-user.
- In splfmt_rts_v1, spltidy::heal now works when granularity_time=='event_*'

# spltidy 2022.5.19

- Dataset covid19_msis_cases_by_time_location included, containing number of Covid19 cases from MSIS registry. The locations are for both national and county level. The percentage per 100.000 population is included. The time period is between 2020-02-21 and 2022-05-03 (data extracted on 2022-05-04).

# spltidy 2022.5.5

- Dataset norway_covid19_icu_and_hospitalization included, containing admissions to the ICU with a positive PCR test and number of new hospitalizations with Covid-19 as the primary cause between 2020-02-21 and 2022-05-03 (data extracted 2022-05-04).

# spltidy 2022.4.26

- save_spl, read_spl functions to save/read data efficiently, allowing passwordless encryption.

# spltidy 2022.4.22

- print.splfmt_rts_data_v1 now automatically rounds numerics to 4 decimal places

# spltidy 2022.4.7

- unique_time_series function added.
