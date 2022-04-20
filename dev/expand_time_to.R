library(data.table)
library(magrittr)
devtools::load_all()
d <- as.data.table(tidyr::fish_encounters)
d %>%
  pivot_wider(names_from = station, values_from = seen)


max_isoyearweek = "2022-05"
d <- spltidy::generate_test_data() |>
  spltidy::set_splfmt_rts_data_v1()

ids <- unique_time_series(d, set_time_series_id = TRUE)

max_vals <- d[, .(max_isoyearweek = max(isoyearweek, na.rm=T)), by=.(time_series_id)]
ids[max_vals, on="time_series_id", max_current_isoyearweek := max_isoyearweek]
ids[, max_isoyearweek := max_isoyearweek]

retval <- vector("list", length = nrow(ids))
for(i in seq_along(retval)){
  index_min <- which(spltime::dates_by_isoyearweek$isoyearweek == ids$max_current_isoyearweek[i])+1
  index_max <- which(spltime::dates_by_isoyearweek$isoyearweek == ids$max_isoyearweek[i])
  if(index_min >= index_max){
    break()
  }
  new_isoyearweeks <- spltime::dates_by_isoyearweek$isoyearweek[index_min:index_max]
  retval[[i]] <- copy(ids[rep(i,length(new_isoyearweeks))])
  retval[[i]][, isoyearweek := new_isoyearweeks]
}

retval <- rbindlist(retval)

x <- rbindlist(list(d, retval), fill = T)
spltidy::set_splfmt_rts_data_v1(x)
setorder(x, time_series_id, date)





d <- spltidy::generate_test_data() |>
  spltidy::set_splfmt_rts_data_v1()
d <- d[1:3]
d
d[1, granularity_time := "day"]
d
expand_time_to(d, max_isoyearweek = "2022-06", max_date = "2022-02-01")

expand_time_to.splfmt_rts_data_v1(d, max_date = "2021-02-01")
