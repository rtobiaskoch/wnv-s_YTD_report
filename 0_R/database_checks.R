data_input = read.csc(fn_database_input)

#find traps that have different dates for the same week
wrong_trap_dates = data_input %>%
  distinct(year,trap_date, week, zone, trap_id, method) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools and spp
  group_by(year, zone, week, method, trap_id) %>% #get number of traps per week per zone
  summarise(n = n())  %>%
  filter(n >1)

if(nrow(wrong_trap_dates) > 0) {
  warning("Warning: your database has traps from the same week with different dates.")
}