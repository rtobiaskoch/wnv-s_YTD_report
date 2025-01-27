rm(list = ls())
list2env(readRDS("1_input/config_params.RDS"),          
         envir = .GlobalEnv)



#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------P T   P L O T   B Y   T R A P---------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
database_input = read.csv(fn_database_input)


#df should be the database pool level data
fun_p_trap = function(df, yr_min, zone_filter, y_limit){
df %>%
    filter(year >= yr_min) %>%
    filter(zone %in% zone_filter) %>%
    group_by(year, week, zone, trap_id, spp) %>%
    reframe(total = sum(total),
            positive_pools = sum(test_code, na.rm = T),
            n_pools = n()) %>%
    mutate(total = if_else(spp == "Pipiens", -total, total)) %>%
    mutate(pos = factor(if_else(positive_pools > 0, 1, 0), levels = c(1,0))) %>%
ggplot(aes(x = week, y = total, size =  pos, shape = spp, color = pos)) +
    geom_hline(yintercept = 0, color = "grey80", alpha = 0.8) +
    geom_point(alpha = 0.7) +
    scale_size_manual(values = c("1" = 1.8, "0" = 1.1)) +
    scale_color_manual(values = c("1" = "red", "0" = "blue")) +
    #scale_color_gradient(low = "blue", high = "red") + #for positive_pools
    scale_y_continuous( limits = c(-y_limit, y_limit), labels = function(x) abs(x)) + 
    scale_x_continuous(breaks = seq(min(df$week), max(df$week), by = 3)) +
    ylab("Mosquitoes per Trap") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank()) +
    facet_grid(zone~year)
}

p_trap =  fun_p_trap(df = database_input, 2019, fc_zones, y_limit = 500)

p_trap


ggsave(plot = p_trap, filename = file.path(dir_output, "p_trap.png"), 
       width = 11, height = 8, units = "in")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-----------------  S A V E   Y E A R L Y   S T A T S -----------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
data_input0 =  read.csv(fn_data_output) #the data zone wk data

data_input = data_input0  %>%
  mutate(type = if_else(year == year_filter, "current", "hx")) %>%
  mutate(pos = factor(if_else(n_pos_pools > 0, 1, 0), levels = c(1,0))) #create binary yes no if any pools were positive

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------------------S A V E   Y E A R L Y--------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#table for report
data_yr_stat = data_input %>%
  filter(year > 2014) %>%
  filter(zone %in% fc_zones) %>%
  filter(spp == "All") %>%
  group_by(year) %>%
  summarise(total_pools_tested = sum(n_pools),
            total_pos_pools = sum(n_pos_pools),
            total_mosq_tested = sum(mosq),
            avg_abund = round(mean(abund),2),
            avg_pir = round(mean(pir),4),
            avg_vi = round(mean(vi),2),
            max_vi = round(max(vi), 2),
            .groups = "drop"
  ) %>%
  mutate(pos_pool_pct = round(total_pos_pools/total_pools_tested,2)) %>%
  select(year, total_mosq_tested, total_pools_tested, total_pos_pools, pos_pool_pct, 
         total_pools_tested, avg_abund, avg_pir, avg_vi, max_vi)


#calc abundance without Boulder bc of trap biases
abund_no_bc = data_input %>%
  filter(year > 2014) %>%
  filter(zone != "BC") %>%
  group_by(year) %>%
  summarise(avg_abund = round(mean(abund),2))


data_yr_stat = data_yr_stat %>%
  mutate(avg_abund = abund_no_bc$avg_abund)

data_yr_stat_no_current = data_yr_stat %>%
  filter(year != year_filter) %>%
  mutate(year = "15-23") %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), ~ round(mean(.x),4)))

data_yr_stat2 = data_yr_stat %>%
  rbind(data_yr_stat_no_current)

write.csv(data_yr_stat2, file.path(dir_output, "data_yr_stat.csv"), row.names = F)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------- P L O T T I N G   T I L E    H E A T M A P -----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#df is the data_zone_wk data
fun_heat_map = function(df, value) {
  
  df %>% #should be zone level data
  ggplot(aes(x = week, y = zone, fill = {{value}})) +
  geom_tile(color = "gray20") + # Adds white grid lines
  scale_fill_gradient(low = "white", high = "red", na.value = "gray") +
  facet_wrap(~ year, ncol = 2) + # Facets for each "Variable" like Vi
  scale_x_continuous(breaks = seq(min(data_input0$week), max(data_input0$week), by = 1)) +
  theme_classic() +
  theme(
    panel.grid = element_blank(),   # Remove grid lines
    strip.background = element_blank(), # Remove facet strip background
    strip.text = element_text(size = 10) # Customize facet labels
  )
}

p_vi_heat =   data_input0 %>% 
  filter(year > 2014) %>%
  filter(zone %in% fc_zones) %>%
  fun_heat_map(vi)

p_vi_heat 
 
ggsave(plot = p_vi_heat, filename = "3_output/p_vi_heat.png",
        width = 10, height = 7, units = "in")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-----------------------Z O N E   R E P O R T   D A T A ----------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  data_input_long = data_input %>%
    select(all_of(grp_vars), n_pools, n_pos_pools, mosq_L, abund, pir, pir_lci, pir_uci, vi, vi_lci, vi_uci) %>%
    pivot_longer(cols = -c(grp_vars),
                 names_to = "est",
                 values_to = "value") %>% 
    mutate(type = if_else(year == year_filter, "current", "hx"))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------------- P L O T : S P P ( A L L ) -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

df_all_long = data_input_long %>%
    filter(zone %in% fc_zones) %>%
    mutate(type = factor(type, levels = c("hx", "current")),
          zone = factor(zone, levels = zone_lvls)) %>%
   pivot_wider(names_from = est, values_from = value) %>%
   group_by(zone, week, spp, type) %>%
   summarise(abund = mean(abund), 
            pir = mean(pir), 
            vi = mean(vi)) %>%
  filter(spp == "All")



p_df_all_fun = function(df, value, text) {
  
  ggplot(df, aes(x = week, y = {{value}}, 
             color = type, fill = type, group = type)) +
  geom_hline(yintercept = 0) +
  geom_area(position = "dodge", alpha = 0.3) +
  facet_grid(zone ~ .) +
  theme_classic() +
  ggtitle(text) +
  scale_x_continuous(breaks = seq(min(df$week), max(df$week), by = 1)) +
  scale_color_manual(values = curr_hx_pal) +
  scale_fill_manual(values = curr_hx_pal)
}   

p_abund = p_df_all_fun(df_all_long, abund, "Abundance")


p_pir = p_df_all_fun(df_all_long, pir, "Pooled Infection Rate")


p_vi = p_df_all_fun(df_all_long, vi, "Vector Index") + 
  geom_hline(yintercept = vi_threshold, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25))


#description was removed which describes the years where samples were

p_hx_current0 = p_abund + p_pir+  p_vi + 
  #plot_annotation(caption = c(description)) +
  plot_layout(#widths =c(3,3,3,1), 
              guides = "collect") & theme(legend.position = 'bottom', 
                                          legend.title = element_blank())
p_hx_current0 

ggsave("3_output/hx_plot_all.png", p_hx_current0, height = 7, width = 10, units = "in")





