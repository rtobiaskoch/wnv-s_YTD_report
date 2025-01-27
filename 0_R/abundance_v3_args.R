rm(list = ls())

list2env(readRDS("1_input/config_params.RDS"),         
         envir = .GlobalEnv)

#ABUNDANCE_V3_ARGS
#code copied from abundance_v2.R on 24-01-08 and modified to calculate for all years and all weeks
#incorporating changes to database to include 0 traps so that abundance can be calculated independently of 
#the routine trap list.
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#check and read in data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

args = commandArgs(trailingOnly = T)

fn_1_input = as.character(args[1])
fn_1_input = fn_database_input

#below is modification to the check_read_fun.R without the year and week filters
1_input =  rio::import(fn_1_input) %>% 
  mutate(zone = factor(zone, levels = zone_lvls), 
         week = factor(week),
         spp = factor(spp, levels = c("Pipiens", "Tarsalis", "All"))) %>% 
  arrange(year, zone, week, spp)

#EDIT REMOVED FUNCTIONAL TRAP BC 0 TRAPS ARE NOW IN THE DATABASE AS OF 24-12-26

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>#GET WEEKS TRAP NUMBERS
#>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

suppressMessages({
  trap_p_wk0 = 1_input %>%
  distinct(year,trap_date, week, zone, trap_id, method) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools and spp
  group_by(year, zone, week, method) %>% #get number of traps per week per zone
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = method, values_from = n, 
              names_prefix = "trap_",values_fill = 0) %>%
  mutate(n_trap = trap_L + trap_G)
})


#find traps that have different dates for the same week
wrong_trap_dates = 1_input %>%
  distinct(year,trap_date, week, zone, trap_id, method) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools and spp
  group_by(year, zone, week, method, trap_id) %>% #get number of traps per week per zone
  summarise(n = n())  %>%
  filter(n >1)


  #GET WEEKS TRAP NUMBERS: FC
  #>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  suppressMessages({
    fc_trap_wk0 = trap_p_wk0 %>% 
      filter(zone %in% fc_zones) %>%
      group_by(year,week) %>%
      summarise(
        zone = "FC",
        trap_G = sum(trap_G, na.rm = TRUE),
        trap_L = sum(trap_L, na.rm = TRUE),
        n_trap = sum(n_trap, na.rm = TRUE)
      )
  } )
  
  #>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #>combine data
  #>#>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
  trap_p_wk = rbind(trap_p_wk0, fc_trap_wk0) %>%
    left_join(func_trap_L, by = c("year", "week", "zone")) %>% #merge with the routine functional trap list
    mutate(trap_L_0 = active - trap_L_func - malfunction) %>%
    mutate(func_GT_wk = trap_L_func <= trap_L) %>% #is the routine greater than the weekly? if not there is an error
    mutate(func_GT_wk = if_else(is.na(func_GT_wk), T, func_GT_wk)) %>%
    mutate(trap_L_func = if_else(trap_L_func == 0, trap_L, trap_L_func)) %>% #for WC and BC that don't have active traps
    mutate(zone = factor(zone, levels = zone_lvls)) %>%
    arrange(zone)

#account for boulder with no obvious traps
  BC_trap = trap_p_wk %>%
    filter(zone == "BC") %>%
    mutate(method = if_else(is.na(method), "L", method),
           active = if_else(is.na(active), trap_L, active),
           malfunction = if_else(is.na(malfunction), 0, malfunction),
           trap_L_func = if_else(is.na(trap_L_func), trap_L, trap_L_func),
           trap_L_0    = if_else(is.na(trap_L_0), active-trap_L, trap_L_0)
           )
  
  trap_p_wk = trap_p_wk %>%
    filter(zone != "BC") %>%
    rbind(BC_trap)
  
  write.csv(trap_p_wk, fn_trap_p_wk, row.names = F)
  
  
#>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>check trap numbers
#>#>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

trap_check_FC_LV =   trap_p_wk %>%
  filter(zone %in% c(fc_zones, "LV"))

#if(any(trap_check_FC_LV$func_GT_wk == F)) {
if(F){
  
  stop("check your trap data. 
        The number of traps in this weeks data is greater than the expected routine traps.")

  } else {
  
  suppressMessages({
  
     
    #ALL ZONES
    #get number of mosquitoes per night per trap
    m_p_wk0 = 1_input %>%
      group_by(across(all_of(c(grp_vars, "method")))) %>% #get number of mosquitoes per week per zone per species
      summarize(mosq = sum(total)) %>%
      ungroup()%>%
      pivot_wider(names_from = method, values_from = mosq, 
                  names_prefix = "mosq_", values_fill = 0) %>%
      mutate(mosq = mosq_L + mosq_G)
    
    
    #FC
    #get number of mosquitoes per night per trap
    fc_m_p_wk = m_p_wk0 %>%
      filter(zone %in% fc_zones) %>%
      group_by(year, week, spp) %>%
      summarise(zone = "FC", 
                across(where(is.numeric), ~sum(.x, na.rm = TRUE)))
    
    m_p_wk = rbind(m_p_wk0, fc_m_p_wk)
    
    #get abundance per trap
   abund_zone_wk_with_G = left_join(trap_p_wk, m_p_wk, by = c("year","zone", "week")) %>%
      mutate(abund = round(mosq_L/trap_L_func,2)) #WHERE CALC HAPPENS
    
    abund_zone_wk = abund_zone_wk_with_G %>%
      select(year, week, zone, spp, mosq, mosq_L, trap_L_func, abund) %>%
      mutate(zone = factor(zone, levels = zone_lvls)) %>%
      arrange(zone)
    
  })
  
  
  write.csv(abund_zone_wk, paste0(fn_abund_out, ".csv"), row.names = F)
  
  
} # end of else




