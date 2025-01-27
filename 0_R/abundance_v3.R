#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------ H E A D E R  ------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#AUTHOR: TOBY KOCH
# DATE OF LAST REVISION: 25-01-20
#DESCRIPTION: 
# this script utilizes the wnv summer surveillance database (wnv-s_database)
# to calculate the abundance of mosquitoes in CDC Lights Traps.
# it excludes gravid traps

#VERSIONS
#V3 
# - contains code to account for 0 traps and malfunction traps being account for in the 
#   database.This simplifies code and only requires the database as an input
# - includes list of used hardcoded parameters that user can hardcode if the config_params.RDS
#   file is unavailable
# - added more messages to inform the user of what is happening within the code
# - doesn't filter by week or year by using check_read_fun.R, processes all data

#V2 honestly I dont remember how this is different from V1


rm(list = ls())

# IF NO CONFIG EXISTS DEFINE THEM YOURSELF PEASANT!
suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(tidyverse)
})

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ---------------------- H A R D C O D E D   P A R A M E T E R S  --------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#these parameters will be used if a config file is supplied. 
#note that the config paramters names and these hardcoded file names must match

#DIRECTORIES
dir_input = "1_input"# <<<<<<<<<<-----------------------------------------------------  USER INPUT
dir_mid = "2_mid"

#CHECK THAT DIRECTORIES EXIST
if(!dir.exists(dir_input)) {
  cat(dir_input, " doesn't exist. Please create it and add database to file") }

if(dir.exists(dir_mid)) {
  cat(dir_mid, " exists and will be used for output files.")
} else {
  dir.create(dir_mid)
  cat(dir_mid, " was created and will be used for output files. ")
}

#CONFIG FILE
config_params_file = file.path(dir_input, "config_params.RDS") # <<<<<<<<<------------  USER INPUT

#FILENAMES
fn_database_input = file.path(dir_input, "wnv-s_database - data.csv") # <<<<<<<--------------- USER INPUT
fn_abund_mid =  file.path(dir_mid, "abundance.csv")

#GROUPING VARIABLES
fc_zones = fc_zones = c("NE", "SE", "NW", "SW")
grp_vars = c("year", "week", "zone", "spp")
grp_dist_trap = c("year", "week", "zone", "trap_id") #grouping variables to determine distinct trap #


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------ C O N F I G ------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#IF THE CONFIG PARAMS FILE EXISTS US THAT TO DEFINE INPUTS 
# ELSE DEFINE BELOW


if(file.exists(config_params_file)){
  list2env(readRDS(config_params_file), 
           envir = .GlobalEnv)
  print("using config file for filenames and parameters...")
} else { 
  cat("Hardcoded user input inside of this script will be used.")
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# -------------- C H E C K  &  R E A D  D A T A ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fn_data_input = fn_database_input

if(file.exists(fn_data_input)) {
  data_input0 = read.csv(fn_data_input)
  cat("data_input file ", fn_data_input, " loaded successfully.\n",
      "contains ", nrow(data_input0), " rows & ", ncol(data_input0), "columns. \n")
} else {
  stop(cat("data_input file ", fn_data_input, " doesn't exist. please run 0_check_load_input.R and run script again.\n\n"))
}

#EDIT REMOVED FUNCTIONAL TRAP BC 0 TRAPS ARE NOW IN THE DATABASE AS OF 24-12-26


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ----------- D A T A B A S E   P R E P R O C E S S I N G ------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_input = data_input0


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ------------ G E T  T R A P  N U M B E R S ---------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 
#should be the same for every week that doesn't have a  malfunction or a 
trap_p_wk0 = data_input %>%
  filter(note != "m") %>% #remove malfunction traps
  filter(method == "L") %>% # remove gravid traps which aren't used for abundance
  distinct(across(all_of(grp_dist_trap))) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools and spp
  group_by(year, zone, week) %>% #get number of traps per week per zone
  summarize(trap_L = n(), .groups = "drop")



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ------------ C H E C K  T R A P  N U M B E R S -----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
trap_check = trap_p_wk0 %>%
  filter(year < 2024)#filter before we started tracking malfunctioning traps

cat("Checking trap L NE not equal to 10 before 2024...\n")
if(trap_check %>% filter(zone == "NE" &trap_L!= 10)  %>% nrow >0){
  warning("warning some weeks don't equal to 10\n\n")
} else {
  cat("All traps equal to 10. \n\n")
}

cat("Checking trap_L NW not equal to 9 before 2024...\n")
if(trap_check %>% filter(zone == "NW"&  trap_L != 9)  %>% nrow >0){
  warning("warning some weeks don't equal to 9\n")
} else {
  cat("All traps equal to 9. \n\n")
}


cat("Checking  trap_L SE L not equal to 15 before 2024...\n")
if(trap_check %>% filter(zone == "SE"&  trap_L != 15)  %>% nrow >0){
  warning("warning some weeks don't equal to 15\n")
} else {
  cat("All traps equal to 15. \n\n")
}


cat("Checking  trap_L SW not equal to 9 before 2024.\n")
if(trap_check %>% filter(zone == "SW" &  trap_L != 9)  %>% nrow >0){
  warning("warning some weeks don't equal to 9\n")
} else {
  cat("All traps equal to 9. \n\n")
}

rm(trap_check)

#GET WEEKS TRAP NUMBERS: FC
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fc_trap_wk0 = trap_p_wk0 %>% 
    filter(zone %in% fc_zones) %>%
    group_by(year,week) %>%
    summarise(
      zone = "FC",
      trap_L = sum(trap_L),
      .groups = "drop"
      )
  
  #>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #>combine data
  #>#>#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
  trap_p_wk = rbind(trap_p_wk0, fc_trap_wk0) %>%
    arrange(across(any_of(grp_vars)))
  
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # ------------ G E T  T O T A L   M O S Q U I T O E S ------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #ALL ZONES
    #get number of mosquitoes per night per trap
    m_p_wk0 = data_input %>%
      group_by(across(all_of(c(grp_vars, "method")))) %>% #get number of mosquitoes per week per zone per species
      summarize(mosq = sum(total), .groups = "drop") %>%
      pivot_wider(names_from = method, values_from = mosq, 
                  names_prefix = "mosq_", values_fill = 0) %>%
      mutate(mosq = mosq_L + mosq_G)
    
    
#FC
#get number of mosquitoes per night per trap
fc_m_p_wk = m_p_wk0 %>%
    filter(zone %in% fc_zones) %>%
    group_by(year, week, spp) %>%
     summarise(zone = "FC", 
              across(where(is.numeric), ~sum(.x)),
              .groups = "drop")

m_p_wk = rbind(m_p_wk0, fc_m_p_wk)  %>%
  arrange(across(any_of(grp_vars))) #%>%
  #select(-any_of(c("mosq_G", "mosq"))) # EDIT IF YOU WANT GRAVID TRAPS
    
abund = m_p_wk %>% #starting with m_p_wk on left because has spp
  left_join(trap_p_wk, by = c("year", "week", "zone")) %>%
      mutate(abund = round(mosq_L/trap_L,2)) %>%
      arrange(across(any_of(grp_vars)))
 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ----------------- A B U N D   S U M M A R Y  -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
abund_sum = abund %>%
  group_by(year) %>%
  summarize(n_traps = sum(trap_L)/2,
            n_mosq = sum(mosq_L),
            .groups = "drop") %>%
  mutate(abund = n_mosq/n_traps)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ----------------- A B U N D   W R I T E -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
write.csv(abund, fn_abund_mid, row.names = F)


if(file.exists(fn_abund_mid)) {
  cat(fn_abund_mid, " file saved successfully \n")
  # print(abund_sum)
  # cat("\n\n", nrow(data_input) - nrow(data_input0), " samples were filtered during data input preprocessing. \n\n")

} else {
  cat(fn_abund_mid, " file didn't save. Please check script \n")
}



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ----------------- A B U N D   L O G  -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

log_file = file(paste0("logs/Processing_Log_", Sys.Date(), ".txt"), open = "a")
sink(log_file)
cat("*********************** A B U N D A N C E   L O G  **********************\n\n")
cat("Abundance Stats by Year \n")
write.table(abund_sum, row.names = FALSE, col.names = TRUE)
cat("\n\n", nrow(data_input) - nrow(data_input0), " samples were filtered during data input preprocessing. \n\n")
sink()


