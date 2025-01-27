#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------ H E A D E R  ------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#AUTHOR: TOBY KOCH
# DATE OF LAST REVISION: 25-01-20
#DESCRIPTION: this script utilizes the wnv summer surveillance database (wnv-s_database)
#to calculate the number of total pools in both CDC Light and Gravid Traps 
# which are tested for wnv (and as of 2024 SLEV)

#DATA INPUT 
#pool level data from database

#VERSIONS
#V2
# - contains code to account for 0 traps and malfunction traps being account for in the 
#   database.This simplifies code and only requires the database as an input
# - includes list of used hardcoded parameters that user can hardcode if the config_params.RDS
#   file is unavailable
# - added more messages to inform the user of what is happening within the code
# - doesn't filter by week or year by using check_read_fun.R, processes all data


rm(list = ls())

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
fn_pools_mid = file.path(dir_mid,"pools.csv")

#GROUPING VARIABLES
fc_zones = fc_zones = c("NE", "SE", "NW", "SW")
grp_vars = c("year", "week", "zone", "spp")
grp_dist_trap = c("year", "week", "zone", "trap_id") #grouping variables to determine distinct trap #


#----  end of hardcoded parameters
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

#----

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

missing_test_code = data_input0 %>% 
  filter(total > 0 & is.na(test_code)) 

if(missing_test_code %>% nrow() > 0) {
  stop(cat("warning your database has pools ", missing_test_code$csu_id, " with missing test codes"))
} else {
  cat("All Pools n > 0 have test_code. Nice. \n\n")
}

rm(missing_test_code)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ----------- D A T A B A S E   P R E P R O C E S S I N G ------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_input = data_input0 %>%
  filter(!is.na(test_code)) #remove imputed 0 traps with no mosquitoes 


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# --------------------- C A L C   P O O L S --------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



#calculate number of pools  
  n_pools_zone = data_input %>% 
    group_by(across(all_of(c(grp_vars, "method")))) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = method, values_from = n, 
                names_prefix = "pools_", values_fill = 0) %>%
    mutate(n_pools = pools_L + pools_G)
 
  if(any(is.na(n_pools_zone$n_pools))) {
    stop(cat("Warning pools contain missing values"))
  } 
  
  fc_n_pools = data_input %>% 
    filter(zone %in% fc_zones) %>%
    group_by(year, week, spp, method) %>%
    summarise(zone = "FC",
              n = n(),
              .groups = "drop") %>%
    pivot_wider(names_from = method, values_from = n, 
                names_prefix = "pools_", values_fill = 0) %>%
    mutate(n_pools = pools_L + pools_G)
  
  if(any(is.na(fc_n_pools$n_pools))) {
    stop(cat("Warning pools contain missing values"))
  } 
  
  n_pools =  rbind(n_pools_zone, fc_n_pools)
  
  #calculate the number of positive pools
  pos_pools_zone =  data_input %>% 
    group_by(across(all_of(c(grp_vars, "method")))) %>%
    summarise(n = sum(test_code), .groups = "drop") %>%
    pivot_wider(names_from = method, values_from = n, 
                names_prefix = "pos_pools_", values_fill = 0) %>%
    mutate(n_pos_pools = pos_pools_L + pos_pools_G)
  
  fc_pos_pools = data_input %>% 
    filter(zone %in% fc_zones) %>%
    group_by(year, week, spp, method) %>%
    summarise(zone = "FC",
              n = sum(test_code)) %>%
    pivot_wider(names_from = method, values_from = n, 
                names_prefix = "pos_pools_", values_fill = 0) %>%
    mutate(n_pos_pools = pos_pools_L + pos_pools_G)
  
  pos_pools =  rbind(pos_pools_zone, fc_pos_pools)
  
  pools = left_join(n_pools, pos_pools, by = grp_vars)
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # ----------------- P O O L   S U M M A R Y  -------------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
pool_sum = pools %>% 
    group_by(year) %>%
    summarise(n_pools = sum(n_pools),
              pos_pools = sum(n_pos_pools),
              .groups = "drop") %>%
    mutate(pos_rate = round(pos_pools/n_pools,2))
  

  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # ----------------- P O O L   W R I T E ------------------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
write.csv(pools, fn_pools_mid, row.names = F)

  if(file.exists(fn_pools_mid)) {
    cat(fn_pools_mid, " file saved successfully \n")
        print(pool_sum)
        cat("\n\n", nrow(data_input) - nrow(data_input0), " samples were filtered during data input preprocessing. \n\n")

  } else {
    cat(fn_pools_mid, " file didn't save. Please check script \n")
  }
  
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # ----------------- P O O L    L O G  ------------------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
  log_file = file(paste0("logs/Processing_Log_", Sys.Date(), ".txt"), open = "a")
  sink(log_file)
  cat("*********************** P O O L E D    L O G  **********************\n\n")
  cat("Pooled Stats by Year \n")
  write.table(pool_sum, row.names = FALSE, col.names = TRUE)
  cat("\n\n", nrow(data_input) - nrow(data_input0), " samples were filtered during data input preprocessing. \n\n")
  sink()
  
