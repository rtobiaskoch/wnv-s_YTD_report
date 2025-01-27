#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------ H E A D E R  ------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#AUTHOR: TOBY KOCH
# DATE OF LAST REVISION: 25-01-20
#DESCRIPTION: this script utilizes the wnv summer surveillance database (wnv-s_database)
#to calculate the number of total pools in both CDC Light and Gravid Traps 
# which are tested for wnv (and as of 2024 SLEV)


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
  pacman::p_load(tidyverse, PooledInfRate)
})

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ---------------------- H A R D C O D E D   P A R A M E T E R S  --------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#these parameters will be used if a config file is supplied. 
#note that the config paramters names and these hardcoded file names must match

#DIRECTORIES
dir_input = "1_input" # <<<<<<<<<<-----------------------------------------------------  USER INPUT
dir_mid = "2_mid"
dir_output <- "3_output"

#CHECK THAT INPUT DIRECTORIES EXIST
if(!dir.exists(dir_input)) {
  cat(dir_input, " doesn't exist. Please create it and add database to file") }

#CHECK THAT MID DIRECTORIES EXIST
if(dir.exists(dir_mid)) {
  cat(dir_mid, " exists and will be used for output files.")
} else {
  dir.create(dir_mid)
  cat(dir_mid, " was created and will be used for output files.")
}

#CHECK THAT OUTPUT DIRECTORIES EXIST
if(dir.exists(dir_output)) {
  cat(dir_output, " exists and will be used for output files.")
} else {
  dir.create(dir_output)
  cat(dir_output, " was created and will be used for output files.")
}

#CONFIG FILE
config_params_file = file.path(dir_input, "config_params.RDS") # <<<<<<<<<------------  USER INPUT

#FILENAMES
fn_database_input = file.path(dir_input, "wnv-s_database - data.csv")
fn_pools_mid = file.path(dir_mid,"pools.csv")
fn_abund_mid = file.path(dir_mid,"abundance.csv")
fn_data_output = file.path(dir_output, "data_zone_wk_stats.csv")


#GROUPING VARIABLES
fc_zones = fc_zones = c("NE", "SE", "NW", "SW")
zone_lvls = c("NW", "NE", "SE","SW", "FC", "LV", "BE", "BC")
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
# -------------- C H E C K  &  R E A D  D A T A B A S E --------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

read_check = function(fn) {
  if(file.exists(fn)) {
  data_input0 = read.csv(fn)
  cat("data_input file ", fn, " loaded successfully.\n",
      "contains ", nrow(data_input0), " rows & ", ncol(data_input0), "columns. \n")
} else {
  stop(cat("data_input file ", fn, " doesn't exist. Please run abundance.R \n
           and pools.R script and run again.\n\n"))
}
  return(data_input0)
}

data_input0 = read_check(fn_database_input)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ------------ C H E C K  &  R E A D  A B U N D A N C E --------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
abund = read_check(fn_abund_mid)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ------------ C H E C K  &  R E A D  A B U N D A N C E --------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
pools = read_check(fn_pools_mid)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ----------- D A T A B A S E   P R E P R O C E S S I N G ------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
t = anti_join(abund, pools, by = grp_vars)
  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ----------- D A T A B A S E   P R E P R O C E S S I N G ------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_input = data_input0 %>%
  filter(!is.na(test_code)) #remove imputed 0 traps with no mosquitoes 


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------- C A L C   P I R:  A L L   Z O N E S --------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#create a grouping variable for mle
  data_input = data_input %>%
    arrange(across(all_of(grp_vars))) %>% #dont split by method because PIR includes gravid traps
    mutate(grp = paste(year,week,zone,spp, sep ="-"))
  
  #run pIR
  mle = pIR(test_code ~ total|grp, data = data_input, pt.method = "firth")
  
  
#create pIR dataframe
 df_pir0 = as.data.frame(mle) %>%
   separate(grp,
            into = c("year", "week", "zone", "spp"),
            sep = "-") %>%
   transmute(year = as.integer(year),
             week = as.integer(week),
             zone = zone,
             spp = spp,
             pir = round(P,4),
             pir_lci = round(Lower,4),
             pir_uci = round(Upper,4)
           )
 
 
 #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 #----------------- C A L C   P I R:  F C -----------------------------
 #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 #calculate FC row for sum
   fc_pir0 = data_input %>%
     filter(zone %in% fc_zones) %>% #keep only fc zones
     mutate(zone = "FC") %>% #change zone to be FC
     mutate(grp = paste(year,week,zone,spp, sep ="-"))
   
   mle = pIR(test_code ~ total|grp, data = fc_pir0, pt.method = "firth")
   
  fc_pir0 =  as.data.frame(mle) %>%
     separate(grp,
              into = c("year", "week", "zone", "spp"),
              sep = "-") %>%
     transmute(year = as.integer(year),
               week = as.integer(week),
               zone = zone,
               spp = spp,
               pir = round(P,4),
               pir_lci = round(Lower,4),
               pir_uci = round(Upper,4)
     )
   

 df_pir = rbind(df_pir0, fc_pir0)

 #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #-------------- C O M B I N E   D A T A -----------------------------
 #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  data_report0 = pools %>%
    left_join(abund, by = grp_vars) %>%
    left_join(df_pir, by = grp_vars) %>%
    mutate(vi = round(abund * pir,4),
          vi_lci = round(abund * pir_lci,4),
          vi_uci = round(abund * pir_uci,4)) %>%
    mutate(year = factor(year),
           week = factor(week),
           zone = factor(zone, levels = zone_lvls),
           spp = factor(spp, levels = c("Pipiens", "Tarsalis"))) %>%
    arrange(zone, spp)

  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #--------------- S U M    A L L   S P P ----------------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  sum_col = c("pools_G", "pools_L", "n_pools",
              "pos_pools_G", "pos_pools_L", "n_pos_pools", 
              "mosq", "mosq_G", "mosq_L",
              "abund", "vi", "vi_lci", "vi_uci")
 
  distinct_col = c("trap_L_func", "trap_L")
  
  
  data_report_spp_all0 = data_report0 %>%
    mutate(spp =  "All") %>%
    group_by(year, week, zone, spp) %>%
    summarise(spp = "All",
              across(any_of(sum_col), sum),
              across(any_of(distinct_col), ~max(.)),
              .groups = "drop"
              )
  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------- C A L C   P I R   A L L   S P P ---------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>å
pir_all_spp = data_report0 %>%
      group_by(year, week, zone) %>%
      summarize(spp = "All",
                pir = round(sum(vi)/sum(abund),4),
                pir_lci = round(sum(vi_lci)/sum(abund),4),
                pir_uci = round(sum(vi_uci)/sum(abund),4),
                .groups = "drop")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------- J O I N   A L L   S P P ---------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_report_spp_all = left_join(data_report_spp_all0, pir_all_spp, by = grp_vars)

  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------- C O M B I N E   A L L   S P P ---------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_report = rbind(data_report0, data_report_spp_all) %>% 
    arrange(year,week, zone , spp)
  
  
write.csv(data_report, fn_data_output, row.names = F)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ----------------- R E P O R T   S U M M A R Y  -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_report_sum = data_report %>% 
  group_by(year) %>%
  summarise(abund = round(mean(abund),2),
            n_pos_pools = sum(n_pos_pools),
            pir = round(sum(pir),2),
            vi = round(sum(vi),2),
            .groups = "drop")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ----------------- R E P O R T   W R I T E ------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
write.csv(data_report, fn_data_output, row.names = F)

if(file.exists(fn_data_output)) {
  cat(fn_data_output, " file saved successfully \n")
  print(data_report_sum)
  
} else {
  cat(fn_data_output, " file didn't save. Please check script \n")
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# --------------- D A T A   R E P O R T   L O G  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
log_file = file(paste0("logs/Processing_Log_", Sys.Date(), ".txt"), open = "a")
sink(log_file)
cat("*********************** D A T A   R E P O R T    L O G  **********************\n\n")
cat("Data Report Stats by Year \n")
write.table(data_report_sum, row.names = FALSE, col.names = TRUE)
#cat("\n\n", nrow(data_input) - nrow(data_input0), " samples were filtered during data input preprocessing. \n\n")
sink()


