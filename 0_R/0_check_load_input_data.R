
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------L O A D  P A C K A G E S ----------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rm(list = ls())
suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(googlesheets4, googledrive, googledrive, argparse, #importing and exporting
                 progress, #for progress bar
                 tidyverse#manipulation
  )
  
})

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------ C O N F I G ------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#IF THE CONFIG PARAMS FILE EXISTS US THAT TO DEFINE INPUTS 
# ELSE DEFINE THEM HERE
config_params_file = "1_input/config_params.RDS" # <<<<<<<<<<----------------------------------  USER INPUT

if(file.exists(config_params_file)){
  list2env(readRDS(config_params_file), 
           envir = .GlobalEnv)
} else { # if no config define them yourself peasant.
  
  #INPUTS FOR THIS SCRIPT IF CONFIG FILE DOESN'T EXIST *****************
  
  #KEYS
  key_database_gsheet = "12Mf-w9I9NHTTDjzEPRoxUE08ka4WZ6RE-RM1s-FW7qA"
  
  dir_input = "1_input"
  dir_scripts = "0_R"
  fn_gdrive_database = file.path(dir_input,"wnv-s_database - data.csv") # <<<<<<<--------------------------------------------------- U S E R   I N P U T
  
} # end of if config_params_file exist statement


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------- L O A D   G S H E E T _ P U L L . R -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fn_gsheet_pull = paste0(dir_scripts, '/gsheet_pull_prompt.R')

#tries to load gsheet_pull_prompt from github first
devtools::source_url(paste0("https://raw.githubusercontent.com/rtobiaskoch/wnv-s_data_tools/refs/heads/main/",
                            fn_gsheet_pull)
)
#if that does
if(!exists('fun_gsheet_pull_prompt', mode = 'function')) {
  if(file.exists(fn_gsheet_pull)) {
    source(fn_gsheet_pull)
    print(paste0(fn_gsheet_pull, " file loaded from local directory"))
  } else { warning(paste0(fn_gsheet_pull, "script is unavailable. Check github or scripts."))  }
} else {
  print(paste0(fn_gsheet_pull, " file successfully loaded from github"))
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ------C H E C K  &  L O A D  F I L E S   F R O M  G D R I V E ------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fun_gsheet_pull_prompt(fn_gdrive_database, "data", key_database_gsheet)







