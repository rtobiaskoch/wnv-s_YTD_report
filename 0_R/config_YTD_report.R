#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# H E A D E R
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE:
#1 checks and loads necessay package for pipeline
#2 defines input data directory
#3 checks if input directories exist
#4 checks if input files exist
#5 defines google sheet keys
#6 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#L O A D   P A C K A G E S   F O R   P I P E L I N E 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rm(list = ls())

suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(googlesheets4, googledrive, argparse, #importing and exporting
                 tidyverse, janitor, lubridate, rquery, #manipulation
                 PooledInfRate, #analysis
                 ggpubr, wesanderson, paletteer, leaflet, patchwork# plotting
                 )
})


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# D E F I N E   D I R E C T O R I E S   &   I N P U T   F I L E S 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# <<<<<<<------------------------------------------------------------------------------------------------ U S E R   I N P U T
#INPUT DIRECTORIES
dir_input <- "1_input"
config_params_file = file.path(dir_input, "config_params.RDS")

#SCRIPTS
dir_scripts <- "0_R"

#OUTPUT
dir_mid <- "2_mid"
dir_output <- "3_output"


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

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# C H E C K   F O R   I N I T I A L   D I R E C T O R I E S 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Initialize a vector to store missing directories
# Create a named list of directories for clarity
directories <- list(
  input = dir_input, 
  scripts = dir_scripts, 
  mid = dir_mid, 
  output = dir_output
)

missing_dirs <- c()

# Check for missing directories and collect them
for (dir_name in names(directories)) {
  dir_path <- directories[[dir_name]]
  
  if (!dir.exists(dir_path)) {
    missing_dirs <- c(missing_dirs, dir_path)
  }
}

# If there are missing directories, stop the script and display them
if (length(missing_dirs) > 0) {
  stop(
    paste(
      "The following directories are missing:\n",
      paste(missing_dirs, collapse = "\n"),
      "\nPlease create them in your working directory or update the config R file."
    )
  )
} else {
  # If all directories exist, proceed with the script
  print("All required directories exist.")
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# -------- C H E C K   I N P U T   S U B D I R  &   F I L E S
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fn_database_input = file.path(dir_input, "wnv-s_database - data.csv")


# Create a named list of directories to check
input_subdirs <- list(
  database = dir_input,
  scripts = dir_scripts
)

# Initialize a vector to store empty directories
empty_dirs <- c()

# Check each directory for emptiness
for (dir_name in names(input_subdirs)) {
  dir_path <- input_subdirs[[dir_name]]
  
  if (dir.exists(dir_path)) {
    # Check if the directory is empty
    if (length(list.files(dir_path)) == 0) {
      empty_dirs <- c(empty_dirs, dir_path)
    }
  } else {
    # If the directory doesn't exist, it's considered empty
    empty_dirs <- c(empty_dirs, dir_path)
  }
}

# If there are empty directories, stop the script and display them
if (length(empty_dirs) > 0) {
  warning(
    print(paste(
      "The following directories are empty or don't exist:\n",
      paste(empty_dirs, collapse = "\n"),
      "\nPlease populate them with the required files manually or with 0_check_load_input_data.R script."
    )
  ))
} else {
  # If all directories contain files, proceed with the script
  print("All required directories are populated.")
}


if(file.exists(fn_database_input)) {
  print("Database file exists. Update if desired or Proceed with processing.")
} else {
  warning(print("Database file doesn't exist. Be to download the file in the input directory."))
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ---------------- D E F I N E   G S H E E T   K E Y --------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
key_database_gsheet = "12Mf-w9I9NHTTDjzEPRoxUE08ka4WZ6RE-RM1s-FW7qA"
#key_foco_trap = "1Jna3Bu47gjBWWz5vCoel4ksa-LBuo8R3zVfQYFl73wI"

fn_gdrive_database = file.path(dir_input,"wnv-s_database - data.csv") # <<<<<<<--------------------------------------------------- U S E R   I N P U T
#fn_trap <- file.path(dir_input, "foco_trap - data.csv")


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-----U S E R   D E F I N E D   S T A T I C   D A T A   P A R A M E T E R S : -------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fc_zones = c("NE", "SE", "NW", "SW", "LV", "BC", "BE")
non_fc_zones = c("LV", "BC", "BE")
all_zones = c("NE", "SE", "NW", "SW", "LV", "BC", "BE")

copy_threshold = 500 # <<<<<<<------------------------------------------------------------------------------------------------ U S E R   I N P U T
rn_threshold = 34000 # <<<<<<<------------------------------------------------------------------------------------------------ U S E R   I N P U T
vi_threshold = 0.75 # <<<<<<<------------------------------------------------------------------------------------------------- U S E R   I N P U T

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------ U S E R   D E F  V A R I A B L E   D A T A   P A R A M E T E R S : -----------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#variables that will change from week to week
# IF if this code is run in pipeline via bash this variables will be defined there.
#otherwise they are hardcoded below
week_hardcode = 37  # <<<<<<<------------------------------------------------------------------------------------------------ U S E R   I N P U T
year_hardcode = 2024 # <<<<<<<----------------------------------------------------------------------------------------------- U S E R   I N P U T

# Create an argument parser
  parser <- ArgumentParser(description = "Script to handle config file data inputs")
  
  # Add arguments for week and year
  parser$add_argument("--week", help = "week of report", type = "integer")
  parser$add_argument("--year", help = "year of the report", type = "integer")
  
  # Parse arguments
  args <- parser$parse_args()
  
  # Assign default values if arguments are not provided
  week_filter <- if (!is.null(args$week)) args$week else week_hardcode
  year_filter <- if (!is.null(args$year)) args$year else year_hardcode
  
  # Output the values
  cat("Week filter set to:", week_filter, "\n")
  cat("Year filter set to:", year_filter, "\n")

week_filter_yr= 23:week_filter
week_filter_hx = 23:37
year_filter_hx = seq(year_filter-12, year_filter-1, by = 1)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------- F I L E  N A M E S  M I D ------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#fn_database_update = file.path(dir_mid, "wnv-s_database_update.csv")
fn_abund_mid = file.path(dir_mid,"abundance.csv")
fn_pools_mid = file.path(dir_mid, "pools.csv")
fn_trap_p_wk = file.path(dir_mid, "trap_p_wk.csv")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- F I L E  N A M E S   O U T P U T ------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fn_gdrive_archive = paste0("wnv-s_database_pre_y",year_filter, "_w", week_filter,".gsheet")
fn_data_output = file.path(dir_output, "data_zone_wk_stats.csv")



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------------------- C O L U M N  S E L E C T I O N -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
col_trap = c("zone", "lat", "long")

col_input_database = c("csu_id", "trap_id", "year", "week", "trap_date", 
                   "county", "method", "spp", "total", "test_code", "zone")

col_database = c( "csu_id", "trap_id", "zone", "year", "week", "trap_date", "county", "method", "spp",
  "total", "test_code", "cq", "copies_WNV", "seq", "cq", "lat", "long")

col_class_database <- c("csu_id" = "character", 
            "trap_id" = "character", 
            "year" = "numeric", 
            "week" = "numeric", 
            "trap_date" = "character", 
            "county" = "character", 
            "method" = "character", 
            "genus" = "character", 
            "spp" = "character", 
            "sex" = "logical", 
            "no_gravid" = "numeric", 
            "no_deplete" = "numeric", 
            "total" = "numeric", 
            "test_code" = "numeric", 
            "seq" = "numeric", 
            "cq" = "numeric",
            "zone" = "character",
            "lat" = "numeric",
            "long" = "numeric")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------- G R O U P _ B Y  V A R S -------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
grp_vars = c("year", "week", "zone", "spp")
hx_grp_vars = c("week", "zone")
zone_lvls = c("NW", "NE", "SE","SW", "FC", "LV", "BE", "BC")
non_routine_zones = c("BC") # <<<<<<<--------------------------------------------------------------------- U S E R   I N P U T


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------- C O L O R  S E T T I N G S -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
color_palette = wes_palette('Darjeeling1')

#https://coolors.co/palette/fb8b24-d90368-820263

pal_mozzy = c("hx_Tarsalis" = "grey50",
              "hx_Pipiens" = "grey30",
              "current_Tarsalis" = "#e9724c",
              "current_Pipiens" = "#820263")

pal_mozzy2 = c("Pipiens" = "#820263",
               "Tarsalis" = "#e9724c",
               "All" = "#faa916")


pal_mozzy3 = c("#ffc857", "#e9724c", "#c5283d")

curr_hx_pal = c("current" = "#e9724c",
                "hx"      = "grey50")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------- E X P O R T   C O N F I G ----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
all_params <- ls(envir = .GlobalEnv)

# Create a list containing all the values of these objects
all_params_list <- mget(all_params, envir = .GlobalEnv)

saveRDS(all_params_list, config_params_file)


if(file.exists(config_params_file)) {
  cat("config file ", config_params_file, "save successfully.")
}


# save to a log file
if(!dir.exists("logs")){
  dir.create("logs")
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------- S A V E  L O G  F I L E  ------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
log_file = paste0("logs/params_y",year_filter,"_w",week_filter, "_", Sys.Date(), ".txt")
sink(log_file)
print(paste0(config_params_file, " Log"))
all_params_list
sink()




