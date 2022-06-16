# setwd("/Users/delvin/Documents/OneDrive - SickKids/scheduler/Scheduler_Input_Data/")
setwd("C:/Users/lauren erdman/Desktop/MLCore/pos_scheduler/")

base_dir = "C:/Users/lauren erdman/Desktop/MLCore/pos_scheduler/"

data_dir = "C:/Users/lauren erdman/OneDrive - SickKids/"


library(readxl)
library(dplyr)
library(lubridate)
library(purrr)
library(tictoc)

source(paste0(base_dir,"/scheduler/run_scheduler_funs.R"))
source(paste0(base_dir,"/scheduler/service_funs.R"))
source(paste0(base_dir,"/scheduler/plotting_funs.R"))
source(paste0(base_dir,"/scheduler/post_hoc_funs.R"))
source(paste0(base_dir,"/scheduler/constraints.R"))
source(paste0(base_dir,"/scheduler/test_funs.R"))

# --- static params ----

{
  options <- c("all")
  phase_dates_xlsx <- paste0(data_dir,"/example_data/PhaseDates.xlsx")
  services <- list(
    "General", "Otolaryngology",
    "Ophthalmology",
    # "Gynaecology",
    "Cardiovascular",
    "Orthopaedics", "Plastics", "Urology", "Dentistry"
  )
  
  unit_targets <- c(
    "5A" = 8, "5B" = 3, "5C" = 3, "8C" = 5,
    "5A Step Down" = 3, "5B Step Down" = 2,
    "5C Step Down" = 3, "8C Step Down" = 2,
    "ICU" = 3
  )
  pacu_target <- 18
  
  # this waitlist is simulated
  wtis_in <- paste0(data_dir,"/example_data/Waitlist_DEID.xlsx")
  turnover_buffer <- 25
}


# ------- run the scheduler -------
set.seed(2021)

## schedules to compare with scheduler
sched = paste0(data_dir,"/example_data/",c("Sched_JanApr.xlsx","Sched_AprJul.xlsx","Sched_JulSept.xlsx","Sched_SeptDec.xlsx",
                                                          "Sched_JanApr.xlsx","Sched_AprJul.xlsx"))

sched_compare = paste0(data_dir,"/example_data/",c("Sched_JanApr_compare.xlsx","Sched_AprJul_compare.xlsx",
                                                   "Sched_JulSept_compare.xlsx","Sched_SeptDec_compare.xlsx",
                                                  "Sched_JanApr_compare.xlsx","Sched_AprJul_compare.xlsx"))

options = list(sched, sched_compare)

## Run scheduler for each schedule
sched_compare <- 1:length(options) %>%
  map(function(idx) {
    print(options[[idx]])
    
    if (str_detect(options[[idx]][[1]], 'April')){
      start_date = '2022-04-04'
    } else{
      start_date = '2022-04-04'
    }
    start_date = as.Date(start_date)
    
    tic()
    res <- run_scheduler(
      wtis_in = wtis_in,
      sub_n = NULL,
      case_ids = NULL,
      phase_dates_xlsx = phase_dates_xlsx, 
      phase_list_xlsx = options[[idx]],
      services = services,
      start_date = start_date, 
      verbose_run = TRUE,
      turnover_buffer = turnover_buffer,
      time = "surgeon",
      # time = "anesthesia", ## Lauren debug
      rotating_services = c("Opthalmology", "Orthopaedics", "Urology", "Otolaryngology"),
      add_cases = FALSE,
      high_pri_prop = 0.5,
      home_only = FALSE,
      max_time = (10.5*60)-turnover_buffer, # max block time in hours accounting for turn over buffer
      surg_opts = "first",
      post_op_opts = "all", # whether to schedule service by service, or consider all at once'service' or 'all'
      unit_opts = unit_targets, # ensure this named vector contains all post op destinations found in the waitlist
      pacu_opts = pacu_target,
      surg_sched = NULL,
    )
    toc()
    return(res)
  })


## extract data frame structured schedules from list of schedules
spreadsheets <- sched_compare %>%
  map(~ get_spreadsheet_per_service_sched(
    in_sched_raw = .x,
    per_block = TRUE,
    per_case = TRUE,
    per_day = TRUE,
    write_files = FALSE,
    orig_data_only = TRUE
  ))

## should = number of schedules 
length(spreadsheets)

## 3 schedule data frames at different levels of summarization by row:
##  per-block
##  per-day
##  per-case 
names(spreadsheets[[1]])

###
## Plot post-op destination
###

  ## From baseline schedule
plot_post_op_dest(day_df = spreadsheets[[1]]$day_df)

  ## from comparitor schedule
plot_post_op_dest(day_df = spreadsheets[[2]]$day_df)


###
## Plot burndown
###

  ## from baseline schedule
plot_burndown_per_service_from_day_df(day_df = spreadsheets[[1]]$day_df)

  ## from comparitor schedule
plot_burndown_per_service_from_day_df(day_df = spreadsheets[[2]]$day_df)

