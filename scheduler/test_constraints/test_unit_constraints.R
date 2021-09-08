setwd("scheduler/Scheduler_Input_Data/")
library(readxl)
library(dplyr)
library(lubridate)
library(purrr)
library(tictoc)

source("../scheduler/run_scheduler_funs.R")
source("../scheduler/service_funs.R")
source("../scheduler/plotting_funs.R")
source("../scheduler/post_hoc_funs.R")
source("../scheduler/constraints.R")
# source("../scheduler/old_funs.R")


# --- static params ----
# options <- c('service', 'all') # comparing 'service' and 'all' - need to deprecate service scheduler

{
  options <- c("all")
  phase_dates_xlsx <- "PhaseDates_v2_20201210.xlsx"
  phase_list_xlsx = rep("Phase4_v2.xlsx", 6) # could use different schedules if you wanted
  # phase_list_xlsx <- rep("Phase4_20201210_7hrblocks.xlsx", 6)
  services <- list(
    "General", "Otolaryngology", "Ophthalmology", "Orthopaedics",
    "Gynaecology", "Plastics", "Urology", "Dentistry"
  )
  start_date <- as.Date("2021-10-04")
}


## ------ 1 week historical validation --------

# also need to change columns  of the waitlist
# looks like services need to be changed
# wtis_in = "historical_validation/historical_waitlist_2019-08-01_2019-08-15.xls"
# phase_list_xlsx = rep("historical_validation/Phase6_v1_2x7.5ur_7.5op.xlsx", 6) # 6 is # of phases? ASK LAUREN


## ------ 1 month historical validation --------
# services = list("General", "Orthopaedics", "Plastics", "Otolaryngology",
#                   "Dentistry", "Urology", "Ophthalmology", "Neurosurgery")
# start_date = as.Date("2021-01-15")
# wtis_in = "historical_validation/historical_waitlist_2021-01-15_2021-02-15.xls"
# # dynamic
# phase_list_xlsx = rep("historical_validation/historical_blocksched_2021-01-15_2021-02-15_target.xlsx", 6)
# # simple
# # phase_list_xlsx = rep("historical_validation/historical_blocksched_2021-01-15_2021-02-15_v0.xlsx", 6)


get_params <- function(use_test_data = TRUE){
  
  if (use_test_data){
    set = 'test'
    unit_targets <- c(
      "5A" = 3, "5B" = 3, "5C" = 3, "8C" = 3, "ICU" = 2,
      "5A Step Down" = 2, "5B Step Down" = 2,
      "5C Step Down" = 2, "8C Step Down" = 2
    )
    pacu_target <- 8
    
    wtis_in <- "wtis_20200615_sheet1_relevant_units_FAKE.xlsx" # subset with relevant target units and fake times (i think 1minute each) for surgery duration
    turnover_buffer <- 0 # MAKE SURE TO USE THE RIGHT ONE FOR DEVELOPMENT AND ACTUAL USE CASES
    print("USING DEV PARAMS")
    
  } else{
    set = 'real'
    unit_targets <- c(
      "5A" = 8, "5B" = 3, "5C" = 3, "8C" = 5,
      "5A Step Down" = 3, "5B Step Down" = 2,
      "5C Step Down" = 3, "8C Step Down" = 2,
      "ICU" = 3
    )
    pacu_target <- 18
    
    wtis_in <- "wtis_20200615_sheet1_relevant_units.xlsx" # just the testing ones
    # wtis_in <- "Waitlist Dec9th2020.xlsx"
    # wtis_in <- "wtis_20200615_sheet1.xlsx" # with all post op
    turnover_buffer <- 25
  }
  
  return(list('set' = set, 'unit_targets' = unit_targets, 'pacu_target' = pacu_target, 'turnover_buffer' = turnover_buffer, 'wtis_in' = wtis_in))
}

get_params()[['pacu_target']]
# ------- run the scheduler -------
set.seed(2021)
options <- c(TRUE, FALSE)
constraints_compare <- options %>%
  map(function(sched_opts) {
    print(sched_opts)
    
    test_unit_targets <- sched_opts
    
    params <- get_params(test_unit_targets)
    print(get_params()[['pacu_target']])
    tic()
    res <- run_scheduler(
      wtis_in = params[['wtis_in']],
      sub_n = NULL,
      case_ids = NULL,
      phase_dates_xlsx = phase_dates_xlsx,
      phase_list_xlsx = phase_list_xlsx,
      services = services,
      start_date = start_date, ## Make start date nearest Monday.∏
      verbose_run = TRUE,
      turnover_buffer = params[['turnover_buffer']],
      time = "surgeon",
      rotating_services = c("Opthalmology", "Orthopaedics", "Urology", "Otolaryngology"),
      add_cases = FALSE, ## Need to update case adding:
      high_pri_prop = 0.5,
      home_only = FALSE,
      max_time = 180,
      surg_opts = "first",
      post_op_opts = 'all', # whether to schedule service by service, or consider all at once'service' or 'all'
      unit_opts = params[['unit_targets']], # ensure this named vector contains all post op destinations found in the waitlist
      pacu_opts = params[['pacu_target']],
      surg_sched = NULL,
    )
    toc()
    return(res)
  })

# save.image('../test_constraints.RData')
# load.image('../test_constraints.RData')
# plot burndown graphs
constraints_compare %>% map(~ plot_burndown_per_service(in_sched_raw = .x,
                                                        phase_dates_xlsx = phase_dates_xlsx ))


# ---- looking at target constraints ----

spreadsheets <- constraints_compare %>%
  map(~ get_spreadsheet_per_service_sched(
    in_sched_raw = .x,
    per_block = TRUE,
    per_case = TRUE,
    per_day = TRUE,
    write_files = FALSE,
    orig_data_only = TRUE
  ))

########### set to FALSE if we want to assess a 'real' dataset, else TRUE for a stress/dev test ########### 
{
  test_params <- TRUE
  params <- get_params(test_params) # true for test, false for real data
  params
  wtis_dat <- read_excel(params[['wtis_in']])
  # test %>% count(`Planned Post-op Destination`, sort = TRUE)
  # test %>% count(Service)
  }


# ground truth is the post-op destination from the waitlist, irrespective of the service
out_spreadsheet <- if(test_params) spreadsheets[[1]]$case_df else spreadsheets[[2]]$case_df

joined_dat <- as_tibble(out_spreadsheet) %>%
  left_join(wtis_dat %>%
              select(
                "case_id" = "Case #",
                "planned_post_op_destination" = `Planned Post-op Destination`
              ))


# distribution of cases, length
# joined_dat %>%
#     mutate(unit = stringr::str_split_fixed(planned_post_op_destination, pattern = " ", n = 2)[, 1]) %>%
#     group_by(unit) %>%
#     summarize(total_length = sum(case_length), total_cases = n(), broom::tidy(quantile(case_length))) %>%
#     tidyr::spread(names, x) %>%
#     select(total_length, total_cases, `0%`, `25%`, `50%`, `75%`, `100%`)

unit_targets <- params[['unit_targets']]
unit_targets_tbl <- tibble("sub_unit" = names(unit_targets), "unit_targets" = unit_targets) %>%
  mutate(unit = stringr::str_split_fixed(sub_unit, pattern = " ", n = 2)[, 1])

# get post-op counts, by date
unit_check <- joined_dat %>%
  count(Date, planned_post_op_destination) %>%
  mutate(unit = stringr::str_split_fixed(planned_post_op_destination, pattern = " ", n = 2)[, 1]) %>%
  arrange(-n)


# sanity check without Home - day by day
# unit_check %>%
#   filter(planned_post_op_destination != "Home") %>%
#   # arrange(planned_post_op_destination) %>%
#   group_by(Date) %>%
#   arrange(Date, unit) %>%
#   .[1:30, ] %>%
#   group_split()

# overall, by unit
unit_check %>%
  filter(planned_post_op_destination != "Home") %>%
  group_by(Date, unit) %>%
  summarize(total = sum(n)) %>%
  arrange(-total)

# check for main units and ICU
# distribution of DAILY UNIT CASES
unit_check %>%
  # filter(planned_post_op_destination != "Home") %>%
  mutate(unit = ifelse(str_detect(unit, "[A-Z]{2}CU"), "ICU", unit)) %>%
  group_by(unit, Date) %>%
  summarize(daily_total = sum(n)) %>%
  group_by(unit) %>%
  summarize(total = sum(daily_total), broom::tidy(quantile(daily_total))) %>%
  tidyr::spread(names, x) %>%
  select(unit, total, `0%`, `25%`, `50%`, `75%`, `100%`) %>%
  left_join(unit_targets_tbl %>% filter(str_detect(sub_unit, regex("^[0-9][A-Z]$|ICU"))), by = c("unit")) %>%
  mutate(over = ifelse(`100%` > unit_targets, 1, 0)) %>% suppressWarnings()

# unit_check %>%
#     # filter(planned_post_op_destination != "Home") %>%
#     mutate(unit = ifelse(str_detect(unit, "[A-Z]{2}CU"), "ICU", unit)) %>%
#     group_by(unit) %>%
#     summarize(total = sum(n), broom::tidy(quantile(n))) %>%
#     tidyr::spread(names, x) %>%
#     select(unit, total, `0%`, `25%`, `50%`, `75%`, `100%`) %>%
#     left_join(unit_targets_tbl %>% filter(str_detect(sub_unit, regex("^[0-9][A-Z]$|ICU"))), by = c("unit")) %>%
#     mutate(over = ifelse(`100%` > unit_targets, 1, 0))

# on step down
# 
unit_check %>%
  filter(str_detect(planned_post_op_destination, "Constant")) %>%
  # filter(planned_post_op_destination != "Home") %>%
  mutate(unit = ifelse(str_detect(unit, "[A-Z]{2}CU"), "ICU", unit)) %>%
  group_by(planned_post_op_destination, unit, Date) %>%
  summarize(daily_total = sum(n)) %>%
  group_by(planned_post_op_destination, unit) %>%
  summarize(total = sum(daily_total), broom::tidy(quantile(daily_total))) %>%
  tidyr::spread(names, x) %>%
  select(unit, planned_post_op_destination, total, `0%`, `25%`, `50%`, `75%`, `100%`) %>%
  left_join(unit_targets_tbl %>% filter(str_detect(sub_unit, "Step"))) %>%
  mutate(over = ifelse(`100%` > unit_targets, 1, 0)) %>% suppressWarnings()


unit_targets[stringr::str_detect(names(unit_targets), "Step Down")]

# pacu check
joined_dat %>%
  mutate(pacu = ifelse(!stringr::str_detect(planned_post_op_destination, "Home|[A-Z]{2}CU"), "pacu", "not_pacu")) %>%
  count(Date, pacu) %>%
  tidyr::spread(pacu, n) %>%
  arrange(-pacu)


arrange(Date) # 06/13/20 etc are NA since only dentistry is slotted and these all go home
# ungroup() %>%
# filter(total == 4 & unit == "5B")

# icu check, by date (granular)
# unit_check %>%
#     filter(stringr::str_detect(planned_post_op_destination, "[A-Z]{2}CU")) %>%
#     group_by(Date) %>%
#     arrange(Date)

# # icu check, by date (total)
# unit_check %>%
#     filter(stringr::str_detect(planned_post_op_destination, "[A-Z]{2}CU")) %>%
#     group_by(Date) %>%
#     arrange(Date) %>%
#     summarize(total = sum(n)) %>%
#     arrange(-total)

# check ic units that are failing - FIXED
# joined_dat %>% filter(Date == "2021-01-08", stringr::str_detect(planned_post_op_destination, "[A-Z]{2}CU"))


# ---- sanity check for surgeons per block per service per week (surg_opt == '"FIRST")------
spreadsheets %>% map(function(x) {
  x$case_df %>%
    as_tibble() %>%
    # filter(services == "Dentistry") %>%
    count(Date, weeks, services, surgeon, block_num) %>%
    arrange(services) %>%
    # count(Date, weeks, services) %>%
    print(n = 50) %>%
    count(Date, services, block_num) %>%
    arrange(services) # %>% filter(n > 1)
})

