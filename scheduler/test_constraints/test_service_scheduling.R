# Compare the old and new service scheduling functions

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
source("../scheduler/old_funs.R")

# static
options <- c("service", "all")
phase_dates_xlsx <- "PhaseDates_v2_20201210.xlsx"

# -----default params for development -----
# wtis_in <- "Waitlist Dec9th2020.xlsx"
# start_date <- as.Date("2021-01-04")


# # phase_list_xlsx = rep("Phase4_v2.xlsx", 6) # could use different schedules if you wanted
# phase_list_xlsx <- rep("Phase4_20201210_7hrblocks.xlsx", 6)
# services <- list(
#     "General", "Otolaryngology", "Ophthalmology", "Orthopaedics",
#     "Gynaecology", "Plastics", "Urology", "Dentistry"
# )

## ------ 1 week historical validation --------

# also need to change columns  of the waitlist
# looks like services need to be changed
# wtis_in = "historical_validation/historical_waitlist_2019-08-01_2019-08-15.xls"
# phase_list_xlsx = rep("historical_validation/Phase6_v1_2x7.5ur_7.5op.xlsx", 6) # 6 is # of phases? ASK LAUREN


## ------ 1 month historical validation --------
services <- list(
    "General", "Orthopaedics", "Plastics", "Otolaryngology",
    "Dentistry", "Urology", "Ophthalmology", "Neurosurgery"
)
start_date <- as.Date("2021-01-15")
wtis_in <- "historical_validation/historical_waitlist_2021-01-15_2021-02-15.xls"
# # dynamic
# phase_list_xlsx = rep("historical_validation/historical_blocksched_2021-01-15_2021-02-15_target.xlsx", 6)
# simple
phase_list_xlsx <- rep("historical_validation/historical_blocksched_2021-01-15_2021-02-15_v0.xlsx", 6)

# ------- run the scheduler -------
set.seed(2021)
time_compare <- options %>%
    map(function(sched_opts) {
        print(sched_opts)
        tic()
        res <- run_scheduler(
            wtis_in = wtis_in,
            sub_n = NULL,
            case_ids = NULL,
            phase_dates_xlsx = phase_dates_xlsx,
            phase_list_xlsx = phase_list_xlsx,
            services = services,
            start_date = start_date, ## Make start date nearest Monday.
            verbose_run = TRUE,
            turnover_buffer = 25,
            time = "anesthesia",
            rotating_services = c("Opthalmology", "Orthopaedics", "Urology", "Otolaryngology"),
            add_cases = FALSE,
            high_pri_prop = 0.5,
            home_only = FALSE,
            max_time = 180,
            surg_opts = "first",
            post_op_opts = sched_opts # whether to schedule service by service, or consider all at once'service' or 'all'
        )
        toc()
        return(res)
    })


plot_burndown_per_service(in_sched_raw = time_compare[[1]], phase_dates_xlsx = phase_list_xlsx[1])

# not too many days, depends on how significant a single day for surgeries is?
an_times <- time_compare %>%
    map(~ get_time_remaining_per_service(
        in_sched = .x,
        return_table = TRUE,
        verbose = TRUE
    ))

an_times[1]
an_times[2]
# ---- comparing post op scheduler functions -----

# options <- c('service', 'all')

service <- "General"
time_compare[[1]][[1]][[service]]$cases_scheduled
time_compare[[1]][[1]][[service]]$cases_scheduled

services %>% map(function(service) {
    print(identical(time_compare[[1]][[1]][[service]], time_compare[[1]][[1]][[service]]))
    print(identical(time_compare[[1]][[2]][[service]], time_compare[[1]][[2]][[service]]))
})