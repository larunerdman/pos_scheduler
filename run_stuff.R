
library(readxl)
library(dplyr)
library(lubridate)
library(purrr)
setwd("Scheduler_Input_Data/")

## CREATE SCHEDULING LIST


source('../run_scheduler_funs.R')
source('../service_funs.R')


an_sched_out <- run_scheduler(
    wtis_in = "Waitlist Dec9th2020.xlsx",
    sub_n = NULL,
    case_ids = NULL,
    phase_dates_xlsx = "PhaseDates_v2_20201210.xlsx",
    phase_list_xlsx = c(
        "Phase4_20201210_7hrblocks.xlsx", 
        "Phase4_20201210_7hrblocks.xlsx", 
        "Phase4_20201210_7hrblocks.xlsx",
        "Phase4_20201210_7hrblocks.xlsx", 
        "Phase4_20201210_7hrblocks.xlsx",
        "Phase4_20201210_7hrblocks.xlsx"
    ), ## order must match order in phase_dates
    # services = list("General","Otolaryngology","Neurosurgery","Ophthalmology","Orthopaedics",
    #                 "Gynaecology","Plastics","Urology","Dentistry"),
    services = list(
        "General", "Otolaryngology", "Ophthalmology", "Orthopaedics",
        "Gynaecology", "Plastics", "Urology", "Dentistry"
    ),
    start_date = as.Date("2021-01-04"), ## Make start date nearest Monday.
    verbose_run = TRUE,
    turnover_buffer = 25,
    time = "anesthesia",
    rotating_services = c("Opthalmology", "Orthopaedics", "Urology", "Otolaryngology"),
    add_cases = FALSE, ## Need to update case adding:
    ## simulate adding week by week while orig cases are still on list?
    ## Add cases when new week is added
    ## (maybe have a "check" to see if list has grown, then concat cases based on week's month)
    high_pri_prop = 0.5,
    home_only = FALSE,
    max_time = 180
)
all.equal(an_sched_out2, # original add_case_block
          an_sched_out) # refactored add_case_block
          an_sched_out3 # no max_block_sum fix

df1 <- get_spreadsheet_per_service_sched(in_sched_raw = an_sched_out)$day_df %>% as_tibble() %>% mutate(weekday = weekdays(date)) 
df2 <- get_spreadsheet_per_service_sched(in_sched_raw = an_sched_out2)$day_df %>% as_tibble() %>% mutate(weekday = weekdays(date)) 

all.equal(df1 %>% filter(!weekday %in% c('Saturday')), df2)


source("../plotting_funs.R")
source("../post_hoc_funs.R")
## COMPUTE REMAINING TIME TO CLEARING PER-SERVICE
(an_times_df <- get_time_remaining_per_service(an_sched_out,
    return_table = TRUE,
    verbose = TRUE
))
``
# amount of time that is shown in the plot 



#View(an_times_df)


## RUN THIS AND ADD BOTH POST-OP DESTINATION AND WHETHER OR NOT IT'S AN ORIG CASE PER-CASE, PER-BLOCK, AND PER-DAY

# USE THIS TO DEBUG 
# look per case and see if cases in the same block have the same schedule
# plot(original burndown vs the new burndown) 
# 
dfs_list <- get_spreadsheet_per_service_sched(
    in_sched_raw = an_sched_out,
    per_block = TRUE,
    per_case = TRUE,
    per_day = TRUE,
    write_files = FALSE,
    orig_data_only = TRUE
)

# 
# surgeon not booked to same slot currently
dfs_list$case_df %>% as_tibble() %>% group_by(services, Date, weeks) %>% count(surgeon) 

# View(dfs_list$block_df)
View(dfs_list$case_df) ## SCHEDULING A LOT OF SIM CASES EARLY ON WITH CURRENT SIM FRAMEWORK
# View(dfs_list$day_df)

# write.csv(dfs_list$block_df,"Block_df_Model2_3b_20201103.csv",quote = FALSE,
#           row.names=FALSE)
# write.table(dfs_list$case_df,"Case_df_20201214_7hrblocks.tsv",quote = FALSE,
#           row.names=FALSE,sep = ";") ## SCHEDULING A LOT OF SIM CASES EARLY ON WITH CURRENT SIM FRAMEWORK
# write.csv(dfs_list$day_df,"Day_df_Model2_3b_20201103.csv",quote = FALSE,
#           row.names=FALSE)


case_counts_per_month <- count_cases_per_month(case_df = dfs_list$case_df)

# write.csv(case_counts_per_month,"Monthly_case_count_Model2_3b_20201103.csv",quote = FALSE,
#           row.names=FALSE)

# View(case_counts_per_month)

# write.csv(case_counts_per_month,
#           file="case_counts_per_month_plus10block.csv",
#           quote=FALSE)

View(count_cases_to_date(dfs_list$case_df, date = as.Date("2021-01-01")))

## PLOT BURNDOWN PER SERVICE
plot_burndown_per_service(in_sched_raw = an_sched_out,  phase_dates_xlsx = "PhaseDates_v2_20201210.xlsx")

# plot_post_op_dest(day_df = dfs_list$day_df, end_date = "2020-09-01")
plot_post_op_dest(day_df = dfs_list$day_df)



plot_inpt_post_op_dest(day_df = dfs_list$day_df, dow = FALSE)
plot_inpt_post_op_dest(day_df = dfs_list$day_df, dow = TRUE)

## make into function creating daily graphs (give option to split by service)


##
##    PLOTTING BLOCK TIMES VS BOOKED CASE TIMES
##

boxplot_block_times_vs_booked(dfs_list$block_df)
