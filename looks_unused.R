
## SEE IF CASES CAN BE SPLIT MORE EFFICIENTLY
## index by date
create_schedule_df <- function(schedule_list ## output from function: run_scheduler
) {
  case_ids <- c()
  case_services <- c()
  case_length <- c()
  long_short <- c()
  days <- c()
  weeks <- c()
  blocks <- c()
  block_types <- c()

  for (week in 1:length(schedule_list)) {
    for (day in 1:length(schedule_list[[week]])) {
      for (block in 1:length(schedule_list[[week]][[day]][["procedure_blocks"]])) {
        case_ids <- c(case_ids, schedule_list[[week]][[day]][["procedure_blocks"]][[block]]$case_ids)
        case_length <- c(case_length, schedule_list[[week]][[day]][["procedure_blocks"]][[block]]$case_times)

        case_services <- c(case_services, rep(
          schedule_list[[week]][[day]][["procedure_blocks"]][[block]]$service,
          length(schedule_list[[week]][[day]][["procedure_blocks"]][[block]]$case_times)
        ))
        block_types <- c(block_types, rep(
          schedule_list[[week]][[day]][["procedure_blocks"]][[block]]$block_type,
          length(schedule_list[[week]][[day]][["procedure_blocks"]][[block]]$case_times)
        ))
        long_short <- c(long_short, rep(
          schedule_list[[week]][[day]][["procedure_blocks"]][[block]]$short_long,
          length(schedule_list[[week]][[day]][["procedure_blocks"]][[block]]$case_times)
        ))
        days <- c(days, rep(day, length(schedule_list[[week]][[day]][["procedure_blocks"]][[block]]$case_times)))
        weeks <- c(weeks, rep(week, length(schedule_list[[week]][[day]][["procedure_blocks"]][[block]]$case_times)))
        blocks <- c(blocks, rep(block, length(schedule_list[[week]][[day]][["procedure_blocks"]][[block]]$case_times)))
      }
    }
  }

  out_df <- data.frame(case_ids, case_services, case_length, long_short, block_types, days, weeks, blocks)

  lapply(list(case_ids, case_services, case_length, long_short, block_types, days, weeks, blocks), length)

  return(out_df)
}

## Find avg number of cases per day given the schedule
get_mean_cases_per_day <- function(sched_list, ## schedule list
                                   day_counts_only = FALSE ## if true, will remove days with no cases
) {
  day_counts <- c()

  for (week in 1:length(sched_list)) {
    for (day in 1:length(sched_list[[week]])) {
      day_count <- 0
      for (block in 1:length(sched_list[[week]][[day]][["procedure_blocks"]])) {
        day_count <- day_count + length(sched_list[[week]][[day]][["procedure_blocks"]][[block]]$case_ids)
      }
      day_counts <- c(day_counts, day_count)
    }
  }

  day_counts[day_counts == 0] <- NA

  if (day_counts_only) {
    return(day_counts)
  } else {
    return(mean(na.omit(day_counts)))
  }
}