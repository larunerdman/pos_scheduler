
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


## make a daycare blcok matrix -- this isn't used yet
make_daycare_block_matrix <- function(service, daycare_sched, rotating_services) {
  num_r_services <- length(rotating_services)
  mods <- paste0("Mod", ((1:num_r_services) - 1))
  splits <- paste0("Split", ((1:num_r_services)))

  weekend_blocks <- as.numeric(na.omit(as.character(unlist(daycare_sched[daycare_sched[, 1] == "Weekend daycare", 2:6]))))
  weekend_split <- lapply(split(x = weekend_blocks, f = 1:num_r_services), function(y) {
    ifelse(length(y) > 0, y, NA)
  })
  names(weekend_split) <- splits

  weekday_blocks <- as.numeric(na.omit(as.character(unlist(daycare_sched[daycare_sched[, 1] == "Weekly daycare", 2:6]))))
  weekday_split <- lapply(split(x = weekday_blocks, f = 1:num_r_services), function(y) {
    ifelse(length(y) > 0, y, NA)
  })
  names(weekday_split) <- rev(splits)

  service_idx <- which(rotating_services == service)
  idx_vec <- c(1:num_r_services, 1:num_r_services)
  daycare_block_mat_list <- sapply(mods, function(mod) {
    mod_num <- as.numeric(substr(x = mod, start = 4, stop = 4))
    sched_idx <- idx_vec[(service_idx + mod_num)]

    if (!is.na(weekday_split[[paste0("Split", sched_idx)]])) {
      weekday_vec <- table(weekday_split[[paste0("Split", sched_idx)]])
      weekday_block_mat <- data.frame(
        blocks_per_week = weekday_vec,
        block_length = as.numeric(names(weekday_vec)),
        block_length_mins = as.numeric(names(weekday_vec)) * 60,
        block_type = paste0("T", names(weekday_vec), "h"),
        weekend = rep(FALSE, length(weekday_vec))
      )
    } else {
      weekday_block_mat <- data.frame(
        blocks_per_week = c(),
        block_length = c(),
        block_length_mins = c(),
        block_type = c(),
        weekend = c()
      )
    }
    if (!is.na(weekend_split[[paste0("Split", sched_idx)]])) {
      weekend_vec <- table(weekend_split[[paste0("Split", sched_idx)]])
      weekend_block_mat <- data.frame(
        blocks_per_week = weekend_vec,
        block_length = as.numeric(names(weekend_vec)),
        block_length_mins = as.numeric(names(weekend_vec)) * 60,
        block_type = paste0("T", names(weekend_vec), "h"),
        weekend = rep(TRUE, length(weekend_vec))
      )
    } else {
      weekend_block_mat <- data.frame(
        blocks_per_week = c(),
        block_length = c(),
        block_length_mins = c(),
        block_type = c(),
        weekend = c()
      )
    }


    all_daycare_blocks <- data.frame(rbind(weekday_block_mat, weekend_block_mat))

    return(all_daycare_blocks)
  })

  return(daycare_block_mat_list)
}


## make a daycare blcok matrix -- this isn't used yet
make_daycare_block_matrix = function(service,daycare_sched,rotating_services){
  
  num_r_services = length(rotating_services)
  mods = paste0("Mod",((1:num_r_services)-1))
  splits = paste0("Split",((1:num_r_services)))
  
  weekend_blocks = as.numeric(na.omit(as.character(unlist(daycare_sched[daycare_sched[,1] == "Weekend daycare",2:6]))))
  weekend_split = lapply(split(x = weekend_blocks,f = 1:num_r_services),function(y){ifelse(length(y) > 0,y,NA)})
  names(weekend_split) = splits
  
  weekday_blocks = as.numeric(na.omit(as.character(unlist(daycare_sched[daycare_sched[,1] == "Weekly daycare",2:6]))))
  weekday_split = lapply(split(x = weekday_blocks,f = 1:num_r_services),function(y){ifelse(length(y) > 0,y,NA)})
  names(weekday_split) = rev(splits)

  service_idx = which(rotating_services == service)
  idx_vec = c(1:num_r_services,1:num_r_services)
  daycare_block_mat_list = sapply(mods, function(mod){
    mod_num = as.numeric(substr(x = mod,start = 4,stop = 4))
    sched_idx = idx_vec[(service_idx + mod_num)]
    
    if(!is.na(weekday_split[[paste0("Split",sched_idx)]])){
      weekday_vec = table(weekday_split[[paste0("Split",sched_idx)]])
      weekday_block_mat = data.frame(blocks_per_week = weekday_vec,
                                     block_length = as.numeric(names(weekday_vec)),
                                     block_length_mins = as.numeric(names(weekday_vec))*60,
                                     block_type = paste0("T",names(weekday_vec),"h"),
                                     weekend = rep(FALSE,length(weekday_vec)))      
    } else{
      weekday_block_mat = data.frame(blocks_per_week = c(),
                                     block_length = c(),
                                     block_length_mins = c(),
                                     block_type = c(),
                                     weekend = c())
    }
    if(!is.na(weekend_split[[paste0("Split",sched_idx)]])){
      weekend_vec = table(weekend_split[[paste0("Split",sched_idx)]])
      weekend_block_mat = data.frame(blocks_per_week = weekend_vec,
                                     block_length = as.numeric(names(weekend_vec)),
                                     block_length_mins = as.numeric(names(weekend_vec))*60,
                                     block_type = paste0("T",names(weekend_vec),"h"),
                                     weekend = rep(TRUE,length(weekend_vec)))      
    } else{
      weekend_block_mat = data.frame(blocks_per_week = c(),
                                     block_length = c(),
                                     block_length_mins = c(),
                                     block_type = c(),
                                     weekend = c())      
    }
    
    
    all_daycare_blocks = data.frame(rbind(weekday_block_mat,weekend_block_mat))
    
    return(all_daycare_blocks)
  })
  
  return(daycare_block_mat_list)
  
}