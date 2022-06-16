
## Within service scheduling functions..


## creates empty block
make_block <- function(block_type, ## the number of hours the block takes + "hr"
                       daycare, ## T/F if the block is a daycare (rotating out-patient) block -- currently not in use in the model
                       weekend # T/F if the block is a weekend block -- currently not in use in the model
) {
  block_list <- list(
    "block_type" = block_type, ## the number of hours the block takes + "hr"
    "short_long" = NA, ## initialize as missing but after compute if the block is way over or under time
    "daycare_only" = daycare, ## "daycare" refers to a block which rotates between services and often is only out-patient surgeries, currently the model doesn't account for these
    "weekend" = weekend, ## if the block is on the weekend, currently I build my week excluding weekends
    "surgeon" = c(), ## initializing an empty vector which will hold the surgeons for each case, ultimately we may want to fix it to only allow the same surgeon within a block
    "case_ids" = c(), ## init empty vector to list case ID numbers in
    "case_times" = c(), ## init empty vector to list length of case in minutes
    "post_op_destination" = c(), ## init empty vector to list post-op location (ultimately we'll want to include a predicted length of stay for this parameter)
    "orig_data" = c()
  ) ## this was put here to indicate if the case is original or if it was simulated (in the case where we add random data to simulate the reality of cases being added to the waitlist)
  return(block_list)
}

#' Creates a day
#' @param block_matrix matrix, block times for each day
#' @param in_date date the day is set on
#' @return list, start date and the start day of the week
#' @examples
make_day <- function(block_matrix,
                     in_date = NA) {
  if (is.na(block_matrix)[1]) {
    day_list <- list(
      "date" = in_date,
      "full_day" = FALSE,
      "no_blocks" = TRUE,
      "procedure_blocks" = NULL
    )
  } else {
    procedure_blocks <- list()
    i <- 1
    for (row in 1:nrow(block_matrix)) {
      for (i_block in 1:block_matrix$blocks_per_week[row]) {
        procedure_blocks[[i]] <- make_block(block_matrix$block_type[row],
          daycare = block_matrix$daycare[row],
          weekend = block_matrix$weekend[row]
        )
        i <- i + 1
      }
    }

    icu_beds <- list(
      "case_ids" = c(),
      "service" = c()
    )

    day_list <- list(
      "date" = in_date,
      "full_day" = FALSE,
      "no_blocks" = FALSE,
      "procedure_blocks" = procedure_blocks,
      "icu_beds" = icu_beds
    )
  }
  return(day_list)
}

#' Creates an empty week
#' @param weekly_block_matrix list, of weekly block matrices, named by phase
#' @param start_date date the week starts on
#' @param phase_dates matrix, listing the dates over which each phase is relevant
#' @return list, start date and the start day of the week
#' @examples
#' # ADD WEEK NUM - week number is based off of month
make_week <- function(weekly_block_matrix,
                      start_date,
                      schedule_start_date, # DEPRECATED
                      # week_num,
                      phase_dates) {

  # get the phase which is the greatest relative to the given start date
  start_phase <- names(phase_dates)[max(which(unlist(phase_dates[1, ]) > start_date))]

  phase_block_mat <- weekly_block_matrix[[start_phase]]

  day_of_week_start <- toupper(weekdays(start_date))

  if (day_of_week_start == "SATURDAY") {
    week_start_date <- start_date + 2
    day_of_week_start <- toupper(weekdays(week_start_date))
  } else if (day_of_week_start == "SUNDAY") {
    week_start_date <- start_date + 1
    day_of_week_start <- toupper(weekdays(week_start_date))
  } else {
    week_start_date <- start_date
  }

  day_phase_block <- purrr::pluck(phase_block_mat[[grep(day_of_week_start, names(phase_block_mat))]], 1)

  week_num <- get_week_from_date(start_date)
  cat("\nWeek_num", week_num, "\n")

  if (!is.na(day_phase_block)[[1]]) { # DS: CHECK THIS this code is repeated in find_phase_date_block_mat..
    day_phase_block <- get_relevant_blocks(day_phase_block, week_num = week_num)
  }

  # if (length(relevant_week_found) > 2 & !all(relevant_week_found)){
  #   browser()
  # }
  # if(any(relevant_week_found)){browser()}

  daycare <- phase_block_mat[["Daycare"]] # doesn't look used rn

  if (is.na(day_phase_block[1])) {
    week_list <- list(
      "Start date" = week_start_date,
      "Days" = list("MONDAY" = make_day(block_matrix = NA, in_date = week_start_date))
    )
  } else {
    week_list <- list(
      "Start date" = week_start_date,
      "Days" = list("MONDAY" = make_day(block_matrix = day_phase_block, in_date = week_start_date))
    )
  }
  # browser()
  return(week_list)
}


## creates a list object for a surgical case
# this could be deprecated
make_case_list <- function(case_id, ## case value
                           case_length, ## length of case in minutes
                           case_priority, ## PCATS (or other priority score if this is changed in the future)
                           orig_data, ## T/F if the case was sampled in simulation (F) and T if the case is from the original waitlist
                           post_op_destination,
                           case_surgeon,
                           turnover_buffer ## a length in time in minutes to account for one case leaving and another case entering the same OR
) {
  case_list <- list(
    "case_id" = case_id,
    "case_length" = case_length + turnover_buffer,
    "case_priority" = case_priority,
    "case_surgeon" = case_surgeon,
    "post-op destination" = post_op_destination,
    "orig_data" = orig_data
  )

  return(case_list)
}


#' Best-fit algorithm version of find_open_block. This algorithm will pick the block that is filled the most (max_block_sum), where there are multiple blocks the case can be scheduled into - the algorithm will choose the one
#' to ensure the block takes up the maximum amount of time. More specifically, the case will be scheduled into the first block where it fits, but as it continues to loop
#' through the blocks, if there is a block where scheduling the case results in a larger block sum, but under the max, it will be scheduled there instead. This is doen to ensure the block takes up as much time as possible.
#' @param case_list list, a case's ID, length, surgeon, post-op destination, etc
#' @param block_schedule list, block schedule list in which case will be added (weeks, days, blocks)
#' @param block_buffer int, a time (in minutes) added to blocks to buffer underestimating the block length -- may be too long
#' @param surg_opts
#' @param unit_opts
#' @param pacu_opts
#' @return block_index_set NA or list of week, day, block, updated_short_long, current_max_block_sum, block_sum, and block_time
#' @examples
find_bf_open_block <- function(case_list,
                               surg_opts,
                               unit_opts,
                               pacu_opts,
                               service,
                               #  block_schedule, # currently service specific schedule!!!, instead, pass in the service.
                               block_buffer = 30,
                               all_service_schedule,
                               surg_sched,
                               block_schedule = NULL) {
  require(stringr)

  # iterating over each week, for each day, within each day, each block -> is there enough room to put this case inside the block
  block_index_set <- NA
  max_block_sum <- 0

  # some logic to assign new block schedule for use in the re-factored make_sched_all_services, otherwise comparison script will break
  if (is.null(block_schedule)) block_schedule <- all_service_schedule[[service]]
  # block schedule should be the service specific schedule

  current_post_op_destination <- case_list$`post-op destination`

  # if (case_list$case_id == 99483) browser() # not working as intended for 5B?

  for (week in 1:length(block_schedule)) {
    for (day in 1:length(block_schedule[[week]][["Days"]])) {
      # THIS GETS CONFUSING SINCE IT'S A NEGATIVE -> NO BLOCKS -> TRUE
      no_blocks <- block_schedule[[week]][["Days"]][[day]][["no_blocks"]][[1]]

      if (!no_blocks) { # if blocks are available
        # cat(paste0("Week: ", week, " Day: ", day, "\n"))
        # ---- logic for unit and daily constraints ----
        # if (case_list$case_id == 101207) browser() # blocks exist but no current cases
        if (!stringr::str_detect(stringr::str_to_lower(current_post_op_destination), "home")) {
          failed_check <- check_target_constraints(current_post_op_destination, unit_opts, pacu_targets =  pacu_opts, week, day, all_service_schedule)
          if (failed_check) next
        }
        # ---- end logic for unit and daily constraints ----

        # ---- logic for surgeon schedule -----

        if (!is.null(surg_sched)) {
          # check if surgeon is specified at all in surgeon schedule
          if (!case_list$case_surgeon %in% c(surg_sched %>% reduce(c))) {
            stop(paste0("Please check that ", case_list$case_surgeon, " is specified in the surgeon schedule."))
          }

          current_dow <- weekdays(purrr::pluck(.x = block_schedule, week, "Days", day, "date"))
          
          # if current surgeon not in day, then go to next day
          if (!case_list$case_surgeon %in% surg_sched[[current_dow]]) {
            next
          }
        }

        for (block in 1:length(block_schedule[[week]][["Days"]][[day]][["procedure_blocks"]])) {
          block_type <- block_schedule[[week]][["Days"]][[day]][["procedure_blocks"]][[block]]$block_type

          if (!is.null(block_type)) { # if there is block availability

            current_surgeons <- purrr::pluck(
              .x = block_schedule, week, "Days",
              day, "procedure_blocks", block, "surgeon"
            ) %>% unique()

            if (surg_opts == "first") {
              if (!is.null(current_surgeons)) { # if there is a surgeon assigned, check if there is more than one
                if (length(current_surgeons) != 1) {
                  stop("Surgeon Options = 'first' but there is more than one unique surgeon for the block...")
                }
                if ((case_list$case_surgeon != current_surgeons)) {
                  # cat("Valid block found for current case but surgeon constraint in effect, skipping block\n")
                  next # goes to next block
                }
              }
            }

            block_sum <- sum(na.omit(c(
              case_list$case_length,
              block_schedule[[week]][["Days"]][[day]][["procedure_blocks"]][[block]]$case_times
            )))
            # get block in hours
            block_time <- readr::parse_number(block_type) * 60 + block_buffer

            # cat("\nblock_buffer\n")
            # cat(readr::parse_number(block_type) )
            # 
            # cat("\nblock_type\n")
            # cat(block_type)
            # 
            # cat("\nblock_time\n")
            # cat(block_time)
            # 
            # cat("\nblock_sum\n")
            # cat(block_sum)


            # if the current block sum is less than the alotted block time
            if ((block_sum < block_time) & (block_sum > max_block_sum)) {
              new_short_long <- update_short_long(proc_len_vec = block_sum, block_time = block_time)
              block_index_set <- list(
                "week" = week,
                "day" = day,
                "block" = block,
                "updated_short_long" = new_short_long,
                "current_max_block_sum" = max_block_sum,
                "block_sum" = block_sum,
                "block_time" = block_time
              )
              max_block_sum <- block_sum
            }
          }
        }
      }
    }
  }


  return(block_index_set) ## returns NA if no available block
}



#' update whether the block is short or long relative to it's prescribed length
#' @param proc_len_vec vector: int, procedure lengths (in minutes) from block list matrix, block times for each day
#' @param block_time int, duration of the block (in minutes) + block buffer
#' @param shortfall_threshold int, threshold (in minutes) to call block short
#' @param long_time_threshold int, threshold to call block long
#' @return str, whether the block is short, long, or takes up the entire block relative to the block as is.
#' @examples
update_short_long <- function(proc_len_vec,
                              block_time,
                              shortfall_threshold = 90,
                              long_time_threshold = 30) {
  block_len <- sum(proc_len_vec)
  
  # browser()

  # cat("\nblock_len\n")
  # cat(block_len)
  # cat("\nblock_time\n")
  # cat(block_time)
  # cat("\nshortfall_threshold\n")
  # cat(shortfall_threshold)
  
  
  
  # if (block_len < (block_time - shortfall_threshold)) {
  #   short_long <- "short"
  # } else if (block_len > (block_time + long_time_threshold)) {
  #   short_long <- "long"
  # } else {
  #   short_long <- "full_block"
  # }
  # return(short_long)
  return(NA)
}


#' Add a case to an open block, applies only when there is an available block. Otherwise add_case_block is invoked
#' @param case_list list, a case's ID, length, surgeon, post-op destination, etc
#' @param block_schedule list, block schedule list in which case will be added (weeks, days, blocks)
#' @param block_update list,  where to add the case in the schedule, output from function: find_bf_open_block
#' @return block_schedule, updated block schedule list
#' @examples
add_case <- function(case_list,
                     block_schedule,
                     block_update) {
  # browser()
  # names are the elements for block schedule, elements are the case_list names
  names_list <- c(
    "case_ids" = "case_id",
    "surgeon" = "case_surgeon",
    "case_times" = "case_length",
    "post_op_destination" = "post-op destination",
    "orig_data" = "orig_data",
    "short_long" = "updated_short_long"
    # "service" = "case_service" @DS: Lauren -> not currently used?
  )
  require(purrr)

  # og_block_schedule <- block_schedule
  block_schedule2 <- block_schedule

  exist_case <- purrr::pluck(
    .x = block_schedule2,
    block_update$week, "Days", block_update$day, "procedure_blocks", block_update$block, "surgeon"
  ) %>%
    length()

  for (i in 1:length(names_list)) {
    this_field <- names(names_list)[[i]]

    # print(paste0('Field: ', this_field))

    # print(paste0('Pre-update: ', purrr::pluck(.x = block_schedule2,
    #               block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list
    #               this_field)))

    if (this_field != "short_long") {
      existing_elements <- purrr::pluck(
        .x = block_schedule2,
        block_update$week, "Days", block_update$day, "procedure_blocks", block_update$block, # accessing the list
        this_field
      )

      # print(paste0('Existing: ', existing_elements))
      # print(paste0('Case List: ', case_list[[names_list[this_field]]]))

      to_append <- c(existing_elements, case_list[[names_list[this_field]]])
      # print(paste0('Existing + Case List: ', to_append))

      pluck(
        .x = block_schedule2,
        block_update$week, "Days", block_update$day, "procedure_blocks", block_update$block, # accessing the list
        this_field
      ) <- to_append
    } else {
      # if updating short_long, we don't need the existing values.


      to_append <- c(block_update[[names_list[this_field]]])
      # print(paste0('Existing + Case List: ', to_append))

      pluck(
        .x = block_schedule2,
        block_update$week, "Days", block_update$day, "procedure_blocks", block_update$block, # accessing the list
        this_field
      ) <- to_append
    }

    # print(paste0('Post-update: ', purrr::pluck(.x = block_schedule,
    #               block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list
    #               this_field)))
  }

  #   block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$case_ids <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$case_ids, case_list$case_id)
  #   block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$surgeon <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$surgeon, case_list$case_surgeon)
  #   block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$case_times <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$case_times, case_list$case_length)
  #   block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$post_op_destination <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$post_op_destination, case_list$`post-op destination`)
  #   block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$orig_data <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$orig_data, case_list$orig_data)
  #   block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$short_long <- block_update$updated_short_long
  #   block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$service <- case_list$case_service

  #  # browser()
  #   stopifnot(all.equal(
  #     block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]],

  #             purrr::pluck(.x = block_schedule2,
  #                       block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block)

  #                       ))

  return(block_schedule2)
}


#' given a date, find which phase and therefore block schedule is relevant
#' @param phase_dates, str, list of phase dates
#' @param service_open_block_list, list,  matrices of weekly blocks for service
#' @param in_date str, date for which we want to find the phase and relevant block matrix
#' @return NA, or list of block schedule (data.frame), with blocks_per_week, block_length, block_length_mins, block_type, weekend, daycare
#' @examples
#'
#' # find block schedule for the week (by checking the phase the input date belongs in)
find_phase_date_block_mat <- function(phase_dates,
                                      service_open_block_list, ## list of block matrices across services
                                      in_date ## date for which you want to find the phase and relevant block matrix
) {
  if (in_date < as.Date(phase_dates[["Phase4"]])) {
    cat("Date provided before beginning of Phase 4\n")
  } else if (in_date < as.Date(phase_dates[["Phase5"]])) {
    phase_block_mat <- service_open_block_list[["Phase4"]]
    cat("Phase 4\n")
  } else if (in_date < as.Date(phase_dates[["Phase6"]])) {
    phase_block_mat <- service_open_block_list[["Phase5"]]
    cat("Phase 5\n")
  } else if (in_date >= as.Date(phase_dates[["Phase6"]])) {
    phase_block_mat <- service_open_block_list[["Phase6"]]
    cat("Phase 6\n")
  }

  # print(phase_block_mat)

  dow <- toupper(weekdays(in_date))

  if (toupper(weekdays(in_date)) %in% c("SATURDAY", "SUNDAY")) {
    cat("DATE ON WEEKEND PROVIDED -- no_blocks AVAILABLE\n")
    block_mat_out <- NA
  } else {
    block_mat_out <- phase_block_mat[[toupper(weekdays(in_date))]][[1]]
  }

  # if (!is.na(block_mat_out)){
  #     if(nrow(block_mat_out)> 1){
  #         if(block_mat_out$relevant_weeks[[2]]  != 'all'){
  #             browser()
  #             }
  #     }
  # }
  week_num <- get_week_from_date(in_date)

  if (!is.na(block_mat_out)[1]) { # DS: CHECK THIS
    block_mat_out <- get_relevant_blocks(block_mat_out, week_num = week_num)
  }

  return(block_mat_out)
}

next_weekday <- function(date, wday) {
  # get next weekday for add_case_block_refactor
  # https://stackoverflow.com/questions/32434549/how-to-find-next-particular-day
  # Sunday = 1, Saturday = 7
  date <- as.Date(date)
  diff <- wday - wday(date)
  if (diff < 0) {
    diff <- diff + 7
  }
  return(date + diff)
}

get_week_from_date <- function(dates) {
  # used to pass into get_relevant_blocks, calculate current week of the month
  print(dates)
  require(lubridate)
  # https://stackoverflow.com/questions/25199851/r-how-to-get-the-week-number-of-the-month
  week_num <- (5 + day(dates) + wday(floor_date(dates, "mo
           nth"))) %/% 7

  return(week_num)
}

get_relevant_blocks <- function(pertinent_block, week_num) {
  # used in make_week and find_phase_date_block to subset a schedule with week based blocks

  print(paste0("Week Number: ", week_num))
  # print(as_tibble(pertinent_block))

  relevant_week_found <- stringr::str_detect(pertinent_block$relevant_weeks, as.character(week_num))
  all_weeks <- pertinent_block$relevant_weeks == "all"

  # if (length(relevant_week_found) > 1 & any(relevant_week_found)){
  #   browser()
  # }

  if (any(relevant_week_found) | any(all_weeks)) {
    # cat('relevant week found, subsetting day phase block with all\n')
    new_pertinent_block <- rbind(pertinent_block[relevant_week_found, ], pertinent_block[all_weeks, ])
  } else {
    # browser()
    new_pertinent_block <- NA
    # cat('no relevant weeks found at all!\n')
  }
  # print(as_tibble(new_pertinent_block))
  return(new_pertinent_block)
}


#' Days added to schedule until open block that is suitable for the case is available, then adds the case
#' @param case_list list, a case's ID, length, surgeon, post-op destination, etc
#' @param block_schedule list, block schedule list in which case will be added (weeks, days, blocks)
#' @param service_block_matrix list,  matrices of weekly blocks for service
#' @param phase_dates matrix of dates for each phase
#' @param all_service_schedule list of all services
#' @return block_schedule, updated block schedule list
#' @examples

add_case_block_refactor <- function(case_list, block_schedule, service_block_matrix, phase_dates, surg_sched,
                                    all_service_schedule, service, unit_opts,
                                    pacu_opts) {
  
  current_post_op_destination <- case_list$`post-op destination`
  
  # get last week and day to subset list from (check w/ Lauren)
  n_weeks <- length(block_schedule)
  n_days <- length(purrr::pluck(block_schedule, n_weeks, "Days"))
  block_schedule_out <- block_schedule # make a copy to update.

  new_date <- block_schedule_out %>% pluck(n_weeks, "Days", n_days, "date") + 1
  next_available_day <- toupper(weekdays(new_date))

  day_blocks <- find_phase_date_block_mat(
    phase_dates = phase_dates,
    service_open_block_list = service_block_matrix,
    in_date = new_date
  )

  block_schedule_out[[n_weeks]][["Days"]][[next_available_day]] <- make_day(block_matrix = day_blocks, in_date = new_date)
  

  # if not supplied, we default to FALSE to give the loop an opportunity to exit (we will never break out otherwise), otherwise we set to TRUE
  # if (is.null(surg_sched)) {
  #   surgeon_flag <- FALSE
  # } else {
  #   surgeon_flag <- TRUE
  # }
  # 
  surgeon_flag <- if(is.null(surg_sched)) FALSE else TRUE
  
  # we assume the day is full from the get go and leave it to check_target_constraints() to determine whether it is full. otherwise, there are 
  # edge cases where if blocks are available (no_blocks == TRUE), and the day is in fact full, the case will be scheduled
  unit_flag <- TRUE
  
  # for the while loop to exit, all elements of the vector need to be FALSE
  while (block_schedule_out[[n_weeks]][["Days"]][[next_available_day]][["no_blocks"]] || unit_flag || surgeon_flag) {
    # reset params for each day, unit_flag = FALSE means we are under capacity, otherwise the flag will carry over from the previous day
    unit_flag <- FALSE
    
    surgeon_flag <- if(is.null(surg_sched)) FALSE else TRUE

    
    n_weeks <- length(block_schedule_out)
    n_days <- length(purrr::pluck(block_schedule_out, n_weeks, "Days"))
    new_date <- block_schedule_out %>% pluck(n_weeks, "Days", n_days, "date") + 1
    next_available_day <- toupper(weekdays(new_date))

    # assuming we are in Friday
    if (next_available_day %in% c("SATURDAY", "SUNDAY")) { 
      new_week_start <- pluck(block_schedule_out, n_weeks, "Days", n_days, "date") %>% next_weekday(., 2) # get date of next Monday

      new_week <- make_week(
        weekly_block_matrix = service_block_matrix,
        start_date = new_week_start,
        schedule_start_date = block_schedule[[1]]$`Start date`,
        phase_dates = phase_dates
      )

      block_schedule_out <- append(block_schedule_out, list(new_week))

      # update date for finding phase availability and for block creation
      new_date <- new_week_start
      next_available_day <- toupper(weekdays(new_date))
      n_weeks <- length(block_schedule_out)
      n_days <- 0 # Sunday, +1 later below #### n_days + 2 # for the unit constraint fun
    }
    
    day_blocks <- find_phase_date_block_mat(
      phase_dates = phase_dates, # schedule_start_date =  block_schedule[[1]]$`Start date` ,
      service_open_block_list = service_block_matrix, in_date = new_date
    )
    
    block_schedule_out[[n_weeks]][["Days"]][[next_available_day]] <- make_day(block_matrix = day_blocks, in_date = new_date)

    # by default, unit_flag is set to FALSE (to 'pass') since it's assumed capacities are under by default..
    # if check_target_constraints FAILS, then it will return TRUE causing the while loop to continue until it is FALSE
    if (!stringr::str_detect(stringr::str_to_lower(current_post_op_destination), "home")) {
      unit_flag <- (check_target_constraints(current_post_op_destination, unit_opts, pacu_opts, week = n_weeks, day = n_days +1, all_service_schedule))
      # if (unit_flag) browser()
    }

    # if current surgeon not in day, then go to next day
    
    if (!is.null(surg_sched)) {
      # check if surgeon is specified at all in surgeon schedule
      if (!case_list$case_surgeon %in% c(surg_sched %>% reduce(c))) {
        stop(paste0("Please check that ", case_list$case_surgeon, " is specified in the surgeon schedule."))
      }
      
      current_dow <- weekdays(new_date)
      if (case_list$case_surgeon %in% surg_sched[[current_dow]]) {
        surgeon_flag <- FALSE
        # browser()
      }
    }
    
    
    # (block_schedule_out[[n_weeks]][["Days"]][[next_available_day]][["no_blocks"]] && constraint_flag)
    # 
    # all(c(TRUE, TRUE)) # TRUE
    # all(c(TRUE, FALSE)) # identical to all(c(FALSE, TRUE)), FALSE
    # all(c(FALSE, FALSE)) # FALSE
    # 
    # !all(c(TRUE, TRUE)) # FALSE
    # !all(c(TRUE, FALSE)) # identical to !all(c(FALSE, TRUE)), TRUE
    # !all(c(FALSE, FALSE)) # TRUE
    # 
    # c(TRUE || TRUE) # TRUE
    # c(TRUE || FALSE) # TRUE 
    # c(FALSE || FALSE) # FALSE
    
    # want it to evaluate FALSE if and ONLY when both conditions are FALSE< otherwise return TRUE
  }
  if (unit_flag == TRUE || surgeon_flag == TRUE){
    stop('Something went terribly wrong. Unit or surgeon flag is somehow TRUE outside of while loop ')
  }
  # browser()
  # if(case_list$case_id == 98834){browser()}
  # browser()
  # the while statement must continue until BOTH no_blocks is FALSE and constraint_flag is FALSE. in other words, there are blocks and the next dow the surgeon can be slotted in.
  # no_blocks can be FALSE (when a suitable day block is found and created through find_phase_date_block_mat + make_day, 
  # but the while loop does not exit because constraint_flag is still TRUE
  # what happens now?
  # in this case, days are continued to be added until a suitable day for the surgeon is found (where constraint_flag will be set to FALSE)
  
  # right now, day_blocks ends up being NA meaning there is no $block_length_mins attribute - breaking update_short_long()
  # previously, day_blocks would be created AND [["no_blocks"]] would be true
  # if(block_schedule_out[[n_weeks]][["Days"]][[next_available_day]][["no_blocks"]] == FALSE  && constraint_flag == FALSE && is.na(day_blocks[[1]])) browser()
  
  
  # BLOCKS TO EXIST ( FALSE), SURGEON TO PASS (FALSE), UNIT CHECK TO PASS ( FALSE)
  short_long_update <- update_short_long(proc_len_vec = case_list$case_length, block_time = day_blocks$block_length_mins[1])

  n_weeks <- length(block_schedule_out)
  n_days <- length(block_schedule_out[[n_weeks]][["Days"]])

  # create block_index_list, what find_bf_open_block would have returned IF there was immediate availability
  block_update_list <- list(
    "week" = n_weeks,
    "day" = n_days,
    "block" = 1,
    "updated_short_long" = short_long_update # for reference
  )

  return(list(
    "block_update_list" = block_update_list,
    "block_schedule_out" = block_schedule_out
  ))
}
