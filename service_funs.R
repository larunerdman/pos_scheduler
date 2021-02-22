# csv header with no files w/ same ordered columns
# updating waitlist 
# rechecks the waitlist as booking cases

# dealing with high priority cases -> space
# if urgent booking -> maybe just rm from waitlist and proceed with running 


# validation
# data connected ->


# make_multi_perservice_sched
# make_single_service_sched
# make_week -> make_day -> make_block 
  # make_case_list
  # find_bf_open_block
  # add_case
  # add_case_block

# daycare - rotated blocks amongst services with outpatients they want to clear
# 


# specs 
# 

#' Creates service schedule for each service, for each phase block ....
#' @param all_cases, data.frame: processed waitlist 
#' @param phase_block_sched list, list of phase dates with services and dotw
#' @param start_date date to start scheduling (?)
#' @param estimate_remaining_time bool,
#' @param n_weeks_for_estimate ??
#' @param verbose_run bool, verbose level
#' @param turnover_buffer int, account for mvoing patients in and out 
#' @param services list:str services
#' @return 
#' @examples
make_multi_perservice_sched <- function(all_cases, ## waitlist
                                        phase_block_sched,
                                        start_date = as.Date("2020-07-06"),
                                        estimate_remaining_time = TRUE,
                                        n_weeks_for_estimate = 3,
                                        verbose_run = TRUE,
                                        turnover_buffer = 25, ## condition buffer on case type
                                        services = list(
                                          "General", "Otolaryngology", "Neurosurgery",
                                          "Ophthalmology", "Orthopaedics",
                                          "Gynaecology", "Plastics",
                                          "Urology", "Dentistry"
                                        )) {


  # take elements of the named list                                        
  phase_dates <- phase_block_sched[["Phase dates"]]
  all_service_blocks <- phase_block_sched[["Blocks per service"]]

  # for each service, for each block
  all_service_schedules <- lapply(services, function(service) {

    # get all phase blocks for the specified service
    service_block <- lapply(all_service_blocks, function(phase_blocks) {
      phase_blocks[[service]]
    })
    # browser()
    service_cases <- all_cases[grep(pattern = service, x = all_cases$Service), ]
    service_cases <- service_cases[order(service_cases$PCATS, service_cases$InWindow, service_cases$Surgeon), ]

    service_schedule <- make_single_service_sched(
      start_date = start_date,
      weekly_block_mat = service_block,
      phase_dates = phase_dates,
      in_case_list = service_cases,
      turnover_buffer = turnover_buffer
    )
    return(service_schedule)
  })

  return(all_service_schedules)
}

## MAKE SCHEDULE FOR 1 SERVICE

#' Creates service schedule for a service
#' @param turnover_buffer int, number of minutes to add to each case to account for the time to move one person out of the OR and another in
#' @param start_date date, when to start the schedule
#' @param weekly_block_mat ?, formatted block schedule openings (i.e. available times to do surgery)
#' @param phase_dates matrix, phase_dates
#' @param in_case_list, list, cases to add to schedule, all from the same service 
#' @param in_sched ?, if there's an existing schedule you'd like to add to, you can input it here
#' @param verbose bool, verbosity level
#' @param BF bool, whether to use best-fit or first-fit algorithm
#' @return
#' 
#' 
#' 
#' 
# maybe more than one block type in a day 

# 
make_single_service_sched <- function(turnover_buffer = 25, 
                                      start_date = NULL, 
                                      weekly_block_mat = NULL, 
                                      phase_dates = NULL, 
                                      in_case_list = NULL, 
                                      in_sched = NULL, 
                                      verbose = TRUE,
                                      BF = TRUE 
) {
  cases_scheduled <- c()

  if (is.null(start_date)) {
    start_date <- Sys.Date()
  }

  if (is.null(weekly_block_mat)) {
    return("Please enter weekly_block_mat to create schedule")
  } else if (is.null(in_case_list)) {
    return("Please enter list of cases to create scedule")
  } else if (is.null(phase_dates)) {
    return("Please enter phase_dates to create scedule")
  } else {
    if (is.null(in_sched)) {
      ## initialize schedule
      ## makes an empty week to fill up with actual booked cases
      # composed of weeks, 
      ## creates pink sector of slide 3, starts with 1 day
      # 
      sched_list <- list(make_week(
        weekly_block_matrix = weekly_block_mat,
        start_date = start_date,
        phase_dates = phase_dates
      ))
  # if no_blocks, goes in adds a new day to the schedule of the week
    } else { # DS: in this case I assume it would take in an existing schedule?
      sched_list <- in_sched
    }

    in_case_list <- in_case_list[complete.cases(in_case_list), ]

    for (my_row in 1:nrow(in_case_list)) {
      if (length(in_case_list[my_row, "ID"]) == 0) {
        if (verbose) {
          print("Missing row passed")
        }
      } else if (is.na(in_case_list[my_row, "ID"])) {
        if (verbose) {
          print("Missing row passed")
        }
      } else {
        my_case_list <- make_case_list(
          case_id = unlist(in_case_list[my_row, "ID"]),
          case_length = unlist(in_case_list[my_row, "time"]),
          case_priority = in_case_list[my_row, "PCATS"],
          case_surgeon = in_case_list[my_row, "Surgeon"],
          post_op_destination = in_case_list[my_row, "PostOPDest"],
          orig_data = in_case_list[my_row, "orig_list"],
          turnover_buffer = turnover_buffer
        )
        if (verbose) {
          print(paste0("Finding block for Case ID: ", my_case_list$case_id))
        }
        # outputs NA if there is no opening 
        open_block_update <- find_bf_open_block(case_list = my_case_list, block_schedule = sched_list)
        # ^ list w/ openings
        # both functions below will return a booked case 
        if (!is.na(open_block_update[1])) {
          # outputs schedule with the case booked in 
          # the more common ...

          if(verbose){ print("Open block found in schedule, adding case..")}

          sched_list <- add_case(case_list = my_case_list, block_schedule = sched_list, block_update = open_block_update)
        } else {
          # updates the schedule list to have more days 
          # days added to schedule until -> open block that is suitable for the case 
          # also schedules the case 
          # one case per block (orthopaedics is most common for this)
          if(verbose){ print("No open block found in schedule, adding days until block available for case..")}
          sched_list <- add_case_block(case_list = my_case_list, block_schedule = sched_list, service_block_matrix = weekly_block_mat, phase_dates = phase_dates)
        }

        if (verbose) {
          print(paste0("Case ID: ", my_case_list$case_id, " added to schedule."))
        }
        cases_scheduled <- c(cases_scheduled, my_case_list$case_id)
      }
    }

    out_list <- list("sched_list" = sched_list, "cases_scheduled" = cases_scheduled)

    return(out_list)
  }
}

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

#' Creates an empty day
#' @param block_matrix matrix, block times for each day 
#' @param in_date date the day is set on
#' @return list, start date and the start day of the week
#' @examples
make_day <- function(block_matrix, 
                     in_date = NA) { 
  # browser()
  if (is.na(block_matrix)[1]) {
    day_list <- list(
      "date" = in_date,
      "full_day" = FALSE,
      "no_blocks" = TRUE,
      "procedure_blocks" = NULL
    )
  } else {
    #browser()
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
make_week <- function(weekly_block_matrix, 
                      start_date, 
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

  # browser()

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

  return(week_list)
}


## creates a list object for a surgical case
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




## Best-fit algorithm version of find_open_block
find_bf_open_block <- function(case_list, ## list for a given case providing it's ID, length, surgeon, post-op destination, etc
                               block_schedule, ## block schedule list (weeks, days, blocks)
                               block_buffer = 30 ## a time (in minutes) added to blocks to buffer underestimating the block length -- may be too long
) {

  # browser()
  # iterating over each week, for each day, within each day, each block -> is there enough room to put this case inside the block
  # 
  block_index_set <- NA
  max_block_sum <- 0


  for (week in 1:length(block_schedule)) {
    for (day in 1:length(block_schedule[[week]][["Days"]])) {


      no_blocks <- block_schedule[[week]][["Days"]][[day]][["no_blocks"]][[1]]
      
      if (!no_blocks) {

        for (block in 1:length(block_schedule[[week]][["Days"]][[day]][["procedure_blocks"]])) {
          
          block_type <- block_schedule[[week]][["Days"]][[day]][["procedure_blocks"]][[block]]$block_type
          
          if (!is.null(block_type)) {
            browser()
            block_sum <- sum(na.omit(c(case_list$case_length, block_schedule[[week]][["Days"]][[day]][["procedure_blocks"]][[block]]$case_times)))

            # print(block_sum)

            block_time <- as.numeric(substr(block_type, 2, nchar(block_type) - 1)) * 60 + block_buffer

            if ((block_sum < block_time) & (block_sum > max_block_sum)) {
              new_short_long <- update_short_long(proc_len_vec = block_sum, block_time = block_time)
              block_index_set <- list(
                "week" = week,
                "day" = day,
                "block" = block,
                "updated_short_long" = new_short_long
              )
            }
          }
        }
      }
    }
  }

  return(block_index_set) ## returns NA if no available block
}



## function to update whether the block is short or long relative to it's prescribed length
update_short_long <- function(proc_len_vec, ## vector of procedure lengths (in minutes) from block list
                              block_time, ## length of block
                              shortfall_threshold = 90, ## threshold (in minutes) to call block short
                              long_time_threshold = 30 ## threshold (in minutes) to call block long
) {
  block_len <- sum(proc_len_vec)

  # browser()

  if (block_len < (block_time - shortfall_threshold)) {
    short_long <- "short"
  } else if (block_len > (block_time + long_time_threshold)) {
    short_long <- "long"
  } else {
    short_long <- "full_block"
  }
  return(short_long)
}



## Add a case to an open block
add_case <- function(case_list, ## list for a given case providing it's ID, length, surgeon, post-op destination, etc
                     block_schedule, ## block schedule list in which case will be added (weeks, days, blocks)
                     block_update ## list indicating where to add the case in the schedule, output from function: find_bf_open_block
) {
  # browser()
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$case_ids <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$case_ids, case_list$case_id)
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$surgeon <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$surgeon, case_list$case_surgeon)
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$case_times <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$case_times, case_list$case_length)
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$post_op_destination <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$post_op_destination, case_list$`post-op destination`)
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$orig_data <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$orig_data, case_list$orig_data)
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$short_long <- block_update$updated_short_long
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$service <- case_list$case_service


  return(block_schedule)
}


## given a date, find which phase and therefore block schedule is relevant
find_phase_date_block_mat <- function(phase_dates, ## matrix with dates for each phase
                                      service_open_block_list, ## list of block matrices across services
                                      in_date ## date for which you want to find the phase and relevant block matrix
) {
  # browser()
  if (in_date < as.Date(phase_dates[["Phase4"]])) {
    cat("Date provided before beginning of Phase 4")
  } else if (in_date < as.Date(phase_dates[["Phase5"]])) {
    phase_block_mat <- service_open_block_list[["Phase4"]]
  } else if (in_date < as.Date(phase_dates[["Phase6"]])) {
    phase_block_mat <- service_open_block_list[["Phase5"]]
  } else if (in_date >= as.Date(phase_dates[["Phase6"]])) {
    phase_block_mat <- service_open_block_list[["Phase6"]]
  }

  if (toupper(weekdays(in_date)) == "SATURDAY") {
    cat("DATE ON SATURDAY PROVIDED -- no_blocks AVAILABLE")
  } else if (weekdays(in_date) == "SUNDAY") {
    cat("DATE ON SUNDAY PROVIDED -- no_blocks AVAILABLE")
  } else {
    block_mat_out <- phase_block_mat[[toupper(weekdays(in_date))]][[1]]
  }

  return(block_mat_out)
}

add_case_block <- function(case_list, ## list for a given case providing it's ID, length, surgeon, post-op destination, etc
                           block_schedule, ## block schedule list in which case will be added (weeks, days, blocks)
                           service_block_matrix, ## list of matrices providing weekly blocks for service
                           phase_dates ## matrix giving the date for each phase
) {
  #   # days added to schedule until -> open block that is suitable for the case 
  # browser()

  n_weeks <- length(block_schedule)
  n_days <- length(block_schedule[[n_weeks]][["Days"]])

  block_schedule_out <- block_schedule

  # browser()

  if (toupper(names(block_schedule[[n_weeks]][["Days"]])[n_days]) != "FRIDAY") {
    new_date <- block_schedule_out[[n_weeks]][["Days"]][[n_days]][["date"]] + 1
    # n_days = n_days + 1

    day_blocks <- find_phase_date_block_mat(
      phase_dates = phase_dates,
      service_open_block_list = service_block_matrix,
      in_date = new_date
    )

    block_schedule_out[[n_weeks]][["Days"]][[toupper(weekdays(new_date))]] <- make_day(block_matrix = day_blocks, in_date = new_date)

    # if(block_schedule_out[[n_weeks]][["Days"]][[toupper(weekdays(new_date))]][["no_blocks"]]){
    while (block_schedule_out[[n_weeks]][["Days"]][[toupper(weekdays(new_date))]][["no_blocks"]]) {

      # if(case_list$case_id == "39040"){
      #   browser() ## THIS GUY ISN'T FINDING HIS PLACE FOR SOME REASON
      # }

      n_weeks <- length(block_schedule_out)
      n_days <- length(block_schedule_out[[n_weeks]][["Days"]])
      new_date <- block_schedule_out[[n_weeks]][["Days"]][[n_days]][["date"]] + 1

      if (toupper(weekdays(new_date)) != "SATURDAY") {
        day_blocks <- find_phase_date_block_mat(
          phase_dates = phase_dates,
          service_open_block_list = service_block_matrix,
          in_date = new_date
        )
        block_schedule_out[[n_weeks]][["Days"]][[toupper(weekdays(new_date))]] <- make_day(block_matrix = day_blocks, in_date = new_date)
      } else {
        new_week_start <- block_schedule_out[[n_weeks]][["Days"]][["FRIDAY"]][["date"]] + 3 ## WEEKS ALWAYS END ON FRIDAY

        # browser()
        new_week <- make_week(
          weekly_block_matrix = service_block_matrix,
          start_date = new_week_start,
          phase_dates = phase_dates
        )

        block_schedule_out <- append(block_schedule_out, list(new_week))

        new_date <- new_week_start
        n_weeks <- length(block_schedule_out)

        day_blocks <- find_phase_date_block_mat(
          phase_dates = phase_dates,
          service_open_block_list = service_block_matrix,
          in_date = new_date
        )
        n_days <- length(block_schedule_out[[n_weeks]][["Days"]])
        #
        block_schedule_out[[n_weeks]][["Days"]][[toupper(weekdays(new_date))]] <- make_day(block_matrix = day_blocks, in_date = new_date)

        # # if(block_schedule_out[[n_weeks + 1]][["Days"]][[toupper(weekdays(new_date))]][["no_blocks"]]){
        #   while(block_schedule_out[[n_weeks + 1]][["Days"]][[toupper(weekdays(new_date))]][["no_blocks"]]){
        #     n_days = length(block_schedule_out[[n_weeks + 1]][["Days"]])
        #     new_date = block_schedule_out[[n_weeks + 1]][["Days"]][[n_days]][["date"]] + 1
        #     day_blocks = find_phase_date_block_mat(phase_dates = phase_dates,
        #                                            service_open_block_list = service_block_matrix,
        #                                            in_date = new_date)
        #     block_schedule_out[[n_weeks + 1]][["Days"]][[toupper(weekdays(new_date))]] = make_day(block_matrix = day_blocks, in_date = new_date)
        #   }
        # # }
      }
    }
    # }

    short_long_update <- update_short_long(proc_len_vec = case_list$case_length, block_time = day_blocks$block_length_mins[1])
    n_weeks <- length(block_schedule_out)
    n_days <- length(block_schedule_out[[n_weeks]][["Days"]])

    block_update_list <- list(
      "week" = n_weeks,
      "day" = n_days,
      "block" = 1,
      "updated_short_long" = short_long_update
    )
    updated_block_sched <- add_case(
      case_list = case_list,
      block_schedule = block_schedule_out,
      block_update = block_update_list
    )
  } else {

    ## ADD WEEK
    ## FIGURE OUT PHASE FOR THE NEXT WEEK -- DONE AUTOMATICALLY IN "MAKE WEEK"
    ## COMPUTE START DAY FOR THE NEXT WEEK
    ## FIND FIRST DAY THAT HAS BLOCKS, MAKE EMPTY DAYS UP TO THAT POINT
    ##

    new_week_start <- block_schedule_out[[n_weeks]][["Days"]][["FRIDAY"]][["date"]] + 3 ## WEEKS ALWAYS END ON FRIDAY

    # browser()
    new_week <- make_week(
      weekly_block_matrix = service_block_matrix,
      start_date = new_week_start,
      phase_dates = phase_dates
    )

    block_schedule_out <- append(block_schedule, list(new_week))

    new_date <- new_week_start
    day_blocks <- find_phase_date_block_mat(
      phase_dates = phase_dates,
      service_open_block_list = service_block_matrix,
      in_date = new_date
    )
    # n_days = length(block_schedule_out[[n_weeks + 1]][["Days"]])

    # if(block_schedule_out[[n_weeks + 1]][["Days"]][[toupper(weekdays(new_date))]][["no_blocks"]]){
    while (block_schedule_out[[n_weeks + 1]][["Days"]][[toupper(weekdays(new_date))]][["no_blocks"]]) {
      n_days <- length(block_schedule_out[[n_weeks + 1]][["Days"]])
      new_date <- block_schedule_out[[n_weeks + 1]][["Days"]][[n_days]][["date"]] + 1
      day_blocks <- find_phase_date_block_mat(
        phase_dates = phase_dates,
        service_open_block_list = service_block_matrix,
        in_date = new_date
      )
      block_schedule_out[[n_weeks + 1]][["Days"]][[toupper(weekdays(new_date))]] <- make_day(block_matrix = day_blocks, in_date = new_date)
    }
    # }


    short_long_update <- update_short_long(proc_len_vec = case_list$case_length, block_time = day_blocks$block_length_mins[1])

    n_weeks <- length(block_schedule_out)
    n_days <- length(block_schedule_out[[n_weeks]][["Days"]])

    block_update_list <- list(
      "week" = n_weeks,
      "day" = n_days,
      "block" = 1,
      "updated_short_long" = short_long_update
    )
    updated_block_sched <- add_case(
      case_list = case_list,
      block_schedule = block_schedule_out,
      block_update = block_update_list
    )
  }

  return(updated_block_sched)
}



