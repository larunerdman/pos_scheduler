
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
#' 
#' # ADD WEEK NUM - week number is based off of month 
make_week <- function(weekly_block_matrix, 
                      start_date, 
                      schedule_start_date,  # DEPRECATED
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
  cat('\nWeek_num', week_num, '\n')
  day_phase_block <- get_relevant_blocks(day_phase_block, week_num = week_num)

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
#' through the blocks, if there is a block where scheduling the case results in a larger block sum it will be scheduled there instead. This is doen to ensure the block takes up as much time as possible.
#' @param case_list list, a case's ID, length, surgeon, post-op destination, etc
#' @param block_schedule list, block schedule list in which case will be added (weeks, days, blocks)
#' @param block_buffer int, a time (in minutes) added to blocks to buffer underestimating the block length -- may be too long
#' @return block_index_set NA or list of week, day, block, updated_short_long, current_max_block_sum, block_sum, and block_time
#' @examples
find_bf_open_block <- function(case_list, 
                               surg_opts,
                               block_schedule, 
                               block_buffer = 30
) {

  # find_bf_open_block <- constraints, surgeon 


  # iterating over each week, for each day, within each day, each block -> is there enough room to put this case inside the block
  block_index_set <-block_index_set2 <- NA
  max_block_sum <- 0

  for (week in 1:length(block_schedule)) 
  {

    for (day in 1:length(block_schedule[[week]][["Days"]])) {

      no_blocks <- block_schedule[[week]][["Days"]][[day]][["no_blocks"]][[1]]
      if (!no_blocks) { # if blocks are available

        for (block in 1:length(block_schedule[[week]][["Days"]][[day]][["procedure_blocks"]])) {
          
          block_type <- block_schedule[[week]][["Days"]][[day]][["procedure_blocks"]][[block]]$block_type
          
          if (!is.null(block_type)) { # if there is block availability

            # browser()

 
            current_surgeons =  purrr::pluck(.x = block_schedule, week, 'Days', day, 'procedure_blocks', block, 'surgeon') %>% unique()
            if (surg_opts == 'first'){
              if (!is.null(current_surgeons)){ # if there is a surgeon assigned, check if there is more than one
                # browser()
                if (length(current_surgeons) != 1){
                  stop("Surgeon Options = 'first' but there is more than one unique surgeon for the block...")
                }
                if ((case_list$case_surgeon != current_surgeons)){
                    # cat('Valid block found for current case but surgeon constraint in effect, skipping block\n')
                    next
                }
              }
            }

            block_sum <- sum(na.omit(c(case_list$case_length, 
                                       block_schedule[[week]][["Days"]][[day]][["procedure_blocks"]][[block]]$case_times)))

            # get block in hours
            block_time <- readr::parse_number(block_type) * 60 + block_buffer

            # if the current block sum is less than the alotted block time
            if ((block_sum < block_time) & (block_sum > max_block_sum)) {
              
              new_short_long <- update_short_long(proc_len_vec = block_sum, block_time = block_time)
              block_index_set <- list(
                "week" = week,
                "day" = day,
                "block" = block,
                "updated_short_long" = new_short_long,
                'current_max_block_sum' = max_block_sum,
                'block_sum' = block_sum,
                'block_time' = block_time
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
                              long_time_threshold = 30 
) {
  block_len <- sum(proc_len_vec)

  if (block_len < (block_time - shortfall_threshold)) {
    short_long <- "short"
  } else if (block_len > (block_time + long_time_threshold)) {
    short_long <- "long"
  } else {
    short_long <- "full_block"
  }
  return(short_long)
}


#' Add a case to an open block, applies only when there is an available block. Otherwise add_case_block is invoked
#' @param case_list list, a case's ID, length, surgeon, post-op destination, etc
#' @param block_schedule list, block schedule list in which case will be added (weeks, days, blocks)
#' @param block_update list,  where to add the case in the schedule, output from function: find_bf_open_block
#' @return block_schedule, updated block schedule list
#' @examples
add_case <- function(case_list, 
                     block_schedule, 
                     block_update
) {
  # browser()
  # names are the elements for block schedule, elements are the case_list names
  names_list <- c(
    "case_ids" = "case_id",
    "surgeon" = "case_surgeon",
    "case_times" = "case_length",
    "post_op_destination" = "post-op destination",
    "orig_data" = "orig_data",
    "short_long" = "updated_short_long"
    #"service" = "case_service" @DS: Lauren -> not currently used?
  )
  require(purrr)


og_block_schedule <- block_schedule
  block_schedule2 <- block_schedule

exist_case <- purrr::pluck(.x = block_schedule2, 
                      block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, 'surgeon') %>%
                      length()
# if (exist_case == 1) {browser()}

# map/lapply returns a list, but if we are iterating through n names then we are also creating n schedules. for loop should be okay here since 
# there is not too many fields to go through..


for (i in 1:length(names_list)){

        this_field <- names(names_list)[[i]]

        # print(paste0('Field: ', this_field))

        # print(paste0('Pre-update: ', purrr::pluck(.x = block_schedule2, 
        #               block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
        #               this_field)))

        if (this_field != "short_long"){

          existing_elements <- purrr::pluck(.x = block_schedule2, 
                block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
                this_field) 

          # print(paste0('Existing: ', existing_elements))
          # print(paste0('Case List: ', case_list[[names_list[this_field]]]))

          to_append <- c(existing_elements, case_list[[names_list[this_field]]] )
          # print(paste0('Existing + Case List: ', to_append))

          pluck(.x = block_schedule2, 
                      block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
                      this_field) <- to_append

        } else{
          # if updating short_long, we don't need the existing values. 


          to_append <- c(block_update[[names_list[this_field]]] )
          # print(paste0('Existing + Case List: ', to_append))

          pluck(.x = block_schedule2, 
                      block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
                      this_field) <- to_append
    


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

# find block schedule for the week (by checking the phase the input date belongs in)

find_phase_date_block_mat <- function(phase_dates, 
                                      service_open_block_list, ## list of block matrices across services
                                      # schedule_start_date,
                                      in_date ## date for which you want to find the phase and relevant block matrix
) {
  # browser()

  if (in_date < as.Date(phase_dates[["Phase4"]])) {
    cat("Date provided before beginning of Phase 4\n")
  } else if (in_date < as.Date(phase_dates[["Phase5"]])) {
    phase_block_mat <- service_open_block_list[["Phase4"]]
    cat('Phase 4\n')
  } else if (in_date < as.Date(phase_dates[["Phase6"]])) {
    phase_block_mat <- service_open_block_list[["Phase5"]]
    cat('Phase 5\n')
  } else if (in_date >= as.Date(phase_dates[["Phase6"]])) {
    phase_block_mat <- service_open_block_list[["Phase6"]]
    cat('Phase 6\n')
  }

  # print(phase_block_mat)

  dow = toupper(weekdays(in_date))

  if (toupper(weekdays(in_date)) == "SATURDAY") {
    cat("DATE ON SATURDAY PROVIDED -- no_blocks AVAILABLE\n")
    block_mat_out <- NA
  } else if (weekdays(in_date) == "SUNDAY") {
    cat("DATE ON SUNDAY PROVIDED -- no_blocks AVAILABLE\n")
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
  # if (week_num == 3){browser()}
  if(!is.na(block_mat_out)){
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
  if( diff < 0 )
    diff <- diff + 7
  return(date + diff)
}

get_week_from_date <- function(dates){
  # used to pass into get_relevant_blocks, calculate current week of the month
  print(dates)
  require(lubridate)
  #https://stackoverflow.com/questions/25199851/r-how-to-get-the-week-number-of-the-month
  week_num <- (5 + day(dates) + wday(floor_date(dates, 'mo
           nth'))) %/% 7

  return(week_num)
}
get_relevant_blocks <- function(pertinent_block, week_num){
  # used in make_week and find_phase_date_block to subset a schedule with week based blocks


  print(paste0('Week Number: ', week_num))
  print(as_tibble(pertinent_block))

  relevant_week_found <- stringr::str_detect(pertinent_block$relevant_weeks, as.character(week_num))
  all_weeks <- pertinent_block$relevant_weeks == 'all'

  # if (length(relevant_week_found) > 1 & any(relevant_week_found)){
  #   browser()
  # }

  if (any(relevant_week_found) | any(all_weeks)){
    cat('relevant week found, subsetting day phase block with all\n')
    new_pertinent_block <- rbind(pertinent_block[relevant_week_found,], pertinent_block[all_weeks,]) 

    # new_pertinent_block <- pertinent_block#[relevant_week_found, ]
  # } else if (any(pertinent_block$relevant_weeks == 'all')){
  #   cat('no relevant week found, subsetting day phase block to all\n')
  #   new_pertinent_block <- pertinent_block[pertinent_block$relevant_weeks == 'all',] 
  } else {
    # browser()
    new_pertinent_block <- NA
    cat('no relevant weeks found at all!\n')
  }
  print(as_tibble(new_pertinent_block))
  return(new_pertinent_block)
}
# surgeon_constraint <- function(current_surgeons, case_surgeons){

#   constraint_met <- TRUE
#   if (!is.null(current_surgeons)){ # if there is a surgeon assigned, check if there is more than one
#     # browser()
#     if (length(current_surgeons) != 1){
#       stop("Surgeon Options = 'first' but there is more than one unique surgeon for the block...")
#     }
#     if ((case_list$case_surgeon != current_surgeons)){
#         cat('Skipping Case ID:', case_list$case_id)
#         constraint_met <- FALSE
#     }
#   }
#   return(constraint_met)
# }


#' Days added to schedule until open block that is suitable for the case is available, then adds the case
#' @param case_list list, a case's ID, length, surgeon, post-op destination, etc
#' @param block_schedule list, block schedule list in which case will be added (weeks, days, blocks)
#' @param service_block_matrix list,  matrices of weekly blocks for service
#' @param phase_dates matrix of dates for each phase
#' @return block_schedule, updated block schedule list
#' @examples
add_case_block_refactor <- function(case_list, block_schedule, service_block_matrix, phase_dates){
  # get last week and day to subset list from (check w/ Lauren)
  n_weeks <- length(block_schedule)
  n_days <- length(purrr::pluck(block_schedule, n_weeks, 'Days'))
  block_schedule_out <- block_schedule # make a copy to update.

  new_date <- block_schedule_out %>% pluck(n_weeks, 'Days', n_days, 'date') + 1
  next_available_day <- toupper(weekdays(new_date)) 
  
  day_blocks <- find_phase_date_block_mat(
    phase_dates = phase_dates,
    service_open_block_list = service_block_matrix,
    in_date = new_date
  )
  
  block_schedule_out[[n_weeks]][["Days"]][[next_available_day]] <- make_day(block_matrix = day_blocks, in_date = new_date)
  while (block_schedule_out[[n_weeks]][["Days"]][[next_available_day]][["no_blocks"]]) {

      # if(case_list$case_id == "39040"){browser()} ## THIS GUY ISN'T FINDING HIS PLACE FOR SOME REASON
      n_weeks <- length(block_schedule_out); n_days <- length(purrr::pluck(block_schedule_out, n_weeks, 'Days'))
      new_date <-  block_schedule_out %>% pluck(n_weeks, 'Days', n_days, 'date') + 1
      next_available_day <- toupper(weekdays(new_date)) 

    if (next_available_day %in% c("SATURDAY", "SUNDAY")){ # assuming we are in Friday
        new_week_start <- pluck(block_schedule_out, n_weeks, 'Days', n_days, 'date') %>% next_weekday(., 2) # get date of next Monday
        # browser()
        new_week <- make_week(weekly_block_matrix = service_block_matrix,
                              start_date = new_week_start, 
                              schedule_start_date =  block_schedule[[1]]$`Start date` ,
                              phase_dates = phase_dates)

        block_schedule_out <- append(block_schedule_out, list(new_week))

        # update date for finding phase availability and for block creation 
        new_date <- new_week_start
        next_available_day <- toupper(weekdays(new_date)) 
        n_weeks <- length(block_schedule_out)

      }


      day_blocks <- find_phase_date_block_mat(phase_dates = phase_dates, #schedule_start_date =  block_schedule[[1]]$`Start date` , 
                                              service_open_block_list = service_block_matrix, in_date = new_date)
      block_schedule_out[[n_weeks]][["Days"]][[next_available_day]] <- make_day(block_matrix = day_blocks, in_date = new_date)
    }

    short_long_update <- update_short_long(proc_len_vec = case_list$case_length, block_time = day_blocks$block_length_mins[1])

    n_weeks <- length(block_schedule_out); n_days <- length(block_schedule_out[[n_weeks]][["Days"]])

    # create block_index_list, what find_bf_open_block would have returned IF there was immediate availability
    block_update_list <- list(
      "week" = n_weeks,
      "day" = n_days,
      "block" = 1,
      "updated_short_long" = short_long_update # for reference 
    )

  return(list('block_update_list' = block_update_list, 
             'block_schedule_out' = block_schedule_out))

}

