

# #' Add a case to an open block, applies only when there is an available block. Otherwise add_case_block is invoked
# #' @param case_list list, a case's ID, length, surgeon, post-op destination, etc
# #' @param block_schedule list, block schedule list in which case will be added (weeks, days, blocks)
# #' @param block_update list,  where to add the case in the schedule, output from function: find_bf_open_block
# #' @return block_schedule, updated block schedule list
# #' @examples
# add_case <- function(case_list, 
#                      block_schedule, 
#                      block_update 
# ) {
#   # browser()
#   # names are the elements for block schedule, elements are the case_list names
#   names_list <- c(
#     "case_ids" = "case_id",
#     "surgeon" = "case_surgeon",
#     "case_times" = "case_length",
#     "post_op_destination" = "post-op destination",
#     "orig_data" = "orig_data",
#     "short_long" = "updated_short_long"
#     #"service" = "case_service" @DS: Lauren -> not currently used?
#   )
#   require(purrr)

# og_block_schedule <- block_schedule
#   block_schedule2 <- block_schedule

# # map/lapply returns a list, but if we are iterating through n names then we are also creating n schedules. for loop should be okay here since 
# # there is not too many fields to go through..
# for (i in 1:length(names_list)){

#         this_field <- names(names_list)[[i]]

#         print(paste0('Field: ', this_field))

#         print(paste0('Pre-update: ', purrr::pluck(.x = block_schedule2, 
#                       block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
#                       this_field)))

#         if (this_field != "short_long"){

#           existing_elements <- purrr::pluck(.x = block_schedule2, 
#                 block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
#                 this_field) 

#           print(paste0('Existing: ', existing_elements))
#           print(paste0('Case List: ', case_list[[names_list[this_field]]]))

#           to_append <- c(existing_elements, case_list[[names_list[this_field]]] )
#           print(paste0('Existing + Case List: ', to_append))

#           pluck(.x = block_schedule2, 
#                       block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
#                       this_field) <- to_append

#         } else{
#           # if updating short_long, we don't need the existing values. 


#           to_append <- c(block_update[[names_list[this_field]]] )
#           print(paste0('Existing + Case List: ', to_append))

#           pluck(.x = block_schedule2, 
#                       block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
#                       this_field) <- to_append
    


#         }

#         # print(paste0('Post-update: ', purrr::pluck(.x = block_schedule, 
#         #               block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
#         #               this_field)))

#       }
  

  

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

#   return(block_schedule)
# }


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
                                        ),
                                        surg_opts = NULL,
                                        unit_opts = NULL,
                                        pacu_opts = NULL
                                        ) {


  # take elements of the named list                                        
  phase_dates <- phase_block_sched[["Phase dates"]]
  all_service_blocks <- phase_block_sched[["Blocks per service"]]

  # pass all cases in 

  # get service from the current case

  # subset the service block from the service 

  # for each service, for each block
  all_service_schedules <- lapply(services, function(service) {

    # get all phase blocks for the specified service
    service_block <- lapply(all_service_blocks, function(phase_blocks) {
      phase_blocks[[service]]
    })
    service_cases <- all_cases[grep(pattern = service, x = all_cases$Service), ]
    service_cases <- service_cases[order(service_cases$PCATS, service_cases$InWindow, service_cases$Surgeon), ]
    service_schedule <- make_single_service_sched_refactor(
      start_date = start_date,
      weekly_block_mat = service_block,
      phase_dates = phase_dates,
      in_case_list = service_cases,
      turnover_buffer = turnover_buffer,
      surg_opts = surg_opts
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
make_single_service_sched_refactor <- function(turnover_buffer = 25, 
                                      start_date = NULL, 
                                      weekly_block_mat = NULL, 
                                      phase_dates = NULL, 
                                      in_case_list = NULL, 
                                      in_sched = NULL, 
                                      verbose = TRUE,
                                      BF = TRUE ,
                                      surg_opts = NULL
) {
  cases_scheduled <- c()

  if (is.null(start_date)) {
    start_date <- Sys.Date()
  }

  if (is.null(weekly_block_mat)) {
    return("Please enter weekly_block_mat to create schedule")
  } else if (is.null(in_case_list)) {
    return("Please enter list of cases to create schedule")
  } else if (is.null(phase_dates)) {
    return("Please enter phase_dates to create schedule")
  } else {
    if (is.null(in_sched)) {
      ## initialize schedule
      ## makes an empty week to fill up with actual booked cases, composed of weeks, 
      ## creates pink sector of slide 3, starts with 1 day
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

      # if (length(in_case_list[my_row, "ID"]) == 0) {
      #   if (verbose) {
      #     print("Missing row passed")
      #   }
      # } else if (is.na(in_case_list[my_row, "ID"])) {
      #   if (verbose) {
      #     print("Missing row passed")
      #   }
      # } else {
        current_case <- in_case_list[my_row, ]

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
          cat(paste0("Finding block for Case ID: ", my_case_list$case_id, '\n'))
        }

        # if (is.na(my_case_list$case_id)) {browser()}
        open_block_update <- find_bf_open_block(case_list = my_case_list, 
                                                surg_opts = surg_opts,
                                                block_schedule = sched_list)

        # re-factored so the end result is add_case regardless of what find_bf_open_block returns
        if (is.na(open_block_update[[1]])) {  # this actually checks week
          # updates the schedule list to have more days 
          # days added to schedule until -> open block that is suitable for the case 
          # also schedules the case 
          # one case per block (orthopaedics is most common for this)

          if(verbose){ cat("\tNo open block found in schedule, adding days until block available for case..\n")}
          # now returns a list of two objects, one which is an updated schedule to add the case and 
          # an non-NA object identical to what is returned by find_bf_open_block, that we can pass into add_case
          new_stuff <- add_case_block_refactor(case_list = my_case_list, 
                                      block_schedule = sched_list, 
                                      service_block_matrix = weekly_block_mat, 
                                      phase_dates = phase_dates)

          open_block_update <- new_stuff[['block_update_list']]
          sched_list <- new_stuff[['block_schedule_out']]   # aka block_schedule              
        } 
        # add_case 
        sched_list <- add_case(case_list = my_case_list, 
                        block_schedule = sched_list, 
                        block_update = open_block_update
                        )

        if (verbose) {
          print(paste0("Case ID: ", my_case_list$case_id, " added to schedule."))
  
        }
        cases_scheduled <- c(cases_scheduled, my_case_list$case_id)
      }
    }

    out_list <- list("sched_list" = sched_list, "cases_scheduled" = cases_scheduled)

    return(out_list)
  }


