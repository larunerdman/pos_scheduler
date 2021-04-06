

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

# map/lapply returns a list, but if we are iterating through n names then we are also creating n schedules. for loop should be okay here since 
# there is not too many fields to go through..
for (i in 1:length(names_list)){

        this_field <- names(names_list)[[i]]

        print(paste0('Field: ', this_field))

        print(paste0('Pre-update: ', purrr::pluck(.x = block_schedule2, 
                      block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
                      this_field)))

        if (this_field != "short_long"){

          existing_elements <- purrr::pluck(.x = block_schedule2, 
                block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
                this_field) 

          print(paste0('Existing: ', existing_elements))
          print(paste0('Case List: ', case_list[[names_list[this_field]]]))

          to_append <- c(existing_elements, case_list[[names_list[this_field]]] )
          print(paste0('Existing + Case List: ', to_append))

          pluck(.x = block_schedule2, 
                      block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
                      this_field) <- to_append

        } else{
          # if updating short_long, we don't need the existing values. 


          to_append <- c(block_update[[names_list[this_field]]] )
          print(paste0('Existing + Case List: ', to_append))

          pluck(.x = block_schedule2, 
                      block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
                      this_field) <- to_append
    


        }

        # print(paste0('Post-update: ', purrr::pluck(.x = block_schedule, 
        #               block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block, # accessing the list 
        #               this_field)))

      }
  

  

  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$case_ids <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$case_ids, case_list$case_id)
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$surgeon <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$surgeon, case_list$case_surgeon)
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$case_times <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$case_times, case_list$case_length)
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$post_op_destination <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$post_op_destination, case_list$`post-op destination`)
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$orig_data <- c(block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$orig_data, case_list$orig_data)
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$short_long <- block_update$updated_short_long
  block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]]$service <- case_list$case_service

 # browser()
  stopifnot(all.equal(
    block_schedule[[block_update$week]][["Days"]][[block_update$day]][["procedure_blocks"]][[block_update$block]],
            
            purrr::pluck(.x = block_schedule2, 
                      block_update$week, 'Days', block_update$day, 'procedure_blocks', block_update$block)
                      
                      ))

  return(block_schedule)
}

