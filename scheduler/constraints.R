"
For target constraints, make_multi_perservice_sched() was refactored into make_sched_all_services() so a master schedule is stored in memory and accessible from functions.
Theoretically, the scheduler should not yield a different schedule if all is implemented correctly (testing has shown this to be true.)
TODO: add surgeon block constraint here
TODO: surgeon constraints are too simplistic (or actually too complex..) to add here?
"

library(stringr)

get_current_day_post_op_cts <- function(all_service_schedule, week, day) {
  "
  given a current week, day, and the master schedule, compute the post op counts for each service

  # for each service, at the day level of the schedule,
  # we want to retrieve all blocks under 'procedure_blocks', and tally or keep count of the units
  # get the post op destinations stored at the lowest level (the block number)
  "

  cts <- lapply(names(all_service_schedule), function(service) {
    service_blocks <- pluck(.x = all_service_schedule, service, week, "Days", day, "procedure_blocks")

    post_op_destinations <- bind_rows(service_blocks) %>% na.omit() # would we ever need a block identifier - doubt it. removes blocks with no scheduled cases
    

    # bind_rows() will return an empty tibble (dim 0,0) if there are no scheduled services, the latter checks for an edge case where blocks may exist but no schedule has been added yet.
    if (nrow(post_op_destinations) == 0 | !("case_ids" %in% names(post_op_destinations))) {
      return(NULL)
    }
  
    # parsing out the current post op destination and unit, if applicable. if not the unit and post op destination will be identical
    post_op_cts <- count(post_op_destinations, post_op_destination) %>% mutate(service = service)
    post_op_cts$unit <- stringr::str_split_fixed(post_op_cts$post_op_destination, " ", n = 2)[, 1]
    post_op_cts$week <- week
    post_op_cts$day <- day
    return(post_op_cts)
  }) %>%
    bind_rows()

  return(cts)
}

get_relevant_post_op_ct <- function(current_days_post_op_cts, case_destination, case_destination_unit) {
  "
  using post op counts from get_current_day_post_op_cts(), aggregates across units
  returns current unit capacity and total pacu counts

  TODO: rename capacity
  "
  icu_pattern <- "[A-Z]{2}CU"
  # this will only be true if there are scheduled blocks
  if (nrow(current_days_post_op_cts) > 0) {
    all_current_days_unit_cts <- current_days_post_op_cts %>%
      group_by(unit) %>%
      summarize(total = sum(n))

    # check icu counts if current case is in icu and counts currently has an icu
    # must supersede unit-level checks otherwise the *CU threshold will be taken as the capacity
    if ((stringr::str_detect(case_destination, icu_pattern))) {
      # if (any(stringr::str_detect(unique(all_current_days_unit_cts$unit), icu_pattern))) {
      current_day_unit_capacity <- sum(all_current_days_unit_cts[stringr::str_detect(all_current_days_unit_cts$unit, icu_pattern), ]$total)
      # }
      # check if unit exists in counts, if not set to 0
    } else if (case_destination_unit %in% current_days_post_op_cts$unit) {
      current_day_unit_capacity <- all_current_days_unit_cts[all_current_days_unit_cts$unit == case_destination_unit, ]$total
    } else {
      current_day_unit_capacity <- 0
    }

    # browser()

    # check if sub-unit destination exists
    if ((stringr::str_detect(stringr::str_to_lower(case_destination), "constant")) & (case_destination %in% unique(current_days_post_op_cts$post_op_destination))) {
      all_current_days_post_op_cts <- current_days_post_op_cts %>%
        group_by(post_op_destination) %>%
        summarize(total = sum(n))
      current_day_unit_granular_capacity <- all_current_days_post_op_cts[all_current_days_post_op_cts$post_op_destination == case_destination, ]$total
    } else {
      current_day_unit_granular_capacity <- 0
    }

    # get pacu counts
    current_day_total_pacu <- sum(all_current_days_unit_cts[!stringr::str_detect(all_current_days_unit_cts$unit, "Home|[A-Z]{2}CU"), ]$total)
  }
  # accounts for NULL and no rows - means current unit has zero capacity
  else {
    current_day_unit_capacity <- 0
    current_day_total_pacu <- 0
    current_day_unit_granular_capacity <- 0
  }
  return(c(
    "current_day_unit_capacity" = current_day_unit_capacity,
    "current_day_unit_granular_capacity" = current_day_unit_granular_capacity,
    "current_day_total_pacu" = current_day_total_pacu
  ))
}

check_target_constraints <- function(current_post_op_destination, unit_targets, pacu_targets, week, day, all_service_schedule) {
  "
    computes current days unit capacity given master schedule, retrieves the relevant unit capacity, assigns target thresholds based on post-op destination, ultimately performing the check for the constraint
  "
  fail_constraint <- FALSE
  # check if current case is going home if yes then we can ignore the target capacities


  # identify the current post op unit, if applicable
  current_post_op_unit <- stringr::str_split_fixed(current_post_op_destination, " ", n = 2)[, 1]
  names(current_post_op_unit) <- "Current Post-Op Unit"

  # if going to constant obs, create the threshold for the constant obs
  if (stringr::str_detect(stringr::str_to_lower(current_post_op_destination), "constant")) {

    # check whether the granular unit target is in the supplied named vector
    step_down_unit <- paste(current_post_op_unit, "Step Down")
    if (!step_down_unit %in% names(unit_targets)) {
      stop(paste0("Please check that you have provided the following destination's target capacity in the supplied vector: ", step_down_unit))
    }
    current_post_op_granular_threshold <- unit_targets[[step_down_unit]]
  } else {
    current_post_op_granular_threshold <- 0
  }

  # ---- UNIT check ----
  if (stringr::str_detect(current_post_op_destination, "[A-Z]{2}CU")) {
    current_unit_threshold <- unit_targets[["ICU"]]
  } else {
    # this implicitly accounts for both short stay and 'normal' destinations, eg. 5A, 5B, 5C, etc.
    # explicit is better than implicit but leave for now.
    if (!current_post_op_unit %in% names(unit_targets)) {
      stop(paste0("Please check that you have provided the following destination's target capacity in the supplied vector: ", current_post_op_unit))
    }
    current_unit_threshold <- unit_targets[[current_post_op_unit]]
  }
  names(current_unit_threshold) <- current_post_op_unit

  # check current week-day destination thresholds
  current_days_post_op_cts <- get_current_day_post_op_cts(all_service_schedule, week, day)
  # subset current destination thresholds for comparison
  cts <- get_relevant_post_op_ct(current_days_post_op_cts, case_destination = current_post_op_destination, case_destination_unit = current_post_op_unit)

  current_day_total_pacu <- cts["current_day_total_pacu"]
  current_day_unit_capacity <- cts["current_day_unit_capacity"]
  current_day_post_op_capacity <- cts["current_day_unit_granular_capacity"]

  # if current DAILY total is greater than the threshold, move onto next day for scheduling

  if (current_day_total_pacu >= pacu_targets) {
    cat("PACU over target, going to next day\n")
    fail_constraint <- TRUE
    # check total unit capacity, then check sub-unit
  } else if (current_day_unit_capacity >= current_unit_threshold) {
    cat("Unit over target, going to next day\n")
    fail_constraint <- TRUE
    # if both constant obs and capacity is over, skip
  } else if (stringr::str_detect(stringr::str_to_lower(current_post_op_destination), "constant")) {
    if (current_day_post_op_capacity >= current_post_op_granular_threshold) {
      cat("Constant Obs over target, going to next day\n")
      fail_constraint <- TRUE
    }
  }

  if (fail_constraint) {
    # cat(paste0("Week: ", week, " Day: ", day, "\n"))
    # cat(paste(
    #   "Current Post Op Dest: ", current_post_op_destination, "\n",
    #   "Current Post Op Dest Threshold: ", current_post_op_granular_threshold, "\n",
    #   "Current Post Op Dest Capacity: ", current_day_post_op_capacity, "\n\n",
    #   "Current Unit: ", current_post_op_unit, "\n",
    #   "Current Unit Threshold: ", current_unit_threshold, "\n",
    #   "Current Unit Capacity: ", current_day_unit_capacity, "\n\n",
    #   "PACU Target: ", pacu_target, "\n",
    #   "Current Day PACU Total: ", current_day_total_pacu, "\n"
    # ))
  }

  return(fail_constraint)
}
