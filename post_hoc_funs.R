
# Plot and Other Post-Hoc Functions (split when I am more familiar)

# ---- post-hoc -----

## count cases booked up to a given date
count_cases_to_date <- function(case_df, date = as.Date("2020-11-30")) {
  new_case_df <- case_df[case_df$Date < date, ]
  new_case_df$n_cases <- 1
  out_counts <- aggregate(new_case_df$n_cases,
    by = list(new_case_df$services),
    FUN = sum
  )
  names(out_counts) <- c("Service", "Cases")
  return(out_counts)
}

## count the number of cases seen each month
count_cases_per_month <- function(case_df) {
  # browser()
  min_date <- min(case_df$Date)
  max_date <- max(case_df$Date)
  date_seq <- seq(min_date, max_date, by = "month")

  cases_per_service <- data.frame(matrix(nrow = length(date_seq) - 1, ncol = length(unique(case_df$services)) + 1))
  names(cases_per_service) <- c("Date_range", unique(as.character(case_df$services)))

  for (i in 1:(length(date_seq) - 1)) {
    # browser()
    cases_per_service$Date_range[i] <- paste0(date_seq[i], " -- ", date_seq[i + 1])

    new_case_df <- case_df[case_df$Date > date_seq[i] & case_df$Date < date_seq[i + 1], ]
    new_case_df$n_cases <- 1
    out_counts <- aggregate(new_case_df$n_cases,
      by = list(new_case_df$services),
      FUN = sum
    )
    cases_per_service[i, as.character(out_counts$Group.1)] <- out_counts$x
  }

  return(cases_per_service)
}


## ADD FUNCTIONS:
## - CREATE BLOCK-LEVEL DF: EACH ROW IS 1 BLOCK, BLOCK TYPE, LENGTH, DATE, SERVICE
get_time_left <- function(block_list, ## dataframe described above ^
                          return_time_left = TRUE, ## whether to return the amount of time left
                          verbose = TRUE ## whether to write output to console
) {

  # browser()

  if (length(block_list[[length(block_list)]]) == 5) {
    weeks <- length(block_list)
    days <- 0
  } else {
    weeks <- length(block_list)
    days <- length(block_list[[length(block_list)]][["Days"]])
  }

  if (verbose) {
    cat(paste0("Time to clearing list: ", weeks, " weeks and ", days, " days."))
  }

  if (return_time_left) {
    return(data.frame(weeks = weeks, days = days))
  }
}

get_time_remaining_per_service <- function(in_sched, ## schedule to find time remaining over
                                           services = list(), ## list of services you're interested in analyzing
                                           return_table = TRUE, ## return a table of services with their time left
                                           verbose = TRUE ## write output to console
) {

  ## @LE DEBUG
  # browser()
  in_sched_rev <- lapply(in_sched$schedule, function(x) {
    x[["sched_list"]]
  })

  services <- as.list(names(in_sched_rev))

  # if(length(services) == 0){
  #   services = as.list(names(in_sched))
  # }
  time_left_by_service <- lapply(services, function(service) {
    # browser()
    cat(paste0("\nService:", service[[1]], "\n"))
    time_left <- get_time_left(in_sched_rev[[service[[1]]]], return_time_left = return_table, verbose = verbose)
    cat("\n\n")
    return(time_left)
  })

  times_out <- Reduce(rbind, time_left_by_service)
  times_out$service <- unlist(services)

  return(times_out[, c("service", "weeks", "days")])
}


