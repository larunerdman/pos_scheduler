
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


## EXTRACT CALENDAR SPREADSHEET OF SCHEDULE LIST. Post-hoc? 
# @DS - big function, can this be re-factored
get_spreadsheet_per_service_sched <- function(in_sched_raw,
                                              per_block = TRUE,
                                              per_case = TRUE,
                                              per_day = TRUE,
                                              write_files = TRUE,
                                              orig_data_only = TRUE) {

  # browser()
  out_list <- list()
  block_df <- NULL
  case_df <- NULL
  day_df <- NULL

  in_sched <- in_sched_raw$sched

  if (per_block) {
    services <- c()
    weeks <- c()
    date <- c()
    block_type <- c()
    block_length <- c()
    n_cases <- c()
    block_long_short <- c()

    for (service in names(in_sched)) {

      for (week in 1:length(in_sched[[service]]$sched_list)) {

        for (day in 1:length(in_sched[[service]]$sched_list[[week]][["Days"]])) {

          for (block in 1:length(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]])) {
            services <- c(services, service)
            weeks <- c(weeks, week)
            date <- c(date, in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["date"]])
            # browser()
            if (is.null(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["block_type"]])) {
              my_block_type <- NA
            } else {
              my_block_type <- in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["block_type"]]
            }
            block_type <- c(block_type, my_block_type)
            block_length <- c(block_length, sum(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["case_times"]]))
            n_cases <- c(n_cases, length(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["case_ids"]]))
            if (is.null(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["short_long"]])) {
              long_short <- NA
            } else {
              long_short <- in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["short_long"]]
            }
            block_long_short <- c(block_long_short, long_short)
          }
        }
      }
    }

    # browser()
    block_df <- data.frame(services, weeks, as.Date(date, origin = "1970-01-01"), block_type, block_length, n_cases, block_long_short)
  }

  if (per_case) {
    services <- c()
    weeks <- c()
    date <- c()
    block_num <- c()
    block_type <- c()
    case_id <- c()
    case_length <- c()
    case_orig <- c()
    surgeon <- c()
    block_long_short <- c()

    for (service in names(in_sched)) {

      for (week in 1:length(in_sched[[service]]$sched_list)) {

        for (day in 1:length(in_sched[[service]]$sched_list[[week]][["Days"]])) {

          for (block in 1:length(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]])) {

            if (length(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]]$case_ids) > 0) {

              for (case in 1:length(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]]$case_ids)) {
                services <- c(services, service)
                weeks <- c(weeks, week)
                date <- c(date, in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["date"]])
                if (is.null(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["block_type"]])) {
                  my_block_type <- NA
                } else {

                  my_block_type <- in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["block_type"]]
                }
                block_type <- c(block_type, my_block_type)
                block_num <- c(block_num, block)
                case_id <- c(case_id, in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["case_ids"]][case])
                case_length <- c(case_length, in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["case_times"]][case])
                case_orig <- c(case_orig, in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["orig_data"]][case])
                surgeon <- c(surgeon, in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["surgeon"]][case])

                if (is.null(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["short_long"]])) {
                  long_short <- NA
                } else {
                  long_short <- in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["short_long"]]
                }
                block_long_short <- c(block_long_short, long_short)
              }
            }
          }
        }
      }
    }
    # browser()
    case_df <- data.frame(services, weeks, Date = as.Date(date, origin = "1970-01-01"), block_type, block_num, case_id, case_length, surgeon, block_long_short, case_orig)
  }

  if (per_day) {
    services <- c()
    weeks <- c()
    date <- c()
    n_cases <- c()
    no_blocks <- c()
    cases_length <- c()
    picu_oicu <- c()
    constant_obs <- c()
    in_patient <- c()
    short_stay <- c()
    cccu <- c()
    fiveA <- c()
    fiveAsss <- c()
    fiveB <- c()
    fiveCsd <- c()
    eightC <- c()


    for (service in names(in_sched)) {
      for (week in 1:length(in_sched[[service]]$sched_list)) {
        for (day in 1:length(in_sched[[service]]$sched_list[[week]][["Days"]])) {
          services <- c(services, service)
          weeks <- c(weeks, week)
          date <- c(date, in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["date"]])
          no_blocks <- c(no_blocks, in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["no_blocks"]])

          cases_vec <- c()
          cases_len <- c()
          post_op_dest_vec <- c()

          for (block in 1:length(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]])) {
            cases_vec <- c(cases_vec, length(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["case_ids"]]))
            cases_len <- c(cases_len, sum(in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["case_times"]]))
            post_op_dest_vec <- c(post_op_dest_vec, in_sched[[service]]$sched_list[[week]][["Days"]][[day]][["procedure_blocks"]][[block]][["post_op_destination"]])
          }

          # browser()

          cases_length <- c(cases_length, sum(cases_len))
          n_cases <- c(n_cases, sum(cases_vec))
          picu_oicu <- c(picu_oicu, sum(post_op_dest_vec == "PICU" | post_op_dest_vec == "OICU"))
          constant_obs <- c(constant_obs, length(grep(pattern = "Constant Observation", x = post_op_dest_vec)))
          fiveA <- c(fiveA, length(grep(pattern = "5A Constant Observation", x = post_op_dest_vec)))
          fiveAsss <- c(fiveAsss, length(grep(pattern = "5A Surgical Short Stay Constant Obs", x = post_op_dest_vec)))
          fiveB <- c(fiveB, length(grep(pattern = "5B Constant Observation", x = post_op_dest_vec)))
          fiveCsd <- c(fiveCsd, length(grep(pattern = "5C Stepdown", x = post_op_dest_vec)))
          eightC <- c(eightC, length(grep(pattern = "8C Constant Observation", x = post_op_dest_vec)))

          ### WRONG  -- ?
          in_patient <- c(in_patient, sum(post_op_dest_vec != "Home"))
          short_stay <- c(short_stay, length(grep("Short Stay", post_op_dest_vec)))
          cccu <- c(cccu, sum(post_op_dest_vec == "CCCU"))

          # browser()
        }
      }
    }
    browser()
    day_df <- data.frame(
      services, weeks, as.Date(date, origin = "1970-01-01"),
      no_blocks, n_cases, cases_length, picu_oicu, constant_obs, in_patient, short_stay, cccu,
      fiveA, fiveAsss, fiveB, fiveCsd, eightC
    )
    names(day_df) <- c(
      "services", "weeks", "date",
      "no_blocks", "n_cases", "case_length", "picu_oicu", "constant_obs", "in_patient", "short_stay", "cccu",
      "fiveA", "fiveAsss", "fiveB", "fiveCsd", "eightC"
    )

    day_df_withbd <- data.frame(matrix(nrow = 0, ncol = length(c("services", "weeks", "date", "no_blocks", "n_cases", "cases_length", "picu_oicu", "constant_obs", "in_patient", "short_stay", "cccu", "fiveA", "fiveAsss", "fiveB", "fiveCsd", "eightC"))))
    names(day_df_withbd) <- c("services", "weeks", "date", "no_blocks", "n_cases", "cases_length", "picu_oicu", "constant_obs", "in_patient", "short_stay", "cccu", "fiveA", "fiveAsss", "fiveB", "fiveCsd", "eightC")

    for (service in unique(day_df$services)) {
      # browser()
      service_df <- day_df[day_df$services == service, ]
      service_df <- service_df[order(service_df$date), ]

      total_cases <- sum(service_df$n_cases)

      for (day in 1:nrow(service_df)) {
        remaining_cases <- total_cases - sum(na.omit(service_df[1:day, "n_cases"]))
        service_df[day, "remaining_cases"] <- remaining_cases
      }
      day_df_withbd <- rbind(day_df_withbd, service_df)
    }
  }

  if (write_files) {
    if (!is.null(block_df)) {
      write.csv(block_df, file = "per_block_data.csv", quote = FALSE)
    }
    if (!is.null(case_df)) {
      write.csv(case_df, file = "per_case_data.csv", quote = FALSE)
    }
    if (!is.null(day_df)) {
      write.csv(day_df, file = "per_day_data.csv", quote = FALSE)
    }
  }

  out_list <- list("block_df" = block_df, "case_df" = case_df, "day_df" = day_df_withbd)

  return(out_list)
}
