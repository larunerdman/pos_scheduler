library(readxl)
# These are the main functions needed to run the scheduler, more specifically
# run_scheduler()
# make_multi_perservice_sched
# make_single_service_sched

# Run scheduler
# input: WTIS and make phase schedule (WHAT ARE THESE OBJECTS AFTER PRE-PROCESSING??)
# make_multi_perservice_sched
# make_single_service_sched (for each service)
# make_week -> fills in first day and starts with blocks on first day
# make_day
# make_block
# make_case_list
# find_bf_open_block
# add_case OR add_case_block


#' Runs the scheduler
#'
#' @param wtis_in str, name of waitlist excel sheet.
#' @param sub_n int,  subsets waitlist UP TO this row number.
#' @param case_ids vector, case_idsâ
#' @param phase_dates_xlsx str, simple spreadsheet with columns saying "Phase 4", ... and dates below
#' @param phase_list_xlsx vector: str, phase excel sheets whose order must match phase_dates_xlsx (???????)
#' @param resample_xlsx str, Not relevant at the moment -- parameters for resampling
#' @param services list: str, list of services we want in the schedule
#' @param start_date Date, schedule start date
#' @param rotating_services vector: str, services to rotate in daycare blocks (UNUSED)
#' @param verbose_run bool, detail what's happening as model runs
#' @param turnover_buffer int, number of minutes to add to each case to account for the time to move one person out of the OR and another in
#' @param time vector: str, choose which time to base your schedule on (anesthesia, procedure, and case are predicted, surgeon is provided in the waitlist
#' @param add_cases bool, whether to simulate cases should be added to the waitlist (UNUSED)
#' @param high_pri_prop float, proportion of high priority cases to add when simulating cases being added to the model
#' @param home_only bool, whether to make a schedule only for same-day cases
#' @param max_time NULL/int, only include procedures < a certain length in minutes
#' @param surg_opts str, constraints by surgeon. options are 'first', 'first-owner', 'input_sched'
#' @param post_op_opts str, constraints by post op destination, options are NULL or 'all'
#' @param pacu_opts named vector
#' @param unit_opts int
#' @return List of .....
#' @examples
run_scheduler <- function(wtis_in,
                          sub_n = NULL,
                          case_ids = NULL, #
                          phase_dates_xlsx = "PhaseDates_extended.xlsx",
                          phase_list_xlsx = c("Phase4_v1.xlsx", "Phase5_v1.xlsx", "Phase6_v1.xlsx", "Phase7_v1.xlsx", "Phase8_v1.xlsx"),
                          resample_xlsx = "Copy of Ongoing Wait List Summary Tables.xlsx",
                          services = list("General", "Otolaryngology", "Neurosurgery", "Ophthalmology", "Orthopaedics", "Gynaecology", "Plastics", "Urology", "Dentistry"),
                          start_date = as.Date("2020-11-02"),
                          rotating_services = c("Opthalmology", "Orthopaedics", "Urology", "Otolaryngology"),
                          verbose_run = TRUE,
                          turnover_buffer = 25,
                          time = c("anesthesia", "procedure", "case", "surgeon"),
                          add_cases = FALSE,
                          high_pri_prop = 0.5,
                          home_only = FALSE,
                          max_time = NULL,
                          surg_opts = NULL, # TODO: add arguments
                          post_op_opts = NULL,
                          unit_opts = NULL,
                          pacu_opts = NULL,
                          surg_sched = NULL) {


  ## --- Verify surgeon schedule is compatible with block schedule ----
  
  if(!is.null(surg_sched)){
    
    validate_input_surgeon_schedule(waitlist_file = wtis_in, 
                                    phase_list_file = phase_list_xlsx[[1]], # assumed to be duplicates
                                    surgeon_schedule = surg_sched)
  }
  ## ---- PREP WTIS ----
  wtis <- data.frame(read_excel(wtis_in, sheet = 1))
  
  # print(names(wtis))
  
  if (home_only) {
    wtis_sub <- wtis[
      wtis$Patient.Class == "Outpatient Surgical Day Care",
      c(
        "Case..", "Surgeon", "Length..minutes.", "Service", "PCATS.Priority",
        "In.Window", "Planned.Post.op.Destination",
        "Case.Procedures", "Patient.Class"
      )
    ] # [complete.cases(wtis[,c("WTIS.ID","Length..minutes.","Service","WTIS.Priority")]),]
  } else {
    wtis_sub <- wtis[, c(
      "Case..", "Surgeon", "Length..minutes.", "Service",
      "PCATS.Priority", "In.Window", "Planned.Post.op.Destination", "Case.Procedures", "Patient.Class"
    )] # [complete.cases(wtis[,c("WTIS.ID","Length..minutes.","Service","WTIS.Priority")]),]
  }
  
  # print(head(wtis_sub))
  

  names(wtis_sub) <- c("ID", "Surgeon", "Time", "Service", "PCATS", "InWindow", "PostOPDest", "Case.Procedures", "Patient.Class")
  wtis_sub$Service[wtis_sub$Service == "Orthopedics"] <- "Orthopaedics"
  # browser()
  wtis_sub <- wtis_sub[complete.cases(wtis_sub), ] ## may need to comment this at some point -- if there is any missing data in the selected columns, they will be dropped
  wtis_sub <- wtis_sub[wtis_sub$Service != "Cardiovascular", ]
  wtis_sub <- wtis_sub[wtis_sub$Service %in% unlist(services), ]
  wtis_sub <- wtis_sub[order(wtis_sub$PCATS, wtis_sub$InWindow), ]

  # if(verbose_run){print(wtis_sub)}

  # ----- PREDICT TIME OR USE SURGEON INPUT TIME AS CASE TIME VARIABLE -------
  

  if (time == "anesthesia") {
    wtis_sub$time <- pred_wtis_an_time(
      wtis_dat = wtis_sub, id_col = "ID",
      model_file = "AN_TIMES_RF_LOGGED_20200701.rds"
    )
  } else if (time == "procedure") {
    wtis_sub$time <- pred_wtis_an_time(
      wtis_dat = wtis_sub, id_col = "ID",
      model_file = "PROC_TIMES_RF_LOGGED_20200701.rds"
    )
  } else if (time == "case") {
    wtis_sub$time <- pred_wtis_an_time(
      wtis_dat = wtis_sub, id_col = "ID",
      model_file = "CASE_TIMES_RF_LOGGED_20200701.rds"
    )
  } else if (time == "surgeon") {
    wtis_sub$time <- wtis_sub$Time
  } else {
    cat("PLEASE SPECIFY MODEL TIME")
  }

  if (!is.null(max_time)) {
    wtis_sub <- wtis_sub[wtis_sub$time < max_time, ]
  }

  in_dat <- wtis_sub
  in_dat$orig_list <- TRUE

  if (!is.null(sub_n)) {
    in_dat <- wtis_sub[1:sub_n, ]
  }

  if (!is.null(case_ids)) {
    in_dat <- wtis_sub[wtis_sub$ID %in% case_ids, ]
  }

  ## ----- CREATE LIST OF PER-SERVICE SCHEDULES FOR EACH PHASE ------
  phase_sched <- make_phase_sched(
    phase_dates_xlsx = phase_dates_xlsx,
    phase_list = phase_list_xlsx,
    services = services,
    rotating_services = rotating_services ## to be added in completion
  )

  # browser()
  if (post_op_opts == "service") {
    print("USING OLD FUNCTION")
    per_serv_sched <- make_multi_perservice_sched(
      all_cases = in_dat,
      phase_block_sched = phase_sched,
      start_date = start_date,
      verbose_run = verbose_run,
      services = services,
      turnover_buffer = turnover_buffer,
      surg_opts = surg_opts,
      unit_opts = unit_opts,
      pacu_opts = pacu_opts
      # surg_sched = surg_sched
    )
  } else if (post_op_opts == "all") {
    print("USING NEW FUNCTION")
    per_serv_sched <- make_sched_all_services(
      all_cases = in_dat,
      phase_block_sched = phase_sched,
      start_date = start_date,
      verbose_run = verbose_run,
      services = services,
      turnover_buffer = turnover_buffer,
      surg_opts = surg_opts,
      unit_opts = unit_opts,
      pacu_opts = pacu_opts,
      surg_sched = surg_sched
    )
  }
  names(per_serv_sched) <- unlist(services)

  # default schedule is a list where each element pertains to a service and
  # contains two elements, the first is the schedule and the second is the scheduled cases

  if (!add_cases) {
    out_list <- list("schedule" = per_serv_sched, "case_list" = in_dat)
    return(out_list)
  }
}



# MAKE SCHEDULE FOR ALL SERVICES?

# TODO -REVAMP
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
#' # amalgamation of make_single_service_sched and make_multi_perservice_sched
make_sched_all_services <- function(all_cases = NULL,
                                    phase_block_sched = NULL,
                                    services = NULL,
                                    turnover_buffer = 25,
                                    start_date = NULL,
                                    in_sched = NULL,
                                    verbose_run = TRUE,
                                    BF = TRUE,
                                    surg_opts = NULL,
                                    unit_opts = NULL,
                                    pacu_opts = NULL,
                                    surg_sched = NULL
                                    # phase_dates = NULL,
) {
  cases_sched_by_service <- list()

  # take elements of the named list

  # from parent make_multi_perservice_sched
  phase_dates <- phase_block_sched[["Phase dates"]]
  all_service_blocks <- phase_block_sched[["Blocks per service"]]

  # initialize schedule of services
  sched_by_service <- services %>%
    map(function(service) {
      print(service)

      weekly_block_mat <- lapply(all_service_blocks, function(phase_blocks) {
        phase_blocks[[service]]
      })

      sched_list <- list(make_week(
        weekly_block_matrix = weekly_block_mat, # synonymous w/ service_block or weekly_block_mat
        start_date = start_date,
        phase_dates = phase_dates
      ))

      return(sched_list)
    })
  stopifnot(length(sched_by_service) == length(services))
  names(sched_by_service) <- services

  # also from parent make_multi_perservice_sched, why is this sorted again (as in twice)?
  in_case_list <- all_cases[order(all_cases$PCATS, all_cases$InWindow, all_cases$Surgeon), ]

  in_case_list <- in_case_list[complete.cases(in_case_list), ]

  for (my_row in 1:nrow(in_case_list)) {
    current_case <- in_case_list[my_row, ]
    # subset current case service
    current_service <- current_case$Service
    # subset service's schedule
    sched_list <- sched_by_service[[current_service]]
    # subset service's block sched
    weekly_block_mat <- lapply(all_service_blocks, function(phase_blocks) {
      phase_blocks[[current_service]]
    })


    my_case_list <- make_case_list(
      case_id = unlist(in_case_list[my_row, "ID"]),
      case_length = unlist(in_case_list[my_row, "time"]),
      case_priority = in_case_list[my_row, "PCATS"],
      case_surgeon = in_case_list[my_row, "Surgeon"],
      post_op_destination = in_case_list[my_row, "PostOPDest"],
      orig_data = in_case_list[my_row, "orig_list"],
      turnover_buffer = turnover_buffer
    )
    if (verbose_run) {
      cat(paste0("Finding block for Case ID: ", my_case_list$case_id, "\n"))
    }
    # outputs NA if there is no opening
    if (my_row > 100 && !stringr::str_detect(stringr::str_to_lower(my_case_list$`post-op destination`), "home|[a-z]{2}cu")) { # debug for adding post op destinations
    }
    
    # if(my_case_list$case_id == 111229) browser()
    open_block_update <- find_bf_open_block(
      case_list = my_case_list,
      surg_opts = surg_opts,
      unit_opts = unit_opts,
      pacu_opts = pacu_opts,
      surg_sched = surg_sched,
      # block_schedule = sched_list,
      service = current_service,
      all_service_schedule = sched_by_service
    )

    # re-factored so the end result is add_case regardless of what ZA2w returns
    
    if (is.na(open_block_update[[1]])) { # this actually checks week

      # updates the schedule list to have more days
      # days added to schedule until -> open block that is suitable for the case
      # also schedules the case
      # one case per block (orthopaedics is most common for this)

      if (verbose_run) {
        cat("\tNo open block found in schedule, adding days until block available for case..\n")
      }
      # now returns a list of two objects, one which is an updated schedule to add the case and
      # an non-NA object identical to what is returned by find_bf_open_block, that we can pass into add_case

      new_stuff <- add_case_block_refactor(
        case_list = my_case_list,
        block_schedule = sched_list,
        service_block_matrix = weekly_block_mat,
        phase_dates = phase_dates,
        all_service_schedule = sched_by_service,
        surg_sched = surg_sched,
        unit_opts = unit_opts,
        pacu_opts = pacu_opts,
        service = current_service
      )

      open_block_update <- new_stuff[["block_update_list"]]
      sched_list <- new_stuff[["block_schedule_out"]] # aka block_schedule
    }
    # add_case
    sched_list <- add_case(
      case_list = my_case_list,
      block_schedule = sched_list,
      block_update = open_block_update
    )

    if (verbose_run) {
      print(paste0("Case ID: ", my_case_list$case_id, " added to schedule."))
    }
    # replace the old sched list with the new, updated one
    sched_by_service[[current_service]] <- sched_list
    # add case to the schedule
    cases_sched_by_service[[current_service]] <- c(cases_sched_by_service[[current_service]], my_case_list$case_id)
  }
  # }

  # HERE - loop through to recreate the same output as the lapply from make_multi..
  out_list <- map(services, function(service) {
    return(list(
      "sched_list" = sched_by_service[[service]],
      "cases_scheduled" = cases_sched_by_service[[service]]
    ))
  })

  return(out_list)
}



### ----- preparing the phase schedule ------


#' Create phase schedule list
#'
#' @param phase_dates_xlsx str, simple spreadsheet with columns saying "Phase 4", ... and dates below
#' @param phase_list vector: str, phase excel sheets whose order must match phase_dates_xlsx (???????)
#' @param services list: str, list of services we want in the schedule
#' @param rotating_services vector: str, services to rotate in daycare blocks (UNUSED)
#' @return Named list, first element is a vector of dates and
#'  the second element is a list of phase dates, where each phase
#'  is a list of services, and each service contains a list of days
#'  of the week + daycare, and a dataframe is present if a block is scheduled
#'
make_phase_sched <- function(phase_dates_xlsx, ## Excel file with phase dates
                             phase_list,
                             services,
                             rotating_services) {


  # dataframe where each header is a phase# and rows are the dates coinciding with the beginning(?) of phase date
  phase_dates_infile <- data.frame(read_excel(phase_dates_xlsx))

  # creates a list of dataframes, where each element is a phase#
  phase_sched_list <- lapply(as.list(phase_list), function(file_name) {
    in_file <- data.frame(read_excel(file_name))
    names(in_file)[1] <- "Service"
    return(in_file)
  })

  # assigns phase #s
  names(phase_sched_list) <- names(phase_dates_infile)

  # for each phase, for each service -> make_service_daily_block_mat
  daily_phase_scheds <- lapply(phase_sched_list, function(phase_infile) {
    service_schedules <- lapply(services, function(service) {
      make_service_daily_block_mat(
        service = service,
        phase_sched = phase_infile,
        rotating_services = rotating_services
      )
    })
    names(service_schedules) <- unlist(services)

    return(service_schedules)
  })

  # browser() DS

  out_phase_sched <- list(
    "Phase dates" = phase_dates_infile,
    "Blocks per service" = daily_phase_scheds
  )

  return(out_phase_sched)
}


prep_block_matrix <- function(block_matrix ## matrix of block availabilities
) {
  # parses and creates metadata for block matrix for use in scheduler

  block_matrix$relevant_weeks <- "all"

  block_matrix <- 1:nrow(block_matrix) %>%
    map_dfr(function(row) {
      bt <- block_matrix[row, ]$block_length
      comma_detected <- (stringr::str_detect(bt, "\\,"))
      # uses the comma to detect which weeks are relevant for the given block

      if (comma_detected) {
        block_matrix$relevant_weeks[[row]] <- as.character(stringr::str_extract_all(bt, "(?<=\\().+?(?=\\))"))
        block_matrix$block_length[[row]] <- stringr::str_split_fixed(bt, " ", n = 2)[, 1]
      }

      return(block_matrix[row, ])
    })

  block_matrix$block_length_mins <- as.numeric(block_matrix$block_length) * 60

  block_matrix$block_type <- paste0("T", block_matrix$block_length, "h")
  block_matrix$weekend <- FALSE
  block_matrix$daycare <- FALSE

  week_total_num_blocks <- sum(block_matrix$blocks_per_week)
  all_week_blocks <- unlist(sapply(
    1:length(block_matrix$block_type),
    function(i) rep(block_matrix$block_type[i], block_matrix$blocks_per_week[i])
  ))
  block_day_assignments <- rep(1:5, times = ceiling(week_total_num_blocks / 5))[1:week_total_num_blocks]
  blocks_per_day <- table(block_day_assignments)

  out_list <- list(
    "blocks_per_day" = blocks_per_day,
    "updated_block_matrix" = block_matrix
  )
  return(out_list)
}

#' Format daily block matrices per service
#' @param services str, the specific service
#' @param phase_sched data.frame, schedule of interest
#' @param rotating_services vector:str, services to rotate
#' @return List of .....
#' @examples
make_service_daily_block_mat <- function(service, phase_sched, rotating_services) {

  # for given service, subset phase sched
  service_sched <- phase_sched[grep(pattern = service, x = phase_sched[, 1]), ]


  days_of_week <- list("MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "DAYCARE")

  daily_sched <- lapply(days_of_week[1:5], function(day) {
    day_hours <- as.matrix(table(na.omit(service_sched[, day])))

    if (length(day_hours) > 0) { ## check that any blocks are scheduled
      block_mat <- prep_block_matrix(block_matrix = data.frame(blocks_per_week = day_hours, block_length = rownames(day_hours)))$updated_block_matrix
    } else { ## otherwise no block matrix
      block_mat <- NA
    }
    # print(block_mat)
    return(list(block_mat))
  })

  if (service %in% rotating_services) {
    daycare_sched <- phase_sched[grep(pattern = "daycare", x = phase_sched[, 1]), ]
    if (nrow(daycare_sched) > 0) {
      daily_sched[["DAYCARE"]] <- make_daycare_block_matrix(service, daycare_sched, rotating_services)
    } else {
      daily_sched[["DAYCARE"]] <- NA
    }
  } else {
    daily_sched[["DAYCARE"]] <- NA
  }

  # if (length(names(daily_sched)) != length(days_of_week)) {browser()}
  names(daily_sched) <- days_of_week[1:5]
  return(daily_sched)
}

### ----- for predicting surgery (?) time -----
# untouched so far
# called by run scheduler


pred_wtis_an_time <- function(wtis_dat, ## WAITLIST DATA
                              model_file, ## "CASE_TIMES_RF_LOGGED_20200701.rds", "PROC_TIMES_RF_LOGGED_20200701.rds", "AN_TIMES_RF_LOGGED_20200701.rds"
                              id_col = "ID",
                              surgeon_col = "Surgeon",
                              service_col = "Service",
                              proc_col = "Case.Procedures",
                              surg_pred_time_col = "Time",
                              patient_class_col = "Patient.Class",
                              logged_output = TRUE) {
  # browser()
  require(randomForest)

  pred_df <- prep_wtis_dat(
    wtis_dat = wtis_dat,
    id_col = "ID",
    surgeon_col = "Surgeon",
    service_col = "Service",
    proc_col = "Case.Procedures",
    surg_pred_time_col = "Time",
    patient_class_col = "Patient.Class"
  )

  pred_model <- readRDS(model_file)

  # browser()

  pred_out <- predict(object = pred_model, newdata = pred_df)

  if (logged_output) {
    pred_out <- exp(pred_out)
  }

  return(pred_out)
}



## PREDICT TIMES
prep_wtis_dat <- function(wtis_dat, ## waitlist data
                          id_col, ## id column name
                          surgeon_col, ## surgeon column name
                          service_col, ## service column name
                          proc_col, ## procedure column name
                          surg_pred_time_col, ## surgeon predicted procedure time column name
                          patient_class_col ## patient class column name
) {
  # browser()

  my_surg_df <- make_surgeon_df(
    case_id_vec = wtis_dat[, id_col],
    surgeon_vec = wtis_dat[, surgeon_col]
  )
  my_proc_df <- make_proc_df(
    case_id_vec = wtis_dat[, id_col],
    proc_vec = wtis_dat[, proc_col]
  )

  out_dat <- wtis_dat[, c(id_col, surgeon_col, service_col, surg_pred_time_col, patient_class_col)]

  out_dat$Service_fac <- factor(out_dat[, service_col], levels = c(
    "Orthopedics", "Plastics", "Gynaecology", "Neurosurgery",
    "Ophthalmology", "Urology", "General", "Dentistry", "Cardiovascular",
    "Otolaryngology", "Gastroenterology", "Oral Surgery", "Cardiology",
    "Rheumatology", "Pediatrics", "Diagnostic Imaging", "Haematology",
    "Trauma", "Anesthesia", "Respirology", "Neurology", "Dermatology", "Transplant"
  ))

  out_dat$Patient.Class_fac <- factor(out_dat[, service_col],
    levels = c(
      "Surgical Admit", "Outpatient Surgical Day Care", "Inpatient Acute",
      "Outpatient Ambulatory Care", "Outpatient Surgical Emergencies", "Outpatient Medical Day Care"
    )
  )
  out_dat$surg_pred_time <- out_dat[, surg_pred_time_col]

  sub_merg <- out_dat[, c(id_col, "Service_fac", "Patient.Class_fac", "surg_pred_time")]

  sub_with_proc <- merge(sub_merg, my_proc_df, by.x = id_col, by.y = "Case_ID")
  full_pred_df <- merge(sub_with_proc, my_surg_df, by.x = id_col, by.y = "Case_ID")

  return(full_pred_df)
}

## MAKE A 1-HOT DATA FRAME OF THE MOST COMMON SICKKIDS SURGEONS
make_surgeon_df <- function(case_id_vec, surgeon_vec) {
  surgeons <- c(
    "LEBEL", "CUSHING", "ALI", "CASAS", "BARRETT", "GARISTO",
    "PIERRO", "JAMES", "FORREST", "BAERTSCHIGER", "WALES", "HOWARD",
    "LORENZO", "PAPSIN", "AZZIE", "BAGLI", "CAMPISI", "DRAKE",
    "KELLEY", "KOYLE", "MALLIPATNA", "MIRESKANDARI", "MUNI", "NAJM-TEHRANI",
    "RUTKA", "WAN", "WOLTER", "PHILLIPS", "KWAN-WONG", "NARAYANAN",
    "FISH", "HOPYAN", "FISHER", "CLARKE", "BOUCHARD", "CARMICHAEL",
    "HADDAD", "CHEMALY", "GARBEDIAN", "IBRAHIM", "ZELLER", "SMITH",
    "WONG", "HEON", "PAPANIKOLAOU", "ALLEN", "DEANGELIS", "LEWIS",
    "CAMP", "FECTEAU", "VINCENT", "DAVIDGE", "PROPST", "BORSCHEL",
    "COLES", "BARRON", "SAYED", "HIMIDAN", "WOLINSKA", "CATTRAL",
    "CHIU", "LANGER", "KRAFT", "KULKARNI", "SUWWAN", "CHUNG",
    "DIRKS", "GHANEKAR", "FURLONGE", "GALLIE", "KERTES", "ZANI",
    "KIVES", "HONJO", "HALLER", "SPITZER", "REGINALD", "AGGARWAL",
    "MILLAR"
  )

  # browser()

  surg_df <- data.frame(matrix(0, nrow = length(case_id_vec), ncol = (length(surgeons) + 1)))
  names(surg_df) <- c("Case_ID", surgeons)
  surg_df$Case_ID <- case_id_vec

  for (surg in surgeons) {
    surg_df[grep(pattern = surg, x = toupper(surgeon_vec)), surg] <- 1
  }

  names(surg_df) <- gsub(pattern = "-", replacement = ".", names(surg_df))
  # names(full_pred_df) = gsub(pattern = "-",".",x = names(full_pred_df))

  return(surg_df)
}

## MAKE A 1-HOT DATAFRAME OF THE MOST COMMON SICKKIDS PROCEDURES
make_proc_df <- function(case_id_vec, proc_vec) {

  # browser()

  wtis_proc_codes <- unique(readRDS("wtis_proc_cods.rds"))
  proc_df <- data.frame(matrix(0, nrow = length(case_id_vec), ncol = (length(wtis_proc_codes) + 1)))
  names(proc_df) <- c("Case_ID", paste0("C", wtis_proc_codes))
  proc_df$Case_ID <- case_id_vec

  for (proc in wtis_proc_codes) {
    proc_df[grep(pattern = proc, x = toupper(proc_vec)), paste0("C", proc)] <- 1
  }


  return(proc_df)
}

