
# ---- plot ---
## plot the number of cases seen in the block schedule by week
plot_cases_by_week <- function(block_list) {
  n_cases_per_day <- unlist(lapply(block_list, function(x) lapply(x, function(y) length(unlist(lapply(y[["procedure_blocks"]], function(z) z$case_ids))))))
  days <- 1:length(n_cases_per_day)
  total_cases <- sum(n_cases_per_day)

  cases_per_day <- c()
  week <- c()
  for (my_week in 1:length(block_list)) {
    cases_per_day <- c(cases_per_day, unlist(lapply(block_list[[my_week]], function(y) length(unlist(lapply(y[["procedure_blocks"]], function(z) z$case_ids))))))
    print(cases_per_day)
    week <- c(week, rep(my_week, length(cases_per_day)))
    print(week)
  }
  days <- 1:length(week)
  print(days)

  cases_burn_down <- c()
  for (day in 1:length(days)) {
    cases_burn_down <- c(cases_burn_down, total_cases - sum(cases_per_day[1:day]))
  }

  burn_down_df <- data.frame(cases_burn_down, days, week)

  ## ggplot
  require(ggplot2)

  theme_set(
    theme_light(base_size = 20)
  )
  ggplot(burn_down_df, aes(x = days, y = cases_burn_down)) +
    geom_line() +
    xlab("Days") +
    ylab("Number of cases") +
    scale_x_continuous(sec.axis = sec_axis(trans = ~ . / 7, name = "Weeks"))

  ### plot days of week in each facet

  # + scale_x_continuous(sec.axis = ~ ./7, name = "Weeks")

  # par(mar=c(6,3,3,6))
  # plot(y = cases_burn_down,x = days,
  #      type = 'l',bty = "n",xlab = "",axes = FALSE)
  # axis(2,seq(0,max(cases_burn_down),by = 50),line=0)
  # axis(1,seq(0,length(days),by = 5),line=1,col="black",col.ticks="black",col.axis="black")
  # mtext("Days",1,line=2,at=1,col="black")
  # at_vec = c(seq(0,max(days),by = 7), max(seq(0,max(days),by = 7)) + 7)
  # axis(1,at_vec,labels=(0:(length(seq(0,max(days),by = 7)))),line=3,col="blue",col.ticks="blue",col.axis="blue")
  # mtext("Weeks",1,line=4,at=0,col="blue")

  ## use days axis for now
  ## add weeks axis as well
}

## plot the number of cases left to see in waitlist for each block schedule in a list
plot_multi_burndown <- function(list_of_scheds) {
  sched_id <- c()
  plot_df_cases_per_day <- c()
  plot_df_cases_burn_down <- c()
  plot_df_week <- c()
  plot_df_days <- c()

  for (sched in 1:length(list_of_scheds)) {

    # n_cases_per_day = unlist(lapply(block_list,function(x)lapply(x,function(y)length(unlist(lapply(y[["procedure_blocks"]],function(z)z$case_ids))))))
    # days = 1:length(n_cases_per_day)
    # total_cases = sum(n_cases_per_day)

    block_list <- list_of_scheds[[sched]]

    days <- c()
    cases_per_day <- c()
    cases_burn_down <- c()
    week <- c()

    for (my_week in 1:length(block_list)) {
      weekly_cases_per_day <- unlist(lapply(block_list[[my_week]], function(y) length(unlist(lapply(y[["procedure_blocks"]], function(z) z$case_ids)))))
      cases_per_day <- c(cases_per_day, weekly_cases_per_day)
      # print(cases_per_day)
      week <- c(week, rep(my_week, length(weekly_cases_per_day)))
      # print(week)
    }
    days <- 1:length(cases_per_day)
    # print(days)

    total_cases <- sum(cases_per_day)

    for (day in 1:length(days)) {
      cases_burn_down <- c(cases_burn_down, total_cases - sum(cases_per_day[1:day]))
    }
    sched_id <- c(sched_id, rep(sched, length(cases_per_day) + 1))
    plot_df_cases_per_day <- c(plot_df_cases_per_day, c(0, cases_per_day))
    plot_df_cases_burn_down <- c(plot_df_cases_burn_down, c(total_cases, cases_burn_down))
    plot_df_week <- c(plot_df_week, c(0, week))
    plot_df_days <- c(plot_df_days, c(0, days)) ## start at 0
  }

  burn_down_df <- data.frame(sched_id,
    cases_per_day = plot_df_cases_per_day,
    case_burn_down = plot_df_cases_burn_down,
    day = plot_df_days,
    week = plot_df_week
  )

  burn_down_df$sched_id <- factor(burn_down_df$sched_id)

  print(burn_down_df)

  ## ggplot
  require(ggplot2)

  theme_set(
    theme_light(base_size = 20)
  )
  ggplot(burn_down_df, aes(x = day, y = case_burn_down, col = sched_id)) +
    geom_line(aes(col = sched_id)) +
    xlab("Days") +
    ylab("Cases left on list") +
    scale_x_continuous(sec.axis = sec_axis(trans = ~ . / 5, name = "Weeks"))
}

## can't remember what this does but you can ignore it
plot_multi_burndown_with_ests <- function(list_of_scheds,
                                          est_weeks = 3) {
  sched_id <- c()
  plot_df_cases_per_day <- c()
  plot_df_cases_burn_down <- c()
  plot_df_week <- c()
  plot_df_days <- c()

  for (sched in 1:length(list_of_scheds)) {

    # n_cases_per_day = unlist(lapply(block_list,function(x)lapply(x,function(y)length(unlist(lapply(y[["procedure_blocks"]],function(z)z$case_ids))))))
    # days = 1:length(n_cases_per_day)
    # total_cases = sum(n_cases_per_day)

    block_list <- list_of_scheds[[sched]]$sched_list
    rest_of_cases <- list_of_scheds[[sched]]$case_list

    days <- c()
    cases_per_day <- c()
    cases_burn_down <- c()
    week <- c()

    for (my_week in 1:length(block_list)) {
      weekly_cases_per_day <- unlist(lapply(block_list[[my_week]], function(y) length(unlist(lapply(y[["procedure_blocks"]], function(z) z$case_ids)))))
      cases_per_day <- c(cases_per_day, weekly_cases_per_day)
      # print(cases_per_day)
      week <- c(week, rep(my_week, length(weekly_cases_per_day)))
      # print(week)
    }
    days <- 1:length(cases_per_day)
    # print(days)

    total_cases <- sum(cases_per_day)

    for (day in 1:length(days)) {
      cases_burn_down <- c(cases_burn_down, total_cases - sum(cases_per_day[1:day]))
    }

    cases_burn_down <- cases_burn_down + nrow(rest_of_cases)
    time_remaining <- get_time_left(
      block_list = block_list,
      remaining_cases = rest_of_cases,
      estimate = TRUE,
      verbose = FALSE,
      weeks_to_est = est_weeks
    )

    sched_id <- c(sched_id, rep(sched, length(cases_per_day) + 2))
    plot_df_cases_burn_down <- c(plot_df_cases_burn_down, (total_cases + nrow(rest_of_cases)), cases_burn_down, 0)
    plot_df_days <- c(plot_df_days, 0, days, time_remaining) ## start at 0
  }

  burn_down_df <- data.frame(sched_id,
    case_burn_down = plot_df_cases_burn_down,
    day = plot_df_days
  )

  burn_down_df$sched_id <- factor(burn_down_df$sched_id)

  head(burn_down_df)

  ## ggplot
  require(ggplot2)

  theme_set(
    theme_light(base_size = 20)
  )
  ggplot(burn_down_df, aes(x = day, y = case_burn_down, col = sched_id)) +
    geom_line(aes(col = sched_id)) +
    xlab("Days") +
    ylab("Cases left on list") +
    scale_x_continuous(sec.axis = sec_axis(trans = ~ . / 5, name = "Weeks")) +
    scale_y_continuous(
      breaks = seq(0, max(burn_down_df$case_burn_down) + 500, 500),
      minor_breaks = seq(0, max(burn_down_df$case_burn_down) + 500, 100)
    )
}


## PLOT BURNDOWN PER SERVICE and include phase dates
plot_burndown_per_service <- function(in_sched_raw, phase_dates_xlsx = "PhaseDates_v2_20201103.xlsx") {
  phase_dates_infile <- c(data.frame(read_excel(phase_dates_xlsx))[1, ])

  # browser()
  # my_sched = in_sched_raw$sched

  plot_df <- get_spreadsheet_per_service_sched(in_sched_raw = in_sched_raw)

  require(ggplot2)

  theme_set(
    theme_light(base_size = 20)
  )
  ggplot(plot_df$day_df, aes(x = date, y = remaining_cases)) +
    geom_line(lwd = 0.8, aes(col = services, lty = services), alpha = 0.8) +
    # geom_point(aes(col = services), alpha = 0.8) + 
    xlab("Date") +
    ylab("Number of Cases on List") +
    geom_vline(xintercept = as.Date(phase_dates_infile$"Phase4"), col = "red", lty = 2, lwd = 1.2) +
    geom_vline(xintercept = as.Date(phase_dates_infile$"Phase5"), col = "red", lty = 2, lwd = 1.2) +
    geom_vline(xintercept = as.Date(phase_dates_infile$"Phase6"), col = "red", lty = 2, lwd = 1.2) +
    geom_vline(xintercept = as.Date(phase_dates_infile$"Phase7"), col = "red", lty = 2, lwd = 1.2) +
    geom_vline(xintercept = as.Date(phase_dates_infile$"Phase8"), col = "red", lty = 2, lwd = 1.2)   +
    geom_vline(xintercept = as.Date(phase_dates_infile$"Phase9"), col = "red", lty = 2, lwd = 1.2)
}

## plot number of patients going to each post-op destination
plot_post_op_dest <- function(day_df,
                              end_date = NULL) {
  picu_oicu_counts <- sapply(sort(unique(day_df$date)), function(x) {
    sum(day_df$picu_oicu[day_df$date == x])
  })
  const_obs_counts <- sapply(sort(unique(day_df$date)), function(x) {
    sum(day_df$constant_obs[day_df$date == x])
  })
  inpatient_counts <- sapply(sort(unique(day_df$date)), function(x) {
    sum(day_df$in_patient[day_df$date == x])
  })
  short_stay_counts <- sapply(sort(unique(day_df$date)), function(x) {
    sum(day_df$short_stay[day_df$date == x])
  })
  cccu_counts <- sapply(sort(unique(day_df$date)), function(x) {
    sum(day_df$cccu[day_df$date == x])
  })

  counts <- c(
    picu_oicu_counts,
    const_obs_counts,
    inpatient_counts,
    short_stay_counts,
    cccu_counts
  )
  labels <- c(
    rep("picu_oicu", length(picu_oicu_counts)),
    rep("const_obs", length(const_obs_counts)),
    rep("inpatient", length(inpatient_counts)),
    rep("short_stay", length(short_stay_counts)),
    rep("cccu", length(cccu_counts))
  )
  post_op_days <- rep(unique(dfs_list$day_df$date), 5)

  post_op_df <- data.frame(counts, labels, post_op_days)
  post_op_df$labels_fac <- factor(post_op_df$labels,
    levels = c("picu_oicu", "const_obs", "inpatient", "short_stay", "cccu"),
    labels = c("PICU/OICU", "Constant Obs", "In-patient", "Short Stay", "CCCU")
  )

  if (is.null(end_date)) {
    ggplot(post_op_df, aes(x = post_op_days, y = counts, fill = labels_fac)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_grid(~labels_fac) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      xlab("Date") +
      ylab("Number of patients") +
      scale_fill_manual(values = c("PICU/OICU" = "orangered", "Constant Obs" = "deeppink2", "In-patient" = "darkorchid", "Short Stay" = "seagreen", "CCCU" = "dodgerblue"), name = "Post-Op \nDestination")
  } else {
    post_op_df_2mo <- post_op_df[post_op_df$post_op_days < "2020-09-01", ]
    head(post_op_df_2mo)

    ggplot(post_op_df_2mo, aes(x = post_op_days, y = counts, fill = labels_fac)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_grid(~labels_fac) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      xlab("Date") +
      ylab("Number of patients") +
      scale_fill_manual(values = c("PICU/OICU" = "orangered", "Constant Obs" = "deeppink2", "In-patient" = "darkorchid", "Short Stay" = "seagreen", "CCCU" = "dodgerblue"), name = "Post-Op \nDestination")
  }
}

## plot the length of the blocks vs the amount of time booked within them
boxplot_block_times_vs_booked <- function(my_block_df, ## dataframe from block schedule, output by function: get_spreadsheet_per_service_sched
                                          make_boxplot = TRUE) {
  # browser()
  my_block_df <- my_block_df[complete.cases(my_block_df), ]
  my_block_df$block_type_length <- as.numeric(substr(x = as.character(my_block_df$block_type), start = 2, stop = nchar(as.character(my_block_df$block_type)) - 1)) * 60
  my_block_df$block_length_hrs <- my_block_df$block_length / 60

  names(my_block_df)[3] <- "Date"

  ggplot(my_block_df, aes(x = block_type_length, y = block_length)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0)

  if (make_boxplot) {
    ggplot(my_block_df, aes(y = block_length_hrs, x = block_type)) +
      geom_boxplot(aes(fill = block_type)) +
      scale_fill_manual(values = c(
        "T4h" = "black",
        "T5h" = "blue", "T6h" = "dodgerblue",
        "T7.5h" = "deeppink2", "T8.5h" = "deeppink4",
        "T9.5h" = "darkorchid4"
      ), name = "Block") +
      xlab("Block") +
      ylab("Block cases length (hours)")
  }
}

## plot inpatient post-op destinations
plot_inpt_post_op_dest <- function(day_df, dow = TRUE) {
  require(ggplot2)

  if (dow) {
    day_df$dow <- weekdays(day_df$date)

    fiveA <- sapply(sort(unique(day_df$dow)), function(x) {
      mean(day_df$fiveA[day_df$dow == x])
    })
    fiveAsss <- sapply(sort(unique(day_df$dow)), function(x) {
      mean(day_df$fiveAsss[day_df$dow == x])
    })
    fiveB <- sapply(sort(unique(day_df$dow)), function(x) {
      mean(day_df$fiveB[day_df$dow == x])
    })
    fiveCsd <- sapply(sort(unique(day_df$dow)), function(x) {
      mean(day_df$fiveCsd[day_df$dow == x])
    })
    eightC <- sapply(sort(unique(day_df$dow)), function(x) {
      mean(day_df$eightC[day_df$dow == x])
    })
  } else {
    fiveA <- sapply(sort(unique(day_df$date)), function(x) {
      sum(day_df$fiveA[day_df$date == x])
    })
    fiveAsss <- sapply(sort(unique(day_df$date)), function(x) {
      sum(day_df$fiveAsss[day_df$date == x])
    })
    fiveB <- sapply(sort(unique(day_df$date)), function(x) {
      sum(day_df$fiveB[day_df$date == x])
    })
    fiveCsd <- sapply(sort(unique(day_df$date)), function(x) {
      sum(day_df$fiveCsd[day_df$date == x])
    })
    eightC <- sapply(sort(unique(day_df$date)), function(x) {
      sum(day_df$eightC[day_df$date == x])
    })
  }

  counts <- c(fiveA, fiveAsss, fiveB, fiveCsd, eightC)
  labels <- c(
    rep("5A", length(fiveA)),
    rep("5ASSS", length(fiveAsss)),
    rep("5B", length(fiveB)),
    rep("5CSD", length(fiveCsd)),
    rep("8C", length(eightC))
  )
  if (dow) {
    post_op_days <- rep(sort(unique(day_df$dow)), 5)
  } else {
    post_op_days <- rep(unique(day_df$date), 5)
  }

  post_op_df <- data.frame(counts, labels, post_op_days)
  post_op_df$labels_fac <- factor(post_op_df$labels,
    levels = c("5A", "5ASSS", "5B", "5CSD", "8C"),
    labels = c("5A Constant Obs", "5A Surgical Short Stay", "5B Constant Obs", "5C Stepdown", "8C Constant Obs")
  )

  if (dow) {
    post_op_df$dow_f <- factor(post_op_df$post_op_days, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    ggplot(post_op_df, aes(x = dow_f, y = counts, fill = labels_fac)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_grid(~labels_fac) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      xlab("Day of the week") +
      ylab("Number of patients") +
      scale_fill_manual(values = c(
        "5A Constant Obs" = "orangered",
        "5A Surgical Short Stay" = "deeppink2",
        "5B Constant Obs" = "darkorchid",
        "5C Stepdown" = "seagreen",
        "8C Constant Obs" = "dodgerblue"
      ), name = "Post-Op \nDestination")
  } else {
    ggplot(post_op_df, aes(x = post_op_days, y = counts, fill = labels_fac)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_grid(~labels_fac) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      xlab("Date") +
      ylab("Number of patients") +
      scale_fill_manual(values = c(
        "5A Constant Obs" = "orangered",
        "5A Surgical Short Stay" = "deeppink2",
        "5B Constant Obs" = "darkorchid",
        "5C Stepdown" = "seagreen",
        "8C Constant Obs" = "dodgerblue"
      ), name = "Post-Op \nDestination")
  }
}

