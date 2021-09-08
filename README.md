
## POS Scheduler

All credits to Lauren Erdman for the original code.

## Modifications

Generally speaking, some modifications I've made include 
- general re-factoring for logic
- accounting for a block schedule where there can be different blocks depending on the week of month
- surgeon schedule 
- unit and daily target constraints, required complete re-factor of make_multi_perservice_sched()
- first surgeon to block, gets the block 
- make_multi_perservice_sched() -> make_sched_all_services(), which creates and retains a list of all services and schedules at once as opposed to a schedule by service
## Running the Scheduler

See examples in `test_constraints/`

```
 sched <- run_scheduler(
            wtis_in = wtis_in,
            sub_n = NULL,
            case_ids = NULL,
            phase_dates_xlsx = phase_dates_xlsx,
            phase_list_xlsx = phase_list_xlsx,
            services = services,
            start_date = start_date, ## Make start date nearest Monday.∏
            verbose_run = TRUE,
            turnover_buffer = turnover_buffer,
            time = "surgeon",
            rotating_services = c("Opthalmology", "Orthopaedics", "Urology", "Otolaryngology"),
            add_cases = FALSE, 
            high_pri_prop = 0.5,
            home_only = FALSE,
            max_time = 180,
            surg_opts = "first",  # "first" or "any", the former will schedule the first surgeon to a block 
            post_op_opts = "all", # whether to schedule service by service, or consider all at once'service' or 'all'
            unit_opts = unit_targets, # ensure this named vector contains all post op destinations found in the waitlist
            pacu_opts = pacu_target,
            surg_sched = surg_sched, # named vector of days of week with surgeon names as found in waitlist
        )
```