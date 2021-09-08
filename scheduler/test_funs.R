create_fake_surgeon_sched <- function(waitlist_file = wtis_in, phase_list_file = phase_list_xlsx[[1]], type){
  "
  create a fake surgeon schedule using the waitlist, where each surgeon's preferred dow is pretty much the days they're scheduled to work
  this guarantees an input schedule that is compatible 
  
  type: distinct, compatible, incompatible (grows indefinitely b/c no suitable slots)
  "
  wtis_tbl <- read_excel(waitlist_file)
  block_spread <- read_excel(phase_list_file)
  dow_services_block <- block_spread %>%
    rename("Service" = "...1") %>%
    tidyr::gather(key = "Weekday", value = "hours", -c(Service)) %>%
    na.omit() %>%
    mutate(Service = stringr::str_replace(Service, ' [0-9]', ''),
           Weekday = str_to_lower(Weekday),
           Weekday = str_to_title(Weekday))
  
  # create the surgeon schedule based off waiting list
  surg_sched_tbl <- wtis_tbl %>% distinct(Surgeon, Service) %>%
    mutate(Service = ifelse(Service == 'Orthopedics', 'Orthopaedics', Service)) %>%
    left_join(dow_services_block)
  
  surg_sched <- surg_sched_tbl %>%
    distinct(Service, Surgeon, Weekday) %>%
    # na.omit() %>%
    group_by(Weekday) %>%
    group_split() %>%
    map(function(df){
      weekday = c(unique(df$Weekday))
      surgeon = list(df$Surgeon)
      names(surgeon) = weekday
      return(surgeon)
    }) %>%
    flatten()
  
  if(type == "compatible"){
    
  } else if (type == "distinct"){
    
    # create a schedule where each surgeon is distinct wrt each eay of week
    surg_pref_tbl <- tibble::enframe(surg_sched) %>% tidyr::unnest(value)
    distinct_surg_pref_tbl <- surg_pref_tbl %>% 
      filter(name != "") %>%
      .[sample(nrow(.)), ] %>%
      group_by(value) %>% distinct(name) %>% sample() %>%
      filter(row_number() == 1) 
    
    surg_sched <- distinct_surg_pref_tbl %>% rename('Weekday' = 'name', 'Surgeon' = 'value') %>% group_by(Weekday) %>% group_split() %>% 
      map(function(df){
        weekday = c(unique(df$Weekday))
        surgeon = list(df$Surgeon)
        names(surgeon) = weekday
        return(surgeon)
      }) %>%
      flatten()
    
  } else if (type == "incompatible"){
    
    
    set.seed(2021)
    surgeon_vec <- wtis_tbl$Surgeon %>%
      unique() %>%
      sample()
    # https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
    chunk2 <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))
    surg_sched <- chunk2(surgeon_vec, n = 5)
    names(surg_sched) <- c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday"
    )
  } else{
    stop('Please ensure surgeon_sched is one of distinct, compatible, or incompatible.')
  }
  
  return(surg_sched)
}

validate_input_surgeon_schedule <- function(waitlist_file = wtis_in, phase_list_file = phase_list_xlsx[[1]], surgeon_schedule = surg_sched){
  "
  validates an input surgeon schedule using the waitlist (to check which services the surgeon belong to), and a tidied block schedule
  loops through each surgeon to get the service they belong to, then for each service checks the surgeon's schedule (days of week) against
  the corresponding look-up table 'dow_services_block' containing service-days of week, raising an error if a mismatch is found.
  
  "
  surg_pref_tbl <- tibble::enframe(surgeon_schedule) %>% tidyr::unnest(value)
  
  # testing
  {
    wtis_tbl <- read_excel(waitlist_file)
    block_spread <- read_excel(phase_list_file)
    
    # tidying the block schedule
    dow_services_block <- block_spread %>%
      rename("Service" = "...1") %>%
      tidyr::gather(key = "Weekday", value = "hours", -c(Service)) %>%
      na.omit() %>%
      mutate(Service = stringr::str_replace(Service, ' [0-9]', ''),
             Weekday = str_to_lower(Weekday),
             Weekday = str_to_title(Weekday))
    
    # create the surgeon schedule based off waiting list
    surg_sched_tbl <- wtis_tbl %>% distinct(Surgeon, Service) %>%
      mutate(Service = ifelse(Service == 'Orthopedics', 'Orthopaedics', Service)) #%>%
    # left_join(dow_services_block)
  }
  
  
  surgeon_vec <- surg_pref_tbl %>% 
    # na.omit() %>% 
    filter(name != "") %>%
    pull(value) %>% 
    unique()
  
  # loops through each surgeon to get the service they belong to, then for each service checks the surgeon's schedule (days of week) against
  # the corresponding look-up table 'dow_services_block' containing service-days of week
  
  surgeon_vec %>% 
    map(function(surgeon){
      print(surgeon)
      
      # surg_pref_tbl %>% filter(value == surgeon) 
      
      # get services for surgeon, in waitlist
      s <- surg_sched_tbl %>% filter(Surgeon == surgeon) %>% pull(Service) %>% unique()
      print(s)
      
      # check block schedule to see dow availability of service 
      dow_subset <- dow_services_block %>% filter(Service %in% s)
      
      # get surgeons preferred days of week
      dow_pref <- surg_pref_tbl %>% filter(value == surgeon) %>% pull(name)
      
      print(dow_pref)
      print(dow_subset$Weekday)
      
      if (!all(dow_pref %in% dow_subset$Weekday)){
        stop(paste("Please check that surgeon '", surgeon,"' with preferred days '", paste0(dow_pref, collapse = ", "), "', and who belongs to service '", s,
                   "' line up with the services of the block schedule, which are '", paste0(dow_subset$Weekday, collapse = ", " ), "'"))
      }
      return(dow_subset)
    })
}