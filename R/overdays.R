#' Check for overdays in the daily or season data
#'
#' The \code{overdays} function checks Harvest Survey daily or season data to determine if total days hunted exceed the length of the state and species' season.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr tibble
#' @importFrom dplyr rename
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom dplyr n
#' @importFrom lubridate ymd
#' 
#' @param data Daily or season data table
#' @param ref_data The reference data table that corresponds to the year of the data
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}

overdays <-
  function(data, ref_data){
    suppressMessages(
      dates <- 
        wrangle_ref(ref_data) %>% 
        select(seasonyear, state = st, speciesgroup, open, close, spp) %>%
        filter(!is.na(spp) & !is.na(open) & !is.na(close)) %>% 
        group_by(seasonyear, state, spp) %>% 
        summarize(
          open = min(ymd(open), na.rm = T),
          close = max(ymd(close), na.rm = T)) %>%
        ungroup() %>% 
        left_join(
          tibble(
            state = datasets::state.abb,
            sampled_state = datasets::state.name),
          by = "state") %>% 
        select(-c("state", "seasonyear")) %>% 
        rename(sp_group_estimated = spp) %>% 
        # Calculate season length in days
        mutate(season_length = as.numeric(close - open)) %>% 
        select(-c("open", "close"))
    )
    
    # Duplicate the "Doves" lines so they apply to MODO and WWDO
    # Duplicate the "GeeseBrant" lines so they apply to Geese and Brant
    # Duplicate the "CootsGallinules" lines so they apply to Coots and 
    # Gallinules
    specialdates <-
      dates %>% 
      filter(sp_group_estimated == "MODO-WWDO") %>% 
      mutate(sp_group_estimated = "Mourning Dove") %>% 
      bind_rows(
        dates %>% 
          filter(sp_group_estimated == "MODO-WWDO") %>% 
          mutate(sp_group_estimated = "White-Winged Dove")) %>% 
      bind_rows(
        dates %>% 
          filter(sp_group_estimated == "GeeseBrant") %>% 
          mutate(sp_group_estimated = "Geese")) %>% 
      bind_rows(
        dates %>% 
          filter(sp_group_estimated == "GeeseBrant") %>% 
          mutate(sp_group_estimated = "Brant")) %>% 
      bind_rows(
        dates %>% 
          filter(sp_group_estimated == "CootsGallinules") %>% 
          mutate(sp_group_estimated = "Coots")) %>% 
      bind_rows(
        dates %>% 
          filter(sp_group_estimated == "CootsGallinules") %>% 
          mutate(sp_group_estimated = "Gallinules")) 
    
    # Remove specialdates spp from the original dates df
    dates <-
      dates %>% 
      filter(
        !sp_group_estimated %in% 
          c("MODO-WWDO", "GeeseBrant", "CootsGallinules")) %>% 
      bind_rows(specialdates) %>%
      distinct()
    
    if(TRUE %in% c(str_detect(deparse(substitute(data)), "daily"), 
                   str_detect(deparse(substitute(data)), "tibblelist\\[2\\]"))){
      # Daily records
      suppressMessages(
        overday_table <-
          data %>% 
          select(
            selected_hunterID, sampled_state, sp_group_estimated, 
            harvested_date) %>% 
          distinct() %>% 
          group_by(selected_hunterID, sampled_state, sp_group_estimated) %>% 
          summarize(n_days = n()) %>% 
          ungroup() %>% 
          left_join(
            dates,
            by = c("sp_group_estimated", "sampled_state")) %>% 
          filter(n_days >= season_length) %>% 
          rename(
            state = sampled_state,
            sp = sp_group_estimated)
      )
      
      if(nrow(overday_table > 0)){
        return(overday_table)
      }else{
        message("No records found with too many days hunted.")}
    }
    else if(
      TRUE %in% c(str_detect(deparse(substitute(data)), "season"),
                  str_detect(deparse(substitute(data)), "tibblelist\\[3\\]"))){
      # If a season totals table was used in this function, exclude daily
      # records from the season totals table. This allows the season totals data
      # to be evaluated separately from daily data
      if(str_detect(deparse(substitute(data)), "season") == TRUE){
        dataname <- deparse(substitute(data))
        
        data <- 
          data %>% 
          filter(
            !selected_hunterID %in%
              c(get(paste0(
                "daily_records_",
                str_extract(dataname, "[0-9]{4}")
              )) %>%
                select(selected_hunterID) %>%
                pull())
          )
        message("Notice: season data filtered to exclude daily records.")
      # Additional statement for report template compatibility
      }else{
        datayr <- 
          data %>% 
          select(season) %>% 
          distinct() %>% 
          pull()
        
        data <- 
          data %>% 
          filter(
            !selected_hunterID %in%
              c(get(paste0(
                "daily_records_",
                as.character(datayr)
              )) %>%
                select(selected_hunterID) %>%
                pull())
          )
        message("Notice: season data filtered to exclude daily records.")
      }
      
      # Season totals
      overday_table <-
        data %>% 
        select(
          selected_hunterID, sampled_state, sp_group_estimated, days_hunted) %>% 
        left_join(
          dates,
          by = c("sp_group_estimated", "sampled_state")) %>% 
        mutate(
          days_hunted = 
            ifelse(
              str_detect(days_hunted, "NULL"), 
              NA, 
              days_hunted),
          days_hunted = as.numeric(days_hunted)) %>% 
        filter(days_hunted >= season_length) %>%
        rename(
          state = sampled_state,
          sp = sp_group_estimated)
      
      if(nrow(overday_table > 0)){
        return(overday_table)
      }else{
        message("No records found with too many days hunted.")}
    }
    else{
      message(
        paste0(
          "Error: unrecognized `data`. Please supply a daily_records or ",
          "season_totals dataframe."))
    }
  }