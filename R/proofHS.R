#' Check for errors in daily and season data
#'
#' The \code{proofHS} function checks for overbag and overdays values in the Harvest Survey season data. In addition to overbag and overdays, daily data are checked for early and late hunts.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr rename_all
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
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
#' @importFrom stringr str_remove
#' @importFrom lubridate ymd
#' 
#' @param data Daily or season data table
#' @param ref_data The reference data table that corresponds to the year of the daily data
#' 
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}

proofHS <-
  function(data, ref_data){
    
    # {Part 1}
    # Format over bags ref table
    suppressWarnings(
      ref_table <-
        wrangle_ref(ref_data) %>%
        filter(!is.na(spp) & !is.na(bag)) %>% 
        select(seasonyear, state = st, speciesgroup, spp, bag) %>% 
        group_by(seasonyear, state, spp) %>% 
        summarize(max_bag = max(bag, na.rm = T)) %>% 
        ungroup() %>% 
        left_join(
          tibble(
            state = datasets::state.abb,
            sampled_state = datasets::state.name),
          by = "state") %>% 
        select(-c("state", "seasonyear")) %>%
        rename(sp_group_estimated = spp)
    )
    
    # Duplicate the "Doves" lines so they apply to MODO and WWDO
    # Duplicate the "CootsGallinules" lines so they apply to Coots and 
    # Gallinules
    special_table <-
      ref_table %>% 
      filter(sp_group_estimated == "MODO-WWDO") %>% 
      mutate(sp_group_estimated = "Mourning Dove") %>% 
      bind_rows(
        ref_table %>% 
          filter(sp_group_estimated == "MODO-WWDO") %>% 
          mutate(sp_group_estimated = "White-Winged Dove")) %>% 
      bind_rows(
        ref_table %>% 
          filter(sp_group_estimated == "CootsGallinules") %>% 
          mutate(sp_group_estimated = "Coots")) %>% 
      bind_rows(
        ref_table %>% 
          filter(sp_group_estimated == "CootsGallinules") %>% 
          mutate(sp_group_estimated = "Gallinules")) 
    
    # Remove specialdates spp from the original dates df
    ref_table <-
      ref_table %>% 
      filter(
        !sp_group_estimated %in% 
          c("MODO-WWDO", "CootsGallinules")) %>% 
      bind_rows(special_table) %>%
      distinct()
    
    # {Part 2}
    # Format over days ref table
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
        mutate(season_length = as.numeric(close - open))
    )
    
    # Duplicate the "Doves" lines so they apply to MODO and WWDO
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
          c("MODO-WWDO", "CootsGallinules")) %>% 
      bind_rows(specialdates) %>%
      distinct()
    
    if(
      nrow(
        dates %>% 
        group_by(sp_group_estimated, sampled_state) %>% 
        filter(n() > 1)) > 0){
      message(
        paste0(
          "Warning: More than one season length detected for a given species a",
          "nd state."))
      print(
        dates %>% 
          group_by(sp_group_estimated, sampled_state) %>% 
          filter(n() > 1) %>% 
          ungroup())
    }
    
    # {Part 3}
    # Error flagging
    
    if(str_detect(deparse(substitute(data)), "season") == TRUE | 
       str_detect(deparse(substitute(data)), "tibblelist\\[3\\]") == TRUE){
      
      # Season data error flagging: overbags and overdays
      season_errors <- 
        data %>% 
        left_join(
          ref_table,
          by = c("sp_group_estimated", "sampled_state")) %>% 
        left_join(
          dates,
          by = c("sp_group_estimated", "sampled_state")) %>% 
        mutate(
          days_hunted = 
            ifelse(
              str_detect(days_hunted, "NULL"),
              NA,
              as.numeric(days_hunted)),
          retrieved = 
            ifelse(
              str_detect(retrieved, "NULL"),
              NA,
              as.numeric(retrieved)),
          bag_per_day = 
            ifelse(
              days_hunted > 0,
              round(as.numeric(retrieved)/as.numeric(days_hunted), 1),
              0),
          errors = "x") %>% 
        group_by(surveyID, selected_hunterID, sp_group_estimated) |> 
        mutate(
          sum_retrieved = sum(retrieved, na.rm = T), 
          avg_retrieved_over_season = sum_retrieved/season_length) |> 
        ungroup() |> 
        # Flag overbags
        mutate(
          errors = 
            ifelse(
              avg_retrieved_over_season > max_bag,
              paste(errors, "overbag", sep = "-"),
              errors),
          overbag = 
            ifelse(
              avg_retrieved_over_season > max_bag,
              round(avg_retrieved_over_season - max_bag, 2),
              NA)) %>% 
        # Flag overdays
        mutate(
          errors = 
            ifelse(
              days_hunted > season_length,
              paste(errors, "overdays", sep = "-"),
              errors),
          overday = 
            ifelse(
              days_hunted > season_length,
              as.numeric(days_hunted) - as.numeric(season_length),
              NA)) %>% 
        # Remove the x from errors
        mutate(
          errors =
            ifelse(
              errors == "x" | (days_hunted == 0 & retrieved == 0) | 
                (is.na(days_hunted) & is.na (retrieved)),
              "none",
              str_remove(errors, "^x\\-"))) %>%
        select(
          -c("max_bag", "season_length", "bag_per_day", 
             "avg_retrieved_over_season")) 
      
      if(nrow(filter(season_errors, is.na(errors))) > 0){
        message("Warning: Not all species matched.")
        
        print(
          season_errors %>% 
            filter(is.na(errors)) %>% 
            select(surveyID, sampled_state, sp_group_estimated, retrieved)
        )
      } 
      
      return(season_errors)
      
    }else if(str_detect(deparse(substitute(data)), "daily|party") == TRUE){
      
      # Daily data error flagging: overbags, overdays, early hunts, late hunts
      
      daily_errors <- 
        data %>%
        left_join(
          ref_table,
          by = c("sp_group_estimated", "sampled_state")) %>% 
        left_join(
          dates,
          by = c("sp_group_estimated", "sampled_state")) %>% 
        group_by(selected_hunterID, sampled_state, sp_group_estimated) %>% 
        mutate(n_days = n()) %>% 
        ungroup() %>% 
        mutate(
          harvested_date = ymd(harvested_date),
          errors = "x",
          overbag = NA,
          overday = NA,
          early = NA,
          late = NA) %>% 
        # Flag overbags
        mutate(
          errors = 
            ifelse(
              # Overbag error if max_bag + 2 is exceeded 
              retrieved > max_bag + 2,
              paste(errors, "overbag", sep = "-"),
              errors),
          overbag = 
            ifelse(
              retrieved > max_bag + 2,
              retrieved - max_bag,
              NA)) %>% 
        # Flag overdays
        mutate(
          errors = 
            ifelse(
              n_days > season_length,
              paste(errors, "overdays", sep = "-"),
              errors),
          overday = 
            ifelse(
              n_days > season_length,
              n_days - season_length,
              NA)) %>% 
        # Flag early hunts
        mutate(
          errors = 
            ifelse(
              harvested_date < open,
              paste(errors, "early_hunt", sep = "-"),
              errors),
          early = 
            ifelse(
              harvested_date < open,
              open - harvested_date,
              NA)) %>% 
        # Flag late hunts
        mutate(
          errors = 
            ifelse(
              harvested_date > close,
              paste(errors, "late_hunt", sep = "-"),
              errors),
          late = 
            ifelse(
              harvested_date > close,
              harvested_date - close,
              NA)) %>% 
        # Join in seaduck/brant reference table
        left_join(
          seaduck_counties |> 
            select(sampled_state = STATE, county = COUNTY, brant, seaduck) |> 
            filter(county != "Unknown"),
          by = c("sampled_state", "county")
        ) |> 
        # Flag non-seaduck counties
        mutate(
          errors = 
            ifelse(
              is_SeaDuck == "Y" & seaduck == "N",
              paste(errors, "badseaduck", sep = "-"),
              errors)) |> 
        # Flag non-brant counties
        mutate(
          errors = 
            ifelse(
              is_Brant == "Y" & brant == "N",
              paste(errors, "badbrant", sep = "-"),
              errors)) |>
        # Remove the x from errors
        mutate(
          errors =
            ifelse(
              errors == "x" | (n_days == 0 & retrieved == 0),
              "none",
              str_remove(errors, "^x\\-"))) %>%
        select(
          -c("max_bag", "season_length", "n_days", "open", "close", "brant", 
             "seaduck"))
      
      if(nrow(filter(daily_errors, is.na(errors))) > 0){
        message("Warning: Not all species matched.")
        
        print(
          daily_errors %>% 
            filter(is.na(errors)) %>% 
            select(surveyID, sampled_state, sp_group_estimated, retrieved)
        )
      } 
      
      return(daily_errors)  
      
    }else{
      message(
        "Error: `data` must be season_totals or daily_records data frame.")
    }
  }