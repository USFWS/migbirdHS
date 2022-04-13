#' Check for errors in daily and season data
#'
#' The \code{proofHS} function checks for overbag and overdays values in the Harvest Survey season data. In addition to overbag and overdays, daily data are checked for early and late hunts.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr rename_all
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr tibble
#' @importFrom dplyr rename
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom lubridate mdy
#' @importFrom stringr str_remove
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
        ref_data %>% 
        rename_all(~tolower(.)) %>%
        filter(st != "PR" & st != "HI") %>% 
        filter(seasontype != "ExFalc") %>% 
        filter(!str_detect(speciesgroup, "Swan")) %>% 
        mutate(
          speciesgroup = 
            case_when(
              species == "Brant" ~ "Brant",
              str_detect(species, "MODO-WWDO") ~ "MODO-WWDO",
              TRUE ~ speciesgroup)) %>% 
        mutate(
          spp = 
            case_when(
              str_detect(speciesgroup, "Sea") ~ "Specially Regulated Sea Ducks",
              str_detect(speciesgroup, "Crane") ~ "Sandhill Crane",
              speciesgroup == "Brant" ~ "Brant",
              speciesgroup == "CAGO" ~ "Geese",
              speciesgroup == "Geese" ~ "Geese",
              speciesgroup == "Ducks" ~ "Ducks",
              speciesgroup == "AMWO" ~ "Woodcock",
              speciesgroup == "COSN" ~ "Snipe",
              speciesgroup == "MODO" ~ "Mourning Dove",
              speciesgroup == "BTPI" ~ "Band-tailed Pigeon",
              speciesgroup == "Mergansers" ~ "Ducks",
              speciesgroup == "Rails" ~ "Rails",
              speciesgroup == "COMO-PUGA" ~ "Gallinules",
              # For NM "AMCO-COMO", set as "Coots" (they have a separate
              # speciesgroup for "COMO-PUGA" that becomes "Gallinules", above)
              speciesgroup == "AMCO-COMO" & st == "NM" ~ "Coots", 
              # **The "MODO-WWDO" category below should be used for MODO and WWDO
              speciesgroup == "MODO-WWDO" ~ "MODO-WWDO",
              speciesgroup == "MODO-WWDO-WTDO" ~ "MODO-WWDO",
              # **The NM "CAGO-CACG-Brant" category should apply to "Geese" AND
              # "Brant"
              speciesgroup == "CAGO-CACG-Brant" ~ "GeeseBrant",
              # **For AZ, CA, MN, and NV: the "AMCO-COMO" category should apply to
              # "Coots" AND "Gallinules"
              speciesgroup == "AMCO-COMO" & st %in% c("AZ", "CA", "MN", "NV") ~ 
                "CootsGallinules", 
              speciesgroup %in% c("Coots", "COOTS", "AMCO") ~ "Coots",
              TRUE ~ NA_character_)) %>% 
        filter(!is.na(spp) & !is.na(bag)) %>% 
        select(seasonyear, state = st, speciesgroup, spp, bag) %>% 
        group_by(seasonyear, state, spp) %>% 
        summarize(max_bag = max(bag)) %>% 
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
    # Duplicate the "GeeseBrant" lines so they apply to Geese and Brant
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
          filter(sp_group_estimated == "GeeseBrant") %>% 
          mutate(sp_group_estimated = "Geese")) %>% 
      bind_rows(
        ref_table %>% 
          filter(sp_group_estimated == "GeeseBrant") %>% 
          mutate(sp_group_estimated = "Brant")) %>% 
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
          c("MODO-WWDO", "GeeseBrant", "CootsGallinules")) %>% 
      bind_rows(special_table) %>%
      distinct()
    
    # {Part 2}
    # Format over days ref table
    suppressMessages(
      dates <- 
        ref_data %>% 
        rename_all(~tolower(.)) %>% 
        filter(st != "PR" & st != "HI") %>% 
        filter(seasontype != "ExFalc") %>% 
        filter(!str_detect(speciesgroup, "Swan")) %>% 
        mutate(
          speciesgroup = 
            case_when(
              species == "Brant" ~ "Brant",
              str_detect(species, "MODO-WWDO") ~ "MODO-WWDO",
              TRUE ~ speciesgroup)) %>% 
        select(seasonyear, state = st, speciesgroup, open, close) %>% 
        mutate(
          spp = 
            case_when(
              str_detect(speciesgroup, "Sea") ~ "Specially Regulated Sea Ducks",
              str_detect(speciesgroup, "Crane") ~ "Sandhill Crane",
              speciesgroup == "Brant" ~ "Brant",
              speciesgroup == "CAGO" ~ "Geese",
              speciesgroup == "Geese" ~ "Geese",
              speciesgroup == "Ducks" ~ "Ducks",
              speciesgroup == "AMWO" ~ "Woodcock",
              speciesgroup == "COSN" ~ "Snipe",
              speciesgroup == "MODO" ~ "Mourning Dove",
              speciesgroup == "BTPI" ~ "Band-tailed Pigeon",
              speciesgroup == "Mergansers" ~ "Ducks",
              speciesgroup == "Rails" ~ "Rails",
              speciesgroup == "COMO-PUGA" ~ "Gallinules",
              # For NM "AMCO-COMO", set as "Coots" (they have a separate
              # speciesgroup for "COMO-PUGA" that becomes "Gallinules", above)
              speciesgroup =="AMCO-COMO" & state == "NM" ~ "Coots", 
              # *The "MODO-WWDO" category below should be used for MODO and WWDO
              speciesgroup == "MODO-WWDO" ~ "MODO-WWDO",
              speciesgroup == "MODO-WWDO-WTDO" ~ "MODO-WWDO",
              # *The NM "CAGO-CACG-Brant" category should apply to "Geese" AND
              # "Brant"
              speciesgroup == "CAGO-CACG-Brant" ~ "GeeseBrant",
              # *For AZ, CA, MN, and NV: the "AMCO-COMO" category should apply 
              # to"Coots" AND "Gallinules"
              speciesgroup == "AMCO-COMO" & 
                state %in% c("AZ", "CA", "MN", "NV") ~ 
                "CootsGallinules", 
              speciesgroup %in% c("Coots", "COOTS", "AMCO") ~ "Coots",
              TRUE ~ NA_character_),
          open_date = mdy(open),
          close_date = mdy(close)) %>% 
        filter(!is.na(spp) & !is.na(open) & !is.na(close)) %>% 
        group_by(seasonyear, state, spp) %>% 
        summarize(
          open = min(open_date, na.rm = T),
          close = max(close_date, na.rm = T)) %>%
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
    
    # {Part 3}
    # Error flagging
    
    if(str_detect(deparse(substitute(data)), "season") == TRUE){
      
      # Season data error flagging: overbags and overdays
      season_errors <- 
        data %>%
        #filter(days_hunted != 0 & retrieved != 0) %>% 
        left_join(
          ref_table,
          by = c("sp_group_estimated", "sampled_state")) %>% 
        left_join(
          dates,
          by = c("sp_group_estimated", "sampled_state")) %>% 
        mutate(
          bag_per_day = 
            ifelse(
              days_hunted != 0,
              round(retrieved/days_hunted, 1),
              0),
          errors = "x") %>% 
        # Flag overbags
        mutate(
          errors = 
            ifelse(
              bag_per_day > max_bag,
              paste(errors, "overbag", sep = "-"),
              errors)) %>% 
        # Flag overdays
        mutate(
          errors = 
            ifelse(
              days_hunted >= season_length,
              paste(errors, "overdays", sep = "-"),
              errors)) %>% 
        # Remove the x from errors
        mutate(
          errors =
            ifelse(
              errors == "x" | (days_hunted == 0 & retrieved == 0),
              "none",
              str_remove(errors, "^x\\-"))) %>%
        select(-c("max_bag", "season_length", "bag_per_day")) 
      
      if(nrow(filter(season_errors, is.na(errors))) > 0){
        message("Warning: Not all species matched.")
        
        print(
          season_errors %>% 
            filter(is.na(errors)) %>% 
            select(surveyID, sampled_state, sp_group_estimated, retrieved)
        )
      } 
      
      return(season_errors)
      
    }else if(str_detect(deparse(substitute(data)), "daily") == TRUE){
      
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
          errors = "x") %>% 
        # Flag overbags
        mutate(
          errors = 
            ifelse(
              retrieved > max_bag,
              paste(errors, "overbag", sep = "-"),
              errors)) %>% 
        # Flag overdays
        mutate(
          errors = 
            ifelse(
              n_days >= season_length,
              paste(errors, "overdays", sep = "-"),
              errors)) %>% 
        # Flag early hunts
        mutate(
          errors = 
            ifelse(
              harvested_date < open,
              paste(errors, "early_hunt", sep = "-"),
              errors)) %>% 
        # Flag late hunts
        mutate(
          errors = 
            ifelse(
              harvested_date > close,
              paste(errors, "late_hunt", sep = "-"),
              errors)) %>% 
        # Remove the x from errors
        mutate(
          errors =
            ifelse(
              errors == "x" | (n_days == 0 & retrieved == 0),
              "none",
              str_remove(errors, "^x\\-"))) %>%
        select(
          -c("max_bag", "season_length", "n_days", "open", "close"))
      
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