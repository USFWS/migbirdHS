#' Check for overbags in the daily data
#'
#' The \code{overbags} function checks for retrieved values over the bag limit per state and species in the Harvest Survey daily data.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr rename_all
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
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
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr n
#' 
#' @param data Daily data table
#' @param ref_data The reference data table that corresponds to the year of the daily data
#' @param state Default is NA, but can be used to specify what state should be reported in the output. State values can be one of:
#'  \itemize{
#'  \item "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
#'  }
#' @param over Defaults to TRUE
#'  \itemize{
#'  \item TRUE - returns only records with retrieved values that exceed the bag limit
#'  \item FALSE - returns a table of all values
#'  }
#' @param summary Defaults to FALSE
#'  \itemize{
#'  \item TRUE - returns a table summarized by state and species to report the max, mean, min, and count of overbag values
#'  \item FALSE - returns a table unsummarized data
#'  }
#' 
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}

overbags <-
  function(data, ref_data, state = NA, over = T, summary = F){
    suppressMessages(
      ref_table <-
        wrangle_ref(ref_data) %>%
        filter(!is.na(spp)) %>% 
        select(seasonyear, state = st, speciesgroup, spp, bag, possession) %>% 
        group_by(seasonyear, state, spp) %>% 
        summarize(
          max_bag = max(bag),
          max_poss = max(possession)) %>% 
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
    
    if(
      TRUE %in% c(str_detect(deparse(substitute(data)), "daily"), 
                  str_detect(deparse(substitute(data)), "tibblelist\\[2\\]"))){
      bag_errors <-
        data %>% 
        left_join(
          ref_table, 
          by = c("sp_group_estimated", "sampled_state")) %>% 
        mutate(
          error = 
            case_when(
              retrieved > max_poss ~ "over_possession",
              retrieved > max_bag ~ "over_bag",
              TRUE ~ NA_character_)) %>% 
        filter(!is.na(error)) %>% 
        select(
          selected_hunterID, surveyID, sampled_state, county, 
          sp_group_estimated, harvested_date, max_bag, retrieved, 
          unretrieved) %>% 
        arrange(desc(retrieved)) %>% 
        rename(bag_limit = max_bag) %>% 
        arrange(selected_hunterID)
      
      if(over == TRUE){
        if(is.na(state)){
          if(summary == F){
            return(bag_errors)
          }else{
            return(
              bag_errors %>% 
                group_by(sampled_state, sp_group_estimated, bag_limit) %>% 
                summarize(
                  max = max(retrieved, na.rm = T),
                  mean = round(mean(retrieved), 1),
                  min = min(retrieved),
                  n = n()) %>% 
                ungroup()
            )
          }
        }else{
          if(summary == F){
            return(
              bag_errors %>% 
                filter(sampled_state == state)
            )
          }else{
            bag_errors %>% 
              filter(sampled_state == state) %>% 
              group_by(sampled_state, sp_group_estimated, bag_limit) %>% 
              summarize(
                max = max(retrieved, na.rm = T),
                mean = round(mean(retrieved), 1),
                min = min(retrieved),
                n = n()) %>% 
              ungroup() 
          }
        }
      }else{
        # Non-overbags
        return(
          data %>% 
            left_join(
              ref_table, 
              by = c("sp_group_estimated", "sampled_state")) %>% 
            mutate(
              error = 
                case_when(
                  retrieved > max_poss ~ "over_possession",
                  retrieved > max_bag ~ "over_bag",
                  TRUE ~ NA_character_)) %>% 
            filter(is.na(error)) %>% 
            select(
              selected_hunterID, surveyID, sampled_state, county, 
              sp_group_estimated, harvested_date, max_bag, retrieved, 
              unretrieved) %>% 
            arrange(desc(retrieved)) %>% 
            rename(bag_limit = max_bag) %>% 
            arrange(selected_hunterID)
        )
      }
    }else if(
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
        }else if(str_detect(deparse(substitute(data)), "tibblelist\\[3\\]")){
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
      
      data %>% 
        select(
          selected_hunterID, sampled_state, sp_group_estimated, retrieved, 
          days_hunted) %>% 
        filter(days_hunted != 0) %>% 
        mutate(bag_per_day = 
                 round(as.numeric(retrieved)/as.numeric(days_hunted), 1)) %>% 
        left_join(
          ref_table %>% 
            select(-max_poss),
          by = c("sampled_state", "sp_group_estimated")) %>% 
        filter(bag_per_day > max_bag) %>% 
        rename(
          ID = selected_hunterID,
          state = sampled_state,
          sp = sp_group_estimated) %>% 
        mutate(sp = ifelse(str_detect(sp, "Sea"), "Sea Ducks", sp)) %>% 
        arrange(desc(retrieved))
    }else{
      message(
        paste0(
          "Error: unrecognized `data`. Please supply a daily_records or ",
          "season_totals dataframe."))
    }
  }