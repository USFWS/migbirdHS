#' Check for out-of-season harvest
#'
#' The \code{openclose} function checks if harvest dates fall outside of a state's and species's season in the daily data.
#' 
#' @importFrom dplyr rename_all
#' @importFrom dplyr filter
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
#' @importFrom dplyr n
#' @importFrom lubridate ymd
#' 
#' @param data Daily data table
#' @param ref_data The reference data table that corresponds to the year of the daily data
#' @param state Default is NA, but can be used to specify what state should be reported in the output. State values can be one of:
#'  \itemize{
#'  \item "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
#'  }
#' @param summary Defaults to FALSE
#'  \itemize{
#'  \item TRUE - returns a table summarized by state and species group to report the type and count of out-of-season values
#'  \item FALSE - returns a table unsummarized data
#'  }
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}

openclose <-
  function(data, ref_data, state = NA, summary = F){
    
    dates <- 
      wrangle_ref(ref_data) |>
      select(seasonyear, state = st, speciesgroup, open, close, spp) |> 
      filter(!is.na(spp) & !is.na(open) & !is.na(close)) |> 
      group_by(seasonyear, state, spp) |> 
      summarize(
        open = min(ymd(open), na.rm = T),
        close = max(ymd(close), na.rm = T)) |>
      ungroup() |> 
      left_join(
        tibble(
          state = datasets::state.abb,
          sampled_state = datasets::state.name),
        by = "state") |> 
      select(-c("state", "seasonyear")) |> 
      rename(sp_group_estimated = spp)
    
    # Duplicate the "Doves" lines so they apply to MODO and WWDO
    # Duplicate the "GeeseBrant" lines so they apply to Geese and Brant
    # Duplicate the "CootsGallinules" lines so they apply to Coots and 
    # Gallinules
    specialdates <-
      dates |> 
      filter(sp_group_estimated == "MODO-WWDO") |> 
      mutate(sp_group_estimated = "Mourning Dove") |> 
      bind_rows(
        dates |> 
          filter(sp_group_estimated == "MODO-WWDO") |> 
          mutate(sp_group_estimated = "White-Winged Dove")) |> 
      bind_rows(
        dates |> 
          filter(sp_group_estimated == "GeeseBrant") |> 
          mutate(sp_group_estimated = "Geese")) |> 
      bind_rows(
        dates |> 
          filter(sp_group_estimated == "GeeseBrant") |> 
          mutate(sp_group_estimated = "Brant")) |> 
      bind_rows(
        dates |> 
          filter(sp_group_estimated == "CootsGallinules") |> 
          mutate(sp_group_estimated = "Coots")) |> 
      bind_rows(
        dates |> 
          filter(sp_group_estimated == "CootsGallinules") |> 
          mutate(sp_group_estimated = "Gallinules")) 
    
    # Remove specialdates spp from the original dates df
    dates <-
      dates |> 
      filter(
        !sp_group_estimated %in% 
          c("MODO-WWDO", "GeeseBrant", "CootsGallinules")) |> 
      bind_rows(specialdates) |>
      distinct()
    
    date_errors <-
      data |> 
      left_join(
        dates,
        by = c("sp_group_estimated", "sampled_state")) |> 
      mutate(no_season = ifelse(is.na(open), "No season", NA)) |>
      mutate(
        harvested_date = ymd(harvested_date),
        error = 
          case_when(
            harvested_date < open ~ "Early hunt",
            harvested_date > close ~ "Late hunt",
            no_season == "No season" ~ "No season",
            TRUE ~ NA_character_)) |> 
      select(
        selected_hunterID, sampled_state, sp_group_estimated, open, close, 
        error) |> 
      filter(!is.na(error)) 
    
    if(is.na(state)){
      if(summary == F){
        return(date_errors)
      } else{
        return(
          date_errors |> 
            group_by(sampled_state, sp_group_estimated, error) |> 
            summarize(n = n()) |> 
            ungroup() 
        )
      }
    }else{
      if(summary == F){
        return(
          date_errors |> 
            filter(sampled_state == state)
        )
      }else{
        date_errors |> 
          filter(sampled_state == state) |> 
          group_by(sampled_state, sp_group_estimated, error) |> 
          summarize(n = n()) |> 
          ungroup() 
      }
      
    }
  }