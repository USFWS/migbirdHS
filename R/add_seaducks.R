#' Append sea duck information to the Harvest Survey reference table
#'
#' The \code{add_seaducks} function includes the sea duck open date, close date, and bag limit for California and Washington in the reference table. The California and Washington sea duck seasons match the same dates and bags as the states' duck seasons.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfc
#' @importFrom dplyr mutate
#' @importFrom readr type_convert
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr pull
#' @importFrom lubridate mdy
#' @importFrom stats setNames
#'
#' @param ref_data A reference data table
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}

add_seaducks <-
  function(ref_data){
    bind_rows(
      ref_data, 
      bind_rows(
        names(ref_data) %>% 
          map_dfc(setNames, object = list(character(1))) %>% 
          mutate(
            SeasonType = "Regular",
            ST = "CA",
            SpeciesGroup = "Sea ducks",
            Species = "Sea ducks"),
        names(ref_data) %>% 
          map_dfc(setNames, object = list(character(1))) %>% 
          mutate(
            SeasonType = "Regular",
            ST = "WA",
            SpeciesGroup = "Sea ducks",
            Species = "Sea ducks")) %>% 
        type_convert()) %>% 
      mutate(
        Open = 
          case_when(
            ST == "CA" & Species == "Sea ducks" ~ 
              ref_data %>% 
              filter(ST == "CA" & SpeciesGroup == "Ducks") %>% 
              mutate(Open = mdy(Open)) %>% 
              select(Open) %>% 
              pull() %>% 
              min(., na.rm = T) %>% 
              format("%m/%d/%Y") %>% 
              as.character(),
            ST == "WA" & Species == "Sea ducks" ~ 
              ref_data %>% 
              filter(ST == "WA" & SpeciesGroup == "Ducks") %>% 
              mutate(Open = mdy(Open)) %>% 
              select(Open) %>% 
              pull() %>% 
              min(., na.rm = T) %>% 
              format("%m/%d/%Y") %>% 
              as.character(),
            TRUE ~ Open),
        Close = 
          case_when(
            ST == "CA" & Species == "Sea ducks" ~ 
              ref_data %>% 
              filter(ST == "CA" & SpeciesGroup == "Ducks") %>% 
              mutate(Close = mdy(Close)) %>% 
              select(Close) %>% 
              pull() %>% 
              max(., na.rm = T) %>% 
              format("%m/%d/%Y") %>% 
              as.character(),
            ST == "WA" & Species == "Sea ducks" ~ 
              ref_data %>% 
              filter(ST == "WA" & SpeciesGroup == "Ducks") %>% 
              mutate(Close = mdy(Close)) %>% 
              select(Close) %>% 
              pull() %>% 
              max(., na.rm = T) %>% 
              format("%m/%d/%Y") %>% 
              as.character(),
            TRUE ~ Close),
        Bag = 
          case_when(
            ST == "CA" & Species == "Sea ducks" ~ 
              ref_data %>% 
              filter(ST == "CA" & SpeciesGroup == "Ducks") %>% 
              select(Bag) %>% 
              pull() %>% 
              max(., na.rm = T),
            ST == "WA" & Species == "Sea ducks" ~ 
              ref_data %>% 
              filter(ST == "WA" & SpeciesGroup == "Ducks") %>% 
              select(Bag) %>% 
              pull() %>% 
              max(., na.rm = T),
            TRUE ~ Bag)
      ) 
  }