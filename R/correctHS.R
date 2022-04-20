#' Correct errors in the Harvest Survey data
#'
#' The \code{correct} function processes the errors flagged in \code{proofHS} and returns a final data table.
#' 
#' @importFrom stringr str_detect
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#'  
#' @param data Daily or season data table processed by \code{proofHS}
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}
#' 

correctHS <-
  function(data){
    if(str_detect(deparse(substitute(data)), "daily") == TRUE){
      data %>% 
        mutate(
          flag = 
            case_when(
              # Get rid of records with more than one issue
              str_detect(errors, "\\-") ~ "discard",
              # Get rid of records with excessive overbag
              errors == "overbag" & overbag > 3 ~ "discard",
              # Get rid of records with excessive overdays
              errors == "overdays" & overday > 3 ~ "discard",
              errors == "late_hunt" ~ "late",
              errors == "early_hunt" ~ "early",
              TRUE ~ NA_character_)) %>% 
        filter(flag != "discard") %>% 
        select(-c("errors", "overbag", "overday", "late", "early"))
    }
    else if(str_detect(deparse(substitute(data)), "season") == TRUE){
      data %>% 
        mutate(
          flag = 
            case_when(
              # Get rid of records with more than one issue
              str_detect(errors, "\\-") ~ "discard",
              # Get rid of records with excessive overbag
              errors == "overbag" & overbag > 3 ~ "discard",
              # Get rid of records with excessive overdays
              errors == "overdays" & overday > 3 ~ "discard",
              TRUE ~ NA_character_)) %>% 
        filter(flag != "discard") %>% 
        select(-c("errors", "overbag", "overday"))
    }
    else{
      message(
        "Error: `data` must be season_totals or daily_records data frame.")
    }
  }