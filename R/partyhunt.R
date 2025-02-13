#' Check for group hunts in the daily data
#'
#' The \code{partyhunt} function checks to see if any comments in the daily Harvest Survey data indicate that the bag value reported was for two or more people.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' 
#' @param data Daily data table
#' 
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHS}

partyhunt <- 
  function(data){
    data %>% 
      filter(
        str_detect(comment, "[0-9]{1,2} man") |
          str_detect(comment, "[0-9]{1,2} men") |
          str_detect(comment, "[0-9]{1,2} person") | 
          str_detect(comment, "[0-9]{1,2} people") | 
          str_detect(comment, "[0-9]{1,2} hunter") | 
          str_detect(comment, "[0-9]{1,2} guy") | 
          str_detect(comment, "group of") | 
          str_detect(comment, "party of")) %>% 
      select(
        sampled_state, sp_group_estimated, retrieved, unretrieved, comment) %>%
      filter(retrieved != 0) %>% 
      mutate(
        # Pull out obvious group sizes from strings
        party_size = 
          case_when(
            str_detect(comment, "[0-9]{1,2} man") & retrieved != 0 ~ 
              str_extract(comment, "[0-9]{1,2} man") %>% 
              str_remove(., " man"),
            str_detect(comment, "[0-9]{1,2} men") & retrieved != 0 ~ 
              str_extract(comment, "[0-9]{1,2} men") %>% 
              str_remove(., " men"),
            str_detect(comment, "[0-9]{1,2} person") & retrieved != 0 ~ 
              str_extract(comment, "[0-9]{1,2} person") %>% 
              str_remove(., " person"),
            str_detect(comment, "[0-9]{1,2} hunter") & retrieved != 0 ~ 
              str_extract(comment, "[0-9]{1,2} hunter") %>% 
              str_remove(., " hunter"),
            str_detect(comment, "[0-9]{1,2} guy") & retrieved != 0 ~ 
              str_extract(comment, "[0-9]{1,2} guy") %>% 
              str_remove(., " guy"),
            str_detect(comment, "[0-9]{1,2} people") & retrieved != 0 ~ 
              str_extract(comment, "[0-9]{1,2} people") %>% 
              str_remove(., " people"),
            str_detect(comment, "party of [0-9]{1,2}") & retrieved != 0 ~ 
              str_extract(comment, "party of [0-9]{1,2}") %>% 
              str_remove(., "party of "),
            str_detect(
              comment, 
              "party of one|party of two|party of three|party of four|party of five|party of six|party of seven|party of eight|party of nine|party of ten") &
              retrieved != 0 ~ 
              str_extract(
                comment, 
                "party of one|party of two|party of three|party of four|party of five|party of six|party of seven|party of eight|party of nine|party of ten") %>% 
              str_remove(., "party of "),
            str_detect(comment, "group of [0-9]{1,2}") & retrieved != 0 ~ 
              str_extract(comment, "group of [0-9]{1,2}") %>% 
              str_remove(., "group of "),
            str_detect(
              comment, 
              "group of one|group of two|group of three|group of four|group of five|group of six|group of seven|group of eight|group of nine|group of ten") & 
              retrieved != 0 ~ 
              str_extract(
                comment, 
                "group of one|group of two|group of three|group of four|group of five|group of six|group of seven|group of eight|group of nine|group of ten") %>% 
              str_remove(., "group of "),
            TRUE ~ NA_character_))}