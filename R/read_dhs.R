#' Read in Harvest Survey data
#'
#' The \code{read_dhs} function reads in daily, season, and reference Harvest Survey .csv data files from a specified filepath. It will automatically read in all files by default unless a year is specified.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom dplyr group_by
#' @importFrom purrr set_names
#' @importFrom stringr str_extract
#' @importFrom stringr str_subset
#'
#' @param path Directory containing the Harvest Survey daily, season, and/or reference data files
#' @param year Season for which files should be read in
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}

read_dhs <-
  function(path, year){
    if(year == "all"){
      list.files(path) %>% 
        map(~read_csv(paste0(path, .))) %>% 
        set_names(
          paste(
            str_extract(
              list.files(path), "daily_records|season_totals|all_seasons"),
            str_extract(list.files(path), "[0-9]{4}"),
            sep = "_")) %>% 
        list2env(.GlobalEnv)}
    else if(year %in% as.character(2019:2100)){
      list.files(path) %>% 
        str_subset(year) %>% 
        map(~read_csv(paste0(path, .))) %>% 
        set_names(
          paste(
            str_extract(
              list.files(path) %>% 
                str_subset(year), 
              "daily_records|season_totals|all_seasons"),
            year,
            sep = "_")) %>% 
        list2env(.GlobalEnv)
    }
    else{
      message("Error: Invalid year.")
    }
  }