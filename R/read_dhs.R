#' Read in Harvest Survey data
#'
#' The \code{read_dhs} function reads in daily, season, and reference Harvest Survey .csv data files from a specified filepath. It will automatically read in all files by default unless a year is specified.
#'
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom dplyr group_by
#' @importFrom purrr set_names
#' @importFrom stringr str_detect
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
    # Add a final "/" to path, if not included already
    if(!str_detect(path, "\\/$")){
      path <- paste0(path, "/")
    }else{
      path <- path
    }
    if(year %in% as.character(2019:2100)){
      list.files(path) |> 
        str_subset(year) |> 
        map(~read_csv(paste0(path, .))) |> 
        set_names(
          str_extract(
            list.files(path), 
            "daily_records|season_totals|all_seasons")
        ) |> 
        list2env(.GlobalEnv)
    }
    else{
      message(
        paste0(
          "Error: Invalid year. Please specify a character year between 2019 a",
          "nd 2100.")
      )
    }
  }
  