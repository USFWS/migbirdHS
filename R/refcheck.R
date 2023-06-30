#' Check for errors in the reference table
#'
#' The \code{refcheck} function checks for issues in the all_seasons Harvest Survey reference table.
#' 
#' @importFrom dplyr |>
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#'
#' @param ref_data A reference data table 
#' @param type Defaults to "species"
#'  \itemize{
#'  \item "species" - returns species values
#'  \item "season" - returns hunting season open and close dates
#'  }
#' 
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}

refcheck <-
  function(ref_data, type = "species"){
    
    if(type == "species"){
      spp_return <-
        wrangle_ref(ref_data) |>
        filter(is.na(spp)) |>
        select(seasonyear, state = st, speciesgroup, spp, bag) |>
        distinct()
      
      if(nrow(spp_return) == 0) {
        message("No species issues to report.")
      } else {
        return(spp_return)
      }
      
    }else if(type == "season"){
      season_return <-
        wrangle_ref(ref_data) |>
        select(seasonyear, st, speciesgroup, open, close, spp) |> 
        filter(is.na(spp) | is.na(open) | is.na(close))
      
      if(nrow(season_return) == 0) {
        message("No season issues to report.")
      } else {
        return(season_return)
      }
    }else{
      message(paste0("Error: unrecognized `type`. Use 'species' or 'season'."))
    }
  }