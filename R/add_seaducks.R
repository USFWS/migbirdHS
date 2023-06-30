#' Add sea duck dates for eligible states
#'
#' The \code{add_seaducks} function amends the ref_data for AK, CT, DE, ME, MD, MA, NH, NJ, NY, OR, RI, VA, WA, and CA by adding lines to account for seaduck seasons; it uses the same values as duck harvest for open, close, bag, and possession.
#' 
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' 
#' @param ref_data The reference data table that corresponds to the year of the daily data
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}
#' 

add_seaducks <-
  function(ref_data) {
    
    ref_data |> 
    bind_rows(
      ref_data |> 
        filter(
          ST %in% c("AK", "CT", "DE", "ME", "MD", "MA", "NH", 
                    "NJ", "NY", "OR", "RI", "VA", "WA", "CA") &
            SpeciesGroup == "Ducks") |> 
        mutate(
          SeasonType = "Reg",
          SpeciesGroup = "Sea ducks",
          Species = "Sea ducks") |> 
        distinct() |> 
        group_by(ST) |> 
        mutate(
          Open = min(Open, na.rm = T),
          Close = max(Close, na.rm = T),
          Bag = max(Bag, na.rm = T),
          Possession = max(Possession, na.rm = T)
        ) |> 
        ungroup() |> 
        distinct()
      )
  }