#' Consistent reference data wrangling
#'
#' The internal \code{wrangle_ref} function edits the reference data table consistently among all functions in the migbirdMBHS package.
#' 
#' @importFrom dplyr rename_all
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom dplyr case_when
#' 
#' @param ref_data The reference data table that corresponds to the year of the data
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}

wrangle_ref <-
  function(ref_data){
    
    ref_return <-
      ref_data |> 
      rename_all(~tolower(.)) |> 
      filter(
        !st %in% c("PR", "HI") & 
          seasontype != "ExFalc" & 
          !str_detect(speciesgroup, "Swan")) |> 
      mutate(
        speciesgroup = 
          ifelse(
            is.na(speciesgroup),
            species,
            speciesgroup)) |> 
      mutate(
        speciesgroup = 
          case_when(
            species == "Brant" ~ "Brant",
            str_detect(species, "MODO-WWDO") ~ "MODO-WWDO",
            TRUE ~ speciesgroup),
        spp = 
          case_when(
            str_detect(speciesgroup, "Sea") ~ "Specially Regulated Sea Ducks",
            str_detect(speciesgroup, "Crane") ~ "Sandhill Crane",
            speciesgroup == "Brant" ~ "Brant",
            speciesgroup == "Geese" ~ "Geese",
            speciesgroup == "Ducks" ~ "Ducks",
            speciesgroup == "AMWO" ~ "Woodcock",
            speciesgroup == "COSN" ~ "Snipe",
            speciesgroup == "BTPI" ~ "Band-tailed Pigeon",
            speciesgroup == "Rails" ~ "Rails",
            speciesgroup == "COMO-PUGA" ~ "Gallinules",
            # **The "MODO-WWDO" category below should be used for MODO and WWDO
            speciesgroup == "MODO-WWDO" ~ "MODO-WWDO",
            # For NM "AMCO-COMO", set as "Coots" (they have a separate
            # speciesgroup for "COMO-PUGA" that becomes "Gallinules", above)
            speciesgroup == "AMCO-COMO" & st == "NM" ~ "Coots", 
            # **For AZ, CA, MN, and NV: the "AMCO-COMO" category should apply to
            # "Coots" AND "Gallinules"
            speciesgroup == "AMCO-COMO" & st %in% c("AZ", "CA", "MN", "NV") ~ 
              "CootsGallinules", 
            # **For CO, ID, MT, OR, UT, WA, WY: the "AMCO-COMO" category should 
            # apply to "Coots" only
            speciesgroup == "AMCO-COMO" & 
              st %in% c("CO", "ID", "MT", "OR", "UT", "WA", "WY") ~ "Coots", 
            speciesgroup == "Coots" ~ "Coots",
            TRUE ~ NA_character_)
      ) 
    
  }