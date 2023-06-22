#' Check for errors in the reference table
#'
#' The \code{refcheck} function checks for issues in the all_seasons Harvest Survey reference table.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr rename_all
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
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
        ref_data %>% 
        rename_all(~tolower(.)) %>%
        filter(st != "PR" & st != "HI") %>% 
        filter(seasontype != "ExFalc") %>% 
        filter(!str_detect(speciesgroup, "Swan")) %>% 
        mutate(
          speciesgroup = 
            case_when(
              species == "Brant" ~ "Brant",
              str_detect(species, "MODO-WWDO") ~ "MODO-WWDO",
              TRUE ~ speciesgroup)) %>% 
        mutate(
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
              TRUE ~ NA_character_)) %>% 
        filter(is.na(spp)) %>%
        select(seasonyear, state = st, speciesgroup, spp, bag) %>%
        distinct()
      
      if(nrow(spp_return) == 0) {
        message("No species issues to report.")
      } else {
        return(spp_return)
      }
      
    }else if(type == "season"){
      season_return <-
        ref_data %>% 
        rename_all(~tolower(.)) %>% 
        filter(st != "PR" & st != "HI") %>% 
        filter(seasontype != "ExFalc") %>% 
        filter(!str_detect(speciesgroup, "Swan")) %>% 
        mutate(
          speciesgroup = 
            ifelse(
              is.na(speciesgroup),
              species,
              speciesgroup)) %>% 
        mutate(
          speciesgroup = 
            case_when(
              species == "Brant" ~ "Brant",
              str_detect(species, "MODO-WWDO") ~ "MODO-WWDO",
              TRUE ~ speciesgroup)) %>% 
        select(seasonyear, st, speciesgroup, open, close) %>% 
        mutate(
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
          ) %>% 
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