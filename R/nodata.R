#' Check for missing data
#'
#' The \code{nodata} function checks to see if any states or species are missing from the daily or season Harvest Survey online data.
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#' @importFrom stringr str_extract
#' @importFrom dplyr select
#' @importFrom dplyr pull
#' @importFrom dplyr tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr rename
#' @importFrom dplyr bind_rows
#' 
#' @param data Daily or season data table
#' @param ref_data The reference data table that corresponds to the year of the data
#' @param species Whether species should be included in the output, and if so, which species in particular:
#'  \itemize{
#'  \item NA -
#'  \item "all" - 
#'  \item A specific species group, which may be one of:
#'  \itemize{
#'  \item "Band-tailed Pigeon", "Coots", "Ducks", "Gallinules", "Geese", "Rails", "Sandhill Crane", "Snipe", "Specially Regulated Sea Ducks", "Woodcock"}
#'  }
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}

nodata <-
  function(data, ref_data = NA, species = NA){
    # First, if a season totals table was used in this function, exclude daily
    # records from the season totals table. This allows the season totals data
    # to be evaluated separately from daily data
    if(str_detect(deparse(substitute(data)), "season") == TRUE | 
       str_detect(deparse(substitute(data)), "tibblelist\\[3\\]") == TRUE){
      if(str_detect(deparse(substitute(data)), "season") == TRUE){
        dataname <- deparse(substitute(data))
        
        data <- 
          data %>% 
          filter(
            !selected_hunterID %in%
              c(get(paste0(
                "daily_records_",
                str_extract(dataname, "[0-9]{4}")
              )) %>%
                select(selected_hunterID) %>%
                pull())
          )
        message("Notice: season data filtered to exclude daily records.")
        # Additional statement for report template compatibility
      }else if(str_detect(deparse(substitute(data)), "tibblelist\\[3\\]")){
        datayr <- 
          data %>% 
          select(season) %>% 
          distinct() %>% 
          pull()
        
        data <- 
          data %>% 
          filter(
            !selected_hunterID %in%
              c(get(paste0(
                "daily_records_",
                as.character(datayr)
              )) %>%
                select(selected_hunterID) %>%
                pull())
          )
        message("Notice: season data filtered to exclude daily records.")
      }
      }
    if(is.na(species)){
      tibble(sampled_state = datasets::state.name) %>% 
        filter(sampled_state != "Hawaii") %>% 
        left_join(
          data %>% 
            select(sampled_state) %>% 
            distinct() %>% 
            mutate(has_data = "Y"),
          by = "sampled_state") %>% 
        filter(is.na(has_data)) %>% 
        mutate(has_data = "No")}
    else if(species == "all"){
      ref_table <- 
        ref_data %>% 
        select(sampled_state = ST, speciesgroup = SpeciesGroup) %>% 
        distinct() %>% 
        filter(!is.na(speciesgroup)) %>% 
        mutate(
          spp = 
            case_when(
              str_detect(speciesgroup, "Sea") ~ "Specially Regulated Sea Ducks",
              str_detect(speciesgroup, "Crane") ~ "Sandhill Crane",
              speciesgroup == "Brant" ~ "Brant",
              speciesgroup == "CAGO" ~ "Geese",
              speciesgroup == "Geese" ~ "Geese",
              speciesgroup == "Ducks" ~ "Ducks",
              speciesgroup == "AMWO" ~ "Woodcock",
              speciesgroup == "COSN" ~ "Snipe",
              speciesgroup == "MODO" ~ "Mourning Dove",
              speciesgroup == "BTPI" ~ "Band-tailed Pigeon",
              speciesgroup == "Mergansers" ~ "Ducks",
              speciesgroup == "Rails" ~ "Rails",
              speciesgroup == "COMO-PUGA" ~ "Gallinules",
              # For NM "AMCO-COMO", set as "Coots" (they have a separate
              # speciesgroup for "COMO-PUGA" that becomes "Gallinules", above)
              speciesgroup =="AMCO-COMO" & sampled_state == "NM" ~ "Coots", 
              # *The "MODO-WWDO" category below should be used for MODO and WWDO
              speciesgroup == "MODO-WWDO" ~ "MODO-WWDO",
              # **The NM "CAGO-CACG-Brant" category should apply to "Geese" AND
              # "Brant"
              speciesgroup == "CAGO-CACG-Brant" ~ "GeeseBrant",
              # **For AZ, CA, MN, and NV: the "AMCO-COMO" category should apply 
              # to "Coots" AND "Gallinules"
              speciesgroup == "AMCO-COMO" & 
                sampled_state %in% c("AZ", "CA", "MN", "NV") ~ 
                "CootsGallinules", 
              speciesgroup %in% c("Coots", "COOTS", "AMCO") ~ "Coots",
              TRUE ~ NA_character_)) %>% 
        rename(sp_group_estimated = spp) %>% 
        select(-speciesgroup)
      
      # Duplicate the "Doves" lines so they apply to MODO and WWDO
      # Duplicate the "GeeseBrant" lines so they apply to Geese and Brant
      # Duplicate the "CootsGallinules" lines so they apply to Coots and 
      # Gallinules
      special_data <-
        ref_table %>% 
        filter(sp_group_estimated == "MODO-WWDO") %>% 
        mutate(sp_group_estimated = "Mourning Dove") %>% 
        bind_rows(
          ref_table %>% 
            filter(sp_group_estimated == "MODO-WWDO") %>% 
            mutate(sp_group_estimated = "White-winged Dove")) %>% 
        bind_rows(
          ref_table %>% 
            filter(sp_group_estimated == "GeeseBrant") %>% 
            mutate(sp_group_estimated = "Geese")) %>% 
        bind_rows(
          ref_table %>% 
            filter(sp_group_estimated == "GeeseBrant") %>% 
            mutate(sp_group_estimated = "Brant")) %>% 
        bind_rows(
          ref_table %>% 
            filter(sp_group_estimated == "CootsGallinules") %>% 
            mutate(sp_group_estimated = "Coots")) %>% 
        bind_rows(
          ref_table %>% 
            filter(sp_group_estimated == "CootsGallinules") %>% 
            mutate(sp_group_estimated = "Gallinules")) 
      
      ref_table %>% 
        filter(
          !sp_group_estimated %in% 
            c("MODO-WWDO", "GeeseBrant", "CootsGallinules")) %>% 
        bind_rows(special_data) %>%
        distinct() %>% 
        left_join(
          data %>% 
            select(state_name = sampled_state, sp_group_estimated) %>% 
            distinct() %>%
            left_join(
              tibble(
                state_name = datasets::state.name,
                sampled_state = datasets::state.abb),
              by = "state_name") %>% 
            select(-state_name) %>% 
            mutate(has_data = "Y"),
          by = c("sampled_state", "sp_group_estimated")) %>% 
        filter(is.na(has_data)) %>% 
        mutate(has_data = "No") %>% 
        filter(!is.na(sp_group_estimated))
    }else if(species %in% 
             c("Band-tailed Pigeon", "Coots", "Ducks", "Gallinules", "Geese", 
               "Rails", "Sandhill Crane", "Snipe", 
               "Specially Regulated Sea Ducks", "Woodcock")){
      ref_table <- 
        ref_data %>% 
        select(sampled_state = ST, speciesgroup = SpeciesGroup) %>% 
        distinct() %>% 
        filter(!is.na(speciesgroup)) %>% 
        mutate(
          spp = 
            case_when(
              str_detect(speciesgroup, "Sea") ~ "Specially Regulated Sea Ducks",
              str_detect(speciesgroup, "Crane") ~ "Sandhill Crane",
              speciesgroup == "Brant" ~ "Brant",
              speciesgroup == "CAGO" ~ "Geese",
              speciesgroup == "Geese" ~ "Geese",
              speciesgroup == "Ducks" ~ "Ducks",
              speciesgroup == "AMWO" ~ "Woodcock",
              speciesgroup == "COSN" ~ "Snipe",
              speciesgroup == "MODO" ~ "Mourning Dove",
              speciesgroup == "BTPI" ~ "Band-tailed Pigeon",
              speciesgroup == "Mergansers" ~ "Ducks",
              speciesgroup == "Rails" ~ "Rails",
              speciesgroup == "COMO-PUGA" ~ "Gallinules",
              # For NM "AMCO-COMO", set as "Coots" (they have a separate
              # speciesgroup for "COMO-PUGA" that becomes "Gallinules", above)
              speciesgroup =="AMCO-COMO" & sampled_state == "NM" ~ "Coots", 
              # *The "MODO-WWDO" category below should be used for MODO and WWDO
              speciesgroup == "MODO-WWDO" ~ "MODO-WWDO",
              # **The NM "CAGO-CACG-Brant" category should apply to "Geese" AND
              # "Brant"
              speciesgroup == "CAGO-CACG-Brant" ~ "GeeseBrant",
              # **For AZ, CA, MN, and NV: the "AMCO-COMO" category should apply 
              # to "Coots" AND "Gallinules"
              speciesgroup == "AMCO-COMO" & 
                sampled_state %in% c("AZ", "CA", "MN", "NV") ~ 
                "CootsGallinules", 
              speciesgroup %in% c("Coots", "COOTS", "AMCO") ~ "Coots",
              TRUE ~ NA_character_)) %>% 
        rename(sp_group_estimated = spp) %>% 
        select(-speciesgroup)
      
      # Duplicate the "Doves" lines so they apply to MODO and WWDO
      # Duplicate the "GeeseBrant" lines so they apply to Geese and Brant
      # Duplicate the "CootsGallinules" lines so they apply to Coots and 
      # Gallinules
      special_data <-
        ref_table %>% 
        filter(sp_group_estimated == "MODO-WWDO") %>% 
        mutate(sp_group_estimated = "Mourning Dove") %>% 
        bind_rows(
          ref_table %>% 
            filter(sp_group_estimated == "MODO-WWDO") %>% 
            mutate(sp_group_estimated = "White-winged Dove")) %>% 
        bind_rows(
          ref_table %>% 
            filter(sp_group_estimated == "GeeseBrant") %>% 
            mutate(sp_group_estimated = "Geese")) %>% 
        bind_rows(
          ref_table %>% 
            filter(sp_group_estimated == "GeeseBrant") %>% 
            mutate(sp_group_estimated = "Brant")) %>% 
        bind_rows(
          ref_table %>% 
            filter(sp_group_estimated == "CootsGallinules") %>% 
            mutate(sp_group_estimated = "Coots")) %>% 
        bind_rows(
          ref_table %>% 
            filter(sp_group_estimated == "CootsGallinules") %>% 
            mutate(sp_group_estimated = "Gallinules")) 
      
      ref_table %>% 
        filter(
          !sp_group_estimated %in% 
            c("MODO-WWDO", "GeeseBrant", "CootsGallinules")) %>% 
        bind_rows(special_data) %>%
        distinct() %>% 
        left_join(
          data %>% 
            select(state_name = sampled_state, sp_group_estimated) %>% 
            distinct() %>%
            left_join(
              tibble(
                state_name = datasets::state.name,
                sampled_state = datasets::state.abb),
              by = "state_name") %>% 
            select(-state_name) %>% 
            mutate(has_data = "Y"),
          by = c("sampled_state", "sp_group_estimated")) %>% 
        filter(is.na(has_data)) %>%
        filter(sp_group_estimated == species) %>% 
        mutate(has_data = "No")
    }else{
      message("Error: `species` must be one of: 'Band-tailed Pigeon', 'Coots', 
              'Ducks', 'Gallinules', 'Geese', 'Rails', 'Sandhill Crane', 
              'Snipe', 'Specially Regulated Sea Ducks', or 'Woodcock'.")
    }
  }