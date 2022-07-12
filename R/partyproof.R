#' Adjust group hunts reported in Harvest Survey daily data
#'
#' The \code{partyproof} function checks for, and allows the user to edit, any group hunts in the daily Harvest Survey data. Comments are parsed to determine party size. Adjustments to number of birds retrieved must be entered manually, and if deemed necessary, the retrieved value will be divided by the party size. A report file is written out as a .csv to record all changes made.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr rename_all
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr group_by 
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct 
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom rlang parse_expr
#' @importFrom utils write.csv
#' @importFrom dplyr relocate
#' @importFrom rlang na_dbl
#' 
#' @param data Daily data table
#' @param ref_data Reference data table for the same year as the daily data
#' @param outpath Path to write the change log .csv; must end with a forward slash
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}
#' 
partyproof <- 
  function(data, ref_data, outpath){
    
    ref_table <-
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
            species == "MODO" ~ "MODO",
            str_detect(species, "MODO-WWDO") ~ "MODO-WWDO",
            TRUE ~ speciesgroup)) %>% 
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
            speciesgroup =="AMCO-COMO" & st == "NM" ~ "Coots", 
            # **The "MODO-WWDO" category below should be used for MODO and WWDO
            speciesgroup == "MODO-WWDO" ~ "MODO-WWDO",
            speciesgroup == "MODO-WWDO-WTDO" ~ "MODO-WWDO",
            # **The NM "CAGO-CACG-Brant" category should apply to "Geese" AND
            # "Brant"
            speciesgroup == "CAGO-CACG-Brant" ~ "GeeseBrant",
            # **For AZ, CA, MN, and NV: the "AMCO-COMO" category should apply to
            # "Coots" AND "Gallinules"
            speciesgroup == "AMCO-COMO" & st %in% c("AZ", "CA", "MN", "NV") ~ 
              "CootsGallinules", 
            speciesgroup %in% c("Coots", "COOTS", "AMCO") ~ "Coots",
            TRUE ~ NA_character_)) %>% 
      filter(!is.na(spp)) %>% 
      select(seasonyear, state = st, speciesgroup, spp, bag) %>% 
      group_by(seasonyear, state, spp) %>% 
      summarize(max_bag = max(bag)) %>% 
      ungroup() %>% 
      left_join(
        tibble(
          state = datasets::state.abb,
          sampled_state = datasets::state.name),
        by = "state") %>% 
      select(-c("state", "seasonyear")) %>%
      rename(sp_group_estimated = spp) 
    
    special_table <-
      ref_table %>% 
      filter(sp_group_estimated == "MODO-WWDO") %>% 
      mutate(sp_group_estimated = "Mourning Dove") %>% 
      bind_rows(
        ref_table %>% 
          filter(sp_group_estimated == "MODO-WWDO") %>% 
          mutate(sp_group_estimated = "White-Winged Dove")) %>% 
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
    
    # Remove specialdates spp from the original dates df
    ref_table <-
      ref_table %>% 
      filter(
        !sp_group_estimated %in% 
          c("MODO-WWDO", "GeeseBrant", "CootsGallinules")) %>% 
      bind_rows(special_table) %>%
      distinct()
    
    partytable <- 
      data %>% 
      mutate(
        # Pull out obvious group sizes from strings
        party_size = 
          case_when(
            str_detect(comment, "[0-9]{1,2} man") & retrieved != 0 ~ 
              str_extract(comment, "[0-9]{1,2} man") %>% 
              str_remove(., " man"),
            str_detect(comment, "[0-9]{1,2} person") & retrieved != 0 ~ 
              str_extract(comment, "[0-9]{1,2} person") %>% 
              str_remove(., " person"),
            str_detect(comment, "[0-9]{1,2} hunter") & retrieved != 0 ~ 
              str_extract(comment, "[0-9]{1,2} hunter") %>% 
              str_remove(., " hunter"),
            str_detect(comment, "[0-9]{1,2} people") & retrieved != 0 ~ 
              str_extract(comment, "[0-9]{1,2} people") %>% 
              str_remove(., " people"),
            str_detect(comment, "party of [0-9]{1,2}") & retrieved != 0 ~ 
              str_extract(comment, "party of [0-9]{1,2}") %>% 
              str_remove(., "party of "),
            str_detect(
              comment, "party of one|party of two|party of three|party of four|party of five|party of six|party of seven|party of eight|party of nine|party of ten") & 
              retrieved != 0 ~ 
              str_extract(
                comment, 
                "party of one|party of two|party of three|party of four|party of five|party of six|party of seven|party of eight|party of nine|party of ten") %>% 
              str_remove(., "party of "),
            str_detect(comment, "group of [0-9]{1,2}") & retrieved != 0 ~ 
              str_extract(comment, "group of [0-9]{1,2}") %>% 
              str_remove(., "group of "),
            str_detect(
              comment, "group of one|group of two|group of three|group of four|group of five|group of six|group of seven|group of eight|group of nine|group of ten") & 
              retrieved != 0 ~ 
              str_extract(
                comment, 
                "group of one|group of two|group of three|group of four|group of five|group of six|group of seven|group of eight|group of nine|group of ten") %>% 
              str_remove(., "group of "),
            TRUE ~ NA_character_)) %>% 
      rename(original_retrieved = retrieved) %>% 
      mutate(new_retrieved = NA) %>% 
      left_join(ref_table, by = c("sp_group_estimated", "sampled_state")) %>% 
      arrange(desc(party_size))
    
    i <- 1
    total <- 
      partytable %>% 
      select(party_size) %>%
      filter(!is.na(party_size)) %>% 
      nrow()
    
    while(i != total + 1){
      message(
        paste0(
          "\nWhat should the retrieved value be? Max bag is ", 
          partytable$max_bag[i], "."))
      message(paste0("Entry: ", i, "/", total))
      cat(paste("Retrieved: ", partytable$original_retrieved[i]), "\n")
      cat(paste("Party size: ", partytable$party_size[i]), "\n")
      cat(paste("Comment: ", partytable$comment[i]))
      ANSWER <- toupper(scan(what = character(), nmax = 1, quiet = T))
      
      if(str_detect(ANSWER, "[0-9]{1,2}") == TRUE){
        message("Changed retrieved value to ", eval(parse_expr(ANSWER)), ".")
        partytable$new_retrieved[i] <- as.numeric(eval(parse_expr(ANSWER)))
        i <- i + 1
      }else if(ANSWER == "Y"){
        message("Next row.")
        i <- i + 1
      }else{
        message("Whoops. Enter a number, fraction, or 'Y'.")
        i <- i
      }
    }
    
    if(i == total + 1){
      message("Finished!")
      
      proofed_parties <- 
        partytable %>% 
        mutate(
          retrieved = 
            case_when(
              !is.na(new_retrieved) ~ new_retrieved,
              is.na(new_retrieved) ~ original_retrieved,
              TRUE ~ na_dbl)) %>% 
        select(
          surveyID, comment, state, sp_group_estimated, party_size, 
          original_retrieved, new_retrieved, retrieved) %>% 
        filter(!is.na(party_size))
      
      outpath <- c("data/clean_data/")
      write.csv(
        proofed_parties, 
        paste0(
          outpath, 
          "proofed_parties_", 
          str_extract(deparse(substitute(data)), "[0-9]{4}"),
          ".csv"), 
        row.names = F)
      
      return(
        partytable %>% 
          mutate(
            retrieved = 
              case_when(
                !is.na(new_retrieved) ~ new_retrieved,
                is.na(new_retrieved) ~ original_retrieved,
                TRUE ~ na_dbl)) %>% 
          select(
            -c("original_retrieved", "new_retrieved", "party_size", 
               "max_bag")) %>%
          relocate(retrieved, .before = "unretrieved")
      )
    }
  }
