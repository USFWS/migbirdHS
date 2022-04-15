#' Adjust group hunts reported in Harvest Survey daily data
#'
#' The \code{partyproof} function checks for, and allows the user to edit, any group hunts in the daily Harvest Survey data. Comments are parsed to determine party size. Adjustments to number of birds retrieved must be entered manually, and if deemed necessary, the retrieved value will be divided by the party size. A report file is written out as a .csv to record all changes made.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr slice
#' @importFrom dplyr pull
#' @importFrom rlang parse_expr
#' @importFrom utils write.csv
#' @importFrom dplyr relocate
#' @importFrom rlang na_dbl
#' 
#' @param data Daily data table
#' @param outpath Path to write the change log .csv; must end with a forward slash
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}
#' 
partyproof <- 
  function(data, outpath){
    
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
            str_detect(comment, "party of [0-9]{1,2}") & retrieved != 0 ~ 
              str_extract(comment, "party of [0-9]{1,2}") %>% 
              str_remove(., "party of "),
            str_detect(comment, "group of [0-9]{1,2}") & retrieved != 0 ~ 
              str_extract(comment, "group of [0-9]{1,2}") %>% 
              str_remove(., "group of "),
            TRUE ~ NA_character_)) %>% 
      rename(original_retrieved = retrieved) %>% 
      mutate(new_retrieved = NA) %>% 
      arrange(desc(party_size))
    
    i <- 1
    total <- 
      partytable %>% 
      select(party_size) %>%
      filter(!is.na(party_size)) %>% 
      nrow()
    
    while(i != total + 1){
      message("\nWhat should the retrieved value be?")
      message(paste0("Entry: ", i, "/", total))
      cat(
        paste(
          "Retrieved: ", 
          partytable %>% 
            slice(i) %>% 
            select(original_retrieved) %>% 
            pull()), 
        "\n")
      cat(
        paste(
          "Party size: ", 
          partytable %>% 
            slice(i) %>% 
            select(party_size) %>% 
            pull()), 
        "\n")
      cat(
        paste(
          "Comment: ", 
          partytable %>% 
            slice(i) %>% 
            select(comment) %>% 
            pull()))
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
          surveyID, comment, party_size, original_retrieved, new_retrieved,
          retrieved) %>% 
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
          select(-c("original_retrieved", "new_retrieved", "party_size")) %>%
          relocate(retrieved, .before = "unretrieved")
      )
    }
  }
