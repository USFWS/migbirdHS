#' Check if hunters hunted
#'
#' The \code{hunted} function checks the days_hunted field in either Harvest Survey daily or season totals data.
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#' @importFrom stringr str_extract
#' @importFrom dplyr select
#' @importFrom dplyr pull
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 expansion
#' @importFrom ggplot2 element_text
#' @importFrom dplyr mutate
#' @importFrom stats reorder
#' 
#' @param data Daily or season data table
#' @param type "totals" for a summary of all Y and N responses, "state" to view Y and N responses by state, or "species" to view Y and N responses by species group
#' @param output "table" or "plot"
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}

hunted <- 
  function(data, type = "totals", output = "table"){
    # First, if a season totals table was used in this function, exclude daily
    # records from the season totals table. This allows the season totals data
    # to be evaluated separately from daily data
    if(TRUE %in% c(str_detect(deparse(substitute(data)), "season"), 
                   str_detect(deparse(substitute(data)), "tibblelist\\[3\\]"))){
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
    # Second, if daily data are being used then add a col "days_hunted" based on
    # "has_hunted" column
    if(TRUE %in% c(str_detect(deparse(substitute(data)), "daily"), 
                   str_detect(deparse(substitute(data)), "tibblelist\\[2\\]"))){
      data <-
        data %>% 
        mutate(days_hunted = ifelse(has_hunted == "Y", 1, 0))
    }
    if(type == "totals"){
      if(output == "table"){
        data %>%
          select(selected_hunterID, days_hunted) %>% 
          distinct() %>% 
          group_by(days_hunted) %>%
          summarize(n = n()) %>% 
          ungroup()
      }else if(output == "plot"){
        data %>%
          select(selected_hunterID, days_hunted) %>% 
          distinct() %>% 
          mutate(
            days_hunted = 
              ifelse(
                str_detect(days_hunted, "NULL"), 
                NA, 
                days_hunted),
            has_hunted = ifelse(days_hunted > 0, "Y", "N")) %>% 
          group_by(has_hunted) %>%
          summarize(
            n = n(),
            prop = n()/nrow(data)) %>% 
          ungroup() %>% 
          ggplot(aes(x = has_hunted, y = prop)) +
          geom_bar(stat = "identity") +
          geom_text(
            aes(x = has_hunted, y = prop, label = n),
            vjust = 3, hjust = 0.5, color = "white") +
          labs(x = "Has hunted?", y = "Proportion") +
          theme_classic()
      }else{
        message("Error: `output` must be 'table' or 'plot'.")}
    }else if(type == "state"){
      if(output == "table"){
        data %>%
          select(selected_hunterID, days_hunted, sampled_state) %>% 
          distinct() %>% 
          group_by(days_hunted, sampled_state) %>%
          summarize(
            n = n(),
            prop = n()/nrow(data)) %>% 
          ungroup()
      }else if(output == "plot"){
        suppressWarnings(
          data %>%
            select(selected_hunterID, days_hunted, sampled_state) %>% 
            distinct() %>% 
            mutate(
              days_hunted = 
                ifelse(
                  str_detect(days_hunted, "NULL"), 
                  NA, 
                  days_hunted),
              has_hunted = ifelse(days_hunted > 0, "Y", "N")) %>% 
            group_by(has_hunted, sampled_state) %>%
            summarize(n = n()) %>% 
            ungroup() %>% 
            mutate(n_n = ifelse(has_hunted == "N", n, NA)) %>% 
            group_by(sampled_state) %>% 
            mutate(tot_n = sum(n)) %>% 
            ungroup() %>% 
            mutate(ranks = rank(tot_n)) %>% 
            ggplot(aes(x = reorder(sampled_state, -ranks), y = n)) +
            geom_bar(
              aes(fill = has_hunted), 
              stat = "identity") +
            geom_text(
              aes(x = reorder(sampled_state, -ranks),
                  y = tot_n,
                  label = ifelse(!is.na(n_n), paste0("N = ", n_n), " "),
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            labs(x = "Has hunted?", y = "Count", 
                 col = "Has hunted?", fill = "Has hunted?") +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.5))) +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
        )
      }else{
        message("Error: `output` must be 'table' or 'plot'.")}
    }else if(type == "species"){
      if(output == "table"){
        data %>%
          select(selected_hunterID, days_hunted, sp_group_estimated) %>% 
          distinct() %>% 
          group_by(days_hunted, sp_group_estimated) %>%
          summarize(
            n = n(),
            prop = n()/nrow(data)) %>% 
          ungroup()
      }else if(output == "plot"){
        suppressWarnings(
          data %>%
            select(selected_hunterID, days_hunted, sp_group_estimated) %>% 
            distinct() %>% 
            mutate(
              days_hunted = 
                ifelse(
                  str_detect(days_hunted, "NULL"), 
                  NA, 
                  days_hunted),
              has_hunted = ifelse(days_hunted > 0, "Y", "N")) %>% 
            mutate(
              sp_group_estimated = 
                ifelse(
                  str_detect(sp_group_estimated, "Sea"),
                  "Sea Ducks",
                  sp_group_estimated)) %>% 
            group_by(has_hunted, sp_group_estimated) %>%
            summarize(n = n()) %>% 
            ungroup() %>% 
            filter(!is.na(has_hunted)) %>% 
            mutate(n_n = ifelse(has_hunted == "N", n, NA)) %>% 
            group_by(sp_group_estimated) %>% 
            mutate(tot_n = sum(n)) %>% 
            ungroup() %>% 
            mutate(ranks = rank(tot_n)) %>% 
            ggplot(aes(x = reorder(sp_group_estimated, -ranks), y = n)) +
            geom_bar(
              aes(fill = has_hunted), 
              stat = "identity") +
            geom_text(
              aes(x = reorder(sp_group_estimated, -ranks),
                  y = tot_n,
                  label = ifelse(!is.na(n_n), paste0("N = ", n_n), " "),
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            labs(x = "Has hunted?", y = "Count", 
                 col = "Has hunted?", fill = "Has hunted?") +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.5))) +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
        )
      }else{
        message("Error: `output` must be 'table' or 'plot'.")}
    }else{
      message("Error: `type` must be 'totals', 'state', or 'species'.")
    }
  }