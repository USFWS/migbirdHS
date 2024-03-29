#' Check if records were submitted
#'
#' The \code{submitted} function checks the has_submitted field in either Harvest Survey daily or season totals data.
#'
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
#' @references \url{https://github.com/USFWS/migbirdHS}

submitted <- 
  function(data, type = "totals", output = "table"){
    # First, if a season totals table was used in this function, exclude daily
    # records from the season totals table. This allows the season totals data
    # to be evaluated separately from daily data
    if(TRUE %in% c(str_detect(deparse(substitute(data)), "season"), 
                   str_detect(deparse(substitute(data)), "tibblelist\\[3\\]"))){
      if(str_detect(deparse(substitute(data)), "season") == TRUE){
        dataname <- deparse(substitute(data))

        data <- 
          data |> 
          filter(
            !selected_hunterID %in%
              c(get("daily_records") |>
                  select(selected_hunterID) |>
                  pull())
          )
        message("Notice: season data NOT filtered to exclude daily records.")
        # Additional statement for report template compatibility
      }else if(str_detect(deparse(substitute(data)), "tibblelist\\[3\\]")){
        datayr <-
          data |>
          select(season) |>
          distinct() |>
          pull()

        data <-
          data |>
          filter(
            !selected_hunterID %in%
              c(get("daily_records") |>
                  select(selected_hunterID) |>
                  pull())
          )
        message("Notice: season data NOT filtered to exclude daily records.")
      }
    }
    # Second, if daily data are being used then add a col "days_hunted" based on
    # "has_hunted" column
    if(TRUE %in% c(str_detect(deparse(substitute(data)), "daily"), 
                   str_detect(deparse(substitute(data)), "tibblelist\\[2\\]"))){
      data <-
        data |> 
        mutate(days_hunted = ifelse(has_hunted == "Y", "1", "0"))
    }
    if(type == "totals"){
      if(output == "table"){
        data |>
          mutate(
            days_hunted = 
              ifelse(
                str_detect(days_hunted, "NULL"), 
                NA, 
                days_hunted),
            days_hunted = as.numeric(days_hunted)) |> 
          filter(days_hunted > 0) |> 
          select(selected_hunterID, has_submitted) |> 
          distinct() |> 
          group_by(has_submitted) |>
          summarize(n = n()) |> 
          ungroup()
      }else if(output == "plot"){
        data |>
          mutate(
            days_hunted = 
              ifelse(
                str_detect(days_hunted, "NULL"), 
                NA, 
                days_hunted),
            days_hunted = as.numeric(days_hunted)) |> 
          filter(days_hunted > 0) |>
          select(selected_hunterID, has_submitted) |> 
          distinct() |> 
          group_by(has_submitted) |>
          summarize(
            n = n(),
            prop = n()/nrow(data)) |> 
          ungroup() |> 
          ggplot(aes(x = has_submitted, y = prop)) +
          geom_bar(stat = "identity") +
          geom_text(
            aes(x = has_submitted, y = prop, label = n),
            vjust = 3, hjust = 0.5, color = "white") +
          labs(x = "Has submitted?", y = "Proportion") +
          theme_classic()
      }else{
        message("Error: `output` must be 'table' or 'plot'.")}
    }else if(type == "state"){
      if(output == "table"){
        data |>
          mutate(
            days_hunted = 
              ifelse(
                str_detect(days_hunted, "NULL"), 
                NA, 
                days_hunted),
            days_hunted = as.numeric(days_hunted)) |> 
          filter(days_hunted > 0) |>
          select(selected_hunterID, has_submitted, sampled_state) |> 
          distinct() |> 
          group_by(has_submitted, sampled_state) |>
          summarize(
            n = n(),
            prop = n()/nrow(data)) |> 
          ungroup()
      }else if(output == "plot"){
        suppressWarnings(
          data |>
            mutate(
              days_hunted = 
                ifelse(
                  str_detect(days_hunted, "NULL"), 
                  NA, 
                  days_hunted),
              days_hunted = as.numeric(days_hunted)) |> 
            filter(days_hunted > 0) |>
            select(selected_hunterID, has_submitted, sampled_state) |> 
            distinct() |> 
            group_by(has_submitted, sampled_state) |>
            summarize(
              n = n(),
              prop = n()/nrow(data)) |> 
            ungroup() |> 
            mutate(n_y = ifelse(has_submitted == "Y", n, NA)) |> 
            group_by(sampled_state) |> 
            mutate(tot_prop = sum(prop)) |> 
            ungroup() |> 
            mutate(ranks = rank(tot_prop)) |> 
            ggplot(aes(x = reorder(sampled_state, -ranks), y = prop)) +
            geom_bar(
              aes(fill = has_submitted), 
              stat = "identity") +
            geom_text(
              aes(x = reorder(sampled_state, -prop), 
                  y = tot_prop, 
                  label = ifelse(!is.na(n_y), paste0("Y = ", n_y), " "), 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            labs(x = "Has submitted?", y = "Proportion", 
                 col = "Has submitted?", fill = "Has submitted?") +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.5))) +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
        )
      }else{
        message("Error: `output` must be 'table' or 'plot'.")}
    }else if(type == "species"){
      if(output == "table"){
        data |>
          mutate(
            days_hunted = 
              ifelse(
                str_detect(days_hunted, "NULL"), 
                NA, 
                days_hunted),
            days_hunted = as.numeric(days_hunted)) |> 
          filter(days_hunted > 0) |>
          select(selected_hunterID, has_submitted, sp_group_estimated) |> 
          distinct() |> 
          group_by(has_submitted, sp_group_estimated) |>
          summarize(
            n = n(),
            prop = n()/nrow(data)) |> 
          ungroup()
      }else if(output == "plot"){
        suppressWarnings(
          data |>
            mutate(
              days_hunted = 
                ifelse(
                  str_detect(days_hunted, "NULL"), 
                  NA, 
                  days_hunted),
              days_hunted = as.numeric(days_hunted)) |> 
            filter(days_hunted > 0) |>
            select(selected_hunterID, has_submitted, sp_group_estimated) |> 
            distinct() |> 
            mutate(
              sp_group_estimated = 
                ifelse(
                  str_detect(sp_group_estimated, "Sea"), 
                  "Sea Ducks", 
                  sp_group_estimated)) |> 
            group_by(has_submitted, sp_group_estimated) |>
            summarize(
              n = n(),
              prop = n()/nrow(data)) |> 
            ungroup() |> 
            mutate(n_y = ifelse(has_submitted == "Y", n, NA)) |> 
            group_by(sp_group_estimated) |> 
            mutate(tot_prop = sum(prop)) |> 
            ungroup() |> 
            mutate(ranks = rank(tot_prop)) |> 
            ggplot(aes(x = reorder(sp_group_estimated, -ranks), y = prop)) +
            geom_bar(
              aes(fill = has_submitted), 
              stat = "identity") +
            geom_text(
              aes(x = reorder(sp_group_estimated, -prop), 
                  y = tot_prop, 
                  label = ifelse(!is.na(n_y), paste0("Y = ", n_y), " "), 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            labs(x = "Has submitted?", y = "Proportion", 
                 col = "Has submitted?", fill = "Has submitted?") +
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