#' Summarize number of birds retrieved in Harvest Survey data
#'
#' The \code{retrieved} function summarizes the total number of birds retrieved in the daily or season data.
#' 
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom stringr str_detect
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom stats reorder
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 expansion
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' 
#' @param data Daily or season data table
#' @param type Default is "both"
#'  \itemize{
#'  \item "both" - returns birds retrieved summarized by state and species
#'  \item "state" - returns birds retrieved summarized by state
#'  \item "species" - returns birds retrieved summarized by species
#'  }
#' @param output Default is "table"
#'  \itemize{
#'  \item "table" - returns a table
#'  \item "plot" - returns a ggplot
#'  }
#' @param average Default is TRUE
#'  \itemize{
#'  \item TRUE - returns average birds retrieved
#'  \item FALSE - returns total birds retrieved
#'  }
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHS}
#' 
retrieved <- 
  function(data, type = "both", output = "table", average = TRUE){
    # First, if a season totals table was used in this function, exclude daily
    # records from the season totals table. This allows the season totals data
    # to be evaluated separately from daily data
    if(str_detect(deparse(substitute(data)), "season") == TRUE | 
       str_detect(deparse(substitute(data)), "tibblelist\\[3\\]")){
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
        message("Notice: season data filtered to exclude daily records.")
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
        message("Notice: season data filtered to exclude daily records.")
      }
    }
    if(type == "both"){
      if(output == "table"){
        if(average == FALSE){
          data |> 
            select(sampled_state, sp_group_estimated, retrieved) |>
            group_by(sampled_state, sp_group_estimated) |>
            summarize(n_retrieved = sum(retrieved, na.rm = T)) |>
            ungroup()}
        else{
          data |> 
            select(
              selected_hunterID, sampled_state, sp_group_estimated, 
              retrieved) |>
            group_by(selected_hunterID, sampled_state, sp_group_estimated) |>
            summarize(n_retrieved = sum(retrieved, na.rm = T)) |>
            ungroup() |> 
            group_by(sampled_state, sp_group_estimated) |> 
            summarize(mean_retrieved = round(mean(n_retrieved), 1)) |> 
            ungroup()}
      }else if(output == "plot"){
        if(average == FALSE){
          data |> 
            mutate(
              sp_group_estimated = 
                ifelse(
                  str_detect(sp_group_estimated, "Sea"), 
                  "Sea Ducks", 
                  sp_group_estimated)) |> 
            select(sampled_state, sp_group_estimated, retrieved) |>
            group_by(sampled_state, sp_group_estimated) |>
            summarize(n_retrieved = sum(retrieved, na.rm = T)) |>
            ungroup() |> 
            group_by(sampled_state) |> 
            mutate(tot_r = sum(n_retrieved, na.rm = T)) |> 
            ungroup() |> 
            mutate(
              prop_sp = n_retrieved/sum(n_retrieved, na.rm = T),
              prop_tot = tot_r/sum(n_retrieved, na.rm = T)) |> 
            ggplot(
              aes(x = reorder(sampled_state, -prop_tot), 
                  y = prop_sp, 
                  fill = sp_group_estimated)) +
            geom_bar(stat = "identity") +
            geom_text(
              aes(x = reorder(sampled_state, -prop_tot), 
                  y = prop_tot, 
                  label = tot_r, 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.4))) +
            labs(
              x = "State", 
              y = "Proportion of birds retrieved", 
              fill = "Species") +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))}
        else{
          data |> 
            mutate(
              sp_group_estimated = 
                ifelse(
                  str_detect(sp_group_estimated, "Sea"), 
                  "Sea Ducks", 
                  sp_group_estimated)) |> 
            select(
              selected_hunterID, sampled_state, sp_group_estimated, 
              retrieved) |>
            group_by(selected_hunterID, sampled_state, sp_group_estimated) |>
            summarize(n_retrieved = sum(retrieved, na.rm = T)) |>
            ungroup() |> 
            group_by(sampled_state, sp_group_estimated) |> 
            summarize(mean_retrieved = round(mean(n_retrieved), 1)) |> 
            ungroup() |> 
            group_by(sampled_state) |> 
            mutate(tot_r = sum(mean_retrieved)) |> 
            ungroup() |> 
            ggplot(
              aes(x = reorder(sampled_state, -tot_r), 
                  y = mean_retrieved, 
                  fill = sp_group_estimated)) +
            geom_bar(stat = "identity") +
            geom_text(
              aes(x = reorder(sampled_state, -tot_r), 
                  y = tot_r, 
                  label = tot_r, 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.4))) +
            labs(
              x = "State", 
              y = "Average number of birds\nretrieved per hunter", 
              fill = "Species") +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
        }
      }else{
        message("Error: `output` must be 'table' or 'plot'.")
      }
    }else if(type == "species"){
      if(output == "table"){
        if(average == FALSE){
          data |> 
            select(sp_group_estimated, retrieved) |>
            group_by(sp_group_estimated) |>
            summarize(n_retrieved = sum(retrieved, na.rm = T)) |>
            ungroup()}
        else{
          data |> 
            select(selected_hunterID, sp_group_estimated, retrieved) |>
            group_by(selected_hunterID, sp_group_estimated) |>
            summarize(n_retrieved = sum(retrieved, na.rm = T)) |>
            ungroup() |> 
            group_by(sp_group_estimated) |> 
            summarize(mean_retrieved = round(mean(n_retrieved), 1)) |> 
            ungroup()}
      }else if(output == "plot"){
        if(average == FALSE){
          data |> 
            mutate(
              sp_group_estimated = 
                ifelse(
                  str_detect(sp_group_estimated, "Sea"), 
                  "Sea Ducks", 
                  sp_group_estimated)) |> 
            select(sp_group_estimated, retrieved) |>
            group_by(sp_group_estimated) |>
            summarize(n_retrieved = sum(retrieved, na.rm = T)) |>
            ungroup() |> 
            group_by(sp_group_estimated) |> 
            mutate(tot_r = sum(n_retrieved)) |> 
            ungroup() |> 
            mutate(prop_tot = tot_r/sum(n_retrieved)) |> 
            ggplot(
              aes(x = reorder(sp_group_estimated, -prop_tot), y = prop_tot)) +
            geom_bar(stat = "identity") +
            geom_text(
              aes(x = reorder(sp_group_estimated, -prop_tot), 
                  y = prop_tot, 
                  label = tot_r, 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.4))) +
            labs(x = "Species", y = "Proportion of birds retrieved") +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))}
        else{
          data |> 
            mutate(
              sp_group_estimated = 
                ifelse(
                  str_detect(sp_group_estimated, "Sea"), 
                  "Sea Ducks", 
                  sp_group_estimated)) |> 
            select(selected_hunterID, sp_group_estimated, retrieved) |>
            group_by(selected_hunterID, sp_group_estimated) |>
            summarize(n_retrieved = sum(retrieved, na.rm = T)) |>
            ungroup() |> 
            group_by(sp_group_estimated) |> 
            summarize(mean_retrieved = round(mean(n_retrieved), 1)) |> 
            ungroup() |> 
            ggplot(
              aes(x = reorder(sp_group_estimated, -mean_retrieved), 
                  y = mean_retrieved)) +
            geom_bar(stat = "identity") +
            geom_text(
              aes(x = reorder(sp_group_estimated, -mean_retrieved), 
                  y = mean_retrieved, 
                  label = mean_retrieved, 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.4))) +
            labs(
              x = "Species", 
              y = "Average number of birds\nretrieved per hunter") +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
        }
      }else{
        message("Error: `output` must be 'table' or 'plot'.")
      }
    }else if(type == "state"){
      if(output == "table"){
        if(average == FALSE){
          data |> 
            select(sampled_state, retrieved) |>
            group_by(sampled_state) |>
            summarize(n_retrieved = sum(retrieved, na.rm = T)) |>
            ungroup()
        }else{
          data |> 
            select(selected_hunterID, sampled_state, retrieved) |>
            group_by(selected_hunterID, sampled_state) |>
            summarize(n_retrieved = sum(retrieved, na.rm = T)) |>
            ungroup() |> 
            group_by(sampled_state) |> 
            summarize(mean_retrieved = round(mean(n_retrieved), 1)) |> 
            ungroup()}
      }else if(output == "plot"){
        if(average == FALSE){
          data |> 
            select(sampled_state, retrieved) |>
            group_by(sampled_state) |>
            summarize(n_retrieved = sum(retrieved, na.rm = T)) |>
            ungroup() |> 
            group_by(sampled_state) |> 
            mutate(tot_r = sum(n_retrieved)) |> 
            ungroup() |> 
            mutate(prop_tot = tot_r/sum(n_retrieved)) |> 
            ggplot(
              aes(x = reorder(sampled_state, -prop_tot), y = prop_tot)) +
            geom_bar(stat = "identity") +
            geom_text(
              aes(x = reorder(sampled_state, -prop_tot), 
                  y = prop_tot, 
                  label = tot_r, 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.4))) +
            labs(x = "State", y = "Proportion of birds retrieved") +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))}
        else{
          data |> 
            select(selected_hunterID, sampled_state, retrieved) |>
            group_by(selected_hunterID, sampled_state) |>
            summarize(n_retrieved = sum(retrieved, na.rm = T)) |>
            ungroup() |> 
            group_by(sampled_state) |> 
            summarize(mean_retrieved = round(mean(n_retrieved), 1)) |> 
            ungroup() |> 
            ggplot(
              aes(x = reorder(sampled_state, -mean_retrieved), 
                  y = mean_retrieved)) +
            geom_bar(stat = "identity") +
            geom_text(
              aes(x = reorder(sampled_state, -mean_retrieved), 
                  y = mean_retrieved, 
                  label = mean_retrieved, 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.4))) +
            labs(
              x = "State", 
              y = "Average number of birds\nretrieved per hunter") +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
        }
      }else{
        message("Error: `output` must be 'table' or 'plot'.")
      }
    }else{
      message(
        "Error: Invalid `type`. Use 'state', 'species', or 'both'.")
    }
  }