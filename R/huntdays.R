#' Summarize days hunted in Harvest Survey daily data
#'
#' The \code{huntdays} function summarizes the total number of days hunted in the daily data.
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
#' @param data Daily data table
#' @param type Default is "both"
#'  \itemize{
#'  \item "both" - returns days hunted summarized by state and species
#'  \item "state" - returns days hunted summarized by state
#'  \item "species" - returns days hunted summarized by species
#'  }
#' @param output Default is "table"
#'  \itemize{
#'  \item "table" - returns a table
#'  \item "plot" - returns a ggplot
#'  }
#' @param average Default is TRUE
#'  \itemize{
#'  \item TRUE - returns average number of days
#'  \item FALSE - returns total number of days
#'  }
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}
#' 
huntdays <- 
  function(data, type = "both", output = "table", average = TRUE){
    if(type == "both"){
      if(output == "table"){
        if(average == FALSE){
          data |> 
            select(sampled_state, sp_group_estimated, harvested_date) |>
            group_by(sampled_state, sp_group_estimated) |>
            summarize(n_days = n()) |>
            ungroup()}
        else{
          data |> 
            select(
              selected_hunterID, sampled_state, sp_group_estimated, 
              harvested_date) |>
            group_by(selected_hunterID, sampled_state, sp_group_estimated) |>
            summarize(n_days = n()) |>
            ungroup() |> 
            group_by(sampled_state, sp_group_estimated) |> 
            summarize(mean_days = round(mean(n_days), 1)) |> 
            ungroup()}
      }else if(output == "plot"){
        if(average == FALSE){
          data |> 
            select(sampled_state, sp_group_estimated, harvested_date) |>
            group_by(sampled_state, sp_group_estimated) |>
            summarize(n_days = n()) |>
            ungroup() |> 
            group_by(sampled_state) |> 
            mutate(tot_days = sum(n_days)) |> 
            ungroup() |> 
            mutate(
              prop_sp = n_days/sum(n_days),
              prop_tot = tot_days/sum(n_days),
              sp_group_estimated = 
                ifelse(
                  str_detect(sp_group_estimated, "Sea"),
                  "Sea ducks",
                  sp_group_estimated)) |> 
            ggplot(
              aes(x = reorder(sampled_state, -prop_tot), 
                  y = prop_sp, 
                  fill = sp_group_estimated)) +
            geom_bar(stat = "identity") +
            geom_text(
              aes(x = reorder(sampled_state, -prop_tot), 
                  y = prop_tot, 
                  label = tot_days, 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.4))) +
            labs(
              x = "State", y = "Proportion of days hunted", fill = "Species") +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))}
        else{
          data |> 
            select(
              selected_hunterID, sampled_state, sp_group_estimated, 
              harvested_date) |>
            group_by(selected_hunterID, sampled_state, sp_group_estimated) |>
            summarize(n_days = n()) |>
            ungroup() |> 
            group_by(sampled_state, sp_group_estimated) |> 
            summarize(mean_days = round(mean(n_days), 1)) |> 
            ungroup() |> 
            group_by(sampled_state) |> 
            mutate(tot_days = sum(mean_days)) |> 
            ungroup() |> 
            mutate(
              sp_group_estimated = 
                ifelse(
                  str_detect(sp_group_estimated, "Sea"),
                  "Sea ducks",
                  sp_group_estimated)) |> 
            ggplot(
              aes(x = reorder(sampled_state, -tot_days), 
                  y = mean_days, 
                  fill = sp_group_estimated)) +
            geom_bar(stat = "identity") +
            geom_text(
              aes(x = reorder(sampled_state, -tot_days), 
                  y = tot_days, 
                  label = tot_days, 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.4))) +
            labs(
              x = "State", 
              y = "Average number of days\nhunted per hunter", 
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
            select(sp_group_estimated, harvested_date) |>
            group_by(sp_group_estimated) |>
            summarize(n_days = n()) |>
            ungroup()}
        else{
          data |> 
            select(
              selected_hunterID, sp_group_estimated, harvested_date) |>
            group_by(selected_hunterID, sp_group_estimated) |>
            summarize(n_days = n()) |>
            ungroup() |> 
            group_by(sp_group_estimated) |> 
            summarize(mean_days = round(mean(n_days), 1)) |> 
            ungroup()}
      }else if(output == "plot"){
        if(average == FALSE){
          data |> 
            select(sp_group_estimated, harvested_date) |>
            group_by(sp_group_estimated) |>
            summarize(n_days = n()) |>
            ungroup() |> 
            group_by(sp_group_estimated) |> 
            mutate(tot_days = sum(n_days)) |> 
            ungroup() |> 
            mutate(
              prop_tot = tot_days/sum(n_days),
              sp_group_estimated = 
                ifelse(
                  str_detect(sp_group_estimated, "Sea"),
                  "Sea ducks",
                  sp_group_estimated)) |> 
            ggplot(
              aes(x = reorder(sp_group_estimated, -prop_tot), y = prop_tot)) +
            geom_bar(stat = "identity") +
            geom_text(
              aes(x = reorder(sp_group_estimated, -prop_tot), 
                  y = prop_tot, 
                  label = tot_days, 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.4))) +
            labs(x = "Species", y = "Proportion of days hunted") +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))}
        else{
          data |> 
            select(selected_hunterID, sp_group_estimated, harvested_date) |>
            group_by(selected_hunterID, sp_group_estimated) |>
            summarize(n_days = n()) |>
            ungroup() |> 
            group_by(sp_group_estimated) |> 
            summarize(mean_days = round(mean(n_days), 1)) |> 
            ungroup() |> 
            group_by(sp_group_estimated) |> 
            mutate(tot_days = sum(mean_days)) |> 
            ungroup() |> 
            mutate(
              sp_group_estimated = 
                ifelse(
                  str_detect(sp_group_estimated, "Sea"),
                  "Sea ducks",
                  sp_group_estimated)) |> 
            ggplot(
              aes(x = reorder(sp_group_estimated, -tot_days), y = mean_days)) +
            geom_bar(stat = "identity") +
            geom_text(
              aes(x = reorder(sp_group_estimated, -tot_days), 
                  y = tot_days, 
                  label = tot_days, 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.4))) +
            labs(
              x = "Species", 
              y = "Average number of days\nhunted per hunter", 
              fill = "Species") +
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
            select(sampled_state, harvested_date) |>
            group_by(sampled_state) |>
            summarize(n_days = n()) |>
            ungroup()}
        else{
          data |> 
            select(
              selected_hunterID, sampled_state, harvested_date) |>
            group_by(selected_hunterID, sampled_state) |>
            summarize(n_days = n()) |>
            ungroup() |> 
            group_by(sampled_state) |> 
            summarize(mean_days = round(mean(n_days), 1)) |> 
            ungroup()}
      }else if(output == "plot"){
        if(average == FALSE){
          data |> 
            select(sampled_state, harvested_date) |>
            group_by(sampled_state) |>
            summarize(n_days = n()) |>
            ungroup() |> 
            group_by(sampled_state) |> 
            mutate(tot_days = sum(n_days)) |> 
            ungroup() |> 
            mutate(prop_tot = tot_days/sum(n_days)) |> 
            ggplot(
              aes(x = reorder(sampled_state, -prop_tot), y = prop_tot)) +
            geom_bar(stat = "identity") +
            geom_text(
              aes(x = reorder(sampled_state, -prop_tot), 
                  y = prop_tot, 
                  label = tot_days, 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.4))) +
            labs(x = "Species group", y = "Proportion of days hunted") +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))}
        else{
          data |> 
            select(selected_hunterID, sampled_state, harvested_date) |>
            group_by(selected_hunterID, sampled_state) |>
            summarize(n_days = n()) |>
            ungroup() |> 
            group_by(sampled_state) |> 
            summarize(mean_days = round(mean(n_days), 1)) |> 
            ungroup() |> 
            group_by(sampled_state) |> 
            mutate(tot_days = sum(mean_days)) |> 
            ungroup() |> 
            ggplot(aes(x = reorder(sampled_state, -tot_days), y = mean_days)) +
            geom_bar(stat = "identity") +
            geom_text(
              aes(x = reorder(sampled_state, -tot_days), 
                  y = tot_days, 
                  label = tot_days, 
                  angle = 90),
              vjust = 0.2, hjust = -0.2) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.4))) +
            labs(x = "State", y = "Average number of days\nhunted per hunter") +
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