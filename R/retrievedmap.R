#' Map mean retrieved birds per species and state
#'
#' The \code{retrievedmap} function creates a hex bin map of the United States from daily Harvest Survey data. Maps are plotted in a grid to display all species. States that are red have the highest mean harvest, states that are yellow have the lowest mean harvest, and blank (or white) states have no data.
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom stringr str_detect
#' @importFrom dplyr pull
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 geom_sf_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom patchwork wrap_plots
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 unit
#'  
#' @param data Daily data table
#' @param output Default is "grid"
#'  \itemize{
#'  \item "grid" - returns a single plot with a map for each species arranged in a grid
#'  \item "series" - returns each individual species plot in the "Plots" pane; toggle using the GUI left and right arrows
#'  }
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHS}
#' 
retrievedmap <- 
  function(data, output = "grid"){
    
    spp_data <- 
      retrieved(data) |>  
      rename(id = sampled_state)
    
    hexmap_complete <-
      hexmap |> 
      left_join(spp_data, by = "id") |> 
      filter(!is.na(sp_group_estimated)) |> 
      left_join(
        tibble(
          state_abbr = datasets::state.abb,
          id = datasets::state.name),
        by = "id")
    
    plot_list <- 
      map(
        spp_data |> 
          select(sp_group_estimated) |> 
          distinct() |> 
          mutate(
            sp_group_estimated = 
              ifelse(
                str_detect(sp_group_estimated, "Sea"), 
                "Sea Ducks", 
                sp_group_estimated)) |> 
          pull(),
        function(.x){
          hexmap_complete |> 
            mutate(
              sp_group_estimated = 
                ifelse(
                  str_detect(sp_group_estimated, "Sea"), 
                  "Sea Ducks", 
                  sp_group_estimated)) |> 
            filter(sp_group_estimated == .x) |> 
            ggplot() +
            geom_sf(
              ggplot2::aes(geometry = geometry, fill = mean_retrieved),
              color = "white") +
            geom_sf_text(
              ggplot2::aes(label = state),
              nudge_y = 0, size = 3.5) +
            labs(title = .x, fill = "Mean\nRetrieved") +  
            theme_void() +
            scale_fill_gradientn(
              colors = c("#ffffb2",  "#fb9e4f", "#e91332"),
              na.value = "white")  
        }
      )
    
    if(output == "series"){
      print(plot_list)
    }
    else if(output == "grid"){
      wrap_plots(
        map(plot_list, ~.x + theme(plot.margin = unit(c(0,0,0,.5), "cm"))), 
        ncol = 2)
    }
    else{
      message("Error: Invalid `ouput`. Use 'grid' or 'series'.")
    }
  }
