#' Summarize total number of retrieved birds per hunter
#'
#' The \code{bagspp} function summarizes daily Harvest Survey data to determine the total number of retrieved birds per hunter. The hunter’s data are further broken down to determine which species groups the hunter responded to harvesting.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr n
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom treemapify geom_treemap
#' @importFrom treemapify geom_treemap_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom MetBrewer met.brewer
#' @importFrom ggplot2 theme 
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' 
#' @param data Daily data table
#' @param output Default is "table"
#'  \itemize{
#'  \item "table" - returns a table that reports the total number of species groups, the species group combination, and retrieved total per hunter
#'  \item "n" - returns a treemap of the number of species groups per hunter
#'  \item "species" - returns a treemap of the species group combinations represented in the data; each hunter’s pursued species are grouped for this visualization
#'  \item "success" - excludes "retrieved = 0" records to return a treemap of the species group combinations that were successfully hunted; each hunter’s pursued species are grouped for this visualization
#'  }
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}
#' 
bagspp <-
  function(data, output = "table"){
    if(output == "table"){
      data %>%
        select(selected_hunterID, sp_group_estimated, retrieved) %>%
        rename(sp = sp_group_estimated) %>% 
        mutate(sp = ifelse(str_detect(sp, "Sea"), "Sea Ducks", sp)) %>% 
        group_by(selected_hunterID, sp) %>%
        summarize(n_retrieved = sum(retrieved)) %>% 
        ungroup() %>% 
        group_by(selected_hunterID) %>% 
        summarize(
          n_spp = n(),
          spp_combo = paste(sp, collapse = "-"),
          n_retrieved_total = sum(n_retrieved)) %>%
        ungroup()
    }else if(output == "species"){
      treedata <-
        data %>%
        select(selected_hunterID, sp_group_estimated, retrieved) %>%
        rename(sp = sp_group_estimated) %>% 
        mutate(
          sp = 
            case_when(
              str_detect(sp, "Sea") ~ "Sea Ducks", 
              str_detect(sp, "Mourning") ~ "MODO", 
              str_detect(sp, "White") ~ "WWDO", 
              TRUE ~ sp)) %>% 
        group_by(selected_hunterID, sp) %>%
        summarize(n_retrieved = sum(retrieved)) %>% 
        ungroup() %>% 
        group_by(selected_hunterID) %>% 
        summarize(
          n_retrieved_tot = sum(n_retrieved),
          spp_combo = paste(sp, collapse = "-"),
          n_spp = n()) %>%
        ungroup() %>% 
        rename(n_retrieved = n_retrieved_tot) %>% 
        filter(n_retrieved != 0) %>% 
        # For treemap, summarize
        group_by(spp_combo) %>% 
        summarize(n_tot = sum(n_retrieved)) %>% 
        ungroup() %>% 
        mutate(
          prop = round(n_tot/sum(n_tot), 2)*100,
          spp_label = paste0(spp_combo, " (", prop, "%)"))
      
      treedata %>% 
        ggplot(aes(area = n_tot, fill = spp_combo, label = spp_label)) +
        geom_treemap(color = "black") +
        geom_treemap_text(
          colour = c(rep("white", length(unique(treedata$spp_label)))),
          place = "topleft", size = 12) +
        labs(fill = "Species") +
        guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
        scale_fill_manual(
          values = met.brewer("Hokusai3",
                              length(unique(treedata$spp_label)))) +
        theme(legend.position = "bottom")
    }else if(output == "n"){
      treedata <- 
        data %>%
        select(selected_hunterID, sp_group_estimated, retrieved) %>%
        rename(sp = sp_group_estimated) %>% 
        group_by(selected_hunterID, sp) %>%
        summarize(n_retrieved = sum(retrieved)) %>% 
        ungroup() %>% 
        group_by(selected_hunterID) %>% 
        summarize(
          n_retrieved_tot = sum(n_retrieved),
          #spp_combo = paste(sp, collapse = "-"),
          n_spp = as.character(n())) %>%
        ungroup() %>% 
        # For treemap, summarize
        group_by(n_spp) %>% 
        summarize(n_tot = sum(n_retrieved_tot)) %>% 
        ungroup() %>% 
        mutate(
          prop = round(n_tot/sum(n_tot), 2)*100,
          spp_label = paste0(n_spp, " (", prop, "%)"))
      
      treedata %>% 
        ggplot(aes(area = n_tot, fill = n_spp, label = spp_label)) +
        geom_treemap(color = "black") +
        geom_treemap_text(colour = c(rep("white", nrow(treedata))),
                          place = "topleft", size = 12) +
        labs(fill = "Number of species") +
        scale_fill_manual(values = met.brewer("Hokusai3", nrow(treedata))) +
        theme(legend.position = "bottom")}
    else if(output == "success"){
      treedata <-
        data %>%
        filter(retrieved > 0) %>% 
        select(selected_hunterID, sp_group_estimated) %>%
        distinct() %>% 
        rename(sp = sp_group_estimated) %>% 
        mutate(
          sp = 
            case_when(
              str_detect(sp, "Sea") ~ "Sea Ducks", 
              str_detect(sp, "Mourning") ~ "MODO", 
              str_detect(sp, "White") ~ "WWDO", 
              TRUE ~ sp)) %>% 
        group_by(selected_hunterID) %>% 
        summarize(spp_combo = paste(sp, collapse = "-")) %>%
        ungroup() %>% 
        # For treemap, summarize
        group_by(spp_combo) %>% 
        summarize(n = n()) %>% 
        ungroup() %>% 
        mutate(
          prop = round(n/sum(n), 2)*100,
          spp_label = paste0(spp_combo, " (", prop, "%)"))
      
      treedata %>% 
        ggplot(aes(area = n, fill = spp_combo, label = spp_label)) +
        geom_treemap(color = "black") +
        geom_treemap_text(
          colour = c(rep("white", length(unique(treedata$spp_label)))),
          place = "topleft", size = 12) +
        labs(fill = "Species") +
        guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
        scale_fill_manual(
          values = met.brewer("Hokusai3",
                              length(unique(treedata$spp_label)))) +
        theme(legend.position = "bottom")
    }else{
      message("Error: `output` must be 'table', 'n', 'species', or 'success'.")
    }
  }