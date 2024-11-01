   # This file tried to plot the poverty headcount, tax revenue, and distributional effects in world maps
# Shiya ZHAO, 2023/06/19

library(tidyverse)
library(maps)
library(ggplot2)
library(sf)
# 
# data=PoV_log
# lis_plot_y="2030"
# plotvalue = log(PoV$value/1000000)
# legendName = "log(PovertyHeadcount) \n Headcount: million"


# To identify if this script is sourced or run directly ----
# if (sys.nframe() == 0L) {
#   # Then it is run directly
#   print("file not sourced")
#   
#   dir_map <- dirname(rstudioapi::getSourceEditorContext()$path)
#   print(dir_map)
#   
# }else{
#   # Then it is sources
#   print("file sourced")
#   
#.  # This function doesn't work for obtaining directory path!!
#   dir_map <- tempfile()
#   writeLines("f()", dir_map)
#   f <- function() print(sys.frame(-5)$srcfile)
#   source(dir_map)
#   print(dir_map)
#   rm(f)
#   
# }

# You can also specify it interactively. Here is an example
# sw_s <- readline("Is it sourced? y/n") 
# if(sw_s == y){
#   dir_map <- tempfile()
#   writeLines("f()", dir_map)
#   f <- function() print(sys.frame(-4)$srcfile)
#   source(dir_map)
#   dir_map
#   rm(f)
# }else{
#   dir_map <- dirname(rstudioapi::getSourceEditorContext()$path)
#   print(dir_map)
# }




# Function ----
F_plot_globalMap <- function(data=pdata, lis_plot_y="2030", lis_plot_scenario = c("1.5D"), by = c("R_CGE"),
                             legendName = "log(PovertyHeadcount) \n Headcount: million ", df_map = map_R){


  
  outline <- st_read( paste0("../", prog_loc, "/inc_prog/0_PlotWorldMap/chinasheng_world/country1.shp") ) 
  df_world_map_tmp <- outline %>% 
    mutate(R = case_when(GMI_CNTRY == "MON" ~ "MNE",
                         GMI_CNTRY == "ROM" ~ "ROU",
                         GMI_CNTRY == "ZAR" ~ "COD",
                         !(GMI_CNTRY %in% c("MON","ROM", "ZAR")) ~ GMI_CNTRY)) 

  df_world_map <- df_world_map_tmp %>% 
    select("R", "GMI_CNTRY", "SOVEREIGN", "geometry") %>% 
    left_join(df_map) %>% 
    mutate(R_CGE = case_when(is.na(R_CGE) ~ "none", !is.na(R_CGE) ~ R_CGE)) %>%
    # filter(is.na(R_CGE)) %>% 
    # filter(SOVEREIGN == "Zaire")
    full_join(data, by = by, relationship = "many-to-many") 
  
    pdata_map <- df_world_map %>%
      filter(R != "WLD") %>% 
      filter(Y %in% lis_plot_y) 
    
    lis_plot_model <- unique(data$model) %>% as.character()
    
    if(!length(lis_plot_model)){
      lis_plot_model <- c("default")
      pdata_map <- pdata_map %>% mutate(model = "default")
    }

    pdata_map_tmp <- data.frame()
    for(y in 1:length(lis_plot_y)){
      for(scenario in 1:length(lis_plot_scenario)){
        for(model in 1:length(lis_plot_model)){
          pdata_map_tmp0 <- df_world_map %>%
            filter(R != "WLD") %>% 
            filter(is.na(Y))
          
          pdata_map_tmp0$Y <- lis_plot_y[y]
          pdata_map_tmp0$scenario <- lis_plot_scenario[scenario]
          pdata_map_tmp0$model <- lis_plot_model[model]
          
          pdata_map_tmp <- pdata_map_tmp %>%  rbind(pdata_map_tmp0)
        }
      }

    }
     
    pdata_map <- pdata_map %>% 
      rbind(pdata_map_tmp) %>% filter(R_CGE != "World")
    p1 <- ggplot() + 
      geom_sf(data = pdata_map, mapping = aes(fill = value_plot), color = "grey50",  
              size = 0.1) +
      # ggthemes::theme_map() +
      MyTheme +
      theme(axis.title.x = element_blank(),
            axis.ticks.x = element_blank(), 
            axis.line.x = element_blank(), 
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.line.y = element_blank(), 
            axis.text.y = element_blank(),
            legend.position = "bottom") +
      labs(fill = legendName) +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      facet_grid(~Y)
  
    p1
  
  return(p1)
}

