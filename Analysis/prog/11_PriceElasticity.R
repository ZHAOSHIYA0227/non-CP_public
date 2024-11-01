# The Rscript for the agricultural price and price elasticities in the non-CP/CP paper
# This analysis is performed to reveal the reason why the hunger risk is less responsive to the agriculture productivity assumptions
# Shiya ZHAO, 2024.04.01


# load packages -----------------------------------------------------------

require(tidyverse)
require(ggplot2)
require(gdxrrw)

# load price elasticity ---------------------------------------------------

# from AIMHub
pela_hub_tmp <- rgdx.param(paste0(dir_cgeoutput,"/global/global_17/gdx/analysis_agr.gdx"), "Pagmipout") 

pela_hub <- pela_hub_tmp %>% gdata::rename.vars(colnames(.), c("SCENARIO", "Y", "R", "COM", "Variable", "DUM", "Value")) %>% filter(Variable == "PELA", Y %in% seq(2025,2100,5)) %>% 
  left_join(MapScenario) %>% filter(!is.na(Ref)) %>% F_woc() %>% select(-SCENARIO, -DUM) %>% 
  filter(exemption == "None", tech == "None", Gini == "consistent")
rm(pela_hub_tmp)


# from AIMPHI
# => cannot read from cluster: no permissions


