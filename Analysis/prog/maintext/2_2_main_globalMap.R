# The Rscript for the global maps in the non-CP/CP paper
# Shiya ZHAO, 2024.05.17

# Map plot of the benefits of non-CP decarbonization
library(fmsb)

dir.create(paste0(dir_plot_main, "/2_2_Map/"))



# 1. data preparations ----------------------------------------------------


## Poverty ----
df_PoV <- rgdx.param(AnaExp, "PoVExp") %>%
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R), PoVExp = PoVExp/1000000) %>% 
  filter(!is.na(Ref), R %in% lis_Rphi, Y%in% lis_Y) %>%  
  mutate(TH1 = recode(TH, 'pop_1.9' = lis_TH[1], 'pop_3.2' = lis_TH[2], 'pop_5.5' = lis_TH[3]),
         R = factor(R, levels = lis_Rphi), unit = "million") %>% F_filter(s_Baseline_woc = F, lis_R = lis_Rphi) %>% F_woc() 




## Hunger risk ----
df_hunger_r <- paste0("../../DataArchive/hungertool/240627/AllModel_Country_AIMHub.gdx") %>%
  rgdx.param("Risk") %>% gdata::rename.vars(colnames(.), c("model", "U", "SCENARIO", "R_code","Y", "value")) %>% 
  left_join(MapScenario_hunger) %>% left_join(map_Rfao2iso3) %>% select(-R_code) %>% 
  F_woc() %>% left_join(map_R17) %>% left_join(map_R17to5) %>% select(-SCENARIO, -R_full, -R_CGE) %>% mutate(unit = "Million people") %>% 
  filter(U == "model_based")




## Gini ----
df_Gini <- rgdx.param(AnaExp, "Gini_exp") %>%
  F_filter(lis_R=lis_Rphi) %>% F_woc() %>% mutate(Gini_exp = Gini_exp * 100)




## Data integration ------------------------------------------------------

### Poverty headcount change ----
# unit: person
# $1.9 threshold
df1_PoV <- df_PoV %>% filter(exemption == "None", TH1 == "1.9-threshold") %>% select(-Ref, -target, -TH, -TH1, -unit) %>% 
  pivot_wider(values_from = "PoVExp", names_from = "policy") %>% mutate(variable = "Poverty headcount change (Int.)") %>% 
  mutate(`1.5C CP` = (`1.5C CP` - `Baseline`), `1.5C non-CP` = (`1.5C non-CP` - `Baseline`)) %>% select(-Baseline, -`Baseline non-CP`)




df1_PoV_55 <- df_PoV %>% filter(exemption == "None", TH1 == "5.5-threshold") %>% select(-Ref, -target, -TH, -TH1, -unit) %>% 
  pivot_wider(values_from = "PoVExp", names_from = "policy") %>% mutate(variable = "Poverty headcount change (UMIC)") %>% 
  mutate(`1.5C CP` = (`1.5C CP` - `Baseline`), `1.5C non-CP` = (`1.5C non-CP` - `Baseline`)) %>% select(-Baseline, -`Baseline non-CP`)






### Hunger risk ----
# unit: million
# colnames(df1_hug)
df1_hug <- df_hunger_r %>% #filter(SSP == "SSP2") %>% 
  select(-target, -Ref, -unit) %>% pivot_wider(values_from = "value", names_from = "policy") %>% 
  mutate(variable = "Hunger risk change") %>% 
  mutate(`1.5C CP` = (`1.5C CP` - `Baseline`), `1.5C non-CP` = (`1.5C non-CP` - `Baseline`)) %>% select(-Baseline, -`Baseline non-CP`) %>% 
  filter(U == "model_based") %>% select(-U, -model, -R5, -`NA`) %>% distinct()



### Gini ----

df1_Gini <- df_Gini %>% filter(exemption == "None") %>% select(-target, -Ref) %>%  
  pivot_wider(values_from = "Gini_exp", names_from = "policy") %>% 
  mutate(variable = "Gini change rate (median)") %>% 
  mutate(`1.5C CP` = (`1.5C CP` - `Baseline`), `1.5C non-CP` = (`1.5C non-CP` - `Baseline`)) %>% select(-Baseline, -`Baseline non-CP`)



### data integration ----
colnames(df1_Gini)
colnames(df1_hug)
colnames(df1_PoV)
colnames(df1_PoV_55)

df <- df1_PoV %>% rbind(df1_PoV_55) %>% rbind(df1_hug) %>% rbind(df1_Gini)






### Integration ----

F_df_map <- function(x){  
  y <- x %>% filter(value != "NaN") %>% 
  mutate(variable = case_when(variable == "Price change rate (median)_Energy" ~ "Energy price increase",
                              variable == "Price change rate (median)_Food" ~ "Food price increase",
                              variable == "GDP loss" ~ "GDP loss",
                              variable ==  "Consumption loss" ~ "Consumption loss",
                              variable == "Poverty headcount change (Int.)" ~ "Poverty",
                              variable == "Poverty headcount change (UMIC)" ~ "Poverty",
                              variable == "Hunger risk change" ~ "Hunger risk",
                              variable == "Gini change rate (median)" ~ "Income inequality"))
return(y)
}


# 1. mapping --------------------------------------------------------------
map_R_22 <- data.frame(R = c(lis_Rphi, "GUF"), R_CGE = c(lis_Rphi, "FRA")) %>% distinct() 






# 2. plot: absolute -------------------------------------------------------

unique(df$variable)
lis_var <- c("income inequality", "risk of hunger", "poverty (Int.)", "poverty (UMIC)")
for(v in 1: length(lis_var)){
  if(lis_var[v] == "income inequality"){
    txt_var = "Gini change rate (median)"
    txt_leg = "Gini \n percentage point"
  }else if(lis_var[v] == "risk of hunger"){
    txt_var = "Hunger risk change"
    txt_leg = "Hunger risk \n million"
  }else if(lis_var[v] == "poverty (Int.)"){
    txt_var = "Poverty headcount change (Int.)"
    txt_leg = "Poverty headcount \n million"
  }else if(lis_var[v] == "poverty (UMIC)"){
    txt_var = "Poverty headcount change (UMIC)"
    txt_leg = "Poverty headcount \n million"
  }
  
  pdata0 <- df%>% filter(variable == txt_var,SSP == "SSP2", R != "World", 
                                         Y %in% c("2030", "2050"), Gini == "consistent", tech == "None")  %>% 
    mutate(value = (`1.5C CP` - `1.5C non-CP`)) %>% 
    F_df_map() %>% 
    mutate(scenario = "1.5C", value_plot = value, model = "AIMHub") 
  
  
  
  p <- F_plot_globalMap(data=pdata0, lis_plot_y=c("2030", "2050"), lis_plot_scenario = c("1.5C"), by = c("R"),
                        legendName = txt_leg, df_map = map_R_22) +
    theme(strip.text = element_text(size = 10), legend.position = "bottom", legend.key.width = unit(1, 'cm')) +
    labs(title = paste0("Benefits of non-carbon pricing on ",lis_var[v])) +
    scale_fill_gradientn(values = c(0, .2, .4, .6, .8, 1),colours = c("white",  "#ffe109", "#f19809" ,"#8ece6d", "#30b0e0"))
                         # colours = c("#30b0e0", "#8ece6d","#ffe691", "#f19809" ,"#a81129"))
  
  ggsave(p,filename = paste0("2_2_",lis_var[v],".pdf"), path = paste0(dir_plot_main, "/2_2_Map/"),
         width = 30, height = 12, units = "cm")
  
}





# 3. plot: benefit ratio ---------------------------------------------------

# v <- 2
lis_var <- c("income inequality", "risk of hunger", "poverty (Int.)", "poverty (UMIC)")
for(v in 1: length(lis_var)){
  if(lis_var[v] == "income inequality"){
    txt_var = "Gini change rate (median)"
    txt_leg = "Percentage"
  }else if(lis_var[v] == "risk of hunger"){
    txt_var = "Hunger risk change"
    txt_leg = "Percentage"
  }else if(lis_var[v] == "poverty (Int.)"){
    txt_var = "Poverty headcount change (Int.)"
    txt_leg = "Percentage"
  }else if(lis_var[v] == "poverty (UMIC)"){
    txt_var = "Poverty headcount change (UMIC)"
    txt_leg = "Percentage"
  }
  rm(pdata0)
  pdata0 <- df%>% filter(variable == txt_var,SSP == "SSP2", R != "World", 
                         Y %in% c("2030", "2050"), Gini == "consistent", tech == "None")  %>% 
    mutate(value = (`1.5C CP` - `1.5C non-CP`)/`1.5C CP`*100) %>% 
    mutate(value = case_when(abs(value) < 200 ~ value, abs(value) > 200 ~ 0)) %>% 
    F_df_map() %>% 
    mutate(scenario = "1.5C", value_plot = value, model = "AIMHub") 
  
  
  
  p <- F_plot_globalMap(data=pdata0, lis_plot_y=c("2030", "2050"), lis_plot_scenario = c("1.5C"), by = c("R"),
                        legendName = txt_leg, df_map = map_R_22) +
    theme(strip.text = element_text(size = 10), legend.position = "bottom", legend.key.width = unit(1, 'cm')) +
    labs(title = paste0("Benefits of non-carbon pricing on ",lis_var[v])) +
    scale_fill_gradientn(values = c(0, .2, .4, .6, .8, 1),
                         colours = c("white",  "#ffe109", "#f19809" ,"#8ece6d", "#30b0e0"))
  
  ggsave(p,filename = paste0("2_2_ratio_",lis_var[v],".pdf"), path = paste0(dir_plot_main, "/2_2_Map/"),
         width = 30, height = 12, units = "cm")
  
}




