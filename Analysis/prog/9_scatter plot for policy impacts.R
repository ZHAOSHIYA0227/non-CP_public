# The Rscript for the third plot in the non-CP/CP paper
# Shiya ZHAO, 2024.05.10

# Scatter plots, x: GDP per capita, y: policy impacts on poverty/Gini/hunger risk



# Function --------------------------------------------------------------------
F_p_scat <- function(pdata, x = GDPCap, y = change, lab_y = "Percentage change in poverty headcount (%)", lab_x = "GDP per capita"){
  p <- ggplot(pdata) +
    geom_point(aes(x = GDPCap, y = change, color = policy)) + 
    geom_hline(yintercept = 0, color = "grey70") +
    MyTheme + Theme_tran +
    scale_color_manual(values = palette_color_cp) +
    guides(color = guide_legend(title = "Policy")) + 
    labs(x = lab_x, y = lab_y)
  return(p)
} 


lis_Y_fil <- c(2030, 2050, 2070)


# 1. Plot poverty and gini--------------------------------------------------------------------
## 1.1. data -----------------------------------------------------------------

### GDP per capita ----

df_GDPcap <- paste0("../../DataArchive/PHIoutput/gdx/Inputdata.gdx") %>%
  rgdx.param("GDPcap") %>%
  left_join(MapScenario) %>% filter(!is.na(SCENARIO)) %>% 
  F_woc() %>% F_filter(s_Baseline_woc = F, lis_R = lis_Rphi) %>% 
  transform(policy = factor(policy, levels = lis_cp)) %>%
  select(-c("SCENARIO")) %>% mutate(unit = "thousand $(2010) per capita", GDPCap = GDPCap/1000)



### Poverty headcount change ----
df_PoV <- rgdx.param(AnaExp, "PoVExp") %>%
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  filter(!is.na(Ref), 
         Y%in% lis_Y) %>%
  rename.vars(from = 'PoVExp', to = 'value') %>%
  mutate(TH1 = recode(TH, 
                      'pop_1.9' = lis_TH[1],
                      'pop_3.2' = lis_TH[2],
                      'pop_5.5' = lis_TH[3]),
         variable = "Poverty headcount",
         unit = "person") %>%
  select(-"TH") %>%
  F_filter(s_Baseline_woc = F, lis_R = lis_Rphi) %>% F_woc()


### Poverty rate change ----
df_RoP <- rgdx.param(AnaExp, "RoPExp") %>%
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  filter(!is.na(Ref), 
         Y%in% lis_Y) %>%
  rename.vars(from = 'RoPExp', to = 'value') %>%
  mutate(TH1 = recode(TH, 
                      'pop_1.9' = lis_TH[1],
                      'pop_3.2' = lis_TH[2],
                      'pop_5.5' = lis_TH[3]),
         variable = "Poverty rate", value = value * 100, 
         unit = "%") %>%
  select(-"TH") %>%
  F_filter(s_Baseline_woc = F, lis_R = lis_Rphi) %>% F_woc()







### Gini change ----
df_Gini <- rgdx.param(AnaExp, "Gini_exp") %>%
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  filter(!is.na(Ref), 
         Y%in% lis_Y) %>%
  rename.vars(from = 'Gini_exp', to = 'value') %>%
  mutate(TH1 = "Gini",
         variable = "Gini",
         unit = "-") %>%
  F_filter(s_Baseline_woc = F, lis_R = lis_Rphi) %>% F_woc()




### Data integration ----
df <- df_RoP %>% rbind(df_PoV) %>% rbind(df_Gini) %>% F_cha_decom() %>% 
  left_join(df_GDPcap %>% filter(R %in% lis_Rphi) %>% select(-unit, -exemption, -Ref))

df1 <- df %>% mutate(change = (value-Baseline_None)/Baseline_None * 100) %>% left_join(map_R) %>% dplyr::rename(R_agg = R_CGE) 



## 1.2 plot poverty headcount ----
pdata <- df1 %>% filter(variable == "Poverty headcount", exemption == "None") %>% 
  filter(R_agg != "World", Baseline_None > 10000, Y %in% lis_Y_fil)

p <- F_p_scat(pdata) +facet_wrap(~Y)
p
ggsave(p, filename = paste0(dir_fig, "Figure9_point_poverty headcount.pdf"), units = "cm", width = 18, height = 8)

rm(p, pdata)


## 1.3 plot poverty rate ----
pdata <- df1 %>% filter(variable == "Poverty rate", exemption == "None") %>% 
  filter(R_agg != "World", abs(Baseline_None) > .01, Y %in% lis_Y_fil)

p <- F_p_scat(pdata, lab_y = "Percentage change in poverty rate (%)") +facet_wrap(~Y)
p
ggsave(p, filename = paste0(dir_fig, "Figure9_point_poverty rate.pdf"), units = "cm", width = 18, height = 8)

rm(p, pdata)


## 1.4 plot Gini ----
pdata <- df1 %>% filter(variable == "Gini", exemption == "None") %>% 
  filter(R_agg != "World", abs(change) > .0001, Y %in% lis_Y_fil)

p <- F_p_scat(pdata, lab_y = "Percentage change in Gini (%)") +facet_wrap(~Y)
p
ggsave(p, filename = paste0(dir_fig, "Figure9_point_Gini.pdf"), units = "cm", width = 18, height = 8)



rm(p, pdata, df, df1)




# 2. hunger risk ----------------------------------------------------------

## 2.1 Data ----------------------------------------------------------------
# GDP per capita in R17
df_GDPcap17 <- paste0("../../DataArchive/cgeoutput/240627/analysis.gdx") %>%
  rgdx.param("GDP_cap") %>% dplyr::rename(GDPCap = GDP_cap) %>% 
  left_join(MapScenario) %>% filter(!is.na(SCENARIO)) %>% 
  F_woc() %>% F_filter(s_Baseline_woc = F, lis_R = lis_R17) %>% 
  transform(policy = factor(policy, levels = lis_cp)) %>%
  select(-c("SCENARIO")) %>% mutate(unit = "thousand $(2010) per capita")





# Hunger risk change
df_iamc <- cge_var %>%
  rgdx.param("IAMC_Template") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO) %>% 
  gdata::rename.vars(colnames(.), c("R", "VEMF", "Y", "value", "Ref"))

df_hug <- df_iamc %>% filter(VEMF == "Pop_Ris_of_Hun") %>% mutate(unit = "million", variable = "Population at hunger risk") %>% 
  left_join(MapScenario) %>% F_woc() %>% F_filter(lis_R = lis_R17) %>% select(-SCENARIO, -VEMF)


## Data integration ----
df <- df_hug %>% F_cha_decom() %>% 
  left_join(df_GDPcap17 %>% select(-unit, -exemption, -Ref))

df1 <- df %>% mutate(change = (value-Baseline_None)/Baseline_None * 100) %>% dplyr::rename(R_agg = R) 



## 2.2 plot hunger risk R17 ----------------------------------------------------
pdata <- df1 %>% filter(variable == "Population at hunger risk", exemption == "None") %>% 
  filter(R_agg != "World", abs(change) > .0001, Baseline_None > .0001, Y %in% lis_Y_fil)


p <- F_p_scat(pdata, lab_y = "Percentage change in hunger risk (%)") +facet_wrap(~Y)
p
ggsave(p, filename = paste0(dir_fig, "Figure9_point_Hunger risk.pdf"), units = "cm", width = 18, height = 8)


rm(p, pdata, df1, df, df_hug, df_iamc, df_GDPcap, df_GDPcap17, df_Gini, df_PoV, df_RoP)
