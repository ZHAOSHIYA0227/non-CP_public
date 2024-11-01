# The Rscript for the scattering plot with GDP
# Shiya ZHAO, 2024.05.07



# load data --------------------------------------------------------------------
# df_iamc <- cge_var %>%
#   rgdx.param("IAMC_Template") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO) %>% 
#   gdata::rename.vars(colnames(.), c("R", "VEMF", "Y", "value", "Ref"))


# GDP ----
# df_gdpcap <- df_iamc %>% filter(VEMF == "GDP_per_cap") %>% left_join(MapScenario) %>% F_woc() %>%  
#   transform(policy = factor(policy, levels = lis_cp)) %>% filter(policy != "Baseline non-CP", !is.na(policy)) %>% 
#   F_filter(lis_R= lis_R) %>% select(-VEMF, -SCENARIO) %>% mutate(unit_gdp = "$") %>% dplyr::rename(GDPcap = "value")

df_gdpcap <- "../../DataArchive/PHIoutput/gdx/Inputdata.gdx" %>% rgdx.param("GDPCap") %>% 
  F_woc() %>% mutate(unit_gdp = "$") %>% filter(!is.na(policy), Y %in% lis_Y)



## Population ----
df_pop <- rgdx.param("../../DataArchive/PHIoutput/gdx/Inputdata.gdx", "Population") %>% 
  filter(Ref %in% c("SSP2", "SSP1", "SSP3")) %>% mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R), SSP = Ref) %>% select(-Ref) %>% 
  mutate(Population = Population/1000000, unit_pop = "Million people")




## Poverty ----
df_PoV <- rgdx.param(AnaExp, "PoVExp") %>%
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  filter(!is.na(Ref), R %in% lis_Rphi, Y%in% lis_Y) %>%  
  mutate(TH1 = recode(TH, 'pop_1.9' = lis_TH[1], 'pop_3.2' = lis_TH[2], 'pop_5.5' = lis_TH[3]),
         R = factor(R, levels = lis_Rphi), unit_pov = "Million", PoVExp = PoVExp/1000000) %>% 
  F_filter(s_Baseline_woc = F, lis_R = lis_Rphi) %>% F_woc() %>% left_join(df_pop) %>% 
  mutate(`Poverty rate` = PoVExp/Population * 100) %>% select(-TH)




## Hunger risk ----
df_hunger_r <- paste0("../../DataArchive/hungertool/240627/AllModel_Country_AIMHub.gdx") %>%
  rgdx.param("Risk") %>% gdata::rename.vars(colnames(.), c("model", "U", "SCENARIO", "R_code","Y", "hug")) %>% 
  left_join(MapScenario_hunger) %>% left_join(map_Rfao2iso3) %>% select(-R_code) %>% 
  F_woc() %>% #left_join(map_R17) %>% left_join(map_R17to5) %>% 
  mutate(unit_hug = "Million people") %>% 
  filter(U == "model_based") %>% select(-SCENARIO, -Ref, -U) %>% left_join(df_pop) %>% 
  mutate(`Risk of hunger` = hug/Population * 100)





## Gini ----
df_Gini <- rgdx.param(AnaExp, "Gini_exp") %>%
  F_filter(lis_R=unique(.$R)) %>% F_woc() %>% mutate(unit_gini = "percentage point", `Gini coefficient` = `Gini_exp`*100)






# data integration --------------------------------------------------------
colnames(df_gdpcap)
colnames(df_PoV)

colnames(df_hunger_r)
colnames(df_Gini)

df_ind <- df_PoV %>% filter(TH1 == "1.9-threshold") %>% 
  left_join(df_hunger_r) %>% 
  left_join(df_Gini) %>% select(-colnames(.)[grepl("unit", colnames(.))], -Population, -hug, -PoVExp, -Gini_exp) %>% 
  pivot_longer(cols = c("Poverty rate", "Risk of hunger", "Gini coefficient"), values_to = "value", names_to = "variable") %>% F_cha_decom() %>% 
  mutate(change = value-Baseline_None)

df <- df_gdpcap %>% select(-colnames(.)[grepl("unit", colnames(.))]) %>% filter(Y == "2020", policy == "Baseline") %>% 
  select(-Y, -policy, -target, -Ref) %>% 
  right_join(df_ind)




# plot: both CP and non-CP --------------------------------------------------------------------

pdata <- df %>% filter(Y == "2030", !grepl("2C", policy)) %>% F_filter_main()

p <- ggplot(pdata) +
  geom_point(aes(x = GDPCap, y = change)) +
  facet_wrap(policy~variable, scales = "free") + MyTheme
p



# plot: difference between CP and non-CP --------------------------------------------------------------------

pdata <- df %>% filter(Y == "2030", !grepl("2C", policy)) %>% F_filter_main() %>% select(-TH1, -Baseline_None, -`Baseline non-CP_None`, -value) %>% 
  pivot_wider(names_from = policy, values_from = change) %>% mutate(change = `1.5C CP` - `1.5C non-CP`) %>% 
  left_join(map_R) %>% filter(R_CGE != "World")

p <- ggplot(pdata, mapping = aes(x = GDPCap/1000, y = change)) +
  geom_point(aes(color = R_CGE), size = .8) +
  geom_smooth(formula = y ~ I(1/x)) + labs(x = "GDP (thousand USD)", y = "Percentage point") +
  scale_color_manual(values = palette_color_R5) + guides(color = guide_legend(title = "Region")) +
  facet_wrap(~variable, scales = "free") + MyTheme
p

dir.create(paste0(dir_fig, "/12_scatter/"))
ggsave(p, filename = paste0(dir_fig, "/12_scatter/2030_rate.pdf"), height = 6, width = 18, units = "cm")
rm(pdata, p)





# plot: relationship between hunger risk and poverty rate --------------------------------------------------------------------

pdata <- df %>% filter(Y %in% c("2030", "2050"), !grepl("2C", policy)) %>% F_filter_main() %>% select(-TH1, -Baseline_None, -`Baseline non-CP_None`, -value) %>% 
  pivot_wider(names_from = policy, values_from = change) %>% mutate(change = `1.5C CP` - `1.5C non-CP`) %>% 
  select(-`1.5C CP`, -`1.5C non-CP`) %>% pivot_wider(names_from = "variable", values_from = "change")%>% 
  left_join(map_R) %>% filter(R_CGE != "World")
colnames(pdata)
p <- ggplot(pdata, mapping = aes(x = `Poverty rate`, y = `Risk of hunger`)) +
  geom_point(aes(color = R_CGE), size = .8) +
  geom_smooth(formula = y ~ x) +
  labs(x = "Poverty rate", y = "Risk of hunger") +
  scale_color_manual(values = palette_color_R5) + guides(color = guide_legend(title = "Region")) +
  facet_wrap(~Y, scales = "free") + 
  MyTheme
p

dir.create(paste0(dir_fig, "/12_scatter/"))
ggsave(p, filename = paste0(dir_fig, "/12_scatter/pov_hug.pdf"), height = 6, width = 15, units = "cm")
  rm(pdata, p)
