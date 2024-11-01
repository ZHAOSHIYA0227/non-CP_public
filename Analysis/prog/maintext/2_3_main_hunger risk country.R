# This Rscript produces the box and violin plot for country level hunger risk 
# Shiya ZHAO, 2024.06.25


# 1. load data ------------------------------------------------------------

# country level population at risk of hunger output | unit = million 
df_hunger_r <- paste0("../../DataArchive/hungertool/240627/AllModel_Country_AIMHub.gdx") %>%
  rgdx.param("Risk") %>% gdata::rename.vars(colnames(.), c("model", "U", "SCENARIO", "R_code","Y", "value")) %>% 
  left_join(MapScenario_hunger) %>% left_join(map_Rfao2iso3) %>% select(-R_code) %>% 
  F_woc() %>% left_join(map_R17) %>% left_join(map_R17to5) %>% select(-SCENARIO, -R_full, -R_CGE) %>% mutate(unit = "Million people") %>% 
  filter(U == "model_based")


# population | unit = million
df_pop <- rgdx.param("../../DataArchive/PHIoutput/gdx/Inputdata.gdx", "Population") %>% 
  filter(Ref %in% c("SSP2", "SSP1", "SSP3")) %>% mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R), SSP = Ref) %>% select(-Ref) %>% 
  mutate(Population = Population/1000000, unit = "Million people")


# 2. plot ------------------------------------------------------------------
## 2.1 proportion of population at risk of hunger ------------------------------------------------------------------
pdata <- df_hunger_r %>% filter(policy %in% lis_cp, Gini == "consistent", exemption == "None", Y %in% c(2030),!startsWith(policy, "2C")) %>% 
  left_join(df_pop) %>% mutate(rate = value/Population*100)

p <- ggplot(pdata, aes(x = policy, y = rate, fill = policy)) +
  geom_hline(yintercept = 0, color = "grey", alpha = .6) +
  gghalves::geom_half_violin(width = .7,position = position_nudge(x = -.2, y = 0)) + labs(x = "Year", y = "Percentage") +
  geom_boxplot(width = .2, outlier.shape = NA) + MyTheme + Theme_tran + theme(legend.position = "none",
                                                                              axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) + 
  facet_wrap(~factor(R5, levels = c(lis_R)), ncol = 3) +
  scale_fill_manual(values = palette_color_cp) 

ggsave(p,filename = "2_3_risk of hunger_percentage_abs.pdf", path = dir_plot_main,
       width = 18, height = 10, units = "cm")
rm(p)




# legend
p <- ggplot(pdata, aes(x = Y, y = rate*100, fill = policy)) +
  geom_hline(yintercept = 0, color = "grey", alpha = .6) +
  gghalves::geom_half_violin(width = .7, position = position_nudge(x = -.2, y = 0)) + labs(x = "Year", y = "Percentage") +
  geom_boxplot(width = .2, outlier.shape = NA) + MyTheme + Theme_tran + theme(axis.text.x = element_text(angle = 60)) + 
  facet_wrap(~factor(R5, levels = c(lis_R)), ncol = 3) +
  scale_fill_manual(values = palette_color_cp) +
  guides(fill = guide_legend(title = "Policy"))
p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
ggsave(p_leg,filename = "2_3_risk of hunger_percentage_legend.pdf",path = dir_plot_main,
       width = 3, height = 2.7, units = "cm")
rm(p,p_leg)



## 2.2 change in the proportion of population at risk of hunger ------------------------------------------------------------------
pdata <- df_hunger_r %>% filter(policy %in% lis_cp, Gini == "consistent", exemption == "None", Y %in% c(2030, 2050)) %>% 
  left_join(df_pop) %>% mutate(rate = value/Population) %>% select(-Population, -value) %>% dplyr::rename("value" = "rate") %>% 
  F_cha_decom() %>% mutate(change = (value-Baseline_None)*100) 


p <- ggplot(pdata, aes(x = Y, y = change, fill = policy), linewidth = .02) +
  geom_hline(yintercept = 0, color = "grey", alpha = .6) +
  gghalves::geom_half_violin(data = pdata %>% filter(policy == "1.5C CP"), position = position_nudge(x = -.2, y = 0), width = .6, side = "l") + 
  gghalves::geom_half_violin(data = pdata %>% filter(policy != "1.5C CP"), position = position_nudge(x = .2, y = 0), width = .6, side = "r") + 
  geom_boxplot(width = .2, outlier.size = .5, outlier.alpha = .3) + 
  labs(x = "Year", y = "Percentage point") +
  MyTheme + Theme_tran + theme(#lege nd.position = "none",
                               axis.text.x = element_text(angle = 60)) + 
  facet_wrap(~R5, nrow = 2) +
  scale_fill_manual(values = palette_color_cp) +
  guides(fill = guide_legend(title = "Policy"))
p
ggsave(p,filename = "2_3_change_risk of hunger_percentage.pdf", path = dir_plot_main,
       width = 15, height = 10, units = "cm")
 rm(p)


rm(df_hunger_r, df_pop)


print("The END of 2_3_main_hunger risk country.R")