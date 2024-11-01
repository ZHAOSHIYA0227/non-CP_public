# The Rscript for the first plot in the non-CP/CP paper
# Shiya ZHAO, 2024.03.29


# 1. Energy ------------------------------------------------------------
# map 
# The number of: 
# "Fin_Ene_Bui_Res"="Fin_Ene_Res"
# "Fin_Ene_Bui_Res_and_Com"="Fin_Ene_Res_and_Com"
# "Fin_Ene_Bui_Com"="Fin_Ene_Com"
map_FinEne_Sector <- openxlsx::read.xlsx(paste0(dir_def,"/IAMC_VEMF.xlsx"), sheet = "map_finalenergy_sector")



# theme
Th1 <- theme(#legend.position = "none", 
             text = element_text(size = 9),
             axis.text.x = element_text(angle = 60))




## 1.0. load data ------------------------------------------------------------
df_iamc <- cge_var %>%
  rgdx.param("IAMC_Template") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO) %>% 
  gdata::rename.vars(colnames(.), c("R", "VEMF", "Y", "value", "Ref"))


# Primary energy profile
df_prm <- df_iamc %>% left_join(map_Prm) %>% filter(!is.na(com)) %>% F_woc() %>% filter(policy != "Baseline non-CP") %>% select(-VEMF) %>% mutate(com = factor(com, levels = lis_PrmEne_type))

# Secondary energy profile
df_sec <- df_iamc %>% left_join(map_Sec) %>% filter(!is.na(com)) %>% F_woc() %>% filter(policy != "Baseline non-CP") %>% select(-VEMF) %>% mutate(com = factor(com, levels = lis_SecEne_type))

# Power generation profile
df_pow <- df_iamc %>% left_join(map_Pow) %>% filter(!is.na(com)) %>% F_woc() %>% filter(policy != "Baseline non-CP") %>% select(-VEMF) %>% mutate(com = factor(com, levels = lis_PowEne_type))

# Final energy profile
df_fin <- df_iamc %>% left_join(map_Fin) %>% filter(!is.na(com)) %>% F_woc() %>% filter(policy != "Baseline non-CP") %>% select(-VEMF) %>% mutate(com = factor(com, levels = lis_FinEne_type))

# Final energy profile in the residential sector
df_finres <- df_iamc %>% left_join(map_FinRes) %>% filter(!is.na(com)) %>% F_woc() %>% filter(policy != "Baseline non-CP") %>% select(-VEMF) %>% mutate(com = factor(com, levels = lis_FinEneRes_type))

# Final energy demand by sector
df_fin_sector <- df_iamc %>% left_join(map_FinEne_Sector) %>% filter(!is.na(Variable)) %>% F_woc() %>% filter(policy != "Baseline non-CP") %>% select(-VEMF) %>% 
  dplyr::group_by(R, Y, Ref, Variable_type, policy, SSP, exemption, target, tech, Gini) %>% dplyr::reframe(value = sum(value))



# 1.1. plot by fuel types -----------------------------------------------

## 1.1.1 Primary energy supply -----------------------------------------------
pdata <- df_prm %>% F_filter1() %>% filter(Gini == "consistent", !startsWith(policy, "2C"), tech == "None") %>% 
  pivot_wider(names_from = "com", values_from = "value") %>% pivot_longer(cols = any_of(lis_PrmEne_type), names_to = "com", values_to = "value") %>% mutate(com = factor(com, levels = lis_PrmEne_type) %>% fct_rev())
pdata[is.na(pdata)] <- 0
p <- F_plot_ene(pdata,txt_title = "") + Th1 + theme(legend.position = "none") + theme(axis.text.x = element_text(hjust = 1, vjust = 1)) +
  geom_rect(aes(xmin=2005, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4)
p
ggsave(p,filename = paste0("1_SSP2_non-CP_Primary energy.pdf"), path = dir_plot_main,
       width = 5, height = 7, units = "cm")

rm(p, pdata)


## 1.1.2 Power generation energy -----------------------------------------------
pdata <- df_pow %>% F_filter1() %>% filter(Gini == "consistent", !startsWith(policy, "2C"), tech == "None") %>% 
  pivot_wider(names_from = "com", values_from = "value") %>% pivot_longer(cols = any_of(lis_PowEne_type), names_to = "com", values_to = "value") %>% mutate(com = factor(com, levels = lis_PowEne_type) %>% fct_rev())
pdata[is.na(pdata)] <- 0
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_Prm) + Th1 + theme(legend.position = "none") + theme(axis.text.x = element_text(hjust = 1, vjust = 1)) +
  geom_rect(aes(xmin=2005, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4)
p
ggsave(p,filename = paste0("1_SSP2_non-CP_Power sector.pdf"), path = dir_plot_main,
       width = 5, height = 7,units = "cm")


# legend on the right side
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_Prm) + Th1 + 
  geom_rect(aes(xmin=2005, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4)
p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
p_leg
ggsave(p_leg,filename = paste0("1_SSP2_non-CP_PrmPow_legend_right.pdf"), path = dir_plot_main,
       width = 5, height = 9, units = "cm")
rm(p_leg, p)

# legend at the bottom
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_Prm) + Th1 + 
  geom_rect(aes(xmin=2005, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4) + theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Energy carrier", nrow = 3))
p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
p_leg
ggsave(p_leg,filename = paste0("1_SSP2_non-CP_PrmPow_legend_bottom.pdf"), path = dir_plot_main,
       width = 20, height = 3, units = "cm")
rm(p_leg, p, pdata)




## 1.1.3 Final energy -----------------------------------------------
pdata <- df_fin %>% F_filter1() %>% filter(Gini == "consistent", !startsWith(policy, "2C"), tech == "None") %>% 
  pivot_wider(names_from = "com", values_from = "value") %>% pivot_longer(cols = any_of(lis_FinEne_type), names_to = "com", values_to = "value") %>% mutate(com = factor(com, levels = lis_FinEne_type) %>% fct_rev())
pdata[is.na(pdata)] <- 0
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_Fin) + Th1 + theme(axis.text.x = element_text(hjust = 1, vjust = 1), legend.position = "none") +
  geom_rect(aes(xmin=2005, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4)
ggsave(p,filename = paste0("1_SSP2_non-CP_Final energy.pdf"), path = dir_plot_main,
       width = 5, height = 7,units = "cm")



# legend on the right hand side
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_Fin) + Th1 + theme(axis.text.x = element_text(hjust = 1, vjust = 1)) +
  geom_rect(aes(xmin=2005, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4)

p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
p_leg
ggsave(p_leg,filename = paste0("1_SSP2_non-CP_Final energy_legend_right.pdf"), path = dir_plot_main,
       width = 4, height = 6, units = "cm")
rm(p, p_leg)


# legend at the bottom
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_Fin) + Th1 + theme(axis.text.x = element_text(hjust = 1, vjust = 1)) +
  geom_rect(aes(xmin=2005, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4) + theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Energy carrier", nrow = 2))

p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
p_leg
ggsave(p_leg,filename = paste0("1_SSP2_non-CP_Final energy_legend_bottom.pdf"), path = dir_plot_main,
       width = 11, height = 2,  units = "cm")
rm(p, pdata, p_leg)





# 1.2 plot Final energy by sectors -----------------------------------------------
pdata <- df_fin_sector %>% F_filter1() %>% mutate(com = factor(Variable_type, level = c("AFOLU", "Building", "Transport", "Industry", "Others")) %>% fct_rev()) 

p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_sector) + Th1 + theme(axis.text.x = element_text(hjust = 1, vjust = 1), legend.position = "none") +
  geom_rect(aes(xmin=2005, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4)
p
ggsave(p,filename = paste0("1_SSP2_non-CP_Final energy_sector.pdf"), path = dir_plot_main,
       width = 5, height = 7,units = "cm")


# legend on the right hand side
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_sector) + Th1 + theme(axis.text.x = element_text(hjust = 1, vjust = 1)) +
  geom_rect(aes(xmin=2005, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4)
p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()+
  guides(fill = guide_legend(title = "Sector", nrow = 2))
p_leg
ggsave(p_leg,filename = paste0("1_SSP2_non-CP_Final energy_sector_legend_right.pdf"), path = dir_plot_main,
       width = 2.5, height = 3.5, units = "cm")
rm(pdata,p_leg)

# legend at the bottom
p_leg <- ggpubr::get_legend(p+ theme(legend.position = "bottom") +
                              guides(fill = guide_legend(title = "Sector", nrow = 2)), position = NULL) %>% ggpubr::as_ggplot() 
p_leg
ggsave(p_leg,filename = paste0("1_SSP2_non-CP_Final energy_sector_legend_bottom.pdf"), path = dir_plot_main,
       width = 8, height = 2, units = "cm")
rm(p, pdata, p_leg)





rm(df_fin, df_pow, df_prm, df_finres, df_fin_sector, df_sec)






# 2. Emissions by sectors -----------------------------------------------

map_GHG <- openxlsx::read.xlsx(paste0(dir_def,"/IAMC_VEMF.xlsx"), sheet = "map_GHGsector")


# Data preparations
df_GHG_sector <- df_iamc %>% left_join(map_GHG) %>% filter(!is.na(Variable)) %>% F_woc() %>% filter(policy != "Baseline non-CP") %>% select(-VEMF) 

pdata <- df_GHG_sector %>% F_filter1() %>% # filter(Y != "2005") %>%
  mutate(com = factor(Variable, level = c("AFOLU", "Energy demand", "Energy supply", "Industrial Process", "Product use", "Waste", "Others")) %>% fct_rev()) %>% 
  mutate(value = value/1000)

palette_GHG_sector <- c("AFOLU" = "#073b4c", 
                        "Energy demand" = "#abc5d4", 
                        "Energy supply"= "#e9d8a6", 
                        "Industrial Process" = "#ffd166", 
                        "Product use"= "#909342", 
                        "Waste" = "#b0bba0", 
                        "Others" = "#ef476f")

p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_GHG_sector) + Th1 + labs(y = expression(paste("Gt",CO[2],"/year"))) + theme(legend.position = "none") + theme(axis.text.x = element_text(hjust = 1, vjust = 1)) +
  geom_rect(aes(xmin=2005, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4)
p

ggsave(p,filename = paste0("1_SSP2_non-CP_GHG_sector.pdf"), path = dir_plot_main,
       width = 5, height = 7,units = "cm")


# legend on the right hand side
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_GHG_sector) + Th1 + labs(y = expression(paste("Gt",CO[2],"/year"))) + 
  geom_rect(aes(xmin=2004, xmax=2020, ymin=0, ymax=Inf), fill = "white", alpha = 0.4)
p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
ggsave(p_leg,filename = paste0("1_SSP2_non-CP_GHG_sector_legend_right.pdf"), path = dir_plot_main,
       width = 3.8, height = 4.3, units = "cm")
rm(p_leg, p)


# legend at the bottom
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_GHG_sector) + Th1 + labs(y = expression(paste("Gt",CO[2],"/year"))) + 
  geom_rect(aes(xmin=2004, xmax=2020, ymin=0, ymax=Inf), fill = "white", alpha = 0.4) + theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Sector", nrow = 3))
p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
ggsave(p_leg,filename = paste0("1_SSP2_non-CP_GHG_sector_legend_bottom.pdf"), path = dir_plot_main,
       width = 10, height = 3, units = "cm")

rm(p_leg, pdata, p)


rm(df_GHG_sector)









# 3. land use change by sectors -----------------------------------------------
map_VEMF_lancov <- openxlsx::read.xlsx(paste0(dir_def,"/IAMC_VEMF.xlsx"), sheet = "map_landcover")
df_lancov <- df_iamc %>% left_join(map_VEMF_lancov) %>% select(-c("VEMF", "VIAMC")) %>% filter(!is.na(Variable)) %>% dplyr::rename(R_CGE = "R") %>% F_R17full() %>%  dplyr::rename(R = "R_CGE") %>% F_woc()

pdata <- df_lancov %>% F_filter1() %>% dplyr::group_by(R, Y, Ref, target, policy, SSP, exemption, Variable_type) %>% dplyr::reframe(value = sum(value)) %>% 
  mutate(com = fct_rev(factor(Variable_type, levels = c("Forests", "Cropland", "Pasture", "Build-up", "Water Ecosystem", "Other land"))))

palette_landcover <- c("Forests" = "#43aa8b",
                       "Cropland" = "#90be6d",
                       "Pasture" = "#f9c74f",
                       "Build-up" = "#c6d2c6",
                       "Water Ecosystem" = "#9adaff",
                       "Other land" = "#577590")
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_landcover) + Th1 + theme(axis.text.x = element_text(hjust = 1, vjust = 1)) + theme(legend.position = "none") + 
  labs(y = "million ha/year") + 
  geom_rect(aes(xmin=2005, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4) +
  ylim(0, 20000)
p

ggsave(p,filename = paste0("1_SSP2_non-CP_LandCover.pdf"), path = dir_plot_main,
       width = 5.4, height = 7,units = "cm")


# legend on the right hand side
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_landcover) + Th1 + theme(axis.text.x = element_text(hjust = 1, vjust = 1)) + 
  labs(y = "million ha/year") + 
  geom_rect(aes(xmin=2004, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4) +
  ylim(0, 20000)

p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
ggsave(p_leg,filename = paste0("1_SSP2_non-CP_LandCover_legend_right.pdf"), path = dir_plot_main,
       width = 2.5, height = 3, units = "cm")



# legend at the bottom
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_landcover) + Th1 + theme(axis.text.x = element_text(hjust = 1, vjust = 1)) + 
  labs(y = "million ha/year") + 
  geom_rect(aes(xmin=2004, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = 0.4) +
  ylim(0, 20000) + theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Land use", nrow = 2))

p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
ggsave(p_leg,filename = paste0("1_SSP2_non-CP_LandCover_legend_bottom.pdf"), path = dir_plot_main,
       width = 8, height = 2, units = "cm")

rm(df_lancov, pdata, p)
