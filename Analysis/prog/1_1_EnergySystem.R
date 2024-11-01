# This program shows the energy system structure 
# Shiya ZHAO, 2024.03.19


# 0. load data ------------------------------------------------------------

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



# 1. Energy system composition: area plot ------------------------------------------------------------
dir.create(paste0(dir_fig, "/Figure1_1_Energy system"))
lis_SSP <- c("SSP1", "SSP2", "SSP3")
for(s in 1:length(lis_SSP)){
  # Primary energy
  pdata <- df_prm %>% filter(R == "World", SSP == lis_SSP[s]) 
  p <- F_plot_ene(pdata) + facet_grid(~policy) 
  ggsave(p,filename = paste0(lis_SSP[s],"_Primary energy.pdf"), path = paste0(dir_fig, "/Figure1_1_Energy system"),
         width = 24, height = 10, units = "cm")
  rm(p, pdata)
  
  # Secondary energy 
  pdata <- df_sec %>% filter(R == "World", SSP == lis_SSP[s])
  p <- F_plot_ene(pdata, txt_title = "Secondary energy", pal_color = palette_Sec) + facet_grid(~policy) 
  p
  ggsave(p,filename = paste0(lis_SSP[s], "_Secondary energy.pdf"), path = paste0(dir_fig, "/Figure1_1_Energy system"),
         width = 24, height = 10, units = "cm")
  rm(p, pdata)
  
  # Power generation energy
  pdata <- df_pow %>% filter(R == "World", SSP == lis_SSP[s])
  p <- F_plot_ene(pdata, txt_title = "Power sector", pal_color = palette_Prm) + facet_grid(~policy) 
  p
  ggsave(p,filename = paste0(lis_SSP[s], "_Power sector.pdf"), path = paste0(dir_fig, "/Figure1_1_Energy system"),
         width = 24, height = 10, units = "cm")
  rm(p, pdata)
  
  # Final energy
  pdata <- df_fin %>% filter(R == "World", SSP == lis_SSP[s])
  p <- F_plot_ene(pdata, txt_title = "Final energy", pal_color = palette_Fin) + facet_grid(~policy) 
  ggsave(p,filename = paste0(lis_SSP[s], "_Final energy.pdf"), path = paste0(dir_fig, "/Figure1_1_Energy system"),
         width = 24, height = 10, units = "cm")
  rm(p, pdata)
  
  
  # Final energy in the residential sector
  pdata <- df_finres %>% filter(R == "World", SSP == lis_SSP[s])
  p <- F_plot_ene(pdata, txt_title = "Final energy in the residential sector", pal_color = palette_FinRes) + facet_grid(~policy) 
  ggsave(p,filename = paste0(lis_SSP[s], "_Final energy_Residential.pdf"), path = paste0(dir_fig, "/Figure1_1_Energy system"),
         width = 24, height = 10, units = "cm")
  rm(p, pdata)
  
}




# 2. Only SSP2 and 1.5 VA scenario for the maintext -----------------------------------------------

F_filter1 <- function(x){
  y <- x %>% filter(R == "World", SSP == "SSP2", policy == "1.5C non-CP")
  return(y)
}

Th1 <- theme(legend.position = "none", 
             text = element_text(size = 9),
             axis.text.x = element_text(angle = 60))

# Primary energy supply
pdata <- df_prm %>% F_filter1()
p <- F_plot_ene(pdata,txt_title = "") + Th1 +
  geom_rect(aes(xmin=2004, xmax=2020, ymin=0, ymax=Inf), fill = "white", alpha = 0.005)
ggsave(p,filename = paste0("SSP2_non-CP_Primary energy.pdf"), path = paste0(dir_fig, "/Figure1_1_Energy system"),
       width = 5, height = 7, units = "cm")

rm(p, pdata)


# Power generation energy
pdata <- df_pow %>% F_filter1()
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_Prm) + Th1 +
  geom_rect(aes(xmin=2004, xmax=2020, ymin=0, ymax=Inf), fill = "white", alpha = 0.005)
p
ggsave(p,filename = paste0("SSP2_non-CP_Power sector.pdf"), path = paste0(dir_fig, "/Figure1_1_Energy system"),
       width = 5, height = 7,units = "cm")
rm(p, pdata)



# Final energy
pdata <- df_fin %>% F_filter1()
p <- F_plot_ene(pdata, txt_title = "", pal_color = palette_Fin) + Th1 +
  geom_rect(aes(xmin=2004, xmax=2020, ymin=0, ymax=Inf), fill = "white", alpha = 0.005)
ggsave(p,filename = paste0("SSP2_non-CP_Final energy.pdf"), path = paste0(dir_fig, "/Figure1_1_Energy system"),
       width = 5, height = 7,units = "cm")
rm(p, pdata)






rm(F_filter1, Th1)
