# GDP decomposition for non-CP paper 

# Shiya ZHAO, 2024.05.13



# 1. Income and factor approach ----------------------------------------------------

# GDP income share | Million US 2005D per year
# the sum of primary production factors and indirect tax
# primary production factors:land (LND and natural resources: RES), labor (LAB), capital (CAP),
# indirect tax (STAX)
df_GDP_v <- cge_ana %>% rgdx.param("GDP_v") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO)


# GDP factor basis | Million US 2005D per year

df_GDP_vr2 <- cge_ana %>% rgdx.param("GDP_vr2") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO)



a <- df_GDP_v %>% filter(Y == "2005", R == "USA", Ref == "1p5C") 
b <- df_GDP_vr2 %>% filter(Y == "2005", R == "USA", Ref == "1p5C")
c <- a %>% full_join(b)





# 2. Expenditure approach -------------------------------------------------

# expenditure approach of GDP accounting
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Expenditure_approach



# GDP expenditure | Million US 2005D per year: GDP_psi
# the sum of all final goods and services purchased in an economy ??? no governmental spending ???
df_GDP_psi <- cge_ana %>% rgdx.param("GDP_psi") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO)




# GDP expenditure composition | Million US 2005D per year: GDP_s
# categorization based on: consumer spending (HURB), government spending, business investment spending (S-I??), and net exports (???IMP????)
df_GDP_s <- cge_ana %>% rgdx.param("GDP_s") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO)





# 3. Decomposition --------------------------------------------------------

F_p_GDP <- function(data){
  pdata <- data %>% F_woc() %>% F_filter(lis_R = lis_R) %>% filter(SSP == "SSP2") %>% mutate(R = factor(R, levels = lis_R))
  
  p <- ggplot(pdata) +
    geom_col(aes(x = variable, y = value/1000, fill = policy), position = "dodge") +
    facet_wrap(~ R, scales = "free", nrow = 2) + MyTheme + Theme_tran + theme(axis.text.x = element_text(angle = 60)) +
    labs(x = "Component", y = "Billion US 2005D per year") + scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Policy"))
  
  return(p)
}

dir.create(paste0(dir_fig, "/Figure10 GDP decomp/"))




## 3.1 GDP income share | Million US 2005D per year: GDP_v ---------------------------------------------------------------
a <- df_GDP_v  %>% F_woc() %>% F_filter(lis_R = lis_R) %>% dplyr::group_by(Y, R, Ref) %>% reframe(value = sum(GDP_v)) %>% filter(Y %in% c(2030))
colnames(df_GDP_v)
pdata <- df_GDP_v %>% mutate(value = GDP_v, variable = AC) %>% filter(Y %in% c(2030))
p <- F_p_GDP(pdata)
p

ggsave(p, filename = paste0(dir_fig, "/Figure10 GDP decomp/GDPIncomeShare GDP_v.pdf"), width = 18, height = 10, units = "cm")

rm(pdata, p)




## 3.2 GDP factor basis | Million US 2005D per year: GDP_vr2 ---------------------------------------------------------------
pdata <- df_GDP_vr2 %>% mutate(value = GDP_vr2, variable = AC) %>% filter(Y %in% c(2030))
p <- F_p_GDP(pdata)
p

ggsave(p, filename = paste0(dir_fig, "/Figure10 GDP decomp/GDPfactorbasis GDP_vr2.pdf"), width = 18, height = 10, units = "cm")

rm(pdata, p)





## 3.3 GDP expenditure: GDP_psi ---------------------------------------------------------------
lis_R_loop <- lis_R
for(r in 1:length(lis_R_loop)){
  pdata <- df_GDP_psi %>% filter(Y %in% c(2030), R == lis_R_loop[r]) %>% F_woc() %>% F_filter(lis_R = lis_R) %>% mutate(R = factor(R, levels = lis_R)) %>% 
    dplyr::rename(value = GDP_psi, variable = A) %>% 
    F_cha_decom() %>% mutate(change = value-Baseline_None) %>% select(-Baseline_None, -value) %>% pivot_wider(names_from = "policy", values_from = change) %>% 
    mutate(change = `1.5C non-CP` - `1.5C CP`)
  
  p <- ggplot(pdata) +
    geom_col(aes(x = variable, y = change/1000), position = "dodge") +
    facet_wrap(~ R, scales = "free", nrow = 2) + MyTheme + Theme_tran + theme(axis.text.x = element_text(angle = 60)) +
    labs(x = "Component", y = "Billion US 2005D per year") #+ scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Policy"))
  p
  
  ggsave(p, filename = paste0(dir_fig, "/Figure10 GDP decomp/Change in GDPexpenditure due to nonCP GDP_psi", lis_R_loop[r], ".pdf"), 
         width = 18, height = 8, units = "cm")
}
rm(pdata, p)


# all 2030 ----
pdata <- df_GDP_psi %>% filter(Y %in% c(2030)) %>% F_woc() %>% F_filter(lis_R = lis_R) %>% mutate(R = factor(R, levels = lis_R)) %>% 
  dplyr::rename(value = GDP_psi, variable = A) %>% 
  F_cha_decom() %>% mutate(change = value-Baseline_None) %>% select(-Baseline_None, -value) %>% pivot_wider(names_from = "policy", values_from = change) %>% 
  mutate(change = `1.5C non-CP` - `1.5C CP`)

p <- ggplot(pdata) +
  geom_col(aes(x = variable, y = change/1000, fill = variable), position = "dodge") +
  facet_wrap(~ R, scales = "free", ncol = 1) + MyTheme + Theme_tran + theme(axis.text.x = element_text(angle = 60)) +
  labs(x = "Component", y = "Billion US 2005D per year") + #scale_fill_manual(values = palette_color_cp) + 
  guides(fill = guide_legend(title = "Sector and institution", ncol = 2))
p

ggsave(p, filename = paste0(dir_fig, "/Figure10 GDP decomp/Change in GDPexpenditure due to nonCP_2030_GDP_psi.pdf"), 
       width = 24, height = 30, units = "cm")
rm(pdata, p)


# all 2050 ----
pdata <- df_GDP_psi %>% filter(Y %in% c(2050)) %>% F_woc() %>% F_filter(lis_R = lis_R) %>% mutate(R = factor(R, levels = lis_R)) %>% 
  dplyr::rename(value = GDP_psi, variable = A) %>% 
  F_cha_decom() %>% mutate(change = value-Baseline_None) %>% select(-Baseline_None, -value) %>% pivot_wider(names_from = "policy", values_from = change) %>% 
  mutate(change = `1.5C non-CP` - `1.5C CP`)

p <- ggplot(pdata) +
  geom_col(aes(x = variable, y = change/1000, fill = variable), position = "dodge") +
  facet_wrap(~ R, scales = "free", ncol = 1) + MyTheme + Theme_tran + theme(axis.text.x = element_text(angle = 60)) +
  labs(x = "Component", y = "Billion US 2005D per year") + #scale_fill_manual(values = palette_color_cp) + 
  guides(fill = guide_legend(title = "Sector and institution", ncol = 2))
p

ggsave(p, filename = paste0(dir_fig, "/Figure10 GDP decomp/Change in GDPexpenditure due to nonCP_2050_GDP_psi.pdf"), 
       width = 24, height = 30, units = "cm")
rm(pdata, p)


# selected components ----
map_A <- data.frame(A =   c("GRO",         "C_B",         "ARE",                             "OIL",        "OMN",                               "BTR",                                    "BTR3",                                                     "I_S",            "OMF",                 "TRS",                         "CSS",                    "CRP",                                   "NMM",                                                "T_D"),
                  A_abb = c("Other grains","Sugar crops", "Agricultural residue collection", "Oil mining", "Mineral mining and other quarrying", "Biomass transformation 1st generation", "Biomass transformation 2nd generation with solid biomass", "Iron and steel", "Other manufacturing", "Transport and communications", "Other service sectors", "Chemical, plastic and rubber products", "Manufacture of other non-metallic mineral products", "T_D"))
pdata <- df_GDP_psi %>% filter(Y %in% c(2030, 2050), A %in% c("GRO","C_B","ARE","OIL","OMN","BTR","BTR3","I_S","OMF","TRS","CSS","CRP","NMM","T_D")) %>% F_woc() %>% F_filter(lis_R = lis_R) %>% mutate(R = factor(R, levels = lis_R)) %>% 
  dplyr::rename(value = GDP_psi, variable = A) %>% 
  F_cha_decom() %>% mutate(change = value-Baseline_None) %>% select(-Baseline_None, -value) %>% pivot_wider(names_from = "policy", values_from = change) %>% 
  mutate(change = `1.5C non-CP` - `1.5C CP`)

p <- ggplot(pdata) +
  geom_col(aes(x = variable, y = change/1000, fill = variable), position = "dodge") +
  facet_wrap(R ~ Y, scales = "free", ncol = 2) + MyTheme + Theme_tran + theme(axis.text.x = element_text(angle = 60)) +
  labs(x = "Component", y = "Billion US 2005D per year") + #scale_fill_manual(values = palette_color_cp) + 
  guides(fill = guide_legend(title = "Sector and institution", ncol = 1))
p

ggsave(p, filename = paste0(dir_fig, "/Figure10 GDP decomp/Change in GDPexpenditure due to nonCP GDP_psi.pdf"), 
       width = 24, height = 30, units = "cm")
rm(pdata, p)




## 3.4 GDP expenditure composition | Million US 2005D per year: GDP_s ---------------------------------------------------------------
pdata <- df_GDP_s %>% mutate(value = GDP_s, variable = INS_MCR) %>% filter(Y %in% c(2030))
p <- F_p_GDP(pdata)
p

ggsave(p, filename = paste0(dir_fig, "/Figure10 GDP decomp/GDPexpenditurecomposition GDP_s.pdf"), width = 18, height = 10, units = "cm")

rm(pdata, p, map_A)











###################


df_GDP_psi_g <- df_GDP_psi %>% F_woc() %>% F_filter(lis_R = lis_R) %>% left_join(MCO2_S2) %>% left_join(MSCO2_S2S) %>% filter(!is.na(S)) %>% dplyr::group_by(Y, R, policy, SSP, exemption, S) %>% reframe(GDP_psi = sum(GDP_psi))


# 2030 ----
  pdata <- df_GDP_psi_g %>% filter(Y %in% c(2030)) %>% 
    dplyr::rename(value = GDP_psi, variable = S) %>% 
    F_cha_decom() %>% mutate(change = value-Baseline_None) %>% select(-Baseline_None, -value) %>% pivot_wider(names_from = "policy", values_from = change) %>% 
    mutate(change = `1.5C non-CP` - `1.5C CP`)
  
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_col(pdata %>% filter(SSP == "SSP2"), mapping = aes(x = variable, y = change/1000), position = "dodge") +
    geom_point(pdata %>% filter(SSP == "SSP1"), mapping = aes(x = variable, y = change/1000), shape = 2) +
    geom_point(pdata %>% filter(SSP == "SSP3"), mapping = aes(x = variable, y = change/1000), shape = 3) +
    facet_wrap(~ R, scales = "free", nrow = 2) + MyTheme + Theme_tran + theme(axis.text.x = element_text(angle = 60)) +
    labs(x = "Component", y = "Billion US 2005D per year") #+ scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Policy"))
  
  ggsave(p, filename = paste0(dir_fig, "/Figure10 GDP decomp/Change in GDPexpenditure due to nonCP GDP_psi_g_2030.pdf"), 
         width = 20, height = 20, units = "cm")
     rm(pdata, p)
 
     
     
     
# 2050 ----     
pdata <- df_GDP_psi_g %>% filter(Y %in% c(2050)) %>% 
 dplyr::rename(value = GDP_psi, variable = S) %>% 
 F_cha_decom() %>% mutate(change = value-Baseline_None) %>% select(-Baseline_None, -value) %>% pivot_wider(names_from = "policy", values_from = change) %>% 
 mutate(change = `1.5C non-CP` - `1.5C CP`)

p <- ggplot() +
 geom_hline(yintercept = 0, color = "grey70") +
 geom_col(pdata %>% filter(SSP == "SSP2"), mapping = aes(x = variable, y = change/1000), position = "dodge") +
 geom_point(pdata %>% filter(SSP == "SSP1"), mapping = aes(x = variable, y = change/1000), shape = 2) +
 geom_point(pdata %>% filter(SSP == "SSP3"), mapping = aes(x = variable, y = change/1000), shape = 3) +
 facet_wrap(~ R, scales = "free", nrow = 2) + MyTheme + Theme_tran + theme(axis.text.x = element_text(angle = 60)) +
 labs(x = "Component", y = "Billion US 2005D per year") #+ scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Policy"))

ggsave(p, filename = paste0(dir_fig, "/Figure10 GDP decomp/Change in GDPexpenditure due to nonCP GDP_psi_g_2050.pdf"), 
      width = 20, height = 20, units = "cm")
rm(pdata, p)






rm(df_GDP_psi_g, df_GDP_s, df_GDP_psi, df_GDP_v, df_GDP_vr2)

print("The END of 10_GDP_decom.R")


