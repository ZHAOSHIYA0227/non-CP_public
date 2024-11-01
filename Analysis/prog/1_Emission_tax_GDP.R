# This program is a summary of all the figures and plots in the paper 
# "Poverty and inequality in China under the long-term global temperature target"
# This program deals with carbon emission, carbon tax, GDP loss, and price change

# Shiya ZHAO, 2021/06/17

# Figure 1a Emission trajectory ----
# Load GHG emission data 
GHG <- cge_ana %>%
  rgdx.param("GHG") %>%
  left_join(MapScenario)  %>% select(Y,R,GHG,Ref) %>% F_filter(s_Baseline_woc = T, lis_R = lis_R) %>% 
  F_woc() %>% select(-Ref)
# 1000t CO2eq, GHG/1000000, "GHG emission | Gt carbon dioxide equivalent"



## Plot ----
colnames(GHG)
pdata <- GHG %>%
  filter(Y %in% lis_Y, target != "2C") %>% F_filter_main() %>% mutate(GHG = GHG/1000000, v_group = paste0(policy, SSP))  %>% dplyr::rename(value = "GHG") 

p_1a <- F_plot_ribbon(pdata, lab_x = "Year", lab_y = expression(paste("Gt",CO[2],"eq/y"))) + facet_wrap(~ factor(R, levels = lis_R)) + #Theme_tran +
  theme(axis.text.x = element_text(angle = 60)) + theme(legend.background = element_rect(fill='transparent', color = 'transparent'))
ggsave(p_1a,filename = "Figure 1a Emission trajectory.pdf", path = dir_fig,
       width = 18, height = 10, units = "cm")


rm(pdata, p_1a)

 # Figure 1b Carbon tax trajectory ----
# Loading carbon tax data 
PGHG <- cge_ana %>%
  rgdx.param("PGHG") %>%
  left_join(MapScenario) %>% 
  F_filter(lis_R= lis_R) %>% F_woc() %>% select(-Ref, -SCENARIO) 

PGHG1 <- PGHG %>% filter(Y == 2025) %>% mutate(Y = 2020, PGHG = 0)
PGHG <- PGHG %>% rbind(PGHG1)
# $ per tonne CO2eq, "Carbon tax | dollar per tonne carbon dioxide equivalent"


## Plot  ----
pdata <- PGHG %>%
  filter(policy == "1.5C CP", R == "World")  %>% F_filter_main() %>% dplyr::rename(value = "PGHG") %>% 
  mutate(v_group = paste0(policy, SSP))

p_1b <- F_plot_ribbon(pdata, lab_x = "Year", lab_y = expression(paste("$/tonne ",CO[2],"eq"))) + theme(axis.text.x = element_text(angle = 60)) #+ Theme_tran

ggsave(p_1b,filename = "Figure 1b Carbon tax.pdf", path = dir_fig,
       width = 10, height = 12, units = "cm")
rm(pdata, p_1b)





# Figure 1c GDP loss ----
df_iamc <- cge_var %>%
  rgdx.param("IAMC_Template") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO) %>% 
  gdata::rename.vars(colnames(.), c("R", "VEMF", "Y", "value", "Ref"))

GDPloss <- df_iamc %>% filter(VEMF == "Pol_Cos_GDP_Los_rat") %>% F_woc() %>%  
  filter(policy != "Baseline non-CP") %>% F_filter(lis_R= lis_R) %>% select(-VEMF, -Ref)
# $ GDP | Million US dollar(2005) per year
# GDP: Million US dollar(2005) per year; GDPloss: % per year

## Plot ----
pdata <- filter(GDPloss,
                # !(startsWith(scenario, "SSP")),
                Y %in% lis_Y, exemption == "None", target != "2C", R %in% lis_R) %>% mutate(target = "1.5C")  %>% F_filter_main() %>% #mutate(value = value*100) %>% 
  # select(-Baseline, -`2C CP`, -`2C non-CP`, -`Baseline non-CP`) %>% 
  mutate(v_group = paste0(policy, SSP))

p_1c <- F_plot_ribbon(pdata, lab_x = "Year", lab_y = expression(paste("%"))) +
  facet_wrap(~ factor(R, levels = lis_R))  + theme(axis.text.x = element_text(angle = 60))
p_1c
ggsave(p_1c,filename = "Figure 1c GDP loss.pdf", path = dir_fig,
       width = 18, height = 10, units = "cm")

rm(pdata, p_1c)


# Figure 1d consumption loss ----
cnsloss <- df_iamc %>% filter(VEMF == "Pol_Cos_Cns_Los_rat") %>% F_woc() %>%  
  filter(policy != "Baseline non-CP") %>% F_filter(lis_R= lis_R) %>% select(-VEMF, -Ref)
# $ consumption | Million US dollar(2005) per year
# consumption: Million US dollar(2005) per year; GDPloss: % per year

## Plot ----
pdata <- filter(cnsloss,
                Y %in% lis_Y, exemption == "None", target != "2C", R %in% lis_R) %>% mutate(target = "1.5C")  %>% 
  F_filter_main() %>% 
  mutate(v_group = paste0(policy, SSP))

p_1d <- F_plot_ribbon(pdata, lab_x = "Year", lab_y = expression(paste("%"))) +
  facet_wrap(~ factor(R, levels = lis_R))  + theme(axis.text.x = element_text(angle = 60))
p_1d
ggsave(p_1d,filename = "Figure 1d consumption loss.pdf", path = dir_fig,
       width = 18, height = 10, units = "cm")

rm(pdata, p_1d)





# Fig 1e price change trajectories ----
PQchange_tmp <- paste0("../../DataArchive/PHIoutput/gdx/Inputdata.gdx") %>% rgdx.param("PriceChange") %>%
  left_join(MapI) %>%
  filter(!is.na(Ref),# R %in% lis_R,
         Y %in% c(seq(2020, 2100, 10))) %>% mutate(value = PriceChange) %>% select("R", "Y", "I_abb", "Ref", "value") %>% F_woc()

PQchange_tmp %>%
  filter(policy %in% c(lis_cp_mitigation, 'Baseline', 'Baseline non-CP')) %>% select(-"Ref", -"target") %>% 
  write.csv(file = paste0(dir_output,"PriceChange.csv"))

# Plot ----
# baseline price

pdata <- PQchange_tmp %>%
  filter(policy %in% c(lis_cp_mitigation, 'Baseline', 'Baseline non-CP'), SSP == "SSP2") %>% F_filter_main() %>% left_join(map_R17) %>% filter(!is.na(I_abb), !is.na(R_CGE)) %>% #filter(I_abb == "Energy") %>% 
  dplyr::group_by(R_CGE, Y, I_abb, Ref, policy, SSP, exemption, target, tech, Gini) %>% dplyr::reframe(value = mean(value)) %>% filter(grepl("Baseline", policy)) %>% 
  mutate(v_group = paste0(policy, SSP, exemption, R_CGE))


p_1e1 <- ggplot(pdata) +
  geom_line(aes(x = Y, y = value, group = paste0(policy, SSP, exemption, R_CGE), color = R_CGE, linetype = policy)) +
  geom_hline(
    yintercept = 1, color = "grey", linetype = "dashed"
  ) +
  labs(x = "Year", y = "Price index (Price = 1 in 2010)") +
  facet_wrap(~I_abb) + MyTheme + theme(axis.text.x = element_text(angle = 90))
p_1e1

ggsave(p_1e1,filename = "Figure 1d1 Baseline_price.pdf", path = dir_fig,
       width = 26, height = 24, units = "cm")
rm(pdata, p_1d1)

# price change from Baseline: line ------
df_PQ_Baseline <- PQchange_tmp %>% left_join(map_R17) %>% 
  dplyr::group_by(R_CGE, Y, I_abb, Ref, policy, SSP, exemption, target, tech, Gini) %>% dplyr::reframe(value = mean(value)) %>% 
  filter(policy == "Baseline") %>% select(R_CGE,Y, I_abb, SSP, value, tech, Gini) %>% dplyr::rename("value_Baseline" = "value") %>% distinct()

df_PQ <- PQchange_tmp %>% left_join(map_R17) %>% 
  dplyr::group_by(R_CGE, Y, I_abb, Ref, policy, SSP, exemption, target, tech, Gini) %>% dplyr::reframe(value = mean(value)) %>% 
  filter(policy %in% c(lis_cp_mitigation)) %>% select(-"Ref", -"target") %>% left_join(df_PQ_Baseline) %>% 
  mutate(changerate = (value-value_Baseline)/value_Baseline * 100) %>%
  transform(I_abb = factor(I_abb, levels = lis_I_abb)) %>% select("R_CGE", "Y", "I_abb", "policy", "SSP", "exemption", "tech", "Gini", "changerate")

df_PQ %>% filter(Y %in% c(2030, 2050)) %>% 
  dplyr::group_by(I_abb, Y, policy,SSP, exemption, Gini, tech) %>% dplyr::reframe(median = median(changerate), mean = mean(changerate)) %>% 
  openxlsx::write.xlsx(file = paste0(dir_csv, "1d_PHI_PriceChange_SSP_regional.xlsx"))

pdata0 <- df_PQ %>% filter(exemption == "None", !is.na(R_CGE)) %>% mutate(v_group = paste0(policy, SSP, exemption, R_CGE)) %>% F_filter_main() %>% 
  dplyr::rename(value = "changerate") %>% F_R17full()


dir.create(paste0( dir_fig, "Figure1d2_PriceChange/"))
for(i in 1:length(lis_I_abb)){
  pdata <- pdata0 %>% filter(I_abb == lis_I_abb[i]) 
  p <- F_plot_ribbon(pdata, lab_x = "Year", lab_y = "Price change from Baseline (%)") + facet_wrap(~R_CGE) + labs(title = lis_I_abb[i]) + theme(axis.text.x = element_text(angle = 90)) 
  ggsave(p,filename = paste0(dir_fig, "Figure1d2_PriceChange/",gsub("&", "", lis_I_abb[i]),".pdf"), #path = dir_fig,
         width = 26, height = 20, units = "cm")
  rm(p)
}

rm(pdata, pdata0)









# price change decomposition ----
pdata0 <- df_PQ %>% filter(grepl("CP", policy), I_abb == "Energy", tech == "None") %>% F_filter_main()
a <- pdata0 %>% filter(policy == "1.5C non-CP") %>% dplyr::rename("Transition_price" = "changerate") %>% select(-exemption, -policy) 
b <- pdata0 %>% filter(policy == "1.5C CP", exemption == "Direct tax exemption") %>% left_join(a) %>% mutate(CP_price = changerate-Transition_price) %>% select(-exemption, -policy, -changerate, -Transition_price)
c <- pdata0 %>% filter(policy == "1.5C CP", exemption == "None") %>%  left_join(pdata0 %>% filter(policy == "1.5C CP", exemption == "Direct tax exemption") %>% select(-exemption, -policy) %>% dplyr::rename(direct = changerate)) %>% 
  mutate(CP_penalty = changerate-direct) %>% select(-exemption, -policy, -changerate, -direct)
  

# pdata <- pdata0 %>% filter(exemption == "None") %>% select(-exemption) %>% dplyr::rename(Indirect = "changerate") %>%   
#   left_join(pdata0 %>% filter(exemption == "Direct tax exemption") %>% select(-exemption)%>% dplyr::rename(value_direct = "changerate")) %>% 
#   F_R17full() %>% mutate(Direct = Indirect-value_direct) %>% select(-"value_direct") %>% pivot_longer(cols = c("Indirect", "Direct"), names_to = "Effect", values_to = "value") %>% 
#   filter(Y %in% seq(2020, 2100,10))
# pdata[is.na(pdata$value),]$value <- 0

pdata <- a %>% full_join(b) %>% full_join(c) %>% pivot_longer(cols = c("Transition_price", "CP_penalty", "CP_price"), names_to = "Effect", values_to = "value")

dir.create(paste0(dir_fig, "Figure1d3_PriceChange_decomposition/"))
lis_SSP <- c("SSP1", "SSP2", "SSP3")
for(s in 1: length(lis_SSP)){
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "grey") +
    geom_area(pdata %>% filter(SSP == lis_SSP[s]), 
              mapping = aes(x = as.numeric(as.character(Y)), y = value, fill = Effect), position = "stack") +
    MyTheme + 
    labs(x = "Year", y = "%", title = paste0("Energy | ", lis_SSP[s])) +
    facet_wrap(~R_CGE)
  p
  
  ggsave(p,filename = paste0(dir_fig, "Figure1d3_PriceChange_decomposition/",lis_SSP[s],".pdf"), #path = dir_fig,
         width = 28, height = 20, units = "cm")
  rm(p)
}



rm(a,b,c, pdata, df_PQ_Baseline, df_PQ, PQchange_tmp,GDPloss, PGHG, PGHG1, GHG)
