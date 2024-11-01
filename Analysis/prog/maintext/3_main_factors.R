# The Rscript for the third plot in the non-CP/CP paper
# Shiya ZHAO, 2024.04.01

# decomposition analysis


# 0. function -------------------------------------------------------------

F_box_vio <- function(pdata, v_palette, horizontal = F){
  
  p <- ggplot(pdata, aes(x = Effect, y = change * 100, fill = Effect), linewidth = .02) +
    gghalves::geom_half_violin(width = 1, position = position_nudge(x = -.2, y = 0)) + 
    # geom_boxplot(width = .2, outlier.shape = NA) + 
    geom_boxplot(width = .2, outlier.size = .5, outlier.alpha = .3) + 
    geom_hline(yintercept = 0, color = "grey70") +
    labs(x = "Effect", y = "Percentage point") + MyTheme + #Theme_tran  +
    scale_fill_manual(values = v_palette) +
    # scale_color_manual(values = v_palette) +
    facet_grid(~R_CGE)
  
  if(horizontal == F){
    p <- p + geom_hline(yintercept = 0, color = "grey") + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
  }else{
    p <- p + geom_vline(xintercept = 0, color = "grey") + coord_flip() + theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1))
  }
  
  
  return(p)
  
}


# 1. Poverty ------------------------------------------------------------

# 0. Data ---
PoVExp_tmp <- rgdx.param(AnaExp, "PoVExp") %>%
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  filter(!is.na(Ref), #R %in% lis_R,
         Y%in% lis_Y)%>%
  mutate(Effect = 'Expenditure') 


PoVInc_tmp <- rgdx.param(AnaInc, "PoV") %>%
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  filter(!is.na(Ref), #R %in% lis_R,
         Y %in% lis_Y) %>%
  mutate(Effect = 'Income') 


PoV <- PoVExp_tmp %>%
  rename.vars(from = 'PoVExp', to = 'PoV') %>%
  rbind(PoVInc_tmp) %>%
  mutate(TH1 = recode(TH, 
                      'pop_1.9' = lis_TH[1],
                      'pop_3.2' = lis_TH[2],
                      'pop_5.5' = lis_TH[3]),
         # R = factor(R, levels = lis_R),
         unit = "person") %>%
  select(-"TH") %>%
  # filter(scenario %in% c(lis_scenario_noex,lis_scenario_exmp,'Baseline'))%>%
  rename.vars(from = 'PoV', to = 'value') %>% F_filter(s_Baseline_woc = F, lis_R = unique(.$R)) %>% F_woc()

Headcount <- PoV 


# income side
Headcount_Inc <- Headcount %>%
  filter(Effect == 'Income', exemption == "None", target != "2C") %>%
  F_cha_decom(lis_del = c("target", "Ref", "policy", "exemption", "Effect")) %>%
  mutate(`Income` = value - Baseline_None, unit = "person") %>% 
  select(R, Y, TH1, SSP, policy, `Income`, unit, Gini, tech) %>% pivot_wider(names_from = "policy", values_from = "Income") %>% 
  mutate(CP_income = `1.5C CP`-`1.5C non-CP`, Transition_income = `1.5C non-CP`) %>% 
  select(-c(`1.5C CP`,`1.5C non-CP`)) #%>% 
# pivot_longer(cols = c("CP_income", "Transition_income"), names_to = "Effect", values_to = "value")


# expenditure side
Headcount_Exp <- Headcount %>% filter(Effect == 'Expenditure', target != "2C") %>%
  F_cha_decom(lis_del = c("target", "Ref", "policy", "exemption", "Effect")) %>%
  mutate(`Indirect price` = value - Baseline_None, unit = "person") %>% 
  select(R, Y, TH1, SSP, policy, exemption, Gini, tech, `Indirect price`, unit)


# non-CPs
Headcount_Exp_woc <- Headcount_Exp %>% filter(policy == "1.5C non-CP") %>% 
  select(-exemption) %>% dplyr::rename("Transition_price" = "Indirect price") 



# CP
Headcount_Exp_wc <- Headcount_Exp %>% filter(policy != "1.5C non-CP") %>% 
  pivot_wider(names_from = "exemption", values_from = "Indirect price") %>% 
  mutate(`CP_penalty` = `None`-`Direct tax exemption`) %>% select(-c("None", "policy")) %>% 
  dplyr::rename("CP_price_indirect_tran" = "Direct tax exemption")

# combine CP with non-CPs
Headcount_Exp <- Headcount_Exp_woc %>% left_join(Headcount_Exp_wc) %>% mutate(CP_price = `CP_price_indirect_tran` - `Transition_price`) %>% select(-"CP_price_indirect_tran", -"policy")

rm(Headcount_Exp_wc, Headcount_Exp_woc)

lis_eff_pov <- c("CP_penalty", "CP_price", "CP_income", "Transition_price", "Transition_income")
# combine expenditure side effects with income side effects
Headcount_eff <- Headcount_Inc %>% left_join(Headcount_Exp)  %>% filter(!is.na(R)) %>% 
  pivot_longer(cols = any_of(c(lis_eff_pov)), names_to = "Effect", values_to = "value") %>% mutate(Effect = factor(Effect, level = lis_eff_pov) %>% fct_rev()) %>% 
  mutate(value = case_when(value < 1 ~ 0, value >= 1 ~ value/1000000), unit = "Million people") 

rm(Headcount_Inc, Headcount_Exp)

## plot  ----
palette_eff_pov <- c("CP_penalty"= "#cea0aa", 
                     "CP_price" = "#e9c2c5", 
                     "CP_income" = "#fee1dd", 
                     "Transition_price" = "#9db0ce",
                     "Transition_income"= "#b8d8e3" )

pdata <- Headcount_eff %>% filter(
  R == "World",
  Effect %in% lis_eff_pov, Gini == "consistent", tech == "None",
  Y %in% c(2030,2050),
  TH1 %in% c('1.9-threshold')) %>% 
  mutate(Effect = factor(Effect, level = lis_eff_pov)) %>% 
  mutate(R1 = R)
  

if(switch_oth == T){
  pdata <- pdata %>% 
    mutate(R1 = case_when(R == "World" ~ "World", R == "R5ASIA" ~ "R5ASIA", R == "R5MAF" ~ "R5MAF",
                          !(R %in% c("World","R5ASIA","R5MAF")) ~ "R5Other")) %>% 
    mutate(R1 = factor(R1, levels = c("World", "R5ASIA", "R5MAF", "R5Other")))
  
  v_name <- "oth"
  v_width <- 16
}else if(switch_oth == "W"){
  pdata <- pdata %>% filter(R == "World")
  v_name <- "world"
  v_width <- 7
}else{
  pdata <- pdata
  v_name <- "R5"
  v_width <- 22
}


# export the plotting data
pdata %>% openxlsx::write.xlsx(file=paste0(dir_csv, "3_Poverty_decomposition_world.xlsx"))

# plot
p <- ggplot() + 
  geom_col(data = pdata %>% filter(SSP == "SSP2"),
           mapping = aes(x = Y, y = value, fill = Effect), 
           width = 0.5, position="stack") +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(#title = "c)", # Added poverty headcount with carbon taxes 
    x = 'Year',
    y = 'Million people',
    size=10) +
  facet_grid(~R1, scales = "free") + # c("World", "R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")
  MyTheme+ theme(axis.text.x = element_text(angle = 0)) + Theme_tran +
  guides(fill = guide_legend(title = 'Effect', ncol = 1, size = 7, byrow = TRUE)) +
  scale_fill_manual(values = palette_eff_pov) 
p1 <- p + theme(legend.position = "none")

ggsave(p1,filename = paste0("3_",v_name,"_PovertyColumn_decomposition.pdf"), path = dir_plot_main,
       width = v_width, height = 6, units = "cm")
rm(p1)


# legend on the right hand side
p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
p_leg

ggsave(p_leg,filename = paste0("3_",v_name,"_PovertyColumn_decomposition_legend_right.pdf"), path = dir_plot_main,
       width = 4.5, height = 4.5, units = "cm")
rm(p_leg)



# legend at the bottom
p_leg <- ggpubr::get_legend(p + theme(legend.position = "bottom") + guides(fill = guide_legend(title = "Factor", nrow = 2)), position = NULL) %>% ggpubr::as_ggplot()
p_leg

ggsave(p_leg,filename = paste0("3_",v_name,"_PovertyColumn_decomposition_legend_bottom.pdf"), path = dir_plot_main,
       width = 10, height = 2, units = "cm")
rm(p_leg, p)






# 2. Poverty rate ---------------------------------------------------------

## 2.1 data ----------------------------------------------------------------
df_pop <- rgdx.param("../../DataArchive/PHIoutput/gdx/Inputdata.gdx", "Population") %>% 
  filter(Ref == "SSP2") %>% select(-Ref) %>% mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  mutate(Population = Population/1000000, unit = "Million people")

colnames(pdata)
# change in poverty rate
pdata <- Headcount_eff %>% filter(R %in% lis_Rphi) %>% left_join(map_R17to5) %>% select(-R_CGE, -R_full) %>% dplyr::rename("R_CGE" = "R5") %>% #filter(R_CGE != "World")
  left_join(df_pop) %>% mutate(change = value/Population,
                               R_CGE = factor(R_CGE, levels = c("World", "R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")), 
                               Effect = factor(Effect, levels = c("CP_penalty","CP_price","CP_income","Transition_price","Transition_income"))) %>% 
  filter(SSP == "SSP2", tech == "None", TH1 == "1.9-threshold")


if(cross_time == T){
  pdata <- pdata
  txt_y <- "crosstime"
}else if(cross_time == "2030"){
  pdata <- pdata %>% filter( Y == "2030")
  txt_y <- "2030"
}else if(cross_time == "3050"){
  pdata <- pdata %>% filter( Y %in% c("2030", "2050"))
  txt_y <- "20302050"
}

## plot ----
if(switch_oth == T){
  pdata <- pdata %>% 
    mutate(R_CGE = case_when(R_CGE == "World" ~ "World", R_CGE == "R5ASIA" ~ "R5ASIA", R_CGE == "R5MAF" ~ "R5MAF",
                          !(R_CGE %in% c("World","R5ASIA","R5MAF")) ~ "R5Other")) %>% 
    mutate(R_CGE = factor(R_CGE, levels = c("World", "R5ASIA", "R5MAF", "R5Other")))
  
  v_name <- "oth"
  v_width <- 18
}else if(switch_oth == "W"){
  pdata <- pdata %>% filter(R_CGE == "World") %>% mutate(Effect = fct_rev(Effect))
  v_name <- "world"
  v_width <- 8
}else{
  pdata <- pdata
  v_name <- "R5"
  v_width <- 22
}

if(switch_oth != "W"){
  p <- F_box_vio(pdata, palette_eff_pov) 
  p1 <- p + theme(legend.position = "none", axis.text.x = element_text(hjust = 1, vjust = 1))
  
  ggsave(p1,filename = paste0("3_",v_name,"_",txt_y,"_PovertyRate_decomposition.pdf"), path = dir_plot_main,
         width = v_width, height = 7, units = "cm")
  rm(p1)
}else{
  p <- F_box_vio(pdata, palette_eff_pov, horizontal = T)
  p1 <- p + theme(legend.position = "none", axis.text.x = element_text(hjust = 1, vjust = 1))
  ggsave(p1,filename = paste0("3_",v_name,"_",txt_y,"_PovertyRate_decomposition.pdf"), path = dir_plot_main,
         width = v_width, height = 12, units = "cm")
  rm(p1)
}


# legend on the right hand side
p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
p_leg

ggsave(p_leg,filename = paste0("3_",v_name,"_PovertyRate_decomposition_legend_right.pdf"), path = dir_plot_main,
       width = 4.5, height = 4.5, units = "cm")
rm(p_leg)


# legend at the bottom
p_leg <- ggpubr::get_legend(p + theme(legend.position = "bottom") + guides(fill = guide_legend(title = "Factor", nrow = 2)), position = NULL) %>% ggpubr::as_ggplot()
p_leg

ggsave(p_leg,filename = paste0("3_",v_name,"_PovertyRate_decomposition_legend_bottom.pdf"), path = dir_plot_main,
       width = 10, height = 2, units = "cm")
rm(p_leg, p)






# 3. Gini ------------------------------------------------------------
df_Gini <- rgdx.param(AnaExp, "Gini_exp") %>%
  F_filter(lis_R=unique(.$R)) %>% F_woc() 

df_Gini_change <- df_Gini %>% filter(policy != "2C", SSP == "SSP2") %>% dplyr::rename(value = "Gini_exp") %>% F_cha_decom() %>% 
  mutate(change = (value - Baseline_None)) %>%  select(-value, -Baseline_None) %>% mutate(v_group = paste0(policy, "_", exemption)) %>% 
  select(-c(policy, exemption)) %>% pivot_wider(names_from = v_group, values_from = change) %>% 
  mutate(`Transition_price` = `1.5C non-CP_None`,
         `CP_price` = `1.5C CP_Direct tax exemption`-`1.5C non-CP_None`,
         `CP_penalty` = `1.5C CP_None` - `1.5C CP_Direct tax exemption`) %>% 
  select(-colnames(.)[grepl("_[A-Z]+", colnames(.))]) %>% 
  pivot_longer(cols = c("Transition_price", "CP_price", "CP_penalty"), names_to = "Effect", values_to = "change")







## plot --------------------------------------------------------------------

palette_gini_eff <- c("Transition_price" = "#9db0ce", 
                      "CP_price" =  "#ec8a83",
                      "CP_penalty" = "#f0bc68")

palette_gini_eff <- c("Transition_price" = "#9db0ce", 
                      "CP_price" =  "#e9c2c5", 
                      "CP_penalty" = "#cea0aa")

pdata <- df_Gini_change %>% left_join(map_R)  %>% 
  mutate(R_CGE = factor(R_CGE, levels = lis_R)) %>% filter(!is.na(R_CGE))



if(cross_time == T){
  pdata <- pdata
  txt_y <- "crosstime"
}else if(cross_time == "2030"){
  pdata <- pdata %>% filter( Y == "2030")
  txt_y <- "2030"
}else if(cross_time == "3050"){
  pdata <- pdata %>% filter( Y %in% c("2030", "2050"))
  txt_y <- "20302050"
}

if(switch_oth == T){
  pdata <- pdata %>% 
    mutate(R_CGE = case_when(R_CGE == "World" ~ "World", R_CGE == "R5ASIA" ~ "R5ASIA", R_CGE == "R5MAF" ~ "R5MAF",
                             !(R_CGE %in% c("World","R5ASIA","R5MAF")) ~ "R5Other")) %>% 
    mutate(R_CGE = factor(R_CGE, levels = c("World", "R5ASIA", "R5MAF", "R5Other")))
  
  v_name <- "oth"
  v_width <- 18
}else if(switch_oth == "W"){
  pdata <- pdata %>% filter(R_CGE == "World")
  v_name <- "world"
  v_width <- 8
}else{
  pdata <- pdata
  v_name <- "R5"
  v_width <- 22
}



colnames(pdata)
pdata %>% dplyr::group_by(Y, SSP, tech, Gini, Effect, R_CGE) %>% dplyr::reframe(median = median(change)*100, mean = median(change)*100) %>% openxlsx::write.xlsx(file=paste0(dir_csv, "3_Gini_decomposition_world.xlsx"))


p <- F_box_vio(pdata, palette_gini_eff, horizontal = F) + theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5))
p1 <- p + theme(legend.position = "none")

ggsave(p1,filename = paste0("3_",v_name, "_",txt_y,"_Gini.pdf"), path = dir_plot_main,
       width = v_width, height = 6, units = "cm")


# legend on the right hand side
p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
p_leg

ggsave(p_leg,filename = paste0("3_",v_name,"_Gini_legend_right.pdf"), path = dir_plot_main,
       width = 3.2, height = 2.9, units = "cm")
rm(p_leg)



# legend at the bottom
p_leg <- ggpubr::get_legend(p+ theme(legend.position = "bottom") + guides(fill = guide_legend(title = "Factor", nrow = 1)), position = NULL) %>% ggpubr::as_ggplot()
p_leg

ggsave(p_leg,filename = paste0("3_",v_name,"_Gini_legend_bottom.pdf"), path = dir_plot_main,
       width = 9, height = 1.2, units = "cm")


rm(p_leg, p)



print("The END of 3_main_factors.R")

