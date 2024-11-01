# The Rscript for the second plot in the non-CP/CP paper
# Shiya ZHAO, 2024.04.01

# Macroeconomic loss and poverty etc. 
require(patchwork)


# 0. functions ------------------------------------------------------------

F_plot_nishimori <- function(pdata1, pdata2, v_width = c(1.5,1)){
  
  # plot1: area
  lis_R_pov <- c("R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")
  p1 <- ggplot(pdata1) +
    geom_area(aes(x = as.numeric(as.character(Y)), y = value, fill = factor(R, level = lis_R_pov) %>% fct_rev()), 
              position = "stack") +
    MyTheme  + theme(axis.text.x = element_text(angle = 60), legend.position = "none") + Theme_tran + facet_grid(~policy) +
    labs(x = "Year", y = "Million people") +
    scale_fill_manual(values = palette_color_R5) +
    guides(fill = guide_legend(title = "Region")) + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
  
  
  # plot2: col
  a <- pdata1 %>% dplyr::group_by(Ref, Y, unit, policy, SSP, exemption, target) %>% 
    reframe(value_sum = sum(value))
  
  p2 <- ggplot(pdata2) +
    geom_col(aes(x = policy, y = value, fill = factor(R, level = c(lis_R_pov)) %>% fct_rev()), 
             position = "stack", width = .6) +
    facet_wrap(~ Y) +
    MyTheme + 
    theme(axis.text.x = element_text(angle = 60),
                    legend.position = "none",
                    axis.line.y = element_line(linewidth = 0, color = "none"),
                    axis.text.y =  element_text(size = 0),
                    axis.ticks.y = element_line(linewidth = 0)) +
    Theme_tran +
    labs(x = "", y = "") +
    scale_fill_manual(values = palette_color_R5) +
    guides(fill = guide_legend(title = "Region")) +
    scale_y_continuous(limits = c(0,max(a$value_sum)))+ theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
  
  plot <- p1 + p2 + patchwork::plot_layout(ncol=2,widths=c(1.5,1))
  return(plot)
}



# ribbon plot ----
F_plot_ribbon <- function(pdata, lab_x = "Year", lab_y = expression(paste("Gt",CO[2],"eq/y"))){
  
  pdata1 <- pdata %>% select(-"v_group") %>% # filter(SSP != "SSP2") %>%
    mutate(name = paste0(SSP, "_", Gini, "_", tech)) %>% select(-SSP, -Gini, -tech) %>% 
    pivot_wider(names_from = "name", values_from = "value") %>% 
    mutate(v_min = apply(.[colnames(.)[startsWith(colnames(.), "SSP")]],1,min), 
           v_max = apply(.[colnames(.)[startsWith(colnames(.), "SSP")]],1,max))
  
  p <- ggplot()  + 
    geom_ribbon(data= pdata1, mapping = aes(x = Y, ymin=v_min,ymax=v_max, group = policy, fill = policy), alpha=.1) +
    geom_line(data =  pdata, mapping = aes(x = Y, y = value, group = v_group,color = policy, linetype = SSP, linewidth = SSP)) +
    geom_point(data =  pdata, mapping = aes(x = Y, y = value, color = policy, shape = tech), size = 1.2) +
    labs(x = lab_x, y = lab_y, size = 9) +
    geom_abline(intercept = 0, slope = 0, color = "grey") +
    MyTheme + 
    guides(color=guide_legend(title='Scenario',ncol = 1), 
           linetype = guide_legend(title = 'SSP'), 
           # shape = guide_legend(title = "SSP"),
           shape = guide_legend(title = "Technology"),
           fill = guide_legend(title = "Scenario"),
           linewidth = guide_legend(title = "SSP")) +
    scale_color_manual(values = palette_color_cp) +
    scale_fill_manual(values = palette_color_cp) +
    scale_linetype_manual(values = palette_line_SSP) +
    # scale_shape_manual(values = palette_point_SSP) +
    scale_linewidth_manual(values = c("SSP2" = .6, "SSP1" = .4, "SSP3" = .4)) + 
    scale_x_discrete(breaks=seq(2010, 2100, 10)) 
  # p
  return(p)
}







# 1. GDP loss -----------------------------------------------------------------

# Loading GDP 
GDP <- cge_ana %>%
  rgdx.param("GDP") %>%
  left_join(MapScenario) %>% 
  F_filter(lis_R= lis_R) %>% F_woc() %>% 
  # transform(policy = factor(policy, levels = lis_cp)) %>%
  select(-c("SCENARIO")) 


GDPloss <- GDP %>% filter(exemption == "None") %>% 
  select(-Ref, -target) %>% spread(policy, GDP) %>%
  pivot_longer(cols = colnames(.)[startsWith(colnames(.), "1.5C") | startsWith(colnames(.), "2C") ], names_to = "policy", values_to = "value") %>% 
  mutate(GDPloss = (Baseline-`value`)/value) %>% select(-"Baseline non-CP")
# $ GDP | Million US dollar(2005) per year
# GDP: Million US dollar(2005) per year; GDPloss: % per year

## Plot ----
pdata <- GDPloss %>% filter(Y %in% c(seq(2020,2050,10)), R == "World", Gini == "consistent",!startsWith(policy, "2C"), tech == "None", SSP == "SSP2")  %>% 
  mutate(value = GDPloss*100) %>% mutate(target = "1.5C") %>% select(-GDPloss)  %>% select(-Baseline) %>% mutate(v_group = paste0(policy, SSP, Gini, tech))


p <- ggplot(pdata) +
  geom_line(aes(x = Y, y = value, color = policy, group = policy)) + MyTheme +
  geom_point(aes(x = Y, y = value, color = policy, group = policy), shape = 1, size = 2.5) +
  scale_color_manual(values = palette_color_cp) + labs(x = "Year", y = "%") +
  guides(color=guide_legend(title='Policy',ncol = 2)) +
  theme(axis.text.x = element_text(angle = 0), legend.position = "none") + Theme_tran 
p

ggsave(p, filename = paste0("2_GDPloss.pdf"), path = dir_plot_main,
       width = 5.5, height = 6, units = "cm")



# legend
p1 <- p +
  theme(axis.text.x = element_text(angle = 0), legend.position = "bottom") + Theme_tran 
p1

p_leg <- ggpubr::get_legend(p1, position = NULL) %>% ggpubr::as_ggplot()
ggsave(p_leg,filename = paste0("2_GDPloss_legend.pdf"), path = dir_plot_main,
       width = 5.8, height = .5, units = "cm")

rm(GDPloss, GDP)





# 2.consumption -------------------------------------------------------------
df_iamc <- cge_var %>%
  rgdx.param("IAMC_Template") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO) %>% 
  gdata::rename.vars(colnames(.), c("R", "VEMF", "Y", "value", "Ref"))


# df_cns <- df_iamc %>% filter(VEMF == "CNS") %>% F_woc() %>%  
#   transform(policy = factor(policy, levels = lis_cp)) %>% filter(policy != "Baseline non-CP") %>% F_filter(lis_R= lis_R) %>% select(-VEMF) 
# 
# 
# df_cnsloss <- df_cns %>% filter(exemption == "None") %>% 
#   F_woc() %>% select(-Ref, -target) %>% spread(policy, value) %>%
#   pivot_longer(cols = colnames(.)[startsWith(colnames(.), "1.5C")], names_to = "policy", values_to = "value") %>% 
#   mutate(cnsloss = (Baseline-`value`)/value) 

df_cnsloss <- df_iamc %>% filter(VEMF == "Pol_Cos_Cns_Los_rat") %>% F_woc() %>%  
  # transform(policy = factor(policy, levels = lis_cp)) %>% 
  filter(policy != "Baseline non-CP", target == "1.5C") %>% F_filter(lis_R= lis_R) %>% select(-VEMF, -Ref) %>% 
  spread(policy, value) %>%
  pivot_longer(cols = colnames(.)[startsWith(colnames(.), "1.5C")], names_to = "policy", values_to = "cnsloss")

# $ consumption | billion USD_2010/yr per year
# consumption: billion USD_2010/yr per year; GDPloss: % per year


pdata <- df_cnsloss %>% filter(Y %in% c(seq(2010,2050,10)), R == "World", Gini == "consistent", !startsWith(policy, "2C"), 
                               tech == "None", SSP == "SSP2", Y != "2010", exemption == "None") %>% 
  mutate(value = cnsloss)%>% mutate(target = "1.5C") %>% select(-cnsloss)  %>% 
  mutate(v_group = paste0(policy, SSP, Gini, tech))


p <- ggplot(pdata) +
  geom_line(aes(x = Y, y = value, color = policy, group = policy)) + MyTheme +
  geom_point(aes(x = Y, y = value, color = policy, group = policy), shape = 1, size = 2.5) +
  scale_color_manual(values = palette_color_cp) + labs(x = "Year", y = "%") +
  guides(color=guide_legend(title='Policy',ncol = 2)) +
  theme(axis.text.x = element_text(angle = 0), legend.position = "none") + Theme_tran 
p

ggsave(p, filename = paste0("2_Consumption loss.pdf"), path = dir_plot_main,
       width = 5.5, height = 6, units = "cm")

# legend
p1 <- p +
  theme(axis.text.x = element_text(angle = 0), legend.position = "bottom")
p1

p_leg <- ggpubr::get_legend(p1, position = NULL) %>% ggpubr::as_ggplot()

ggsave(p_leg,filename = paste0("2_Consumption loss_legend.pdf"), path = dir_plot_main,
       width = 5.8, height = .5, units = "cm")

rm(df_cnsloss, p_leg)


# ribbon ----------------------------

p <- F_plot_ribbon(pdata, lab_x = "Year", lab_y = expression(paste("%"))) + #theme(legend.position = "none") +
   theme(axis.text.x = element_text(angle = 60), legend.position = "none") + Theme_tran
p
ggsave(p,filename = "2_ribbon_Consumption loss.pdf", path = dir_plot_main,
       width = 5.5, height = 6, units = "cm")


# legend
p1 <- p +
  theme(axis.text.x = element_text(angle = 0), legend.position = "bottom")
p1

p_leg <- ggpubr::get_legend(p1, position = NULL) %>% ggpubr::as_ggplot()

ggsave(p_leg,filename = paste0("2_ribbon_Consumption loss_legend.pdf"), path = dir_plot_main,
       width = 5.8, height = .5, units = "cm")

rm(df_cnsloss, p_leg)


rm(pdata, p)


rm(df_cns, df_cnsloss)









# 3. Price change ---------------------------------------------------------
df_PQ <- paste0("../../DataArchive/PHIoutput/gdx/Inputdata.gdx") %>%
  rgdx.param("PriceChange") %>%
  left_join(MapScenario) %>% 
  left_join(MapI) %>% 
  filter(!is.na(I_abb)) %>% 
  F_woc() %>% 
  mutate(value = PriceChange) %>% 
  select(-c("SCENARIO", "PriceChange")) 
  
pdata <- df_PQ %>% filter(R == "WLD", Y %in% lis_Y, exemption == "None", !startsWith(policy, "2C"), tech == "None") %>% 
  F_filter(lis_R= c("WLD", lis_R)) %>% mutate(v_group = paste0(policy, SSP, Gini, tech)) %>% select(-Ref) 


p <- F_plot_ribbon(pdata, lab_x = "Year", lab_y = expression(paste("%"))) + #theme(legend.position = "none") +
  geom_hline(yintercept = 1, color = "grey") +
  facet_wrap(~I_abb) +
  theme(axis.text.x = element_text(angle = 60)) + Theme_tran
p
rm(p, pdata)

# price change
df_PQ_rate <- df_PQ %>% 
  F_cha_decom() %>% 
  mutate(value = (value-Baseline_None)/Baseline_None * 100) %>%
  transform(I_abb = factor(I_abb, levels = lis_I_abb)) 

pdata <- df_PQ_rate %>% filter(Y %in% c(2030),  I_abb %in% c("Food", "Energy", "Transport"),
                               exemption == "None", Gini == "consistent", !startsWith(policy, "2C")) %>% 
  mutate(v_group = paste0(policy, "_", tech))

p <- ggplot(pdata, aes(x = v_group, y = value, fill = v_group, group = v_group, alpha = policy), linewidth = .01) + MyTheme + theme(legend.position = "none") +
  gghalves::geom_half_violin(width = .7, position = position_nudge(x = -.2, y = 0)) + labs(x = "Commodity", y = "%") +
  geom_boxplot(width = .2, outlier.size = .5, outlier.alpha = .3) + Theme_tran + 
  geom_hline(yintercept = 0, color = "grey", alpha = .5) +
  facet_grid(~I_abb) + theme(axis.text.x = element_text(angle = 90)) +
  # scale_fill_manual(values = palette_color_cp1) + 
  scale_alpha_manual(values = palette_alpha_cp) + 
  guides(fill = guide_legend(title = "Policy"))
p  

ggsave(p,filename = "2_PriceChange.pdf", path = dir_plot_main,
       width = 12, height = 6, units = "cm")
rm(p, pdata)



# food
pdata <- df_PQ_rate %>% filter(Y %in% c(2030), I_abb %in% c("Food"), exemption == "None", Gini == "consistent",!startsWith(policy, "2C")) %>% 
  mutate(v_group = paste0(tech))

p <- ggplot(pdata, aes(x = v_group, y = value, fill = v_group, group = v_group), linewidth = .01) + MyTheme + theme(legend.position = "none") +
  gghalves::geom_half_violin(width = .7, position = position_nudge(x = -.2, y = 0)) + labs(x = "Commodity", y = "%") +
  geom_boxplot(width = .2, outlier.size = .5, outlier.alpha = .3) + Theme_tran + 
  geom_hline(yintercept = 0, color = "grey", alpha = .5) + labs(title = "Food", x = "Technology assumption") + 
  facet_grid(~policy) + theme(axis.text.x = element_text(angle = 90)) +
  # scale_fill_manual(values = palette_color_cp) +
  guides(fill = guide_legend(title = "Policy"))
p  

ggsave(p,filename = "2_PriceChange_food.pdf", path = dir_plot_main,
       width = 15, height = 10, units = "cm")
rm(p, pdata, df_PQ)






## 3.1 Global price -----------------------------------------------------
pdata <- df_PQ_rate %>% filter(Y %in% c(seq(2020, 2050, 10)),  I_abb %in% c("Food", "Energy"), SSP == "SSP2",
                               exemption == "None", Gini == "consistent", !startsWith(policy, "2C"), R == "WLD") %>% 
  F_filter_main()
# value is percentage change

# put energy and food in one plot
p <- ggplot(pdata, aes(x = Y, y = value, fill = policy, group = policy), linewidth = .01) + MyTheme + 
  theme(legend.position = "bottom") +
  geom_col(position = "dodge") + Theme_tran + 
  geom_hline(yintercept = 0, color = "grey", alpha = .5) + labs(y = "%", x = "Year") + 
  facet_grid(I_abb~.) + 
  # facet_grid(fct_rev(I_abb)~.) + 
  theme(axis.text.x = element_text(angle = 0), axis.text.y = element_text(angle = 0)) +
  scale_fill_manual(values = palette_color_cp) +
  guides(fill = guide_legend(title = "Policy",ncol = 2)) 
p  

ggsave(p,filename = "2_PriceChange_foodenergy.pdf", path = dir_plot_main,
       width = 6.2, height = 12, units = "cm")
rm(p, df_PQ)


# put energy and food in separate plots
lis_I_loop <- unique(pdata$I_abb)
for(i in 1:length(lis_I_loop)){
  
  pdata1 <- pdata %>% filter(I_abb == lis_I_loop[i])
  p <- ggplot(pdata1, aes(x = Y, y = value, fill = policy, group = policy), linewidth = .01) + MyTheme + 
    theme(legend.position = "none") +
    geom_col(position = "dodge") + Theme_tran + 
    geom_hline(yintercept = 0, color = "grey", alpha = .5) + labs(y = "%", x = "Year") + 
    # facet_grid(I_abb~.) + 
    # facet_grid(fct_rev(I_abb)~.) + 
    theme(axis.text.x = element_text(angle = 0), axis.text.y = element_text(angle = 0)) +
    scale_fill_manual(values = palette_color_cp) +
    guides(fill = guide_legend(title = "Policy",ncol = 2)) 
  p  
  
  ggsave(p,filename = paste0("2_PriceChange_",lis_I_loop[i],".pdf"), path = dir_plot_main,
         width = 5.5, height = 5.5, units = "cm")
  rm(p, pdata1)
  
}

rm(pdata)








rm(df_PQ, df_PQ_rate)

# 4. Poverty --------------------------------------------------------------

# 4.1 load data ---
PoVExp_tmp <- rgdx.param(AnaExp, "PoVExp") %>%
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  filter(!is.na(Ref), R %in% lis_R,
         Y%in% lis_Y)%>%
  mutate(Effect = 'Expenditure') 


PoVInc_tmp <- rgdx.param(AnaInc, "PoV") %>%
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  filter(!is.na(Ref), R %in% lis_R,
         Y %in% lis_Y) %>%
  mutate(Effect = 'Income') 


PoV <- PoVExp_tmp %>%
  rename.vars(from = 'PoVExp', to = 'PoV') %>%
  rbind(PoVInc_tmp) %>%
  mutate(TH1 = recode(TH, 
                      'pop_1.9' = lis_TH[1],
                      'pop_3.2' = lis_TH[2],
                      'pop_5.5' = lis_TH[3]),
         R = factor(R, levels = lis_R),
         unit = "person") %>%
  select(-"TH") %>%
  # filter(scenario %in% c(lis_scenario_noex,lis_scenario_exmp,'Baseline'))%>%
  rename.vars(from = 'PoV', to = 'value') %>% F_filter(s_Baseline_woc = F, lis_R = lis_R) %>% F_woc()

Headcount <- PoV 
Headcount_change <- Headcount %>% F_cha_decom() %>% 
  mutate(value = value/1000000, unit = "Million people")



## 4.2 Plot: line ----
pdata <- filter(Headcount,
                R == "World",
                TH1 %in% c('1.9-threshold'),
                Y %in% seq(2010, 2050, 10),
                exemption == "None", Gini == "consistent",!startsWith(policy, "2C"), tech == "None",
                Effect == 'Expenditure') %>% select(-Ref) %>% 
  mutate(v_group =paste0(policy, SSP, exemption, tech), value = value/1000000)


p <- F_plot_ribbon(pdata, lab_y = "Million people") + Theme_tran + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 60))
p
ggsave(p,filename = "2_PovertyHeadcount_line.pdf", path = dir_plot_main,
       width = 5.3, height = 6, units = "cm")
rm(p)



# legend
p <- F_plot_ribbon(pdata, lab_y = "Million people") + Theme_tran +
  theme(axis.text.x = element_text(angle = 60))
p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
ggsave(p_leg,filename = paste0("2_PovertyHeadcount_legend.pdf"), path = dir_plot_main,
       width = 3.5, height = 5, units = "cm")
  rm(pdata, p, p_leg)








## 4.3 plot stack global baseline ----
pdata <- Headcount %>% filter(Effect == "Expenditure", exemption == "None", Gini == "consistent", !startsWith(policy, "2C"), tech == "None", Y %in% seq(2020, 2050, 10), R != "World", SSP == "SSP2") %>% F_TH1() %>% 
  filter(TH == "$1.9 per capita per day", policy == "Baseline")

lis_R_pov <- c("R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")
p <- ggplot(pdata) +
  geom_area(aes(x = as.numeric(as.character(Y)), y = value/1000000, fill = factor(R, level = lis_R_pov) %>% fct_rev()), 
            position = "stack") +
  # facet_wrap(~policy) +
  MyTheme+ Theme_tran +
  labs(x = "Year", y = "Million people") +
  scale_fill_manual(values = palette_color_R5) +
  guides(fill = guide_legend(title = "Region", nrow = 2))
p1 <- p + theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = .7), 
                axis.text.y = element_text(angle = 0, vjust = .8), 
                legend.position = "none") 
ggsave(p1,filename = "2_PovertyArea_Baseline.pdf", path = dir_plot_main,
       width = 5.8, height = 5, units = "cm")
rm(p1,p)


# legend
p <-  ggplot(pdata) +
  geom_area(aes(x = as.numeric(as.character(Y)), y = value/1000000, fill = factor(R, level = lis_R_pov)), 
            position = "stack") +
  # facet_wrap(~policy) +
  MyTheme+ Theme_tran +
  labs(x = "Year", y = "Million people") +
  scale_fill_manual(values = palette_color_R5) +
  guides(fill = guide_legend(title = "Region", nrow = 2))+ theme(legend.position = "bottom") 

p_leg <- ggpubr::get_legend(p  , position = NULL) %>% ggpubr::as_ggplot()
ggsave(p_leg,filename = paste0("2_PovertyArea_legend.pdf"), path = dir_plot_main,
       width = 8.5, height = 1.1, units = "cm")
rm(pdata, p, p_leg)






## 4.5 Plot column region ---------------------------------------------------------
pdata <- Headcount %>% filter(Effect == "Expenditure", exemption == "None", Y %in% c(2030, 2050), R != "World", SSP == "SSP2", Gini == "consistent", !startsWith(policy, "2C"), tech == "None") %>% F_TH1() %>% 
  filter(TH == "$1.9 per capita per day", policy != "Baseline")

lis_R_pov <- c("R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")
p <- ggplot(pdata) +
  geom_col(aes(x = policy, y = value/1000000, fill = factor(R, level = lis_R_pov) %>% fct_rev()), 
            position = "stack", width = .6) +
  facet_wrap(~ Y) +
  MyTheme + theme(axis.text.x = element_text(angle = 0), legend.position = "none") + Theme_tran +
  labs(x = "Year", y = "Million people") +
  scale_fill_manual(values = palette_color_R5) +
  guides(fill = guide_legend(title = "Region"))
p
ggsave(p,filename = "2_PovertyHeadcount_policy.pdf", path = dir_plot_main,
       width = 10, height = 10, units = "cm")
rm(pdata,p, lis_R_pov)





## 4.6 Plot poverty headcount increase by region ------------
lis_R_pov <- c("R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")

pdata <- Headcount %>%
  filter(Effect == 'Expenditure', exemption == "None", TH1 %in% c('1.9-threshold'), R %in% lis_R_pov, Y %in% c(2030, 2050), SSP == "SSP2", Gini == "consistent", !startsWith(policy, "2C"), tech == "None") %>%
  F_cha_decom(lis_del = c("target", "Ref", "policy", "exemption", "Effect")) %>%
  mutate(`change` = value - Baseline_None, unit = "person")
# colnames(pdata)
pdata1 <- pdata %>% dplyr::group_by(Y, TH1, tech, Gini, policy, SSP, exemption) %>% 
  dplyr::reframe(change_sum = sum(change))

p <- ggplot() +
    geom_col(data = pdata, mapping = aes(x = Y, y = change/1000000, fill = factor(R, level = c(lis_R_pov)) %>% fct_rev()), 
           position = "stack", width = .5) +
  geom_point(data = pdata1, mapping = aes(x = Y, y = change_sum/1000000), shape = 18) + 
  geom_hline(yintercept = 0, color = "grey") +
  facet_wrap(~ policy) +
  MyTheme + theme(axis.text.x = element_text(angle = 0), legend.position = "none", strip.text = element_text(size = 8)) + Theme_tran +
  labs(x = "Year", y = "Million people") +
  scale_fill_manual(values = palette_color_R5) +
  guides(fill = guide_legend(title = "Region"))
p
ggsave(p,filename = "2_PovertyChange_policy.pdf", path = dir_plot_main,
       width = 6.48, height = 6.8, units = "cm")

rm(pdata1, pdata,p, lis_R_pov)





## 4.7 Plot combine area with col ----------------------------------------
# plot 1: area
lis_R_pov <- c("R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")
pdata1 <- Headcount %>% filter(Effect == "Expenditure", exemption == "None", Y %in% seq(2010, 2050, 10), R != "World", SSP == "SSP2", Gini == "consistent", !startsWith(policy, "2C"), tech == "None") %>% F_TH1() %>% 
  filter(TH == "$1.9 per capita per day", policy == "Baseline") %>% mutate(value = value/1000000, policy = "Baseline")

# plot 2: column
pdata2 <- Headcount %>%
  filter(Effect == 'Expenditure', exemption == "None", TH1 %in% c('1.9-threshold'), R %in% lis_R_pov, Y %in% c(2030, 2050), SSP == "SSP2", Gini == "consistent", !startsWith(policy, "2C"), tech == "None") %>%
  F_cha_decom(lis_del = c("target", "Ref", "policy", "exemption", "Effect")) %>%
  mutate(value= value/1000000, unit = "Million people")


p <- F_plot_nishimori(pdata1, pdata2, v_width = c(1,1)) + Theme_tran

ggsave(p,filename = "2_Poverty_areacol.pdf", path = dir_plot_main,
       width = 8, height = 8, units = "cm")
rm(pdata1, pdata2,p,lis_R_pov)




# 5. Hunger risk ----------------------------------------------------------
## 5.1 World line ----------------------------------------------------------
df_hug <- df_iamc %>% filter(VEMF == "Pop_Ris_of_Hun") %>% mutate(unit = "million") %>% left_join(MapScenario) %>% F_woc() %>% F_filter(lis_R = lis_R) %>% select(-SCENARIO)

pdata <- df_hug %>% filter(R == "World", exemption == "None", Y %in% seq(2010,2050,10), Gini == "consistent", !startsWith(policy, "2C"), tech == "None") %>% select(-Ref) %>% mutate(v_group = paste0(SSP, policy, exemption, target)) 
colnames(pdata)

p <- F_plot_ribbon(pdata, lab_y = "Million people") + Theme_tran + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 60))
p


ggsave(p,filename = "2_POPHungerRisk_line.pdf", path = dir_plot_main,
       width = 5.3, height = 6, units = "cm")
 rm(p, pdata)



## 5.2 Regional stack Baseline----------------------------------------------------------
 
 lis_R_pov <- c("R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")
 
 
 pdata <- df_hug %>% F_filter(lis_R = lis_R_pov, s_Baseline_woc = F) %>% 
   filter(policy == "Baseline", SSP == "SSP2", Y %in% seq(2020, 2050,10), 
          Gini == "consistent", !startsWith(policy, "2C"), tech == "None")
 
 p <- ggplot(pdata) +
   geom_area(aes(x = as.numeric(as.character(Y)), y = value, fill = factor(R, level = lis_R_pov) %>% fct_rev()), 
             position = "stack") +
   # facet_wrap(policy~SSP) +
   MyTheme +
   labs(x = "Year", y = "Million people") +
   scale_fill_manual(values = palette_color_R5) + Theme_tran + 
   theme(legend.position = "none", axis.text.x = element_text(angle = 0, vjust = .5, hjust = .8), 
         axis.text.y = element_text(angle = 0, vjust = 1)) +
   guides(fill = guide_legend(title = "Region"))
 p
 ggsave(p,filename = "2_hunger risk headcount area_Baseline.pdf", path = dir_plot_main,
        width = 5.8, height = 5, units = "cm")
  rm(p)
 
 
 
 
 
  ## 5.3 Regional column change in hunger risk policy ----------------------------------------------------------
 
 lis_R_pov <- c("R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")
 
 
 pdata <- df_hug %>% F_filter(lis_R = lis_R_pov, s_Baseline_woc = F) %>% 
   filter(SSP == "SSP2", Y %in% c(2030, 2050), exemption== "None", Gini == "consistent", !startsWith(policy, "2C"), tech == "None") %>% 
   F_cha_decom() %>% mutate(change = value-`Baseline_None`)
 
 pdata1 <- pdata %>% dplyr::group_by(Y, tech, Gini, policy, SSP, exemption) %>% 
   dplyr::reframe(change_sum = sum(change))

 
 p <- ggplot(pdata) +
   geom_col(aes(x = as.character(Y), y = change, fill = factor(R, level = lis_R_pov) %>% fct_rev()), 
             position = "stack", width = .6) +
   geom_point(data = pdata1, mapping = aes(x = Y, y = change_sum), shape = 18) + 
   geom_hline(yintercept = 0, color = "grey") +
   facet_wrap(~policy) +
   MyTheme +
   labs(x = "Year", y = "Million people") +
   scale_fill_manual(values = palette_color_R5) + Theme_tran + 
   theme(legend.position = "none", axis.text.x = element_text(angle = 0), strip.text = element_text(size = 1)) +
   guides(fill = guide_legend(title = "Region"))
 p
 ggsave(p,filename = "2_hunger risk headcount_change_col_policy.pdf", path = dir_plot_main,
        width = 6.48, height = 6.8, units = "cm")
  rm(p)
 
 
 
 
 

 

## 5.4 Plot combine area with col ------------------------------------------------------------------
 
 pdata1 <- df_hug %>% F_filter(lis_R = lis_R_pov, s_Baseline_woc = F) %>% filter(policy == "Baseline", SSP == "SSP2", Y %in% seq(2010, 2050,10), Gini == "consistent", !startsWith(policy, "2C"), tech == "None")
 
 pdata2 <- df_hug %>% F_filter(lis_R = lis_R_pov, s_Baseline_woc = F) %>% filter(policy != "Baseline", SSP == "SSP2", Y %in% c(2030, 2050), exemption== "None", Gini == "consistent", !startsWith(policy, "2C"), tech == "None")
 
 
 p <- F_plot_nishimori(pdata1, pdata2, v_width = c(1.2,1)) + Theme_tran
 p
 ggsave(p,filename = "2_hunger risk areacol.pdf", path = dir_plot_main,
        width = 8, height = 8, units = "cm")
 rm(p, pdata1, pdata2)
 
 
 
 
 
 
## 5.5 hunger risk: country level ------------------------------------------------------------------ 
source(paste0("../", prog_loc, "/prog/maintext/2_3_main_hunger risk country.R"))
 
 

# 6. Gini -----------------------------------------------------------------

df_Gini <- rgdx.param(AnaExp, "Gini_exp") %>%
  F_filter(lis_R=unique(.$R)) %>% F_woc() 



## 6.1 Gini change: regional----
pdata <- df_Gini %>% dplyr::rename("value" = "Gini_exp") %>% F_cha_decom() %>% 
  mutate(change = value - Baseline_None) %>% 
   filter( exemption== "None", Gini == "consistent", !startsWith(policy, "2C"), tech == "None", Y %in% c(2030, 2050), SSP == "SSP2") %>% 
  select(-c("Baseline_None", "value")) %>% # pivot_wider(names_from = "policy", values_from = "change") %>% 
  left_join(map_R)

 
p <- ggplot(pdata, aes(x = Y, y = change*100, fill = policy)) +
  geom_hline(yintercept = 0, color = "grey", alpha = .6) +
  # gghalves::geom_half_violin(width = .7) + 
  gghalves::geom_half_violin(data = pdata %>% filter(policy == "1.5C CP"), position = position_nudge(x = -.2, y = 0), width = .6, side = "l") + 
  gghalves::geom_half_violin(data = pdata %>% filter(policy != "1.5C CP"), position = position_nudge(x = .2, y = 0), width = .6, side = "r") + 
  labs(x = "Year", y = "Percentage point") +
  geom_boxplot(width = .2, outlier.size = .5, outlier.alpha = .3) + MyTheme + Theme_tran + theme(legend.position = "none", axis.text.x = element_text(angle = 0)) + 
  facet_wrap(~factor(R_CGE, levels = c(lis_R)), ncol = 3) +
  scale_fill_manual(values = palette_color_cp)
p
ggsave(p,filename = "2_Gini_regional.pdf", path = dir_plot_main,
       width = 14, height = 8, units = "cm")



## 6.2 Gini change: global ----
pdata <- df_Gini %>% dplyr::rename("value" = "Gini_exp") %>% F_cha_decom() %>% 
  mutate(change = value - Baseline_None) %>% 
  filter( exemption== "None", Gini == "consistent", !startsWith(policy, "2C"), tech == "None", 
          Y %in% c(2030, 2050), SSP == "SSP2") %>% 
  select(-c("Baseline_None", "value")) %>% # pivot_wider(names_from = "policy", values_from = "change") %>% 
  left_join(map_R) %>% filter(R_CGE == "World")


p <- ggplot(pdata, aes(x = Y, y = change*100, fill = policy)) +
  geom_hline(yintercept = 0, color = "grey", alpha = .6) +
  # gghalves::geom_half_violin(width = .7) + 
  gghalves::geom_half_violin(data = pdata %>% filter(policy == "1.5C CP"), position = position_nudge(x = -.2, y = 0), width = .6, side = "l") + 
  gghalves::geom_half_violin(data = pdata %>% filter(policy != "1.5C CP"), position = position_nudge(x = .2, y = 0), width = .6, side = "r") + 
  labs(x = "Year", y = "Percentage point") +
  geom_boxplot(width = .2, outlier.size = .5, outlier.alpha = .3) + MyTheme + Theme_tran + theme(legend.position = "none", axis.text.x = element_text(angle = 0)) + 
  # facet_wrap(~factor(R_CGE, levels = c(lis_R)), ncol = 3) +
  scale_fill_manual(values = palette_color_cp)
p
ggsave(p,filename = "2_Gini_wld.pdf", path = dir_plot_main,
       width = 5.6, height = 6, units = "cm")




# writing the data to excels
colnames(pdata)
pdata %>% dplyr::group_by(R_CGE, Y, SSP, policy, exemption, Gini, tech) %>% dplyr::reframe(mean = mean(change), median = median(change)) %>% 
  openxlsx::write.xlsx(file = paste0(dir_csv, "2_Gini_change_SSP_regional.xlsx"))


pdata %>% dplyr::group_by(R_CGE, Y, policy, exemption) %>% dplyr::reframe(mean = mean(change), median = median(change)) %>% 
  openxlsx::write.xlsx(file = paste0(dir_csv, "2_Gini_change_regional.xlsx"))


# legend
p <- ggplot(pdata, aes(x = Y, y = change*100, fill = policy)) +
  geom_hline(yintercept = 0, color = "grey", alpha = .6) +
  gghalves::geom_half_violin(width = .7) + labs(x = "Commodity", y = "Percentage point") +
  geom_boxplot(width = .2, outlier.shape = NA) + MyTheme + Theme_tran + 
  facet_grid(~factor(R_CGE, levels = c(lis_R))) + guides(fill = guide_legend(title = "Policy")) +
  scale_fill_manual(values = palette_color_cp)

p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
ggsave(p_leg,filename = paste0("2_Gini_legend.pdf"), path = dir_plot_main,
       width = 3, height = 2, units = "cm")
rm(pdata, p, p_leg)

rm(df_Gini)







# 7. calorie consumption --------------------------------------------------
df_fd <- df_iamc %>% filter(VEMF == "Foo_Dem", R == "World", Y %in% c(seq(2020, 2050, 10))) %>% F_woc() %>% F_filter_main() %>% filter(target != "2C", exemption == "None", SSP == "SSP2") %>% 
  F_cha_decom() %>% mutate(change = -(value-Baseline_None)/Baseline_None)

pdata <- df_fd %>% filter(R == "World")

p <- ggplot(pdata, aes(x = Y, y = change*100, color = policy)) +
  geom_line(aes(group = policy)) +
  geom_point(shape = 1, size = 2.5) +
  geom_hline(yintercept = 0, color = "grey", alpha = .6) +
  labs(x = "Year", y = "%") +
  MyTheme + Theme_tran + theme(legend.position = "none", axis.text.x = element_text(angle = 0)) + 
  scale_color_manual(values = palette_color_cp)
p

ggsave(p,filename = "2_FoodDemand_wld.pdf", path = dir_plot_main,
       width = 5.5, height = 6, units = "cm")

rm(df_fd, p, pdata)







# 4&5&6 Baseline in regions -------------------------------------------------
## the background map -------------------------------------------------
# create data for world coordinates using  
# map_data() function 
world_coordinates <- map_data("world") 

# create world map using ggplot() function 
p <- ggplot() + 
  # geom_map() function takes world coordinates  
  # as input to plot world map 
  geom_map( 
    data = world_coordinates, map = world_coordinates, 
    aes(long, lat, map_id = region), color = "grey50", fill = "white"
  ) + MyTheme + theme()

ggsave(p, filename = "2_backmap.pdf", path = dir_plot_main, height = 11, width = 20, units = "cm")


## the baseline trajectories -------------------------------------------------

### poverty headcount in the baseline and 1.5C CP scenarios ------
pdata1 <- Headcount %>% filter(Effect == "Expenditure", exemption == "None", Gini == "consistent", !startsWith(policy, "2C"), 
                               tech == "None", Y %in% seq(2020, 2050, 10), R != "World") %>% F_TH1() %>% 
  filter(TH == "$1.9 per capita per day", policy %in% c("Baseline", "1.5C CP")) %>% 
  mutate(Poverty = value/1000000, unit = "million") %>% 
  select(-value, -TH, -TH1)


### hunger risk in the baseline and 1.5C CP scenarios ------
pdata2 <- df_hug %>% filter(R != "World", exemption == "None", Y %in% seq(2020,2050,10), 
                            Gini == "consistent", policy %in% c("Baseline", "1.5C CP"), tech == "None") %>% 
  select(-VEMF) %>% dplyr::rename(Hunger = value)


### combining poverty and hunger risk in one df for plotting ------
pdata <- pdata1 %>% left_join(pdata2) %>% mutate_at(which(colnames(.) == "Hunger"), ~replace(., is.na(.), 0)) %>% 
  pivot_longer(c("Poverty", "Hunger"), names_to = "variable", values_to = "value")

rm(pdata1, pdata2)


### plotting poverty and hunger risk ------
lis_R_loop <- unique(pdata$R)
dir.create(paste0(dir_plot_main,"/2_poverty_n_hunger/"))

palette_color_povhung <- c("Hunger" = "#e89788", "Poverty" = "#645481")
# palette_linetype_povhung <- c("Hunger" = "#e89788", "Poverty" = "#645481")
palette_color_policy <- c("1.5C CP" = "#e76f51","Baseline" = "grey50")


if(sw_base_ribbon == F){
  for(r in 1:length(lis_R_loop)){
    
    # plot: poverty and hunger
    pdata0 <- pdata %>% filter(R == lis_R_loop[r]) %>% filter(SSP == "SSP2")
    p <- ggplot(pdata0, mapping = aes(x = Y, y = value, group = paste0(variable, policy), color = policy, linetype = variable)) +
      geom_hline(yintercept = 0, color = "grey") +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2, shape = 1) +  ggthemes::theme_base()  + 
      theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
      labs(x = "Year", y = "Million",subtitle = "Poverty & Hunger") + #facet_wrap(~R)  + 
      scale_color_manual(values = palette_color_policy) +
      guides(color=guide_legend(title='Policy',ncol = 1),
             fill=guide_legend(title='Policy',ncol = 1),
             linetype=guide_legend(title='Indicator',ncol = 1)) 
    ggsave(p,filename = paste0("/2_poverty_n_hunger/",lis_R_loop[r],".pdf"), path = dir_plot_main,
           width = 6.4, height = 7, units = "cm")
    
    # legend
    p <- p + theme(legend.position = "bottom") +
      guides(color=guide_legend(title='Policy',ncol = 2),
             fill=guide_legend(title='Policy',ncol = 2),
             linetype=guide_legend(title='Indicator',ncol = 2)) 
    
    p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
    ggsave(p_leg,filename =  paste0("/2_poverty_n_hunger/legend.pdf"), path = dir_plot_main,
           width = 16, height = 2, units = "cm")
    rm(p, pdata0, p_leg)
    
    
  }
}else{
  # for(r in 1:length(lis_R_loop)){
  #   
  #   pdata0 <- pdata %>% filter(R == lis_R_loop[r]) %>% mutate(v_group = variable) %>% select(-Ref)
  #   
  #   pdata1 <- pdata0 %>% select(-"v_group") %>% # filter(SSP != "SSP2") %>%
  #     mutate(name = paste0(SSP, "_", Gini, "_", tech)) %>% select(-SSP, -Gini, -tech) %>% 
  #     pivot_wider(names_from = "name", values_from = "value") %>% 
  #     mutate(v_min = apply(.[colnames(.)[startsWith(colnames(.), "SSP")]],1,min), 
  #            v_max = apply(.[colnames(.)[startsWith(colnames(.), "SSP")]],1,max))
  #   
  #   pdata2 <- pdata0 %>% filter(SSP == "SSP2")
  #   
  #   p <- ggplot()  + 
  #     geom_ribbon(data= pdata1, mapping = aes(x = Y, ymin=v_min,ymax=v_max, group = paste0(variable, policy), fill = variable), alpha=.1) +
  #     geom_line(data =  pdata2, mapping = aes(x = Y, y = value, group = paste0(variable, policy),color = variable, linetype = SSP, linewidth = SSP)) +
  #     geom_point(data =  pdata2, mapping = aes(x = Y, y = value, color = paste0(variable, policy), shape = tech), size = 2, shape = 1) +
  #     geom_hline(yintercept = 0, color = "grey") +
  #     labs(x = "Year", y = "Million", size = 9) +
  #     geom_abline(intercept = 0, slope = 0, color = "grey") +
  #     MyTheme + 
  #     guides(color=guide_legend(title='Indicator',ncol = 1),
  #            fill=guide_legend(title='Indicator',ncol = 1)) +
  #     scale_color_manual(values = palette_color_povhung) +
  #     scale_linetype_manual(values = palette_line_SSP) +
  #     scale_linewidth_manual(values = c("SSP2" = .6, "SSP1" = .4, "SSP3" = .4)) + 
  #     scale_x_discrete(breaks=seq(2010, 2100, 10)) +
  #     ggthemes::theme_base()  + theme(legend.position = "none") +
  #     labs(x = "Year", y = "Million") + facet_wrap(~R)
  #   p
  #   ggsave(p,filename = paste0("/2_poverty_n_hunger/",lis_R_loop[r],".pdf"), path = dir_plot_main,
  #          width = 7, height = 7, units = "cm")
  #   
  #   # legend
  #   p <- ggplot()  + 
  #     geom_ribbon(data= pdata1, mapping = aes(x = Y, ymin=v_min,ymax=v_max, group = variable, fill = variable), alpha=.1) +
  #     geom_line(data =  pdata2, mapping = aes(x = Y, y = value, group = v_group,color = variable)) +
  #     geom_point(data =  pdata2, mapping = aes(x = Y, y = value, color = variable, shape = tech), size = 2, shape = 1) +
  #     geom_hline(yintercept = 0, color = "grey") +
  #     labs(x = "Year", y = "Million", size = 9) +
  #     geom_abline(intercept = 0, slope = 0, color = "grey") +
  #     MyTheme + 
  #     guides(color=guide_legend(title='Indicator',ncol = 2),
  #            fill=guide_legend(title='Indicator',ncol = 2)) +
  #     # scale_linetype_manual(values = palette_line_SSP) +
  #     # scale_linewidth_manual(values = c("SSP2" = .6, "SSP1" = .4, "SSP3" = .4)) + 
  #     scale_color_manual(values = palette_color_povhung) +
  #     scale_x_discrete(breaks=seq(2010, 2100, 10)) +
  #     ggthemes::theme_base()  + theme(legend.position = "bottom") +
  #     labs(x = "Year", y = "Million") + facet_wrap(~R)
  #   
  #   p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
  #   ggsave(p_leg,filename =  paste0("/2_poverty_n_hunger/legend.pdf"), path = dir_plot_main,
  #          width = 8, height = 1.5, units = "cm")
  #   rm(p, pdata0, pdata1, pdata2, p_leg)
  #   
  # }
}

rm(pdata)




### Gini in the baseline and 1.5C CP scenarios ------
pdata3 <- df_Gini %>% dplyr::rename("value" = "Gini_exp") %>% 
  filter( exemption== "None", Gini == "consistent", policy %in% c("Baseline", "1.5C CP"),
          tech == "None", Y %in% seq(2020,2050,10), SSP == "SSP2") %>% 
  left_join(map_R)


# plotting: Gini
for(r in seq_along(lis_R_loop)){
  pdata <- pdata3 %>% filter(R_CGE == lis_R_loop[r]) %>% filter(SSP == "SSP2")    
  
  p <- ggplot(data = pdata) +
    geom_boxplot(mapping = aes(x = Y, y = value, fill = policy), width = .7, outlier.alpha = .5, outlier.size = .5 )+MyTheme +
    labs(x = "Year", y = "Percentage point", subtitle = "Gini") + 
    ggthemes::theme_base() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
    scale_fill_manual(values = palette_color_policy) +
    guides(fill=guide_legend(title='Policy',ncol = 2))
  p
  ggsave(p,filename = paste0("/2_poverty_n_hunger/",lis_R_loop[r],"_Gini.pdf"), path = dir_plot_main,
         width = 5.5, height = 7, units = "cm")
}


 
p <- p + theme(legend.position = "bottom") +
  guides(color=guide_legend(title='Policy',ncol = 2)) 

p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
ggsave(p_leg,filename =  paste0("/2_poverty_n_hunger/legend_Gini.pdf"), path = dir_plot_main,
       width = 8, height = 2, units = "cm")
rm(p, pdata, p_leg)

 
 
rm(df_iamc)
 
 
 rm(Headcount, Headcount_change, PoV, PoVExp_tmp, PoVInc_tmp, lis_R_loop)
 
 
 
print("The END of 2_main_GDP....R")



