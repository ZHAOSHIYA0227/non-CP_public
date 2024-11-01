# This program is a summary of all the figures and plots in the paper 
# "Poverty and inequality in China under the long-term global temperature target"
# This program deals with Expenditure and its change

# Shiya ZHAO, 2021/06/17

## Loading packages ----
require(gdxrrw)
require(ggplot2)
require(tidyverse)
require(cowplot)
require(gdata)
require(grid)
require(Rmisc)
require(ggpmisc)


# 0. data loading ----
rm(lis_y)
lis_y <- c(seq(2020,2050,10))



if(sw_decile == "Rdata"){
  load(paste0("../../DataArchive/PHIoutput/gdx/Decile.Rdata")) 
  df_Exp_ALL <- df_Exp_ALL %>% dplyr::rename(Ref = "scenario", con = "consumption")
  
  # budget
  
  df_budget <- df_Exp_ALL %>% select(R, Y, Ref,DEC, budget) %>% 
    gdata::rename.vars(colnames(.), c("R", "Ref", "DEC", "Y", "budget")) %>% 
    distinct() %>%
    filter( Ref %in% lis_ref_all,
            Y %in% lis_y)  %>%
    F_woc() %>% F_filter_main() 
  
  
  
  # consumption
  
  Con <-  df_Exp_ALL %>% select(R, Y, Ref,DEC, I, con) %>%
    distinct() %>%
    filter( Ref %in% lis_ref_all,
            Y %in% lis_y) %>%
    left_join(MapI) %>%
    select(-"I") %>% dplyr::rename(I = "I_abb") %>%
    F_woc() %>% F_filter_main() 
  
  Con_Baseline <- Con %>%
    filter(policy == "Baseline") %>%
    dplyr::rename(con_base = "con") %>%
    select(R, Y, DEC, I, SSP, con_base)

  
  
  # expenditure 
  
  Exp <-  df_Exp_ALL %>%  mutate(exp = con * PQchange) %>% select(R, Y, Ref,DEC, I, exp) %>%
    distinct() %>%
    filter( Ref %in% lis_ref_all,
            Y %in% lis_y) %>%
    left_join(MapI) %>%
    select(-"I") %>% dplyr::rename(I = "I_abb") %>%
    F_woc() %>% F_filter_main() 
  
  Exp_Baseline <- Exp %>%
    filter(policy == "Baseline") %>% F_filter_main() %>%
    dplyr::rename(exp_base = "exp") %>%
    select(R, Y, DEC, I, SSP, exp_base)
  
  
  # price change
  PQ <- df_Exp_ALL %>% select(R, Y, Ref,I,PQchange) %>%
    distinct() %>%
    filter(Ref %in% lis_ref_all,
           Y %in% lis_y) %>% 
    left_join(MapI) %>%
    select(-c('I')) %>%
    dplyr::rename(I = "I_abb") %>%
    transform(I = factor(I,levels = lis_I_abb)) %>%
    F_woc() %>% F_filter_main() 
  
  Price_Baseline <- PQ %>%
    filter(policy == "Baseline") %>%
    dplyr::rename(PQ_base = "PQchange") %>%
    select(R, Y, I, SSP, PQ_base)
  
  
}else if(sw_decile == "gdx"){
  dir_decile <- paste0("../../DataArchive/PHIoutput/gdx/Decile.gdx")
  
  # budget
  df_budget <- rgdx.param(dir_decile, "output_Budget")[,-1]%>% 
    gdata::rename.vars(colnames(.), c("R", "Ref", "DEC", "Y", "budget")) %>% 
    distinct() %>%
    filter( Ref %in% lis_ref_all,
            Y %in% lis_y)  %>%
    F_woc() %>% F_filter_main() 
  
  # consumption
  Con <-  rgdx.param(dir_decile, "output_Con_I")[,-1] %>% 
    gdata::rename.vars(colnames(.), c("R", "Ref", "DEC", "I", "Y", "con")) %>% 
    distinct() %>%
    filter( Ref %in% lis_ref_all,
            Y %in% lis_y) %>%
    left_join(MapI) %>%
    select(-"I") %>% dplyr::rename(I = "I_abb") %>%
    F_woc() %>% F_filter_main() 
  
  Con_Baseline <- Con %>%
    filter(policy == "Baseline") %>%
    dplyr::rename(con_base = "con") %>%
    select(R, Y, tech, Gini, DEC, I, SSP, con_base)
  
  
  # expenditure
  Exp <- rgdx.param(dir_decile, "output_Exp_I")[,-1] %>% 
    gdata::rename.vars(colnames(.), c("R", "Ref", "DEC", "I", "Y", "Exp")) %>% 
    distinct() %>%
    filter( Ref %in% lis_ref_all,
            Y %in% lis_y) %>%
    left_join(MapI) %>%
    select(-"I") %>% dplyr::rename(I = "I_abb") %>%
    F_woc() %>% F_filter_main() 
  
  
  
  # price change
  PQ <- rgdx.param(dir_decile, "output_Price_I")[,-1] %>% 
    gdata::rename.vars(colnames(.), c("R", "Ref", "I", "Y", "PQchange")) %>% 
    distinct() %>%
    filter(Ref %in% lis_ref_all,
           Y %in% lis_y) %>% 
    left_join(MapI) %>%
    select(-c('I')) %>%
    dplyr::rename(I = "I_abb") %>%
    transform(I = factor(I,levels = lis_I_abb)) %>%
    F_woc() %>% F_filter_main() 
  
  Price_Baseline <- PQ %>%
    filter(policy == "Baseline") %>%
    dplyr::rename(PQ_base = "PQchange") %>%
    select(R, Y, tech, Gini, I, SSP, PQ_base)

}



# 1. Total EV calculation ----
df_nominator <- Con %>% left_join(Con_Baseline) %>% left_join(Price_Baseline) %>% filter(target != "Baseline") %>%
  mutate(nominator = con * PQ_base - con_base * PQ_base) #%>%
df_denominator <- df_nominator %>% dplyr::group_by(R, Y, Ref, DEC) %>%
  reframe(denominator = sum(con_base * PQ_base))

df_EVi <- df_nominator %>% left_join(df_denominator) %>% mutate(EVi = nominator/denominator) %>% ungroup() %>%
  select("R", "Y", "Ref", "DEC","I", "policy", "SSP", "exemption", "target", "Gini", "tech", "EVi" ) %>% distinct()

rm(df_nominator, df_denominator)


df_EV <- Con %>% left_join(Con_Baseline) %>% left_join(Price_Baseline) %>% filter(target != "Baseline") %>%
  dplyr::group_by(R, Y, Ref, DEC) %>%
  reframe(EV = sum(con * PQ_base - con_base * PQ_base)/sum(con_base * PQ_base)) %>% F_woc() %>% distinct()

rm(EV_total_tmp, EVi_total, df_Exp_ALL)
lis_y <- c('2020','2030','2040','2050')


# 2.1. plot EV total -------------------------------------------------------------

csvEV <- df_EV %>%
  write.csv(file = paste0(dir_output,"3_EVtotal.csv"))


csvEVi <- df_EVi %>%
  write.csv(file = paste0(dir_output,"3_EVdecom.csv"))


## Fig 1 EV total effect ----
pdata <- df_EV %>% filter(Y %in% c('2030','2050')) %>% F_filter_main() %>% filter(target != "2C", exemption == "None") %>% left_join(map_R)

p_3a1 <- ggplot() +
  geom_boxplot(pdata %>% filter(SSP == "SSP2", DEC != "ALL", exemption == "None"),
               mapping = aes(x = DEC, y = EV*100,
                             color = policy, group = paste0(policy, DEC, SSP, exemption)), outlier.alpha = 0.4, outlier.size = 0.5) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(x = "Income decile", y = "Equivalent variation (%)" ) +
  facet_grid(R_CGE~Y) +
  MyTheme +
  guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE)) +
  scale_color_manual(values = palette_color_cp)

p_3a1

ggsave(p_3a1,filename = "Figure 3a1 EV.pdf", path = dir_fig,
       width = 20, height = 20, units = "cm")

# Fig 2 budget change by deciles ----
pdata <- df_budget %>% dplyr::rename(value = "budget") %>% F_filter_main()  %>% filter(target != "2C", exemption == "None") %>% F_cha_decom() %>%
  mutate(change = (value-Baseline_None)/Baseline_None) %>% left_join(map_R) %>% filter(DEC != "ALL", Y %in% c(2030, 2050))

p_3b1 <- ggplot(pdata %>% filter(exemption == "None"),
                  mapping = aes(x = DEC, y = change * 100,
                                color = policy, group = paste0(policy, DEC))) +
  geom_boxplot(outlier.shape = NA, width = 0.7, linewidth = 0.5) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(x = "Income decile",
    y = "Change in household budget (%)" ) +
  facet_grid(R_CGE~Y) +
  MyTheme +
  guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE))  +
  scale_color_manual(values = palette_color_cp)

p_3b1

ggsave(p_3b1,filename = "Figure 3b1 change in budget.pdf", path = dir_fig,
       width = 18, height = 24, units = "cm")



## Fig 2.1 decomposition ----


# 3. EVi ----
## Fig 3 EVi
pdata <- filter(df_EVi,
                Y %in% c('2030','2040','2050')) %>% F_filter_main()  %>% filter(target != "2C", exemption == "None") %>%
  select("R","Y", "DEC", "I", "policy", "exemption","tech","exemption", "target", "SSP", "EVi") %>%
   filter(DEC %in% c("1", "10")) %>% pivot_wider(names_from = "DEC", values_from = "EVi") %>%
  mutate(`1`=`1`*100, `10` = `10`*100)


p_3c1 <- ggplot(data = pdata %>% filter(SSP == "SSP2"),
               mapping = aes(x = `1`, y = `10`,
                             color = policy, size = abs(`10`/`1`), shape = exemption
                             )) +
  geom_point(alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, colour = 'grey', linetype = "dashed") +
  geom_hline(yintercept = 0, colour = 'grey') +
  geom_vline(xintercept = 0, colour = 'grey') +
  labs(x = "EV in the 1st decile (%)", y = "EV in the 10th decile (%)")+
  facet_wrap(~(factor(I,levels = lis_I_abb)), scales = 'free') +
  MyTheme +
  scale_color_manual(values = palette_color_cp) +
  scale_shape_manual(values = c("None" = 16, "Direct tax exemption" = 4)) +
  scale_size(guide = 'none') +
  guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE),
         shape = guide_legend(title = 'Tax exemption', ncol = 1, byrow = TRUE))

p_3c1

ggsave(p_3c1,filename = "Figure 3c1 EVi_point.pdf", path = dir_fig,
       width = 24, height = 20, units = "cm")






## regional
dir.create(paste0(dir_fig,"/Figure 3c1 EVi_point/"))
lis_R_loop <- unique(map_R$R_CGE)
for(r in 1:length(lis_R_loop)){
  pdata <- filter(df_EVi,
                  Y %in% c('2030','2040','2050')) %>% F_filter_main() %>%
    left_join(map_R) %>% 
    filter(R_CGE == lis_R_loop[r]) %>% 
    filter(target != "2C",  SSP == "SSP2") %>%
    select("R_CGE", "R","Y", "DEC", "I", "policy", "exemption","tech","exemption", "target", "SSP", "EVi") %>%
    filter(DEC %in% c("1", "10")) %>% pivot_wider(names_from = "DEC", values_from = "EVi") %>%
    mutate(`1`=`1`*100, `10` = `10`*100)

  p_3c1 <- ggplot(data = pdata %>% filter(SSP == "SSP2"),
                  mapping = aes(x = `1`, y = `10`,
                                color = policy, size = abs(`10`/`1`), shape = exemption
                  )) +
    geom_point(alpha = 0.4) +
    geom_abline(intercept = 0, slope = 1, colour = 'grey', linetype = "dashed") +
    geom_hline(yintercept = 0, colour = 'grey') +
    geom_vline(xintercept = 0, colour = 'grey') +
    labs(subtitle = lis_R_loop[r], x = "EV in the 1st decile (%)", y = "EV in the 10th decile (%)")+
    facet_wrap(~(factor(I,levels = lis_I_abb)), scales = 'free') +
    MyTheme +
    scale_color_manual(values = palette_color_cp) +
    scale_shape_manual(values = c("None" = 16, "Direct tax exemption" = 4)) +
    scale_size(guide = 'none') +
    guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE),
           shape = guide_legend(title = 'Tax exemption', ncol = 1, byrow = TRUE))

  p_3c1


ggsave(p_3c1,filename = paste0("/Figure 3c1 EVi_point/",lis_R_loop[r],".pdf"), path = dir_fig,
       width = 24, height = 20, units = "cm")

}
rm( pdata,p)








# stacking EVi in R5REF
for(r in 1:length(lis_R_loop)){
  pdata <- filter(df_EVi,
                  Y %in% c('2030')) %>% F_filter_main() %>%
    left_join(map_R) %>%
    filter(R_CGE == lis_R_loop[r], exemption == "None") %>%
    select("R_CGE", "R","Y", "DEC", "I", "policy", "exemption", "SSP", "EVi")



  p_3c1 <- ggplot(data = pdata,
                  mapping = aes(x = DEC, y = EVi * 100,color = policy, shape = exemption)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, colour = 'grey') +
    labs(x = "Income deciles", y = "EV by commodity")+
    facet_wrap(~(factor(I,levels = lis_I_abb))) +
    MyTheme +
    scale_color_manual(values = palette_color_cp) +
    # scale_size(guide = 'none') +
    guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE))

  p_3c1


  ggsave(p_3c1,filename = paste0("/Figure 3c1 EVi_point/box_",lis_R_loop[r], "_2030.pdf"), path = dir_fig,
         width = 24, height = 20, units = "cm")


  colnames(pdata)
  p <- ggplot(data = pdata %>% dplyr::group_by(R_CGE, R, Y, DEC, policy, exemption, SSP) %>% reframe(EV = sum(EVi)),
                  mapping = aes(x = DEC, y = EV*100,color = policy, shape = exemption)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, colour = 'grey') +
    labs(x = "Income deciles", y = "EV by commodity")+
    MyTheme +
    scale_color_manual(values = palette_color_cp) +
    # scale_size(guide = 'none') +
    guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE))

  p


  ggsave(p,filename = paste0("/Figure 3c1 EVi_point/box_EV_",lis_R_loop[r], "_2030.pdf"), path = dir_fig,
         width = 12, height = 10, units = "cm")
}

rm( pdata,p_3c1)



# 4. Gini -----------------------------------------------------------------


df_Gini <- rgdx.param(AnaExp, "Gini_exp") %>%
  F_filter(lis_R = lis_R_phi) %>% F_woc() 

## Fig 4.1 Baseline Gini ----
pdata <- df_Gini %>% filter(Y %in% c('2030','2040','2050'), target == "Baseline", exemption == "None") %>% F_filter_main() %>% left_join(map_R) %>% 
  select("R", "R_CGE", "Y", "Gini_exp", "policy", "SSP")

pdata1 <- pdata %>% dplyr::group_by(R_CGE, Y, policy, SSP) %>% reframe(value = median(Gini_exp)) %>% 
  pivot_wider(names_from = "SSP", values_from = "value") %>% 
  mutate(v_min = apply(.[colnames(.)[startsWith(colnames(.), "SSP")]],1,min), v_max = apply(.[colnames(.)[startsWith(colnames(.), "SSP")]],1,max))

p_3d1 <- ggplot() + 
  geom_boxplot(data = pdata %>% filter(SSP == "SSP2"),
               mapping = aes(x = Y, y = Gini_exp*100, color = Y), width = 0.3, linewidth = 0.3, outlier.alpha = 0.4, outlier.size = 0.5) +
  geom_ribbon(pdata1,
              mapping = aes(x = Y, ymin=v_min*100,ymax=v_max*100, group = policy), fill = "#34a0a4", alpha=0.2) +
  geom_hline(yintercept = 0, colour = 'grey') +
  geom_vline(xintercept = 0, colour = 'grey') +
  labs(x = "Region", y = "Percentage point")+
  facet_wrap(~factor(R_CGE, levels = lis_R), ncol = 3) +
  MyTheme + theme(axis.text.x = element_text(angle = 0)) +
  scale_color_manual(values = palette_y) +
  guides(color = guide_legend(title = 'Year', ncol = 1, byrow = TRUE))  #+

p_3d1
ggsave(p_3d1,filename = "Figure 3d1 Gini baseline.pdf", path = dir_fig,
       width = 14, height = 8, units = "cm")



## Fig 4.2 Gini change----

pdata <- df_Gini %>% filter(Y %in% c('2030','2040','2050'), exemption == "None", target != "2C")  %>% F_filter_main() %>% 
  dplyr::rename(value = "Gini_exp") %>% F_cha_decom() %>% 
  mutate(change = (value - Baseline_None)) %>% left_join(map_R) %>% select(-value) %>% 
  filter(R_CGE != "WLD")
  
pdata1 <- pdata %>% dplyr::rename(value = change) %>% dplyr::group_by(R_CGE, Y, policy, SSP) %>% reframe(value = median(value)) %>% 
  pivot_wider(names_from = "SSP", values_from = "value") %>% 
  mutate(v_min = apply(.[colnames(.)[startsWith(colnames(.), "SSP")]],1,min), v_max = apply(.[colnames(.)[startsWith(colnames(.), "SSP")]],1,max))




p_3d2 <- ggplot() + 
  geom_boxplot(data = pdata %>% filter(SSP == "SSP2"),
               mapping = aes(x = Y, y = change*100, color = policy), width = 0.5, linewidth = 0.3, outlier.alpha = 0.4, outlier.size = 0.5) +
  geom_ribbon(pdata1,
              mapping = aes(x = Y, ymin=v_min*100,ymax=v_max*100, group = policy, fill = policy), alpha=0.3) + # fill = "#34a0a4", 
  geom_hline(yintercept = 0, colour = 'grey') +
  geom_vline(xintercept = 0, colour = 'grey') +
  labs(x = "Region", y = "Percentage point")+
  facet_wrap(~factor(R_CGE, levels = lis_R), ncol = 3) +
  MyTheme + theme(axis.text.x = element_text(angle = 0)) +
  scale_color_manual(values = palette_color_cp) +
  scale_fill_manual(values = palette_color_cp) +
  guides(color = guide_legend(title = 'Policy', ncol = 1, byrow = TRUE),
         fill = guide_legend(title = 'Policy', ncol = 1, byrow = TRUE))

p_3d2
ggsave(p_3d2,filename = "Figure 3d2 Gini change.pdf", path = dir_fig,
       width = 14, height = 8,units = "cm")


 
# 5. Change of Gini, comparing with/without direct taxation ----
## Fig 5.1 
pdata <- df_Gini %>% filter(Y %in% c('2030','2040','2050')) %>% dplyr::rename(value = "Gini_exp") %>% F_cha_decom() %>% 
  mutate(change = (value - Baseline_None)/Baseline_None*100) %>% left_join(map_R) %>% select(-value, -Baseline_None) %>% 
  filter(R_CGE != "WLD") %>% filter(policy == "1.5C CP") %>% 
  pivot_wider(names_from = "exemption", values_from = "change")

dir.create(paste0(dir_fig, "/Figure3e1 Gini change/"))
lis_SSP <- c("SSP1", "SSP2", "SSP3")
for(s in 1:length(lis_SSP)){
p_3e1 <- ggplot(data = pdata %>% filter(SSP == lis_SSP[s]) ,
                mapping = aes(x = None, y = `Direct tax exemption`)) + 
  geom_point(color = "#f8b976") +
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = 'grey') +
  geom_vline(xintercept = 0, colour = 'grey') +
  labs(x = "Change in Gini with direct tax exemption (percentage point)", 
       y = "Change in Gini without direct tax exemption (percentage point)",
       title = lis_SSP[s])+
  facet_wrap(~R_CGE, ncol = 3) +
  MyTheme +
  stat_poly_line(alpha = 0.3) +
  stat_poly_eq(use_label(c("eq")), label.y = 0.95, label.x = 0.2) +
  stat_poly_eq(use_label(c("adj.R2")), label.y = 0.85, label.x = 0.2) +
  guides(color = guide_legend(title = 'Year', ncol = 1, byrow = TRUE))

p_3e1
ggsave(p_3e1,filename = paste0("Figure3e1 Gini change/",lis_SSP[s],".pdf"), path = dir_fig,
       width = 18, height = 15, units = "cm")
}
rm(s, pdata, p_3e1, p_3d1, p_3d2, p_3c1, p_3b1, p_3a1)






# 6. Expenditure ----------------------------------------------------------

pdata <- Exp %>% filter(Y %in% c(2030)) %>% F_filter_main() %>% left_join(map_R) %>% filter(target != "2C", policy != "Baseline non-CP") %>% 
  dplyr::group_by(R,Y,Ref,DEC,policy,SSP, Gini, tech, exemption, target, R_CGE) %>% 
  reframe(tot = sum(Exp), share = Exp/tot, I = factor(I, levels = lis_I_abb))
# mean(pdata$share)
# median(pdata$share)
colnames(pdata)


p <- ggplot(pdata) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_boxplot(aes(x = I, y = share * 100, color = policy), outliers = F) +
  facet_wrap(~ R_CGE,  scales = "free") + MyTheme + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
  labs(x = "Region", y = "Expenditure") +
  scale_color_manual(values = palette_color_cp) + guides(color = guide_legend(title = "Policy"))
p

ggsave(p,filename = paste0("Figure 3f1 Exp share 2030.pdf"), path = dir_fig, width = 35, height = 20, units = "cm")



print("END 3_EVGini.R")
