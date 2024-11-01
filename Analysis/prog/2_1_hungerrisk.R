# This program is a summary of all the figures and plots in the paper 
# "Poverty and inequality in China under the long-term global temperature target"
# This program deals with hunger risk

# Shiya ZHAO, 2024/04/09




# 1. hunger risk data----------------------------------------------------------


df_hug <- df_iamc %>% filter(VEMF == "Pop_Ris_of_Hun") %>% mutate(unit = "million") %>% 
  left_join(MapScenario) %>% F_woc() %>% F_filter_main() %>% filter(target != "2C") %>% 
  F_filter(lis_R = lis_R) %>% select(-SCENARIO) %>% pivot_wider(names_from = "R", values_from = "value") %>% mutate(`R5OECD90+EU` = 0) %>% 
  pivot_longer(cols = c("World", colnames(.)[grepl("R5",colnames(.))]), names_to = "R", values_to = "value")




## line plot ---------------------------------------------------------------

pdata <- df_hug %>% F_filter_main() %>% filter(exemption == "None", target != "2C", Y %in% lis_Y) %>% select(-Ref) %>% mutate(v_group = paste0(SSP, policy, exemption, target), R = factor(R, level = c("World",lis_R_pov))) 

p_21a <- F_plot_ribbon(pdata, lab_y = "Million people") + Theme_tran + #theme(legend.position = "none") +
  facet_wrap(~R) +
  theme(axis.text.x = element_text(angle = 60))
p_21a


ggsave(p_21a,filename = "Figure 2_1a hungerisk.pdf", path = dir_fig,
       width = 20, height = 12, units = "cm")
rm(p_21a)



## regional area ---------------------------------------------------------------

lis_R_pov <- c("R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")


pdata <- df_hug %>% F_filter(lis_R = lis_R_pov, s_Baseline_woc = F) %>% F_cha_decom() %>% mutate(change = value-Baseline_None)

p_21b <- ggplot(pdata) +
  geom_area(aes(x = as.numeric(as.character(Y)), y = change, fill = factor(R, level = lis_R_pov) %>% fct_rev()), 
            position = "stack") +
  facet_wrap(policy~SSP) +
  MyTheme +
  labs(x = "Year", y = "Million people") +
  scale_fill_manual(values = palette_color_R5) +
  guides(fill = guide_legend(title = "Region"))
p_21b
ggsave(p_21b,filename = "Figure 2_1b hunger risk headcount area.pdf", path = dir_fig,
       width = 20, height = 8, units = "cm")
rm(p_21b)



## regional columns ---------------------------------------------------------------

lis_R_pov <- c("R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")


pdata <- df_hug %>% filter(exemption == "None", Y %in% lis_Y, R != "World", policy == "Baseline",SSP == "SSP2",
                              target != "2C") %>% F_filter_main() 

pdata <- df_hug %>% filter( exemption == "None", Y %in% lis_Y, R != "World", 
                            policy %in% c("Baseline", "1.5C CP","1.5C non-CP"),SSP == "SSP2") %>%  F_filter_main() 


p_2c2 <- ggplot(pdata) +
  geom_col(aes(x = as.character(Y), y = value/1000000, fill = factor(R, level = lis_R) ), 
           position = "fill", size = .7) +
  facet_wrap(~policy) +
  MyTheme + theme(axis.text.x = element_text(angle = 60)) +
  labs(x = "Year", y = "Share") +
  scale_fill_manual(values = palette_color_R5) +
  guides(fill = guide_legend(title = "Region"))
p_2c2
ggsave(p_2c2,filename = "Figure 2_1c hunger risk headcount fill.pdf", path = dir_fig,
       width = 20, height = 8, units = "cm")
rm(p_2c2)




## decomposition ---------------------------------------------------------------

lis_R_pov <- c("R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")


df_hug1 <- df_hug %>% filter(Y %in% c(seq(2020, 2050, 5))) %>% 
  F_filter(lis_R = lis_R, s_Baseline_woc = F) %>% F_cha_decom() %>% mutate(change = value-Baseline_None) %>% 
  select("R","Y","unit","SSP","policy","exemption", "change")

df_hug_cp <- df_hug1 %>% filter(policy == "1.5C CP") %>% select(-policy) %>% dplyr::rename(change_cp=change)
df_hug_ncp <- df_hug1 %>% filter(policy == "1.5C non-CP") %>% select(-policy) %>% dplyr::rename(change_ncp=change)

df_hug_decom <- df_hug_cp %>% left_join(df_hug_ncp) %>% mutate(`Transition` = `change_ncp`, `Carbon price` = `change_cp`-`change_ncp`) %>% 
  select("R", "Y", "unit", "SSP", "exemption","Transition", "Carbon price") %>% pivot_longer(cols = c("Transition", "Carbon price"),
                                                                                             values_to = "change", names_to = "Effect")


### regional
pdata <- df_hug_decom %>% filter(R %in% lis_R_pov)

p_21c <- ggplot(pdata) +
  geom_col(aes(x = Y, y = change, fill = Effect), 
            position = "stack") +
  facet_wrap(SSP~R, ncol = 4) +
  MyTheme + theme(axis.text.x = element_text(angle = 60)) +
  labs(x = "Year", y = "Million people") +
  # scale_fill_manual(values = palette_color_R5) +
  guides(fill = guide_legend(title = "Effect"))
p_21c
ggsave(p_21c,filename = "Figure 2_1c hunger risk headcount decomposition_area.pdf", path = dir_fig,
       width = 20, height = 12, units = "cm")
rm(p_21c)


### global
pdata <- df_hug_decom %>% filter(R == "World")

p_21d <- ggplot(pdata) +
  geom_col(aes(x = Y, y = change, fill = SSP, alpha = SSP), 
           position = "dodge") +
  facet_wrap(~Effect, ncol = 4) +
  MyTheme + theme(axis.text.x = element_text(angle = 60)) +
  labs(x = "Year", y = "Million people") +
  scale_fill_manual(values = palette_color_SSP) +
  scale_alpha_manual(values = palette_alpha_SSP) +
  guides(fill = guide_legend(title = "SSP"))
p_21d
ggsave(p_21d,filename = "Figure 2_1d hunger risk headcount decomposition_world.pdf", path = dir_fig,
       width = 14, height = 7, units = "cm")
rm(p_21d)

