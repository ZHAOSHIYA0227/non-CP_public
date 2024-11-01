# This program is a summary of all the figures and plots in the paper 
# "Poverty and inequality in China under the long-term global temperature target"
# This program deals with Poverty headcounts and relative poverty

# Shiya ZHAO, 2021/06/17

## Loading packages ----
library(gdxrrw)
library(ggplot2)
library(tidyverse)
# library(cowplot)
library(gdata)
library(grid)
library(Rmisc)

getwd()# 


# World development database 
wdi <- "../../../model/data/WDI/Data_Extract_From_World_Development_Indicators.xlsx"
wdi <- "../../data/WDI/Data_Extract_From_World_Development_Indicators.xlsx"



# ## Fig 2a Historical poverty headcount under the three thresholds ----
# Y_pov <- c(seq(2005,2020,1))
# Y_pov1 <- c(seq(2005,2016,1))
# 
# His_pop <- readxl::read_xlsx(wdi,sheet = 'population',
#                     na = c('NA','#DIV/0!','..'))[1,]
# His_povrate <- readxl::read_xlsx(wdi,sheet = 'povertyrate',
#                         na = c('NA','#DIV/0!','..'))[1:4,]
# 
# His_povheadcount <- readxl::read_xlsx(wdi,sheet = 'povertyheadcount',
#                              na = c('#value!','#VALUE!','NA','#DIV/0!','..')) %>%
#   rename.vars(from = colnames(.)[3:18], to =  c(seq(2005,2020,1)))
# 
# 
# df_his_povheadcount <- His_povheadcount %>%
#   # na.omit() #%>%
#   mutate_all(~replace(., is.na(.), 0)) %>%
#   gather(Y, value,as.character(seq(2005,2020,1)))  %>%
#   rename.vars(from = 'poverty headcount', to = 'TH0') %>%
#   mutate(TH = recode(TH0,
#                      '1.9' = lis_TH[1],
#                      '3.2' = lis_TH[2],
#                      '5.5' = lis_TH[3],
#                      'national' = 'national')) %>%
#   transform(Y = factor(Y, levels = c(seq(2005,2020,1)))) %>%
#   filter(value != 0)
# 
# 
# ## Plot ----
# pdata <- df_his_povheadcount %>%
#   mutate(TH= recode(.$TH,"1.9-threshold" = "$1.9",
#                 "3.2-threshold" = "$3.2",
#                 "5.5-threshold" = "$5.5",
#                 "1.9-national" = "national"))
# 
# p_2a <- ggplot(data =  pdata, 
#                mapping = aes(x = Y, y = as.numeric(value)/1000000, 
#                              group = TH, colour = TH)) + 
#   geom_line() +
#   labs(title = "a)", #  Poverty headcount historical trend
#        x = "Year",
#        y = 'Million people',
#        size=9)+
#   #facet_wrap(~TH1,scales = "free") +
#   MyTheme  +
#   theme(legend.position = c(1, 1),
#         legend.justification = c("right", "top"),
#         legend.box.just = "right",
#         legend.margin = margin(1, 1, 1, 1)) +
#   guides(colour = guide_legend(title = 'Thresholds', ncol = 1, byrow = TRUE, size = 7)) +
#   scale_x_discrete(breaks=seq(2000, 2020, 5),drop = FALSE)
# p_2a
# 
# 

## Fig 2b Poverty headcount  ----
# Poverty headcount ---
PoVExp_tmp <- rgdx.param(AnaExp, "PoVExp") %>%
  # left_join(MapScenario) %>% 
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  filter(!is.na(Ref), R %in% lis_R,
           # Ref %in% lis_ref,
         Y%in% lis_Y)%>%
  mutate(Effect = 'Expenditure') 


PoVInc_tmp <- rgdx.param(AnaInc, "PoV") %>%
  # left_join(MapScenario) %>% 
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  filter(!is.na(Ref), R %in% lis_R,
         Y %in% lis_Y) %>%
  mutate(Effect = 'Income') 


# c <-PoVExp_tmp


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



## Plot ----
pdata <- filter(Headcount,
                TH1 %in% c('1.9-threshold'),
                Y %in% c(lis_Y), target != "2C",
                exemption == "None",
                Effect == 'Expenditure') %>% select(-Ref) %>% F_filter_main() %>% 
  mutate(v_group =paste0(policy, SSP, exemption), value = value/1000000)

p_2b <- F_plot_ribbon(pdata, lab_y = "Million people") + facet_wrap(~R) +
  theme(axis.text.x = element_text(angle = 60))

ggsave(p_2b,filename = "Figure 2b Poverty headcount line.pdf", path = dir_fig,
       width = 20, height = 12, units = "cm")
rm(p_2b)


# Fig 2c1 poverty stacking global ------------------------------------------
pdata <- Headcount %>% filter(Effect == "Expenditure", exemption == "None", Y %in% lis_Y, R != "World", policy == "Baseline",SSP == "SSP2",
                              target != "2C") %>% 
  F_filter_main() %>% F_TH1()

lis_R_pov <- c("R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")
p_2c1 <- ggplot(pdata) +
  geom_area(aes(x = as.numeric(as.character(Y)), y = value/1000000, fill = factor(R, level = lis_R_pov) %>% fct_rev()), 
            position = "stack") +
  facet_wrap(~TH) +
  MyTheme +
  labs(x = "Year", y = "Poverty headcount (million people)") +
  scale_fill_manual(values = palette_color_R5) +
  guides(fill = guide_legend(title = "Region"))
p_2c1
ggsave(p_2c1,filename = "Figure 2c1 Poverty headcount stack.pdf", path = dir_fig,
       width = 20, height = 8, units = "cm")
rm(p_2c1)

# Fig 2c2 poverty stacking global column ------------------------------------------
# baseline
pdata <- Headcount %>% filter(Effect == "Expenditure", exemption == "None", Y %in% lis_Y, R != "World", policy == "Baseline",target != "2C") %>% 
  F_filter_main() %>%  F_TH1()

p_2c2 <- ggplot(pdata) +
  geom_col(aes(x = as.character(Y), y = value/1000000, fill = factor(R, level = lis_R) ), 
            position = "fill", size = .7) +
  facet_wrap(~TH) +
  MyTheme + theme(axis.text.x = element_text(angle = 60)) +
  labs(x = "Year", y = "Share") +
  scale_fill_manual(values = palette_color_R5) +
  guides(fill = guide_legend(title = "Region"))
p_2c2
ggsave(p_2c2,filename = "Figure 2c2 Poverty headcount fill_SSP2base.pdf", path = dir_fig,
       width = 20, height = 8, units = "cm")
rm(p_2c2)



# 1.5C CP
rm(pdata,p)
pdata <- Headcount %>% filter(Effect == "Expenditure", exemption == "None", Y %in% lis_Y, R != "World", policy == "1.5C CP",SSP == "SSP2") %>% F_filter_main() %>% F_TH1()

p_2c2 <- ggplot(pdata) +
  geom_col(aes(x = as.character(Y), y = value/1000000, fill = factor(R, level = lis_R) ), 
           position = "fill", size = .7) +
  facet_wrap(~TH) +
  MyTheme + theme(axis.text.x = element_text(angle = 60)) +
  labs(x = "Year", y = "Share") +
  scale_fill_manual(values = palette_color_R5) +
  guides(fill = guide_legend(title = "Region"))
p_2c2
ggsave(p_2c2,filename = "Figure 2c2 Poverty headcount fill_1.5CP.pdf", path = dir_fig,
       width = 20, height = 8, units = "cm")
rm(p_2c2)





# 1.5C non-CP
rm(pdata,p)
pdata <- Headcount %>% filter(Effect == "Expenditure", exemption == "None", Y %in% lis_Y, R != "World", policy == "1.5C non-CP",SSP == "SSP2") %>% F_filter_main() %>% F_TH1()

p_2c2 <- ggplot(pdata) +
  geom_col(aes(x = as.character(Y), y = value/1000000, fill = factor(R, level = lis_R) ), 
           position = "fill", size = .7) +
  facet_wrap(~TH) +
  MyTheme + theme(axis.text.x = element_text(angle = 60)) +
  labs(x = "Year", y = "Share") +
  scale_fill_manual(values = palette_color_R5) +
  guides(fill = guide_legend(title = "Region"))
p_2c2
ggsave(p_2c2,filename = "Figure 2c2 Poverty headcount fill_1.5nonCP.pdf", path = dir_fig,
       width = 20, height = 8, units = "cm")
rm(p_2c2)






# 1.5C non-CP
rm(pdata,p)
pdata <- Headcount %>% filter(Effect == "Expenditure", exemption == "None", Y %in% lis_Y, R != "World", policy %in% c("Baseline", "1.5C CP","1.5C non-CP"),SSP == "SSP2") %>% 
  F_TH1() %>% F_filter_main() %>% 
  filter(TH1 == "1.9-threshold")

p_2c2 <- ggplot(pdata) +
  geom_col(aes(x = as.character(Y), y = value/1000000, fill = factor(R, level = lis_R) ), 
           position = "fill", size = .7) +
  facet_wrap(~policy) +
  MyTheme + theme(axis.text.x = element_text(angle = 60)) +
  labs(x = "Year", y = "Share") +
  scale_fill_manual(values = palette_color_R5) +
  guides(fill = guide_legend(title = "Region"))
p_2c2
ggsave(p_2c2,filename = "Figure 2c2 Poverty headcount fill_region.pdf", path = dir_fig,
       width = 20, height = 8, units = "cm")
rm(p_2c2)










# Fig 2d Poverty headcount by Effect ----

## load data ---- 
# income side
Headcount_Inc <- Headcount %>%
  filter(Effect == 'Income', exemption == "None") %>%
  F_cha_decom(lis_del = c("target", "Ref", "policy", "exemption", "Effect")) %>%
  mutate(`Income` = value - Baseline_None, unit = "person") %>% 
  select(R, Y, TH1, SSP, Gini, tech, policy, `Income`, unit)


# expenditure side
Headcount_Exp <- Headcount %>% filter(Effect == 'Expenditure') %>%
  F_cha_decom(lis_del = c("target", "Ref", "policy", "exemption", "Effect")) %>%
  mutate(`Indirect price` = value - Baseline_None, unit = "person") %>% 
  select(R, Y, TH1, SSP, Gini, tech, policy, exemption, `Indirect price`, unit)


# non-CPs
Headcount_Exp_woc <- Headcount_Exp %>% filter(policy == "1.5C non-CP") %>% 
  select(-exemption) %>% mutate(`Direct tax` = 0)


# CP
Headcount_Exp_wc <- Headcount_Exp %>% filter(!grepl("non-CP", policy)) %>% 
  pivot_wider(names_from = "exemption", values_from = "Indirect price") %>% 
  mutate(`Direct tax` = `None`-`Direct tax exemption`) %>% select(-c("None")) %>% 
  dplyr::rename("Indirect price" = "Direct tax exemption")

# combine CP with non-CPs
Headcount_Exp <- Headcount_Exp_woc %>% rbind(Headcount_Exp_wc)

rm(Headcount_Exp_wc, Headcount_Exp_woc)


# combine expenditure side effects with income side effects
Headcount_eff <- Headcount_Inc %>% left_join(Headcount_Exp)  %>% 
  pivot_longer(cols = any_of(c(lis_eff)), names_to = "Effect", values_to = "value") %>% mutate(Effect = factor(Effect, level = lis_eff) %>% fct_rev()) %>% 
  mutate(value = case_when(value < 1 ~ 0, value >= 1 ~ value/1000000), unit = "Million people") 

rm(Headcount_Inc, Headcount_Exp)

## plot  ----
pdata <- Headcount_eff %>% filter(
                Effect %in% lis_eff,
                Y %in% c('2030','2050','2070'),
                TH1 %in% c('1.9-threshold')) 


p_2d <- ggplot() + 
  geom_col(data = pdata %>% filter(SSP == "SSP2"),
           mapping = aes(x = policy, y = value, fill = Effect), 
           width = 0.5, position="stack") +
  geom_abline(intercept = 0, slope = 0, colour = 'grey')+
  labs(#title = "c)", # Added poverty headcount with carbon taxes 
       x = 'Policy',
       y = 'Addition to poverty headcount (million people)',
       size=10) +
  facet_grid(R~Y, scales = "free") +
  MyTheme+ theme(axis.text.x = element_text(angle = 90)) +
  guides(fill = guide_legend(title = 'Effect', ncol = 1, size = 7, byrow = TRUE)) +
  scale_fill_manual(values = palette_eff)
p_2d

ggsave(p_2d,filename = "Figure 2d Poverty headcount decomposition.pdf", path = dir_fig,
       width = 24, height = 26, units = "cm")
rm(p_2d)
