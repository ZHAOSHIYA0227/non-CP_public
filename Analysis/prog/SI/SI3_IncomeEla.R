# This program is a summary of all the figures and plots in the paper SI
# "Poverty and inequality in China under the long-term global temperature target"
# This program deals with AIDADS calibration estimates

# Shiya ZHAO, 2021/06/17

## Loading packages ----
library(gdxrrw)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(gdata)
library(grid)
library(Rmisc)


prog_loc <- "E:/szhao/model/AIMPHI/analysis/R/"
setwd(paste0(prog_loc,"carbon_tax_distribution/"))

dir_fig <- "../output/fig/"

source('0_PlotSettings.R')
source('0_Functions.R')
source('0_Maps.R')

F_DEC_tmp <- function(Demand_data,OD,lis_y, split_all){
  StepLength <-  50
  LengthThreshold <- 100
  
  # lis_y <- c(seq(2020,2050,10))
  # CHN_demand <- "../../../output/gdx/demand/country/CHN.gdx"
  # PoutputNational <- rgdx.param(CHN_demand, "PoutputNational") %>%
  # filter( Ref %in% lis_ref,
  #         Y %in% lis_y) %>%
  # # VC == "abs") %>%
  # # select(-"VC") %>%
  # dplyr::rename(value = "PoutputNational") %>%
  # left_join(MapI) %>%
  # select(-"I")
  
  # OD <- PoutputNational
  # Demand_data <- "../../../output/gdx/demand/country/CHN.gdx"
  # split_all <- TRUE
  # Demand_data <- demand
  # OD <- EVi
  
  
  lis_ref <- as.character(unique(OD$Ref))
  lis_I_abb <- as.character(unique(OD$I_abb))
  
  
  Seg_pop <- rgdx.param(Demand_data, "FreqSegall") %>%
    rename.vars(colnames(.), c('Ref','Y','Seg','pop')) %>%
    SegNum()
  
  
  Mu_exp <- rgdx.param(Demand_data, "Mu_exp") %>%
    filter(Ref %in% lis_ref)
  
  Sigma_exp <- rgdx.param(Demand_data, "Sigma_exp") %>%
    filter(Ref %in% lis_ref)
  
  q <- seq(from = 0.1, to = 1, by = 0.1)
  
  # the q_value for each scenario and each year
  data_q_value <- Sigma_exp %>%
    left_join(Mu_exp) %>%
    group_by(Ref,Y) %>%
    dplyr::summarise(Sigma_exp = Sigma_exp, Mu_exp = Mu_exp, 
                     q_value = c(qlnorm(q,Mu_exp,Sigma_exp)[-10],1000000),
                     .groups = "drop") %>%
    mutate(DEC = as.character(c(t(rep(q*10,nrow(.)/length(q)))))) 
  
  # identifying the decile where each income segment belongs
  data_tmp1 <- data_q_value  %>%
    spread(DEC,q_value) %>%
    left_join(Seg_pop)
  data_tmp2 <- ifelse((data_tmp1[,which(colnames(data_tmp1) %in% as.character(q*10))]-data_tmp1$Lower) <0, 1,0) %>%
    apply(.,1,sum) %>%
    as.data.frame() %>%
    dplyr::rename(DEC_tmp = colnames(.)) %>%
    mutate(DEC = as.character(DEC_tmp +1))
  data1_tmp3 <- data_tmp1%>%
    cbind(data_tmp2) %>%
    select(-c(as.character(q*10),"DEC_tmp")) %>%
    left_join(data_q_value) %>%
    mutate(idx = ifelse((Lower <= q_value & q_value <= Upper),1,0)) %>% # idx == 1: the segment is cut by q_value
    right_join(OD) %>%
    spread(I_abb, value)
  
  # filtering and splitting the segments where the decile thresholds cut
  data1_tmp4 <- data1_tmp3 %>%
    mutate(floor1 = ifelse(idx == 1, floor((q_value - Lower)/StepLength),floor((Upper - Lower)/StepLength)),
           floor2 = ifelse(idx == 1, floor((Upper - q_value)/StepLength), 0),
           ceiling1 = ifelse(idx == 1, ceiling((q_value - Lower)/StepLength), ceiling((Upper - Lower)/StepLength)),
           ceiling2 = ifelse(idx == 1, ceiling((Upper - q_value)/StepLength), 0)) %>%
    gather(I, value, all_of(lis_I_abb))%>%
    mutate(ExpShare = value/Upper) %>%
    # select(-"total") %>%
    transform(DEC = as.numeric(DEC))
  colnames(data1_tmp4)
  
  data1_ExpShare <- data1_tmp4 %>%
    select(c("Ref", "Y","Sigma_exp", "Mu_exp", "Seg", "I", "ExpShare"))
  if(split_all == TRUE){
    data1_tmp6 <- data1_tmp4 %>%
      # filter(Upper-Lower >= LengthThreshold) %>%
      select(-"ExpShare") %>%
      spread(I, value) %>%
      select(-all_of(lis_I_abb))  
  }else{
    data1_tmp6 <- data1_tmp4 %>%
      filter(DEC != "10",idx == 1) %>% 
      filter(Upper-Lower >= LengthThreshold) %>%
      select(-"ExpShare") %>%
      spread(I, value) %>%
      select(-all_of(lis_I_abb))
  }
  
  
  data1_tmp7 <- data1_tmp6 %>%
    dplyr::group_by(Y,Ref,DEC,Seg1,Seg2) %>%
    slice(rep(1:n(), first(ceiling1+ceiling2)))%>%
    # group_by(Y,Ref,DEC,Seg1,Seg2) %>%
    dplyr::mutate(idx1 = 1,
                  idxnstep = cumsum(idx1),
                  part = ifelse(idxnstep <= ceiling1,1,2),
                  nstep = ifelse(part == 1,idxnstep,idxnstep-ceiling1),
                  n = ifelse(part == 1, ceiling1, ceiling2)) %>%
    select(-c(ceiling1,ceiling2,floor1,floor2)) 
  
  
  dat2 <- data1_tmp7 %>%
    mutate(UpperLim = ifelse(part == 1, ifelse(nstep < n, Lower+StepLength*nstep,
                                               ifelse(idx == 1, q_value,Upper)), # min(q_value,Upper)
                             ifelse(nstep < n, q_value+StepLength*nstep, Upper)), 
           LowerLim = ifelse(part == 1, ifelse(nstep < n, Lower+StepLength*(nstep-1),  Lower+StepLength*(n-1)),
                             ifelse(nstep < n, q_value+StepLength*(nstep-1), q_value+StepLength*(n-1))))
  
  data1 <- dat2 %>%
    ungroup() %>%
    mutate(DEC_new = DEC+part-1) %>%
    select(-c("Upper", "Lower", "DEC")) %>%
    dplyr::rename(Upper = "UpperLim", Lower = "LowerLim", DEC = "DEC_new") %>%
    left_join(data1_ExpShare) %>%
    mutate(value_new = Upper*ExpShare, 
           density = dlnorm((Upper+Lower),meanlog = Mu_exp, sdlog = Sigma_exp), 
           pop_new = (Upper-Lower)*density) %>%
    select(-c("pop","density","idx1","idxnstep","part","nstep","n")) %>%
    dplyr::rename(value = "value_new", pop = "pop_new")
  # view(data1) 
  
  if(split_all == TRUE){
    data2 <- data1 %>%
      mutate(pop_new = (Upper-Lower)*dlnorm((Upper+Lower)/2,meanlog = Mu_exp, sdlog = Sigma_exp),
             pop = (Upper-Lower)*dlnorm(Upper,meanlog = Mu_exp, sdlog = Sigma_exp)) %>%
      select(-"idx") %>%
      transform(I = factor(I, levels = lis_I_abb)) %>%
      transform(DEC = factor(as.character(DEC), levels = as.character(seq(1,10,1)))) %>%
      # gather(I, value, all_of(lis_I_abb)) %>%
      group_by(Ref,Y,DEC,I) %>%
      dplyr::summarise(value_seg = sum(value*pop_new)/sum(pop_new),  pop = sum(pop_new),#pop_old = sum(pop),
                       .groups="drop")
  }else{
    data2 <- data1_tmp4 %>%
      filter(!(idx == 1 & DEC != "10")) %>% 
      select(colnames(data1)) %>%
      rbind(data1) %>%
      mutate(pop_new = (Upper-Lower)*dlnorm((Upper+Lower)/2,meanlog = Mu_exp, sdlog = Sigma_exp)) %>%
      select(-"idx") %>%
      transform(I = factor(I, levels = lis_I_abb)) %>%
      transform(DEC = factor(as.character(DEC), levels = as.character(seq(1,10,1)))) %>%
      # gather(I, value, all_of(lis_I_abb)) %>%
      group_by(Ref,Y,DEC,I) %>%
      dplyr::summarise(value_seg = sum(value*pop_new)/sum(pop_new),  pop = sum(pop_new),#pop_old = sum(pop),
                       .groups="drop")
    #   dplyr::summarise(value_seg = sum(value*pop)/sum(pop),  pop = sum(pop),#pop_old = sum(pop),
    #                    .groups="drop") 
  }
  
  
  return(data2)
}


## Directory assignment and data loading ----
# CGE directory and data 

cge_chn_ana <- 	"../../../data/cgeoutput/gdx/CHN_analysis.gdx"
cge_chn_var <- "../../../data/cgeoutput/gdx/CHN_emf.gdx"
cge_global_ana <- "../../../data/cgeoutput/gdx/global_analysis.gdx"
cge_global_var <- "../../../data/cgeoutput/gdx/global_17_emf.gdx"

# PHI directory and data
dir_phi <- "../../../data/phioutput/gdx"
lis_gdxphi <- list.files(dir_phi,pattern = ".gdx", full.names = TRUE)
#view(lis_gdxphi)

AIDADSCalib <- "../../data/PHIoutput/AIDADSCalibNationResults.gdx"
input <- "../../../model/data/Inputdata.gdx"
demand <- "../../data/PHIoutput/CHN_demand.gdx"

AIDADSCalib_prj_GCD <- "../../data/PHIoutput/AIDADSCalibNationResults_DF.gdx"

#view(c(AIDADSCalib,AnaExp,AnaInc,demand,pricechange))

# World development database 
dir_wdi <- "../../../data/WDI"
lis_xlsxwdi <-  list.files(dir_wdi,pattern = ".xlsx", full.names = TRUE)
# view(lis_xlsxwdi)

# Output directory 
dir_fig <- "../output/fig/"


# Fig 1 Income elasticity from AIDADS calibration ----

lis_y <- c('2020','2030','2040','2050')
IncomeEla <- rgdx.param(AIDADSCalib, "PoutputIncomeElaNational") %>%
  gdata::rename.vars(from = colnames(.), to = c("R","Seg","Y","I","value")) %>%
  left_join(MapI) %>%
  left_join(map_seg) %>%
  left_join(map_seg_abb) %>%
  select(-c("I")) %>%
  na.omit() %>%
  transform(I_abb = factor(I_abb, levels = lis_I_abb))

IncomeEla_GCD <- rgdx.param(AIDADSCalib_prj_GCD, "PoutputIncomeElaNational") %>%
  gdata::rename.vars(from = colnames(.), to = c("R","Seg","Y","I","value")) %>%
  filter(R == "CHN") %>%
  left_join(MapI) %>%
  left_join(map_seg_gcd) %>%
  select(-c("I")) %>%
  na.omit() %>%
  transform(I_abb = factor(I_abb, levels = lis_I_abb))



## plot ----
pdata <- IncomeEla_GCD

p1 <- ggplot() + 
  geom_point(data = pdata,
             mapping = aes(x = factor(Seg_gcd_abb,levels = lis_Seg_gcd_abb), y = value), 
             size = 1) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(#title = "c) ", # EV ratio decomposition
    # subtitle = "Total (income effect + price effect)",
    x = "Income decile", y = "Income Elasticity")+
  facet_wrap(~I_abb, scales = 'free') +
  MyTheme_x90+
  cowplot::panel_border(color = "grey85", linetype = 1)+
  guides(color = guide_legend(title = 'Y', ncol = 1, byrow = TRUE)) # +
# scale_color_manual(values = palette_Sc) 

p1

ggsave(filename = paste0(dir_fig,"tiff/Fig SI6 Income Ela GCD.png"),p1,
                         width = 18,height = 12, unit = "cm")


## plot ----
pdata <- IncomeEla


p2 <- ggplot() + 
  geom_point(data = pdata,
             mapping = aes(x = factor(Seg_abb,levels = c(lis_rural_abb,lis_urban_abb)), y = value, 
                           color = Y), 
              size = 1) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(#title = "c) ", # EV ratio decomposition
       # subtitle = "Total (income effect + price effect)",
       x = "Income decile", y = "Income Elasticity")+
  facet_wrap(~I_abb, scales = 'free',ncol = 3) +
  MyTheme_x90+
  cowplot::panel_border(color = "grey85", linetype = 1)+
  guides(color = guide_legend(title = 'Y', ncol = 1, byrow = TRUE)) # +
  # scale_color_manual(values = palette_Sc) 

p2

ggsave(filename = paste0(dir_fig,"tiff/Fig SI6 Income Ela Phi.png"),p2,
       width = 21,height = 14, unit = "cm")



# Fig 2 Income elasticity projection ----
lis_y <- c('2020','2030','2040','2050')
IncomeEla_od <- rgdx.param(demand, "IncomeEla") %>%
  filter( Ref %in% lis_ref,
          Y %in% lis_y) %>%
  dplyr::rename(value = "IncomeEla") %>%
  left_join(MapI) %>%
  select(-"I") %>%
  mutate(GiniSet = "SSP2")
IncomeEla_tmp <- F_DEC_tmp(demand,IncomeEla_od,lis_y, split_all = T)
IncomeEla_prj <- IncomeEla_tmp %>%
  left_join(MapScenario) %>%
  select(-c("Ref")) %>%
  na.omit() %>%
  dplyr::rename(value = value_seg) %>%
  mutate(GiniSet = "SSP2")


## BACKUP ----
# ## Fig SI6a Income ela Baseline 
# pdata <- IncomeEla_prj %>%
#   filter(scenario == 'Baseline',
#          GiniSet == "SSP2")
# 
# p_SI6a <- ggplot() + # , group = factor(Ref, levels = lis_scenario)
#   geom_point(data = pdata,
#              mapping = aes(x = DEC, y = value, 
#                            color = as.character(Y)),
#              # shape = 1,
#              size = 1) +
#   # geom_line(data = pdata,
#              # mapping = aes(x = DEC, y = value,
#                            # color = as.character(Y),group = Y)
#   # ) +
#   geom_abline(intercept = 0, slope = 0, colour = 'grey') +
#   labs(title = 'a)', # Income elasticity (Baseline)
#        x = 'Income segment',
#        y = 'Elasticity') +
#   facet_wrap(~I, scales = 'free',ncol = 6) +
#   MyTheme_lowLegend  +
#   # panel_border(color = "grey85", size = 1, linetype = 1)+
#   guides(color = guide_legend(title = 'Year', nrow = 1, byrow = TRUE)) +
#   scale_fill_manual(values = palette_y)
# p_SI6a
# # ggsave(p_SI6a,filename = "Figure SI6a IncomeEla_Baseline.png", path = dir_fig,
# #        width = 10,height = 8)
# 
# 
# ## Fig SI6b Income ela 2050 across scenario
# pdata <- IncomeEla_prj %>%
#   filter(scenario %in% c('NDC'),
#                 GiniSet == "SSP2")
# p_SI6b <- ggplot(data = pdata,
#                  mapping = aes(x = DEC, y = value, 
#                                color = as.character(Y))) + # , group = factor(Ref, levels = lis_scenario)
#   geom_point(shape = 1,
#              size = 1) +
#   geom_line(data = pdata,
#             mapping = aes(x = DEC, y = value,
#                           color = as.character(Y),group = Y)
#   ) +
#   geom_abline(intercept = 0, slope = 0, colour = 'grey') +
#   labs(title = 'b) Income elasticity (NDC)',
#        x = 'Income segment',
#        y = 'Elasticity') +
#   facet_wrap(~I, scales = 'free',ncol = 6) +
#   MyTheme_lowLegend  +
#   panel_border(color = "grey85", size = 1, linetype = 1)+
#   guides(color = guide_legend(title = 'Year', nrow = 1, byrow = TRUE)) +
#   scale_fill_manual(values = palette_y)
# p_SI6b
# # ggsave(p_SI6b,filename = "Figure SI6b IncomeEla_NDC.png", path = dir_fig,
# #        width = 10,height = 8)
# 
# 
# 
# ## Fig SI6c Income ela 2030 across scenario
# pdata <- filter(IncomeEla_prj, Y %in% c(2030), scenario %in% lis_scenario_noex,
#                 GiniSet == "SSP2")
# p_SI6c <- ggplot(data = pdata,
#                  mapping = aes(x = DEC, y = value,
#                              color = scenario)) + # , group = factor(Ref, levels = lis_scenario)
#   geom_point(shape = 1,
#              size = 1) +
#   geom_line(data = pdata,
#             mapping = aes(x = DEC, y = value,
#                           color = scenario,group = scenario)
#   ) +
#   geom_abline(intercept = 0, slope = 0, colour = 'grey') +
#   labs(title = 'c) Income elasticity (2030)',
#        x = 'Income segment',
#        y = 'Elasticity') +
#   facet_wrap(~I, scales = 'free',ncol = 6) +
#   # facet_wrap(~setting, scales = 'free') +
#   # facet_wrap(~Y, scales = 'free') +
#   MyTheme_lowLegend  +
#   panel_border(color = "grey85", size = 1, linetype = 1)+
#   guides(color = guide_legend(title = 'Scenario', nrow = 1, byrow = TRUE)) +
#   scale_fill_manual(values = palette_y)
# p_SI6c
# # ggsave(p_SI6c,filename = "Figure SI6c IncomeEla_2030.png", path = dir_fig,
# #        width = 10,height = 8)
# 
# 
# 
# ## Fig SI6d Income ela 2050 across scenario
# pdata <- filter(IncomeEla_prj, Y %in% c(2050), scenario %in% lis_scenario_noex,
#                 GiniSet == "SSP2")
# p_SI6d <- ggplot(data = pdata,
#                  mapping = aes(x = DEC, y = value,
#                                color = scenario)) + # , group = factor(Ref, levels = lis_scenario)
#   geom_point(shape = 1,
#              size = 1) +
#   geom_line(data = pdata,
#             mapping = aes(x = DEC, y = value,
#                           color = as.character(Y),group = Y)
#   ) +
#   geom_abline(intercept = 0, slope = 0, colour = 'grey') +
#   labs(title = 'd) Income elasticity (2050)',
#        x = 'Income segment',
#        y = 'Elasticity') +
#   facet_wrap(~I, scales = 'free',ncol = 6) +
#   # facet_wrap(~setting, scales = 'free') +
#   # facet_wrap(~Y, scales = 'free') +
#   MyTheme_lowLegend  +
#   panel_border(color = "grey85", size = 1, linetype = 1)+
#   guides(color = guide_legend(title = 'Scenario', nrow = 1, byrow = TRUE)) +
#   scale_fill_manual(values = palette_y)
# p_SI6d
# # ggsave(p_SI6d,filename = "Figure SI6d IncomeEla_NDC_2050.png", path = dir_fig,
# #        width = 10,height = 8)
# 
# 
# 

##THE END of backup ----

# # Paste ----
# p_SI6a
# p_SI6b
# p_SI6c
# p_SI6d
# 
# tiff(filename = paste0(dir_fig,'tiff/SI 6. Income elasticity projections.tiff'),
#      res = 300,
#      width = 1800,height = 2000)
# 
# grid.newpage()  ###新建图表版面
# pushViewport(viewport(layout = grid.layout(4,1))) ####???版面??????2*2矩阵
# vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
# print(p_SI6a, vp = vplayout(1,1))   ###??????1,1)???(1,2)???位置画图chart3
# print(p_SI6b, vp = vplayout(2,1))     ###???(2,1)???位置画图chart2
# print(p_SI6c, vp = vplayout(3,1))   ###??????1,1)???(1,2)???位置画图chart3
# print(p_SI6d, vp = vplayout(4,1))     ###???(2,1)???位置画图chart2
# 
# dev.off()#
# 
# # p_1 <- multiplot(p_SI4a0,p_SI4b, layout = matrix(c(1,1,2), nrow=3,  byrow=TRUE))
# 
# # "Fig SI6 Future elasticity.png", 1400:1900
# # dev.off()#


## Functions ----
SegNum <- function(a){
  lis_Seg <- c('Fiftytok',
               'ktoTk',
               'TktoHk',
               'HktoM')
  lis_Seg11 <- c(50,
                 1000,
                 10000,
                 100000)
  lis_Seg3 <- c(10,
                100,
                1000,
                10000)
  tmp2 <- data.frame()
  for(i in 1:4){
    tmp <- a[which(startsWith(as.character(a$Seg),lis_Seg[i])),]%>%
      mutate(Seg0 = lis_Seg[i], Seg1 = lis_Seg11[i])
    tmp1 <- mutate (tmp, 
                    Seg2 = as.numeric(substring(as.character(tmp$Seg),nchar(lis_Seg[i])+1)),
                    Seg3 = Seg1+Seg2*lis_Seg3[i])
    tmp2 <- rbind(tmp2,tmp1)
  }
  return(tmp2)
}

Q_lognormal <- function(q,mu,sigma){
  q1 <- qlnorm(q,meanlog = mu, sdlog = sigma)
  return(q1)
}

F_DEC_IncomeEla <- function(Demand_data,lis_ref,lis_y,lis_I){
  Seg_pop <- rgdx.param(Demand_data, "FreqSegall")
  colnames(Seg_pop) <- c('Ref','Y','Seg','pop')
  
  IncomeEla <- rgdx.param(Demand_data, "IncomeEla") %>%
    filter( Ref %in% lis_ref,
            Y %in% lis_y) %>%
    SegNum()%>%
    merge(Seg_pop, by = c('Ref','Y','Seg'))
  
  Mu_exp <- rgdx.param(Demand_data, "Mu_exp") %>%
    filter( Ref %in% lis_ref,
            Y %in% lis_y) 
  
  Sigma_exp <- rgdx.param(Demand_data, "Sigma_exp") %>%
    filter( Ref %in% lis_ref,
            Y %in% lis_y) 
  
  c <- merge(Sigma_exp,Mu_exp, by = c('Ref','Y'))
  
  q <- seq(from = 0.1, to = 1, by = 0.1)
  
  
  output_2 <- data.frame()
  output_3 <- data.frame()
  data <- merge(IncomeEla, c, by = c('Ref','Y'))
  for(m in 1:length(lis_I)){
    for(r in 1:length(lis_ref)){
      for(i in 1:length(lis_y)){ # i is the index for "Y"
        tmp <- data[which(data$Y == lis_y[i] & data$Ref == lis_ref[r] & data$I == lis_I[m]),]
        q_value <-  c(Q_lognormal(q,tmp$Mu_exp[1],tmp$Sigma_exp[1])[-10],max(tmp$Seg3))
        for(j in 1:length(q)){
          if(j == 1){
            tmp2 <- tmp[which(tmp$Seg3 <= q_value[[j]]),]
            tmp3 <- data.frame(t(sum(tmp2$IncomeEla*tmp2$pop)/sum(tmp2$pop)))%>%
              mutate(Y = lis_y[i], Ref = lis_ref[r], I = lis_I[m],
                     quantile = q[[j]], q_value = q_value[j])
            colnames(tmp3)[1] <- 'IncomeEla'
            output_1 <- tmp3
          }
          else{
            tmp2 <- tmp[which(tmp$Seg3 <= q_value[[j]] & tmp$Seg3 > q_value[[j-1]]),]
            tmp3 <- data.frame(t(sum(tmp2$IncomeEla*tmp2$pop)/sum(tmp2$pop))) %>%
              mutate(Y = lis_y[i], Ref = lis_ref[r],I = lis_I[m],
                     quantile = q[[j]], q_value = q_value[j])
            colnames(tmp3)[1] <- 'IncomeEla'
            output_1 <- rbind.data.frame(output_1,tmp3)
          }
        }
        output_2 <- rbind(output_2,output_1)
      }
    }
  }
  return(output_2)
}


## Directory assignment and data loading ----
# CGE directory and data 


#view(c(AIDADSCalib,AnaExp,AnaInc,demand,pricechange))

# World development database 
dir_wdi <- "..//..//..//data\\WDI"
lis_xlsxwdi <-  list.files(dir_wdi,pattern = ".xlsx", full.names = TRUE)
# view(lis_xlsxwdi)

## Lists ----
lis_scenario_0 <- c('Baseline',
                    '50%',
                    '65%',
                    '80%',
                    'NDC')

lis_scenario <- c('Baseline',
                  '50%',
                  '65%',
                  '80%',
                  'NDC',
                  '50%_exempt',
                  '65%_exempt',
                  '80%_exempt',
                  'NDC_exempt')

lis_ref_mitigation <- c('SSP2_NS1_CM1_exempt', 
                        'SSP2_NS1_CM2_exempt',
                        'SSP2_NS1_CM3_exempt',
                        'SSP2_NS1_CM4_exempt',
                        'SSP2_NS1_CM1', 
                        'SSP2_NS1_CM2',
                        'SSP2_NS1_CM3',
                        'SSP2_NS1_CM4'
)

lis_ref <- c('SSP2_NS1_Baseline',
             'SSP2_NS1_CM1_exempt', 
             'SSP2_NS1_CM2_exempt',
             'SSP2_NS1_CM3_exempt',
             'SSP2_NS1_CM4_exempt',
             'SSP2_NS1_CM1', 
             'SSP2_NS1_CM2',
             'SSP2_NS1_CM3',
             'SSP2_NS1_CM4')


lis_scenario_noex <- c('50%',
                       '65%',
                       '80%',
                       'NDC')

lis_scenario_exmp <- c('50%_exempt',
                       '65%_exempt',
                       '80%_exempt',
                       'NDC_exempt')

lis_scenario_mitigation <- c(lis_scenario_noex,lis_scenario_exmp)
# Commodities 

lis_I <- c('Food and nonalcoholic beverages',
           'Alcoholic beverages, tobacco, and narcotics',
           'Clothing and footwear',
           'Housing, water, electricity, gas and other fuels',
           'Furnishings, household equipment and maintenance',
           'Health',
           'Transport',
           'Communication',
           'Recreation and culture',
           'Education',
           'Restaurants and hotels',
           'Miscellaneous goods and services')


lis_I_abb <- c('Food',
               'Alcohol&Others',
               'Clothing',
               'Residentials',
               'Equipment',
               'Health',
               'Transport',
               'Communication',
               'Recreation',
               'Education',
               'Restaurant&Hotel',
               'Miscellaneous')

# list of deciles
lis_DEC <- c('1','2','3','4','5','6','7','8','9','10')


# File list 

dir_demand <- "..//..//..//data\\phioutput\\gdx\\demand"
lis_gdxdemand <- list.files(dir_demand,pattern = ".gdx", full.names = TRUE)





## Palette ----
palette_I_abb <- c('FNB' = '#FF3366', #'#9999CC', 
                   'ATN' = '#CC66FF', #CCCCFF',
                   'CFW' = '#CC6699', # 
                   'HWE' = '#FF9966', 
                   'FHE' = '#CCFF00', 
                   'HLT' = '#3399FF', 
                   'TRN' = '#33CC33', 
                   'CMN' = '#CC0000', 
                   'REC' = '#FFCC00', 
                   'EDC' = '#66CCFF', 
                   'REH' = '#0099CC', 
                   'MGS' = '#999999')

palette_y <- c('2020' = '#FF3366', #'#9999CC', 
               '2030' = '#CC66FF', #CCCCFF',
               '2040' = '#CC6699', # 
               '2050' = '#FF9966')


## Fig 1a Income Elasticity from future projections ----
lis_ref <- c("SSP2_Baseline",
             "SSP2_NS1_Baseline" ,
             "SSP2_NS1_CM1_exempt",
             "SSP2_NS1_CM2_exempt",
             "SSP2_NS1_CM3_exempt",
             "SSP2_NS1_CM4_exempt",
             "SSP2_NS1_CM5_exempt",
             "SSP2_NS1_CM6_exempt",
             "SSP2_NS1_CM7_exempt",
             "SSP2_NS1_CM8_exempt",
             "SSP2_NS1_CM1",
             "SSP2_NS1_CM2",
             "SSP2_NS1_CM3",
             "SSP2_NS1_CM4",          
             "SSP2_NS1_CM5",
             "SSP2_NS1_CM6",
             "SSP2_NS1_CM7",
             "SSP2_NS1_CM8")

lis_y <- c(2020,2030,2040,2050)

lis_I <- c('Food and nonalcoholic beverages',
           'Alcoholic beverages, tobacco, and narcotics',
           'Clothing and footwear',
           'Housing, water, electricity, gas and other fuels',
           'Furnishings, household equipment and maintenance',
           'Health',
           'Transport',
           'Communication',
           'Recreation and culture',
           'Education',
           'Restaurants and hotels',
           'Miscellaneous goods and services')

IncomeEla_tmp <- F_DEC_IncomeEla(demand,lis_ref,lis_y,lis_I)
IncomeEla <- IncomeEla_tmp %>%
  # transform(quantile = as.character(quantile)) %>%
  left_join(MapScenario) %>%
  left_join(MapI) %>%
  left_join(MapDec) %>%
  transform(I_abb = factor(I_abb, levels = lis_I_abb)) %>%
  transform(DEC = factor(DEC, levels = lis_DEC))

## Fig SI6a Income ela Baseline ----
pdata <- filter(IncomeEla, scenario == 'Baseline')
p_SI6a <- ggplot(data = pdata,
                 mapping = aes(x = DEC, y = IncomeEla, 
                               color = as.character(Y))) + # , group = factor(Ref, levels = lis_scenario)
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = 'a) Income elasticity (Baseline)',
       x = 'Income segment',
       y = 'Elasticity') +
  facet_wrap(~I_abb, scales = 'free',ncol = 4) +
  MyTheme_lowLegend  +
  panel_border(color = "grey85", size = 1, linetype = 1)+
  guides(color = guide_legend(title = 'Year', nrow = 1, byrow = TRUE)) +
  scale_fill_manual(values = palette_y)
p_SI6a
# ggsave(p_SI6a,filename = "Figure SI6a IncomeEla_Baseline.png", path = dir_fig,
#        width = 10,height = 8)


## Fig SI6b Income ela 2050 across scenario ---- 
pdata <- filter(IncomeEla, scenario %in% c('NDC'))
p_SI6b <- ggplot(data = pdata,
                 mapping = aes(x = DEC, y = IncomeEla, 
                               color = as.character(Y))) + # , group = factor(Ref, levels = lis_scenario)
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = 'b) Income elasticity (NDC)',
       x = 'Income segment',
       y = 'Elasticity') +
  facet_wrap(~I_abb, scales = 'free',ncol = 4) +
  MyTheme_lowLegend  +
  panel_border(color = "grey85", size = 1, linetype = 1)+
  guides(color = guide_legend(title = 'Year', nrow = 1, byrow = TRUE)) +
  scale_fill_manual(values = palette_y)
p_SI6b
# ggsave(p_SI6b,filename = "Figure SI6b IncomeEla_NDC.png", path = dir_fig,
#        width = 10,height = 8)



## Fig SI6c Income ela 2030 across scenario ---- 
pdata <- filter(IncomeEla, Y %in% c(2030), scenario %in% lis_scenario_noex)
p_SI6c <- ggplot(data = pdata,
                 mapping = aes(x = DEC, y = IncomeEla, 
                               color = scenario)) + # , group = factor(Ref, levels = lis_scenario)
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = 'a) Income elasticity (2030)',
       x = 'Income segment',
       y = 'Elasticity') +
  facet_wrap(~I_abb, scales = 'free',ncol = 4) +
  MyTheme_lowLegend +
  panel_border(color = "grey85", size = 1, linetype = 1)+
  guides(color = guide_legend(title = 'Scenario', nrow = 1, byrow = TRUE)) +
  scale_fill_manual(values = palette_y)
p_SI6c
# ggsave(p_SI6c,filename = "Figure SI6c IncomeEla_2030.png", path = dir_fig,
#        width = 10,height = 8)



## Fig SI6d Income ela 2050 across scenario ---- 
pdata <- filter(IncomeEla, Y %in% c(2050), scenario %in% lis_scenario_noex)
p_SI6d <- ggplot(data = pdata,
                 mapping = aes(x = DEC, y = IncomeEla, 
                               color = scenario)) + # , group = factor(Ref, levels = lis_scenario)
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = 'b) Income elasticity (2050)',
       x = 'Income segment',
       y = 'Elasticity') +
  facet_wrap(~I_abb, scales = 'free',ncol = 4) +
  MyTheme_lowLegend  +
  # theme(text = element_text(size=7)) +
  panel_border(color = "grey85", size = 1, linetype = 1)+
  guides(color = guide_legend(title = 'Scenario', nrow = 1, byrow = TRUE)) +
  scale_fill_manual(values = palette_y)
p_SI6d
# ggsave(p_SI6d,filename = "Figure SI6d IncomeEla_NDC_2050.png", path = dir_fig,
#        width = 10,height = 8)




## Paste ----
p_SI6a
p_SI6b
p_SI6c
p_SI6d

tiff(filename = paste0(dir_fig,'tiff/SI 61. Income elasticity projections_crossTime.tiff'),
     res = 300,
     width = 18,height = 22, units = "cm")
grid.newpage()  ###�V��???�\�Ŗ�
pushViewport(viewport(layout = grid.layout(2,1))) ####���Ŗʕ���2*2��???
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
print(p_SI6a, vp = vplayout(1,1))   ###���i1,1)�a(1,2)�I�ʒu��???chart3
print(p_SI6b, vp = vplayout(2,1))     ###��(2,1)�I�ʒu��???chart2
dev.off()#

tiff(filename = paste0(dir_fig,'tiff/SI 62. Income elasticity projections_crossScenario.tiff'),
     res = 300,
     width = 18,height = 22, units = "cm")
grid.newpage()  ###�V��???�\�Ŗ�
pushViewport(viewport(layout = grid.layout(2,1))) ####���Ŗʕ���2*2��???
print(p_SI6c, vp = vplayout(1,1))   ###���i1,1)�a(1,2)�I�ʒu��???chart3
print(p_SI6d, vp = vplayout(2,1))     ###��(2,1)�I�ʒu��???chart2

dev.off()#

