# This program is a summary of all the figures and plots in the paper 
# "Poverty and inequality in China under the long-term global temperature target"
# This program deals with Expenditure and its change

# Shiya ZHAO, 2021/06/17

## Loading packages ----
library(gdxrrw)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(gdata)
library(grid)
library(Rmisc)
library(scales)


# prog_loc <- "E:/szhao/model/AIMPHI/analysis/R/"
# setwd(paste0(prog_loc,"carbon_tax_distribution/"))

source('0_PlotSettings.R')
source('0_Functions.R')
source('0_Maps.R')
source('0_Directories.R')


## Directory assignment and data loading ----

dir_phi <- "../../../data/phioutput/gdx"
lis_gdxphi <- list.files(dir_phi,pattern = ".gdx", full.names = TRUE)
#view(lis_gdxphi)

# Functions ----
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

# Expenditure share block ----

# Consumption loading
# EV, Mu and Sigma are stored in 

# CON_tmp <- F_DEC_Consumption(demand,lis_ref,lis_y,lis_I,LengthThreshold = 10000, StepLength = 10, all=FALSE)

lis_y <- c(seq(2020,2050,10))
demand <- "../../../output/gdx/demand/country/CHN.gdx"
demand <- "../../data/PHIoutput/CHN_demand.gdx"

PoutputNational <- rgdx.param(demand, "PoutputNational") %>%
  filter( Ref %in% lis_ref,
          Y %in% lis_y) %>%
  dplyr::rename(value = "PoutputNational") %>%
  left_join(MapI) %>%
  select(-"I") 

CON_tmp <- F_DEC_tmp(demand,PoutputNational,lis_y, split_all = T)

CON <- CON_tmp %>%
  # left_join(MapDec) %>%
  left_join(MapScenario) %>%
  select(-c('Ref')) %>%
  transform(I = factor(I,levels = lis_I_abb)) %>%
  # transform(DEC = factor(DEC,levels = lis_DEC)) %>%
  mutate(GiniSet = "SSP2")
# dplyr::rename(I = "I_abb")
view(CON)  



# F_DEC_ConsumptionChange <- function(Demand_data,lis_ref,lis_y,lis_I)
# CON_pq_tmp <- F_DEC_ConsumptionChange(demand,lis_ref_mitigation,lis_y,lis_I)


## Fig 3a Expenditure share by decile  ----
# EV, Mu and Sigma are stored in demand 
lis_y <- c('2020','2030','2040','2050')
Demand_data <- demand
PQ <- rgdx.param(Demand_data, "PQchange") %>%
  filter(Ref %in% lis_ref,
         Y %in% lis_y)%>%
  left_join(MapScenario) %>%
  left_join(MapI) %>%
  select(-c('Ref','I')) %>%
  dplyr::rename(I = "I_abb") %>%
  transform(I = factor(I,levels = lis_I_abb)) %>%
  mutate(GiniSet = "SSP2")


ExpNational <- CON %>%
  left_join(PQ) %>%
  mutate(Exp = value_seg*PQchange) %>%
  transform(I = factor(I, levels = c(lis_I_abb))) %>%
  transform(DEC = factor(DEC, levels = lis_DEC) )

csvExpNational <- ExpNational %>%
  filter(Y %in% c('2030','2040','2050'),
         GiniSet == "SSP2",
         scenario %in% lis_scenario_noex) %>%
  select(-c("pop","value_seg","PQchange")) %>%
  spread(I, Exp) %>%
  write.csv(file = paste0(dir_output,"74_Expenditure.csv"))

csvExpNational_share <- ExpNational %>%
  filter(Y %in% c("2020",'2030','2040','2050'),
         GiniSet == "SSP2",
         scenario %in% c("Baseline",lis_scenario_noex)) %>%
  group_by(Y, DEC,scenario) %>%
    dplyr::summarise(total = sum(Exp), I = I, Exp = Exp) %>%
    mutate(share = Exp/total) %>%
  select(-c("total","Exp")) %>%
  spread(I, share) %>%
  write.csv(file = paste0(dir_output,"74_Expenditure_share.csv"))



csvCon <- CON %>%
  filter(Y %in% c('2030','2040','2050'),
         GiniSet == "SSP2",
         scenario %in% lis_scenario_noex) %>%
  select(-c("pop")) %>%
  spread(I, value_seg) %>%
  write.csv(file = paste0(dir_output,"74_Con.csv"))

## Plot ----
pdata <- ExpNational %>%
  filter(Y %in% c(2020), 
         scenario == 'Baseline',
         GiniSet == "SSP2") %>%
  na.omit()

p_3a  <- ggplot(data =  pdata,
                mapping = aes(x = DEC, y = Exp,  # factor(quantile*10,levels = lis_DEC)
                              fill = I)) + 
  geom_col(position = 'fill',
           show.legend  = TRUE) +
  labs(title = "a) ", # Expenditure pattern
       # subtitle = '(Baseline, 2020)',
       x = "Income decile",
       y = "",
       fill = "gear") +
  MyTheme +
  cowplot::panel_border(color = "grey85", size = 1, linetype = 1)+
  guides(fill = guide_legend(title = 'Commodity', ncol = 1, byrow = TRUE), size = 7) +
  scale_fill_manual(values = palette_I_abb) 


p_3a


## Figure 3b real expenditure share ----
ExpNational2 <- ExpNational %>%
  select(-c('pop','PQchange')) %>%
  group_by(Y,DEC,scenario, GiniSet) %>%
  dplyr::summarise(sum = sum(Exp), Exp=Exp, I = I) %>%
  mutate(share = Exp/sum*100) %>%
  filter(GiniSet == "SSP2")


## Plot ----
pdata <- ExpNational2 %>%
  filter(Y %in% c('2020', '2030','2040','2050'), 
         scenario %in% c('Baseline'),
         GiniSet == "SSP2") 

p_3b  <- ggplot(data =  pdata,
                mapping = aes(x = DEC, y = share, 
                              group = Y, color = Y)) + 
  geom_line() +
  geom_point(shape = 1, size = 1) +
  labs(title = "b) ", 
       x = "cyl",
       fill = "gear")+
  xlab("Income decile") +
  ylab("%") +
  facet_wrap(~ (factor(I, levels = lis_I_abb)), scales = "free") + #fct_rev
  MyTheme+
  cowplot::panel_border(color = "grey85", size = 1, linetype = 1)+
  guides(color = guide_legend(title = 'Year', ncol = 1, byrow = TRUE, size = 7))  +
  scale_fill_manual(values = palette_Sc) 

p_3b



pdata <- filter(ExpNational2, scenario %in% c('Baseline',
                                              '50%',
                                              '65%',
                                              '80%',
                                              'NDC'))
p_3b2  <- ggplot(data =  pdata,
                mapping = aes(x = DEC, y = share, 
                              group = scenario)) + 
  geom_line(aes(color = scenario)) +
  labs(title = "Expenditure share",
       x = "Income decile",
       y = "Expenditure share",
       fill = "gear")+
  # facet_grid(~factor(c$scenario,levels = lis_scenario)) +
  facet_grid(Y~factor(I,levels = lis_I_abb), scales = "free") +
  MyTheme +
  panel_border(color = "grey85", size = 1, linetype = 1)+
  guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE))+
  scale_color_manual(values = palette_Sc)
#facet_grid(vars(scenario), vars(Y), scales = 'free')

p_3b2




## Fig 3c Consumption change compared to baseline ----
CON_exp_dec4 <- CON %>%
  select(-c('pop')) %>%
  spread(scenario,value_seg) %>%
  select(Y,GiniSet,DEC,I,all_of(lis_scenario_noex),"Baseline") %>%
  gather(scenario,value, c(all_of(lis_scenario_noex))) %>%
  mutate(change = (value-Baseline)/Baseline)
colnames(CON_exp_dec4)

## plot ----
pdata <- filter(CON_exp_dec4,
                Y %in% c('2050'),
                scenario %in% c(lis_scenario_noex),
                GiniSet == "SSP2")



p_3c <- ggplot(data =  pdata,
               mapping = aes(x = DEC, y = change*100,colour = scenario)) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  geom_line(mapping = aes(group =  scenario)) +
  geom_point(shape = 1, size = 1) + # position = 'dodge'
  labs(title = "c) ", # Change in consumption compared to Baseline (2050)"
       x = "cyl",
       fill = "gear")+
  xlab("Year") +
  ylab("%") +
  facet_wrap(~factor(I, levels = lis_I_abb), scales = 'free') +
  MyTheme  +
  cowplot::panel_border(color = "grey85", size = 1, linetype = 1)+
  guides(colour = guide_legend(title = 'Scenario', row = 1, byrow = TRUE, size = 7))
p_3c


## Paste ----
p_3a
p_3b
p_3c

tiff(filename = paste0(dir_fig,'tiff/Figure 4. Expenditure share by decile.tiff'),
     res = 300,
     width = 22,height = 20,
     units = "cm")

grid.newpage()  ###??????蟒ｺ蝗ｾ陦???迚磯???
pushViewport(viewport(layout = grid.layout(6,11))) ####????迚磯???蛻?????2*2遏ｩ髦???
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
print(p_3a, vp = vplayout(3:4,1:4))
print(p_3b, vp = vplayout(1:3,5:11))     ###???(2,1)???位置画图chart2
print(p_3c, vp = vplayout(4:6,5:11))   ###??????1,1)???(1,2)???位置画图chart3

# Fig 3 EV_total 1300 1100
dev.off()


## EPS ----
p_3a
p_3b
p_3c


setEPS()

postscript(paste0(dir_fig,'eps/Figure 4. Expenditure share by decile.eps'),
           width = 8,height = 8)

grid.newpage()  ###新建图表版面
pushViewport(viewport(layout = grid.layout(6,17))) ####???版面??????2*2矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}

print(p_3a, vp = vplayout(3:4,1:6))
print(p_3b, vp = vplayout(1:3,7:17))     ###???(2,1)???位置画图chart2
print(p_3c, vp = vplayout(4:6,7:17))   ###??????1,1)???(1,2)???位置画图chart3

dev.off()#

