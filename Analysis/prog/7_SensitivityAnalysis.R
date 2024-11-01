# This program is for  the uncertainty analysis in the master thesis
# By Shiya Zhao, 2022/02/02

library(gdxrrw)
library(ggplot2)
library(tidyverse)
library(gdata)
library(grid)
# library(ggalluvial)
# library(waterfalls)

prog_loc <- "E:/szhao/model/AIMPHI/analysis/R/"
setwd(paste0(prog_loc,"carbon_tax_distribution/"))

source('0_PlotSettings.R')
source('0_Functions.R')
source('0_Maps.R')


#getwd()# 
dir_fig <- paste0(prog_loc,"output/fig/")
dir_csv <- paste0(prog_loc,"output/csv/")
dir.create(dir_fig)
dir.create(dir_csv)

AIDADSCalib_prj_Phi <- "../../data/SensitivityAnalysis/ProjectionPhi/AIDADSCalib/AIDADSCalibNationResults.gdx"
AnaExp_prj_Phi <- "../../data/SensitivityAnalysis/ProjectionPhi/AnalysisExpenditure.gdx"
AnaInc_prj_Phi <- "../../data/SensitivityAnalysis/ProjectionPhi/AnalysisIncome.gdx"
demand_prj_Phi <- "../../data/SensitivityAnalysis/ProjectionPhi/demand/country/CHN.gdx"
pricechange_prj_Phi <- "../../data/SensitivityAnalysis/ProjectionPhi/demand/pricechange.gdx"

AIDADSCalib_prj_GCD <- "../../data/SensitivityAnalysis/ProjectionGCD/AIDADSCalib/AIDADSCalibNationResults.gdx"
AnaExp_prj_GCD <- "../../data/SensitivityAnalysis/ProjectionGCD/AnalysisExpenditure.gdx"
AnaInc_prj_GCD <- "../../data/SensitivityAnalysis/ProjectionGCD/AnalysisIncome.gdx"
demand_prj_GCD <- "../../data/SensitivityAnalysis/ProjectionGCD/demand/country/CHN.gdx"
pricechange_prj_GCD <- "../../data/SensitivityAnalysis/ProjectionGCD/demand/pricechange.gdx"

AnaInc <- "../../data/SensitivityAnalysis/DatabasePhi/AnalysisIncome.gdx"
# 1. Assumption: Gini coefficient ----
## Directory assignment and data loading ----
# Phi output
AIDADSCalib <- 	"../../../output/gdx/AIDADSCalib/country/CHN.gdx"
AnaExp <- "../../../output/gdx/AnalysisExpenditure.gdx"
AnaInc <- "../../../output/gdx/AnalysisIncome.gdx"
demand <- "../../../output/gdx/demand/country/CHN.gdx"
pricechange <- "../../../output/gdx/demand/pricechange.gdx"
input <- "../../../model/data/Inputdata.gdx"

# AIDADSCalib <- 	"../../data/PHIoutput/AIDADSCalibNational.gdx"
# AnaExp <- "../../data/PHIoutput/AnalysisExpenditure.gdx"
# AnaInc <- "../../data/PHIoutput/AnalysisIncome.gdx"
# demand <- "../../data/PHIoutput/CHN_demand.gdx"
# pricechange <- "../../data/PHIoutput/demand/pricechange.gdx"
# input <- "../../data/PHIoutput/Inputdata.gdx"


# Plot 1 Gini coefficient ----
Gini <- rgdx.param(input, "Gini") %>%
  filter(R == "CHN",
         Ref %in% c("SSP1","SSP2","SSP3","SSP4","SSP5") ,
         Y%in% c(seq(2010,2050,5)))


pdata <- Gini %>%
  filter(R == "CHN",
         Ref %in% c("SSP1","SSP2","SSP3"))

p_71 <- ggplot() +
  geom_line(data = pdata,
            mapping = aes(x = Y, y = Gini/100, 
                          group = Ref,color = Ref)) +
  labs(#title = "a)", # Poverty headcount Databases
       x = 'Year',
       y ='Gini',
       size=9)+
  MyTheme  +
  guides(color = guide_legend(title = "Gini coefficient", ncol = 1, byrow = TRUE, size = 7)) +
  # scale_x_discrete(breaks=seq(2020, 2050, 5))  +
  scale_color_manual(values = palette_GiniSet) 

p_71
ggsave(filename = paste0(dir_fig,"Fig SI 141 SensAna_GiniSet.png"),
       p_71,
       width = 12,height = 8, unit = "cm")


# Poverty headcount
PoVExp_tmp <- rgdx.param(AnaExp, "PoVExp") %>%
  filter(R == "CHN",
         Ref %in% lis_ref ,
         Y%in% c(seq(2020,2050,5)))%>%
  left_join(MapScenario)


PoVInc_tmp <- rgdx.param(AnaInc, "PoV") %>%
  filter(R == "CHN",
         Ref %in% lis_ref,
         Y %in% c(seq(2020,2050,5))) %>%
  dplyr::rename(valueInc = "PoV") %>%
  left_join(MapScenario)

PoV <- PoVExp_tmp %>%
  left_join(PoVInc_tmp) %>%
  rename.vars(from = 'PoVExp', to = 'PoV') %>%
  mutate(TH1 = recode(TH, 
                      'pop_1.9' = lis_TH[1],
                      'pop_3.2' = lis_TH[2],
                      'pop_5.5' = lis_TH[3])) %>%
  select(-"TH") %>%
  filter(scenario %in% c(lis_scenario_noex,lis_scenario_exmp,'Baseline'))%>%
  rename.vars(from = 'PoV', to = 'value')



# Relative poverty 
RPoVExp <- rgdx.param(AnaExp, "RPoVExp") %>%
  filter(R == "CHN",
         Ref %in% lis_ref ,
         Y%in% c(seq(2020,2050,5)))%>%
  # mutate(prj = 'Phi') %>%
  left_join(MapScenario)


RPoVInc_tmp <- rgdx.param(AnaInc, "RPoV") %>%
  filter(R == "CHN",
         Ref %in% lis_ref ,
         Y%in% c(seq(2020,2050,5)))%>%
  dplyr::rename(valueInc = "RPoV") %>%
  left_join(MapScenario)


RPoV <- RPoVExp %>%
  left_join(RPoVInc_tmp) %>%
  rename.vars(from = 'RPoVExp', to = 'RPoV') %>%
  filter(scenario %in% c(lis_scenario_noex,lis_scenario_exmp,'Baseline'))%>%
  rename.vars(from = 'RPoV', to = 'value')%>%
  # select(-"TH") %>%
  mutate(TH1 = 'Relative poverty')

Headcount <- rbind(PoV,RPoV) %>%
  transform(GiniSet = factor(GiniSet, levels = c("SSP1","SSP2","SSP3","SSP4","SSP5")))


csv <- Headcount %>%
  filter(scenario %in% c(lis_scenario_noex,"Baseline"),
         # TH1 %in% c('Relative poverty','3.2-threshold'),
         Y %in% c('2020','2030','2040','2050')) %>%
  select(-c("Ref", "valueInc")) %>%
  spread(scenario, value) %>%
  write.csv(file = paste0(dir_csv,"71_Robustness_povertyheadcount.csv"))

# Plot 2 Poverty trend ----
pdata1 <- filter(Headcount,
                 scenario %in% c(lis_scenario_noex),
                 TH1 %in% c('Relative poverty',"3.2-threshold"),
                 Y %in% c('2020','2030','2040','2050'),
                 GiniSet %in% c("SSP1","SSP2","SSP3")) %>%
  mutate(linetype = GiniSet,
         group = paste0(scenario,"_",GiniSet))

pdata2 <- Headcount %>%
  filter(scenario %in% c(lis_scenario_noex),
         TH1 %in% c('Relative poverty',"3.2-threshold"),
         Y %in% c('2020','2030','2040','2050'),
         GiniSet %in% c("SSP1","SSP3")) %>%
  select(-"valueInc") %>%
  spread(GiniSet,value) %>%
  dplyr::rename("SSP3_tot" = SSP3) %>%
  mutate(SSP3 = SSP3_tot-SSP1) %>%
  gather(GiniSet, value, c("SSP1","SSP3")) %>%
  mutate(linetype = GiniSet)


p_72 <- ggplot() +
  geom_line(data = pdata1,
            mapping = aes(x = as.numeric(as.character(Y)), y = value/1000000,
                          # linetype = linetype,
                          group = GiniSet,
                          color = GiniSet)) +
  geom_area(data = pdata2,
             mapping = aes(x = as.numeric(as.character(Y)), y = value/1000000,
                           fill = fct_rev(factor(GiniSet, levels = c("SSP1","SSP3"))),
                           alpha = 0.2), 
            position = 'stack') + #, alpha = TRUE
  labs(title = "a)", # Poverty headcount Databases
       x = 'Year',
       y ='Million people',
       size=9)+
  facet_grid(TH1~scenario,scales = "free") +
  MyTheme  +
  theme(      panel.spacing.x = unit(0.8,"lines"),
              panel.spacing.y = unit(0.8,"lines")) +
  guides(fill = "none",
         alpha = "none",
         linetype = "none",
         color = guide_legend(title = "Estimation", ncol = 1, byrow = TRUE, size = 7)) +
  # scale_x_discrete(breaks=seq(2020, 2050, 5))  +
  scale_color_manual(values = palette_GiniSet) +
  scale_fill_manual(values = c("SSP1" = "white", #white
                               "SSP3" = "grey")) #+

p_72
ggsave(filename = paste0(dir_fig,"Fig SI 142 SensAna_Gini.png"),
       p_72,
       width = 14,height = 10, unit = "cm")



# Plot 3 Poverty Effects ----

Headcount_Inc <- Headcount %>%
  select(-c("Ref","value")) %>%
  mutate(Effect = 'Income')%>%
  na.omit()%>%
  spread(scenario, valueInc) %>%
  gather(scenario, value, all_of(lis_scenario_mitigation)) %>%
  mutate(change = value - Baseline) %>%
  select(-c("value","Baseline")) %>%
  dplyr::rename(value = "change")

Headcount_baseline <- Headcount %>%
  na.omit()%>%
  filter(scenario == "Baseline") %>%
  select(-c("Ref","scenario")) %>%
  dplyr::rename(BaselineExp = "value", BaselineInc = "valueInc")


Headcount_Exp <- Headcount %>%
  select(-"Ref") %>%
  rename.vars(from = c("value","valueInc"), to = c("Expenditure", "Income")) %>%
  # spread(Effect, value)%>%
  na.omit() %>%
  mutate(exp_sc = Expenditure-Income) %>%
  select(-c('Expenditure','Income')) %>%
  spread(scenario, exp_sc) %>%
  gather(scenario, value, all_of(lis_scenario_mitigation)) %>%
  mutate(change = value - Baseline) %>%
  mutate(Effect = 'Price_side') %>%
  cbind(str_split(.$scenario,"_",simplify = TRUE)) %>%
  rename.vars(from = c('1','2'), to = c('scenario1','scenario2')) %>%
  select(-c("value","Baseline"))

Headcount_Exp[which(Headcount_Exp$scenario2 != 'exempt'),]$scenario2 <- 'non_exempt'

Headcount_Exp_price <- Headcount_Exp %>%
  transform(scenario2 = factor(scenario2, levels = c('non_exempt','exempt'))) %>%
  select(-c('scenario')) %>%
  spread(scenario2,change) %>%
  rename.vars(from = c('non_exempt','exempt','scenario1'), 
              to = c('Total_price','Indirect_price','scenario'))%>%
  mutate(Direct_price = Total_price-Indirect_price) %>%
  gather(Effect, value, Total_price,Indirect_price,Direct_price) 

Headcount_eff <- Headcount_Inc %>%
  select(colnames(Headcount_Exp_price)) %>%
  rbind(Headcount_Exp_price) %>%
  transform(Effect = factor(Effect, levels = c("Income","Indirect_price","Direct_price","Total_price"))) %>%
  transform(GiniSet = factor(GiniSet, levels = c("SSP1","SSP2","SSP3","SSP4","SSP5")))
view(Headcount_eff)


csv <- Headcount_eff %>%
  # left_join(Headcount_baseline) %>%
  filter(scenario %in% c(lis_scenario_noex),
         Effect %in% c("Income",'Direct_price','Indirect_price'),
         Y %in% c('2030','2040',"2050"),
           TH1 %in% c('3.2-threshold',"Relative poverty")) %>%
  transform(GiniSet = factor(GiniSet, levels = c("SSP1","SSP2","SSP3","SSP4","SSP5"))) %>%
  spread(GiniSet, value) %>%
  write.csv(file = paste0(dir_csv,"72_povertyheadcountEffect.csv"))

## plot 73 ----
pdata <- filter(Headcount_eff,
                scenario %in% c("50%","NDC"),
                # scenario %in% lis_scenario_noex,
                Effect %in% c('Direct_price','Indirect_price'),
                Y %in% c('2030','2040',"2050"),
                TH1 %in% c('3.2-threshold')
) 

pdata1 <- filter(Headcount_eff,
                 scenario %in% c("50%","NDC"),
                 # scenario %in% lis_scenario_noex,
                 Effect %in% c('Income','Direct_price','Indirect_price'),
                 Y %in% c('2030','2040',"2050"),
                 TH1 %in% c('3.2-threshold'),
                 GiniSet == "SSP2") 
pdata2 <- filter(Headcount_eff,
                 scenario %in% c("50%","NDC"),
                 # scenario %in% lis_scenario_noex,
                 Effect %in% c('Income','Direct_price','Indirect_price'),
                 Y %in% c('2030','2040',"2050"),
                 TH1 %in% c('3.2-threshold'),
                 GiniSet %in% c("SSP1","SSP3"))  


p_73 <- ggplot() + 
  geom_line(data = pdata1,
            mapping = aes(x = Y, y = value/1000000, 
                          group = scenario,color = GiniSet)) +
  geom_point(data = pdata2,
             mapping = aes(x = Y, y = value/1000000,
                           color = GiniSet)) + 
  geom_abline(intercept = 0, slope = 0, colour = 'grey')+
  labs(title = "b)", # Added poverty headcount with carbon taxes 
       x = 'Year',
       y = 'Million people',
       size=10) +
  facet_grid(Effect~scenario, scales = "free") +
  MyTheme +
  guides(color = guide_legend(title = 'Estimation', ncol = 1, size = 7, byrow = TRUE)) +
  # scale_color_discrete(breaks = lis_scenario) +
  scale_color_manual(values = palette_GiniSet)

p_73

ggsave(filename = paste0(dir_fig,"Fig SI 143 SensAna_PovertyDecom.png"),
       p_73,
       width = 16,height = 12, unit = "cm")


## plot 731 ----
pdata <- filter(Headcount_eff,
                scenario %in% c("50%","NDC"),
                # scenario %in% lis_scenario_noex,
                Effect %in% c('Income','Direct_price','Indirect_price'),
                Y %in% c('2040',"2050"),
                TH1 %in% c('3.2-threshold'),
                GiniSet %in% c("SSP1","SSP2","SSP3")
) 
p_731 <- ggplot() + 
  geom_col(data = pdata,
            mapping = aes(x = GiniSet, y = value/1000000,fill = Effect),
           position = "fill") +
  geom_abline(intercept = 0, slope = 0, colour = 'grey')+
  labs(title = "c)", # Added poverty headcount with carbon taxes 
       x = 'Year',
       y = '%',
       size=10) +
  facet_grid(Y~scenario, scales = "free") +
  MyTheme +
  guides(color = guide_legend(title = 'Estimation', ncol = 1, size = 7, byrow = TRUE)) +
  # scale_color_discrete(breaks = lis_scenario) +
  scale_color_manual(values = palette_GiniSet)

p_731


ggsave(filename = paste0(dir_fig,"Fig SI 1431 SensAna_PovertyDecom_fill.png"),
       p_731,
       width = 16,height = 12, unit = "cm")



# Plot 4 EV ----
# ## EV price ----
lis_y <- seq(2020,2050,10)
PoutputNational <- rgdx.param(demand, "PoutputNational") %>%
  filter( Ref %in% lis_ref,
          Y %in% lis_y) %>%
  dplyr::rename(value = "PoutputNational") %>%
  left_join(MapI) %>%
  select(-"I")
CON_tmp <- F_DEC_tmp(demand,PoutputNational,lis_y, split_all = T)
CON_tmp_Phi <- CON_tmp %>%
  mutate(prj = "Phi")

PoutputNational_prj_GCD <- rgdx.param(demand_prj_GCD, "PoutputNational") %>%
  filter( Ref %in% lis_ref,
          Y %in% lis_y) %>%
  dplyr::rename(value = "PoutputNational") %>%
  left_join(MapI) %>%
  select(-"I")
CON_prj_GCD_tmp <- F_DEC_tmp(demand_prj_GCD,PoutputNational_prj_GCD,lis_y, split_all = T)
CON_prj_GCD <- CON_prj_GCD_tmp %>%
  mutate(prj = "GCD")


CON <- CON_tmp_Phi %>%
  rbind(CON_prj_GCD) %>%
  transform(DEC = as.numeric(DEC)) %>%
  left_join(MapDec) %>%
  left_join(MapScenario) %>%
  select(-c('Ref')) %>%
  transform(I = factor(I,levels = lis_I_abb)) %>%
  transform(DEC = factor(DEC,levels = lis_DEC)) %>%
  dplyr::rename(value = "value_seg")

PQ_tmp <- rgdx.param(demand, "PQchange") %>%
  mutate(prj = "Phi")

PQ_prj_GCD_tmp <- rgdx.param(demand_prj_GCD, "PQchange") %>%
  mutate(prj = "GCD")


PQ <- PQ_tmp %>%
  rbind(PQ_prj_GCD_tmp)%>%
  filter(Ref %in% lis_ref,
         Y %in% lis_y)%>%
  left_join(MapScenario) %>%
  left_join(MapI) %>%
  select(-c('Ref','I')) %>%
  dplyr::rename(I = "I_abb") %>%
  transform(I = factor(I,levels = lis_I_abb))

Price_baseline <- PQ %>%
  filter(scenario == 'Baseline') %>%
  dplyr::rename(PQ_baseline = "PQchange") %>%
  select(-scenario)

EV_total_tmp <- CON %>%
  select(-c('pop','quantile')) %>%
  na.omit() %>%
  spread(scenario,value) %>%
  gather(scenario,value, all_of(lis_scenario_mitigation)) %>%
  transform(DEC = factor(as.character(DEC), levels = lis_DEC)) %>%
  select(c("Y","GiniSet","DEC","I","prj","Baseline","scenario","value")) %>%
  left_join(Price_baseline) 

EV_numerator <- EV_total_tmp %>%
  mutate(numerator = (value*PQ_baseline-Baseline*PQ_baseline)) %>%
  select(c("Y","DEC","GiniSet","I","scenario","prj","R","numerator"))

EV_denominator <- EV_total_tmp %>%
  group_by(Y,DEC,scenario,GiniSet,R,prj) %>%
  dplyr::summarise(denominator = sum(Baseline*PQ_baseline),.groups = "drop") 
view(EV_denominator)


EVi_total <- EV_numerator %>%
  left_join(EV_denominator) %>%
  mutate(EVi = numerator/denominator) %>%
  transform(prj = factor(prj, levels = c("Phi","GCD")),
            Effect = "total")

EV_total <- EVi_total %>%
  group_by(Y,GiniSet,DEC,scenario,R,prj) %>%
  dplyr::summarise(EV = sum(EVi)) %>%
  mutate(group = paste0(scenario,"_",prj))  

csvEV <- EV_total %>%
  filter(Y %in% c('2030','2040','2050'),
         scenario %in% c("80%")) %>%
  select(-c("group")) %>%
  spread(prj, EV) %>%
  write.csv(file = paste0(dir_output,"73_EVtotal.csv"))

lis_y <- c('2020','2030','2040','2050')
EVi_pri_od <- rgdx.param(demand_prj_Phi, "EVi") %>%
  filter( Ref %in% lis_ref,
          Y %in% lis_y) %>%
  dplyr::rename(value = "EVi") %>%
  left_join(MapI) %>%
  select(-"I")
EVi_pri_tmp <- F_DEC_tmp(demand_prj_Phi,EVi_pri_od,lis_y, split_all = T)
EVi_pri_tmp_Phi <- EVi_pri_tmp %>%
  mutate(prj = "Phi")


EVi_pri_GCD_od <- rgdx.param(demand_prj_GCD, "EVi") %>%
  filter( Ref %in% lis_ref,
          Y %in% lis_y) %>%
  dplyr::rename(value = "EVi") %>%
  left_join(MapI) %>%
  select(-"I")
EVi_pri_GCD_tmp <- F_DEC_tmp(demand_prj_GCD,EVi_pri_GCD_od,lis_y, split_all = T)
EVi_pri_GCD <- EVi_pri_GCD_tmp %>%
  mutate(prj = "GCD")


EVi_pri <- EVi_pri_tmp_Phi %>%
  rbind(EVi_pri_GCD) %>%
  transform(prj = factor(prj, levels = c("Phi","GCD"))) %>%
  left_join(MapScenario)%>%
  select(-c("Ref","pop")) %>%
  mutate(#group = paste0(scenario,"_",prj),
    Effect = "price",
    R = "CHN") %>%
  dplyr::rename(EVi = value_seg)


EVi <- EVi_total %>%
  select(colnames(EVi_pri)) %>%
  rbind(EVi_pri) 

EV <- EVi %>%
  group_by(R,GiniSet,Effect,Y,DEC,scenario,prj) %>%
  dplyr::summarise(EV = sum(EVi)) 


## plot ----

pdata <- EV %>%
  filter(GiniSet %in% c("SSP1","SSP2","SSP3"),
         scenario %in% c("50%","NDC"),
         # Effect == "total",
         prj == "Phi",
         Y %in% c("2030","2040","2050")) %>%
  mutate(group = paste0(scenario,"_",GiniSet))
palette_Sc <- c('50%' = '#00BFC4', #'#CCFF99', #'#CD2626',
                'NDC' = '#C77CFF')#'#009966') #'#9932CC')

p_74 <- ggplot() + 
  geom_line(pdata,
            mapping = aes(x = factor(DEC,levels = lis_DEC), y = EV*100, 
                          color = factor(scenario, levels = lis_scenario), 
                          linetype = GiniSet,
                          group = group)) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey')+
  labs(title = "d)", # Added poverty headcount with carbon taxes 
       x = 'Year',
       y = 'Million people',
       size=10) +
  facet_grid(Effect~Y, scales = "free") +
  MyTheme +
  theme(      panel.spacing.x = unit(0.3,"lines"),
              panel.spacing.y = unit(0.3,"lines")) +
  guides(color = guide_legend(title = 'Scenario', ncol = 1, size = 7, byrow = TRUE),
         linetype = guide_legend(title = 'Estimation', ncol = 1, size = 7, byrow = TRUE)) +
  # guides() +
  # scale_color_discrete(breaks = lis_scenario) +
  scale_color_manual(values = palette_Sc) +
  scale_linetype_manual(values = palette_line_GiniSet)  

p_74


## Paste ----
p_72
p_73
p_731
p_74

png(filename = paste0(dir_fig,'tiff/SI 141 SensAna.png'),
    res = 300,
    width = 19,height = 24,
    units = "cm")

grid.newpage()  ###新建图表版面
pushViewport(viewport(layout = grid.layout(3,6))) ####�?版面�?�?2*2矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
print(p_72, vp = vplayout(1,1:6))   ###�??�?1,1)�?(1,2)�?位置画图chart3
print(p_73, vp = vplayout(2,1:3))   ###�??�?1,1)�?(1,2)�?位置画图chart3
print(p_731, vp = vplayout(2,4:6))     ###�?(2,1)�?位置画图chart2
print(p_74, vp = vplayout(3,2:5))     ###�?(2,1)�?位置画图chart2
# Fig 3 EV_total 1300 1100
dev.off()



## EPS ----
setEPS()
postscript(paste0(dir_fig,'eps/SI 141 SensAna.eps'),
           width = 20,height = 16)

grid.newpage()  ###新建图表版面
pushViewport(viewport(layout = grid.layout(2,2))) ####�?版面�?�?2*2矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
print(p_72, vp = vplayout(1,1:2))   ###�??�?1,1)�?(1,2)�?位置画图chart3
print(p_73, vp = vplayout(2,1))   ###�??�?1,1)�?(1,2)�?位置画图chart3
print(p_731, vp = vplayout(2,2))     ###�?(2,1)�?位置画图chart2

dev.off() #



# 2. Alternative dataset ----
# Database
AIDADSCalib_prj_Phi <- "../../data/SensitivityAnalysis/ProjectionPhi/AIDADSCalib/AIDADSCalibNationResults.gdx"
AnaExp_prj_Phi <- "../../data/SensitivityAnalysis/ProjectionPhi/AnalysisExpenditure.gdx"
AnaInc_prj_Phi <- "../../data/SensitivityAnalysis/ProjectionPhi/AnalysisIncome.gdx"
demand_prj_Phi <- "../../data/SensitivityAnalysis/ProjectionPhi/demand/country/CHN.gdx"
pricechange_prj_Phi <- "../../data/SensitivityAnalysis/ProjectionPhi/demand/pricechange.gdx"

AIDADSCalib_prj_GCD <- "../../data/SensitivityAnalysis/ProjectionGCD/AIDADSCalib/AIDADSCalibNationResults.gdx"
AnaExp_prj_GCD <- "../../data/SensitivityAnalysis/ProjectionGCD/AnalysisExpenditure.gdx"
AnaInc_prj_GCD <- "../../data/SensitivityAnalysis/ProjectionGCD/AnalysisIncome.gdx"
demand_prj_GCD <- "../../data/SensitivityAnalysis/ProjectionGCD/demand/country/CHN.gdx"
pricechange_prj_GCD <- "../../data/SensitivityAnalysis/ProjectionGCD/demand/pricechange.gdx"

AnaInc <- "../../data/SensitivityAnalysis/DatabasePhi/AnalysisIncome.gdx"

AIDADSCalib_prj_GCD <- "../../data/PHIoutput/AIDADSCalibNationResults_DF.gdx"
## Palette ----
palette_Sc <- c('50%' = '#00BFC4', #'#CCFF99', #'#CD2626',
                'NDC' = '#C77CFF')#'#009966') #'#9932CC')


# Fig 75 Relative PovHeadcount ----
PoVExp_tmp <- rgdx.param(AnaExp_prj_Phi, "PoVExp") %>%
  filter(R == "CHN",
         Ref %in% lis_ref ,
         Y%in% c(seq(2020,2050,5)))%>%
  mutate(prj = 'Phi') %>%
  left_join(MapScenario)


PoVExp_prj_GCD_tmp <- rgdx.param(AnaExp_prj_GCD, "PoVExp") %>%
  filter(R == "CHN",
         Ref %in% lis_ref ,
         Y%in% c(seq(2020,2050,5)))%>%
  mutate(prj = 'GCD') %>%
  left_join(MapScenario)

PoVInc_tmp <- rgdx.param(AnaInc, "PoV") %>%
  filter(R == "CHN",
         Ref %in% lis_ref,
         Y %in% c(seq(2020,2050,5))) %>%
  dplyr::rename(valueInc = "PoV") %>%
  left_join(MapScenario)


PoV <- PoVExp_tmp %>%
  rbind(PoVExp_prj_GCD_tmp) %>%
  left_join(PoVInc_tmp) %>%
  rename.vars(from = 'PoVExp', to = 'PoV') %>%
  mutate(TH1 = recode(TH, 
                      'pop_1.9' = lis_TH[1],
                      'pop_3.2' = lis_TH[2],
                      'pop_5.5' = lis_TH[3])) %>%
  select(-"TH") %>%
  filter(scenario %in% c(lis_scenario_noex,lis_scenario_exmp,'Baseline'))%>%
  rename.vars(from = 'PoV', to = 'value') 


# Relative poverty 
RPoVExp_tmp <- rgdx.param(AnaExp_prj_Phi, "RPoVExp") %>%
  filter(R == "CHN",
         Ref %in% lis_ref ,
         Y%in% c(seq(2020,2050,5)))%>%
  mutate(prj = 'Phi') %>%
  left_join(MapScenario)

RPoVExp_prj_GCD_tmp <- rgdx.param(AnaExp_prj_GCD, "RPoVExp") %>%
  filter(R == "CHN",
         Ref %in% lis_ref ,
         Y%in% c(seq(2020,2050,5)))%>%
  mutate(prj = 'GCD') %>%
  left_join(MapScenario)


RPoVInc_tmp <- rgdx.param(AnaInc, "RPoV") %>%
  filter(R == "CHN",
         Ref %in% lis_ref ,
         Y%in% c(seq(2020,2050,5)))%>%
  dplyr::rename(valueInc = "RPoV") %>%
  left_join(MapScenario)



RPoV <- RPoVExp_tmp %>%
  rbind(RPoVExp_prj_GCD_tmp) %>%
  left_join(RPoVInc_tmp) %>%
  rename.vars(from = 'RPoVExp', to = 'RPoV') %>%
  filter(scenario %in% c(lis_scenario_noex,lis_scenario_exmp,'Baseline'))%>%
  rename.vars(from = 'RPoV', to = 'value')%>%
  # select(-"TH") %>%
  mutate(TH1 = 'Relative poverty')



Headcount <- rbind(PoV,RPoV) %>%
  transform(prj = factor(prj, levels = c("Phi","GCD"))) %>%
  mutate(group = paste0(scenario,"_",prj))
  



csv <- filter(Headcount,
              scenario %in% c(lis_scenario_noex,"Baseline"),
              TH1 %in% c('Relative poverty','3.2-threshold'),
              Y %in% c('2020','2030','2040','2050')) %>%
  select(-c("Ref","valueInc")) %>%
  spread(scenario, value) %>%
  write.csv(file = paste0(dir_output,"71_Robustness_povertyheadcount.csv"))

## Plot ----
pdata1 <- filter(Headcount,
                scenario %in% c("50%","NDC"),
                TH1 %in% c('Relative poverty',"3.2-threshold"),
                Y %in% c('2020','2030','2040','2050'),
                GiniSet %in% c("SSP2"),
                prj %in% c("Phi")) %>%
  mutate(group = paste0(scenario,"_",prj))

pdata2 <- filter(Headcount,
                 scenario %in% c("50%","NDC"),
                 TH1 %in% c('Relative poverty',"3.2-threshold"),
                 Y %in% c('2020','2030','2040','2050'),
                 GiniSet %in% c("SSP2"),
                 prj %in% c("GCD")) %>%
  mutate(group = paste0(scenario,"_",prj))


p_75 <- ggplot() +
  geom_line(data = pdata1,
            mapping = aes(x = Y, y = value/1000000, 
                          color = prj,
                          group = group)) +
  geom_point(data = pdata2,
             mapping = aes(x = Y, y = value/1000000,
                           color = prj)) + #, shape = prj
  labs(title = "a)", # Poverty headcount Databases
       x = 'Year',
       y ='Million people',
       size=9)+
  facet_grid(TH1~scenario,scales = "free") +
  MyTheme  +
  guides(color = guide_legend(title = "Database", ncol = 1, byrow = TRUE, size = 7),
         linetype = guide_legend(title = "Scenario", ncol = 1, byrow = TRUE, size = 7)) +
  scale_color_manual(values = palette_prj) +
  scale_x_discrete(breaks=seq(2020, 2050, 5))  
  
p_75


# Fig 76 Poverty headcount by Effect ----
Headcount_Inc <- Headcount %>%
  select(-c("Ref","value","group")) %>%
  mutate(Effect = 'Income')%>%
  na.omit()%>%
  spread(scenario, valueInc) %>%
  gather(scenario, value, all_of(lis_scenario_mitigation)) %>%
  mutate(change = value - Baseline) %>%
  select(-c("value","Baseline")) %>%
  dplyr::rename(value = "change")

Headcount_baseline <- Headcount %>%
  na.omit()%>%
  filter(scenario == "Baseline") %>%
  select(-c("Ref","scenario","group")) %>%
  dplyr::rename(BaselineExp = "value", BaselineInc = "valueInc")
view(Headcount_baseline)

Headcount_Exp <- Headcount %>%
  select(-c("Ref","group")) %>%
  rename.vars(from = c("value","valueInc"), to = c("Expenditure", "Income")) %>%
  # spread(Effect, value)%>%
  na.omit() %>%
  mutate(exp_sc = Expenditure-Income) %>%
  select(-c('Expenditure','Income')) %>%
  spread(scenario, exp_sc) %>%
  gather(scenario, value, all_of(lis_scenario_mitigation)) %>%
  mutate(change = value - Baseline) %>%
  mutate(Effect = 'Price_side') %>%
  cbind(str_split(.$scenario,"_",simplify = TRUE)) %>%
  rename.vars(from = c('1','2'), to = c('scenario1','scenario2')) %>%
  select(-c("value","Baseline"))
view(Headcount_Exp)


Headcount_Exp[which(Headcount_Exp$scenario2 != 'exempt'),]$scenario2 <- 'non_exempt'

Headcount_Exp_price <- Headcount_Exp %>%
  transform(scenario2 = factor(scenario2, levels = c('non_exempt','exempt'))) %>%
  select(-c('scenario')) %>%
  spread(scenario2,change) %>%
  rename.vars(from = c('non_exempt','exempt','scenario1'), 
              to = c('Total_price','Indirect_price','scenario'))%>%
  mutate(Direct_price = Total_price-Indirect_price) %>%
  gather(Effect, value, Total_price,Indirect_price,Direct_price) 
view(Headcount_Exp_price)

Headcount_eff <- Headcount_Exp_price %>%
  rbind(Headcount_Inc) %>%
  transform(Effect = factor(Effect, levels = c("Income","Indirect_price","Direct_price","Total_price")))
view(Headcount_eff)


csv <- Headcount_eff %>%
  left_join(Headcount_baseline) %>%
  filter(scenario %in% c("80%"),
         Effect %in% c("Income",'Direct_price','Indirect_price'),
         Y %in% c('2030','2040',"2050"),
         TH1 %in% c('3.2-threshold')) %>%
  transform(prj = factor(prj, levels = c("Phi","A","B"))) %>%
  spread(prj, value) %>%
  write.csv(file = paste0(dir_output,"72_povertyheadcountEffect.csv"))

# Headcount_eff <- rbind(Headcount_Exp_price,Headcount_Inc) %>%
#   transform(Effect = factor(Effect, levels= c('Income','Total_price','Indirect_price','Direct_price')))

## plot  ----

pdata <- filter(Headcount_eff,
                scenario %in% c("80%"),
                Effect %in% c('Direct_price','Indirect_price'),
                Y %in% c('2030','2040',"2050"),
                TH1 %in% c('3.2-threshold'),
                GiniSet %in% c("SSP2")
) %>%
  transform(prj = factor(prj, levels = c("Phi","GCD")))  %>%
  mutate(group = paste0(scenario,"_",prj))

pdata1 <- filter(Headcount_eff,
                 scenario %in% c("50%","NDC"),
                 Effect %in% c('Income','Direct_price','Indirect_price'),
                 Y %in% c('2030','2040',"2050"),
                 TH1 %in% c('3.2-threshold'),
                 prj == "Phi",
                 GiniSet %in% c("SSP2"))  %>%
  transform(prj = factor(prj, levels = c("Phi","GCD")))  %>%
  mutate(group = paste0(scenario,"_",prj))

pdata2 <- filter(Headcount_eff,
                 scenario %in% c("50%","NDC"),
                 Effect %in% c('Income','Direct_price','Indirect_price'),
                 Y %in% c('2030','2040',"2050"),
                 TH1 %in% c('3.2-threshold'),
                 prj %in% c("GCD"),
                 GiniSet %in% c("SSP2"))  %>%
  transform(prj = factor(prj, levels = c("Phi","GCD")))  %>%
  mutate(group = paste0(scenario,"_",prj))
 

p_76 <- ggplot() + 
  geom_line(data = pdata1,
            mapping = aes(x = Y, y = value/1000000, 
                          group = group,
                          # linetype = scenario,
                          color = prj)) +
  geom_point(data = pdata2,
             mapping = aes(x = Y, y = value/1000000,
                           color = prj)) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey')+
  labs(title = "b)", # Added poverty headcount with carbon taxes 
    x = 'Year',
    y = 'Million people',
    size=10) +
  facet_grid(scenario~Effect, scales = "free") +
  MyTheme +
  guides(color = guide_legend(title = 'Database', ncol = 1, size = 7, byrow = TRUE)) +
  # scale_color_discrete(breaks = lis_scenario) +
  scale_color_manual(values = palette_prj)
  
p_76


# Tiff output ----
png(filename = paste0(dir_fig,"Fig 71. Povertyheadcount.png"),
     res = 300,
     width = 16,height = 15, unit = "cm")

grid.newpage()  ###新建图表版面
pushViewport(viewport(layout = grid.layout(10,7))) ####???版面??????2*2矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}

print(p_71, vp = vplayout(1:6,1:7))   ###??????1,1)???(1,2)???位置画图chart3
print(p_72, vp = vplayout(7:10,2:6))     ###???(2,1)???位置画图chart2

dev.off()#




# Fig 77 Welfare loss total ----
lis_y <- c('2020','2030','2040','2050')
PoutputNational <- rgdx.param(demand, "PoutputNational") %>%
  filter( Ref %in% lis_ref,
          Y %in% lis_y) %>%
  dplyr::rename(value = "PoutputNational") %>%
  left_join(MapI) %>%
  select(-"I")
CON_tmp <- F_DEC_tmp(demand,PoutputNational,lis_y, split_all = T)
CON_tmp_Phi <- CON_tmp %>%
  mutate(prj = "Phi")

PoutputNational_prj_GCD <- rgdx.param(demand_prj_GCD, "PoutputNational") %>%
  filter( Ref %in% lis_ref,
          Y %in% lis_y) %>%
  dplyr::rename(value = "PoutputNational") %>%
  left_join(MapI) %>%
  select(-"I")
CON_prj_GCD_tmp <- F_DEC_tmp(demand_prj_GCD,PoutputNational_prj_GCD,lis_y, split_all = T)
CON_prj_GCD <- CON_prj_GCD_tmp %>%
  mutate(prj = "GCD")


CON <- CON_tmp_Phi %>%
  rbind(CON_prj_GCD) %>%
  transform(DEC = as.numeric(DEC)) %>%
  left_join(MapDec) %>%
  left_join(MapScenario) %>%
  select(-c('Ref')) %>%
  transform(I = factor(I,levels = lis_I_abb)) %>%
  transform(DEC = factor(DEC,levels = lis_DEC)) %>%
  dplyr::rename(value = "value_seg")

PQ_tmp <- rgdx.param(demand, "PQchange") %>%
  mutate(prj = "Phi")

PQ_prj_GCD_tmp <- rgdx.param(demand_prj_GCD, "PQchange") %>%
  mutate(prj = "GCD")


PQ <- PQ_tmp %>%
  rbind(PQ_prj_GCD_tmp)%>%
  filter(Ref %in% lis_ref,
         Y %in% lis_y)%>%
  left_join(MapScenario) %>%
  left_join(MapI) %>%
  select(-c('Ref','I')) %>%
  dplyr::rename(I = "I_abb") %>%
  transform(I = factor(I,levels = lis_I_abb))

Price_baseline <- PQ %>%
  filter(scenario == 'Baseline') %>%
  dplyr::rename(PQ_baseline = "PQchange") %>%
  select(-scenario)

EV_total_tmp <- CON %>%
  select(-c('pop','quantile')) %>%
  na.omit() %>%
  spread(scenario,value) %>%
  gather(scenario,value, all_of(lis_scenario_mitigation)) %>%
  transform(DEC = factor(as.character(DEC), levels = lis_DEC)) %>%
  select(c("Y","GiniSet","DEC","I","prj","Baseline","scenario","value")) %>%
  left_join(Price_baseline) 

EV_numerator <- EV_total_tmp %>%
  mutate(numerator = (value*PQ_baseline-Baseline*PQ_baseline)) %>%
  select(c("Y","DEC","GiniSet","I","scenario","prj","R","numerator"))

EV_denominator <- EV_total_tmp %>%
  group_by(Y,DEC,scenario,GiniSet,R,prj) %>%
  dplyr::summarise(denominator = sum(Baseline*PQ_baseline),.groups = "drop") 
view(EV_denominator)


EVi_total <- EV_numerator %>%
  left_join(EV_denominator) %>%
  mutate(EVi = numerator/denominator) %>%
  transform(prj = factor(prj, levels = c("Phi","GCD")),
            Effect = "total")

EV_total <- EVi_total %>%
  group_by(Y,GiniSet,DEC,scenario,R,prj) %>%
  dplyr::summarise(EV = sum(EVi)) %>%
  mutate(group = paste0(scenario,"_",prj))  

csvEV <- EV_total %>%
  filter(Y %in% c('2030','2040','2050'),
                scenario %in% c("80%")) %>%
  select(-c("group")) %>%
  spread(prj, EV) %>%
  write.csv(file = paste0(dir_output,"73_EVtotal.csv"))



## Plot ----
pdata1 <- filter(EV_total,
                Y %in% c('2030','2040','2050'),
                scenario %in% c("50%","NDC"),
                prj %in% c("Phi","GCD"),
                GiniSet %in% c("SSP2"))


p_77 <- ggplot() + 
  geom_line(pdata1,
            mapping = aes(x = factor(DEC,levels = lis_DEC), y = EV*100, 
                          color = factor(scenario, levels = lis_scenario), 
                          linetype = prj,
                          group = group)) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = "c) ", 
       x = "Income decile", y = "%")+
  facet_grid(~Y, scales = 'free') +
  MyTheme +
  cowplot::panel_border(color = "grey85", linetype = 1) +
  guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE),
         linetype = guide_legend(title = 'Database', ncol = 1, byrow = TRUE))  +
  scale_linetype_manual(values = palette_line_prj) +
  scale_color_manual(values = palette_Sc)

p_77
ggsave(filename = paste0(dir_fig,"Figure 77 residential energy poverty_change in median compared to 2020.png"),
       p_77,
       width = 9, height = 4, units = "cm")



# Fig 78 Welfare price ----
lis_y <- c('2020','2030','2040','2050')
EVi_pri_od <- rgdx.param(demand_prj_Phi, "EVi") %>%
  filter( Ref %in% lis_ref,
          Y %in% lis_y) %>%
  dplyr::rename(value = "EVi") %>%
  left_join(MapI) %>%
  select(-"I")
EVi_pri_tmp <- F_DEC_tmp(demand_prj_Phi,EVi_pri_od,lis_y, split_all = T)
EVi_pri_tmp_Phi <- EVi_pri_tmp %>%
  mutate(prj = "Phi")


EVi_pri_GCD_od <- rgdx.param(demand_prj_GCD, "EVi") %>%
  filter( Ref %in% lis_ref,
          Y %in% lis_y) %>%
  dplyr::rename(value = "EVi") %>%
  left_join(MapI) %>%
  select(-"I")
EVi_pri_GCD_tmp <- F_DEC_tmp(demand_prj_GCD,EVi_pri_GCD_od,lis_y, split_all = T)
EVi_pri_GCD <- EVi_pri_GCD_tmp %>%
  mutate(prj = "GCD")


EVi_pri <- EVi_pri_tmp_Phi %>%
  rbind(EVi_pri_GCD) %>%
  transform(prj = factor(prj, levels = c("Phi","GCD"))) %>%
   left_join(MapScenario)%>%
  select(-c("Ref","pop")) %>%
  mutate(#group = paste0(scenario,"_",prj),
         Effect = "price",
         R = "CHN") %>%
  dplyr::rename(EVi = value_seg)


csvEVi <- filter(EVi_pri,
                Y %in% c('2030','2040','2050'),
                scenario %in% c("50%","NDC")) %>%
  # select(-c("pop","group")) %>%
  spread(prj, EVi) %>%
  write.csv(file = paste0(dir_output,"74_CSVEVprice.csv"))


EVi <- EVi_total %>%
  select(colnames(EVi_pri)) %>%
  rbind(EVi_pri) %>%
  mutate(group = paste0(scenario,"_",prj))

EV <- EVi %>%
  group_by(R,GiniSet,Effect,Y,DEC,scenario,prj) %>%
  dplyr::summarise(EV = sum(EVi), group = group) 


## Plot ----
pdata_price <- EV %>%
  filter(Effect == "price",
         Y %in% c('2030','2040','2050'),
         scenario %in% c("50%","NDC"),
         prj %in% c("Phi","GCD"),
         GiniSet %in% c("SSP2")) 

pdata_total <- EV %>%
  filter(Effect == "total",
         Y %in% c('2030','2040','2050'),
         scenario %in% c("50%","NDC"),
         prj %in% c("Phi","GCD"),
         GiniSet %in% c("SSP2"))

pdata <- pdata_price %>%
  select(colnames(pdata_total)) %>%
  rbind(pdata_total) %>%
  filter(Effect == "price")

p_78 <- ggplot() + 
  geom_line(pdata,
            mapping = aes(x = factor(DEC,levels = lis_DEC), y = EV*100, 
                          color = factor(scenario, levels = lis_scenario), 
                          linetype = prj,
                          group = group)) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = "d) ", # EV ratio decomposition
       # subtitle = "Total (income effect + price effect)",
       x = "Income decile", y = "%")+
  facet_grid(~Y, scales = 'free') +
  MyTheme +
  cowplot::panel_border(color = "grey85", linetype = 1)+
  guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE),
         linetype = guide_legend(title = 'Database', ncol = 1, byrow = TRUE))  +
  scale_linetype_manual(values = palette_line_prj) +
  scale_color_manual(values = palette_Sc) 
  # scale_fill_manual(values = palette_Sc)

p_78

## Paste ----
p_75
p_76
p_77
p_78

png(filename = paste0(dir_fig,'tiff/SI 142 SensAna_HBS.png'),
    res = 300,
    width = 18,height = 12,
    units = "cm")

grid.newpage()  ###新建图表版面
pushViewport(viewport(layout = grid.layout(5,2))) ####�?版面�?�?2*2矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
print(p_75, vp = vplayout(1:3,1))   ###�??�?1,1)�?(1,2)�?位置画图chart3
print(p_76, vp = vplayout(1:3,2))   ###�??�?1,1)�?(1,2)�?位置画图chart3
print(p_77, vp = vplayout(4:5,1))     ###�?(2,1)�?位置画图chart2
print(p_78, vp = vplayout(4:5,2))     ###�?(2,1)�?位置画图chart2
# Fig 3 EV_total 1300 1100
dev.off()



## EPS ----
setEPS()
postscript(paste0(dir_fig,'eps/SI 142 SensAna_HBS.eps'),
           width = 20,height = 16)

grid.newpage()  ###新建图表版面
pushViewport(viewport(layout = grid.layout(5,2))) ####�?版面�?�?2*2矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
print(p_75, vp = vplayout(1:3,1))   ###�??�?1,1)�?(1,2)�?位置画图chart3
print(p_76, vp = vplayout(1:3,2))   ###�??�?1,1)�?(1,2)�?位置画图chart3
print(p_77, vp = vplayout(4:5,1))     ###�?(2,1)�?位置画图chart2
print(p_78, vp = vplayout(4:5,2))     ###�?(2,1)�?位置画图chart2

dev.off() #



# Fig 75 EV Decomposition ----
## Plot 75 HWE ----
pdata_total <- EVi %>%
  filter(GiniSet == "SSP2",
         Effect == "total") %>%
  mutate(group = paste0(scenario,"_",prj))  


pdata_price <- EVi %>%
  filter(GiniSet == "SSP2",
         Effect == "price") %>%
  mutate(group = paste0(scenario,"_",prj)) 


pdata <- pdata_total %>%
  rbind(pdata_price)%>%
  filter(Y %in% c('2030','2040','2050'),
         scenario %in% c("50%","NDC"),
         I == "HWE")
 

p_75 <- ggplot() + 
  geom_line(data = pdata,
            mapping = aes(x = factor(DEC,levels = lis_DEC), y = EVi*100, 
                          color = factor(scenario, levels = lis_scenario), 
                          linetype = prj,
                          group = group)) +
  geom_point(data = pdata,
            mapping = aes(x = factor(DEC,levels = lis_DEC), y = EVi*100, 
                          color = factor(scenario, levels = lis_scenario)), 
            shape = 1, size = 1) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = "b) ", # EV ratio decomposition
       # subtitle = "Total (income effect + price effect)",
       x = "Income decile", y = "%")+
  facet_grid(Effect~Y, scales = 'free') +
  MyTheme+
  cowplot::panel_border(color = "grey85", linetype = 1)+
  guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE),
         linetype = guide_legend(title = 'Database', ncol = 1, byrow = TRUE))  +
  scale_linetype_manual(values = palette_line_prj) +
  scale_color_manual(values = palette_Sc) 
  
p_75



# Plor 76 FNB ----
pdata_total <- EVi %>%
  filter(GiniSet == "SSP2",
         Effect == "total") %>%
  mutate(group = paste0(scenario,"_",prj))  


pdata_price <- EVi %>%
  filter(GiniSet == "SSP2",
         Effect == "price") %>%
  mutate(group = paste0(scenario,"_",prj)) 


pdata <- pdata_total %>%
  rbind(pdata_price)%>%
  filter(Y %in% c('2030','2040','2050'),
         scenario %in% c("50%","NDC"),
         I == "FNB")


p_76 <- ggplot() + 
  geom_line(data = pdata,
            mapping = aes(x = factor(DEC,levels = lis_DEC), y = EVi*100, 
                          color = factor(scenario, levels = lis_scenario), 
                          linetype = prj,
                          group = group)) +
  geom_point(data = pdata,
             mapping = aes(x = factor(DEC,levels = lis_DEC), y = EVi*100, 
                           color = factor(scenario, levels = lis_scenario)), 
             shape = 1, size = 1) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = "c) ", # EV ratio decomposition
       # subtitle = "Total (income effect + price effect)",
       x = "Income decile", y = "%")+
  facet_grid(Effect~Y, scales = 'free') +
  MyTheme+
  cowplot::panel_border(color = "grey85", linetype = 1)+
  guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE),
         linetype = guide_legend(title = 'Database', ncol = 1, byrow = TRUE))  +
  scale_linetype_manual(values = palette_line_prj) +
  scale_color_manual(values = palette_Sc) 

p_76


# png output ----
png(filename = paste0(dir_fig,"Fig 73. EV Sensitivity of the total distributional effects.png"),
     res = 300,
     width = 12,height = 16, unit = "cm")

grid.newpage()  ###新建图表版面
pushViewport(viewport(layout = grid.layout(2,1))) ####???版面??????2*2矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}

print(p_74, vp = vplayout(1,1))     ###???(2,1)???位置画图chart2
print(p_75, vp = vplayout(2,1))     ###???(2,1)???位置画图chart2

dev.off()#

# THE END ----

# 3. Others ----


# Fig 77 Income elasticity ----
lis_y <- c('2020','2030','2040','2050')
IncomeEla_od <- rgdx.param(demand_prj_Phi, "IncomeEla") %>%
  filter( Ref %in% lis_ref,
          Y %in% lis_y) %>%
  dplyr::rename(value = "IncomeEla") %>%
  left_join(MapI) %>%
  select(-"I") %>%
  mutate(GiniSet = "SSP2")
IncomeEla_tmp <- F_DEC_tmp(demand_prj_Phi,IncomeEla_od,lis_y, split_all = T)
IncomeEla_tmp_Phi <- IncomeEla_tmp %>%
  mutate(prj = "Phi")

IncomeEla_GCD_od <- rgdx.param(demand_prj_GCD, "IncomeEla") %>%
  filter( Ref %in% lis_ref,
          Y %in% lis_y) %>%
  dplyr::rename(value = "IncomeEla") %>%
  left_join(MapI) %>%
  select(-"I") %>%
  mutate(GiniSet = "SSP2")
IncomeEla_tmp <- F_DEC_tmp(demand_prj_GCD,IncomeEla_GCD_od,lis_y, split_all = T)
IncomeEla_tmp_GCD <- IncomeEla_tmp %>%
  mutate(prj = "GCD")


IncomeEla <- IncomeEla_tmp_Phi %>%
  rbind(IncomeEla_tmp_GCD) 


## plot ----
pdata_total <- EVi %>%
  filter(GiniSet == "SSP2",
         Effect == "total") %>%
  mutate(group = paste0(scenario,"_",prj))  


pdata_price <- EVi %>%
  filter(GiniSet == "SSP2",
         Effect == "price") %>%
  mutate(group = paste0(scenario,"_",prj)) 


pdata <- IncomeEla %>%
  filter(Y %in% c('2030','2040','2050'),
         scenario %in% c("50%","NDC"),
         I == "FNB")


p_77 <- ggplot() + 
  geom_line(data = pdata,
            mapping = aes(x = factor(DEC,levels = lis_DEC), y = EVi*100, 
                          color = factor(scenario, levels = lis_scenario), 
                          linetype = prj,
                          group = group)) +
  geom_point(data = pdata,
             mapping = aes(x = factor(DEC,levels = lis_DEC), y = EVi*100, 
                           color = factor(scenario, levels = lis_scenario)), 
             shape = 1, size = 1) +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = "c) ", # EV ratio decomposition
       # subtitle = "Total (income effect + price effect)",
       x = "Income decile", y = "%")+
  facet_grid(Effect~Y, scales = 'free') +
  MyTheme+
  cowplot::panel_border(color = "grey85", linetype = 1)+
  guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE),
         linetype = guide_legend(title = 'Database', ncol = 1, byrow = TRUE))  +
  scale_linetype_manual(values = palette_line_prj) +
  scale_color_manual(values = palette_Sc) 

p_77


## Fig 76 Phi B Welfare decomposition ----
# Plot ----
MapEneCombine <- data.frame(c('FNB','ATN','CFW','HSG','WTR',
                              'SLD','GAS','LQD','ELE','BIO','OTH','HWE',
                              'FHE','HLT','TRN','CMN','REC','EDC','REH','MGS'),
                            c('FNB','ATN','CFW','HSG','WTR',
                              'SLD','GAS','LQD','ELE','BIO','OTH','HWE',
                              'FHE','HLT','TRN','CMN','REC','EDC','REH','MGS')) %>%
  gdata::rename.vars(from = colnames(.), to = c("I","COM"))

pdata <- EVi_total %>%
  filter(Y %in% c('2030','2040','2050'),
         scenario %in% c("80%")) %>%
  left_join(MapEneCombine) %>%
  filter(COM %in% c('HSG','WTR',
                    'SLD','GAS','LQD','ELE','BIO','OTH','HWE'),
         prj != "A")


p_76 <- ggplot(data = pdata,
               mapping = aes(x = DEC, y = EVi*100, 
                             fill = fct_rev(factor(COM, levels = lis_COM)))) + 
  geom_col(position="stack") +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = "a) ", # EV ratio decomposition
       # subtitle = "Total (income effect + price effect)",
       x = "Income decile", y = "%")+
  facet_grid(Y~prj, scales = 'free') +
  MyTheme_nolegend +
  cowplot::panel_border(color = "grey85", linetype = 1)+
  guides(fill = guide_legend(title = 'Commodity', ncol = 1, byrow = TRUE))  +
  # scale_fill_manual(values = palette_I_abb)
  scale_fill_manual(values = palette_COM)

p_76



## Fig 77 Phi A Welfare price decomposition ----

# plot ----
pdata <- filter(Decile_EVi,
                Y %in% c('2030','2040','2050'),
                scenario %in% c("80%")) %>%
  left_join(MapEneCombine) %>%
  filter(COM %in% c('HSG', 'WTR', 
                    'SLD','GAS','LQD','ELE','BIO','OTH',
                    'HWE'),
         prj != "A")


p_77 <-  ggplot(data = pdata,
                mapping = aes(x = DEC, y = EV*100, 
                              fill = fct_rev(factor(COM, levels = lis_COM)))) + 
  geom_col(position="stack") +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = "b) ", # EV ratio decomposition
       # subtitle = "Total (income effect + price effect)",
       x = "Income decile", y = "%")+
  facet_grid(Y~prj, scales = 'free') +
  MyTheme +
  cowplot::panel_border(color = "grey85", linetype = 1)+
  guides(fill = guide_legend(title = 'Commodity', ncol = 1, byrow = TRUE))  +
  # scale_fill_manual(values = palette_I_abb)
  scale_fill_manual(values = palette_COM)

p_77





# Tiff output ----
png(filename = paste0(dir_fig,"Fig 74. PhiB EV decomposition.png"),
     res = 300,
     width = 16,height = 10, unit = "cm")

grid.newpage()  ###新建图表版面
pushViewport(viewport(layout = grid.layout(1,9))) ####???版面??????2*2矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}

print(p_76, vp = vplayout(1,1:4))     ###???(2,1)???位置画图chart2
print(p_77, vp = vplayout(1,5:9))     ###???(2,1)???位置画图chart2

dev.off()#


# Fig 78 national calibration ----
## Data ----

Calib_PoutputNational <- AIDADSCalib %>%
  rgdx.param("PoutputNational")   %>%
  rename.vars(from = colnames(.), 
              to = c("R","VC","Indicator","Stat",
                     "Seg","Y","I","PoutputNational")) %>%
  left_join(MapI) %>%
  select(-"I") %>%
  mutate(prj = "Phi")

Calib_PoutputNational_prj_GCD <- AIDADSCalib_prj_GCD %>%
  rgdx.param("PoutputNational")   %>%
  rename.vars(from = colnames(.), 
              to = c("R","VC","Indicator","Stat",
                     "Seg","Y","I","PoutputNational")) %>%
  left_join(MapI) %>%
  select(-"I") %>%
  mutate(prj = "A")
  # unique(Calib_PoutputNational_prj_GCD)

Calib_PoutputNational_prjB <- AIDADSCalib_prjB %>%
  rgdx.param("PoutputNational")   %>%
  rename.vars(from = colnames(.), 
              to = c("R","VC","Indicator","Stat",
                     "Seg","Y","I","PoutputNational")) %>%
  left_join(MapI) %>%
  select(-"I") %>%
  mutate(prj = "B")

Calib_PoutputNational_all <- Calib_PoutputNational %>%
  rbind(Calib_PoutputNational_prj_GCD) %>%
  rbind(Calib_PoutputNational_prjB) %>%
  filter(VC == 'abs') %>%
  select(-'Stat') %>%
  spread(Indicator, PoutputNational) %>%
  na.omit()

## Plot Phi ----
pdata <- Calib_PoutputNational_all %>%
  filter(R == "CHN",
         prj == "Phi") %>%
  group_by(I_abb) %>%
  transform(I_abb = factor(I_abb, levels = lis_I_abb))

p781 <- ggplot(data = pdata, 
               mapping = aes(x = obs, y = EST)) +
  geom_point(size = .5) +
  facet_wrap(~I_abb,scales = "free", ncol = 4) + # factor(I, lis_I)
  geom_abline(intercept = 0, slope = 1, color = 'grey') +
  labs(x = "observations",
       y = "estimates") +
  ggpmisc::stat_poly_eq(formula = y ~ x,
               mapping = aes(label =..rr.label..),parse = TRUE) +
  MyTheme
p781
ggsave(paste0(dir_fig,"Fig 78. Uncertainty_NationalCalib_Phi.png"), 
       p781, 
       dpi = 300, width = 200, height = 200, units = "mm")



## Plot A ----
pdata <- Calib_PoutputNational_all %>%
  filter(R == "CHN",
         prj == "A") %>%
  group_by(I_abb) %>%
  transform(I_abb = factor(I_abb, levels = c("FNB", "ATN", "CFW", 
                                             "HWE", "FHE", "HLT",
                                             "TRN", "CMN", "REC",
                                             "EDC", "REH", "MGS")))

p782 <- ggplot(data = pdata, 
              mapping = aes(x = obs, y = EST)) +
  geom_point(size = .5) +
  facet_wrap(~I_abb,scales = "free", ncol = 4) + # factor(I, lis_I)
  geom_abline(intercept = 0, slope = 1, color = 'grey') +
  labs(x = "observations",
       y = "estimates") +
  ggpmisc::stat_poly_eq(formula = y ~ x,
               mapping = aes(label =..rr.label..),parse = TRUE) +
  MyTheme
p782
ggsave(paste0(dir_fig,"Fig 78. Uncertainty_NationalCalib_A.png"),
       p782, 
       dpi = 300, width = 200, height = 160, units = "mm")



## Plot B ----
pdata <- Calib_PoutputNational_all %>%
  filter(R == "CHN",
         prj == "B") %>%
  group_by(I_abb) %>%
  transform(I_abb = factor(I_abb, levels = lis_I_abb))

p783 <- ggplot(data = pdata, mapping = aes(x = obs, y = EST)) +
  geom_point(size = .5) +
  facet_wrap(~I_abb,scales = "free", ncol = 4) + # factor(I, lis_I)
  geom_abline(intercept = 0, slope = 1, color = 'grey') +
  labs(x = "observations",
       y = "estimates") +
  ggpmisc::stat_poly_eq(formula = y ~ x,
               mapping = aes(label =..rr.label..),parse = TRUE) +
  MyTheme
p783
ggsave(paste0(dir_fig,"Fig 78. Uncertainty_NationalCalib_B.png"), 
       p783, 
       dpi = 300, width = 200, height = 200, units = "mm")