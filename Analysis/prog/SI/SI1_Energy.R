# This program is a summary of all the figures and plots in the paper SI
# "Poverty and inequality in China under the long-term global temperature target"
# This program deals with Primary energy, power sector, and final energy

# Shiya ZHAO, 2021/06/17

## Loading packages ----
library(gdxrrw)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(gdata)
library(grid)
library(Rmisc)


## Directory assignment and data loading ----
# CGE directory and data 
dir_cge <- "..//..//..//data\\cgeoutput\\gdx"
lis_gdxcge <- list.files(dir_cge,pattern = ".gdx", full.names = TRUE)
# view(lis_gdxcge)

cge_chn_ana <- 	"..//..//..//data/cgeoutput/gdx/CHN_analysis.gdx"
cge_chn_var <- "..//..//..//data/cgeoutput/gdx/CHN_emf.gdx"
cge_global_ana <- "..//..//..//data/cgeoutput/gdx/global_analysis.gdx"
cge_global_var <- "..//..//..//data/cgeoutput/gdx/global_17_emf.gdx"



# Output directory 
dir_fig <- "..//..//..//fig/SI1"


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

# Energy types

lis_PrmEne_type <- c('Coal (without CCS)',
                     'Coal (with CCS)',
                     'Oil (without CCS)',
                     'Oil (with CCS)',
                     'Gas (without CCS)',
                     'Gas (with CCS)',
                     'Hydropower',
                     'Nuclear',
                     'Solar',
                     'Wind',
                     'Geothermal',
                     'Bioenergy (without CCS)',
                     'Bioenergy (with CCS)',
                     'Others')

lis_PowEne_type <- c('Coal (without CCS)',
                     'Coal (with CCS)',
                     'Oil (without CCS)',
                     'Oil (with CCS)',
                     'Gas (without CCS)',
                     'Gas (with CCS)',
                     'Hydropower',
                     'Nuclear',
                     'Solar',
                     'Wind',
                     'Geothermal',
                     'Bioenergy (without CCS)',
                     'Bioenergy (with CCS)',
                     'Others')


lis_FinEne_type <- c('Electricity',
                     'Gas',
                     'Geothermal',
                     'Heat',
                     'Hydropower',
                     'Liquid',
                     'Solids')

lis_FinEne_type <- c('Electricity',#1
                     'Gas',#2
                     'Geothermal',#3
                     'Heat',#4
                     'Hydropower',#5
                     'Liquid',#6
                     'Solids',#7
                     'Coal',#8
                     'Biomass',#9
                     'Bio_liquid')#10


# Primary energy variables
lis_VPrm_Ene <- c(  "Prm_Ene_Coa_wo_CCS",
                    "Prm_Ene_Coa_w_CCS",
                    "Prm_Ene_Oil_wo_CCS",
                    "Prm_Ene_Oil_w_CCS",
                    "Prm_Ene_Gas_wo_CCS",
                    "Prm_Ene_Gas_w_CCS",
                    "Prm_Ene_Hyd",
                    "Prm_Ene_Nuc",
                    "Prm_Ene_Solar",
                    "Prm_Ene_Win",
                    "Prm_Ene_Geo",
                    "Prm_Ene_Bio_wo_CCS",
                    "Prm_Ene_Bio_w_CCS",
                    "Prm_Ene_Oth")

# Secondary energy variables
lis_VSec_Ene <- c('Sec_Ene_Ele',
                  'Sec_Ene_Gas',
                  'Sec_Ene_Heat',
                  'Sec_Ene_Solids',
                  'Sec_Ene_Liq')

lis_VSec_Power <- c('Sec_Ene_Ele_Nuc',
                    'Sec_Ene_Ele_Oth',
                    'Sec_Ene_Ele_Bio_w_CCS',
                    'Sec_Ene_Ele_Bio_wo_CCS',
                    'Sec_Ene_Ele_Coa_w_CCS',
                    'Sec_Ene_Ele_Coa_wo_CCS',
                    'Sec_Ene_Ele_Gas_w_CCS',
                    'Sec_Ene_Ele_Gas_wo_CCS',
                    'Sec_Ene_Ele_Geo',
                    'Sec_Ene_Ele_Hyd',
                    'Sec_Ene_Ele_Oil_w_CCS',
                    'Sec_Ene_Ele_Oil_wo_CCS',
                    'Sec_Ene_Ele_Solar',
                    'Sec_Ene_Ele_Win')

# Final energy variables
lis_VFin_Ene <- c('Fin_Ene_Ele',
                  'Fin_Ene_Gas',
                  'Fin_Ene_Geo',
                  'Fin_Ene_Heat',
                  'Fin_Ene_Hyd',
                  'Fin_Ene_Liq',
                  'Fin_Ene_Solids')



## Recode ----

# data <-   mutate( data, scenario = recode(data$SCENARIO,
#                                                         'SSP2_NS1_Baseline_NoCC'=  lis_scenario[1],
#                                                         'SSP2_NS1_CM1_NoCC'=  lis_scenario[2],
#                                                         'SSP2_NS1_CM2_NoCC'=  lis_scenario[3],
#                                                         'SSP2_NS1_CM3_NoCC'=  lis_scenario[4],
#                                                         'SSP2_NS1_CM4_NoCC'=  lis_scenario[5],
#                                                         'SSP2_NS1_CM1_exempt_NoCC'=  lis_scenario[6],
#                                                         'SSP2_NS1_CM2_exempt_NoCC'=  lis_scenario[7],
#                                                         'SSP2_NS1_CM3_exempt_NoCC'=  lis_scenario[8],
#                                                         'SSP2_NS1_CM4_exempt_NoCC'=  lis_scenario[9]))



## Color palette ----

palette_Sc <- c('Baseline' ='#990000', # '#8B0000',
                '50%' = '#CCCC33', # '#A2CD5A',
                '65%' = '#CC99FF', #'#187CD',
                '80%' = '#CCFF99', #'#CD2626',
                'NDC' =  '#009966') #'#9932CC')

palette_Prm <- c('Coal (without CCS)' = '#990000', #'#9999CC',
                 'Coal (with CCS)' = '#CC6666', #CCCCFF',
                 'Oil (without CCS)' = '#CC9966',
                 'Oil (with CCS)' = '#CCCC33',
                 'Gas (without CCS)' = '#FF6600',
                 'Gas (with CCS)' = '#FF9966',
                 'Hydropower' = '#66CCFF',
                 'Nuclear' = '#CC99FF',
                 'Solar' = '#FFCC33',
                 'Wind' = '#CCFF99',
                 'Geothermal' = '#CC3399',
                 'Bioenergy (without CCS)' = '#009966',
                 'Bioenergy (with CCS)' = '#00CC99',
                 'Others' = '#CCCC99'
)

palette_Fin <- c('Electricity' = '#99CCFF', # ele
                 'Gas' = '#FF6600', # gas
                 'Geothermal' = '#CC3399', # geo
                 'Heat' = '#FF3366', # heat
                 'Hydropower' = '#66CCFF', # hyd
                 'Liquid' = '#CC9966', # liq
                 'Solids' = '#990000', # solid
                 'Coal' = '#CC6666',
                 'Biomass' = '#009966',
                 'Bio_liquid' = '#CCCC99'
)



## Fig SI4 Energy information ----
## Energy DATA loading ----
EMF <- filter(rgdx.param(cge_chn_var, "EMFtemp"),
              REMF == "CHN",
              YEMF %in% c('2005', '2010', '2015', '2020', '2025', '2030',
                          '2035', '2040', '2045' , '2050')) #%>%
EMF <- mutate(EMF,scenario = recode(EMF$EMFsce,
                                    'SSP2_NS1_Baseline_NoCC'=  lis_scenario[1],
                                    'SSP2_NS1_CM1_NoCC'=  lis_scenario[2],
                                    'SSP2_NS1_CM2_NoCC'=  lis_scenario[3],
                                    'SSP2_NS1_CM3_NoCC'=  lis_scenario[4],
                                    'SSP2_NS1_CM4_NoCC'=  lis_scenario[5],
                                    'SSP2_NS1_CM1_exempt_NoCC'=  lis_scenario[6],
                                    'SSP2_NS1_CM2_exempt_NoCC'=  lis_scenario[7],
                                    'SSP2_NS1_CM3_exempt_NoCC'=  lis_scenario[8],
                                    'SSP2_NS1_CM4_exempt_NoCC'=  lis_scenario[9]))


# Final energy in residential sector ----
Fin_Ene_res <- rgdx.param(cge_chn_var, "EMFtemp1")%>%
  filter(REMF == "CHN",
         Y %in% c('2010','2015','2020','2025','2030','2035','2040','2045','2050'),
         VEMF != 'Fin_Ene_Res_and_Com_and_AFO')

Fin_Ene_res <- Fin_Ene_res[(startsWith(as.character(Fin_Ene_res[,which(colnames(Fin_Ene_res)=='VEMF')]),
                                       'Fin_Ene_Res_and_Com')),]%>%
  filter(VEMF != 'Fin_Ene_Res_and_Com',
      #   VEMF !='Fin_Ene_Res_and_Com_Gas',
         VEMF %in% c("Fin_Ene_Res_and_Com_Ele",
                     "Fin_Ene_Res_and_Com_Gas",
                     "Fin_Ene_Res_and_Com_Heat", 
                     "Fin_Ene_Res_and_Com_Liq",
                     "Fin_Ene_Res_and_Com_SolidsCoa",
                     "Fin_Ene_Res_and_Com_SolidsBio"))

Fin_Ene_res <-   mutate(Fin_Ene_res, scenario = recode(Fin_Ene_res$SCENARIO,
                                                       'SSP2_NS1_Baseline_NoCC'=  lis_scenario[1],
                                                       'SSP2_NS1_CM1_NoCC'=  lis_scenario[2],
                                                       'SSP2_NS1_CM2_NoCC'=  lis_scenario[3],
                                                       'SSP2_NS1_CM3_NoCC'=  lis_scenario[4],
                                                       'SSP2_NS1_CM4_NoCC'=  lis_scenario[5],
                                                       'SSP2_NS1_CM1_exempt_NoCC'=  lis_scenario[6],
                                                       'SSP2_NS1_CM2_exempt_NoCC'=  lis_scenario[7],
                                                       'SSP2_NS1_CM3_exempt_NoCC'=  lis_scenario[8],
                                                       'SSP2_NS1_CM4_exempt_NoCC'=  lis_scenario[9]))
#levels(factor(Fin_Ene_res$VEMF))
Fin_Ene_res <-   mutate( Fin_Ene_res,
                         FinEne_type = recode(Fin_Ene_res$VEMF,
                                              "Fin_Ene_Res_and_Com_Ele" = 'Electricity',
                                              "Fin_Ene_Res_and_Com_Gas" = "Gas",
                                              "Fin_Ene_Res_and_Com_Heat" = "Heat", 
                                              "Fin_Ene_Res_and_Com_Liq" = "Liquid",
                                              "Fin_Ene_Res_and_Com_SolidsCoa" = "Coal",
                                              "Fin_Ene_Res_and_Com_SolidsBio" = "Biomass"))


## Fig SI4a Primary energy demand periodical ----
Prm_Ene <- filter(EMF,
                  VEMF == "Prm_Ene_Coa_wo_CCS" |
                    VEMF == 'Prm_Ene_Coa_w_CCS' |
                    VEMF == 'Prm_Ene_Oil_wo_CCS' |
                    VEMF == 'Prm_Ene_Oil_w_CCS' |
                    VEMF == 'Prm_Ene_Gas_wo_CCS' |
                    VEMF == 'Prm_Ene_Gas_w_CCS' |
                    VEMF == 'Prm_Ene_Hyd' |
                    VEMF == 'Prm_Ene_Nuc' |
                    VEMF == 'Prm_Ene_Solar' |
                    VEMF == 'Prm_Ene_Win' |
                    VEMF == 'Prm_Ene_Geo' |
                    VEMF == 'Prm_Ene_Bio_wo_CCS' |
                    VEMF == 'Prm_Ene_Bio_w_CCS' |
                    VEMF == 'Prm_Ene_Oth') %>%
  mutate(PrmEne_type = '0')


Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Coa_wo_CCS'),]$PrmEne_type <-  "Coal (without CCS)" 
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Coa_w_CCS'),]$PrmEne_type <-  "Coal (with CCS)"
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Oil_wo_CCS'),]$PrmEne_type <-  "Oil (without CCS)" 
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Oil_w_CCS'),]$PrmEne_type <- "Oil (with CCS)" 
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Gas_wo_CCS'),]$PrmEne_type <- "Gas (without CCS)" 
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Gas_w_CCS'),]$PrmEne_type <- "Gas (with CCS)" 
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Hyd'),]$PrmEne_type <-  "Hydropower"
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Nuc'),]$PrmEne_type <- "Nuclear"
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Solar'),]$PrmEne_type <- "Solar"
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Win'),]$PrmEne_type <- "Wind" 
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Geo'),]$PrmEne_type <-  "Geothermal"
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Bio_wo_CCS'),]$PrmEne_type <- "Bioenergy (without CCS)"
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Bio_w_CCS'),]$PrmEne_type <- "Bioenergy (with CCS)" 
Prm_Ene[which(Prm_Ene$VEMF == 'Prm_Ene_Oth'),]$PrmEne_type <- "Others"
Prm_Ene_total <- filter(EMF,
                        VEMF == 'Prm_Ene',
                        YEMF %in% c('2030','2040','2050')) %>%
  mutate(PrmEne_type = 'Total')


## Fig SI4b Energy composition in power sector ----
Sec_Power <- filter(EMF,
                    VEMF == 'Sec_Ene_Ele_Coa_wo_CCS' |
                      VEMF == 'Sec_Ene_Ele_Coa_w_CCS' |
                      VEMF == 'Sec_Ene_Ele_Oil_wo_CCS' |
                      VEMF == 'Sec_Ene_Ele_Oil_w_CCS' |
                      VEMF == 'Sec_Ene_Ele_Gas_wo_CCS' |
                      VEMF == 'Sec_Ene_Ele_Gas_w_CCS' |
                      VEMF == 'Sec_Ene_Ele_Hyd' |                      
                      VEMF == 'Sec_Ene_Ele_Nuc' |
                      VEMF == 'Sec_Ene_Ele_Solar' |
                      VEMF == 'Sec_Ene_Ele_Win' |
                      VEMF == 'Sec_Ene_Ele_Geo' |
                      VEMF == 'Sec_Ene_Ele_Bio_wo_CCS' |
                      VEMF == 'Sec_Ene_Ele_Bio_w_CCS' |
                      VEMF == 'Sec_Ene_Ele_Oth') %>%
  mutate(PowEne_type = '0')
Sec_Power_Total <- filter(EMF,
                          VEMF == 'Sec_Ene_Ele',
                          YEMF %in% c('2030', '2040', '2050')) %>%
  mutate(PowEne_type = 'Total')

Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Coa_wo_CCS'),]$PowEne_type <-   "Coal (without CCS)" 
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Coa_w_CCS'),]$PowEne_type <-  "Coal (with CCS)"
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Oil_wo_CCS'),]$PowEne_type <- "Oil (without CCS)"
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Oil_w_CCS'),]$PowEne_type <- "Oil (with CCS)" 
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Gas_wo_CCS'),]$PowEne_type <- "Gas (without CCS)"
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Gas_w_CCS'),]$PowEne_type <- "Gas (with CCS)"
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Hyd'),]$PowEne_type <- "Hydropower"
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Nuc'),]$PowEne_type <- "Nuclear"
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Solar'),]$PowEne_type <- "Solar"
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Win'),]$PowEne_type <-  "Wind" 
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Geo'),]$PowEne_type <-  "Geothermal"
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Bio_wo_CCS'),]$PowEne_type <- "Bioenergy (without CCS)"
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Bio_w_CCS'),]$PowEne_type <- "Bioenergy (with CCS)" 
Sec_Power[which(Sec_Power$VEMF == 'Sec_Ene_Ele_Oth'),]$PowEne_type <- "Others"



## Fig SI4a0 temporal change, both primary and power, baseline and NDC ----
apo <- filter(Sec_Power,
              YEMF %in% c('2010', '2020', '2030','2040', '2050'),
              scenario %in% c('Baseline','50%','65%','80%', 'NDC'))%>%
  mutate(ENE = 'Power')%>%
  rbind(data.frame(.i1 = c('AIM/CGE','AIM/CGE','AIM/CGE','AIM/CGE','AIM/CGE','AIM/CGE','AIM/CGE','AIM/CGE','AIM/CGE'),
                   EMFsce = c('SSP2_NS1_CM1_NoCC','SSP2_NS1_CM1_NoCC','SSP2_NS1_CM2_NoCC','SSP2_NS1_CM3_NoCC','SSP2_NS1_CM3_NoCC','SSP2_NS1_CM4_NoCC','SSP2_NS1_CM4_NoCC','SSP2_NS1_CM4_NoCC','SSP2_NS1_CM4_NoCC'),
                   REMF = c('CHN','CHN','CHN','CHN','CHN','CHN','CHN','CHN','CHN'),
                   VEMF = c("Sec_Ene_Ele_Coa_w_CCS","Sec_Ene_Ele_Gas_w_CCS","Sec_Ene_Ele_Coa_w_CCS","Sec_Ene_Ele_Coa_w_CCS","Sec_Ene_Ele_Coa_wo_CCS","Sec_Ene_Ele_Coa_w_CCS","Sec_Ene_Ele_Oil_w_CCS","Sec_Ene_Ele_Gas_w_CCS","Sec_Ene_Ele_Bio_w_CCS"),
                   UEMF = c('EJ/yr','EJ/yr','EJ/yr','EJ/yr','EJ/yr','EJ/yr','EJ/yr','EJ/yr','EJ/yr'),
                   YEMF = c('2020','2020','2020','2020','2050','2030','2030','2030','2030'),
                   EMFtemp = c(0,0,0,0,0,0,0,0,0),
                   scenario = c('50%','50%','65%','80%','80%','NDC','NDC','NDC','NDC'),
                   PowEne_type = c('Coal (with CCS)','Gas (with CCS)','Coal (with CCS)','Coal (with CCS)','Coal (without CCS)','Coal (with CCS)','Oil (with CCS)','Gas (with CCS)','Bioenergy (with CCS)'),
                   ENE = c('Power','Power','Power','Power','Power','Power','Power','Power','Power')))
names(apo)[9] <- 'Ene_type'

apr <- filter(Prm_Ene, 
              YEMF %in% c('2010', '2020', '2030', '2040', '2050'),
              scenario == 'Baseline' |
                scenario == '50%' |
                scenario == '65%' |
                scenario == '80%' |
                scenario == 'NDC'
)%>%
  mutate(ENE = 'Primary') %>%
  rbind(data.frame(.i1 = c('AIM/CGE','AIM/CGE','AIM/CGE','AIM/CGE','AIM/CGE','AIM/CGE','AIM/CGE','AIM/CGE','AIM/CGE'),
                   EMFsce = c('SSP2_NS1_CM1_NoCC','SSP2_NS1_CM2_NoCC','SSP2_NS1_CM2_NoCC','SSP2_NS1_CM3_NoCC','SSP2_NS1_CM3_NoCC','SSP2_NS1_CM4_NoCC','SSP2_NS1_CM4_NoCC','SSP2_NS1_CM4_NoCC','SSP2_NS1_CM4_NoCC'),
                   REMF = c('CHN','CHN','CHN','CHN','CHN','CHN','CHN','CHN','CHN'),
                   VEMF = c("Prm_Ene_Coa_w_CCS","Prm_Ene_Coa_w_CCS","Prm_Ene_Coa_w_CCS","Prm_Ene_Gas_w_CCS","Prm_Ene_Gas_w_CCS","Prm_Ene_Coa_w_CCS","Prm_Ene_Oil_w_CCS","Prm_Ene_Gas_w_CCS","Prm_Ene_Bio_w_CCS"),
                   UEMF = c('EJ/yr','EJ/yr','EJ/yr','EJ/yr','EJ/yr','EJ/yr','EJ/yr','EJ/yr','EJ/yr'),
                   YEMF = c('2020','2020','2020','2020','2020','2030','2030','2030','2030'),
                   EMFtemp = c(0,0,0,0,0,0,0,0,0),
                   scenario = c('50%','65%','65%','80%','80%','NDC','NDC','NDC','NDC'),
                   PrmEne_type = c('Coal (with CCS)','Coal (with CCS)','Gas (with CCS)','Gas (with CCS)','Coal (with CCS)','Coal (with CCS)','Oil (with CCS)','Gas (with CCS)','Bioenergy (with CCS)'),
                   ENE = c('Primary','Primary','Primary','Primary','Primary','Primary','Primary','Primary','Primary')))
names(apr)[9] <- 'Ene_type'


afin_r <- filter(rename.vars(Fin_Ene_res,from = c('Y','FinEne_type','EMFtemp1','SCENARIO'), to = c('YEMF','Ene_type','EMFtemp','EMFsce')), 
                 YEMF == '2010' |
                   YEMF == '2020' |
                   YEMF == '2030' |
                   YEMF == '2040' |
                   YEMF == '2050',
                 scenario %in% c('Baseline','50%','65%','80%','NDC')
)%>%
  mutate(ENE = 'Final energy (residential)',.i1 = 'AIM/CGE',UEMF = 'EJ/year')


Energy <- rbind(apr,apo,afin_r)



## Plot SI4a0 Legend ----
pdata <- filter(Energy, ENE %in% c('Primary'))

p_SI4a0 <-  ggplot(data =  pdata,
                   mapping = aes(x =  as.numeric(as.character(YEMF)), 
                                 y = EMFtemp,
                                 fill = fct_rev(factor(Ene_type,levels = lis_PrmEne_type))))  + # Ene_type
  geom_area(size = 0.5, position = 'stack')  + 
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = 'a) Primary energy demand',
       x = "Year",
       y = "Energy | EJ/yr") +
  facet_grid(~scenario,scales = "free") +
  theme(text = element_text(size=16),
        axis.text.x = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        axis.text.y = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        panel.grid =element_blank(),
        axis.line = element_line(size=0.5, colour = "black"),
        panel.background = element_blank(),
        panel.spacing.x=unit(1.5, "lines")
        # legend.position = 'bottom'
  )  +
  guides(fill = guide_legend(title = 'Energy type', ncol = 1, byrow = TRUE)) +
  scale_fill_manual(values = palette_Prm)

p_SI4a0
require(cowplot)
leg12 <- get_legend(p_SI4a0)

## Plot SI4a1 Primary ----
pdata <- filter(Energy, ENE %in% c('Primary'))
# pdata$ENE <-   recode(pdata$ENE,'Primary'='Primary')%>%
#   factor(levels = c('Primary','Power sector'))
p_SI4a1 <-  ggplot(data =  pdata,
                  mapping = aes(x =  as.numeric(as.character(YEMF)), 
                                y = EMFtemp,
                                fill = fct_rev(factor(Ene_type,levels = lis_PrmEne_type))))  + # Ene_type
  geom_area(size = 0.5, position = 'stack')  + 
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = 'a) Primary energy supply',
       x = "Year",
       y = "Primary energy | EJ/yr") +
  facet_grid(~scenario,scales = "free") +
  theme(text = element_text(size=16),
        axis.text.x = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        axis.text.y = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        panel.grid =element_blank(),
        axis.line = element_line(size=0.5, colour = "black"),
        panel.background = element_blank(),
        panel.spacing.x=unit(1.5, "lines"),
       legend.position = "None"
  )  +
  guides(fill = guide_legend(title = 'Energy type', ncol = 1, byrow = TRUE)) +
  scale_fill_manual(values = palette_Prm)

p_SI4a1




## Plot SI4a2 Electricity generation ----
pdata <- filter(Energy, ENE %in% c('Power'))
pdata$ENE <-   recode(pdata$ENE,
                      'Power'= 'Electricity generation')

p_SI4a2 <-  ggplot(data =  pdata,
                   mapping = aes(x = as.numeric(as.character(YEMF)),
                                 y = EMFtemp,
                                 fill = fct_rev(factor(Ene_type,levels = lis_PowEne_type))))  +
  geom_area(size = 0.5, position = 'stack') +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = 'b) Electricity generation',
       x = "Year",
       y = "Electricity | EJ/yr") +
  facet_grid(~scenario,scales = "free") +
  theme(text = element_text(size=16),
        axis.text.x = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        axis.text.y = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        panel.grid =element_blank(),
        axis.line = element_line(size=0.5, colour = "black"),
        panel.background = element_blank(),
        panel.spacing.x=unit(1.5, "lines"),
        legend.position = "None"
        # legend.position = 'bottom'
  )  +
  guides(fill = guide_legend(title = 'Energy type', ncol = 1, byrow = TRUE)) +
  scale_fill_manual(values = palette_Prm)

p_SI4a2


## Fig SI4b Final energy ----
lis_FinEne_type <- c('Coal',
                     'Biomass',
                     'Liquid',
                     'Gas',
                     'Heat',
                     'Electricity')

## Plot ----
palette_Fin <- c('Electricity' = '#99CCFF', # ele
                 'Gas' = '#FF6600', # gas
                 'Geothermal' = '#CC3399', # geo
                 'Heat' = '#FF3366', # heat
                 'Hydropower' = '#66CCFF', # hyd
                 'Liquid' = '#CC9966', # liq
                 'Solids' = '#990000', # solid
                 'Coal' = '#CC6666',
                 'Biomass' = '#009966',
                 'Bio_liquid' = '#CCCC99'
)
lis_FinEne_type

pdata <- filter(Energy, ENE %in% c('Final energy (residential)'))
p_SI4b <-  ggplot(data =  pdata,
                  mapping = aes(x = as.numeric(as.character(YEMF)),
                                y = EMFtemp,
                                fill = (factor(Ene_type,levels = lis_FinEne_type))))  +
  geom_area(size = 0.5, position = 'stack') +
  geom_abline(intercept = 0, slope = 0, colour = 'grey') +
  labs(title = 'c) Final energy demand (residential and commercial)',
       x = "Year",
       y = "Final energy | EJ/yr") +
  facet_grid(~scenario,scales = "free") +
  theme(text = element_text(size=16),
        axis.text.x = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        axis.text.y = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        panel.grid =element_blank(),
        axis.line = element_line(size=0.5, colour = "black"),
        panel.background = element_blank(),
        panel.spacing.x=unit(1.5, "lines")
        # legend.position = 'bottom'
  )  +
  guides(fill = guide_legend(title = 'Energy type', ncol = 1, byrow = TRUE)) +
  scale_fill_manual(values = palette_Fin)

p_SI4b





## Paste ----
p_SI4a1
p_SI4a2
leg <- plot_grid(leg12, ncol=1, rel_widths=c(1))
p_SI4b

grid.newpage()  ###新建图表版面
pushViewport(viewport(layout = grid.layout(3,20))) ####???版面??????2*2矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
print(p_SI4a1, vp = vplayout(1,1:17))   ###??????1,1)???(1,2)???位置画图chart3
print(p_SI4a2, vp = vplayout(2,1:17))   ###??????1,1)???(1,2)???位置画图chart3
print(leg, vp = vplayout(1:2,18:20))   ###??????1,1)???(1,2)???位置画图chart3
print(p_SI4b, vp = vplayout(3,1:19))     ###???(2,1)???位置画图chart2

dev.off()#
#plot_grid(p_SI4a1, p_SI4a2, leg12, ncol=3, rel_widths=c(1, 1, 1))
# p_1 <- multiplot(p_SI4a0,p_SI4b, layout = matrix(c(1,1,2), nrow=3,  byrow=TRUE))

# "Fig SI3 Energy.png",1600:1400 
dev.off()#
ggsave(p_1,filename = "Fig 1 emission_tax_gdp_price.png", path = dir_fig, width = 12,height = 9)

png(file="myplot.png", bg="transparent")


