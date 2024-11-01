# This program is a summary of all the figures and plots in the paper 
# "Poverty and inequality in China under the long-term global temperature target"
# This program deals with Poverty headcounts and relative poverty

# Shiya ZHAO, 2021/06/17

## Loading packages ----
library(gdxrrw)
library(ggplot2)
library(tidyverse)
library(plyr)
library(cowplot)
library(gdata)
library(grid)
library(base)
library(Rmisc)



## Directory assignment and data loading ----

# PHI directory and data
dir_phi <- "..//..//..//data/phioutput/gdx"
lis_gdxphi <- list.files(dir_phi,pattern = ".gdx", full.names = TRUE)
#view(lis_gdxphi)

AIDADSCalib <- 	"..//..//..//data/phioutput/gdx/AIDADSCalibNationResults.gdx"
AnaExp <- "..//..//..//data/phioutput/gdx/AnalysisExpenditure.gdx"
AnaInc <- "..//..//..//data/phioutput/gdx/AnalysisIncome.gdx"
demand <- "..//..//..//data/phioutput/gdx/CHN_demand.gdx"
pricechange <- "..//..//..//data/phioutput/gdx/pricechange.gdx"
#view(c(AIDADSCalib,AnaExp,AnaInc,demand,pricechange))

# World development database 
dir_wdi <- "..//..//..//data/WDI"
lis_xlsxwdi <-  list.files(dir_wdi,pattern = ".xlsx", full.names = TRUE)
# view(lis_xlsxwdi)

# Output directory 
dir_fig <- "..//..//..//fig/1"



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

lis_ref <- c('SSP2_Baseline',
             'SSP2_NS1_Baseline',
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


lis_TH <- c('1.9-threshold',
            '3.2-threshold',
            '5.5-threshold')


## Recode ----
# 
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
palette_TH <- c('1.9-threshold' = '#990000',
                '3.2-threshold' = '#CCCC33',
                '5.5-threshold' = '#CC99FF',
                'Relative poverty' ='#009966' )

palette_Sc <- c('Baseline' ='#990000', # '#8B0000',
                '50%' = '#CCCC33', # '#A2CD5A',
                '65%' = '#CC99FF', #'#187CD',
                '80%' = '#CCFF99', #'#CD2626',
                'NDC' =  '#009966') #'#9932CC')



## Fig SI7a Poverty headcount  ----
# Poverty headcount ---
PoVExp_tmp <- rgdx.param(AnaExp, "PoVExp") %>%
  filter(R == "CHN",
         Ref %in% lis_ref ,
         Y%in% c('2020', '2025' , '2030', '2035', '2040', '2045', '2050'))%>%
  mutate(Effect = 'Expenditure', TH1 = '0')


PoVInc_tmp <- rgdx.param(AnaInc, "PoV") %>%
  filter(R == "CHN",
         Ref %in% lis_ref,
         Y %in% c('2020', '2025' , '2030', '2035', '2040', '2045', '2050')) %>%
  mutate(Effect = 'Income',TH1 = '0')


c <-PoVExp_tmp
PoVExp <-   mutate(PoVExp_tmp, scenario = recode(Ref,
                                                 'SSP2_NS1_Baseline'=  lis_scenario[1],
                                                 'SSP2_NS1_CM1'=  lis_scenario[2],
                                                 'SSP2_NS1_CM2'=  lis_scenario[3],
                                                 'SSP2_NS1_CM3'=  lis_scenario[4],
                                                 'SSP2_NS1_CM4'=  lis_scenario[5],
                                                 'SSP2_NS1_CM1_exempt'=  lis_scenario[6],
                                                 'SSP2_NS1_CM2_exempt'=  lis_scenario[7],
                                                 'SSP2_NS1_CM3_exempt'=  lis_scenario[8],
                                                 'SSP2_NS1_CM4_exempt'=  lis_scenario[9]))


PoV_tmp <- rbind(PoVInc_tmp, rename.vars(PoVExp_tmp,from = 'PoVExp', to = 'PoV')) %>%
  mutate( scenario = recode(Ref,
                            'SSP2_NS1_Baseline'=  lis_scenario[1],
                            'SSP2_NS1_CM1'=  lis_scenario[2],
                            'SSP2_NS1_CM2'=  lis_scenario[3],
                            'SSP2_NS1_CM3'=  lis_scenario[4],
                            'SSP2_NS1_CM4'=  lis_scenario[5],
                            'SSP2_NS1_CM1_exempt'=  lis_scenario[6],
                            'SSP2_NS1_CM2_exempt'=  lis_scenario[7],
                            'SSP2_NS1_CM3_exempt'=  lis_scenario[8],
                            'SSP2_NS1_CM4_exempt'=  lis_scenario[9]))

PoV_tmp[which(PoV_tmp$TH == 'pop_1.9'),]$TH1 <- lis_TH[1]
PoV_tmp[which(PoV_tmp$TH == 'pop_3.2'),]$TH1 <- lis_TH[2]
PoV_tmp[which(PoV_tmp$TH == 'pop_5.5'),]$TH1 <- lis_TH[3]
PoV <- PoV_tmp[-which(colnames(PoV_tmp)=='TH')]

PoV1 <-filter(PoV,scenario %in% c(lis_scenario_noex,lis_scenario_exmp,'Baseline'))%>%
  rename.vars(from = 'PoV', to = 'Value')

# Relative poverty ---
RPoVExp <- rgdx.param(AnaExp, "RPoVExp") %>%
  filter(R == "CHN", 
         Y %in% c('2020','2025','2030','2035','2040','2045','2050') ) 

RPoVInc <- rgdx.param(AnaInc, "RPoV") %>%
  filter(R == "CHN", 
         Y %in% c('2020','2025','2030','2035','2040','2045','2050') )

RPoV_tmp <- rbind(mutate(RPoVInc, Effect = 'Income'), 
                  mutate(rename.vars(RPoVExp,from = 'RPoVExp', to = 'RPoV'),
                         Effect = 'Expenditure'))%>%
  filter(Ref %in% lis_ref)

RPoV <-   mutate(RPoV_tmp, scenario = recode(Ref,
                                             'SSP2_NS1_Baseline'=  lis_scenario[1],
                                             'SSP2_NS1_CM1'=  lis_scenario[2],
                                             'SSP2_NS1_CM2'=  lis_scenario[3],
                                             'SSP2_NS1_CM3'=  lis_scenario[4],
                                             'SSP2_NS1_CM4'=  lis_scenario[5],
                                             'SSP2_NS1_CM1_exempt'=  lis_scenario[6],
                                             'SSP2_NS1_CM2_exempt'=  lis_scenario[7],
                                             'SSP2_NS1_CM3_exempt'=  lis_scenario[8],
                                             'SSP2_NS1_CM4_exempt'=  lis_scenario[9]))


RPoV1 <- filter(RPoV,scenario %in% c(lis_scenario_noex,lis_scenario_exmp,'Baseline'))%>%
  rename.vars(from = 'RPoV', to = 'Value')%>%
  mutate(TH1 = 'Relative poverty')


Headcount <- rbind(PoV1,RPoV1)

data <- mutate(Headcount,scenario0 = '0')
for(i in 1:length(c(lis_scenario_noex))){
  data[which(startsWith(as.character(data$scenario),lis_scenario_noex[i])),]$scenario0 <- lis_scenario_noex[i] 
}
data1 <- cbind(data,str_split(data$scenario,"_",simplify = TRUE)) %>%
  mutate(scenario2 = '0')%>%
  rename.vars(from = c('1','2','scenario2'), to = c('scenario1','scenario11','scenario2'))

# colnames(data1)[which(colnames(data1) %in% c('1','2','scenario2')))] <- c('scenario1','scenario11','scenario2')
data1[which(data1$scenario11 != 'exempt'),]$scenario2 <- 'non-exemption'
data1[which(data1$scenario11 == 'exempt'),]$scenario2 <- 'exemption'
data1$scenario2 <- factor(data1$scenario2, levels = c('non-exemption','exemption'))
data1 <- data1[,-which(colnames(data1) %in% c('Ref','q_value','scenario','scenario0','scenario11'))]

data2 <- spread(data1, scenario2, Value)

write.csv(x = filter(data2,
                     Y %in% c('2030','2040','2050')), file = "Povertyheadcount.csv",
          row.names = FALSE)

## Plot ----
pdata <- filter(Headcount,
                scenario %in% c(lis_scenario_noex,"Baseline"),
                TH1 %in% c('1.9-threshold','3.2-threshold','5.5-threshold','Relative poverty'),
                Y %in% c('2020','2030','2040','2050'),
                Effect == 'Expenditure')

p_SI7a <- ggplot(data = pdata ,
               mapping = aes(x = Y, y = Value/1000000, group = scenario, colour = scenario)) +
  geom_line() +
  # xlab("Year") +
  # ylab("Poverty headcount | Million people") +
  labs(title = "a) Poverty headcount projections", 
       x = 'Year',
       y ='Headcount | Million people')+
  facet_wrap(~TH1,scales = "free") +
  theme(text = element_text(size=16,  family="serif"),
        axis.text.x = element_text(size = 14,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        axis.text.y = element_text(size = 14,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        panel.grid =element_blank(),
        axis.line = element_line(size=0.5, colour = "black"),
        panel.background = element_blank(),
        #legend.position = 'bottom'
  )  +
  guides(colour = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE)) +
  scale_x_discrete(breaks=seq(2020, 2050, 5)) +
  scale_color_discrete(breaks = lis_scenario)+
  scale_fill_manual(values = palette_Sc)
p_SI7a



## Fig SI7b effect ----
Headcount_Inc_tmp <- filter(Headcount[,-which(colnames(Headcount) %in% c('Ref'))], 
                            Effect == 'Income')%>%
  na.omit()%>%
  spread(scenario, Value)

Headcount_Inc <- Headcount_Inc_tmp
Headcount_Inc[,which(colnames(Headcount_Inc) %in% lis_scenario)] <- Headcount_Inc_tmp[,which(colnames(Headcount_Inc_tmp) %in% lis_scenario)]-Headcount_Inc_tmp[,which(colnames(Headcount_Inc_tmp) == 'Baseline')]
Headcount_Inc <- gather(Headcount_Inc, scenario, Value, lis_scenario_mitigation)

Headcount_Exp_tmp <- spread(Headcount[,-which(colnames(Headcount) %in% c('Ref'))], 
                            Effect, Value)%>%
  na.omit()
Headcount_Exp_tmp <- mutate(Headcount_Exp_tmp, exp_sc = Expenditure-Income)

Headcount_Exp_tmp1 <- spread(Headcount_Exp_tmp[,-which(colnames(Headcount_Exp_tmp) %in% c('Expenditure','Income'))], 
                             scenario, exp_sc)
Headcount_Exp <- Headcount_Exp_tmp1
Headcount_Exp[,which(colnames(Headcount_Exp) %in% lis_scenario)] <- Headcount_Exp_tmp1[,which(colnames(Headcount_Exp_tmp1) %in% lis_scenario)]-Headcount_Exp_tmp1[,which(colnames(Headcount_Exp_tmp1) == 'Baseline')]

Headcount_Exp <- gather(Headcount_Exp, scenario, Value, lis_scenario_mitigation)%>%
  mutate(Effect = 'Price_side')

Headcount_Exp_price <- Headcount_Exp
data <- mutate(Headcount_Exp_price,scenario0 = '0')
for(i in 1:length(c(lis_scenario_noex))){
  data[which(startsWith(as.character(data$scenario),lis_scenario_noex[i])),]$scenario0 <- lis_scenario_noex[i] 
}
data1 <- cbind(data,str_split(data$scenario,"_",simplify = TRUE)) %>%
  mutate(scenario2 = '0')%>%
  rename.vars(from = c('1','2','scenario2'), to = c('scenario1','scenario11','scenario2'))

data1[which(data1$scenario11 != 'exempt'),]$scenario2 <- 'non-exemption'
data1[which(data1$scenario11 == 'exempt'),]$scenario2 <- 'exemption'
data1$scenario2 <- factor(data1$scenario2, levels = c('non-exemption','exemption'))
data1 <- data1[,-which(colnames(data1) %in% c('Ref','q_Value','scenario0','scenario','scenario11'))]

Headcount_Exp_price <- spread(data1,scenario2,Value)%>%
  rename.vars(from = c('non-exemption','exemption','scenario1'), to = c('Total_price','Indirect_price','scenario'))%>%
  mutate(Direct_price = Total_price-Indirect_price, )
Headcount_Exp_price <- gather(Headcount_Exp_price, Effect, Value, Total_price,Indirect_price,Direct_price)

Headcount_eff <- rbind(Headcount_Exp_price,Headcount_Inc)
Headcount_eff$Effect <- factor(Headcount_eff$Effect, levels= c('Income','Total_price','Indirect_price','Direct_price'))

## Fig 2c Poverty headcount by Effect ----
pdata <- filter(Headcount_eff,
                scenario %in% c(lis_scenario_noex),
                Effect %in% c('Income','Direct_price','Indirect_price'),
                Y %in% c('2030','2040','2050'),
                TH1 %in% c('1.9-threshold','3.2-threshold','5.5-threshold')
) 

p_SI7b <- ggplot() + 
  geom_col(data = pdata ,
           mapping = aes(x = Y, y = Value/1000000, 
                         fill = Effect), 
           # group = Effect,
           position="stack") +
  geom_abline(intercept = 0, slope = 0, colour = 'grey')+
  labs(title = "b) Additional poverty headcount by carbon taxes", 
       x = 'Year',
       y = 'Headcount | Million people') +
  # xlab("Year") +
  # ylab("Headcount | Million people") +
  facet_wrap(TH1~scenario,scales = "free", nrow = 3, ncol = 4) +
  theme(text = element_text(size=16,  family="serif"),
        axis.text.x = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        axis.text.y = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        #  axis.line.x=element_blank(),
        # axis.ticks.y.left = element_blank(),
        panel.grid =element_blank(),
        axis.line = element_line(size=0.5, colour = "black"),
        panel.background = element_blank(),
        #legend.position = 'bottom'
  )+
  guides(fill = guide_legend(title = 'Effect', ncol = 1, byrow = TRUE)) +
  scale_color_discrete(breaks = lis_scenario)
p_SI7b


## Paste the figs his+projection+effect ----
p_SI7a
p_SI7b

grid.newpage()  ###新建图表版面
pushViewport(viewport(layout = grid.layout(20,20))) ####将版面分成2*2矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)} 
print(p_SI7a, vp = vplayout(1:9,1:19))   ###将（1,1)和(1,2)的位置画图chart3
print(p_SI7b, vp = vplayout(10:20,1:20))     ###将(2,1)的位置画图chart2  

dev.off() #
# "..//..//..//fig/1"
# "Fig SI7b Poverty headcount' 1000:1400 
