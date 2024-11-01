
## Load package ----
library(gdxrrw)
library(ggplot2)
library(tidyverse)
library(plyr)
library(cowplot)
library(gdata)
library(grid)
library(base)

## Directory assignment and data loading ----
# CGE directory and data 

# World development database 
dir_wdi <- "..//..//..//data\\WDI"
lis_xlsxwdi <-  list.files(dir_wdi,pattern = ".xlsx", full.names = TRUE)
lis_xlsxwdi

# Output directory 
dir_Povdecom_fig <- "..//..//..//fig\\Povdecomp"


# 5 Price elasticity block ----
## Directory ----
demand <- "..//..//..//data\\phioutput\\gdx/CHN_demand.gdx"

## Function ----
F_DEC_PriceEla <- function(Demand_data,lis_ref,lis_y,lis_I){
  Seg_pop <- rgdx.param(Demand_data, "FreqSegall")
  colnames(Seg_pop) <- c('Ref','Y','Seg','pop')
  
  PriceEla <- rgdx.param(demand, "PriceEla") %>%
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
  data <- merge(PriceEla, c, by = c('Ref','Y'))
  for(m in 1:length(lis_I)){
    for(r in 1:length(lis_ref)){
      for(i in 1:length(lis_y)){ # i is the index for "Y"
        tmp <- data[which(data$Y == lis_y[i] & data$Ref == lis_ref[r] & data$I == lis_I[m]),]
        q_value <-  c(Q_lognormal(q,tmp$Mu_exp[1],tmp$Sigma_exp[1])[-10],max(tmp$Seg3))
        for(j in 1:length(q)){
          if(j == 1){
            tmp2 <- tmp[which(tmp$Seg3 <= q_value[[j]]),]
            tmp3 <- data.frame(t(sum(tmp2$PriceEla*tmp2$pop)/sum(tmp2$pop)))%>%
              mutate(Y = lis_y[i], Ref = lis_ref[r], I = lis_I[m],
                     quantile = q[[j]], q_value = q_value[j])
            colnames(tmp3)[1] <- 'PriceEla'
            output_1 <- tmp3
          }
          else{
            tmp2 <- tmp[which(tmp$Seg3 <= q_value[[j]] & tmp$Seg3 > q_value[[j-1]]),]
            tmp3 <- data.frame(t(sum(tmp2$PriceEla*tmp2$pop)/sum(tmp2$pop))) %>%
              mutate(Y = lis_y[i], Ref = lis_ref[r],I = lis_I[m],
                     quantile = q[[j]], q_value = q_value[j])
            colnames(tmp3)[1] <- 'PriceEla'
            output_1 <- rbind.data.frame(output_1,tmp3)
          }
        }
        output_2 <- rbind(output_2,output_1)
      }
    }
  }
  return(output_2)
}

## Fig 51 Decile Price Elasticity of NPbaseline ----
output_2 <- F_DEC_PriceEla(demand,lis_ref,lis_y,lis_I)

PriceEla <- mutate(output_2, 
                   DEC = recode(as.character(output_2$quantile),
                                '0.1' = '1',
                                '0.2' = '2',
                                '0.3' = '3',
                                '0.4' = '4',
                                '0.5' = '5',
                                '0.6' = '6',
                                '0.7' = '7',
                                '0.8' = '8',
                                '0.9' = '9',
                                '1' = '10')) %>%
  rename.vars(from = 'Ref', to = 'scenario')

PriceEla$I <- factor(PriceEla$I,levels = lis_I)

PriceEla$I <- recode(PriceEla$I,'Food and nonalcoholic beverages' = 'FNB',
                     'Alcoholic beverages, tobacco, and narcotics' = 'ATN',
                     'Clothing and footwear' = 'CFW',
                     'Housing, water, electricity, gas and other fuels' = 'HWE',
                     'Furnishings, household equipment and maintenance' = 'FHE',
                     'Health' = 'HLT',
                     'Transport' = 'TRN',
                     'Communication' = 'CMN',
                     'Recreation and culture' = 'REC',
                     'Education' = 'EDC',
                     'Restaurants and hotels' = 'REH',
                     'Miscellaneous goods and services' = 'MGS')#%>%

PriceEla$scenario <- recode(PriceEla$scenario,
                            'SSP2_Baseline'=  lis_scenario[1],
                            'SSP2_NS1_Baseline'=  lis_scenario[2],
                            'SSP2_NS1_CM1'=  lis_scenario[3],
                            'SSP2_NS1_CM2'=  lis_scenario[4],
                            'SSP2_NS1_CM3'=  lis_scenario[5],
                            'SSP2_NS1_CM4'=  lis_scenario[6],
                            'SSP2_NS1_CM1_exempt'=  lis_scenario[7],
                            'SSP2_NS1_CM2_exempt'=  lis_scenario[8],
                            'SSP2_NS1_CM3_exempt'=  lis_scenario[9],
                            'SSP2_NS1_CM4_exempt'=  lis_scenario[10],
                            'SSP2_1p5deg'=  lis_scenario[11],
                            'SSP2_2deg'=  lis_scenario[12],
                            'SSP2_NS1_CM5'=  lis_scenario[13],
                            'SSP2_NS1_CM6'=  lis_scenario[14])
## Plotting ----

pdata <- filter(PriceEla,Y %in% c('2020','2030','2040','2050'),scenario == 'Baseline')


p_51 <- ggplot(data =  pdata,
               mapping = aes(x = factor(DEC,levels = lis_DEC), y = PriceEla, 
                             group = Y,
                             color =  factor(Y, levels = c('2020','2030','2040','2050')))) +
  geom_abline(intercept = -1, slope = 0, colour = 'grey') +
  geom_line() +
  xlab("Year") +
  ylab("expenditure change| %") +
  facet_wrap(~factor(I, levels = lis_I_abb), scales = 'free') +
  theme(text = element_text(size=14,  family="serif"),
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
        legend.position = 'bottom')  +
  guides(color = guide_legend(title = 'Scenario', row = 1, byrow = TRUE))
p_51

ggsave(p_51,filename = "Fig 51 Decile Price Elasticity of Baseline.png", 
       path = dir_Povdecom_fig,
       width = 15,height = 15)

