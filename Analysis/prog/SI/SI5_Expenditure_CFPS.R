# This program is a summary of all the figures and plots in the paper SI
# "Poverty and inequality in China under the long-term global temperature target"
# This program deals with AIDADS calibration estimates

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

prog_loc <- "E:/szhao/model/AIMPHI/analysis/R/"
setwd(paste0(prog_loc,"carbon_tax_distribution/"))

source('0_PlotSettings.R')
source('0_Functions.R')
source('0_Maps.R')
source('0_Directories.R')

## Directory assignment and data loading ----
# CGE directory and data 
GCD <- rgdx.param("..//..//..//data\\phioutput\\gdx\\GCD.gdx","ConsumptionCPPPP")%>%
  filter(R == 'CHN')
colnames(GCD) <- c('R','ConsumptionSegment','Area','I','GCD')
GCD$Area <- str_to_lower(GCD$Area)  #å°å??
GCD <- unite(GCD, "Seg",ConsumptionSegment,Area) %>%
  filter(Seg %in% lis_GCD_Seg)


# Output directory 
dir_fig <- "../output/fig/"


## Lists ----
lis_setting_name <- c(S0 = 'Without national data',
                      S1 = 'With national data')

lis_I_CHN_abb <-  c('Food&\nBeverages',
                    'Clothing',
                    'Residentials',
                    'Equipment',
                    'Health',
                    'Transportation&\nCommunication',
                    'Education&\nEntertainment',
                    'Miscellaneous')
  



palette_I_CHN_abb <- c('Food&Beverages' = '#FF3366',
                       'Clothing' = '#CC6699',
                       'Residentials' = '#FF9966',
                       'Equipment' = '#CCFF00',
                       'Health' = '#3399FF',
                       'Transportation&Communication' = '#33CC33',
                       'Education&Entertainment' = '#66CCFF',
                       'Miscellaneous' = '#999999')

lis_seg_dec_gcd_1 <- c('R1','R2','R3','R4','R5','R6','R7','R8','R9','R10',
                       'U1','U2','U3','U4','U5','U6','U7','U8','U9','U10',
                       'RLowest', 'RLow','RMiddle','RHigher',
                       'ULowest', 'ULow','UMiddle','UHigher')

## Data loading ----

CHN_hh_exp <- "../../data/PHIoutput/CHN_hh_exp.gdx"


CFPS_EXP <- rgdx.param(CHN_hh_exp, "CFPS_EXP") %>%
  mutate(Stat = 'CFPS_EXP')%>%
  mutate(I = recode(CHNSector, "Food and Beverages" = 'Food&\nBeverages',
                    "Clothing and footwear" = 'Clothing',
                    "Housing,water,electricity,gas and other fuels"	= 'Residentials',
                    "Family equipment and daily necessities" = 'Equipment',
                    "Medical and fitness" = 'Health',
                    "Communication and transportation" = 'Transportation&\nCommunication',
                    "Education and entertainment" = 'Education&\nEntertainment',
                    "Other" = 'Miscellaneous'))%>%
  mutate(IS = recode(Income,
              'DECILE1_rural' = 'R1',
              'DECILE2_rural' = 'R2',
              'DECILE3_rural' = 'R3',
              'DECILE4_rural' = 'R4',
              'DECILE5_rural' = 'R5',
              'DECILE6_rural' = 'R6',
              'DECILE7_rural' = 'R7',
              'DECILE8_rural' = 'R8',
              'DECILE9_rural' = 'R9',
              'DECILE10_rural' = 'R10',
              'DECILE1_urban' = 'U1',
              'DECILE2_urban' = 'U2',
              'DECILE3_urban' = 'U3',
              'DECILE4_urban' = 'U4',
              'DECILE5_urban' = 'U5',
              'DECILE6_urban' = 'U6',
              'DECILE7_urban' = 'U7',
              'DECILE8_urban' = 'U8',
              'DECILE9_urban' = 'U9',
              'DECILE10_urban' = 'U10',
                     'DECILE1_National' = 'N1',
                     'DECILE2_National' = 'N2',
                     'DECILE3_National' = 'N3',
                     'DECILE4_National' = 'N4',
                     'DECILE5_National' = 'N5',
                     'DECILE6_National' = 'N6',
                     'DECILE7_National' = 'N7',
                     'DECILE8_National' = 'N8',
                     'DECILE9_National' = 'N9',
                     'DECILE10_National' = 'N10'))

pdata <-  filter(CFPS_EXP, IS %in% lis_seg_dec_gcd_1)
pdata$I <- factor(pdata$I,levels = (lis_I_CHN_abb))

p_SI6a <- ggplot(data = pdata ,
                  mapping = aes(x = factor(IS,levels = (lis_seg_dec_gcd_1)), y = CFPS_EXP, 
                                fill = fct_rev(I))) + 
  geom_col(position = "fill") +
  labs( x = 'Income segment',
       y = 'Expenditure | US$(2010)') +
  facet_wrap(~year,scales = "free", nrow = 2) +
  MyTheme_x90  +
  theme(legend.key.height=unit(0.5, "cm")) +
  scale_fill_manual(values = c('Food&\nBeverages' = '#FF3366',
                               'Clothing' = '#CC6699',
                               'Residentials' = '#FF9966',
                               'Equipment' = '#CCFF00',
                               'Health' = '#3399FF',
                               'Transportation&\nCommunication' = '#33CC33',
                               'Education&\nEntertainment' = '#66CCFF',
                               'Miscellaneous' = '#999999')) +
  #xlim(0,4) + ylim(0,4) +
  guides(fill = guide_legend(title = 'Commodity', ncol = 1, byrow = TRUE)) 

p_SI6a
ggsave(p_SI6a,filename = paste0(dir_fig,"tiff/Fig SI6a Expenditure_CFPS.png"), 
       width = 20,height = 10, units = "cm")

ggsave(p_SI6a,filename = paste0(dir_fig,"eps/Fig SI6a Expenditure_CFPS.eps"), 
       width = 20,height = 10, units = "cm")
