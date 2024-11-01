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
library(ggpmisc)

prog_loc <- "E:/szhao/model/AIMPHI/analysis/R/"
setwd(paste0(prog_loc,"carbon_tax_distribution/"))

source('0_PlotSettings.R')
source('0_Functions.R')
source('0_Maps.R')
## Directory assignment and data loading ----
# CGE directory and data 

AIDADSCalib_prj_Phi <- "../../data/PHIoutput/AIDADSCalibNationResults.gdx"

AIDADSCalib_prj_GCD <- "../../data/PHIoutput/AIDADSCalibNationResults_DF.gdx"


# lis_gdxAIDADSCalib <- list.files(dir_AIDADSCalib,pattern = ".gdx", full.names = TRUE)


## Lists ----
lis_setting_name <- c('Without national data','With national data')

names(lis_setting_name) <- c('S0', 'S1')


lis_IAGG <- c('Clothing and footwear',
              'Housing, water, electricity, gas and other fuels',
              'Health',
              'Miscellaneous goods and services',
              'AGG Food and beverages',
              'AGG Daily, communication and transportation',
              'AGG Education and entertainment',
              'Total')	





## Fig SI5a EST national calibration DF and NS ----

GCD <- rgdx.param("../../../model/data/GCD.gdx","ConsumptionCPPPP")%>%
  filter(R == 'CHN') %>%
  gdata::rename.vars(from = colnames(.), to = c('R','ConsumptionSegment','Area','I','GCD'))

GCD$Area <- str_to_lower(GCD$Area)  #小�??
GCD <- unite(GCD, "Seg",ConsumptionSegment,Area) %>%
  filter(Seg %in% lis_GCD_Seg)


PoutputNational_DF <- rgdx.param(AIDADSCalib_prj_GCD, "PoutputNational") %>%
    mutate(setting = 'DF') %>%
  gdata::rename.vars(from = colnames(.), 
                     to = c('R','Indicator','VC','Stat','Seg','Y','I','PoutputNational','setting')) %>%
  filter( R == 'CHN')

PoutputNational_NS <- rgdx.param(AIDADSCalib_prj_Phi, "PoutputNational") %>%
  mutate(setting = 'NS') %>%
  gdata::rename.vars(from = colnames(.), 
                     to = c('R','Indicator','VC','Stat','Seg','Y','I','PoutputNational','setting')) %>%
  filter( R == 'CHN')

PoutputNational <- rbind(PoutputNational_DF,PoutputNational_NS) %>%
  left_join(MapI) %>%
  select(-c("Stat"))

EST_NS <- PoutputNational %>%
  filter(setting == "NS",
         VC == "EST") %>%
  dplyr::rename()

EST_DF <- PoutputNational %>%
  filter(setting == "DF",
         VC == "EST")

EST <- PoutputNational %>%
  filter(VC == "EST") %>%
  spread(setting, PoutputNational) %>%
  dplyr::rename(EST_DF = DF, EST_NS = NS) %>%
  select(-c("VC"))
  
obs <- PoutputNational %>%
  filter(VC == "obs") %>%
  spread(setting, PoutputNational) %>%
  dplyr::rename(obs_DF = DF, obs_NS = NS) %>%
  select(-c("VC"))

EST_ob <- EST %>%
  full_join(obs)

## Plot national----
pdata <-  PoutputNational %>%
  spread(VC, PoutputNational) %>%
  left_join(data.frame(setting = c("DF","NS"),
                       setting_full = c("Default","National")))
# pdata$setting <- recode(pdata$setting,'DF'='Default','NS'='National')

# write.csv(x = PoutputNational, file = "AIDADS_estimates.csv",
          # row.names = FALSE)

p_SI5a <- ggplot(data = pdata ,
                 mapping = aes(x = obs, y = EST)) +
  geom_point(data = pdata ,
             mapping = aes(x = obs, y = EST, 
                           colour = factor(I_abb,levels = lis_I_abb)),
             size = 0.8) +
  geom_abline(intercept = 0 ,slope = 1, color = 'grey')+
  labs(title = 'a) Calibration performance',
       x = 'Observation | PPP$(2010)',
       y = 'Estimation | PPP$(2010)') +
  facet_wrap(~setting_full,scales = "free", ncol = 4, nrow = 3) +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        mapping = aes(label =..rr.label..),
                        parse = TRUE) +
  MyTheme  +
  guides(color = guide_legend(title = 'Scenario', ncol = 2, byrow = TRUE)) +
  scale_fill_manual(values = palette_I_abb)  
p_SI5a
ggsave(p_SI5a,filename = "Fig SI5a all_estimation.png", path = dir_fig, 
       width = 10,height = 5)



## Fig SI5b EST national calibration DF by I ----
## Plot ----
pdata <- PoutputNational %>%
  filter(setting == 'DF') %>%
  spread(VC, PoutputNational)

p_SI5b <- ggplot(data = pdata ,
                 mapping = aes(x = obs, y = EST)) +
  geom_point(size = 0.8) +
  geom_abline(intercept = 0 ,slope = 1, color = 'grey') +
  labs(title = 'b) Estimation by I (Default)',
       x = 'Observation | PPP$(2010)',
       y = 'Estimation | PPP$(2010)') +
  facet_wrap(~factor(I_abb,levels = lis_I_abb),scales = "free",
             ncol = 4, nrow = 3) +
  MyTheme  +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        mapping = aes(label =..rr.label..),
                        parse = TRUE) +
  guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE)) 


p_SI5b
ggsave(p_SI5b,filename = "Fig SI5b estimation_I.png", path = dir_fig, width = 14,height = 10)


## Fig SI5c EST national calibration NS by I ----
## Plot ----
pdata <- PoutputNational %>%
  filter(setting == 'NS') %>%
  spread(VC, PoutputNational)

p_SI5c <- ggplot(data = pdata ,
                 mapping = aes(x = obs, y = EST)) +
  geom_point(size = 0.8) +
  geom_abline(intercept = 0 ,slope = 1, color = 'grey') +
  labs(title = 'c) Estimation by I (National)',
       x = 'Observation | PPP$(2010)',
       y = 'Estimation | PPP$(2010)') +
  facet_wrap(~factor(I_abb,levels = lis_I_abb),scales = "free", ncol = 4, nrow = 3) +
  MyTheme  +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        mapping = aes(label =..rr.label..),
                        parse = TRUE) +
  guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE)) 

p_SI5c

ggsave(p_SI5c,filename = "Fig SI5c estimation_I_ns.png", path = dir_fig, width = 14,height = 10)




p_SI5c1 <- ggplot(data = pdata ,
                 mapping = aes(x = Seg, y = obs, fill = I)) +
  geom_col(position = 'fill') +
  geom_abline(intercept = 0 ,slope = 1, color = 'grey') +
  labs(title = 'c) Estimation by I (National)',
       x = 'Observation | PPP$(2010)',
       y = 'Estimation | PPP$(2010)') +
  facet_wrap(~Y,scales = "free") +
  #facet_wrap(~factor(I,levels = lis_I_abb),scales = "free", ncol = 4, nrow = 3) +
  MyTheme +
  guides(color = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE)) +
# scale_x_discrete(breaks=seq(2010, 2050, 10)) +
 scale_fill_manual(values = palette_I_abb)
p_SI5c1


a <- filter(pdata,EST == max(pdata$EST))


## Paste ----
p_SI5a
p_SI5b
p_SI5c

png(filename = paste0(dir_fig,"tiff/SI 132 R-square.png"),
    res = 300,
    width = 18,height = 24, unit = "cm")

grid.newpage()  ###新建图表版面
pushViewport(viewport(layout = grid.layout(15,10))) ####�?版面�?�?2*2矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)} 
print(p_SI5a, vp = vplayout(1:3,2:9))
print(p_SI5b, vp = vplayout(4:9,1:10))     ###�?(2,1)�?位置画图chart2    
print(p_SI5c, vp = vplayout(10:15,1:10)) 

dev.off()
# Fig SI 5 Estimation_observation.png 1200:1800
##THE END ----

## COICOP and AGG expenditure ----
# Data formation ----

## DF --
lis_GCD_TH_y <- c(2.97*365,8.44*365,23.03*365)
lis_I8_I <- c('I1','I2','I3','I4','I5','I6','I7','I8')

CFPS_exp_IR1 <- read.csv("../CFPS/output/csv/CFPS_exp_I8_IR_PPP.csv")
data <- CFPS_exp_IR1[,-which(colnames(CFPS_exp_IR1) %in% c('X','fid_y','fid_10','Exp','R','Econ'))]
year <- c(2010)
output_1 <- data.frame()
output_2 <- data.frame()
q1 <- c(2.97*365,8.44*365,23.03*365,"max(tmp$Income)")
data <- data/as.vector(data$familysize)
for(i in 1:length(year)){ # i is the index for "year"
  for(n in 0:2){ # n is the index for "urban(1),rural(0),national(2)"
    
    if (n !=2){
      if(n == 0){
        quantile <- c('Lowest_rural','Low_rural','Middle_rural','Higher_rural','All_rural')
      }
      else{
        quantile <- c('Lowest_urban','Low_urban','Middle_urban','Higher_urban','All_urban')
      }
      tmp <- data[which(data$year == year[i] & data$urban == n),]
      q <- c(2.97*365,8.44*365,23.03*365,max(tmp$Income))
      for(j in 1:(length(q1)+1)){
        if(j == 1){
          tmp_1 <- tmp[which(tmp$Income <= q[[j]]),]
          tmp_2 <- c(year[i],apply(tmp_1[,which(colnames(tmp_1) %in% c('urban','familysize',lis_I8_I))],2,sum)/nrow(tmp_1)) #sum(tmp_1$familysize))
          
          output_1 <- tmp_2
        }
        else{
          if(j != (length(q1)+1)){
            tmp_1 <- tmp[which(tmp$Income <= q[[j]] & tmp$Income > q[[j-1]]),]
            tmp_2 <- c(year[i],apply(tmp_1[,which(colnames(tmp_1) %in% c('urban','familysize',lis_I8_I))],2,sum)/nrow(tmp_1)) # sum(tmp_1$familysize))
            output_1 <- rbind.data.frame(output_1,tmp_2)
          }
          else{
            tmp_1 <- tmp
            tmp_2 <- c(year[i],apply(tmp_1[,which(colnames(tmp_1) %in% c('urban','familysize',lis_I8_I))],2,sum)/nrow(tmp_1)) # sum(tmp_1$familysize))
            output_1 <- rbind.data.frame(output_1,tmp_2)        
          }
        }
      }
      
      names(output_1) <-c('year','urban','familysize',lis_I8_I)  
      output_1 <- cbind(quantile,output_1)
      output_2 <- rbind(output_2,output_1)
    }
    else{
      quantile <- c('Lowest_national','Low_national','Middle_national','Higher_national','All_national')
      tmp <- data[which(data$year == year[i]),]
      q <- c(2.97*365,8.44*365,23.03*365,max(tmp$Income))
      for(j in 1:(length(q1)+1)){
        if(j == 1){
          tmp_1 <- tmp[which(tmp$Income <= q[[j]]),]
          tmp_2 <- c(year[i],apply(tmp_1[,which(colnames(tmp_1) %in% c('urban','familysize',lis_I8_I))],2,sum)/nrow(tmp_1)) #sum(tmp_1$familysize))
          
          #tmp_2 <- apply(tmp_1[,which(colnames(tmp_1) %in% c('year','urban','familysize',lis_I8_I))],2,sum)/sum(tmp_1$familysize) 
          output_1 <- tmp_2
        }else{
          if(j !=(length(q1)+1)){
            tmp_1 <- tmp[which(tmp$Income <= q[[j]] & tmp$Income > q[[j-1]]),]
            tmp_2 <- c(year[i],apply(tmp_1[,which(colnames(tmp_1) %in% c('urban','familysize',lis_I8_I))],2,sum)/nrow(tmp_1)) #sum(tmp_1$familysize))
            
            # tmp_2 <- apply(tmp_1[,which(colnames(tmp_1) %in% c('year','urban','familysize',lis_I8_I))],2,sum)/sum(tmp_1$familysize)
            output_1 <- rbind.data.frame(output_1,tmp_2)
          }
          else{
            tmp_1 <- tmp
            tmp_2 <- c(year[i],apply(tmp_1[,which(colnames(tmp_1) %in% c('urban','familysize',lis_I8_I))],2,sum)/nrow(tmp_1)) #sum(tmp_1$familysize))
            # tmp_2 <- apply(tmp_1[,which(colnames(tmp_1) %in% c('year','urban','familysize',lis_I8_I))],2,sum)/sum(tmp_1$familysize)
            output_1 <- rbind.data.frame(output_1,tmp_2)        
          }
        }
        
      }
      names(output_1) <-c('year','urban','familysize',lis_I8_I) 
      output_1 <- cbind(quantile,output_1)
      output_2 <- rbind(output_2,output_1)
    }
  }  
}
CFPS_obs_8_0 <- output_2
CFPS_obs_8_0 <-  mutate(output_2,Iagg1 = output_2$I1,
                        Iagg2 = output_2$I2,
                        Iagg3 = output_2$I3,
                        Iagg4 = output_2$I4+output_2$I6,
                        Iagg5 = output_2$I5,
                        Iagg6 = output_2$I7,
                        Iagg7 = output_2$I8)
CFPS_obs_8_01 <- CFPS_obs_8_0[,-which(colnames(CFPS_obs_8_0) %in% c(lis_I8_I,'urban','familysize'))]%>%
  gather(I, OBS,lis_Iagg7[-which(lis_Iagg7 == 'Total')])
colnames(CFPS_obs_8_01) <- c('Seg','Y','I','OBS')



## est 2010 --
Exp_est_0 <- rgdx.param("../../../data/phioutput/gdx/summaryAIDADS/AIDADSCalibNationResults0.gdx",
                        "PoutputNational") %>%
  filter( i2 != 'share')
colnames(Exp_est_0) <- c('R','Indicator','VC','Stat','Seg','Y','I','S0')
Exp_est_0 <- Exp_est_0[,-which(colnames(Exp_est_0) %in% c('Stat','Indicator'))]
Exp_est_0 <- spread(Exp_est_0,VC,S0)%>%
  mutate(setting = 'S0')

Exp_est_0$I <- recode(Exp_est_0$I,'Food and nonalcoholic beverages' = 'CP01',
                      'Alcoholic beverages, tobacco, and narcotics' = 'CP02',
                      'Clothing and footwear' = 'CP03',
                      'Housing, water, electricity, gas and other fuels' = 'CP04',
                      'Furnishings, household equipment and maintenance' = 'CP05',
                      'Health' = 'CP06',
                      'Transport' = 'CP07',
                      'Communication' = 'CP08',
                      'Recreation and culture' = 'CP09',
                      'Education' = 'CP10',
                      'Restaurants and hotels' = 'CP11',
                      'Miscellaneous goods and services' = 'CP12',
                      'Food and beverages' = 'CP0102')

Exp_est_01 <- spread(Exp_est_0[,-which(colnames(Exp_est_0) == 'obs')],I,EST)

Exp_est_02 <- spread(Exp_est_0[,-which(colnames(Exp_est_0) == 'EST')],I,obs)
Exp_est_02 <-   mutate(Exp_est_02,Iagg1 = Exp_est_02$CP01+Exp_est_02$CP02, #"AGG Food and beverages"
                       Iagg2 = Exp_est_02$CP03, # "Clothing and footwear"
                       Iagg3 = Exp_est_02$CP04,# "Housing, water, electricity, gas and other fuels"
                       Iagg4 = Exp_est_02$CP05+Exp_est_02$CP07+Exp_est_02$CP08, # "AGG Daily, communication and transportation"
                       Iagg5 = Exp_est_02$CP06, # "Health"
                       Iagg6 = Exp_est_02$CP10+Exp_est_02$CP09+Exp_est_02$CP11, # "AGG Education and entertainment"
                       Iagg7 = Exp_est_02$CP12)
Exp_est_02 <- Exp_est_02[,-which(colnames(Exp_est_02) %in% lis_I_CP)]%>%
  gather(I,GCD, lis_Iagg7[-which(lis_Iagg7 == 'Total')])


Exp_est_01 <-   mutate(Exp_est_01,Iagg1 = Exp_est_01$CP01+Exp_est_01$CP02, #"AGG Food and beverages"
                       Iagg2 = Exp_est_01$CP03, # "Clothing and footwear"
                       Iagg3 = Exp_est_01$CP04,# "Housing, water, electricity, gas and other fuels"
                       Iagg4 = Exp_est_01$CP05+Exp_est_01$CP07+Exp_est_01$CP08, # "AGG Daily, communication and transportation"
                       Iagg5 = Exp_est_01$CP06, # "Health"
                       Iagg6 = Exp_est_01$CP10+Exp_est_01$CP09+Exp_est_01$CP11, # "AGG Education and entertainment"
                       Iagg7 = Exp_est_01$CP12)
Exp_est_01 <- Exp_est_01[,-which(colnames(Exp_est_01) %in% lis_I_CP)]%>%
  gather(I,EST, lis_Iagg7[-which(lis_Iagg7 == 'Total')])

Exp_est_obs_0 <- merge(Exp_est_01,CFPS_obs_8_01, by = c('Y','Seg','I'))%>%
  rbind()

Exp_est_obs_02 <- merge(Exp_est_02,CFPS_obs_8_01, by = c('Y','Seg','I'))%>%
  rbind()

c1 <- merge(Exp_est_obs_02,Exp_est_obs_3, by = c())
pSI31a2 <- ggplot(data = filter(Exp_est_obs_02,
                                setting == 'S0'),
                  mapping = aes(x = OBS, y = GCD))+#, color = I)) +  #,colour = Seg
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = 'grey') +
  xlab("National data | PPP$") +
  ylab("GCD | PPP$") +
  facet_wrap(~I,scales = "free", ncol = 4)  + #labeller = labeller(setting = lis_settings))+
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
        #legend.position = 'bottom',
        legend.text = element_text(size = 10)
        # legend.title = element_text(text = 'Commodity'),guides(fill = guide_legend(title = 'Commodity', ncol = 5, byrow = TRUE)) 
  ) +
  labs(color = 'Commodity category')+guides(color = guide_legend(ncol = 1))
pSI31a2
ggsave(pSI31a2,filename = "fig SI31a2 obs_CFPS_GCD.png", path = dir_AIDADSCalib_fig,width = 13, height = 7)


# National --
Exp_est_3 <- rgdx.param(AIDADSCalib,
                        "PoutputNational") %>%
  filter( i2 != 'share') #i3 == 'EST',
colnames(Exp_est_3) <- c('R','Indicator','VC','Stat','Seg','Y','I','S3')
Exp_est_3 <- Exp_est_3[,-which(colnames(Exp_est_3) %in% c('Stat','Indicator'))]

Exp_est_3 <- spread(Exp_est_3,VC,S3)%>%
  mutate(setting = 'S3')

Exp_est_3$I <- recode(Exp_est_3$I,'Food and nonalcoholic beverages' = 'CP01',
                      'Alcoholic beverages, tobacco, and narcotics' = 'CP02',
                      'Clothing and footwear' = 'CP03',
                      'Housing, water, electricity, gas and other fuels' = 'CP04',
                      'Furnishings, household equipment and maintenance' = 'CP05',
                      'Health' = 'CP06',
                      'Transport' = 'CP07',
                      'Communication' = 'CP08',
                      'Recreation and culture' = 'CP09',
                      'Education' = 'CP10',
                      'Restaurants and hotels' = 'CP11',
                      'Miscellaneous goods and services' = 'CP12',
                      'Food and beverages' = 'CP0102')

Exp_est_31 <- spread(Exp_est_3[,-which(colnames(Exp_est_3) == 'obs')],I,EST)
Exp_est_31 <-   mutate(Exp_est_31,Iagg1 = Exp_est_31$CP01+Exp_est_31$CP02, #"AGG Food and beverages"
                       Iagg2 = Exp_est_31$CP03, # "Clothing and footwear"
                       Iagg3 = Exp_est_31$CP04,# "Housing, water, electricity, gas and other fuels"
                       Iagg4 = Exp_est_31$CP05+Exp_est_31$CP07+Exp_est_31$CP08, # "AGG Daily, communication and transportation"
                       Iagg5 = Exp_est_31$CP06, # "Health"
                       Iagg6 = Exp_est_31$CP10+Exp_est_31$CP09+Exp_est_31$CP11, # "AGG Education and entertainment"
                       Iagg7 = Exp_est_31$CP12) # "Miscellaneous goods and services"
Exp_est_31 <- Exp_est_31[,-which(colnames(Exp_est_31) %in% lis_I_CP)]%>%
  gather(I,EST, lis_Iagg7[-which(lis_Iagg7 == 'Total')])

Exp_est_obs_3 <- merge(Exp_est_31,CFPS_obs_2, by = c('R','Y','Seg','I'))
## Fig SI31a R-sqr COICOP expenditure default----
c1 <- rbind(Exp_est_obs_0,Exp_est_obs_3)
pSI31a <- ggplot(data = filter(c1,
                               setting == 'S0'),
                 mapping = aes(x = OBS, y = EST))+#, color = I)) +  #,colour = Seg
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = 'grey') +
  xlab("Observation | PPP$") +
  ylab("Estimation | PPP$") +
  facet_wrap(~I,scales = "free", ncol = 4)  + #labeller = labeller(setting = lis_settings))+
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
        #legend.position = 'bottom',
        legend.text = element_text(size = 10)
        # legend.title = element_text(text = 'Commodity'),guides(fill = guide_legend(title = 'Commodity', ncol = 5, byrow = TRUE)) 
  ) +
  labs(color = 'Commodity category')+guides(color = guide_legend(ncol = 1))
pSI31a
ggsave(pSI31a,filename = "fig SI31a obs_est_df.png", path = dir_AIDADSCalib_fig,width = 13, height = 7)


## Fig SI31b R-sqr COICOP expenditure ns----

## national data --
Exp_est_3 <- rgdx.param(AIDADSCalib,
                        "PoutputNational") %>%
  filter( i2 != 'share') #i3 == 'EST',
colnames(Exp_est_3) <- c('R','Indicator','VC','Stat','Seg','Y','I','S3')
Exp_est_3 <- Exp_est_3[,-which(colnames(Exp_est_3) %in% c('Stat','Indicator'))]

Exp_est_3 <- spread(Exp_est_3,VC,S3)%>%
  mutate(setting = 'S3')

Exp_est_3$I <- recode(Exp_est_3$I,'Food and nonalcoholic beverages' = 'CP01',
                      'Alcoholic beverages, tobacco, and narcotics' = 'CP02',
                      'Clothing and footwear' = 'CP03',
                      'Housing, water, electricity, gas and other fuels' = 'CP04',
                      'Furnishings, household equipment and maintenance' = 'CP05',
                      'Health' = 'CP06',
                      'Transport' = 'CP07',
                      'Communication' = 'CP08',
                      'Recreation and culture' = 'CP09',
                      'Education' = 'CP10',
                      'Restaurants and hotels' = 'CP11',
                      'Miscellaneous goods and services' = 'CP12',
                      'Food and beverages' = 'CP0102')

Exp_est_31 <- spread(Exp_est_3[,-which(colnames(Exp_est_3) == 'obs')],I,EST)
Exp_est_31 <-   mutate(Exp_est_31,Iagg1 = Exp_est_31$CP01+Exp_est_31$CP02, #"AGG Food and beverages"
                       Iagg2 = Exp_est_31$CP03, # "Clothing and footwear"
                       Iagg3 = Exp_est_31$CP04,# "Housing, water, electricity, gas and other fuels"
                       Iagg4 = Exp_est_31$CP05+Exp_est_31$CP07+Exp_est_31$CP08, # "AGG Daily, communication and transportation"
                       Iagg5 = Exp_est_31$CP06, # "Health"
                       Iagg6 = Exp_est_31$CP10+Exp_est_31$CP09+Exp_est_31$CP11, # "AGG Education and entertainment"
                       Iagg7 = Exp_est_31$CP12) # "Miscellaneous goods and services"
Exp_est_31 <- Exp_est_31[,-which(colnames(Exp_est_31) %in% lis_I_CP)]%>%
  gather(I,EST, lis_Iagg7[-which(lis_Iagg7 == 'Total')])

Exp_est_obs_3 <- merge(Exp_est_31,CFPS_obs_2, by = c('R','Y','Seg','I'))
c <- Exp_est_obs_3
pSI31b <- ggplot(data = c,
                 mapping = aes(x = OBS, y = EST)) + #,colour = Seg
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = 'grey') +
  xlab("Observation | PPP$") +
  ylab("Estimation | PPP$") +
  facet_wrap(~I,scales = "free", ncol = 4) + #labeller = labeller(setting = lis_settings))+
  theme(text = element_text(size=16,  family="serif"),
        axis.text.x = element_text(size = 10,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        axis.text.y = element_text(size = 10,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        panel.grid =element_blank(),
        axis.line = element_line(size=0.5, colour = "black"),
        panel.background = element_blank(),
        # legend.position = 'bottom',
        legend.text = element_text(size = 10)
        # legend.title = element_text(text = 'Commodity'),guides(fill = guide_legend(title = 'Commodity', ncol = 5, byrow = TRUE)) 
  ) +
  labs(color = 'Commodity category')+guides(color = guide_legend(nrow = 3))
pSI31b
ggsave(pSI31b,filename = "Fig SI32 EST_OBS_exp_ns.png", path = dir_AIDADSCalib_fig,width = 12, height = 7)

# c1 <- spread(PoutputNational_1,VC,PoutputNational)%>%
#   filter(Indicator == 'abs')
# c <- filter(c1,
#             setting == 'S1')%>%
#   na.omit()


## Fig SI31c R-sqr ns and df ----
c1 <- rbind(Exp_est_obs_0,Exp_est_obs_3)
pSI31c <- ggplot(data = c1,
                 mapping = aes(x = OBS, y = EST))+#, color = I)) +  #,colour = Seg
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = 'grey') +
  xlab("Observation") +
  ylab("Estimation") +
  facet_grid(~setting,scales = "free") + #labeller = labeller(setting = lis_settings))+
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
        #legend.position = 'bottom',
        legend.text = element_text(size = 10)
        # legend.title = element_text(text = 'Commodity'),guides(fill = guide_legend(title = 'Commodity', ncol = 5, byrow = TRUE)) 
  ) +
  labs(color = 'Commodity category')+guides(color = guide_legend(ncol = 1))
pSI31c
ggsave(pSI31c,filename = "fig SI31c obs_est_all.png", path = dir_AIDADSCalib_fig,width = 12, height = 6)


# R-sqr R-adj COICOP ----
c1 <- as.data.frame(rbind(Exp_est_obs_0,Exp_est_obs_3))
write.csv("df_ns_est_obs.csv", c1)

write.csv(x =  c1, file = "df_ns_est_obs.csv",
          row.names = FALSE)


S <- c('S0','S3')
#list_I <- c(lis_Iagg7,'Total')
list_I <- c("Iagg1", "Iagg2", "Iagg3", "Iagg4", "Iagg5", "Iagg6", "Iagg7")
c3 <- data.frame()
for(i in 1:2){
  c12 <- c1[which(c1$setting == S[i]),]
  for(j in 1:length(list_I)){
    c2 <- c12[which(c12$I == list_I[j]),]
    relation <- lm(as.numeric(c2$EST) ~ as.numeric(c2$OBS))
    # if(j <= length(list_I)-1){
    #   c2 <- c12[which(c12$I == list_I[j]),]
    #   relation <- lm(as.numeric(c2$EST) ~ as.numeric(c2$OBS))
    #   
    # }else{
    #   c12 <- c1[which(c1$setting == S[i+1]),]
    #   relation <- lm(as.numeric(c2$EST) ~ as.numeric(c2$OBS))
    #   
    # }
    c4 <- data.frame(S[i],list_I[j], summary(relation)$r.squared, summary(relation)$adj.r.squared)
    c3 <- rbind(c3,c4)  
  }
}
colnames(c3) <- c('setting','I','R_squ','R_adj')
Rsqr <- mutate(c3, set = 'PoutputNational')


####
## Elasticity ----

IncomeEla_DF <- rgdx.param(AIDADSCalib_prj_GCD, "PoutputIncomeElaNational") %>%
  mutate(setting = 'DF') %>%
  filter( i1 == 'CHN')
names(IncomeEla_DF) <- c('R','Seg','Y','I','value','setting')


IncomeEla_NS <- rgdx.param(AIDADSCalib_prj_Phi, "PoutputIncomeElaNational") %>%
  mutate(setting = 'NS')
names(IncomeEla_NS) <- c('R','Seg','Y','I','value','setting')

IncomeEla <- rbind(IncomeEla_DF,IncomeEla_NS)
IncomeEla$I <- recode(IncomeEla$I, 'Food and nonalcoholic beverages' = 'FNB',
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
                                       'Miscellaneous goods and services' = 'MGS')

## Fig SI 51a Income elasticity ----


## Plot ----
pdata <-  spread(IncomeEla,setting,value)%>%
  na.omit()

p_SI51a <- ggplot(data = pdata ,
                 mapping = aes(x = DF, y = NS, colour = factor(I,levels = lis_I_abb))) + 
  geom_point(size = 1.2) +
  geom_abline(intercept = 0 ,slope = 1, color = 'grey') +
  labs(title = 'a) Income elasticity from the calibration',
       x = 'Default',
       y = 'National') +
  #facet_wrap(~setting,scales = "free", ncol = 4, nrow = 3) +
  theme(text = element_text(size=16,  family="serif"),
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
        # legend.position = 'bottom'
  )  +
  scale_color_manual(values = palette_I_abb) +
 xlim(0,4) + ylim(0,4) +
 guides(color = guide_legend(title = 'Commodity', ncol = 2, byrow = TRUE)) 

p_SI51a


## Fig SI 51b Income elasticity ----
## Plot NS ----
pdata <-  spread(IncomeEla,setting,value) %>%
  mutate(IS = recode(Seg,
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
         'Lowest_rural' = 'RLowest',
         'Low_rural' = 'RLow',
         'Middle_rural' = 'RMiddle',
         'Higher_rural' = 'RHigher',
         'Lowest_urban' = 'ULowest',
         'Low_urban' = 'ULow',
         'Middle_urban' = 'UMiddle',
         'Higher_urban' = 'UHigher'))
pdata$IS <- factor(pdata$IS, levels = lis_seg_dec_gcd_1)

p_SI51b <- ggplot(data = pdata ,
                  mapping = aes(x = IS, y = NS, colour = Y)) + 
  geom_point(size = 1.2) +
  labs(title = 'b) Income elasticity from the calibration',
       x = 'Income elasticity',
       y = 'Income segments') +
  facet_wrap(~I,scales = "free",  ncol = 3, nrow = 4) + 
  theme(text = element_text(size=16,  family="serif"),
        axis.text.x = element_text(size = 14,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 90),
        axis.text.y = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        panel.grid =element_blank(),
        axis.line = element_line(size=0.5, colour = "black"),
        panel.background = element_blank(),
        # legend.position = 'bottom'
  )  +
 # scale_color_manual(values = palette_I_abb) +
 # xlim(0,4) + ylim(0,4) +
  guides(color = guide_legend(title = 'Year', ncol = 1, byrow = TRUE)) 

p_SI51b

## plot df ----
pdata <-  spread(IncomeEla,setting,value) 
pdata <- mutate(pdata,IS = recode(pdata$Seg,
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
                     'Lowest_rural' = 'RLowest',
                     'Low_rural' = 'RLow',
                     'Middle_rural' = 'RMiddle',
                     'Higher_rural' = 'RHigher',
                     'Lowest_urban' = 'ULowest',
                     'Low_urban' = 'ULow',
                     'Middle_urban' = 'UMiddle',
                     'Higher_urban' = 'UHigher')) 

pdata$IS <- factor(pdata$IS, levels = lis_seg_dec_gcd_1)
pdata <- na.omit(pdata)

p_SI51b_df <- ggplot(data = pdata ,
                     mapping = aes(x = IS, y = DF)) + 
  geom_point() +
  labs(title = 'Income elasticity from the calibration (Default)',
       x = 'Income elasticity',
       y = 'Income segments') +
  facet_wrap(~I,scales = "free",  ncol = 3, nrow = 4) + 
  theme(text = element_text(size=10,  family="serif"),
        axis.text.x = element_text(size = 10,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 90),
        axis.text.y = element_text(size = 10,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        panel.grid =element_blank(),
        axis.line = element_line(size=0.5, colour = "black"),
        panel.background = element_blank(),
        # legend.position = 'bottom'
  )  +
  # scale_color_manual(values = palette_I_abb) +
  # xlim(0,4) + ylim(0,4) +
  guides(color = guide_legend(title = 'Year', ncol = 1, byrow = TRUE)) 

p_SI51b_df
ggsave(p_SI51b_df,filename = "Fig SI5b_df IncomeEla_I_df.png", path = dir_fig, width = 5,height = 7)

## Paste ----
p_SI51a
p_SI51b

grid.newpage()  ###新建图表版面
pushViewport(viewport(layout = grid.layout(12,4))) ####�?版面�?�?2*2矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)} 
print(p_SI51a, vp = vplayout(1:3,2:3))
print(p_SI51b, vp = vplayout(4:12,1:4))     ###�?(2,1)�?位置画图chart2    

dev.off()

# "Fig SI51 Calibration Ela.png" 1600:2200




## National data ----
EXP_8_dir <- "../../../data/CFPS/CFPS_exp_DEC_CHN8.gdx"
EXP_8 <- rgdx.param(EXP_8_dir, "Expenditure") 
names(EXP_8) <- c('Seg','Y','R','I','Econ','value')

pdata <- EXP_8
p_3a  <- ggplot(data =  pdata,
                mapping = aes(x = Seg, y = value , fill = I
                              )) + 
  geom_col(position = 'fill',
           show.legend  = TRUE) +
  labs(title = "a) Expenditure pattern",
       subtitle = '(Baseline, 2020)',
       x = "Income decile",
       y = "Expenditure share",
       fill = "gear")+
  # facet_grid(~factor(c$scenario,levels = lis_scenario)) +
  facet_grid(~Y, scales = "free") +
  theme(text = element_text(size=16,  family="serif"),
        axis.text.x = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        axis.text.y = element_text(size = 16,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 0),
        panel.grid =element_blank(),
        strip.text = element_text(size = 18),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.text = element_text(size = 18),
        panel.background = element_blank(),
        #legend.position = 'bottom'
  ) +
  panel_border(color = "grey85", size = 1, linetype = 1)+
  guides(fill = guide_legend(title = 'Scenario', ncol = 1, byrow = TRUE)) #+
#  scale_fill_manual(values = palette_I_abb) 
#facet_grid(vars(scenario), vars(Y), scales = 'free')

p_3a


## T-test and P-value ----

tdata_DF <-  spread(PoutputNational,VC,PoutputNational) %>%
  filter(setting == 'DF')

t.test(tdata_DF$EST,tdata_DF$obs) # y1和y2�?为数值型向�?

tdata_NS <-  spread(PoutputNational,VC,PoutputNational) %>%
  filter(setting == 'NS')

t.test(tdata_NS$EST,tdata_NS$obs) # y1和y2�?为数值型向�?





