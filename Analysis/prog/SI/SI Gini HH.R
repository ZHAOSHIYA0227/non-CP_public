# This program is for Gini coefficient calculation in HH data for CPP1
# By Shiya Zhao, 2022/02/02

library(gdxrrw)
library(ggplot2)
library(tidyverse)
library(gdata)
library(grid)
library(DescTools)

prog_loc <- "C:/Users/shiya/taiki/model/AIMPHI/analysis/R/"
setwd(paste0(prog_loc,"carbon_tax_distribution/"))

source('0_PlotSettings.R')
source('0_Directories.R')

# Household data ----
## Directory assignment and data loading ----

CFPS_CHN8 <- 	"../../data/CFPSoutput/tmp/CFPS_CHN8.gdx"
input <- "../../data/PHIoutput/Inputdata.gdx"

# Plot 1 Gini coefficient ----
Gini_WDI <- rgdx.param(input, "Gini") %>%
  filter(R == "CHN",
         Ref %in% c("SSP2") ,
         Y%in% c(seq(2010,2018,2))) %>%
  select(-"Ref")
colnames(Gini_WDI)

ExpenditureCapita <- CFPS_CHN8 %>%
  rgdx.param("ExpenditureCapita") %>%
  gdata::rename.vars(from = colnames(.),
                     to = c("R","Y","fid_y","fid_10","urban","familysize","Category","value")) %>%
  filter(Category == "Total") %>%
  select(c("R","Y","fid_y","urban","familysize","value")) %>%
  gdata::rename.vars("value","income") %>%
  transform(income = as.double(as.character(income))) %>%
  transform(familysize = as.double(as.character(familysize)))



# Gini coefficient projections ----

Gini_HH_output <- data.frame()

for(u in 0:2){
  if(u == 2){
    df_1 <- ExpenditureCapita # national
  }else{
    df_1 <- ExpenditureCapita %>%
      filter(urban == u) # u = 1, urban; u = 0, rural
  }
  lis_df_1_y <- df_1$Y %>%
    unique() %>%
    as.character() %>%
    as.numeric()
  
  for(i in 0:length(lis_df_1_y)){
    if(i == 0){
      df <- df_1
      y <- "all"
    }else{
      df <- ExpenditureCapita %>%
        filter(Y == lis_df_1_y[i])
      y <- lis_df_1_y[i]
    }
    Gini_HH_i <-  data.frame(Y = y,
                             Gini = DescTools::Gini(df$income,df$familysize),
                             urban = u,
                             R = r)
    Gini_HH_output <- rbind(Gini_HH_output,Gini_HH_i)
  }
}
Gini_HH <- Gini_HH_output %>%
  mutate(urban = recode(.$urban,
                        "0" = "rural",
                        "1" = "urban",
                        "2" = "national"))


Gini <- Gini_HH %>%
  filter(urban == "national") %>%
  mutate(value_HH = Gini * 100) %>%
  select("Y","value_HH") %>%
  full_join(Gini_WDI) %>%
  gdata::rename.vars("Gini","value_WDI")
  
csvGini <- Gini %>%
  select("R","Y","value_HH","value_WDI") %>%
  write.csv(file = paste0(dir_csv,"Gini_HH.csv"))
