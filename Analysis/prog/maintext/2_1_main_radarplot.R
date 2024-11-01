# The Rscript for the third plot in the non-CP/CP paper
# Shiya ZHAO, 2024.05.07

# Radar plot of the benefits of non-CP decarbonization
library(fmsb)

dir.create(paste0(dir_plot_main, "/2_1_radar/"))


# 0. function -------------------------------------------------------------

# data preparations for radar charts
F_radar_data <- function(data, v_min = 0, v_max = 1){
  pdata <- as.data.frame(pdata) %>% rbind(t(data.frame( t1 = rep(v_min,ncol(pdata)), t2 = rep(v_max,ncol(pdata))) ))
  colnames(pdata) <- pdata[1,]
  pdata <- pdata[2:nrow(pdata),]
  
  # convert all characters into numerics
  pdata <- pdata %>% mutate_all(function(x) as.numeric(as.character(x))) %>% mutate(rname = rownames(.))
  
  
  # order the dataframe according to the row names
  rownames(pdata) <- c(seq(nrow(pdata),1,-1))
  pdata <- pdata[ order(as.numeric(row.names(pdata)), decreasing = FALSE), ]
  
  rownames(pdata) <- pdata$rname
  pdata <- pdata %>% select(-rname)
  return(pdata)
  
}




# plotting beautiful radar chart
F_radarchart <- function(data, color = "#00AFBB", 
                         vlabels = colnames(data), vlcex = 0.7,
                         caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.2), plwd = 1, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}




# dimensions of the radar plot
## GDP loss
## increases in food price
## increases in energy price
## increases in poverty headcount
## increases in hunger risk
## increases in income inequality


# 1. data preparations ----------------------------------------------------
df_iamc <- cge_var %>%
  rgdx.param("IAMC_Template") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO) %>% 
  gdata::rename.vars(colnames(.), c("R", "VEMF", "Y", "value", "Ref"))


## GDP ----

df_GDPloss <- df_iamc %>% filter(VEMF == "Pol_Cos_GDP_Los_rat") %>% F_woc() %>%  
  # transform(policy = factor(policy, levels = lis_cp)) %>% 
  filter(policy != "Baseline non-CP") %>% F_filter(lis_R= lis_R) %>% select(-VEMF, -Ref) %>% 
  spread(policy, value) %>%
  pivot_longer(cols = colnames(.)[startsWith(colnames(.), "1.5C")], names_to = "policy", values_to = "value")
  
colnames(df_GDPloss)


# $ GDP | Million US dollar(2005) per year
# GDP: Million US dollar(2005) per year; GDPloss: % per year
rm(GDP)



## Consumption ----
# Loading Consumption 

# df_cns <- df_iamc %>% filter(VEMF == "CNS") %>% F_woc() %>%  
#   transform(policy = factor(policy, levels = lis_cp)) %>% filter(policy != "Baseline non-CP") %>% F_filter(lis_R= lis_R) %>% select(-VEMF) 
# 
# 
# df_cnsloss <- df_cns %>%
#   F_woc() %>% select(-Ref, -target) %>% spread(policy, value) %>%
#   pivot_longer(cols = colnames(.)[startsWith(colnames(.), "1.5C")], names_to = "policy", values_to = "value") %>% 
#   mutate(cnsloss = (Baseline-`value`)/value) 

df_cnsloss <- df_iamc %>% filter(VEMF == "Pol_Cos_Cns_Los_rat") %>% F_woc() %>%  
  # transform(policy = factor(policy, levels = lis_cp)) %>% 
  filter(policy != "Baseline non-CP") %>% F_filter(lis_R= lis_R) %>% select(-VEMF, -Ref) %>% 
  spread(policy, value) %>%
  pivot_longer(cols = colnames(.)[startsWith(colnames(.), "1.5C")], names_to = "policy", values_to = "cnsloss")

# $ consumption | billion USD_2010/yr per year
# consumption: billion USD_2010/yr per year; GDPloss: % per year
rm(df_cns)





## Food Consumption ----
# Loading Consumption 

df_fd <- df_iamc %>% filter(VEMF == "Foo_Dem") %>% F_woc() %>%  
  transform(policy = factor(policy, levels = lis_cp)) %>% filter(policy != "Baseline non-CP") %>% F_filter(lis_R= lis_R) %>% select(-VEMF) 


df_fdloss <- df_fd %>%
  F_woc() %>% select(-Ref, -target) %>% spread(policy, value) %>%
  pivot_longer(cols = colnames(.)[startsWith(colnames(.), "1.5C")], names_to = "policy", values_to = "value") %>% 
  mutate(fdloss = (Baseline-`value`)/value) 
# $ consumption | billion USD_2010/yr per year
# consumption: billion USD_2010/yr per year; GDPloss: % per year
rm(df_cns)




## Price ----
# load price change 
df_PQ_change <- paste0("../../DataArchive/PHIoutput/gdx/Inputdata.gdx") %>%
  rgdx.param("PriceChange") %>%
  left_join(MapScenario) %>% 
  left_join(MapI) %>% 
  filter(!is.na(I_abb)) %>%  transform(I_abb = factor(I_abb, levels = lis_I_abb)) %>% select(-I) %>% 
  F_woc() %>% 
  mutate(value = PriceChange) %>% 
  select(-c("SCENARIO", "PriceChange")) %>% 
  F_cha_decom() %>% 
  mutate(value = (value-Baseline_None)/Baseline_None ) %>%
  filter(Y %in% c(seq(2020, 2050, 10)),  
         I_abb %in% c("Food", "Energy"), 
         exemption == "None", Gini == "consistent", 
         !startsWith(policy, "2C"), R %in% c("WLD", "R5OECD90+EU", "R5ASIA", "R5LAM", "R5MAF", "R5REF")) %>% 
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  F_filter_main() %>% 
  pivot_wider(values_from = "value", names_from = "policy") %>% mutate(variable = paste0(I_abb, " price"))


## Poverty ----
df_PoV <- rgdx.param(AnaExp, "PoVExp") %>%
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  filter(!is.na(Ref), R %in% lis_R, Y%in% lis_Y) %>%  
  mutate(TH1 = recode(TH, 'pop_1.9' = lis_TH[1], 'pop_3.2' = lis_TH[2], 'pop_5.5' = lis_TH[3]),
         R = factor(R, levels = lis_R), unit = "person") %>% F_filter(s_Baseline_woc = F, lis_R = lis_R) %>% F_woc() 




## Hunger risk ----

df_hug <- df_iamc %>% filter(VEMF == "Pop_Ris_of_Hun") %>% mutate(unit = "million") %>% left_join(MapScenario) %>% 
  F_woc() %>% F_filter(lis_R = lis_R) %>% select(-SCENARIO)





## Gini ----
df_Gini <- rgdx.param(AnaExp, "Gini_exp") %>%
  F_filter(lis_R=unique(.$R)) %>% F_woc() %>% left_join(map_R) 




## Data integration ------------------------------------------------------

### GDP loss ----
df1_GDPloss <- df_GDPloss %>% filter(exemption == "None", target == "1.5C") %>% 
  # select(-value) %>% 
  pivot_wider(values_from = "value", names_from = "policy") %>%
  mutate(variable = "GDP loss") %>% select(-`2C non-CP`, -`2C CP`, -target)


### Consumption loss ----
df1_cnsloss <- df_cnsloss %>% #filter(SSP == "SSP2") %>% 
  # select(-Baseline, -value) %>% 
  pivot_wider(values_from = "cnsloss", names_from = "policy") %>% mutate(variable = "Consumption loss") %>% select(-`2C non-CP`, -`2C CP`, -target)




### Food Consumption loss ----
df1_fdloss <- df_fdloss %>% #filter(SSP == "SSP2") %>% 
  select(-Baseline, -value) %>% 
  pivot_wider(values_from = "fdloss", names_from = "policy") %>% mutate(variable = "Calorie intake") 



### Price change ----
df1_comprice_med <- df_PQ_change %>% select(-I_abb, -`Baseline non-CP_None`, -`Baseline_None`) 



# rm(df_PQ_change)


### Poverty headcount change ----
# unit: person
df1_PoV <- df_PoV %>% filter(exemption == "None", TH1 == "1.9-threshold") %>% select(-Ref, -target, -TH, -TH1, -unit) %>% 
  pivot_wider(values_from = "PoVExp", names_from = "policy") %>% mutate(variable = "Poverty (Int.)") %>% 
  mutate(`1.5C CP` = (`1.5C CP` - `Baseline`)/`Baseline`, `1.5C non-CP` = (`1.5C non-CP` - `Baseline`)/`Baseline`) %>% select(-Baseline, -`2C non-CP`, -`2C CP`, -`Baseline non-CP`)



df1_PoV_55 <- df_PoV %>% filter(exemption == "None", TH1 == "5.5-threshold") %>% select(-Ref, -target, -TH, -TH1, -unit) %>% 
  pivot_wider(values_from = "PoVExp", names_from = "policy") %>% mutate(variable = "Poverty (UMIC)") %>% 
  mutate(`1.5C CP` = (`1.5C CP` - `Baseline`)/`Baseline`, `1.5C non-CP` = (`1.5C non-CP` - `Baseline`)/`Baseline`) %>% select(-Baseline, -`2C non-CP`, -`2C CP`, -`Baseline non-CP`)




### Hunger risk ----
# unit: million
# colnames(df1_hug)
df1_hug <- df_hug %>% #filter(SSP == "SSP2") %>% 
  select(-VEMF, -target, -Ref, -unit) %>% pivot_wider(values_from = "value", names_from = "policy") %>% 
  mutate(variable = "Hunger risk") %>% 
  mutate(`1.5C CP` = (`1.5C CP` - `Baseline`)/`Baseline`, `1.5C non-CP` = (`1.5C non-CP` - `Baseline`)/`Baseline`) %>% select(-Baseline, -`2C non-CP`, -`2C CP`, -`Baseline non-CP`)







### Gini ----

df1_Gini <- df_Gini %>% filter(exemption == "None", !(Y %in% c(2010, 2020))) %>% select(-target, -Ref) %>%  
  pivot_wider(values_from = "Gini_exp", names_from = "policy") %>% 
  mutate(variable = "Income inequality") %>% 
  mutate(`1.5C CP` = (`1.5C CP` - `Baseline`)/`Baseline`, `1.5C non-CP` = (`1.5C non-CP` - `Baseline`)/`Baseline`, change = (`1.5C CP`-`1.5C non-CP`)/`1.5C CP`) %>% 
  dplyr::group_by(Y, R_CGE, SSP, exemption, variable, tech, Gini) %>% 
  reframe(value = median(change)) %>% dplyr::rename(R = "R_CGE") 




### Integration ----

colnames(df1_GDPloss) 
colnames(df1_Gini)
colnames(df1_hug)
colnames(df1_PoV) 
colnames(df1_PoV_55) 
colnames(df1_cnsloss)
colnames(df1_comprice_med)

df_tmp <- df1_GDPloss %>% rbind(df1_hug) %>% rbind(df1_PoV) %>% rbind(df1_PoV_55) %>% rbind(df1_cnsloss) %>% rbind(df1_fdloss) %>% rbind(df1_comprice_med) %>% 
  # filter(SSP == "SSP2") %>% 
  mutate(`1.5C CP` = case_when(abs(`1.5C CP`) >= .0001 ~ `1.5C CP`, abs(`1.5C CP`) < .0001 ~ 0),
         `1.5C non-CP` = case_when(abs(`1.5C non-CP`) >= .0001 ~ `1.5C non-CP`, abs(`1.5C non-CP`) < .0001 ~ 0)) %>% 
  mutate(value = (`1.5C CP` - `1.5C non-CP`)/`1.5C CP`) %>% select(-c("1.5C CP", "1.5C non-CP")) %>% rbind(df1_Gini) %>% filter(value != "NaN") %>% 
  filter(Gini == "consistent", tech == "None")

df <- df_tmp %>% 
  filter(variable %in% c("Food price", "Consumption loss", "Energy price", "Hunger risk", "Poverty (Int.)", "Income inequality"))


colnames(df)
colnames(df1_Gini)


# 1. global by time, all SSPs ----
lis_Y_loop <- c(2030, 2050)
palette_color_SSP <- c('SSP1' ='#94d2bd',
                       'SSP2' ='#5ca1b6',
                       'SSP3' = '#e9c46a')
for(y in 1:length(lis_Y_loop)){
  pdata1 <- df %>% filter(Y == lis_Y_loop[y], R == "World", SSP == "SSP1", Gini == "consistent", tech == "None") %>% select(variable, R, value) %>% 
    pivot_wider(names_from = "R", values_from = "value") %>% dplyr::rename("SSP1" = "World") %>% t()
  pdata2 <- df %>% filter(Y == lis_Y_loop[y], R == "World", SSP == "SSP2", Gini == "consistent", tech == "None") %>% select(variable, R, value) %>% 
    pivot_wider(names_from = "R", values_from = "value") %>% dplyr::rename("SSP2" = "World") %>% t()
  pdata3 <- df %>% filter(Y == lis_Y_loop[y], R == "World", SSP == "SSP3", Gini == "consistent", tech == "None") %>% select(variable, R, value) %>% 
    pivot_wider(names_from = "R", values_from = "value") %>% dplyr::rename("SSP3" = "World") %>% t()
  pdata <- pdata1 %>% rbind(pdata2["SSP2",]) %>% rbind(pdata3["SSP3",])
  pdata <- F_radar_data(pdata, v_min = 0, v_max = 1)
  rownames(pdata) <- c("max", "min", "SSP3", "SSP2", "SSP1")
  
  # to specify the sequence of variables for display
  pdata <- pdata[,c("Consumption loss","Energy price","Income inequality","Poverty (Int.)","Hunger risk","Food price")]
  
  # to rectify the error: "Error in plot.new() : figure margins too large"
  par("mar")
  par(mar = c(.1,.1,.1,.1))
  
  pdf(file = paste0(dir_plot_main, "/2_1_radar/SSPs_",lis_Y_loop[y],".pdf"), width = 5, height = 5)
  F_radarchart(pdata, color = c('#94d2bd', "#5ca1b6", '#e9c46a'), point.size = .6, vlcex = .7, caxislabels = seq(0,100,25), title = lis_Y_loop[y])
  dev.off()
}







pdf(file = paste0(dir_plot_main, "/2_1_radar/SSPs_legend.pdf"), width = 10, height = 7)
F_radarchart(pdata, color = c('#94d2bd', "#5ca1b6", '#e9c46a'),
             vlcex = 0.7, caxislabels = seq(0,100,25), title = lis_Y_loop[y]) 
legend(-2,0,legend=c("SSP1","SSP2", "SSP3"),
       pch=c(15,15,15),
       col=c('#94d2bd', "#5ca1b6", '#e9c46a'),
       lty=c(1,1,1))

dev.off()       




# 2. region by time, all SSPs ----
lis_Y_loop <- c(2030, 2050)
palette_color_SSP <- c('SSP1' ='#94d2bd',
                       'SSP2' ='#5ca1b6',
                       'SSP3' = '#e9c46a')

# r <- 6
v_min <- 0
v_max <- 1
for(r in 1:length(lis_R)){
  for(y in 1:length(lis_Y_loop)){
    pdata1 <- df %>% filter(Y == lis_Y_loop[y], R == lis_R[r], SSP == "SSP1", Gini == "consistent", tech == "None") %>% select(variable, R, value) %>% 
      pivot_wider(names_from = "R", values_from = "value") %>% dplyr::rename("SSP1" = lis_R[r]) %>% t()
    pdata2 <- df %>% filter(Y == lis_Y_loop[y], R == lis_R[r], SSP == "SSP2", Gini == "consistent", tech == "None") %>% select(variable, R, value) %>% 
      pivot_wider(names_from = "R", values_from = "value") %>% dplyr::rename("SSP2" = lis_R[r]) %>% t()
    pdata3 <- df %>% filter(Y == lis_Y_loop[y], R == lis_R[r], SSP == "SSP3", Gini == "consistent", tech == "None") %>% select(variable, R, value) %>% 
      pivot_wider(names_from = "R", values_from = "value") %>% dplyr::rename("SSP3" = lis_R[r]) %>% t()
    pdata <- pdata1 %>% rbind(pdata2["SSP2",]) %>% rbind(pdata3["SSP3",])
    pdata <- F_radar_data(pdata, v_min = v_min, v_max = v_max)
    rownames(pdata) <- c("max", "min", "SSP3", "SSP2", "SSP1")
    

    if(lis_R[r] == "R5OECD90+EU"){
      b <- data.frame(`Hunger risk` = c(v_max, v_min, 0, 0, 0))
      colnames(b) <- "Hunger risk"
      pdata <- cbind(pdata,b) 
    }
    pdata <- pdata[,c("GDP loss","Energy price","Income inequality","Poverty (Int.)","Hunger risk","Food price")]
    
    # to rectify the error: "Error in plot.new() : figure margins too large"
    par("mar")
    par(mar = c(.1,.1,.1,.1))
    
    pdf(file = paste0(dir_plot_main, "/2_1_radar/SSPs_",lis_Y_loop[y],"_",lis_R[r],".pdf"), width = 5, height = 5)
    F_radarchart(pdata, color = c('#94d2bd', "#5ca1b6", '#e9c46a'), vlcex = 0.7, caxislabels = seq(0,v_max*100,v_max*100/4), title = lis_R[r])
    dev.off()
    rm(pdata, pdata1, pdata2, pdata3, pdata0)
  }
}






# 3. plot SSP2 -----------------------------------------------------------------


## 3.1 2030 and 2050 ----
lis_color_R <- c("#5ca1b6", "#F2CC8F", "#E07A5F", "#81B29A", "#A1D6F1", "#5B5F86")
lis_Y_loop <- c(2030, 2050)
v_min <- 0
v_max <- 1
for(y in 1:length(lis_Y_loop)){
  
  # all regions
    pdata <- df %>% filter(Y == lis_Y_loop[y], #R == lis_R[r],
                           SSP == "SSP2", Gini == "consistent", tech == "None") %>% select(variable, R, value) %>% #filter(!grepl( "price", variable)) %>% 
      pivot_wider(names_from = "R", values_from = "value")%>% t()
    pdata <- F_radar_data(pdata, v_min, v_max)
    pdata <- pdata[,c("GDP loss","Energy price","Income inequality","Poverty (Int.)","Hunger risk","Food price")]
    lis_name <- rownames(pdata)[-1:-2]
    
    
    # to rectify the error: "Error in plot.new() : figure margins too large"
    par("mar")
    par(mar = c(1,1,1,1))

    pdf(file = paste0(dir_plot_main, "/2_1_radar/SSP2_",lis_Y_loop[y],".pdf"), width = 5, height = 5)
    F_radarchart(pdata, color = lis_color_R,vlcex = 0.4, caxislabels = seq(0,v_max*100,v_max*100/4), title = lis_Y_loop[y])
    dev.off()
    
    
    # legend
    if(y == 1){    
      pdf(file = paste0(dir_plot_main, "/2_1_radar/Region_legend.pdf"), width = 10, height = 7)
      F_radarchart(pdata, color = lis_color_R,vlcex = 0.4, caxislabels = seq(0,v_max*100,v_max*100/4), title = lis_Y_loop[y])
      legend(-2,0,legend=lis_name,
             pch=c(15,15,15),
             col=lis_color_R,
             lty=c(1,1,1))
      
      dev.off()  
    }
    
  # world
    pdata <- df %>% filter(Y == lis_Y_loop[y], R == "World",
                           SSP == "SSP2", Gini == "consistent", tech == "None") %>% select(variable, R, value) %>% #filter(!grepl( "price", variable)) %>% 
      pivot_wider(names_from = "R", values_from = "value")%>% t()
    lis_name <- rownames(pdata)[-1]
    pdata <- F_radar_data(pdata, v_min, v_max)
    rownames(pdata)[3:nrow(pdata)] <- lis_name
    
    # to rectify the error: "Error in plot.new() : figure margins too large"
    par("mar")
    par(mar = c(1,1,1,1))
    
    pdf(file = paste0(dir_plot_main, "/2_1_radar/SSP2_",lis_Y_loop[y],"_World.pdf"), width = 5, height = 5)
    F_radarchart(pdata, color = "#5ca1b6", vlcex = 0.4, caxislabels = seq(0,v_max*100,v_max*100/4), title = "")
    dev.off()
    
    
    rm(lis_name)
}





## 3.2 global by time ----
lis_Y_loop <- c(2030, 2050)
for(y in 1:length(lis_Y_loop)){
  pdata <- df %>% filter(Y == lis_Y_loop[y], R == "World", SSP == "SSP2", Gini == "consistent", tech == "None") %>% select(variable, R, value) %>% filter(!grepl( "price", variable)) %>% 
    pivot_wider(names_from = "R", values_from = "value")%>% t()
  pdata <- F_radar_data(pdata, v_min = 0, v_max = 1)
  pdata <- pdata[,c("GDP loss","Energy price","Income inequality","Poverty (Int.)","Hunger risk","Food price")]
  
  # to rectify the error: "Error in plot.new() : figure margins too large"
  par("mar")
  par(mar = c(.1,.1,.1,.1))
  
  pdf(file = paste0(dir_plot_main, "/2_1_radar/",lis_Y_loop[y],".pdf"), width = 5, height = 5)
  F_radarchart(pdata, color = "#5ca1b6", vlcex = 0.7, caxislabels = seq(0,100,25), title = lis_Y_loop[y])
  dev.off()
  
  rm(pdata)
}






## 3.3 MAIN plot SSP2 global 2030 and 2050 together -----------------------------------------------------------------
pdata1 <- df %>% filter(Y %in% c(2030), R == "World", SSP == "SSP2", Gini == "consistent", tech == "None") %>% select(variable, Y,value) %>% 
  pivot_wider(names_from = "Y", values_from = "value")%>% t()

pdata2 <- df %>% filter(Y %in% c(2050), R == "World", SSP == "SSP2", Gini == "consistent", tech == "None") %>% select(variable, Y,value) %>% 
  pivot_wider(names_from = "Y", values_from = "value")%>% t()

pdata <- pdata1 %>% rbind(pdata2["2050",]) 
pdata <- F_radar_data(pdata, v_min = 0, v_max = 1)
rownames(pdata) <- c("max", "min", "2050", "2030")
pdata <- pdata[,c("Consumption loss","Energy price","Income inequality","Poverty (Int.)","Hunger risk","Food price")]

palette_color_Y <- c('2030' ="#5ca1b6", 
                       '2050' = '#e9c46a')

# to rectify the error: "Error in plot.new() : figure margins too large"
par("mar")
par(mar = c(.1,.1,.1,.1))

pdf(file = paste0(dir_plot_main, "/2_1_radar/SSP2_global_20302050.pdf"), width = 7, height = 5)
F_radarchart(pdata, color =c(  "#5ca1b6",'#e9c46a'), vlcex = 1, caxislabels = seq(0,100,25))
dev.off()




# legend for radar 

pdf(file = paste0(dir_plot_main, "/2_1_radar/SSP2_global_20302050_legend.pdf"), width = 9, height = 6)
F_radarchart(pdata, color =c(  "#5ca1b6",'#e9c46a'),
             vlcex = 0.7, caxislabels = seq(0,100,25)) 
legend(-2,0,legend=c("2030","2050"),
       pch=c(15,15,15),
       col=c( '#e9c46a', "#5ca1b6"),
       lty=c(1,1,1))

dev.off()       






## 3.3 plot for IAMC presentation SSP2 global 2030 and 2050 together -----------------------------------------------------------------

pdata0 <- df_tmp %>% filter(variable %in% c("Hunger risk", "Poverty (Int.)", "Consumption loss", "Calorie intake","Income inequality"))
pdata1 <- pdata0 %>% filter(Y %in% c(2030), R == "World", SSP == "SSP2", Gini == "consistent", tech == "None") %>% select(variable, Y,value) %>% 
  pivot_wider(names_from = "Y", values_from = "value")%>% t()

pdata2 <- pdata0 %>% filter(Y %in% c(2050), R == "World", SSP == "SSP2", Gini == "consistent", tech == "None") %>% select(variable, Y,value) %>% 
  pivot_wider(names_from = "Y", values_from = "value")%>% t()

pdata <- pdata1 %>% rbind(pdata2["2050",]) 
pdata <- F_radar_data(pdata, v_min = 0, v_max = 1)
rownames(pdata) <- c("max", "min", "2050", "2030")
pdata <- pdata[,c("Consumption loss","Income inequality", "Poverty (Int.)", "Hunger risk","Calorie intake")]

palette_color_Y <- c('2030' ="#5ca1b6", 
                     '2050' = '#e9c46a')

# to rectify the error: "Error in plot.new() : figure margins too large"
par("mar")
par(mar = c(.1,.1,.1,.1))

pdf(file = paste0(dir_plot_main, "/2_1_radar/IAMC_SSP2_global_20302050.pdf"), width = 7, height = 5)
F_radarchart(pdata, color =c(  "#5ca1b6",'#e9c46a'), vlcex = 1, caxislabels = seq(0,100,25))
dev.off()





rm(lis_Y_loop,pdata,df,df1_Gini,df1_hug,df1_PoV,df1_comprice_med,df1_cnsloss,df1_GDPloss,df_hug,df_PoV,df_comprice_med,df_cnsloss,df_GDPloss, palette_color_Y)


print("The END of 2_1_main_radarplot.R")

