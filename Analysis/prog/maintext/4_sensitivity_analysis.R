# This Rscript gives the sensitivity analysis of the poverty headcount to the different scenarios assumptions.
# Shiya ZHAO, 2024.06.27


# 0. loading packages ----
require(gdxrrw)
require(ggplot2)
require(tidyverse)



# 0. loading data ----


## 0.1 population | unit = million ----
df_pop <- rgdx.param("../../DataArchive/PHIoutput/gdx/Inputdata.gdx", "Population") %>% 
  filter(Ref %in% c("SSP2", "SSP1", "SSP3")) %>% mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R), SSP = Ref) %>% select(-Ref) %>% 
  mutate(Population = Population/1000000, unit = "Million people")


## 0.2 iamc template ----
df_iamc <- cge_var %>%
  rgdx.param("IAMC_Template") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO) %>% 
  gdata::rename.vars(colnames(.), c("R", "VEMF", "Y", "value", "Ref"))


## 0.3 GDP loss ----
GDP <- cge_ana %>%
  rgdx.param("GDP") %>%
  left_join(MapScenario) %>% 
  F_filter(lis_R= lis_R) %>% F_woc() %>% 
  # transform(policy = factor(policy, levels = lis_cp)) %>%
  select(-c("SCENARIO")) 


GDPloss <- GDP %>% filter(exemption == "None") %>% 
  select(-Ref, -target) %>% spread(policy, GDP) %>%
  pivot_longer(cols = colnames(.)[startsWith(colnames(.), "1.5C") | startsWith(colnames(.), "2C") ], names_to = "policy", values_to = "value") %>% 
  mutate(GDPloss = (Baseline-`value`)/value*100, unit = "%") %>% select(-"Baseline non-CP", -"value", -"Baseline") %>% mutate(TH1 = "None") %>% dplyr::rename(value = GDPloss)
# $ GDP | Million US dollar(2005) per year
# GDP: Million US dollar(2005) per year; GDPloss: % per year

rm(GDP)

## 0.31 Consumption loss ----
cnsloss <-  df_iamc %>% filter(VEMF == "Pol_Cos_Cns_Los_rat") %>% F_woc() %>%  
  filter(policy != "Baseline non-CP") %>% F_filter(lis_R= lis_R) %>% select(-VEMF, -Ref, -target) %>% mutate(value = value, unit = "%", TH1 = "None")

# Consumption loss: % per year




## 0.4 Poverty headcount ----

PoV <- rgdx.param(AnaExp, "PoVExp") %>%
  # left_join(MapScenario) %>% 
  mutate(R = case_when(R == "WLD" ~ "World", R != "WLD" ~ R)) %>% 
  filter(!is.na(Ref), R %in% lis_R,
         # Ref %in% lis_ref,
         Y%in% lis_Y) %>%
  rename.vars(from = 'PoVExp', to = 'PoV') %>%
  mutate(TH1 = recode(TH, 
                      'pop_1.9' = lis_TH[1],
                      'pop_3.2' = lis_TH[2],
                      'pop_5.5' = lis_TH[3]),
         R = factor(R, levels = lis_R),
         unit = "person") %>%
  select(-"TH") %>% filter(TH1 %in% lis_TH) %>% 
  # filter(scenario %in% c(lis_scenario_noex,lis_scenario_exmp,'Baseline'))%>%
  rename.vars(from = 'PoV', to = 'value') %>% F_filter(s_Baseline_woc = F, lis_R = lis_R) %>% F_woc()

Headcount <- PoV %>% select(-Ref)
Headcount_change <- Headcount %>% F_cha_decom() %>% 
  mutate(value = value/1000000, unit = "Million people") %>% select(-"Baseline_None", -"Baseline non-CP_None") 

Headcount %>% filter(R=="World", Y == "2030", TH1 == "1.9-threshold", SSP == "SSP2", exemption == "None", Gini == "consistent") %>% mutate(unit = "Million people", value = value/1000000) %>% openxlsx::write.xlsx(file = paste0(dir_csv, "/4_sensitivity analysis_tech_poverty.xlsx"))


rm(PoVExp_tmp, PoVInc_tmp, PoV)


## 0.5 Gini coefficient ----

df_Gini <- rgdx.param(AnaExp, "Gini_exp") %>%
  F_filter(lis_R=unique(.$R)) %>% F_woc() %>% mutate(unit = "100", value = Gini_exp * 100, TH1 = "None") %>% select(-Ref, -Gini_exp, -target)





## 0.6 Hunger risk ----
# region level population at risk of hunger output | unit = million 
# df_hug <- df_iamc %>% filter(VEMF == "Pop_Ris_of_Hun") %>% mutate(unit = "Million people", TH1 = "None") %>% left_join(MapScenario) %>% F_woc() %>% F_filter(lis_R = lis_R) %>% select(-SCENARIO, -VEMF, -Ref, -target)


# F_woc_hunger <- function(a){
#   b <- a %>% mutate(policy = case_when(grepl("woc", Ref) & grepl("1p5", Ref) ~ "1.5C non-CP", !grepl("woc", Ref) & grepl("1p5", Ref) ~ "1.5C CP", 
#                                        grepl("woc", Ref) & grepl("2C", Ref) ~ "2C non-CP", !grepl("woc", Ref) & grepl("2C", Ref) ~ "2C CP", 
#                                        grepl("woc", Ref) & grepl("Base", Ref) ~ "Baseline non-CP", 
#                                        !grepl("woc", Ref) & grepl("Base", Ref) ~ "Baseline"),
#                     SSP =  case_when(grepl("SSP1", Ref) ~ "SSP1", grepl("SSP3", Ref) ~ "SSP3", !grepl("SSP", Ref) ~ "SSP2", grepl("SSP2", Ref) ~ "SSP2"),
#                     exemption = case_when(grepl("expt", Ref) ~ "Direct tax exemption", !grepl("expt", Ref) ~ "None"),
#                     target = case_when(grepl("1p5", Ref) ~ "1.5C", grepl("2C", Ref) ~ "2C", grepl("Base", Ref) ~ "Baseline"),
#                     tech = case_when(grepl("LowAgTech", Ref) ~ "LowAgTech", grepl("HighAgTech", Ref) ~ "HighAgTech", !grepl("AgTech", Ref) ~ "None"),
#                     Gini = case_when(grepl("_Gini1", Ref) ~ "SSP1", grepl("_Gini3", Ref) ~ "SSP3", grepl("_Gini2", Ref) ~ "SSP2", !grepl("_Gini1", Ref) & !grepl("_Gini3", Ref) & !grepl("_Gini2", Ref) ~ "consistent"))
#   return(b)
#   
# }
# 
# colnames(df_hug)

df_hug <- rgdx.param(paste0("../../DataArchive/hungertool/240627/AllModel_Country_AIMHub.gdx"), "Risk_org") %>% 
  gdata::rename.vars(colnames(.), c("Model", "U", "SCENARIO", "R_CGE", "Y", "value")) %>% mutate(unit = "Million people", TH1 = "None") %>% filter(U == "fao_based", Model == "AIMHub", Y %in% c(seq(2020, 2100))) %>% select(-U) %>% 
  left_join(map_R17to5 %>% select(R_CGE, R5) %>% distinct() %>% filter(R5 != "World") %>% rbind(R_CGE = "World", R5 = "World")) %>% dplyr::group_by(SCENARIO, Y, unit, TH1, R5) %>% dplyr::reframe(value = sum(value)) %>% dplyr::rename(R=R5)%>% 
  mutate(Gini = case_when(grepl("_Gini1", SCENARIO) ~ "SSP1",
                            grepl("_Gini3", SCENARIO) ~ "SSP3",
                            TRUE ~ "consistent"),
         SCENARIO = gsub("_Gini[1-5]", "", SCENARIO)) %>% 
  left_join(MapScenario %>% F_woc() %>% select(-Ref) %>% filter(exemption == "None")) %>% select(-SCENARIO, -target)
  



## country level population at risk of hunger output | unit = million 
# df_hunger_r <- paste0("../../DataArchive/hungertool/240627/AllModel_Country_AIMHub.gdx") %>%
#   rgdx.param("Risk") %>% gdata::rename.vars(colnames(.), c("model", "U", "SCENARIO", "R_code","Y", "value")) %>% 
#   left_join(MapScenario) %>% left_join(map_Rfao2iso3) %>% select(-R_code) %>% 
#   F_woc() %>% left_join(map_R17) %>% left_join(map_R17to5) %>% select(-SCENARIO, -R_full, -R_CGE) %>% mutate(unit = "Million people") %>%
#   left_join(df_pop) %>% filter(!is.na(Population))


## 0.7 create a large dataframe ----
# check the colnames of all the dataframes
# print(c("df_pop", colnames(df_pop)))
# print(c("df_iamc", colnames(df_iamc)))
print(c("GDPloss", colnames(GDPloss)))
print(c("cnsloss", colnames(cnsloss)))
print(c("Headcount_change", colnames(Headcount_change)))
print(c("df_Gini", colnames(df_Gini)))
print(c("df_hug", colnames(df_hug)))
# print(c("df_hunger_r", colnames(df_hunger_r)))

# check the number of columns of all the dataframes
ncol(GDPloss)
ncol(cnsloss)
ncol(Headcount_change)
ncol(df_Gini)
ncol(df_hug)

# check the unique values of R in all the dataframes
unique(GDPloss$R)
unique(cnsloss$R)
unique(Headcount_change$R)
unique(df_Gini$R)
unique(df_hug$R)



# combine all the dataframes
lis_Y1 <- c(2030, 2050)
df0 <- GDPloss %>% mutate(variable = "GDP loss", R_type = "R5") %>%
  rbind(cnsloss %>% mutate(variable = "Consumption loss", R_type = "R5") ) %>%
  rbind(Headcount_change %>% mutate(variable = "Poverty", R_type = "R5")) %>%
  rbind(df_Gini %>% mutate(variable = "Gini", R_type = "R184")) %>%
  rbind(df_hug %>% mutate(variable = "Hunger", R_type = "R5")) 

colnames(cnsloss)
unique(df0$variable)
lis_var <- unique(df0$variable)
lis_var <- c("Consumption loss", "Poverty", "Hunger")


#======================================  TESTs ======================================
# 001. absolute values in all scenarios ----
# global
# sensitivity analysis regarding the SSP variation (socio-economic assumption)
sw_sensitivity <- "SSP" 
# sensitivity analysis regarding the technology assumptions (tech: None, High agricultural productivities, Low agricultural productivities)
sw_sensitivity <- "tech"

lis_sw <- c("SSP", "tech", "Gini")
for(n1 in 1:length(lis_sw)){
  sw_sensitivity <- lis_sw[n1]
  # switch the scenario selection for the sensitivity analysis
    if(sw_sensitivity == "SSP"){
        F_filter_sen1 <- function(x){
            y <- x %>% filter(tech == "None", Gini == "consistent") %>% mutate(v_color = SSP)
            return(y)
        }
        palette1 <- palette_color_SSP
        
        txt_legend <- "SSP"
        
        palette_alpha1 <- c("SSP1" = .3,
                            "SSP2" = 1,
                            "SSP3" = .3
                            )
    }else if(sw_sensitivity == "tech"){
      F_filter_sen1 <- function(x){
        y <- x %>% filter(SSP == "SSP2", Gini == "consistent") %>% mutate(v_color = factor(tech, levels = c("HighAgTech","None", "LowAgTech"))) 
        return(y)
      }
      palette1 <- c("None" = "#5ca1b6",
                    "HighAgTech" = "#94d2bd",
                    "LowAgTech" = "#e9c46a")
      
      txt_legend <- "Technology"
      
      palette_alpha1 <- c("None" = 1,
                          "HighAgTech" = .3,
                          "LowAgTech" = .3)
    }else if(sw_sensitivity == "Gini"){
      F_filter_sen1 <- function(x){
        y <- x %>% filter(SSP == "SSP2", tech == "None") %>% mutate(Gini = case_when(Gini == "consistent" ~ "SSP2", Gini != "consistent" ~ Gini)) %>% mutate(v_color = Gini)
        return(y)
      }
        palette1 <- palette_color_SSP
        
        
        txt_legend <- "Gini assumptions"
        
        palette_alpha1 <- c("SSP1" = .3,
                            "SSP2" = 1,
                            "SSP3" = .3)
    }
  
  # choose scenarios for the sensitivity analysis
  df1 <- df0 %>% F_filter_sen1() %>% filter(Y %in% lis_Y1, variable != "Gini", R == "World", exemption == "None") 
  
  pdata0 <- df1
  
  for(n in 1:length(lis_var)){
      pdata <- pdata0 %>% filter(variable == lis_var[n], R == "World", TH1 == "None" | TH1 == "1.9-threshold")
    
    p <- ggplot(pdata) +
      geom_col(aes(x = policy, y = value, fill = v_color, alpha = v_color), position = "dodge", width = .7) +
      geom_hline(yintercept = 0, color = "grey") +
      MyTheme + facet_grid(~Y) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1)) +
      labs(x = "Year", y = pdata$unit[1]) + # "Change by removing the carbon price"
      scale_fill_manual(values = palette1)+
      scale_alpha_manual(values = palette_alpha1) +
      guides(fill = guide_legend(title = txt_legend), alpha = guide_legend(title = txt_legend))
    p
    
    ggsave(p, filename = paste0("4_global_allscenario_",sw_sensitivity,"_",lis_var[n],".pdf"), path = dir_plot_main, units = "cm", width = 20, height = 6)
    rm(p, pdata)
    
    if(lis_var[n] == "Headcount_change"){
      pdata <- pdata0 %>% filter(variable == lis_var[n], R == "World")
      palette_alpha2 <- c("1.9-threshold" = 1,
                          "3.2-threshold" = .5,
                          "5.5-threshold" = .2)
      p <- ggplot() +
        geom_col(pdata, mapping = aes(x = policy, y = value, alpha = TH1, fill = v_color), position = "dodge", width = .7) +
        # geom_point(pdata %>% filter(TH1 != "1.9-threshold"), mapping = aes(x = Y, y = value, shape = TH1)) +
        geom_hline(yintercept = 0, color = "grey") +
          MyTheme + facet_grid(~Y) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1)) +
        labs(x = "Year", y = pdata$unit[1]) + # "Change by removing the carbon price"
        scale_fill_manual(values = palette1)+
        scale_alpha_manual(values = palette_alpha2) +
        guides(fill = guide_legend(title = txt_legend), alpha = guide_legend(title = "Threshold"))
      p
      
      ggsave(p, filename = paste0("4_global_allscenario_",sw_sensitivity,"_",lis_var[n],"_th.pdf"), path = dir_plot_main, units = "cm", width = 20, height = 6)
    }
  }

}














# 002. sensitivity analysis ----
# we don't need baseline because we are taking the difference between the carbon pricing and non carbo pricing scenarios
df <- df0 %>% filter(exemption == "None", !grepl("Baseline", policy)) %>% 
  filter(Y %in% lis_Y1) %>% mutate(target = case_when(grepl("1.5C", policy) ~ "1.5C",
                                       grepl("2C", policy) ~ "2C"),
                    CP = case_when(grepl("non-CP", policy) ~ "non-CP",
                                            !grepl("non-CP", policy) ~ "CP")) %>% select(-policy) %>% 
  pivot_wider(names_from = CP, values_from = value) %>% mutate(value = `non-CP` - `CP`) 




# switch for the sensitivity analysis
rm(sw_sensitivity)
# sensitivity analysis regarding the SSP variation (socio-economic assumption)
sw_sensitivity <- "SSP" 
# sensitivity analysis regarding the technology assumptions (tech: None, High agricultural productivities, Low agricultural productivities)
sw_sensitivity <- "tech"
# sensitivity analysis regarding the assumptions on the distribution (Gini and CV, only for SSP2)
sw_sensitivity <- "Gini"
# sensitivity analysis regarding the policy stringency (The temperature target)
# sw_sensitivity <- "target"
# sensitivity analysis regarding the poverty threshold (Only for poverty headcount, TH1: 1.9, 3.2, 5.5)
# sw_sensitivity <- "TH"

if(sw_sensitivity == "SSP"){
    F_filter_sen2 <- function(x){
        y <- x %>% filter(tech == "None", Gini == "consistent") %>% mutate(v_color = SSP)
        return(y)
    }
    palette1 <- palette_color_SSP
    
    txt_legend <- "SSP"
    
    palette_alpha1 <- c("SSP1" = .3,
                        "SSP2" = 1,
                        "SSP3" = .3
                        )
}else if(sw_sensitivity == "tech"){
  F_filter_sen2 <- function(x){
    y <- x %>% filter(SSP == "SSP2", Gini == "consistent") %>% mutate(v_color = factor(tech, levels = c("HighAgTech","None", "LowAgTech"))) 
    return(y)
  }
  palette1 <- c("None" = "#5ca1b6",
                "HighAgTech" = "#94d2bd",
                "LowAgTech" = "#e9c46a")
  
  txt_legend <- "Technology"
  
  palette_alpha1 <- c("None" = 1,
                      "HighAgTech" = .3,
                      "LowAgTech" = .3)
}else if(sw_sensitivity == "Gini"){
  F_filter_sen2 <- function(x){
    y <- x %>% filter(SSP == "SSP2", tech == "None") %>% mutate(v_color = factor(Gini, levels = c("SSP1","consistent", "SSP3"))) 
    return(y)
  }
  palette1 <- c("consistent" = "#5ca1b6",
                "SSP1" = "#94d2bd",
                "SSP3" = "#e9c46a")
  
  txt_legend <- "Within-country distribution"
  
  palette_alpha1 <- c("consistent" = 1,
                      "SSP1" = .3,
                      "SSP3" = .3)
}


# choose scenarios for the sensitivity analysis
df1 <- df %>% F_filter_sen2()


# make a col plot and see the results

pdata0 <- df1 

# by regions
for(n in 1:length(lis_var)){
  if(lis_var[n] == "Headcount_change"){
    pdata <- pdata0 %>% filter(variable == lis_var[n], TH1 == "1.9-threshold")
  }else{
    pdata <- pdata0 %>% filter(variable == lis_var[n])
  }
  
  p <- ggplot(pdata) +
    geom_col(aes(x = R, y = value, fill = v_color, alpha = v_color), position = "dodge", width = .7) +
    geom_hline(yintercept = 0, color = "grey") +
    MyTheme + facet_grid(target~Y) + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
    labs(x = "Region", y = pdata$unit[1]) + # "Change by removing the carbon price"
    scale_fill_manual(values = palette1) +
    scale_alpha_manual(values = palette_alpha1) +
    guides(fill = guide_legend(title = txt_legend), alpha = guide_legend(title = txt_legend))
  p
  
  ggsave(p, filename = paste0("4_region_",sw_sensitivity,"_",lis_var[n],".pdf"), path = dir_plot_main, units = "cm", width = 15, height = 8)
}


# global total
for(n in 1:length(lis_var)){
  if(lis_var[n] == "Headcount_change"){
    pdata <- pdata0 %>% filter(variable == lis_var[n], R == "World", TH1 == "1.9-threshold")
  }else{
    pdata <- pdata0 %>% filter(variable == lis_var[n], R == "World")
  }
  
  p <- ggplot(pdata) +
    geom_col(aes(x = Y, y = value, fill = v_color, alpha = v_color), position = "dodge", width = .7) +
    geom_hline(yintercept = 0, color = "grey") +
    MyTheme + facet_grid(~target) + theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) +
    labs(x = "Year", y = pdata$unit[1]) + # "Change by removing the carbon price"
    scale_fill_manual(values = palette1)+
    scale_alpha_manual(values = palette_alpha1) +
    guides(fill = guide_legend(title = txt_legend), alpha = guide_legend(title = txt_legend))
  p
  
  ggsave(p, filename = paste0("4_global_",sw_sensitivity,"_",lis_var[n],".pdf"), path = dir_plot_main, units = "cm", width = 15, height = 8)
  rm(p, pdata)
  
  if(lis_var[n] == "Headcount_change"){
    pdata <- pdata0 %>% filter(variable == lis_var[n], R == "World")
    palette_alpha2 <- c("1.9-threshold" = 1,
                        "3.2-threshold" = .5,
                        "5.5-threshold" = .3)
    p <- ggplot() +
      geom_col(pdata, mapping = aes(x = TH1, y = value, fill = v_color, alpha = TH1), position = "dodge", width = .7) +
      # geom_point(pdata %>% filter(TH1 != "1.9-threshold"), mapping = aes(x = Y, y = value, shape = TH1)) +
      geom_hline(yintercept = 0, color = "grey") +
      MyTheme + facet_grid(Y~target) + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
      labs(x = "Year", y = pdata$unit[1]) + # "Change by removing the carbon price"
      scale_fill_manual(values = palette1)+
      scale_alpha_manual(values = palette_alpha2) +
      guides(fill = guide_legend(title = txt_legend), alpha = guide_legend(title = "Threshold"))
    p
    
    ggsave(p, filename = paste0("4_global_",sw_sensitivity,"_",lis_var[n],"_th.pdf"), path = dir_plot_main, units = "cm", width = 15, height = 8)
  }
}

#====================================== END of TESTs ======================================


#======================================  visualization for maintext ======================================
# we don't need baseline because we are taking the difference between the carbon pricing and non carbo pricing scenarios
df <- df0 %>% filter(exemption == "None", !grepl("Baseline", policy), Y %in% lis_Y1, TH1 %in% c("None", "1.9-threshold")) %>% 
  mutate(target = case_when(grepl("1.5C", policy) ~ "1.5C",
                                                      grepl("2C", policy) ~ "2C"),
                                   CP = case_when(grepl("non-CP", policy) ~ "non-CP",
                                                  !grepl("non-CP", policy) ~ "CP")) %>% select(-policy, -exemption) %>% 
  pivot_wider(names_from = CP, values_from = value) %>% mutate(value = `CP` - `non-CP`) %>% 
  filter((R == "World" & R_type == "R5") | (R_type == "R184"))
# benefits of non-CP

# main scenario: 1.5C
df_sa_main <- df %>% filter(target == "1.5C", Gini == "consistent", SSP == "SSP2", tech == "None")

# uncertainties concerning the target
df_sa_target <- df %>% filter(target == "2C", Gini == "consistent", SSP == "SSP2", tech == "None") %>% rbind(df_sa_main) %>% mutate(sa = "Target") # Temperature target

# uncertainties concerning the inner country inequality
df_sa_gini <- df %>% filter(target == "1.5C", Gini %in% c("SSP1", "SSP3"), SSP == "SSP2", tech == "None") %>% rbind(df_sa_main) %>% mutate(sa = "Inequality") # Within-country inequality

# uncertainties concerning the economic growth and demographic features
df_sa_ssp <- df %>% filter(target == "1.5C", Gini == "consistent", SSP %in% c("SSP1", "SSP3"), tech == "None") %>% rbind(df_sa_main) %>% mutate(sa = "GDP&Pop") # GDP & Population

# uncertainties concerning the agriculture technology assumptions
df_sa_tech <- df %>% filter(target == "1.5C", Gini == "consistent", SSP == "SSP2", tech %in% c("HighAgTech", "LowAgTech")) %>% rbind(df_sa_main) %>% mutate(sa = "AgProd") # Agricultural productivity



## plot: except for Gini --------------------------------------------------------------------
data0 <- df_sa_main %>% filter(R == "World")
data <- df_sa_target %>% rbind(df_sa_gini) %>% rbind(df_sa_ssp) %>% rbind(df_sa_tech)%>% filter(R == "World")
data1 <- data %>% dplyr::group_by(Y,R,variable) %>% dplyr::reframe(max = max(value), min = min(value)) %>% full_join(data %>% select(Y,R,variable, sa) %>% distinct())
data2 <- data %>% dplyr::group_by(Y,R,variable,sa) %>% dplyr::reframe(max = max(value), min = min(value)) 

palette_sa <- c("Target" = "#adc178",
                "Inequality" = "#fcca46",
                "GDP&Pop" = "#afafdc",
                "AgProd" = "#ed6a5a")
# i <- 1
dir.create(paste0(dir_plot_main, "/4_sensitivityanalysis/"))
for(i in 1:length(unique(data0$variable))){
   pdata0 <- data0 %>% filter(variable == unique(data0$variable)[i])
   pdata <- data %>% filter(variable == unique(data0$variable)[i])
   pdata1 <- data1 %>% filter(variable == unique(data0$variable)[i])
   pdata2 <- data2 %>% filter(variable == unique(data0$variable)[i])
  
  p <- ggplot() +
    geom_col(pdata2, mapping = aes(x = sa, y = max, fill = sa), color = "white", width = .5) +
    geom_col(pdata2, mapping = aes(x = sa, y = min), fill = "white", width = .5) +
    geom_ribbon(data = pdata1 , mapping = aes(x = sa, xmin = -Inf, xmax = Inf, ymin = min, ymax = max, group = paste0(Y,R,variable)), fill = "#ace0f9", alpha = .2) +
    geom_hline(data = pdata0, mapping = aes(yintercept = value), color = "grey60", linewidth = 1) +
    geom_hline(yintercept = 0, color = "grey80", linetype = "dashed") +
    geom_point(data = pdata, mapping = aes(x = sa, y = value, shape = sa), color="grey30")+
    labs(x = "Assumptions", y = pdata0$unit[1], subtitle =  unique(data0$variable)[i]) +
    scale_fill_manual(values = palette_sa) + 
    facet_grid(~Y) + MyTheme + theme(axis.text.x = element_text(angle = 30, hjust = .5, vjust = .5), legend.position = 'none') #+
    # geom_rect(aes(xmin=-Inf, xmax=Inf, ymin = unique(pdata1$min), ymax=unique(pdata1$max)), fill = "white", alpha = 0.5)
  p
  
  ggsave(p, filename = paste0("/4_sensitivityanalysis/",unique(data0$variable)[i],".pdf"), path = dir_plot_main, width = 12, height = 6, units = "cm")
rm(pdata,pdata0,pdata1,pdata2,p)
  
}

rm(data,data0,data1,data2)

## plot: Gini --------------------------------------------------------------


# create dataframe for median level in the main scenario (pdata0)
pdata0_2030 <- df_sa_main %>% filter(variable == "Gini", Y == "2030") 
pdata0_2050 <- df_sa_main %>% filter(variable == "Gini", Y == "2050") 

median2030 <- boxplot.stats(pdata0_2030$value)$stats[3]
median2050 <- boxplot.stats(pdata0_2050$value)$stats[3]

pdata0 <- data.frame(Y = c("2030", "2050"), value=c(median2030, median2050))


# dataframe for sensitivity analysis
pdata <- df_sa_target %>% rbind(df_sa_gini) %>% rbind(df_sa_ssp) %>% rbind(df_sa_tech) %>% filter(variable == "Gini")

# create dataframe (pdata1) for shades
pdata1_2030 <- pdata %>% filter(variable == "Gini", Y == "2030") %>% dplyr::group_by(Y, sa) %>% 
  reframe(v_min = boxplot.stats(value)$stats[2], v_max = boxplot.stats(value)$stats[4])

pdata1_2050 <- pdata %>% filter(variable == "Gini", Y == "2050") %>% dplyr::group_by(Y, sa) %>% 
  reframe(v_min = boxplot.stats(value)$stats[2], v_max = boxplot.stats(value)$stats[4])

pdata1 <- pdata1_2030 %>% mutate(max = max(v_max), min = min(v_min)) %>% rbind(pdata1_2050 %>% mutate(max = max(v_max), min = min(v_min)))

rm(pdata1_2030, pdata1_2050)


p <- ggplot(linewidth = .002) +
  geom_boxplot(data = pdata, mapping = aes(x = sa, y = value, fill = sa), width = .3, outlier.color = "grey70", outlier.size = .5, outlier.alpha = .3,linewidth = .3)+
  gghalves::geom_half_violin(data = pdata, mapping = aes(x = sa, y = value, fill = sa), position = position_nudge(x = -.2, y = 0), width = .5, side = "l", linewidth = .3) + 
  geom_hline(yintercept = 0, color = "grey80", linetype = "dashed") +
  geom_hline(data = pdata0, mapping = aes(yintercept = value), color = "grey60", linewidth = 1, alpha = .6) + 
  geom_ribbon(data = pdata1, mapping = aes(x = sa, xmin = -Inf, xmax = Inf, ymin = min, ymax = max, group = paste0(Y)), fill = "#ace0f9", alpha = .2) +
  labs(x = "Assumptions", y = "Percentage point", subtitle =  "Gini") +
  scale_fill_manual(values = palette_sa) + 
  facet_grid(~Y) + MyTheme + theme(axis.text.x = element_text(angle = 30, hjust = .5, vjust = .5), legend.position = 'none') #+
# geom_rect(aes(xmin=-Inf, xmax=Inf, ymin = unique(pdata1$min), ymax=unique(pdata1$max)), fill = "white", alpha = 0.5)
p

ggsave(p, filename = paste0("/4_sensitivityanalysis/Gini.pdf"), path = dir_plot_main, width = 12, height = 6, units = "cm")
 



