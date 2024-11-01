# This Rscript compares the non-CP/CP paper scenarios with the IPCC scenario database to see where our scenarios situate. 
# Shiya ZHAO, 2024.04.05

# functions ---------------------------------------------------
F_not_all_na <- function(x) any(!is.na(x))



F_fil_ar6 <- function(x, var){
  y <- x %>% filter(Variable %in% var) %>% select(where(F_not_all_na)) %>% 
    pivot_longer(cols = colnames(.)[which(startsWith(colnames(.), "X"))], 
                 values_to = "value", names_to = "Y") %>% 
    left_join(map_ar6_sc) %>% filter(!is.na(Category_Vetting_historical)) %>% mutate(Y = gsub("X", "", Y) %>% as.character()) %>% select(-"Category_name")
  return(y)
}


F_plot_pos <- function(pdata1, pdata2, lab_y = "Value", lab_x = "Year"){
  max_val <- max(pdata1$value)
  min_val <- min(pdata1$value)
  
  pdata01 <- pdata1 %>% mutate(group = paste0(Model, Scenario, Region)) %>%
    select("group", "Y", "value") %>%
    pivot_wider(names_from = "group", values_from = "value") 
  
  
  for(i in 1:nrow(pdata01)){
    for(j in 1:ncol(pdata01)){
      if(is.na(pdata01[i,j])){
        pdata01[i,j] <- (pdata01[i-1,j]+pdata01[i+1,j])/2
      }else{
        pdata01[i,j] <- pdata01[i,j]
      }
    }
  }
  
  pdata0 <- pdata01 %>%
    mutate(v_min = apply(.[colnames(.)[!startsWith(colnames(.), "Y")]],1,min, na.rm=TRUE),
           v_max = apply(.[colnames(.)[!startsWith(colnames(.), "Y")]],1,max, na.rm=TRUE)) %>%
    select(Y, v_min, v_max) %>%  rbind(pdata2 %>% filter(Y == 2010, policy == "1.5C CP", SSP == "SSP2", exemption == "None") %>% mutate(v_max = value, v_min = value) %>% select(Y, v_max, v_min) )
  
  pdata2 <- pdata2 %>% filter(policy %in% c("1.5C CP", "1.5C non-CP"), SSP == "SSP2")
  
  p <- ggplot() +
    geom_ribbon(data= pdata0, mapping = aes(x = Y %>% as.character() %>% as.numeric(), ymin=v_min,ymax=v_max), fill = "grey", alpha=.3) +
    geom_line(pdata1, mapping = aes(x = Y %>% as.character() %>% as.numeric(), y = value, group = paste0(Model, Scenario, Region)), color = "grey", alpha = .5, linewidth = .5) +
    geom_line(pdata2, mapping = aes(x = Y %>% as.character() %>% as.numeric(), y = value, group = Ref, linetype = Ref),  color = "#5ca1b6", linewidth = 1.2) + # color = policy
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    labs(x = lab_x, y = lab_y) +
    MyTheme +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = .8)) + Theme_tran #+
    # scale_color_manual(values = palette_color_cp) +
    # scale_linetype_manual(values = palette_line_SSP) +
    # scale_y_continuous(breaks = seq(ceiling(min_val), ceiling(max_val), by = (ceiling(max_val)-ceiling(min_val))%/%5 ), limits=c(min_val, max_val))
  p
  return(p)
}



F_plot_pos_dcom <- function(pdata1, pdata2, lab_y = "Value", lab_x = "Year"){
  
  p <- ggplot() +
    # geom_ribbon(data= pdata0, mapping = aes(x = Y %>% as.character() %>% as.numeric(), ymin=v_min,ymax=v_max), fill = "grey", alpha=.4) +
    geom_line(pdata1, mapping = aes(x = Y %>% as.character() %>% as.numeric(), y = value, group = paste0(Model, Scenario, Region)), color = "grey", alpha = .7) +
    geom_line(pdata2, mapping = aes(x = Y %>% as.character() %>% as.numeric(), y = value, color = policy, group = Ref, linetype = SSP), alpha = .8, linewidth = .8) +
    labs(x = lab_x, y = lab_y) +
    MyTheme +
    theme(axis.text.x = element_text(angle = 60)) + Theme_tran +
    scale_color_manual(values = palette_color_cp) +
    scale_linetype_manual(values = palette_line_SSP)
  p
  return(p)
}



# 0. variable selection and loading data ---------------------------------------------------
lis_ar6_Y <- seq(2010, 2100, 5)

lis_ar6_var <- openxlsx::read.xlsx(xlsxFile = paste0("../",prog_loc,"/data/AR6_variables.xlsx"), sheet = "Variable")
lis_ar6_var <- unique(lis_ar6_var$Variable)

map_ar6_sc <- openxlsx::read.xlsx(xlsxFile = paste0("../",prog_loc,"/data/AR6_variables.xlsx"), sheet = "Scenario") %>% 
  filter(Category_Vetting_historical == "C1")




# load data
df_ar6_tmp <- read.csv(file = paste0("../",prog_loc,"/data/AR6_Scenarios_Database_World_v11_IIASA/AR6_Scenarios_Database_World_v11_IIASA.csv"))
lis_ar6_var_od <- unique(df_ar6_tmp$Variable) %>% as.data.frame()
openxlsx::write.xlsx(lis_ar6_var_od, file = paste0("../",prog_loc,"/data/AR6_variables_od.xlsx"))
df_ar6 <- df_ar6_tmp %>% select(-c(`X1995`:`X2019`)) %>% filter(Variable %in% lis_ar6_var)
rm(df_ar6_tmp, lis_ar6_var_od)
unique(df_ar6$Variable)
dir.create(paste0(dir_fig,"/Figure8_position"))



df_iamc <- cge_var %>%
  rgdx.param("IAMC_Template") %>% left_join(MapScenario) %>% filter(!is.na(Ref), !grepl("Base_woc", Ref)) %>% select(-SCENARIO) %>% 
  gdata::rename.vars(colnames(.), c("R", "VEMF", "Y", "value", "Ref")) %>% F_woc()


# 1. GDP ------------------------------------------------------------------
# unique(df_ar6_GDP$Scenario)
df_ar6_GDP_PPP <- df_ar6 %>% F_fil_ar6("GDP|PPP") %>% filter(Y %in% lis_ar6_Y)

df_ar6_GDP_MER <- df_ar6 %>% F_fil_ar6("GDP|MER") %>% filter(Y %in% lis_ar6_Y)






# 2. Policy Cost ----------------------------------------------------
## 2.1 GDP Loss ----------------------------------------------------
df_ar6_GDPloss <- df_ar6 %>% F_fil_ar6("Policy Cost|GDP Loss") %>% 
  mutate(value = abs(value))
# confirmed that negative GDP loss reported by REMIND 2.1 and REMIND-MAgPIE 2.1-4.2 are due to reporting errors (the sign was wrong)

df_ar6_GDPloss %>% filter(value<0) %>% openxlsx::write.xlsx(paste0(dir_csv, "/8_AR6_GDPgain.xlsx"))
GDPloss <- df_iamc %>% filter(VEMF == "Pol_Cos_GDP_Los_rat", R == "World") %>% F_woc() %>% dplyr::rename(GDPloss = "value")



# plot
pdata1 <- df_ar6_GDPloss %>% dplyr::rename(value_GDPloss = "value") %>% select(-Variable) %>% 
  left_join(df_ar6_GDP_MER %>% dplyr::rename(value_GDP = "value") %>% select(-Variable)) %>% 
  filter(!is.na(value_GDPloss), !is.na(value_GDP)) %>% 
  mutate(value = case_when(value_GDPloss >= 0 ~ value_GDPloss/(value_GDP + value_GDPloss)*100,
                           value_GDPloss < 0 ~ -value_GDPloss/(value_GDP - value_GDPloss)*100), 
         Y = as.character(Y)) #%>% filter(Y > 2060)

pdata2 <- GDPloss %>% mutate(value = GDPloss, Ref = paste0(SSP, policy, exemption),Y = as.character(Y)) %>% 
  F_filter_main() %>% filter(target != "2C", exemption == "None")

p <- F_plot_pos(pdata1, pdata2, lab_y = "%") + 
  # geom_hline(yintercept = 0, color = "grey30") +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") #+ scale_x_discrete(limits = as.character(seq(2010, 2100,10)))
p

ggsave(p, filename = paste0(dir_fig,"/Figure8_position/GDPloss_mer.pdf"), 
       height = 6, width = 5.8, units = "cm")


p <- p + theme(legend.position = "right")
p
p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
ggsave(p_leg,filename = paste0(dir_fig, "Figure8_position/legend.pdf"),
       width = 2.2, height = 2.9, units = "cm")
rm(p, pdata1, pdata2)





## identify scenarios with negative values --------------
dir.create(paste0(dir_output, "/xlsx/"))
pdata1 <- df_ar6_GDPloss %>% dplyr::rename(GDPloss = "value") %>% select(-Variable) %>% 
  left_join(df_ar6_GDP_PPP %>% dplyr::rename(GDP = "value") %>% select(-Variable)) %>% 
  filter(!is.na(GDPloss), !is.na(GDP)) %>% 
  mutate(GDP_base = GDP + GDPloss, 
         GDP_loss_rate = GDPloss/(GDP + GDPloss)*100, unit_GDP_loss_rate = "%",
         Y = as.character(as.numeric(Y))) %>% filter(GDP_loss_rate < 0 & abs(GDP_loss_rate) > 0.1)
openxlsx::write.xlsx(pdata1, file = paste0(dir_output, "/xlsx/IPCC scenarios with negative GDP loss.xlsx"))


# checking the scenarios with negative GDP loss
unique(pdata1$Scenario)
a <- df_ar6 %>% filter(Variable %in% "GDP|MER", Scenario %in% c(unique(pdata1$Scenario)),
                       Model %in% c("REMIND-MAgPIE 2.1-4.2", "REMIND 2.1")) %>% 
  pivot_longer(cols = colnames(.)[6:86], names_to = "Y", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(BaU_Scenario = case_when( startsWith(Scenario, "CEMICS") & Model == "REMIND 2.1" ~ "CEMICS_NPI",
                                   startsWith(Scenario, "CEMICS_SSP1") & Model == "REMIND-MAgPIE 2.1-4.2" ~ "CEMICS_SSP1-Npi",
                                   startsWith(Scenario, "CEMICS_SSP2") & Model == "REMIND-MAgPIE 2.1-4.2" ~ "CEMICS_SSP2-Npi",
                                   startsWith(Scenario, "EN_") ~ "EN_NPi2100",
                                   startsWith(Scenario, "LeastTotalCost_LTC_brkLR15_SSP1_P50") ~ "LeastTotalCost_NPi_brkLR15_SSP1_P50",
                                   startsWith(Scenario, "R2p1_SSP1") ~ "R2p1_SSP1-NPi",
                                   startsWith(Scenario, "R2p1_SSP2") ~ "R2p1_SSP2-NPi",
                                   startsWith(Scenario, "R2p1_SSP5") ~ "R2p1_SSP5-NPi",
                                   startsWith(Scenario, "EMF33_1.5C_nofuel") ~ "EMF33_Baseline",
                                   startsWith(Scenario, "NGFS2_") ~ "NGFS2_Current Policies",
                                   Scenario == "SusDev_SDP-PkBudg1000" ~ "SusDev_SDP-NPi",
                                   Scenario == "SusDev_SSP1-PkBudg900" ~ "SusDev_SSP1-NPi",
                                   Scenario == "SusDev_SSP2-PkBudg900"  ~ "SusDev_SSP1-NPi",
                                   startsWith(Scenario,"DeepElec_SSP2_ HighRE_Budg900") ~ "DeepElec_SSP2_Npi",  
                                   ))


b <- df_ar6 %>% filter(Variable %in% "GDP|MER", Scenario %in% c(unique(a$BaU_Scenario)),
                       Model %in% c("REMIND-MAgPIE 2.1-4.2", "REMIND 2.1")) %>% 
  pivot_longer(cols = colnames(.)[6:86], names_to = "Y", values_to = "BaU_value") %>% filter(!is.na(BaU_value)) %>% 
  dplyr::rename("BaU_Scenario" = "Scenario")

c <- a %>% left_join(b) %>% mutate(change = (BaU_value-value)) %>% mutate(Y = gsub("X", "", Y))


# comparison with pdata1
d <-  df_ar6_GDPloss %>% dplyr::rename(GDPloss = "value") %>% select(-Variable) %>% 
  left_join(df_ar6_GDP_PPP %>% dplyr::rename(GDP = "value") %>% select(-Variable)) %>% 
  filter(!is.na(GDPloss), !is.na(GDP)) %>% filter(GDPloss < 0) %>% 
  mutate(# GDP_base = GDP - GDPloss,
  #        GDP_loss_rate = -GDPloss/(GDP - GDPloss)*100, unit_GDP_loss_rate = "%",
         Y = as.character(as.numeric(Y)))


e <- d %>% left_join(c %>% select(-Variable)) %>% select(Model, Scenario, Region, Unit, Y, Category, GDP, value, GDPloss, change)

print("Comparing GDPloss (reported by the IPCC AR6 scenario set) with change (manually calculated by comparing the baseline and policy cases) in dataframe e confirmed that negative GDP loss reported by REMIND 2.1 and REMIND-MAgPIE 2.1-4.2 was due to reporting error or in the uncertainty range. The signs are reverse")

rm(GDPloss,pdata1, a,b,c,d,e)



## 2.2 Consumption Loss ----------------------------------------------------
df_ar6_CNS <- df_ar6 %>% F_fil_ar6("Consumption") %>% 
  mutate(value = abs(value))
df_ar6_CNSloss <- df_ar6 %>% F_fil_ar6("Policy Cost|Consumption Loss") %>% 
  mutate(value = abs(value))


CNSloss <- df_iamc %>% filter(VEMF == "Pol_Cos_Cns_Los_rat", R == "World") %>% F_woc() %>% dplyr::rename(CNSloss = "value")



# plot
pdata1 <- df_ar6_CNSloss %>% dplyr::rename(value_CNSloss = "value") %>% select(-Variable) %>% 
  left_join(df_ar6_CNS %>% dplyr::rename(value_CNS = "value") %>% select(-Variable)) %>% 
  filter(!is.na(value_CNSloss), !is.na(value_CNS)) %>% 
  mutate(value = value_CNSloss/(value_CNS + value_CNSloss)*100, 
         Y = as.character(as.numeric(Y))) #%>% filter(Y > 2060)

pdata2 <- CNSloss %>% mutate(value = CNSloss, Ref = paste0(SSP, policy, exemption))%>% 
  F_filter_main() %>% filter(target != "2C", exemption == "None")



p <- F_plot_pos(pdata1, pdata2, lab_y = "%") + 
  geom_hline(yintercept = 0, color = "grey30") +
  theme(legend.position = "none")  

p

ggsave(p, filename = paste0(dir_fig,"/Figure8_position/CNSloss.pdf"), 
       height = 6, width = 5.8, units = "cm")
rm(p, pdata1, pdata2, CNSloss, df_ar6_CNS, df_ar6_CNSloss)




## identify scenarios with negative values --------------
dir.create(paste0(dir_output, "/xlsx/"))
pdata1 <- df_ar6_GDPloss %>% dplyr::rename(GDPloss = "value") %>% select(-Variable) %>% 
  left_join(df_ar6_GDP_PPP %>% dplyr::rename(GDP = "value") %>% select(-Variable)) %>% 
  filter(!is.na(GDPloss), !is.na(GDP)) %>% 
  mutate(GDP_base = GDP + GDPloss, 
         GDP_loss_rate = GDPloss/(GDP + GDPloss)*100, unit_GDP_loss_rate = "%",
         Y = as.character(as.numeric(Y))) %>% filter(GDP_loss_rate < 0 & abs(GDP_loss_rate) > 0.1)
openxlsx::write.xlsx(pdata1, file = paste0(dir_output, "/xlsx/IPCC scenarios with negative GDP loss.xlsx"))

rm(df_ar6_GDPloss, GDPloss,pdata1)






# 3. Carbon price ---------------------------------------------------------
df_ar6_CP <- df_ar6 %>% F_fil_ar6("Price|Carbon") 

PGHG <- cge_ana %>%
  rgdx.param("PGHG") %>%
  left_join(MapScenario) %>% 
  F_filter(lis_R= lis_R) %>% F_woc()  

PGHG1 <- PGHG %>% filter(Y == 2025) %>% mutate(Y = 2020, PGHG = 0)
PGHG <- PGHG %>% rbind(PGHG1)
# $ per tonne CO2eq, "Carbon tax | dollar per tonne carbon dioxide equivalent"


## Plot  ----
pdata1 <- df_ar6_CP %>% filter(value < 10000,  Y %in% lis_ar6_Y, !is.na(value), Y %in% c(seq(2020, 2070, 10), 2090, 2100)) #%>% mutate(Y = as.character(Y))

pdata2 <- PGHG %>% mutate(value = PGHG) %>% filter(policy == "1.5C CP", Y %in% c(seq(2020, 2070, 10), 2090, 2100)) %>% 
  F_filter_main() %>% filter(target != "2C", exemption == "None")

p <- F_plot_pos(pdata1, pdata2, lab_y = "US$2010/t CO2") + theme(legend.position = "none") +
  scale_x_discrete(limits = c(seq(2025, 2100, 25)))
p

ggsave(p, filename = paste0(dir_fig,"/Figure8_position/Carbon price.pdf"), 
       height = 6, width = 6, units = "cm")

rm(PGHG, PGHG1, df_ar6_CP, p, pdata1, pdata2)


# 4. Energy ---------------------------------------------------------


## 4.1 Final energy ---------------------------------------------------------
df_fin <- df_iamc %>% filter(VEMF == "Fin_Ene", R == "World") %>% F_woc() %>% filter(policy != "Baseline non-CP") #%>% select(-VEMF) %>% mutate(com = factor(com, levels = lis_FinEne_type))
df_ar6_FIN <- df_ar6 %>% F_fil_ar6("Final Energy") 

df_GDP_MER <- df_iamc %>% filter(VEMF == "GDP_MER", R == "World") %>% F_woc() %>% filter(policy != "Baseline non-CP") 

# Final energy intensity of GDP (for comparison with IPCC figures)
df_fin_intensity <- df_GDP_MER %>% dplyr::rename("GDP_mer" = "value") %>% select(-VEMF) %>% left_join(df_fin %>% dplyr::rename("fin" = "value") %>% select(-VEMF)) %>% 
  mutate(idx = fin/GDP_mer) 
df_fin_intensity_2020 <- df_fin_intensity %>% filter(Y == "2020") %>% select(-Y, -GDP_mer, -fin) %>% dplyr::rename(idx_2020 = idx) 

df_fin_intensity <- df_fin_intensity %>% left_join(df_fin_intensity_2020) %>% 
  mutate(idx=idx/idx_2020 * 100)
rm(df_fin_intensity_2020)


# Final energy profile

pdata1 <- df_ar6_FIN %>% filter(Y %in% lis_ar6_Y, !is.na(value)) #%>% mutate(Y = as.character(Y))

pdata2 <- df_fin %>% 
  F_filter_main() %>% filter(target != "2C", exemption == "None")

p <- F_plot_pos(pdata1, pdata2, lab_y = "EJ/yr") + theme(legend.position = "none") #+ scale_x_discrete(limits = as.character(seq(2000, 2100,25)))
p

ggsave(p, filename = paste0(dir_fig,"/Figure8_position/Final energy.pdf"), 
       height = 6, width = 6, units = "cm")

rm(df_fin, df_ar6_FIN, p, pdata1, pdata2)




### 4.1.1 Composition in final energy ---------------------------------------------------------
df_fin <- df_iamc %>% filter(R == "World") %>% left_join(map_Fin) %>%  F_woc() %>% filter(policy != "Baseline non-CP", !is.na(com)) %>% 
  mutate(Variable = com)
colnames(df_fin)
df_fin_rate <- df_fin %>% group_by(R, Y, Ref, policy, SSP, exemption, target) %>% 
  mutate(rate = value/sum(value) * 100)




lis_ar6_fin <- c("Final Energy|Electricity",
                 "Final Energy|Gases",
                 "Final Energy|Geothermal",
                 "Final Energy|Heat",
                 "Final Energy|Hydrogen",
                 "Final Energy|Liquids",
                 "Final Energy|Other",
                 "Final Energy|Solar",
                 "Final Energy|Solids|Biomass",
                 "Final Energy|Solids|Coal"
                 
)
df_ar6_fin <- df_ar6 %>% F_fil_ar6(lis_ar6_fin) %>% 
  mutate(Variable =
           recode(.$Variable, 
                  "Final Energy|Electricity" = "Electricity",
                  "Final Energy|Gases" = "Gas",
                  "Final Energy|Geothermal" = "Geothermal",
                  "Final Energy|Heat" = "Heat",
                  "Final Energy|Hydrogen" = "Hydrogen",
                  "Final Energy|Liquids" = "Liquid",
                  "Final Energy|Other" = "Others",
                  "Final Energy|Solar" = "Solar",
                  "Final Energy|Solids|Biomass" = "Biomass",
                  "Final Energy|Solids|Coal"  = "Solids"
           ))


colnames(df_ar6_fin)


pdata1 <- df_ar6_fin
pdata2 <- df_fin %>% filter(policy != "Baseline")%>% 
  F_filter_main() %>% filter(target != "2C", exemption == "None")
p <- F_plot_pos_dcom(pdata1, pdata2, lab_y = "EJ/yr") + facet_wrap(~ Variable, scales = "free") 
p
ggsave(p, filename = paste0(dir_fig,"/Figure8_position/Final energy_com.pdf"), 
       height = 18, width = 24, units = "cm")

rm(pdata1, pdata2, p, lis_ar6_fin, df_ar6_fin, df_fin)









## 4.2 Second energy ---------------------------------------------------------
# Secondary energy profile
df_sec <- df_iamc %>% filter(VEMF == "Sec_Ene", R == "World") %>% F_woc() %>% filter(policy != "Baseline non-CP") #%>% select(-VEMF) %>% mutate(com = factor(com, levels = lis_FinEne_type))
df_ar6_sec <- df_ar6 %>% F_fil_ar6("Secondary Energy") 


pdata1 <- df_ar6_sec %>% filter(Y %in% lis_ar6_Y, !is.na(value))

pdata2 <- df_sec %>% 
  F_filter_main() %>% filter(target != "2C", exemption == "None")

p <- F_plot_pos(pdata1, pdata2, lab_y = "EJ/yr") + theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 300))
p

ggsave(p, filename = paste0(dir_fig,"/Figure8_position/Secondary energy.pdf"), 
       height = 6, width = 6, units = "cm")

rm(df_sec, df_ar6_sec, p, pdata1, pdata2)



### 4.2.1 Composition in Secondary energy ---------------------------------------------------------
df_sec <- df_iamc %>% filter(R == "World") %>% left_join(map_Sec) %>%  F_woc() %>% filter(policy != "Baseline non-CP", !is.na(com)) %>% 
  mutate(Variable = com)


lis_ar6_sec <- c("Secondary Energy|Electricity","Secondary Energy|Gases","Secondary Energy|Heat", 
                 "Secondary Energy|Hydrogen", "Secondary Energy|Liquids", "Secondary Energy|Other Carrier", 
                 "Secondary Energy|Solids|Biomass", "Secondary Energy|Solids|Coal")
df_ar6_sec <- df_ar6 %>% F_fil_ar6(lis_ar6_sec) %>% 
  mutate(Variable = recode(.$Variable, "Secondary Energy|Electricity" = "Electricity",
                           "Secondary Energy|Gases" = "Gas",
                           "Secondary Energy|Heat" = "Hear", 
                           "Secondary Energy|Hydrogen" = "Hydrogen", 
                           "Secondary Energy|Liquids" = "Oil", 
                           "Secondary Energy|Other Carrier" = "Others", 
                           "Secondary Energy|Solids|Biomass" = "Biomass", 
                           "Secondary Energy|Solids|Coal" = "Coal"))



pdata1 <- df_ar6_sec %>% mutate(Y = as.character(Y) %>% as.numeric())
pdata2 <- df_sec %>% filter(policy != "Baseline") %>% mutate(Y = as.character(Y) %>% as.numeric())%>% 
  F_filter_main() %>% filter(target != "2C", exemption == "None")

p <- F_plot_pos_dcom(pdata1, pdata2, lab_y = "EJ/yr") + facet_wrap(~ Variable, scales = "free") #+ scale_x_continuous(limits = (seq(2010, 2100,10)))
p
ggsave(p, filename = paste0(dir_fig,"/Figure8_position/Secondary energy_com.pdf"), 
       height = 18, width = 24, units = "cm")

rm(pdata1, pdata2, p, lis_ar6_sec, df_ar6_sec, df_sec)







### 4.2.2 Composition in Power energy ---------------------------------------------------------
df_sec <- df_iamc %>% filter(R == "World") %>% left_join(map_Pow) %>%  F_woc() %>% filter(policy != "Baseline non-CP", !is.na(com)) %>% 
  mutate(Variable = com)


lis_ar6_sec <- c("Secondary Energy|Electricity|Biomass|w/ CCS",
                 "Secondary Energy|Electricity|Biomass|w/o CCS",
                 "Secondary Energy|Electricity|Coal|w/ CCS",
                 "Secondary Energy|Electricity|Coal|w/o CCS",
                 "Secondary Energy|Electricity|Curtailment",
                 "Secondary Energy|Electricity|Gas|w/ CCS",
                 "Secondary Energy|Electricity|Gas|w/o CCS",
                 "Secondary Energy|Electricity|Geothermal",
                 "Secondary Energy|Electricity|Hydro",
                 "Secondary Energy|Electricity|Nuclear",
                 "Secondary Energy|Electricity|Ocean",
                 "Secondary Energy|Electricity|Oil|w/ CCS",
                 "Secondary Energy|Electricity|Oil|w/o CCS",
                 "Secondary Energy|Electricity|Other",
                 "Secondary Energy|Electricity|Solar",
                 "Secondary Energy|Electricity|Storage Losses",
                 "Secondary Energy|Electricity|Transmission Losses",
                 "Secondary Energy|Electricity|Wind")

df_ar6_sec <- df_ar6 %>% F_fil_ar6(var = lis_ar6_sec) %>% 
  mutate(Variable = recode(.$Variable, 
                           "Secondary Energy|Electricity|Biomass|w/ CCS" = "Biomass (with CCS)",
                           "Secondary Energy|Electricity|Biomass|w/o CCS" = "Biomass (without CCS)",
                           "Secondary Energy|Electricity|Coal|w/ CCS" = "Coal (with CCS)",
                           "Secondary Energy|Electricity|Coal|w/o CCS" = "Coal (without CCS)",
                           "Secondary Energy|Electricity|Curtailment" = "Curtailment",
                           "Secondary Energy|Electricity|Gas|w/ CCS" = "Gas (with CCS)",
                           "Secondary Energy|Electricity|Gas|w/o CCS" = "Gas (without CCS)",
                           "Secondary Energy|Electricity|Geothermal" = "Geothermal",
                           "Secondary Energy|Electricity|Hydro" = "Hydropower",
                           "Secondary Energy|Electricity|Nuclear" = "Nuclear",
                           "Secondary Energy|Electricity|Ocean" = "Ocean",
                           "Secondary Energy|Electricity|Oil|w/ CCS" = "Oil (with CCS)",
                           "Secondary Energy|Electricity|Oil|w/o CCS" = "Oil (without CCS)",
                           "Secondary Energy|Electricity|Other" = "Others",
                           "Secondary Energy|Electricity|Solar" = "Solar",
                           "Secondary Energy|Electricity|Storage Losses" = "Storage Losses",
                           "Secondary Energy|Electricity|Transmission Losses" = "Transmission Losses",
                           "Secondary Energy|Electricity|Wind" = "Wind"))



pdata1 <- df_ar6_sec
pdata2 <- df_sec %>% filter(policy != "Baseline")%>% 
  F_filter_main() %>% filter(target != "2C", exemption == "None")
p <- F_plot_pos_dcom(pdata1, pdata2, lab_y = "EJ/yr") + facet_wrap(~ Variable, scales = "free") #+ scale_x_discrete(limits = as.character(seq(2010, 2100,10)))
p
ggsave(p, filename = paste0(dir_fig,"/Figure8_position/Power sector_com.pdf"), 
       height = 18, width = 34, units = "cm")

rm(pdata1, pdata2, p, lis_ar6_sec, df_ar6_sec, df_sec, df_pow)






## 4.3 Primary energy ---------------------------------------------------------
# Primary energy profile
df_prm <- df_iamc %>% filter(VEMF == "Prm_Ene", R == "World") %>% F_woc() %>% filter(policy != "Baseline non-CP") #%>% select(-VEMF) %>% mutate(com = factor(com, levels = lis_FinEne_type))
df_ar6_prm <- df_ar6 %>% F_fil_ar6("Primary Energy") 


pdata1 <- df_ar6_prm %>% filter(Y %in% lis_ar6_Y, !is.na(value))

pdata2 <- df_prm %>% 
  F_filter_main() %>% filter(target != "2C", exemption == "None") 

p <- F_plot_pos(pdata1, pdata2, lab_y = "EJ/yr") + theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 300))
p

ggsave(p, filename = paste0(dir_fig,"/Figure8_position/Primary energy.pdf"), 
       height = 6, width = 6, units = "cm")


rm(df_prm, df_ar6_prm, p)





### 4.3.1 Composition in Primary energy ---------------------------------------------------------
df_prm <- df_iamc %>% filter(R == "World") %>% left_join(map_Prm) %>%  F_woc() %>% filter(policy != "Baseline non-CP", !is.na(com)) %>% 
  mutate(Variable = com)

lis_ar6_Prm <- c("Primary Energy|Biomass|Modern|w/ CCS","Primary Energy|Biomass|Modern|w/o CCS", 
                 "Primary Energy|Biomass|Traditional","Primary Energy|Coal|w/ CCS","Primary Energy|Coal|w/o CCS","Primary Energy|Gas|w/ CCS",
                 "Primary Energy|Gas|w/o CCS", "Primary Energy|Geothermal", "Primary Energy|Hydro", "Primary Energy|Nuclear", "Primary Energy|Ocean", 
                 "Primary Energy|Oil|w/ CCS", "Primary Energy|Oil|w/o CCS", "Primary Energy|Other", "Primary Energy|Solar","Primary Energy|Wind")
df_ar6_prm <- df_ar6 %>% F_fil_ar6(lis_ar6_Prm) %>% 
  mutate(Variable = recode(.$Variable, "Primary Energy|Biomass|Modern|w/ CCS" = "Biomass (with CCS)",
                           "Primary Energy|Biomass|Modern|w/o CCS" = "Biomass (without CCS)",
                           "Primary Energy|Biomass|Traditional" = "Biomass (traditional)",
                           "Primary Energy|Coal|w/ CCS" = "Coal (with CCS)",
                           "Primary Energy|Coal|w/o CCS" = "Coal (without CCS)",
                           "Primary Energy|Gas|w/ CCS" = "Gas (with CCS)",
                           "Primary Energy|Gas|w/o CCS"  = "Gas (without CCS)",
                           "Primary Energy|Geothermal" = "Geothermal",
                           "Primary Energy|Hydro" = "Hydropower", 
                           "Primary Energy|Nuclear" = "Nuclear", 
                           "Primary Energy|Ocean" = "Ocean", 
                           "Primary Energy|Oil|w/ CCS"  = "Oil (with CCS)", 
                           "Primary Energy|Oil|w/o CCS" = "Oil (without CCS)", 
                           "Primary Energy|Other" = "Others", 
                           "Primary Energy|Solar" = "Solar",
                           "Primary Energy|Wind" = "Wind"))



pdata1 <- df_ar6_prm
pdata2 <- df_prm %>% filter(policy != "Baseline")%>% 
  F_filter_main() %>% filter(target != "2C", exemption == "None")
p <- F_plot_pos_dcom(pdata1, pdata2, lab_y = "EJ/yr") + facet_wrap(~ Variable, scales = "free") #+ scale_x_discrete(limits = as.character(seq(2010, 2100,10)))
p
ggsave(p, filename = paste0(dir_fig,"/Figure8_position/Primary energy_com.pdf"), 
       height = 18, width = 24, units = "cm")





# legend ------------
p <- F_plot_pos_dcom(pdata1, pdata2, lab_y = "EJ/yr") 

p_leg <- ggpubr::get_legend(p, position = NULL) %>% ggpubr::as_ggplot()
p_leg
ggsave(p_leg, filename = paste0(dir_fig,"/Figure8_position/Legend.pdf"), 
       height = 5.8, width = 3, units = "cm")



rm(p_leg, p, pdata1, pdata2)





# 5. Price ----------------------------------------------------------------


## 5.1 Agriculture Price ----------------------------------------------------------------
lis_var <- lis_ar6_var[which(grepl("Price", lis_ar6_var) & grepl("Agriculture", lis_ar6_var))]

df_pri_agr <- df_ar6 %>% F_fil_ar6(lis_var)
unique(df_pri_agr$Model)

df_cge_pri_agr <- df_iamc %>% left_join(data.frame(VEMF = c("Prc_Agr_Cor_Ind","Prc_Agr_Liv_Ind", 
                                                            "Prc_Agr_NonEneCro_and_Liv_Ind","Prc_Agr_NonEneCro_Ind", 
                                                            "Prc_Agr_Ric_Ind", "Prc_Agr_Soy_Ind", "Prc_Agr_Whe_Ind"),
                                                   Variable = c("Price|Agriculture|Corn|Index","Price|Agriculture|Livestock|Index",
                                                                "Price|Agriculture|Non-Energy Crops and Livestock|Index","Price|Agriculture|Non-Energy Crops|Index",
                                                                "Price|Agriculture|Rice|Index", "Price|Agriculture|Soybean|Index",
                                                                "Price|Agriculture|Wheat|Index"))) %>% filter(!is.na(Variable)) %>% filter(R == "World") %>% F_woc()

pdata1 <- df_pri_agr %>% filter(Y %in% lis_ar6_Y, !is.na(value), Variable != "Price|Agriculture|Non-Energy Crops and Livestock|Index") %>% 
  mutate(Variable = recode(.$Variable, "Price|Agriculture|Corn|Index" = "Corn",
                           "Price|Agriculture|Livestock|Index" = "Livestock",
                           "Price|Agriculture|Non-Energy Crops and Livestock|Index" = "Non-Energy Crops and Livestock",
                           "Price|Agriculture|Non-Energy Crops|Index" = "Non-Energy Crops",
                           "Price|Agriculture|Rice|Index" = "Rice", 
                           "Price|Agriculture|Soybean|Index" = "Soybean",
                           "Price|Agriculture|Wheat|Index" ="Wheat"))

pdata2 <- df_cge_pri_agr %>% left_join(df_cge_pri_agr %>% filter(Y == "2010") %>% select(-Y) %>% dplyr::rename(value_2010 = value)) %>% mutate(value_od = value, value = value_od/value_2010) %>% 
  filter(Variable != "Price|Agriculture|Non-Energy Crops and Livestock|Index") %>% 
  mutate(Variable = recode(.$Variable, "Price|Agriculture|Corn|Index" = "Corn",
                           "Price|Agriculture|Livestock|Index" = "Livestock",
                           "Price|Agriculture|Non-Energy Crops and Livestock|Index" = "Non-Energy Crops and Livestock",
                           "Price|Agriculture|Non-Energy Crops|Index" = "Non-Energy Crops",
                           "Price|Agriculture|Rice|Index" = "Rice", 
                           "Price|Agriculture|Soybean|Index" = "Soybean",
                           "Price|Agriculture|Wheat|Index" ="Wheat"))

p <- F_plot_pos_dcom(pdata1, pdata2, lab_y = "Price index") + theme(legend.position = "none") +
  facet_wrap(~Variable, scales = "free") #+ scale_x_discrete(limits = as.character(seq(2010, 2100,10)))
p

ggsave(p, filename = paste0(dir_fig,"/Figure8_position/Price_Agri.pdf"), 
       height = 14, width = 22, units = "cm")


rm(p, pdata1, pdata2, df_cge_pri_agr, df_pri_agr, lis_var)






## 5.2 Energy Price ----------------------------------------------------------------
lis_var <- lis_ar6_var[which(grepl("Price", lis_ar6_var) & grepl("Final Energy", lis_ar6_var) & grepl("Residential and Commercial|Residential", lis_ar6_var))]
lis_var
df_pri_ene <- df_ar6 %>% F_fil_ar6(lis_var)
unique(df_pri_ene$Model)

df_cge_pri_ene <- df_iamc %>% left_join(data.frame(VEMF = c("Prc_Fin_Ene_Res_and_Com_Res_Ele", "Prc_Fin_Ene_Res_and_Com_Res_Gas_Nat_Gas", "Prc_Fin_Ene_Res_and_Com_Res_Liq_Bio",
                                                            "Prc_Fin_Ene_Res_and_Com_Res_Liq_Oil", "Prc_Fin_Ene_Res_and_Com_Res_SolidsBio", "Prc_Fin_Ene_Res_and_Com_Res_SolidsCoa"), 
                                                   Variable = c("Price|Final Energy|Residential and Commercial|Residential|Electricity", 
                                                                "Price|Final Energy|Residential and Commercial|Residential|Gases|Natural Gas", 
                                                                "Price|Final Energy|Residential and Commercial|Residential|Liquids|Biomass", 
                                                                "Price|Final Energy|Residential and Commercial|Residential|Liquids|Oil", 
                                                                "Price|Final Energy|Residential and Commercial|Residential|Solids|Biomass", 
                                                                "Price|Final Energy|Residential and Commercial|Residential|Solids|Coal"))) %>% filter(!is.na(Variable)) %>% filter(R == "World") %>% F_woc()

pdata1 <- df_pri_ene %>% filter(Y %in% lis_ar6_Y, !is.na(value)) %>% 
  mutate(Variable = recode(.$Variable, "Price|Final Energy|Residential and Commercial|Residential|Electricity" = "Electricity", 
                           "Price|Final Energy|Residential and Commercial|Residential|Gases|Natural Gas" = "Natural Gas", 
                           "Price|Final Energy|Residential and Commercial|Residential|Liquids|Biomass" = "Liquids|Biomass", 
                           "Price|Final Energy|Residential and Commercial|Residential|Liquids|Oil" = "Liquids|Oil", 
                           "Price|Final Energy|Residential and Commercial|Residential|Solids|Biomass" = "Solids|Biomass", 
                           "Price|Final Energy|Residential and Commercial|Residential|Solids|Coal" = "Solids|Coal"))

pdata2 <- df_cge_pri_ene %>% 
  mutate(Variable = recode(.$Variable, "Price|Final Energy|Residential and Commercial|Residential|Electricity" = "Electricity", 
                           "Price|Final Energy|Residential and Commercial|Residential|Gases|Natural Gas" = "Natural Gas", 
                           "Price|Final Energy|Residential and Commercial|Residential|Liquids|Biomass" = "Liquids|Biomass", 
                           "Price|Final Energy|Residential and Commercial|Residential|Liquids|Oil" = "Liquids|Oil", 
                           "Price|Final Energy|Residential and Commercial|Residential|Solids|Biomass" = "Solids|Biomass", 
                           "Price|Final Energy|Residential and Commercial|Residential|Solids|Coal" = "Solids|Coal"))



p <- F_plot_pos_dcom(pdata1, pdata2, lab_y = "US$2010/GJ") + theme(legend.position = "none") +
  facet_wrap(~Variable, scales = "free") #+ scale_x_discrete(limits = as.character(seq(2010, 2100,10)))
p

ggsave(p, filename = paste0(dir_fig,"/Figure8_position/Price_Ene.pdf"), 
       height = 14, width = 22, units = "cm")

rm(p, pdata1, pdata2, df_cge_pri_ene, df_pri_ene, lis_var)







# 6. CO2 emissions ---------------------------------------------------------
df_ar6_CO2 <- df_ar6 %>% F_fil_ar6("Emissions|CO2") 

df_CO2 <- df_iamc %>% 
  filter(VEMF == "Emi_CO2") %>% mutate(unit = "Mt CO2/yr") %>% 
  F_filter(lis_R= lis_R) %>% F_woc()  
# $ per tonne CO2eq, "Carbon tax | dollar per tonne carbon dioxide equivalent"


## Plot  ----
pdata1 <- df_ar6_CO2 %>% filter( Y %in% lis_ar6_Y, !is.na(value))

pdata2 <- df_CO2 %>% filter(policy == "1.5C CP", R == "World") %>%   F_filter_main()

p <- F_plot_pos(pdata1, pdata2, lab_y = "Mt CO2/yr") + theme(legend.position = "none") +
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +
  scale_y_continuous(limits = c(-20000, 60000), breaks = seq(-20000, 60000, by = 20000))
p

ggsave(p, filename = paste0(dir_fig,"/Figure8_position/CO2 emissions.pdf"), 
       height = 6, width = 6.3, units = "cm")



rm(p,pdata1,pdata2,df_CO2,df_ar6_CO2,df_ar6,df_ar6_fin,df_ar6_GDP_MER,df_ar6_GDP_PPP, df_ar6_prm, df_ar6_FIN, df_fin_intensity, df_fin_rate, df_GDP_MER, df_prm, df_ar6_GDPloss)


print("The END of 8_Positioning.R")

