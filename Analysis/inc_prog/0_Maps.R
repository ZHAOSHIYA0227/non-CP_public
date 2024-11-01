# This file defines the maps for AIM/PHI analysis 
# In the first feature paper and my thesis

# Shiya ZHAO, 2021/12/13

library(tidyverse)
library(gdata)
# Lists ----


lis_cp <- c("Baseline", "1.5C CP", "1.5C non-CP")

# lis_cgescenario <- c("SSP2_500C_CACN_DAC_NoCC_CGEEndwDAC_global1", "SSP2_BaU_NoCC_CGEEndwDAC_global1", 
#   "SSP2woc_500C_CACN_DAC_NoCC_CGEEndwDAC_global1", "SSP2woc_BaU_NoCC_CGEEndwDAC_global1",
#   "SSP2SSP3_500C_CACN_DAC_NoCC_CGEEndwDAC_global1", "SSP2SSP3_BaU_NoCC_CGEEndwDAC_global1",
#   "SSP2SSP3woc_500C_CACN_DAC_NoCC_CGEEndwDAC_global1","SSP2SSP3woc_BaU_NoCC_CGEEndwDAC_global1", 
#   "SSP2SSP1_500C_CACN_DAC_NoCC_CGEEndwDAC_global1", "SSP2SSP1_BaU_NoCC_CGEEndwDAC_global1", 
#   "SSP2SSP1woc_500C_CACN_DAC_NoCC_CGEEndwDAC_global1","SSP2SSP1woc_BaU_NoCC_CGEEndwDAC_global1")


MapScenario <- readr::read_delim(paste0(dir_def,"/scenario.map"), delim = ".",show_col_types = FALSE, col_names = FALSE) %>% 
  gdata::rename.vars(colnames(.), c("Ref", "SCENARIO")) %>% 
  mutate(Ref = gsub('"', '',.$Ref) %>% gsub('\t', '',.),
         SCENARIO = gsub('"', '',.$SCENARIO) %>% gsub('\t', '',.))


MapScenario_hunger <- readr::read_delim(paste0(dir_def,"/scenario_hunger.map"), delim = ".",show_col_types = FALSE, col_names = FALSE) %>% 
  gdata::rename.vars(colnames(.), c("Ref", "SCENARIO")) %>% 
  mutate(Ref = gsub('"', '',.$Ref) %>% gsub('\t', '',.),
         SCENARIO = gsub('"', '',.$SCENARIO) %>% gsub('\t', '',.))

lis_cgescenario <- unique(MapScenario$SCENARIO)

lis_ref_all <- unique(MapScenario$Ref)  

lis_ref_main <- c("Base", "1p5C", "1p5C_expt", "1p5C_woc", 
                 "SSP3_Base", "SSP3_1p5C", "SSP3_1p5C_expt", "SSP3_1p5C_woc",
                 "SSP1_Base", "SSP1_1p5C", "SSP1_1p5C_expt", "SSP1_1p5C_woc")



lis_cp_mitigation <- c("1.5C CP", "1.5C non-CP")

lis_I <- c('Food and nonalcoholic beverages',
           'Alcoholic beverages, tobacco, and narcotics',
           'Clothing and footwear',
           'Housing', 
           'Water', 
           'Energy',
           'Furnishings, household equipment and maintenance',
           'Health',
           'Transport',
           'Communication',
           'Recreation and culture',
           'Education',
           'Restaurants and hotels',
           'Miscellaneous goods and services')




lis_I_abb <- c('FNB',
               'ATN',
               'CFW',
               'HSG',
               'WTR',
               'ENE',
               'FHE',
               'HLT',
               'TRN',
               'CMN',
               'REC',
               'EDC',
               'REH',
               'MGS')

lis_I_abb <- c('Food',
               'Alcohol&Others',
               'Clothing',
               'Housing',
               'Water',
               'Energy',
               'Equipment',
               'Health',
               'Transport',
               'Communication',
               'Recreation',
               'Education',
               'Restaurant&Hotel',
               'Miscellaneous')



lis_DEC <- as.character(seq(1,10,1))

lis_TH <- c('1.9-threshold',
            '3.2-threshold',
            '5.5-threshold')

lis_rural <-c('DECILE1_rural',
              'DECILE2_rural',
              'DECILE3_rural',
              'DECILE4_rural',
              'DECILE5_rural',
              'DECILE6_rural',
              'DECILE7_rural',
              'DECILE8_rural',
              'DECILE9_rural',
              'DECILE10_rural',
              'ALL_rural')

lis_urban <- c('DECILE1_urban',
               'DECILE2_urban',
               'DECILE3_urban',
               'DECILE4_urban',
               'DECILE5_urban',
               'DECILE6_urban',
               'DECILE7_urban',
               'DECILE8_urban',
               'DECILE9_urban',
               'DECILE10_urban',
               'ALL_urban')

lis_national <- c('DECILE1_national',
                  'DECILE2_national',
                  'DECILE3_national',
                  'DECILE4_national',
                  'DECILE5_national',
                  'DECILE6_national',
                  'DECILE7_national',
                  'DECILE8_national',
                  'DECILE9_national',
                  'DECILE10_national',
                  'TOTAL')

# energy

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
                     'Biomass (without CCS)',
                     'Biomass (with CCS)',
                     'Others')

map_Prm <- data.frame(VEMF = lis_VPrm_Ene, com = lis_PrmEne_type)


# Secondary energy variables
lis_VSec_Ene <- c('Sec_Ene_SolidsCoa',
                  'Sec_Ene_Liq',
                  'Sec_Ene_Gas',
                  'Sec_Ene_Heat',
                  'Sec_Ene_Hyd',
                  'Sec_Ene_Ele',
                  'Sec_Ene_SolidsBio',
                  'Sec_Ene_Oth_Car')

lis_SecEne_type <- c("Coal", 
                     "Oil",
                     "Gas",
                     "Heat",
                     "Hydrogen",
                     "Electricity",
                     "Biomass",
                     "Others")

map_Sec <- data.frame(VEMF = lis_VSec_Ene, com = lis_SecEne_type)


# Secondary: power sector

lis_VSec_Power <- c('Sec_Ene_Ele_Coa_wo_CCS',
                    'Sec_Ene_Ele_Coa_w_CCS',
                    'Sec_Ene_Ele_Oil_wo_CCS',
                    'Sec_Ene_Ele_Oil_w_CCS',
                    'Sec_Ene_Ele_Gas_wo_CCS',
                    'Sec_Ene_Ele_Gas_w_CCS',
                    'Sec_Ene_Ele_Hyd',
                    'Sec_Ene_Ele_Nuc',
                    'Sec_Ene_Ele_Solar',
                    'Sec_Ene_Ele_Win',
                    'Sec_Ene_Ele_Geo',
                    'Sec_Ene_Ele_Bio_wo_CCS',
                    'Sec_Ene_Ele_Bio_w_CCS',
                    'Sec_Ene_Ele_Oth')

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
                     'Biomass (without CCS)',
                     'Biomass (with CCS)',
                     'Others')

map_Pow <- data.frame(VEMF = lis_VSec_Power, com = lis_PowEne_type)


# Final energy variables
lis_VFin_Ene <- c('Fin_Ene_Oth',
                  'Fin_Ene_SolidsBio',
                  'Fin_Ene_Ele',
                  'Fin_Ene_Solar',
                  'Fin_Ene_Geo',
                  'Fin_Ene_Heat',
                  'Fin_Ene_Hyd',
                  'Fin_Ene_Gas',
                  'Fin_Ene_Liq',
                  'Fin_Ene_SolidsCoa')


lis_FinEne_type <- c('Others',
                     'Biomass',
                     'Electricity',
                     'Solar',
                     'Geothermal',
                     'Heat',
                     'Hydrogen',
                     'Gas',
                     'Liquid',
                     'Solids')


map_Fin <- data.frame(VEMF = lis_VFin_Ene, com = lis_FinEne_type)


# Final energy demand in the residential sector

lis_VFin_Ene_Res <- c('Fin_Ene_Res_Oth', 
                      'Fin_Ene_Res_SolidsBio',
                      'Fin_Ene_Res_Ele',
                      'Fin_Ene_Res_Heat',
                      'Fin_Ene_Res_Hyd',
                      'Fin_Ene_Res_Gas',
                      'Fin_Ene_Res_Liq',
                      'Fin_Ene_Res_SolidsCoa')

lis_FinEneRes_type <- c('Others',
                     'Biomass',
                     'Electricity',
                     'Heat',
                     'Hydrogen',
                     'Gas',
                     'Liquid',
                     'Solids')

map_FinRes <- data.frame(VEMF = lis_VFin_Ene_Res, com = lis_FinEneRes_type)


# deciles
lis_seg_DEC_GCD <- c('DECILE1_rural',
                     'DECILE2_rural',
                     'DECILE3_rural',
                     'DECILE4_rural',
                     'DECILE5_rural',
                     'DECILE6_rural',
                     'DECILE7_rural',
                     'DECILE8_rural',
                     'DECILE9_rural',
                     'DECILE10_rural',
                     'DECILE1_urban',
                     'DECILE2_urban',
                     'DECILE3_urban',
                     'DECILE4_urban',
                     'DECILE5_urban',
                     'DECILE6_urban',
                     'DECILE7_urban',
                     'DECILE8_urban',
                     'DECILE9_urban',
                     'DECILE10_urban',
                     'Lowest_rural',
                     'Low_rural',
                     'Middle_rural',
                     'Higher_rural',
                     'Lowest_urban',
                     'Low_urban',
                     'Middle_urban',
                     'Higher_urban')

lis_seg_dec_gcd_1 <- c('R1','R2','R3','R4','R5','R6','R7','R8','R9','R10',
                       'U1','U2','U3','U4','U5','U6','U7','U8','U9','U10',
                       'RLowest', 'RLow','RMiddle','RHigher',
                       'ULowest', 'ULow','UMiddle','UHigher')

lis_GCD_Seg <- c('Lowest_rural',
                 'Low_rural',
                 'Middle_rural',
                 'Higher_rural',
                 'Lowest_urban',
                 'Low_urban',
                 'Middle_urban',
                 'Higher_urban')

lis_Seg_gcd <- c("Lowest_urban",
                 "Low_urban",
                 "Middle_urban",
                 "Higher_urban",
                 "Lowest_rural",
                 "Low_rural",
                 "Middle_rural",
                 "Higher_rural")

lis_Seg_gcd_abb <- c("ULowest",
                     "ULow",
                     "UMiddle",
                     "UHigher",
                     "RLowest",
                     "RLow",
                     "RMiddle",
                     "RHigher")

# # Primary energy variables
# lis_VPrm_Ene <- c(  "Prm_Ene_Coa_wo_CCS",
#                     "Prm_Ene_Coa_w_CCS",
#                     "Prm_Ene_Oil_wo_CCS",
#                     "Prm_Ene_Oil_w_CCS",
#                     "Prm_Ene_Gas_wo_CCS",
#                     "Prm_Ene_Gas_w_CCS",
#                     "Prm_Ene_Hyd",
#                     "Prm_Ene_Nuc",
#                     "Prm_Ene_Solar",
#                     "Prm_Ene_Win",
#                     "Prm_Ene_Geo",
#                     "Prm_Ene_Bio_wo_CCS",
#                     "Prm_Ene_Bio_w_CCS",
#                     "Prm_Ene_Oth")
# 
# # Secondary energy variables
# lis_VSec_Ene <- c('Sec_Ene_Ele',
#                   'Sec_Ene_Gas',
#                   'Sec_Ene_Heat',
#                   'Sec_Ene_Solids',
#                   'Sec_Ene_Liq')
# 
# lis_VSec_Power <- c('Sec_Ene_Ele_Nuc',
#                     'Sec_Ene_Ele_Oth',
#                     'Sec_Ene_Ele_Bio_w_CCS',
#                     'Sec_Ene_Ele_Bio_wo_CCS',
#                     'Sec_Ene_Ele_Coa_w_CCS',
#                     'Sec_Ene_Ele_Coa_wo_CCS',
#                     'Sec_Ene_Ele_Gas_w_CCS',
#                     'Sec_Ene_Ele_Gas_wo_CCS',
#                     'Sec_Ene_Ele_Geo',
#                     'Sec_Ene_Ele_Hyd',
#                     'Sec_Ene_Ele_Oil_w_CCS',
#                     'Sec_Ene_Ele_Oil_wo_CCS',
#                     'Sec_Ene_Ele_Solar',
#                     'Sec_Ene_Ele_Win')
# 
# # Final energy variables
# lis_VFin_Ene <- c('Fin_Ene_Ele',
#                   'Fin_Ene_Gas',
#                   'Fin_Ene_Geo',
#                   'Fin_Ene_Heat',
#                   'Fin_Ene_Hyd',
#                   'Fin_Ene_Liq',
#                   'Fin_Ene_Solids')
# 
# 

# Maps ----
# read the map for CO2 sectors in AIMHub 
# Mapping the sectors and activities
# MCO2_S2(AC,SCO2_S)                                         Map for decomposition analysis
# SCO2_S                                                      Sector for excel output /IND,PWR,OEN,TRS,OTH,RSD,SER,AGR,CCS,BIO,FFE/


# MCO2_S2(AC,SCO2_S)=MCO2_S(AC,SCO2_S);
# MCO2_S2(H,"RSD")=YES;
# MCO2_S2("CSS","SER")=YES;
# MCO2_S2(A,"AGR")$(AAGR(A) AND (NOT A_BTRA(A)))=YES;
# MCO2_S2("FRS","AGR")=YES;
# MCO2_S2("AFS","AGR")=YES;
# MCO2_S2(A,"BIO")$(A_BTRA(A))=YES;
# MCO2_S2(A,"FFE")$(ARFF(A))=YES;
# MCO2_S2(A,"CCS")$(A_CCS(A) OR A_DAC(A) OR A_STO(A))=YES;
# MCO2_S2(A,"OEN")$(A_CCS(A))=YES;
# MCO2_S2("T_D","PWR")=YES;
# MCO2_S2("OHE","PWR")=YES;
# MCO2_S2("RIS","PWR")=YES;



MCO2_S2 <- readxl::read_xlsx(paste0("../",prog_loc,"/data/MCO2_S2.xlsx"), sheet = "MCO2_S2") %>% distinct()
MSCO2_S2S <- readxl::read_xlsx(paste0("../",prog_loc,"/data/MCO2_S2.xlsx"), sheet = "SCO2_S2S") %>% distinct()


# map fao country code for hunger risk assessment
map_Rfao2iso3 <- readr::read_delim("../../DataArchive/hungertool/240627/MapR_WDI.map", delim = ".", 
                                   col_names = c("R_code","R"), 
                                   show_col_types = F) %>% 
  mutate(R_code = gsub('"', '',.$R_code) %>% gsub('\t', '',.),
         R =gsub('\t', '',.$R)) %>% 
  mutate(R_code = as.character(R_code))


MapI <- data.frame(I = lis_I,
                   I_abb = lis_I_abb) 


map_com <- readr::read_delim(paste0(dir_def,"/CommodityWeight.txt"), delim = ".", 
                             col_names = c("COM","I"), 
                             show_col_types = F) %>% 
  mutate(COM = gsub('"', '',.$COM) %>% gsub('\t', '',.),
         I = gsub('\t\"', '',.$I) %>% str_split_i(., '\"\t', i = 1) ) %>% 
  filter(!startsWith(COM, "*"))

lis_com <- gsub('"', '',map_com$COM) %>% gsub('\t', '',.) %>% unique()

map_comname <- openxlsx::read.xlsx(paste0(dir_def,"/commodity_name.xlsx")) %>% 
  mutate(COM_name = case_when(COM_name == "Non ruminant livestock, other livestock and fishery" ~ "Other livestock", 
                              COM_name == "Textiles and apparel and leather and wood products" ~ "Apparel and wood products",
                              COM_name == "Vegetable, fruits and nuts" ~ "Vege and nuts",
                              COM_name == "Ruminant livestock" ~ "Ruminant",
                              !(COM_name %in% c("Non ruminant livestock, other livestock and fishery", "Textiles and apparel and leather and wood products", "Vegetable, fruits and nuts", "Ruminant livestock") )~ COM_name))



MapDec <- data.frame(seq(0.1,1,0.1),seq(1,10,1)) %>%
  gdata::rename.vars(from = colnames(.), to = c("quantile","DEC"))






MapCom <- data.frame(lis_I_abb,
                     c('Food','Others','Others','Housing', 'Others', 'Energy',
                       'Equipment','Others','Transport','Others','Others','Others','Others','Others')) %>%
  gdata::rename.vars(from = colnames(.), to = c("I","COM"))



MapEneCombine <- data.frame(c('FNB','ATN','CFW','HSG','WTR',
                              'SLD','GAS','LQD','ELE','BIO','OTH',
                              'FHE','HLT','TRN','CMN','REC','EDC','REH','MGS'),
                            c('FNB','ATN','CFW','HSG','WTR',
                              'ENE','ENE','ENE','ENE','ENE','ENE',
                              'FHE','HLT','TRN','CMN','REC','EDC','REH','MGS')) %>%
  gdata::rename.vars(from = colnames(.), to = c("I","COM"))

map_seg <- data.frame(c(lis_rural,lis_urban,lis_national),
                      c(rep("rural",11),rep("urban",11),rep("national",11)),
                      rep(c(as.character(seq(1,10,1)),"all"),3)) %>%
  rename.vars(from = colnames(.), to = c("Seg", "Seg0","Seg1"))

lis_rural_abb <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R_all")
lis_urban_abb <- c("U1","U2","U3","U4","U5","U6","U7","U8","U9","U10","U_all")
lis_national_abb <- c("N1","N2","N3","N4","N5","N6","N7","N8","N9","N10","N_all")

map_seg_abb <- data.frame(Seg = c(lis_rural,lis_urban,lis_national),
                      Seg_abb = c(lis_rural_abb,lis_urban_abb,lis_national_abb)) 




map_seg_gcd <- data.frame(Seg = lis_Seg_gcd,
                          Seg_gcd_abb=lis_Seg_gcd_abb)

map_R <- readr::read_delim(paste0(dir_def,"/RegionmapR5.map"), delim = ".", 
                          col_names = c("R","R_CGE"), 
                          show_col_types = F) %>% mutate(R_CGE = gsub('"', '',.$R_CGE) %>% gsub('\t', '',.),
                                                         R =gsub('\t', '',.$R)) %>% 
  mutate(R_CGE = case_when(R_CGE == "WLD" ~ "World", R_CGE != "WLD" ~ R_CGE))


map_R17 <- readr::read_delim(paste0(dir_def,"/0_Regionmap_AIMHub.map"), delim = ".", 
                             col_names = c("R","R_CGE","R_full"), 
                             show_col_types = F) 


map_R17to5 <- map_R17 %>% left_join(map_R %>% dplyr::rename(R5 = R_CGE))
