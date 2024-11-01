# The Rscript deals with the SAM in AIM-PHI comprehensive output
# Shiya ZHAO, 2024.03.29


# packages ----------------------------------------------------------------

require(tidyverse)
require(gdxrrw)



# 0. merge and load -------------------------------------------------------
# merge dgx files
if(!file.exists(paste0(dir_cgeoutput, "/cbnal0.gdx"))){
  setwd(paste0(dir_cgeoutput, "/cbnal0/"))
  gams_merge <- paste0("gdxmerge ", "*.gdx ", "output = ../cbnal0.gdx ")
  system(gams_merge)
  setwd(dir_wd)
}

# load data 

df_sam_value <- paste0(dir_cgeoutput, "cbnal0.gdx") %>% rgdx.param("PSAM_value") %>% 
  gdata::rename.vars(colnames(.), c("SCENARIO", "Y", "R", "A1", "A2", "Value")) %>% 
  mutate(SCENARIO = gsub("global_17_", "", SCENARIO)) %>% left_join(MapScenario) %>% filter(!is.na(Ref), Y %in% seq(2005, 2100,5))

df_matrix <- df_sam_value %>% select(-SCENARIO) %>% filter(Ref %in% c("1p5C", "1p5C_woc", "Base"), Y == "2030") %>% pivot_wider(names_from = "A2", values_from = "Value") 


a <- df_matrix %>% filter(R == "CHN", Ref == "1p5C")

df_sam_volume <- paste0(dir_cgeoutput, "cbnal0.gdx") %>% rgdx.param("PSAM_volume") %>% 
  gdata::rename.vars(colnames(.), c("SCENARIO", "Y", "R", "A1", "A2", "Volume")) %>% 
  mutate(SCENARIO = gsub("global_17_", "", SCENARIO)) %>% left_join(MapScenario) %>% filter(!is.na(Ref), Y %in% seq(2005, 2100,5))
df_matrix_volume <- df_sam_volume %>% select(-SCENARIO) %>% filter(Ref %in% c("1p5C", "1p5C_woc", "Base"), Y == "2030") %>% pivot_wider(names_from = "A2", values_from = "Volume") 



lis_com_agri <- map_com %>% filter(I == "Food and nonalcoholic beverages")
lis_com_agri <- unique(lis_com_agri$COM)

# direct reduction cost for non energy related emissions
map_com1 <- map_com %>% left_join(map_comname) %>% mutate( COM = gsub("COM_", "", COM) %>% toupper())
unique(map_com1$COM)

unique(df_sam_value$A1)
unique(df_sam_value$A2)

a <- data.frame(A = unique(df_sam_value$A2), idx2 = 1) %>% left_join(data.frame(A = unique(df_sam_value$A1), idx1 = 1))

a <- df_sam_value %>% filter(A1 %in% c("TOTAL")) %>% filter(!is.na(SCENARIO)) %>% select(-SCENARIO) 
df_sam_redcos <- df_sam_value %>% filter(A1 %in% c("NEA"),!is.na(SCENARIO)) %>% select(-SCENARIO) %>% 
  # left_join() %>% 
  dplyr::rename("COM" = "A2") %>% left_join(map_com1)



# 1. direct reduction cost in the agriculture sector ----
df_sam_redcos_agri <- df_sam_redcos %>% filter(I == "Food and nonalcoholic beverages")  %>% F_woc()
unique(df_sam_redcos_agri$R)
unique(df_sam_redcos_agri$Y)

dir.create(paste0(dir_fig, "/Figure5_ReductionCost_agri_area/"))
lis_R_loop <- unique(df_sam_redcos_agri$R)
for(r in 1:length(lis_R_loop)){
  pdata <- df_sam_redcos_agri %>% filter(Y %in% seq(2020, 2100,1)) %>% filter(R == lis_R_loop[r], SSP == "SSP2", exemption == "None")
  p <- ggplot(pdata) +
    geom_area(aes(x = as.numeric(as.character(Y)), y = Value, fill = COM_name)) + facet_grid(~policy) +
    MyTheme + theme(axis.text.x = element_text(angle = 60)) + labs(x = "Year", y = "Value", subtitle = lis_R_loop[r])
  p
  
  ggsave(p, filename = paste0(dir_fig, "/Figure5_ReductionCost_agri_area/",lis_R_loop[r],".pdf"), width = 20, height = 10, units = "cm")
}





## 1.1 reduction cost row -----
dir.create(paste0(dir_fig, "/Figure5_ReductionCost_agri_column/"))
lis_R_loop <- unique(df_sam_redcos_agri$R)
# for(r in 1:length(lis_R_loop)){
  pdata <- df_sam_redcos_agri %>% filter(Y %in% c(2030), exemption == "None") #%>% 
    # filter(R == lis_R_loop[r])
  p <- ggplot() +
    geom_col(pdata %>% filter(SSP == "SSP2"), mapping = aes(x = COM_name, y = Value, fill = policy), position = "dodge", linewidth = .7, alpha = .7) + 
    geom_point(pdata %>% filter(SSP != "SSP2"), mapping = aes(x = COM_name, y = Value, color = policy, shape = SSP)) + 
    facet_wrap(~R) +
    MyTheme + theme(axis.text.x = element_text(angle = 60)) + labs(x = "Year", y = "Value of reduction cost") +
    scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Policy"), color = guide_legend(title = "Policy"))
  p
  
  ggsave(p, filename = paste0(dir_fig, "/Figure5_ReductionCost_agri_column.pdf"), width = 30, height = 20, units = "cm")
# }


  
  ## 1.2 reduction cost global ----
  
  pdata <- df_sam_redcos_agri %>% filter(Y %in% c(2030, 2050), exemption == "None") %>% 
    dplyr::group_by(Y, A1, COM_name, policy, SSP, exemption, target) %>% dplyr::reframe(value = sum(Value), R = "World")
  
  p <- ggplot() +
    geom_col(pdata %>% filter(SSP == "SSP2"), mapping = aes(x = COM_name, y = value, fill = policy), position = "dodge", linewidth = .7, alpha = .7) + 
    geom_point(pdata %>% filter(SSP != "SSP2"), mapping = aes(x = COM_name, y = value, color = policy, shape = SSP)) + 
    MyTheme + theme(axis.text.x = element_text(angle = 60)) + labs(x = "Sector", y = "Value of reduction cost") +
    facet_wrap(~Y) +
    scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Policy"), color = guide_legend(title = "Policy"))
  p 
  
  ggsave(p, filename = paste0(dir_fig, "/Figure5_ReductionCost_agri_column_global.pdf"), width = 20, height = 8, units = "cm")
  
  
  
  
  
  
  # 2. direct reduction cost in the eneculture sector ----
  df_sam_redcos_ene <- df_sam_redcos %>% filter(I == "Energy")  %>% F_woc()
  unique(df_sam_redcos_ene$R)
  unique(df_sam_redcos_ene$Y)
  
  dir.create(paste0(dir_fig, "/Figure5_ReductionCost_ene_area/"))
  lis_R_loop <- unique(df_sam_redcos_ene$R)
  for(r in 1:length(lis_R_loop)){
    pdata <- df_sam_redcos_ene %>% filter(Y %in% seq(2020, 2100,1)) %>% filter(R == lis_R_loop[r], SSP == "SSP2", exemption == "None")
    p <- ggplot(pdata) +
      geom_area(aes(x = as.numeric(as.character(Y)), y = Value, fill = COM_name)) + facet_grid(~policy) +
      MyTheme + theme(axis.text.x = element_text(angle = 60)) + labs(x = "Year", y = "Value", subtitle = lis_R_loop[r])
    p
    
    ggsave(p, filename = paste0(dir_fig, "/Figure5_ReductionCost_ene_area/",lis_R_loop[r],".pdf"), width = 20, height = 10, units = "cm")
  }
  
  
  
  
  
  ## 2.1 reduction cost row -----
  dir.create(paste0(dir_fig, "/Figure5_ReductionCost_ene_column/"))
  lis_R_loop <- unique(df_sam_redcos_ene$R)
  # for(r in 1:length(lis_R_loop)){
  pdata <- df_sam_redcos_ene %>% filter(Y %in% c(2030), exemption == "None") #%>% 
  # filter(R == lis_R_loop[r])
  p <- ggplot() +
    geom_col(pdata %>% filter(SSP == "SSP2"), mapping = aes(x = COM_name, y = Value, fill = policy), position = "dodge", linewidth = .7, alpha = .7) + 
    geom_point(pdata %>% filter(SSP != "SSP2"), mapping = aes(x = COM_name, y = Value, color = policy, shape = SSP)) + 
    facet_wrap(~R) +
    MyTheme + theme(axis.text.x = element_text(angle = 60)) + labs(x = "Year", y = "Value of reduction cost") +
    scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Policy"), color = guide_legend(title = "Policy"))
  p
  
  ggsave(p, filename = paste0(dir_fig, "/Figure5_ReductionCost_ene_column.pdf"), width = 30, height = 20, units = "cm")
  # }
  
  
  
  ## 2.2 reduction cost global ----
  
  pdata <- df_sam_redcos_ene %>% filter(Y %in% c(2030, 2050), exemption == "None") %>% 
    dplyr::group_by(Y, A1, COM_name, policy, SSP, exemption, target) %>% dplyr::reframe(value = sum(Value), R = "World")
  
  p <- ggplot() +
    geom_col(pdata %>% filter(SSP == "SSP2"), mapping = aes(x = COM_name, y = value, fill = policy), position = "dodge", linewidth = .7, alpha = .7) + 
    geom_point(pdata %>% filter(SSP != "SSP2"), mapping = aes(x = COM_name, y = value, color = policy, shape = SSP)) + 
    MyTheme + theme(axis.text.x = element_text(angle = 60)) + labs(x = "Sector", y = "Value of reduction cost") +
    facet_wrap(~Y) +
    scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Policy"), color = guide_legend(title = "Policy"))
  p 
  
  ggsave(p, filename = paste0(dir_fig, "/Figure5_ReductionCost_ene_column_global.pdf"), width = 20, height = 8, units = "cm")
  
  
  
  
  
# 3. labor and capital rows --------
  MCO2_S2 <- readxl::read_xlsx(paste0("../",prog_loc,"/data/MCO2_S2.xlsx"), sheet = "MCO2_S2") %>% distinct()
  MSCO2_S2S <- readxl::read_xlsx(paste0("../",prog_loc,"/data/MCO2_S2.xlsx"), sheet = "SCO2_S2S") %>% distinct()
  
# volume
df_lab_vo <- df_sam_volume %>% select(-SCENARIO) %>% filter(Ref %in% c("1p5C", "1p5C_woc", "Base"), Y == "2030") %>% 
  filter(A1 %in% c("LAB", "CAP"), A2 %in% c("GRO","C_B","ARE","OIL","OMN","BTR","BTR3","I_S","OMF","TRS","CSS","CRP","NMM","T_D")) %>% 
    F_woc() %>% select(-Ref) %>% dplyr::rename(value = Volume) %>% F_cha_decom() %>% mutate(change = value-Baseline_None) %>% 
    left_join(map_R17to5 %>% select(R_CGE, R5) %>% dplyr::rename(R = R_CGE) %>% distinct()) %>% 
    dplyr::group_by(Y,A1,A2,SSP,policy,exemption,R5) %>% dplyr::reframe(change = sum(change)) %>% filter(!is.na(R5)) %>% 
    pivot_wider(names_from = "policy", values_from = "change") %>% mutate(change = `1.5C non-CP` - `1.5C CP`)

df_lab_vo <- df_sam_volume %>% select(-SCENARIO) %>% filter(Ref %in% c("1p5C", "1p5C_woc", "Base"), Y == "2030") %>% 
  filter(A1 %in% c("LAB", "CAP")) %>%
  left_join(MCO2_S2 %>% dplyr::rename(A2 = "A")) %>% left_join(MSCO2_S2S) %>% filter(!is.na(S)) %>% dplyr::group_by(Y, A1, R, S,Ref)  %>% reframe(Volume = sum(Volume)) %>%  
  F_woc() %>% select(-Ref) %>% dplyr::rename(value = Volume) %>% F_cha_decom() %>% mutate(change = value-Baseline_None) %>% 
  left_join(map_R17to5 %>% select(R_CGE, R5) %>% dplyr::rename(R = R_CGE) %>% distinct()) %>% 
  dplyr::group_by(Y,A1,S,SSP,policy,exemption,R5) %>% dplyr::reframe(change = sum(change)) %>% filter(!is.na(R5)) %>% 
  pivot_wider(names_from = "policy", values_from = "change") %>% mutate(change = `1.5C non-CP` - `1.5C CP`)
  
  
  
  lis_loop_A1 <- c("LAB", "CAP")
for(i in 1:length(lis_loop_A1)){
  pdata <- df_lab_vo %>% filter(A1 == lis_loop_A1[i])
  p <- ggplot(pdata) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_col(aes(x = S, y = change), position = "dodge") +
    MyTheme + theme(axis.text.x = element_text(angle = 60)) + facet_wrap(~R5) + labs(title = lis_loop_A1[i])
  p
  
  ggsave(p, filename = paste0(dir_fig, "/Figure5_group_",lis_loop_A1[i],"_volume_column_global.pdf"), width = 20, height = 15, units = "cm")
  
}
 
  
  
  
  # value
  
  df_lab_va <- df_sam_value %>% select(-SCENARIO) %>% filter(Ref %in% c("1p5C", "1p5C_woc", "Base"), Y == "2030") %>% 
    filter(A1 %in% c("LAB", "CAP"), A2 %in% c("GRO","C_B","ARE","OIL","OMN","BTR","BTR3","I_S","OMF","TRS","CSS","CRP","NMM","T_D")) %>% 
    F_woc() %>% select(-Ref) %>% dplyr::rename(value = Value) %>% F_cha_decom() %>% mutate(change = value-Baseline_None) %>% 
    left_join(map_R17to5 %>% select(R_CGE, R5) %>% dplyr::rename(R = R_CGE) %>% distinct()) %>% 
    dplyr::group_by(Y,A1,A2,SSP,policy,exemption,R5) %>% dplyr::reframe(change = sum(change)) %>% filter(!is.na(R5)) %>% 
    pivot_wider(names_from = "policy", values_from = "change") %>% mutate(change = `1.5C non-CP` - `1.5C CP`)
  
  lis_loop_A1 <- c("LAB", "CAP")
  for(i in 1:length(lis_loop_A1)){
    pdata <- df_lab_vo %>% filter(A1 == lis_loop_A1[i])
    p <- ggplot(pdata) +
      geom_hline(yintercept = 0, color = "grey") +
      geom_col(aes(x = A2, y = change), position = "dodge") +
      MyTheme + theme(axis.text.x = element_text(angle = 60)) + facet_wrap(~R5) + labs(title = lis_loop_A1[i])
    p
    
    ggsave(p, filename = paste0(dir_fig, "/Figure5_",lis_loop_A1[i],"_value_column_global.pdf"), width = 20, height = 10, units = "cm")
    
  }
  
  
  
  