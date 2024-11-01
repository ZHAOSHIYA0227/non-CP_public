# GDP decomposition for non-CP paper 

# Shiya ZHAO, 2021.06.06


#==== Fujimori et al 2019 NC ====#
# 1. Decomposition analysis according to Fujimori et al 2019 NC --------
# Paper: Energy transformation cost for the Japanese mid-century strategy
# DOI: 10.1038/s41467-019-12730-4

# load the data
# df_loss_dcp <- cge_ana %>% rgdx.param("Loss_dcp_gdp") %>% left_join(MapScenario) %>% filter(!is.na(Ref), SCO2_S != "OTH") %>% select(-SCENARIO)
# df_loss_dcp <- df_loss_dcp %>% 
#   mutate(Approach = case_when(decele %in% c("output_va", "fd_output", "va", "residual1") ~ "demand", decele %in% c("output", "va_output", "residual2") ~ "value-added")) %>% 
#   F_woc()

sw_agg <- F
if(sw_agg == T){
  txt_dcp <- "Loss_dcp_agg_gdp"
}else{
  txt_dcp <- "Loss_dcp_gdp"
}



df_loss_dcp <- cge_ana %>% rgdx.param(txt_dcp) %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO)
df_loss_dcp <- df_loss_dcp %>% 
  mutate(Approach = case_when(# decele %in% c("output_va", "fd_output", "va", "residual1") ~ "demand", 
                              decele %in% c("va","output", "va_output", "residual2") ~ "value-added")) %>% 
  F_woc()

df_va_ratio <- cge_ana %>% rgdx.param("VA_decomp_ind") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO) %>% 
  dplyr::rename(value = "VA_decomp_ind") %>% 
  dplyr::group_by(Y,R,Ref) %>% mutate(tot = sum(value), va_ratio = value/tot) %>% ungroup() %>% F_woc() %>% filter(policy == "Baseline") %>% 
  select( -value, -tot, -Ref, -policy, -target)


df_va_ratio <- cge_ana %>% rgdx.param("VA_decomp_ind") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO) %>% 
  dplyr::rename(value = "VA_decomp_ind") %>% filter(decele == "va_output") %>% 
  # dplyr::group_by(Y,R,Ref) %>% mutate(tot = sum(value), va_ratio = value/tot) %>% ungroup() %>% 
  # select( -value, -tot, -Ref, -policy, -target) %>% 
  F_woc() %>% filter(policy == "Baseline") %>% mutate(va_ratio = value) %>% 
  select(-value, -Ref, -policy, -target, -decele)


colnames(df_gdp)
df_gdp <- cge_ana %>% rgdx.param("GDP") %>% left_join(MapScenario) %>% select(-SCENARIO) %>% F_woc()  %>% 
  filter(!is.na(Ref), exemption == "None", R == "World") %>% dplyr::rename("value" = "GDP")%>% F_cha_decom() %>% F_filter_main() %>% 
  mutate(gdp_ratio = Baseline_None/value) %>% select(-Baseline_None, -`Baseline non-CP_None`, -value)

# define the function
F_plot_dcp <- function(df_col = pdata1, df_poi = pdata2, pal_fill = palette_decele_va){
  pdata3 <- pdata1 %>% dplyr::group_by(Y,R, Ref, Approach, policy, SSP, exemption, target, tech, Gini) %>% dplyr::reframe(value = sum(value))
  # we cannot add it up because this percentage is taken on sectoral value-added
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    geom_hline(data = pdata3, mapping = aes(yintercept = value), color = "grey50") +
    geom_col(df_col, mapping = aes(x = SCO2_S, y = value, fill = decele), position = "stack", width = .5) +
    geom_point(df_poi, mapping = aes(x = SCO2_S, y = value_sum), color = "black", shape = 18) + 
    labs(x = "Sector", y = "Percentage change | %") +
    guides(fill = guide_legend(title = "Factor")) +
    scale_fill_manual(values = pal_fill) +
    MyTheme + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
  
  return(p)
}


# 1.1 the value-added approach --------------------------------------------

palette_decele_va <- c("output" = '#0081a7', 
                    "va_output" =  '#f07171', 
                    "residual2" = '#ffe1b1')


# palette_decele_va <- c("output" = '#0096c7', 
#                        "va_output" =  '#48cae4', 
#                        "residual2" = '#caf0f8')

## 1.1.1 global ------------------------------------------------------------
pdata1 <- df_loss_dcp %>% filter(Approach == "value-added", Y == "2030", SSP == "SSP2", R == "World", Gini == "consistent", tech == "None", target == "1.5C", exemption == "None") %>% 
  mutate(value =  get(str_glue("{txt_dcp}")) * 100)

if( !("SCO2_S" %in% names(pdata1))){
  pdata1 <- pdata1 %>% dplyr::rename("SCO2_S" = "AggSecCO2") 
}

pdata1 <- pdata1 %>% left_join(df_va_ratio) %>%   left_join(df_gdp) %>% mutate(value = value * va_ratio * gdp_ratio)

colnames(pdata1)



pdata2 <- pdata1 %>% group_by(Y, R, SCO2_S, Approach, policy, SSP, exemption, target) %>% reframe(value_sum = sum(value))


# positive values show gains and negative values show losses
p <- F_plot_dcp(pdata1, pdata2) + facet_grid(~policy)
p
ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/value_added_approach_global.pdf"), 
       width = 14, height = 8, units = "cm")
rm(pdata1, pdata2, p)



## 1.1.2 regional------------------------------------------------------------
pdata1 <- df_loss_dcp %>% filter(Approach == "value-added", Y == "2030", SSP == "SSP2", exemption == "None", Gini == "consistent", tech == "None", R %in% lis_R, target == "1.5C") %>% 
  mutate(value = get(str_glue("{txt_dcp}"))*100) 

if( !("SCO2_S" %in% names(pdata1))){
  pdata1 <- pdata1 %>% dplyr::rename("SCO2_S" = "AggSecCO2") 
}

pdata2 <- pdata1 %>% group_by(Y, R, SCO2_S, Approach, policy, SSP, exemption, target) %>% reframe(value_sum = sum(value))
p <- F_plot_dcp(pdata1, pdata2) + facet_wrap(R~policy, ncol = 4)

ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/value_added_approach_regional.pdf"), 
       width = 24, height = 24, units = "cm")
rm(pdata1, pdata2, p)     








# 2. Expenditure approach -------------------------------------------------

palette_decele_fd <- c("output_va" = '#81b29a', 
                       "fd_output" =  '#e07a5f',
                       "va" = "#0081a7",
                       "residual1" = '#ffe1b1')



## 2.1 Global --------------------------------------------------------------

pdata1 <- df_loss_dcp %>% filter(Approach == "demand", Y == "2030", SSP == "SSP2", R == "World") %>%  mutate(value = get(str_glue("{txt_dcp}"))*100) 
pdata2 <- pdata1 %>% group_by(Y, R, SCO2_S, Approach, policy, SSP, exemption, target) %>% reframe(value_sum = sum(value))
p <- F_plot_dcp(pdata1, pdata2, pal_fill = palette_decele_fd) + facet_grid(~policy)
p

ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/demand_approach_global.pdf"), 
       width = 16, height = 10, units = "cm")





## 2.2 regional --------------------------------------------------------------
pdata1 <- df_loss_dcp %>% filter(Approach == "demand", Y == "2030", SSP == "SSP2", R %in% lis_R) %>%  mutate(value = get(str_glue("{txt_dcp}"))*100) 
pdata2 <- pdata1 %>% group_by(Y, R, SCO2_S, Approach, policy, SSP, exemption, target) %>% reframe(value_sum = sum(value))
p <- F_plot_dcp(pdata1, pdata2, pal_fill = palette_decele_fd) + facet_wrap(R~policy, ncol = 4)
p

ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/demand_approach_regional.pdf"), 
       width = 24, height = 24, units = "cm")








# 3. Capital productivity -------------------------------------------------
# capital productivity = capital input (sectoral)/output (sectoral)
txt_decele_output <- "va"

## 3.1 load data -------------------------------------------------
# sectoral output
df_output <- cge_ana %>% rgdx.param("VA_decomp_ind") %>% left_join(MapScenario) %>% filter(!is.na(Ref), decele == txt_decele_output) %>% F_woc() %>% select(-SCENARIO)%>% dplyr::rename("output" = VA_decomp_ind) %>% select(-decele)


df_va <- cge_ana %>% rgdx.param("VA_decomp_ind") %>% left_join(MapScenario) %>% filter(!is.na(Ref), decele == "va") %>% F_woc() %>% select(-SCENARIO) %>% dplyr::rename("va" = VA_decomp_ind) %>% select(-decele)

a <- df_va %>% dplyr::rename(value = va) %>% F_cha_decom() 
a[a$SCO2_S == "CCS",]$`Baseline_None` <- 0
a <- a %>% dplyr::group_by(Y, R, SSP, policy, exemption) %>% mutate(change = (value-Baseline_None)/sum(Baseline_None)*100, contribution = sum(change), va_tot = sum(Baseline_None)) %>% filter(Y == "2030")
colnames(a)
df_va_check <- a
rm(a)




# labor and capital input
# tax and annual price
df_sam_value <- paste0(dir_cgeoutput, "cbnal0.gdx") %>% rgdx.param("PSAM_value") %>% 
  gdata::rename.vars(colnames(.), c("SCENARIO", "Y", "R", "A1", "A2", "Value")) %>% 
  mutate(SCENARIO = gsub("global_17_", "", SCENARIO)) %>% left_join(MapScenario) %>% filter(!is.na(Ref), Y %in% seq(2005, 2100,5))


### base year price (no tax)
# df_sam_volume <- paste0(dir_cgeoutput, "cbnal0.gdx") %>% rgdx.param("PSAM_volume") %>% 
#   gdata::rename.vars(colnames(.), c("SCENARIO", "Y", "R", "A1", "A2", "Volume")) %>% 
#   mutate(SCENARIO = gsub("global_17_", "", SCENARIO)) %>% left_join(MapScenario) %>% filter(!is.na(Ref), Y %in% seq(2005, 2100,5))
# 
# df_sam_price <- paste0(dir_cgeoutput, "cbnal0.gdx") %>% rgdx.param("PSAM_price") %>% 
#   gdata::rename.vars(colnames(.), c("SCENARIO", "Y", "R", "A1", "A2", "Price")) %>% filter(Y == "2005") %>% select(-Y) %>% 
#   mutate(SCENARIO = gsub("global_17_", "", SCENARIO)) %>% left_join(MapScenario) %>% filter(!is.na(Ref))
# 
# df_sam_value <- df_sam_volume %>% left_join(df_sam_price) %>% mutate(Value = Volume * Price) %>% select(-Volume, -Price)
# 
# rm(df_sam_price, df_sam_volume)
###

# reform it into matrix for better understanding
df_matrix <- df_sam_value %>% select(-SCENARIO) %>% filter(Ref %in% c("1p5C", "1p5C_woc", "Base"), Y == "2030") %>% pivot_wider(names_from = "A2", values_from = "Value") 

# lab and capital input and their input efficiency
df_cap_lab <- df_sam_value %>% select(-SCENARIO) %>% filter(Ref %in% c("1p5C", "1p5C_woc", "Base"), Y %in% c("2030", "2050"), A1 %in% c("CAP", "LAB")) %>% 
  # pivot_wider(names_from = "A1", values_from = "Value") %>% 
  dplyr::rename("A" = "A2") %>% left_join(MCO2_S2) %>% filter(!is.na(SCO2_S), SCO2_S != "OTH") %>% 
  group_by(Y, R, A1, SCO2_S, Ref) %>% reframe(value = sum(Value)) %>%
  left_join(df_output) %>% mutate(eff = output/value) %>% left_join(MSCO2_S2S) %>% select(-SCO2_S)

colnames(df_cap_lab)
a <- df_cap_lab %>% mutate(a = eff * value/output)



df_cap_lab_global <- df_sam_value %>% select(-SCENARIO) %>% filter(Ref %in% c("1p5C", "1p5C_woc", "Base"), Y %in% c("2030", "2050"), A1 %in% c("CAP", "LAB")) %>% 
  # pivot_wider(names_from = "A1", values_from = "Value") %>% 
  dplyr::rename("A" = "A2")%>% left_join(MCO2_S2) %>% filter(SCO2_S != "OTH") %>% filter(!is.na(SCO2_S)) %>% 
  group_by(Y, R, A1, SCO2_S, Ref) %>% reframe(value = sum(Value)) %>%
  left_join(df_output) %>% 
  group_by(Y,SCO2_S,A1, policy, SSP, exemption, target) %>% reframe(output = sum(output), value = sum(value)) %>% 
  mutate(eff = output/value) %>% left_join(MSCO2_S2S) %>% select(-SCO2_S)
# rm(df_sam_value, df_matrix, df_output)

# change in the input efficiency
df_eff_change <- df_cap_lab %>% select(Y,R,S,A1,policy, SSP, exemption, target,eff) %>% dplyr::rename(value = "eff") %>% F_cha_decom()
df_eff_change[df_eff_change$S == "CCS",]$`Baseline_None` <- 0
df_eff_change <- df_eff_change %>% mutate(change = (value-Baseline_None)/Baseline_None*100)


df_eff_change_global <- df_cap_lab_global %>% select(Y,S,A1,policy, SSP, exemption, target,eff) %>% dplyr::rename(value = "eff") %>% F_cha_decom()
df_eff_change_global[df_eff_change_global$S == "CCS",]$`Baseline_None` <- 0
df_eff_change_global <- df_eff_change_global %>% mutate(change = (value-Baseline_None)/Baseline_None*100)

  




## 3.2 plot changes in sectoral factor productivities ----------------------------------------------------------------
## LAB ----
# regional
pdata <- df_eff_change %>% filter(Y == "2030", SSP == "SSP2", !is.na(change), A1 == "LAB")
p <- ggplot(pdata) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_col(aes(x = R, y = change, fill = policy), position = "dodge", width = .7) +
  facet_wrap(~S) + labs(x = "Region", y = "Percentage change in labor productivity") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
  scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Climate policy"))
p

ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/LAB_eff_change_sector_regional_",txt_decele_output,".pdf"), 
       width = 24, height = 16, units = "cm")


# global
pdata <- df_eff_change_global %>% filter(Y %in% c("2030", "2050"), SSP == "SSP2", !is.na(change), A1 == "LAB")
p <- ggplot(pdata) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_col(aes(x = S, y = change, fill = policy), position = "dodge", width = .7) +
  facet_wrap(~Y) +
  MyTheme + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) + 
  labs(x = "Region", y = "Percentage change in labor productivity") +
  scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Climate policy"))
p

ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/LAB_eff_change_sector_global_",txt_decele_output,".pdf"), 
       width = 16, height = 8, units = "cm")





## CAP ----
# regional
pdata <- df_eff_change %>% filter(Y == "2030", SSP == "SSP2", !is.na(change), A1 == "CAP")
p <- ggplot(pdata) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_col(aes(x = R, y = change, fill = policy), position = "dodge", width = .7) +
  facet_wrap(~S) + labs(x = "Region", y = "Percentage change in capital productivity") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
  scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Climate policy"))
p

ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/CAP_eff_change_sector_regional_",txt_decele_output,".pdf"), 
       width = 24, height = 16, units = "cm")




# global
pdata <- df_eff_change_global %>% filter(Y %in% c("2030", "2050"), SSP == "SSP2", !is.na(change), A1 == "CAP")
p <- ggplot(pdata) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_col(aes(x = S, y = change, fill = policy), position = "dodge", width = .7) +
  facet_wrap(~Y) +
  MyTheme + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) + 
  labs(x = "Region", y = "Percentage change in labor productivity") +
  scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Climate policy"))
p

ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/CAP_eff_change_sector_global_",txt_decele_output,".pdf"), 
       width = 16, height = 8, units = "cm")






# #### New data, detailed sector definitions based on the SAM ----------------------------------------------------------------
# # output 
# # df_output <- cge_ana %>% rgdx.param("VA_decomp_ind") %>% left_join(MapScenario) %>% filter(!is.na(Ref), decele == txt_decele_output) %>% F_woc() %>% select(-SCENARIO)%>% dplyr::rename("output" = VA_decomp_ind) %>% select(-decele)
# 
# # value added 
# df_va <- cge_ana %>% rgdx.param("GDP_psi") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% F_woc() %>% select(-SCENARIO) %>% dplyr::rename("va" = GDP_psi, "S" = A) 
# 
# # lab and capital input and their input efficiency
# df_cap_lab <- df_sam_value %>% select(-SCENARIO) %>% filter(Ref %in% c("1p5C", "1p5C_woc", "Base"), Y %in% c("2030", "2050"), A1 %in% c("CAP", "LAB")) %>% 
#   dplyr::rename("S" = "A2", "value" = Value) %>% # left_join(MCO2_S2) %>% filter(!is.na(SCO2_S)) %>% 
#   # group_by(Y, R, A1, SCO2_S, Ref) %>% reframe(value = sum(Value)) %>%
#   left_join(df_va) %>% mutate(eff = va/value) %>% filter(!is.na(policy))
# 
# 


## 3.3 plot percentage contribution to GDP -----------------------------------------------
df_va_s_base <- df_va %>% left_join(MSCO2_S2S) %>% select(-SCO2_S) %>% 
  filter(policy == "Baseline") %>% 
  group_by(Y, R,  SSP, exemption) %>%
  reframe(S = S, va_base = va) 

df_va_base <- df_va %>% left_join(MSCO2_S2S) %>% select(-SCO2_S) %>% 
  filter(policy == "Baseline") %>% 
  group_by(Y, R,  SSP, exemption, Ref) %>%
  reframe(va_tot_base = sum(va)) 
colnames(df_va_base)


## labor, sectoral contribution ----
df_lab_cap_base <- df_cap_lab %>% 
  filter(policy == "Baseline") %>% 
  select(Y,R,S,A1,SSP, exemption, value) %>% dplyr::rename("factor_base" = value)


df_lab_cap_contribution <- df_cap_lab %>% select(Y,R,S,A1,policy, SSP, exemption, target,eff) %>% dplyr::rename(value = "eff") %>% F_cha_decom() 
df_lab_cap_contribution[df_lab_cap_contribution$S == "CCS",]$`Baseline_None` <- 0
df_lab_cap_contribution <- df_lab_cap_contribution %>%  mutate(change_eff = (value-Baseline_None)) %>% 
  left_join(df_lab_cap_base) %>% full_join(df_va_s_base) 


df_lab_cap_contribution[df_lab_cap_contribution$S == "CCS",]$`factor_base` <- 0
df_lab_cap_contribution[df_lab_cap_contribution$S == "CCS",]$`va_base` <- 0

df_lab_cap_contribution <- df_lab_cap_contribution %>% left_join(df_va_base) %>% 
  mutate(x = (change_eff * factor_base), contribution = (change_eff * factor_base)/va_tot_base*100, a = Baseline_None * factor_base/va_base)




# regional, LAB
pdata <- df_lab_cap_contribution %>% filter(Y == "2030", A1 == "LAB")
p <- ggplot(pdata) +
  geom_col(aes(x = R, y = contribution, fill = S)) +
  facet_wrap(~policy) + MyTheme
p
ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/LAB_eff_sector_contribution_",txt_decele_output,".pdf"), 
       width = 30, height = 8, units = "cm")


# regional, CAP
pdata <- df_lab_cap_contribution %>% filter(Y == "2030", A1 == "CAP")
colnames(pdata)
p <- ggplot(pdata) +
  geom_col(aes(x = R, y = contribution, fill = S)) +
  facet_wrap(~policy) + MyTheme
p

ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/CAP_eff_sector_contribution_",txt_decele_output,".pdf"), 
       width = 30, height = 8, units = "cm")






# 3.000. sectoral contribution as calculated in Fujimori-sensei's NC paper ----

# sectoral output
df_nc_output <- cge_ana %>% rgdx.param("VA_decomp_ind") %>% left_join(MapScenario) %>% filter(!is.na(Ref), decele == "output") %>% F_woc() %>% select(-SCENARIO) %>% 
  dplyr::rename("output"=VA_decomp_ind) %>% select(-decele)

df_nc_va <- cge_ana %>% rgdx.param("VA_decomp_ind") %>% left_join(MapScenario) %>% filter(!is.na(Ref), decele == "va", SCO2_S != "OTH") %>% F_woc() %>% select(-SCENARIO) %>% 
  dplyr::rename("va"=VA_decomp_ind) %>% select(-decele)  %>% left_join(MSCO2_S2S) %>% select(-SCO2_S) %>% 
  dplyr::group_by(Y,R,Ref,policy, SSP, exemption, target) %>% 
  dplyr::reframe(S = S,va = va, va_tot = sum(va))



df_nc_cap_lab_global <- df_sam_value %>% select(-SCENARIO) %>% filter(Ref %in% c("1p5C", "1p5C_woc", "Base"), Y %in% c("2030", "2050"), A1 %in% c("CAP", "LAB")) %>% 
  # pivot_wider(names_from = "A1", values_from = "Value") %>% 
  dplyr::rename("A" = "A2")%>% left_join(MCO2_S2) %>% filter(!is.na(SCO2_S), SCO2_S != "OTH") %>% 
  group_by(Y, R, SCO2_S, Ref, A1) %>% reframe(value = sum(Value)) %>% 
  left_join(df_nc_output) %>% 
  group_by(Y,SCO2_S, policy, A1, SSP, exemption, target) %>% reframe(output = sum(output), value = sum(value)) %>% 
  mutate(eff = output/value) %>% left_join(MSCO2_S2S) %>% select(-SCO2_S)


# change in the input efficiency

df_nc_eff_change_global <- df_cap_lab %>% select(Y,R,S,A1,policy, SSP, exemption, target,eff) %>% dplyr::rename(value = "eff") %>% F_cha_decom() 
df_nc_eff_change_global[df_nc_eff_change_global$S == "CCS",]$`Baseline_None` <- 0
df_nc_eff_change_global <- df_nc_eff_change_global %>% filter(S != "CCS") %>% mutate(change = (value-Baseline_None)/Baseline_None) %>% 
  left_join(df_nc_va) %>% mutate(contribution = change * va/va_tot)



pdata <- df_nc_eff_change_global %>% filter(Y == "2030", SSP == "SSP2")
colnames(pdata)
p <- ggplot(pdata) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_col(aes(x = R, y = contribution*100, fill = S)) +
  labs(x = "Region", y = "Contributions to GDP change | %") +
  facet_wrap(A1~policy) + MyTheme
p

ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/FS_eff_sector_contribution_va.pdf"), 
       width = 30, height = 20, units = "cm")











rm(df_nc_eff_change_global, df_nc_va, df_nc_output, df_nc_cap_lab_global,df_va, df_va_base, df_va_check, df_va_s_base, df_nc_cap_lab_eff, df_nc_cap_change_global, df_nc_lab_change_global)
rm(df_lab_cap_base, df_lab_cap_contribution, df_lab_change, df_lab_change_global, df_lab_contri, df_lab_contribution, df_cap_change, df_cap_change_global, df_cap_contribution, df_cap_lab, df_cap_lab_global)
rm(df_eff_change, df_eff_change_global, df_loss_dcp, df_matrix)
