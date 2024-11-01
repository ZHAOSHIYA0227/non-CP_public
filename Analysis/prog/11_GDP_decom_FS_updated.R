# GDP decomposition for non-CP paper 

# Shiya ZHAO, 2021.07.18


#==== Fujimori et al 2019 NC ====#
# 1. Decomposition analysis according to Fujimori et al 2019 NC --------
# Paper: Energy transformation cost for the Japanese mid-century strategy
# DOI: 10.1038/s41467-019-12730-4


sw_agg <- F
if(sw_agg == T){
  txt_dcp <- "Loss_dcp_agg_gdp"
}else{
  txt_dcp <- "Loss_dcp_gdp"
}


# load the variables calculated in AIMHub, adopting the value-added approach
df_va_decomp <- cge_ana %>% rgdx.param("VA_decomp_ind") %>% left_join(MapScenario) %>% filter(!is.na(Ref)) %>% select(-SCENARIO) %>% 
  dplyr::rename(value = "VA_decomp_ind") %>% F_woc() %>% filter(exemption == "None",  Y== "2030", decele %in% c("va", "va_output", "output"), target != "2C")

# keep the baseline variables for decomposition
df_va_base <- df_va_decomp %>% filter(policy == "Baseline") %>% select(-target, -Ref, -policy) %>% 
  pivot_wider(names_from = "decele", values_from = "value")

# the changes in each variable from baseline to mitigation
df_va_tot_base <- df_va_decomp %>% filter(policy == "Baseline", decele == "va") %>% select(-target, -Ref, -policy) %>% 
  dplyr::group_by(Y,R,SSP, tech, Gini, exemption) %>% reframe(va_sum = sum(value)) 

# change in value-added that are attributed to the changes in each variable
df_va_delta <- df_va_decomp %>% F_cha_decom() %>% 
  mutate(delta = case_when(SCO2_S != "CCS" ~ value - Baseline_None,
                           SCO2_S == "CCS" ~ value )) %>% select(-Baseline_None, -`Baseline non-CP_None`)

# percentage change in value-added that are attributed to the changes in each variable
df_va_cha <- df_va_delta %>% left_join(df_va_base) %>%
  mutate(change = case_when(decele == "va_output" ~ delta * output,
                            decele == "output" ~ delta * va_output,
                            decele == "va" ~ delta)) %>%
  select(-output, -va, -va_output, -delta, -value) %>% pivot_wider(names_from = "decele", values_from = "change") %>%
  mutate(residual = va - output - va_output)
df_va_cha[df_va_cha$SCO2_S == "CCS",c("output","va_output","residual")] <- 0
  # dplyr::group_by(Y,R,SSP, tech, Gini, policy, exemption) %>% mutate(va_sum = sum(va)) %>% ungroup() %>%
df_va_cha <- df_va_cha %>% left_join(df_va_tot_base) %>%
  pivot_longer(cols = c("output", "va_output", "residual","va"), values_to = "value", names_to = "decele") %>%
  mutate(change_rate = value/va_sum)



# define the plotting function
F_plot_dcp <- function(df_col = pdata1, df_poi = pdata2, pal_fill = palette_decele_va){
  pdata3 <- pdata1 %>% dplyr::group_by(Y,R,  policy, SSP, exemption, tech, Gini) %>% dplyr::reframe(value = sum(value))
  # we cannot add it up because this percentage is taken on sectoral value-added
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    geom_hline(data = pdata3, mapping = aes(yintercept = value), color = "grey50") +
    geom_col(df_col, mapping = aes(x = SCO2_S, y = value, fill = decele), position = "stack", width = .5) +
    geom_point(df_poi, mapping = aes(x = SCO2_S, y = value_sum), color = "black", shape = 18) + 
    labs(x = "Sector", y = "%") +
    guides(fill = guide_legend(title = "Factor")) +
    scale_fill_manual(values = pal_fill) +
    MyTheme + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
  
  return(p)
}


# 1.1 plot --------------------------------------------

palette_decele_va <- c("output" = '#0081a7', 
                       "va_output" =  '#f07171', 
                       "residual" = '#ffe1b1')



## 1.1.1 global ------------------------------------------------------------
pdata1 <- df_va_cha %>% filter(Y == "2030", SSP == "SSP2", R == "World", Gini == "consistent", tech == "None", grepl("1.5C", policy), exemption == "None", decele != "va") %>% 
  mutate(value =  change_rate * 100) %>% filter(SCO2_S != "CCS") %>% mutate(SCO2_S = factor(SCO2_S, levels = c("AGR", "IND", "TRS", "SER", "BIO", "FFE", "PWR", "OEN", "OTH")))

if( !("SCO2_S" %in% names(pdata1))){
  pdata1 <- pdata1 %>% dplyr::rename("SCO2_S" = "AggSecCO2") 
}

pdata2 <- pdata1 %>% group_by(Y, R, SCO2_S, policy, SSP, exemption) %>% reframe(value_sum = sum(value))


# positive values show gains and negative values show losses
p <- F_plot_dcp(pdata1, pdata2) + facet_grid(~policy)
ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/value_added_approach_global.pdf"), 
       width = 16, height = 8, units = "cm")
rm(pdata1, pdata2, p)



## 1.1.2 regional------------------------------------------------------------
pdata1 <- df_va_cha %>% filter(Y == "2030", SSP == "SSP2", exemption == "None", Gini == "consistent", tech == "None", R %in% lis_R, decele != "va") %>% 
  mutate(value = change_rate*100, R = factor(R, levels = lis_R)) %>% filter(R == "R5REF")

if( !("SCO2_S" %in% names(pdata1))){
  pdata1 <- pdata1 %>% dplyr::rename("SCO2_S" = "AggSecCO2") 
}

pdata2 <- pdata1 %>% group_by(Y, R, SCO2_S, policy, SSP, exemption) %>% reframe(value_sum = sum(value))
p <- F_plot_dcp(pdata1, pdata2) + facet_wrap(R~policy, ncol = 4)

ggsave(p, filename = paste0(dir_fig, "/Figure11 GDP decomp/value_added_approach_regional.pdf"), 
       width = 24, height = 24, units = "cm")
rm(pdata1, pdata2, p)     






