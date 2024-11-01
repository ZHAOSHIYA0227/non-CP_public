# This program shows the energy system structure 
# Shiya ZHAO, 2024.03.19

# 0. maps and lists --------------------------------------------------------

# 0. data --------------------------------------------------------
df_iamc <- cge_var %>%
  rgdx.param("IAMC_Template") %>% left_join(MapScenario) %>% filter(!is.na(Ref), !grepl("Base_woc", Ref)) %>% select(-SCENARIO) %>% 
  gdata::rename.vars(colnames(.), c("R", "VEMF", "Y", "value", "Ref")) %>% F_woc()

df_comprice_noctx <- cge_ana %>%
  rgdx.param("Domestic_Price") %>%
  gdata::rename.vars(colnames(.), c("SCENARIO", "Y", "R", "COM", "price")) %>% 
  left_join(MapScenario)  %>% left_join(map_com) %>% filter(!is.na(Ref), !is.na(I), Y != "2005") %>% select(Y,R,COM,I,price,Ref) %>% F_filter(s_Baseline_woc = T, lis_R= c(unique(map_R17$R_CGE))) %>% 
  F_woc() %>% mutate(ctx = "No")

df_comprice_ctx <- cge_ana %>%
  rgdx.param("Domestic_Price_ctx") %>%
  gdata::rename.vars(colnames(.), c("SCENARIO", "Y", "R", "COM", "price")) %>% 
  left_join(MapScenario)  %>% left_join(map_com) %>% filter(!is.na(Ref), !is.na(I), Y != "2005") %>% select(Y,R,COM,I,price,Ref) %>% F_filter(s_Baseline_woc = T, lis_R= c(unique(map_R17$R_CGE))) %>% 
  F_woc() %>% mutate(ctx = "Yes")


df_comprice <- df_comprice_noctx %>% rbind(df_comprice_ctx) %>% left_join(map_comname) %>% dplyr::rename(R_CGE = "R") %>% F_R17full() %>%  dplyr::rename(R = "R_CGE")

rm(df_comprice_ctx, df_comprice_noctx)


# 1 plot goods regional temporal trends ----

dir.create(paste0(dir_output, "/fig/Figure1_2 Price change in CGE/"))
lis_R_loop <- unique(df_comprice$R)
lis_bool <- c("Yes", "No")
for(i in 1:length(lis_bool)){
  for(r in 1:length(lis_R_loop)){
    pdata <- df_comprice %>% filter(R == lis_R_loop[r], ctx == lis_bool[i], target != "2C") %>% F_filter_main() %>% mutate(v_group = paste0(SSP, policy, ctx)) %>% 
      select(-Ref) %>% dplyr::rename(value = "price") %>% select(-"I") %>% distinct()
    p <- F_plot_ribbon(pdata, lab_y = "Price index") + facet_wrap(~COM_name) + theme(axis.text.x = element_text(angle = 90))
    p
    ggsave(p, filename = paste0(dir_output, "/fig/Figure1_2 Price change in CGE/", lis_R_loop[r], "_ctx", lis_bool[i], ".pdf"), 
           width = 40, height = 40, units = "cm")
  }
}

rm(lis_R_loop)

# 2. energy goods: decomposition ----
pdata0 <- df_comprice %>% F_filter_main() %>% filter(I == "Energy") %>% dplyr::rename("value" = "price") %>% F_cha_decom() %>% 
  mutate(change = (value-Baseline_None)/Baseline_None*100) %>% select(R,Y,COM_name, SSP, policy, exemption, change, ctx) %>% 
  pivot_wider(names_from = "ctx", values_from = "change") %>% 
  mutate(`Additional cost` = `No`, `Carbon tax` = (`Yes` - `No`)) %>% select(Y, R, policy, SSP, exemption, COM_name, `Additional cost`, `Carbon tax`) %>% 
  pivot_longer(cols = c("Additional cost", "Carbon tax"), names_to = "Component", values_to = "change") # %>% 
  
pdata1 <- pdata0 %>% pivot_wider(names_from = "policy", values_from = "change") %>% filter(Component == "Additional cost") %>% mutate(`Indirect tax` = `1.5C CP` - `1.5C non-CP`, `Additional cost` = `1.5C non-CP`, policy = "1.5C CP") %>% 
  select(Y,R,SSP,policy, exemption, COM_name, "Indirect tax", "Additional cost") %>% 
  pivot_longer(cols = c("Indirect tax", "Additional cost"), names_to = "Component", values_to = "change")
  
pdata2 <- pdata0 %>% filter(Component == "Carbon tax" | policy == "1.5C non-CP") %>% 
  rbind(pdata1) %>% mutate(Component = factor(Component, levels = c("Additional cost", "Carbon tax", "Indirect tax")))

rm(pdata0, pdata1)

## 2.1 plot decompostion: fixed year column----
dir.create(paste0(dir_output, "/fig/Figure1_2 Price change in CGE/energy_SSP2/"))
lis_R_loop <- unique(df_comprice$R)
lis_Y_loop <- c(2030, 2050, 2070)
for(y in 1:length(lis_Y_loop)){
  for(r in 1:length(lis_R_loop)){
    pdata <- pdata2 %>% filter(SSP == "SSP2", Y == lis_Y_loop[y], R == lis_R_loop[r], !grepl("2C", policy), exemption == "None") 
    p <- ggplot(pdata) + 
      geom_hline(yintercept = 0, color = "grey") +
      geom_col(aes(x = gsub('1.5C','', policy), y = change, fill = fct_rev(Component)), width = 0.7) +
      labs(x  = "Policy", y = "Price change to Baseline (%)", subtitle = lis_R_loop[r]) +
      facet_wrap(~COM_name, ncol = 4) + MyTheme + theme(axis.text.x = element_text(angle = 0)) + scale_fill_manual(values = palette_color_component) + guides(fill = guide_legend(title = "Component"))
    p
    ggsave(p, filename = paste0(dir_output, "/fig/Figure1_2 Price change in CGE/energy_SSP2/", lis_R_loop[r], "_", lis_Y_loop[y], ".pdf"), 
           width = 28, height = 14, units = "cm")
  }

}


# 2.2 Decomposition: temporal trends --------------------------------------
dir.create(paste0(dir_output, "/fig/Figure1_2 Price change in CGE/energy_SSP2_area/"))
for(r in 1:length(lis_R_loop)){
  pdata <- pdata2  %>% filter(SSP == "SSP2", R == lis_R_loop[r], policy == "1.5C CP", exemption == "None") 
  p <- ggplot(pdata) + 
    geom_hline(yintercept = 0, color = "grey") +
    geom_area(aes(x = as.numeric(as.character(Y)), y = change, fill = fct_rev(Component))) +
    labs(x  = "Year", y = "Price change to Baseline (%)", subtitle = lis_R_loop[r]) +
    facet_wrap(~COM_name, ncol = 4, scales = "free") + MyTheme + theme(axis.text.x = element_text(angle = 0)) + scale_fill_manual(values = palette_color_component) + guides(fill = guide_legend(title = "Component"))
  ggsave(p, filename = paste0(dir_output, "/fig/Figure1_2 Price change in CGE/energy_SSP2_area/", lis_R_loop[r],".pdf"), 
         width = 28, height = 14, units = "cm")
}
rm(lis_R_loop)

# 3. agriculture sector -------------------------------------------------

## 3.0 Price change ----------------------------------------------------------
pdata0 <- df_comprice %>% F_filter_main() %>% filter(I == "Food and nonalcoholic beverages", ctx == "Yes", !grepl("2C", policy)) %>% dplyr::rename("value" = "price") 
dir.create(paste0(dir_output, "/fig/Figure1_2 Price change in CGE/agriculture/"))
lis_R_loop <- unique(df_comprice$R)
  
for(r in 1:length(lis_R_loop)){
    pdata <- pdata0 %>% filter(R == lis_R_loop[r]) %>% mutate(v_group = paste0(SSP, policy, ctx)) %>% select(-Ref) %>% select(-"I") %>% distinct()
    p <- F_plot_ribbon(pdata, lab_y = "Price index") + facet_wrap(~COM_name) + theme(axis.text.x = element_text(angle = 90))
    ggsave(p, filename = paste0(dir_output, "/fig/Figure1_2 Price change in CGE/agriculture/", lis_R_loop[r], ".pdf"), 
           width = 30, height = 16, units = "cm")
}

rm(lis_R_loop)

# The most drastic price change occurs in livestock related consumptions and vegetable and fruits
lis_variable_loop <- c("Ruminant livestock", "Raw milk", "Non ruminant livestock, other livestock and fishery", "Vegetable, fruits and nuts")
dir.create(paste0(dir_output, "/fig/Figure1_2 Price change in CGE/agriculture_SSP2/"))

lis_Y_loop <- c("2030", "2050", "2070")
for(y in 1:length(lis_Y_loop)){
  
  pdata <- df_comprice %>% filter(Y == lis_Y_loop[y], COM_name %in% lis_variable_loop, R %in% lis_R17, SSP == "SSP2", !grepl("2C", policy))%>% dplyr::rename("value" = "price") 

  p <- ggplot(pdata) +
    geom_col(aes(x = R, y = value, fill = policy), position = "dodge", width = 0.7) +
    facet_wrap(~COM_name, ncol = 1) +
    labs(x = "Region", y = "Price index", subtitle = lis_Y_loop[y]) +
    MyTheme + theme(axis.text.x = element_text(angle = 0)) + scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Scenario"))
  p
  ggsave(p, filename = paste0(dir_fig, "Figure1_2 Price change in CGE/agriculture_SSP2/", lis_Y_loop[y],".pdf"), units = "cm", 
         width = 20, height = 16)
  }


## 3.1 Reduction cost ----------------------------------------------------------
# Reduction costs
# load data

map_VEMF_redcos <- openxlsx::read.xlsx(paste0(dir_def,"/IAMC_VEMF.xlsx"), sheet = "map_reductioncost")
df_redcos <- df_iamc %>% left_join(map_VEMF_redcos) %>% select(-c("VEMF", "VIAMC")) %>% filter(!is.na(Variable)) %>% dplyr::rename(R_CGE = "R") %>% F_R17full() %>%  dplyr::rename(R = "R_CGE")

### 3.1.1 plot reduction cost all years ----
dir.create(paste0(dir_fig, "Figure1_2_Reduction cost/"))
lis_R_loop <- unique(df_redcos$R)
for(r in 1:length(lis_R_loop)){
  pdata <- df_redcos %>% filter( R == lis_R_loop[r], !grepl("2C", policy), exemption == "None") %>% dplyr::group_by(R, Y, policy, SSP, exemption, Variable_type) %>% dplyr::reframe(value = sum(value))
  p <- ggplot(pdata) +
    # geom_col(aes(x = Y, y = value, fill = Variable_type), position = "stack") +
    geom_area(aes(x = as.numeric(as.character(Y)), y = value, fill = Variable_type), position = "stack") +
    facet_wrap(SSP~policy, ncol = 2) + labs(x = "Year", y = "Reduction cost (million USD 2010/yr)", subtitle = lis_R_loop[r]) +
    MyTheme + theme(axis.text.x = element_text(angle = 90))
  p
  ggsave(p, filename = paste0(dir_fig, "Figure1_2_Reduction cost/", lis_R_loop[r],".pdf"), units = "cm", 
         width = 20, height = 20)
}

# The global area plot in SSP2
pdata <- df_redcos %>% filter( R == "World", SSP == "SSP2",  !grepl("2C", policy), exemption == "None") %>% dplyr::group_by(R, Y, policy, SSP, exemption, Variable_type) %>% dplyr::reframe(value = sum(value))
p <- ggplot(pdata) +
  # geom_col(aes(x = Y, y = value, fill = Variable_type), position = "stack") +
  geom_area(aes(x = as.numeric(as.character(Y)), y = value, fill = Variable_type), position = "stack") +
  facet_wrap(SSP~policy, ncol = 2) + labs(x = "Year", y = "Reduction cost (million USD 2010/yr)", subtitle = "World") +
  MyTheme + theme(axis.text.x = element_text(angle = 0))
p
ggsave(p, filename = paste0(dir_fig, "Figure1_2_Reduction cost_world.pdf"), units = "cm", 
       width = 20, height = 8)
rm(p)
# => the reduction cost in agriculture is the main contributor to the total reduction cost and to the difference between the CP and nonCP scenarios
# => we need to see the inside the cost of agriculture sector



### 3.1.2 plot reduction cost in the agriculture sector SSP2 by country ----
dir.create(paste0(dir_fig, "Figure1_2_Reduction cost/SSP2/"))
lis_R_loop <- unique(df_redcos$R)
for(r in 1:length(lis_R_loop)){
  pdata <- df_redcos %>% filter( R == lis_R_loop[r],  Y %in% c(2030, 2050), SSP == "SSP2", exemption == "None") # Variable_type == "Agriculture",
  p <- ggplot(pdata) +
    geom_col(aes(x = Variable, y = value, fill = policy), position = "dodge", width = 0.7) +
    facet_wrap(~Y, ncol = 2) + labs(x = "Variable", y = "Reduction cost (million USD 2010/yr)", subtitle = lis_R_loop[r]) +
    MyTheme + theme(axis.text.x = element_text(angle = 0)) + scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Scenario"))
  p
  ggsave(p, filename = paste0(dir_fig, "Figure1_2_Reduction cost/SSP2/", lis_R_loop[r],".pdf"), units = "cm", 
        width = 40, height = 10)
  rm(p)
}


### 3.1.3 plot reduction cost in the agriculture sector SSP2 by time----
dir.create(paste0(dir_fig, "Figure1_2_Reduction cost/agriculture_year/"))
lis_Y_loop <- c("2030", "2050", "2070")
for(y in 1:length(lis_Y_loop)){
  pdata <- df_redcos %>% filter(Y == lis_Y_loop[y], Variable_type == "Agriculture", R %in% lis_R17, SSP == "SSP2",  exemption == "None") 
  p <- ggplot(pdata) +
    geom_col(aes(x = R, y = value, fill = policy), position = "dodge", width = 0.7) +
    facet_wrap(~Variable, ncol = 1) +
    labs(x = "Region", y = "Reduction cost (million USD 2010/yr)", subtitle = lis_Y_loop[y]) +
    MyTheme + theme(axis.text.x = element_text(angle = 0)) + scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Scenario"))
  p
  ggsave(p, filename = paste0(dir_fig, "Figure1_2_Reduction cost/agriculture_year/", lis_Y_loop[y],".pdf"), units = "cm", 
         width = 20, height = 15)
  rm(p)
}




## 3.2 land cover ----------
# "million ha/yr"
map_VEMF_lancov <- openxlsx::read.xlsx(paste0(dir_def,"/IAMC_VEMF.xlsx"), sheet = "map_landcover")
df_lancov <- df_iamc %>% left_join(map_VEMF_lancov) %>% select(-c("VEMF", "VIAMC")) %>% filter(!is.na(Variable)) %>% dplyr::rename(R_CGE = "R") %>% F_R17full() %>%  dplyr::rename(R = "R_CGE")

### 3.2.1 plot land cover all years ----
dir.create(paste0(dir_fig, "Figure1_2_Land cover/"))
lis_R_loop <- unique(df_lancov$R)
for(r in 1:length(lis_R_loop)){
  pdata <- df_lancov %>% filter( R == lis_R_loop[r], exemption == "None") %>% dplyr::group_by(R, Y, policy, SSP, exemption, Variable_type) %>% dplyr::reframe(value = sum(value))
  p <- ggplot(pdata) +
    # geom_col(aes(x = Y, y = value, fill = Variable_type), position = "stack") +
    geom_area(aes(x = as.numeric(as.character(Y)), y = value, fill = Variable_type), position = "stack") +
    facet_wrap(SSP~policy, ncol = 3) + labs(x = "Year", y = "Land cover (million ha/yr)", subtitle = lis_R_loop[r]) +
    MyTheme + theme(axis.text.x = element_text(angle = 90))
  p
  ggsave(p, filename = paste0(dir_fig, "Figure1_2_Land cover/", lis_R_loop[r],".pdf"), units = "cm", 
         width = 30, height = 20)
}


### 3.2.2 plot reduction cost in the agriculture sector ----
dir.create(paste0(dir_fig, "Figure1_2_Land cover/SSP2/"))
lis_R_loop <- unique(df_lancov$R)
for(r in 1:length(lis_R_loop)){
  pdata <- df_lancov %>% filter( R == lis_R_loop[r], Y %in% c(2030, 2050), SSP == "SSP2", exemption == "None")
  p <- ggplot(pdata) +
    geom_col(aes(x = Variable, y = value, fill = policy), position = "dodge", width = 0.7) +
    facet_wrap(~Y, ncol = 1) + labs(x = "Land use", y = "Land cover (million ha/yr)", subtitle = lis_R_loop[r]) +
    MyTheme + theme(axis.text.x = element_text(angle = 90)) + scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Scenario"))
  p
  ggsave(p, filename = paste0(dir_fig, "Figure1_2_Land cover/SSP2/", lis_R_loop[r],".pdf"), units = "cm", 
         width = 16, height = 14)
}

### 3.2.3 plot land cover change across regions ----

dir.create(paste0(dir_fig, "Figure1_2_Land cover/SSP2_year/"))
lis_Y_loop <- c("2030", "2050", "2070")
rm(lis_variable_loop)

lis_variable_loop <- c("Pasture", "Forests","Non-Energy Crops") 

for(y in 1:length(lis_Y_loop)){
  pdata <- df_lancov %>% filter(Y == lis_Y_loop[y], Variable %in% lis_variable_loop, R %in% lis_R17, SSP == "SSP2", exemption == "None")
  p <- ggplot(pdata) +
    geom_col(aes(x = R, y = value, fill = policy), position = "dodge", width = 0.7) +
    facet_wrap(~Variable, ncol = 2) +
    labs(x = "Region", y = "Land cover (million ha/yr)", subtitle = lis_Y_loop[y]) +
    MyTheme + theme(axis.text.x = element_text(angle = 90)) + scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Scenario"))
  p
  ggsave(p, filename = paste0(dir_fig, "Figure1_2_Land cover/SSP2_year/", lis_Y_loop[y],".pdf"), units = "cm", 
         width = 24, height = 10)
}

pdata <- df_lancov %>% filter(Y %in% c(2030, 2050), Variable %in% lis_variable_loop, R %in% lis_R17, SSP == "SSP2", exemption == "None")
p <- ggplot(pdata) +
  geom_col(aes(x = R, y = value, fill = policy), position = "dodge", width = 0.7) +
  facet_wrap(Variable~Y, ncol = 2) +
  labs(x = "Region", y = "Land cover (million ha/yr)") +
  MyTheme + theme(axis.text.x = element_text(angle = 90)) + scale_fill_manual(values = palette_color_cp) + guides(fill = guide_legend(title = "Scenario"))
p
ggsave(p, filename = paste0(dir_fig, "Figure1_2_Land cover/SSP2_year/20302050.pdf"), units = "cm", 
       width = 24, height = 20)
rm(p, pdata, lis_variable_loop, lis_Y_loop)





## 3.3 Land rent --------------------------------------------------------------------
df_lanrent <- cge_ana %>% rgdx.param("LandRent") %>% left_join(MapScenario) %>% F_woc() %>% F_filter_main() %>%  
  filter(!is.na(Ref), R %in% c(lis_R17, "World"), !grepl("Baseline_woc", Ref), target != "2C", exemption == "None") %>% 
  select(Y,R,policy, SSP, target, LandRent) %>% 
  dplyr::rename("value" = "LandRent", R_CGE = "R") %>% F_R17full()


pdata <- df_lanrent %>% mutate(v_group = paste0(policy, SSP))

p <- F_plot_ribbon(pdata, lab_y = "Land rent (million $ per 1000 ha)") + facet_wrap(~R_CGE, ncol = 4) + theme(axis.text.x = element_text(angle = 90))
p  
ggsave(p, filename = paste0(dir_fig, "Figure1_2_LandRent.pdf"), units = "cm", 
       width = 26, height = 20)

















# 4. PHI price ---------------------------------------------------------------

df_PQ <- paste0("../../DataArchive/PHIoutput/gdx/Inputdata.gdx") %>%
  rgdx.param("PriceChange") %>% dplyr::rename("value" = "PriceChange") %>% #gdata::rename.vars(colnames(.), c("R", "Ref", "Y", "R3", "I", "value")) %>% 
  left_join(MapI) %>% left_join(MapI) %>% mutate(I = factor(I_abb, levels = unique(MapI$I_abb))) %>% select(-I_abb) %>% filter(!is.na(I)) %>% 
  F_woc() %>% F_filter(lis_R = c( "R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")) %>% mutate(R_CGE = R) %>% F_filter_main()
  # transform(policy = factor(policy, levels = lis_cp)) %>%
  # left_join(map_R) 

lis_R_loop <- unique(df_PQ$R)
for(r in 1:length(lis_R_loop)){

  pdata <- df_PQ %>% filter(R_CGE == lis_R_loop[r], Y %in% c(2020,2030, 2050), exemption == "None") %>% 
    mutate( v_group = paste0(policy, SSP)) %>% select(-Ref) 
  
  
  p <- ggplot(pdata) + # F_plot_ribbon(pdata, lab_x = "Year", lab_y = expression(paste("%"))) +　theme(legend.position = "none") +
    geom_boxplot(aes(x = Y, y = value, color = policy)) + 
    geom_hline(yintercept = 1, color = "grey") +
    facet_wrap(~I) + MyTheme +
    theme(axis.text.x = element_text(angle = 60)) + Theme_tran +
    scale_color_manual(values = palette_color_cp)
  p

  
  ggsave(p, filename = paste0(dir_fig, "Figure1_3_PQ_",lis_R_loop[r],".pdf"), units = "cm", 
         width = 26, height = 20)
  
  
}

rm(lis_R_loop)





# 5. 2030 comparison of PHI prices across regions ---------------------------------------

pdata <- df_PQ %>% filter(Y %in% c(2030), exemption == "None") %>% 
  mutate( v_group = paste0(policy, SSP)) %>% select(-Ref) 
colnames(pdata)
df_PQ %>% filter(Y %in% c(2030, 2050), exemption == "None") %>% 
  mutate( v_group = paste0(policy, SSP)) %>% select(-Ref) %>% dplyr::group_by(R_CGE, Y, I, SSP, policy, exemption, target, Gini, tech) %>% dplyr::reframe(mean = mean(value), median = median( value )) %>% 
  openxlsx::write.xlsx(file = paste0(dir_csv, "1_2_PHI_Prices_SSP_regional.xlsx"))
df_PQ %>% filter(Y %in% c(2030, 2050), exemption == "None") %>% 
  mutate( v_group = paste0(policy, SSP)) %>% select(-Ref) %>% dplyr::group_by(R_CGE, Y, I, policy, exemption, target, Gini, tech) %>% dplyr::reframe(mean = mean(value), median = median( value )) %>%  
  openxlsx::write.xlsx(file = paste0(dir_csv, "1_2_PHI_Prices_regional.xlsx"))

p <- ggplot(pdata) + # F_plot_ribbon(pdata, lab_x = "Year", lab_y = expression(paste("%"))) +　theme(legend.position = "none") +
  geom_boxplot(aes(x = R_CGE, y = value, color = policy), outliers = F) + 
  geom_hline(yintercept = 1, color = "grey") +
  facet_wrap(~I) + MyTheme + labs(x = "Region", y = "Price") +
  theme(axis.text.x = element_text(angle = 60)) + Theme_tran +
  scale_color_manual(values = palette_color_cp) + guides(color = guide_legend(title = "Policy"))

p


ggsave(p, filename = paste0(dir_fig, "Figure1_3_PQ_2030_regions.pdf"), units = "cm", 
       width = 24, height = 16)






rm(p, pdata, df_PQ, df_lanrent, map_VEMF_lancov, df_lancov, df_redcos)
