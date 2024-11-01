# This program is a summary of all the figures and plots in the paper 
# "Poverty and inequality in China under the long-term global temperature target"
# This program deals with price changes in AIMPHI and the consumption share in ExpData.gdx

# Shiya ZHAO, 2021/07/17



dir.create(paste0(dir_fig, "Figure12_Price_ConsumptionShare"))



# 1. Price change ---------------------------------------------------------

## load price data ----
PQchange_tmp <- paste0("../../DataArchive/PHIoutput/gdx/Inputdata.gdx") %>% rgdx.param("PriceChange") %>%
  left_join(MapI) %>%
  filter(!is.na(Ref),# R %in% lis_R,
         Y %in% c(seq(2020, 2100, 10))) %>% mutate(value = PriceChange) %>% select("R", "Y", "I_abb", "Ref", "value") %>% F_woc()

PQchange_tmp %>%
  filter(policy %in% c(lis_cp_mitigation, 'Baseline', 'Baseline non-CP')) %>% select(-"Ref", -"target") %>% 
  write.csv(file = paste0(dir_output,"PriceChange.csv"))

## price change from Baseline: line ------
df_PQ_Baseline <- PQchange_tmp %>% left_join(map_R17) %>% 
  dplyr::group_by(R_CGE, Y, I_abb, Ref, policy, SSP, exemption, target, tech, Gini) %>% dplyr::reframe(value = mean(value)) %>% 
  filter(policy == "Baseline") %>% select(R_CGE,Y, I_abb, SSP, value, tech, Gini) %>% dplyr::rename("value_Baseline" = "value") %>% distinct()

df_PQ <- PQchange_tmp %>% left_join(map_R17) %>% 
  dplyr::group_by(R_CGE, Y, I_abb, Ref, policy, SSP, exemption, target, tech, Gini) %>% dplyr::reframe(value = mean(value)) %>% 
  filter(policy %in% c(lis_cp_mitigation)) %>% select(-"Ref", -"target") %>% left_join(df_PQ_Baseline) %>% 
  mutate(changerate = (value-value_Baseline)/value_Baseline * 100) %>%
  transform(I_abb = factor(I_abb, levels = lis_I_abb)) %>% select("R_CGE", "Y", "I_abb", "policy", "SSP", "exemption", "tech", "Gini", "changerate")

df_PQ %>% filter(Y %in% c(2030, 2050)) %>% 
  dplyr::group_by(I_abb, Y, policy,SSP, exemption, Gini, tech) %>% dplyr::reframe(median = median(changerate), mean = mean(changerate)) %>% 
  openxlsx::write.xlsx(file = paste0(dir_csv, "1d_PHI_PriceChange_SSP_regional.xlsx"))


## price change from Baseline: box ------

pdata0 <- df_PQ %>% filter(exemption == "None", !is.na(R_CGE), !is.na(I_abb), Y %in% c(2030, 2050)) %>% 
  left_join(map_R17to5 %>% select(-R, -R_full) %>% filter(R5 == "World") %>% distinct())%>% 
  F_filter_main() %>% 
  dplyr::rename(value = "changerate") #%>% F_R17full()

pdata <- pdata0

p <- ggplot(pdata) + geom_hline(yintercept = 0, color = "grey") +
  geom_boxplot(aes(x = I_abb, y = value), size = .5, linewidth = .5) + labs(x = "Commodity", y = "%") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) + facet_grid(~Y)


ggsave(p,filename = paste0(dir_fig, "Figure12_Price_ConsumptionShare/global_price_all.pdf"), #path = dir_fig,
       width = 18, height = 8, units = "cm")

rm(pdata, pdata0, df_PQ, df_PQ_Baseline, PQchange_tmp)





# 2. Consumption share ----------------------------------------------------
# national
df_aus <- paste0("../../DataArchive/PHIoutput/gdx/ExpData.gdx") %>% rgdx.param("CNSAUS") %>% mutate(R = "AUS") %>% filter(Y == 2015, Seg == "TOTAL") %>% dplyr::rename(value = "CNSAUS") %>% select(-Seg)
df_can <- paste0("../../DataArchive/PHIoutput/gdx/ExpData.gdx") %>% rgdx.param("CNSCAN") %>% mutate(R = "CAN") %>% filter(Y == 2015, Seg == "TOTAL") %>% dplyr::rename(value = "CNSCAN") %>% select(-Seg)
df_chn <- paste0("../../DataArchive/PHIoutput/gdx/ExpData.gdx") %>% rgdx.param("CNSCHN") %>% mutate(R = "CHN", I = Iall) %>% select(-Iall) %>% filter(Y == 2014, Seg == "TOTAL", !grepl("AGG", I)) %>% dplyr::rename(value = "CNSCHN") %>% select(-Seg)
df_jpn <- paste0("../../DataArchive/PHIoutput/gdx/ExpData.gdx") %>% rgdx.param("CNSJPN") %>% mutate(R = "JPN") %>% filter(Y == 2014, Seg == "TOTAL") %>% dplyr::rename(value = "CNSJPN") %>% select(-Seg)
df_usa <- paste0("../../DataArchive/PHIoutput/gdx/ExpData.gdx") %>% rgdx.param("CNSUSA") %>% mutate(R = "USA") %>% filter(Y == 2015, Seg == "TOTAL") %>% dplyr::rename(value = "CNSUSA") %>% select(-Seg)
# global
df_gcd <- paste0("../../DataArchive/PHIoutput/gdx/ExpData.gdx") %>% rgdx.param("ConsumptionSector") %>% mutate(Y = "2010") %>% filter(ConsumptionSegment == "All", Area == "National") %>% dplyr::rename(value = "ConsumptionSector") %>% select(-ConsumptionSegment, -Area)
df_eurostat <- paste0("../../DataArchive/PHIoutput/gdx/ExpData.gdx") %>% rgdx.param("EUROSTATI")  %>% filter(Y == 2015) %>% filter(quantile == "TOTAL")  %>% dplyr::rename(value = "EUROSTATI") %>% select(-quantile)
# integration
df_cons <- df_aus %>% rbind(df_can) %>% rbind(df_chn) %>% rbind(df_jpn) %>% rbind(df_usa) %>% rbind(df_gcd) %>% rbind(df_eurostat) %>% 
  dplyr::group_by(R,Y) %>% dplyr::mutate(tot = sum(value)) %>% ungroup() %>% mutate(ratio = value/tot * 100) %>% left_join(MapI)
colnames(df_cons)
rm(df_aus, df_can, df_chn, df_jpn, df_usa, df_gcd, df_eurostat)

## consumption share in 2010 ------

pdata <- df_cons %>% mutate(I_abb = factor(I_abb, levels = lis_I_abb))

p <- ggplot(pdata) + geom_hline(yintercept = 0, color = "grey") +
  geom_boxplot(aes(x = I_abb, y = ratio), size = .5, linewidth = .5) + labs(x = "Commodity", y = "%") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) 
p

ggsave(p,filename = paste0(dir_fig, "Figure12_Price_ConsumptionShare/global_consumptionshare_all.pdf"), #path = dir_fig,
       width = 12, height = 8, units = "cm")

rm(pdata, pdata0)


