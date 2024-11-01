# The Rscript for the elasticity related analysis in the non-CP/CP paper
# Shiya ZHAO, 2024.04.01


# Income elasticity calculation ------
F_incela <- function(x){
  
  
  
  
}


# 0. load data ------------------------------------------------------------

df_incela <- demand %>% gdxrrw::rgdx.param("IncomeEla") %>% 
  gdata::rename.vars(colnames(.), c("R","Ref","Seg","Y","R1","I", "value")) %>% select(-R1) %>% filter(Y %in% c(2030, 2050), Ref == "Base")


a <- df_incela %>% filter(I == "Energy")
dir.create(paste0(dir_fig,"6_IncomeEla/"))
lis_R_loop <- unique(df_incela$R)
for(r in 1:length(lis_R_loop)){
  # r <- 1
  pdata <- df_incela %>% filter(R == lis_R_loop[r]) %>% SegNum() %>% F_woc() %>% filter(exemption == "None")
  
  p <- ggplot(pdata) +
    geom_hline(yintercept = 1, color = "grey", alpha = .5) +
    geom_hline(yintercept = 0, color = "grey", alpha = .5) +
    geom_hline(yintercept = -1, color = "grey", alpha = .5) +
    geom_point(aes(x = Upper, y = value, color = Y), alpha = .5, size = .5) + facet_wrap(~I) + MyTheme
  p
  
  ggsave(p, filename = paste0(lis_R_loop[r], ".pdf"), path = paste0(dir_fig,"6_IncomeEla/"), width = 20, height = 16, units = "cm")
}









# Price elasticity calculation ------

df_priela <- demand %>% gdxrrw::rgdx.param("PriceEla") %>% 
  gdata::rename.vars(colnames(.), c("R","Ref","Seg","Y","R1","I", "value")) %>% select(-R1) %>% filter(Y %in% c(2030, 2050), Ref == "Base")


dir.create(paste0(dir_fig,"6_PriceEla/"))
lis_R_loop <- unique(df_incela$R)
for(r in 1:length(lis_R_loop)){
  # r <- 1
  pdata <- df_priela %>% filter(R == lis_R_loop[r]) %>% SegNum() %>% F_woc() %>% filter(exemption == "None")
  
  p <- ggplot(pdata) +
    geom_hline(yintercept = 1, color = "grey", alpha = .5) +
    geom_hline(yintercept = 0, color = "grey", alpha = .5) +
    geom_hline(yintercept = -1, color = "grey", alpha = .5) +
    geom_point(aes(x = Upper, y = value, color = Y), alpha = .5, size = .5) + facet_wrap(~I) + MyTheme
  p
  
  ggsave(p, filename = paste0(lis_R_loop[r], ".pdf"), path = paste0(dir_fig,"6_PriceEla/"), width = 20, height = 16, units = "cm")
}



