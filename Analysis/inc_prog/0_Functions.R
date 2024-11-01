# This file defines the functions for AIM/PHI analysis 
# In the first feature paper and my thesis

# Shiya ZHAO, 2021/12/13


# function for the full names ----
F_R17full <- function(x){
  y <- x %>% 
    mutate(R_CGE = case_when(R_CGE=="XSA" ~ "Rest of Asia",
                             R_CGE=="XER" ~ "Rest of Europe",
                             R_CGE=="XNF" ~ "North Africa",
                             R_CGE=="XAF" ~ "Sub-Saharan Africa",
                             R_CGE=="XLM" ~ "Rest of South America",
                             R_CGE=="CIS" ~ "Former Soviet Union",
                             R_CGE=="XOC" ~ "Oceania",
                             R_CGE=="XE25" ~ "EU",
                             R_CGE=="XME" ~ "Middle East",
                             R_CGE=="BRA" ~ "Brazil",
                             R_CGE=="XSE" ~ "Southeast Asia",
                             R_CGE=="CAN" ~ "Canada",
                             R_CGE=="CHN" ~ "China",
                             R_CGE=="IND" ~ "India",
                             R_CGE=="JPN" ~ "Japan",
                             R_CGE=="TUR" ~ "Turkey",
                             R_CGE=="USA" ~ "USA",
                             !(R_CGE %in% lis_R17) ~ R_CGE)) 
  return(y)
}

# filtering the data ----
F_filter <- function(x, s_Baseline_woc = F, lis_R= lis_R){
  if(s_Baseline_woc == F){
    y <- x %>% filter(!is.na(Ref), R %in% lis_R, Ref %in% lis_ref_all, !grepl("Base_woc", Ref))
  }else{
    y <- x %>% filter(!is.na(Ref), R %in% lis_R, Ref %in% lis_ref_all)
  }
  return(y)
}


# filtering the data for main 
F_filter_main <- function(x){
  y <- x %>% filter(tech == "None", Gini == "consistent")
}




# functions for data preparation for change calculation ----
F_cha_decom <- function(x, lis_del = c("target", "Ref", "policy", "exemption")){
  y <- x %>% mutate(policy_expt = paste0(policy, "_", exemption)) %>% 
    select(-any_of(lis_del)) %>%
    pivot_wider(names_from = "policy_expt", values_from = "value") %>% 
    pivot_longer(cols = colnames(.)[c(which(grepl("1.5C",colnames(.))), which(grepl("2C",colnames(.))))], 
                 names_to = "Ref", values_to = "value") %>% 
    mutate(policy = str_split(.$Ref,"_",simplify = TRUE)[,1],
           exemption = str_split(.$Ref,"_",simplify = TRUE)[,2]) %>% 
    select(-Ref)
  
  return(y)
  
}


# splitting scenario keywords ----
F_woc <- function(a){
  b <- a %>% mutate(policy = case_when(grepl("woc", Ref) & grepl("1p5", Ref) ~ "1.5C non-CP", !grepl("woc", Ref) & grepl("1p5", Ref) ~ "1.5C CP", 
                                       grepl("woc", Ref) & grepl("2C", Ref) ~ "2C non-CP", !grepl("woc", Ref) & grepl("2C", Ref) ~ "2C CP", 
                                       grepl("woc", Ref) & grepl("Base", Ref) ~ "Baseline non-CP", 
                                       !grepl("woc", Ref) & grepl("Base", Ref) ~ "Baseline"),
                      SSP =  case_when(grepl("SSP1", Ref) ~ "SSP1", grepl("SSP3", Ref) ~ "SSP3", !grepl("SSP", Ref) ~ "SSP2", grepl("SSP2", Ref) ~ "SSP2"),
                      exemption = case_when(grepl("expt", Ref) ~ "Direct tax exemption", !grepl("expt", Ref) ~ "None"),
                      target = case_when(grepl("1p5", Ref) ~ "1.5C", grepl("2C", Ref) ~ "2C", grepl("Base", Ref) ~ "Baseline"),
                    tech = case_when(grepl("LowAgTech", Ref) ~ "LowAgTech", grepl("HighAgTech", Ref) ~ "HighAgTech", !grepl("AgTech", Ref) ~ "None"),
                    Gini = case_when(grepl("_Gini1", Ref) ~ "SSP1", grepl("_Gini3", Ref) ~ "SSP3", grepl("_Gini2", Ref) ~ "SSP2", !grepl("_Gini1", Ref) & !grepl("_Gini3", Ref) & !grepl("_Gini2", Ref) ~ "consistent"))
  return(b)
}


# TH names ----
F_TH1 <- function(x){
  y <- x %>% mutate(TH=paste0("$", str_split(.$TH1,"-",simplify = TRUE)[,1], " per capita per day"))
  return(y)
}

# ribbon plot ----
F_plot_ribbon <- function(pdata, lab_x = "Year", lab_y = expression(paste("Gt",CO[2],"eq/y"))){
  
  pdata1 <- pdata %>% select(-"v_group") %>% # filter(SSP != "SSP2") %>%
    pivot_wider(names_from = "SSP", values_from = "value") %>% 
    mutate(v_min = apply(.[colnames(.)[startsWith(colnames(.), "SSP")]],1,min), 
           v_max = apply(.[colnames(.)[startsWith(colnames(.), "SSP")]],1,max))
  
  p <- ggplot()  + 
    geom_ribbon(data= pdata1, mapping = aes(x = Y, ymin=v_min,ymax=v_max, group = policy, fill = policy), alpha=.1) +
    geom_line(data =  pdata, mapping = aes(x = Y, y = value, group = v_group,color = policy, linetype = SSP, linewidth = SSP)) +
    geom_point(data =  pdata, mapping = aes(x = Y, y = value, color = policy, shape = SSP), size = 0.7) +
    labs(x = lab_x, y = lab_y, size = 9)+
    geom_abline(intercept = 0, slope = 0, color = "grey") +
    MyTheme +
    guides(color=guide_legend(title='Scenario',ncol = 1), 
           linetype = guide_legend(title = 'SSP'), 
           shape = guide_legend(title = "SSP"),
           fill = guide_legend(title = "Scenario"),
           linewidth = guide_legend(title = "SSP")) +
    scale_color_manual(values = palette_color_cp) +
    scale_fill_manual(values = palette_color_cp) +
    scale_linetype_manual(values = palette_line_SSP) +
    scale_shape_manual(values = palette_point_SSP) +
    scale_linewidth_manual(values = c("SSP2" = .6, "SSP1" = .4, "SSP3" = .4)) + 
    scale_x_discrete(breaks=seq(2010, 2100, 10)) 
  return(p)
}

# ribbon plot ----
F_plot_ribbon <- function(pdata, lab_x = "Year", lab_y = expression(paste("Gt",CO[2],"eq/y"))){
  
  pdata1 <- pdata %>% select(-"v_group") %>% # filter(SSP != "SSP2") %>%
    pivot_wider(names_from = "SSP", values_from = "value") %>% 
    mutate(v_min = apply(.[colnames(.)[startsWith(colnames(.), "SSP")]],1,min), 
           v_max = apply(.[colnames(.)[startsWith(colnames(.), "SSP")]],1,max))
  
  p <- ggplot()  + 
    geom_ribbon(data= pdata1, mapping = aes(x = Y, ymin=v_min,ymax=v_max, group = policy, fill = policy), alpha=.1) +
    geom_line(data =  pdata, mapping = aes(x = Y, y = value, group = v_group,color = policy, linetype = SSP, linewidth = SSP)) +
    geom_point(data =  pdata, mapping = aes(x = Y, y = value, color = policy, shape = SSP), size = 0.7) +
    labs(x = lab_x, y = lab_y, size = 9)+
    geom_abline(intercept = 0, slope = 0, color = "grey") +
    MyTheme +
    guides(color=guide_legend(title='Scenario',ncol = 1), 
           linetype = guide_legend(title = 'SSP'), 
           shape = guide_legend(title = "SSP"),
           fill = guide_legend(title = "Scenario"),
           linewidth = guide_legend(title = "SSP")) +
    scale_color_manual(values = palette_color_cp) +
    scale_fill_manual(values = palette_color_cp) +
    scale_linetype_manual(values = palette_line_SSP) +
    scale_shape_manual(values = palette_point_SSP) +
    scale_linewidth_manual(values = c("SSP2" = .6, "SSP1" = .4, "SSP3" = .4)) + 
    scale_x_discrete(breaks=seq(2010, 2100, 10)) 
  return(p)
}

# energy area plot ----
F_plot_ene <- function(pdata, pal_color = palette_Prm, txt_title = "Primary energy"){
  pdata1 <- pdata %>% dplyr::group_by(R, Y, Ref, policy, SSP, exemption, target) %>% 
    dplyr::reframe(value_sum = sum(value))
  
  p <- ggplot() +
    geom_area(pdata, mapping = aes(x = as.numeric(as.character(Y)), y = value, fill = fct_rev(com)), position = "stack")  +
    geom_line(pdata1, mapping = aes(x = as.numeric(as.character(Y)), y = value_sum, group = paste0(R, Ref)), linewidth = 0.8, linetype = "dashed") +
    # geom_rect(aes(xmin=2005, xmax=2025, ymin=0, ymax=Inf), fill = "white", alpha = .2) +
    MyTheme +
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
      # legend.box = element_line(color = 'transparent')
    ) +
    labs(x = "Year", y = "EJ/year", title = txt_title) + 
    scale_fill_manual(values = pal_color) +
    guides(fill = guide_legend(title = NULL))
  return(p)
}



# Thresholds ----
F_TH <- function(x){
  y <- x %>% mutate(TH1 = recode(TH,
                                 # 'pop_1.9' = lis_TH[1],
                                 'pop_2.15' = lis_TH[1],
                                 'pop_3.65' = lis_TH[2],
                                 'pop_6.85' = lis_TH[3])) 
}


# Decile calculation updated ----
F_quantile <- function(Consumption_data, OD, df_q = seq(0.1,1,0.1), StepLength=200, LengthThreshold=500, split_all = T, budget_highest=1000000){
  
  # df_FreqSegall <- rgdx.param(paste0("../output/gdx/demand_decomposition/country/JPN.gdx"), "FreqSegall") %>%
  #   gdata::rename.vars(from = colnames(.), to = c("scenario","Y","Seg","pop")) %>%
  #   mutate(R = "JPN") %>% 
  #   filter(scenario %in% lis_scenario,
  #          Y %in% lis_y) %>% 
  #   SegNum() %>% 
  #   select(c("scenario", "Y", "Seg", "Upper")) %>% 
  #   gdata::rename.vars("Upper", "value") %>% 
  #   mutate(I = "total")
  
  lis_scenario_function <- unique(OD$scenario)
  lis_y_function <- unique(OD$Y)
  lis_I_function <- unique(OD$I)
  
  df_Mu_exp <- rgdx.param(Consumption_data, "Mu_exp") %>% 
    gdata::rename.vars(colnames(.),c("R", "scenario","Y","para_Mu_exp")) 
  
  df_Sigma_exp <- rgdx.param(Consumption_data, "Sigma_exp") %>%
    gdata::rename.vars(colnames(.),c("R", "scenario","Y","para_Sigma_exp")) 
  
  OD1 <- OD %>% left_join(df_Mu_exp) %>% left_join(df_Sigma_exp) 
  
  Seg_pop <- rgdx.param(Consumption_data, "FreqSegall")
  colnames(Seg_pop) <- c("R",'scenario','Y','Seg','pop')
  # Seg_pop <- filter(Seg_pop, Y %in% lis_y) %>%
  #   SegNum(budget_highest)
  
  data0 <- OD1 %>% 
    SegNum() %>% 
    left_join(Seg_pop) %>% 
    select(c("scenario", "R", "Y", "Seg", "I", "value", "Upper", "Lower", "para_Mu_exp", "para_Sigma_exp", "pop")) %>% 
    mutate(DEC = case_when(Lower <= qlnorm(0.1, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 1,
                           qlnorm(0.1, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.2, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 2,
                           qlnorm(0.2, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.3, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 3,
                           qlnorm(0.3, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.4, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 4,
                           qlnorm(0.4, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.5, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 5,
                           qlnorm(0.5, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.6, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 6,
                           qlnorm(0.6, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.7, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 7,
                           qlnorm(0.7, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.8, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 8,
                           qlnorm(0.8, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.9, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 9,
                           qlnorm(0.9, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower ~ 10),
           q_value = case_when(DEC == 1 ~ qlnorm(0.1, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 2 ~ qlnorm(0.2, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 3 ~ qlnorm(0.3, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 4 ~ qlnorm(0.4, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 5 ~ qlnorm(0.5, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 6 ~ qlnorm(0.6, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 7 ~ qlnorm(0.7, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 8 ~ qlnorm(0.8, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 9 ~ qlnorm(0.9, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 10 ~ 1000000),
           idx = case_when(Upper > q_value & Lower <= q_value ~ 1,
                           !(Upper > q_value & Lower <= q_value) ~ 0))
  
  # filtering and splitting the segments where the decile thresholds cut
  data1_tmp1 <- data0 %>%
    mutate(floor1 = ifelse(idx == 1, floor((q_value - Lower)/StepLength),floor((Upper - Lower)/StepLength)),
           floor2 = ifelse(idx == 1, floor((Upper - q_value)/StepLength), 0),
           ceiling1 = ifelse(idx == 1, ceiling((q_value - Lower)/StepLength), ceiling((Upper - Lower)/StepLength)),
           ceiling2 = ifelse(idx == 1, ceiling((Upper - q_value)/StepLength), 0)) %>%
    mutate(ExpShare = value/Upper) %>%
    transform(DEC = as.numeric(DEC))
  
  data1_ExpShare <- data1_tmp1 %>%
    select(c("scenario", "R", "Y", "Seg", "I", "ExpShare")) %>% 
    # mutate(I = paste0(I, "_ExpShare")) %>% 
    pivot_wider(names_from = I, values_from = "ExpShare")
  
  if(split_all == TRUE){
    data1_tmp2 <- data1_tmp1 %>%
      # filter(Upper-Lower >= LengthThreshold) %>%
      select(-"ExpShare") %>%
      spread(I, value) %>%
      select(-all_of(lis_I_function))  
  }else{
    data1_tmp2 <- data1_tmp1 %>%
      filter(DEC != "10",idx == 1) %>% 
      filter(Upper-Lower >= LengthThreshold) %>%
      select(-"ExpShare") %>%
      spread(I, value) %>%
      select(-all_of(lis_I_function))
  }
  
  data1_tmp3 <- data1_tmp2 %>%
    dplyr::group_by(Y,scenario,R,DEC,Seg) %>%
    na.omit() %>%
    slice(rep(1:n(), first(ceiling1+ceiling2)))%>%
    # group_by(Y,scenario,DEC,Seg1,Seg2) %>%
    dplyr::mutate(idx1 = 1,
                  idxnstep = cumsum(idx1),
                  part = ifelse(idxnstep <= ceiling1,1,2),
                  nstep = ifelse(part == 1,idxnstep,idxnstep-ceiling1),
                  n = ifelse(part == 1, ceiling1, ceiling2)) %>%
    select(-c(ceiling1,ceiling2,floor1,floor2)) 
  
  dat2 <- data1_tmp3 %>%
    mutate(UpperLim = ifelse(part == 1, ifelse(nstep < n, Lower+StepLength*nstep,
                                               ifelse(idx == 1, q_value,Upper)), 
                             ifelse(nstep < n, q_value+StepLength*nstep, Upper)), 
           LowerLim = ifelse(part == 1, ifelse(nstep < n, Lower+StepLength*(nstep-1),  Lower+StepLength*(n-1)),
                             ifelse(nstep < n, q_value+StepLength*(nstep-1), q_value+StepLength*(n-1))))
  
  data1 <- dat2 %>%
    ungroup() %>%
    mutate(DEC_new = DEC+part-1) %>%
    select(-c("Upper", "Lower", "DEC")) %>%
    dplyr::rename(Upper = "UpperLim", Lower = "LowerLim", DEC = "DEC_new") %>%
    # colnames(data1)
    
    left_join(data1_ExpShare,relationship = "many-to-many") %>%
    pivot_longer(cols = all_of(lis_I_function), names_to = "I", values_to = "ExpShare") %>% 
    # filter(is.na(I))
    mutate(value_new = Upper*ExpShare, 
           density = dlnorm((Upper+Lower),meanlog = para_Mu_exp, sdlog = para_Sigma_exp), 
           pop_new = (Upper-Lower)*density) %>%
    select(-c("pop","density","idx1","idxnstep","part","nstep","n")) %>%
    dplyr::rename(value = "value_new", pop = "pop_new")
  # view(data1) 
  
  if(split_all == TRUE){
    data2 <- data1 %>%
      mutate(pop_new = (Upper-Lower)*dlnorm((Upper+Lower)/2,meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
             pop = (Upper-Lower)*dlnorm(Upper,meanlog = para_Mu_exp, sdlog = para_Sigma_exp)) %>%
      select(-"idx") %>%
      transform(I = factor(I, levels = lis_I_function)) %>%
      transform(DEC = factor(as.character(DEC), levels = as.character(seq(1,10,1)))) %>%
      group_by(scenario,R,Y,DEC,I) %>%
      dplyr::summarise(value_seg = sum(value*pop_new)/sum(pop_new),  pop = sum(pop_new), 
                       .groups="drop")
  }else{
    data2 <- data1_tmp4 %>%
      filter(!(idx == 1 & DEC != "10")) %>% 
      select(colnames(data1)) %>%
      rbind(data1) %>%
      mutate(pop_new = (Upper-Lower)*dlnorm((Upper+Lower)/2,meanlog = para_Mu_exp, sdlog = para_Sigma_exp)) %>%
      select(-"idx") %>%
      transform(I = factor(I, levels = lis_I_function)) %>%
      transform(DEC = factor(as.character(DEC), levels = as.character(seq(1,10,1)))) %>%
      group_by(scenario,R,Y,DEC,I) %>%
      dplyr::summarise(value_seg = sum(value*pop_new)/sum(pop_new),  pop = sum(pop_new), 
                       .groups="drop")
  }
  
  return(data2)
}


# Mutate for scenario name and redistribution -----------------------------
F_reve_mutate <- function(x){
  y <- x %>% 
    mutate(revenue = str_split_i(scenario, "_", 2), 
           scenario_Name = str_split_i(scenario, "_", 1)) %>% 
    mutate(revenue = case_when(is.na(revenue) ~ "Neutral",
                               !is.na(revenue) ~ "Progressive")) 
  return(y)
}


# Segment in the expenditure model ----
SegNum <- function(a){   
  lis_Seg <- c('Fiftytok',
               'ktoTk',
               'TktoHk',
               'HktoM',
               'MtoTM')
  lis_Seg11 <- c(50,
                 1000,
                 10000,
                 100000,
                 1000000)
  lis_Seg3 <- c(10,
                100,
                1000,
                10000,
                100000)
  tmp2 <- data.frame()
  for(i in 1:length(lis_Seg)){
    tmp <- a[which(startsWith(as.character(a$Seg),lis_Seg[i])),]%>%
      mutate(Seg0 = lis_Seg[i], Seg1 = lis_Seg11[i])
    tmp1 <- mutate (tmp, 
                    Seg2 = as.numeric(substring(as.character(tmp$Seg),nchar(lis_Seg[i])+1)),
                    Upper = Seg1+Seg2*lis_Seg3[i],
                    Lower = Seg1+(Seg2-1)*lis_Seg3[i])
    tmp2 <- rbind(tmp2,tmp1)
  }
  tmp3 <- tmp2 #%>%
  # select(-c("Seg0","Seg1","Seg2"))
  return(tmp3)
}

# Plot output for grid ----
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}


# EPS output ----
kmg2.ggsave <- function (filename = default_name(plot), plot = last_plot(),
                         device = default_device(filename), path = NULL, scale = 1,
                         width = par("din")[1], height = par("din")[2], dpi = 300,
                         keep = plot$options$keep, drop = plot$options$drop, ...)
{
  # original
  # if (!inherits(plot, "ggplot"))
  #     stop("plot should be a ggplot2 plot")
  if (!inherits(plot, "recordedplot"))
    stop("plot should be a recordedplot")
  eps <- ps <- function(..., width, height) grDevices::postscript(...,
                                                                  width = width, height = height, onefile = FALSE, horizontal = FALSE,
                                                                  paper = "special")
  tex <- function(..., width, height) grDevices::pictex(...,
                                                        width = width, height = height)
  pdf <- function(..., version = "1.4") grDevices::pdf(...,
                                                       version = version)
  svg <- function(...) grDevices::svg(...)
  wmf <- function(..., width, height) grDevices::win.metafile(...,
                                                              width = width, height = height)
  png <- function(..., width, height) grDevices::png(..., width = width,
                                                     height = height, res = dpi, units = "in")
  jpg <- jpeg <- function(..., width, height) grDevices::jpeg(...,
                                                              width = width, height = height, res = dpi, units = "in")
  bmp <- function(..., width, height) grDevices::bmp(..., width = width,
                                                     height = height, res = dpi, units = "in")
  tiff <- function(..., width, height) grDevices::tiff(...,
                                                       width = width, height = height, res = dpi, units = "in")
  default_name <- function(plot) {
    paste(digest.ggplot(plot), ".pdf", sep = "")
  }
  default_device <- function(filename) {
    pieces <- strsplit(filename, "\\.")[[1]]
    ext <- tolower(pieces[length(pieces)])
    match.fun(ext)
  }
  if (missing(width) || missing(height)) {
    message("Saving ", prettyNum(width * scale, digits = 3),
            "\" x ", prettyNum(height * scale, digits = 3), "\" image")
  }
  width <- width * scale
  height <- height * scale
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  device(file = filename, width = width, height = height, ...)
  on.exit(capture.output(dev.off()))
  print(plot, keep = keep, drop = drop)
  invisible()
}

# Q_lognormal <- function(df_q,mu,sigma){
#   q1 <- qlnorm(df_q,meanlog = mu, sdlog = sigma)
#   return(q1)
# }


# 2M indicator Energy poverty ----
## This function could encounter conflicts due to loaded packages
## unload the "plyr" and "Rmisc" 
## by running : detach('package:plyr', unload = TRUE)
## and : detach('package:Rmisc', unload = TRUE)
## before using this function
F_EnePov <- function(Demand_data,lis_ref = c("SSP2_Baseline"),lis_y = c(2020, 2030, 2050, 2070),lis_Iene = c("Energy"), median = T){
  # lis_y <- c("2020","2030", "2040", "2050")
  # Demand_data <- demand
  # lis_Iene <- lis_I_ene_full
  
  Seg_pop <- rgdx.param(Demand_data, "FreqSegall") %>% 
    rename.vars(from = colnames(.), to = c("R", 'Ref','Y','Seg','pop')) %>%
    filter(Y %in% lis_y,Ref %in% lis_ref) %>%
    group_by(R,Ref,Y) %>%
    dplyr::reframe(pop = pop, Seg = Seg) %>% #popcum = cumsum(pop), 
    SegNum() 
  
  ExpNational_tmp <- rgdx.ppopExpNational_tmp <- rgdx.param(Demand_data, "ExpNational") %>%
    rename.vars(from = colnames(.), to = c("R", 'Ref','Seg','Y',"I",'ExpNational')) %>%
    filter( Ref %in% lis_ref,
            Y %in% lis_y,
            # I %in% lis_Iene,
            # VC == "share"
    ) %>%
    dplyr::group_by(R, Ref, Seg, Y) %>% 
    dplyr::mutate(share = ExpNational/sum(ExpNational)) %>% 
    ungroup() %>% 
    filter(I %in% lis_Iene)
  # view(ExpNational_tmp)
  
  Ene_exp <- ExpNational_tmp %>%
    group_by(R, Ref,Seg,Y) %>%
    dplyr::summarise(ENEshare = sum(share), .groups = "drop")  %>%
    left_join(Seg_pop) %>%
    mutate(ENEabs = ENEshare*Upper) %>%
    select(-c( "Seg0","Seg1","Seg2"))
  # the total share of disposable income spent on residential energy use
  # view(Ene_exp)
  
  
  Ene_share_med_tmp1 <- Ene_exp %>%
    group_by(R, Ref,Y) %>%
    dplyr::arrange(ENEshare, by_group = T) %>%
    dplyr::reframe(ENEshare = ENEshare, Seg = Seg, popcum = cumsum(pop), idx = abs(popcum-0.5))# %>%
  # view(Ene_share_med_tmp1)
  Ene_share_med <-  Ene_share_med_tmp1 %>%
    group_by(R, Ref,Y) %>%
    dplyr::summarise(idx = min(idx), .groups = "drop") %>% # the one closest to 0.5 is deemed as segment with the median value
    left_join(Ene_share_med_tmp1) %>%# here we picked out the income segments whose share of energy expenditure equals the median
    select("R", "Ref","Y","ENEshare") %>%
    dplyr::rename(MedianShare = "ENEshare") 
  # view(Ene_share_med)
  
  
  Ene_abs_med_tmp1 <- Ene_exp %>%
    mutate(ENEabs = ENEshare*Upper) %>%
    group_by(R, Ref,Y) %>%
    dplyr::arrange(ENEabs, by_group = T) %>%
    dplyr::reframe(ENEabs = ENEabs, Seg = Seg, popcum = cumsum(pop), idx = abs(popcum-0.5))# %>%
  # view(Ene_abs_med_tmp1)
  Ene_abs_med <- Ene_abs_med_tmp1 %>%
    group_by(R, Ref,Y) %>%
    dplyr::summarise(idx = min(idx), .groups = "drop") %>% # the one closest to 0.5 is deemed as segment with the median value
    left_join(Ene_abs_med_tmp1) %>%# here we picked out the income segments whose share of energy expenditure equals the median
    select("R", "Ref","Y","ENEabs") %>%
    dplyr::rename(MedianAbs = "ENEabs") 
  # view(Ene_abs_med)
  
  
  EnePov_2M <- Ene_exp %>%
    select("R", "Ref","Seg","Y","ENEshare","pop") %>%
    left_join(Ene_share_med) %>% 
    mutate(Ind = ENEshare-2*MedianShare) %>%
    filter(Ind >= 0) %>%
    group_by(R, Ref,Y) %>%
    dplyr::summarize(TwoM = sum(pop), .groups = "drop")
  # view(EnePov_2M)
  
  EnePov_HEP <- Ene_exp %>%
    select("R", "Ref","Seg","Y","ENEabs","pop") %>%
    left_join(Ene_abs_med) %>%
    mutate(Ind = ENEabs-0.5*MedianAbs) %>%
    filter(Ind <= 0) %>%
    group_by(R, Ref,Y) %>%
    dplyr::summarize(HEP = sum(pop), .groups = "drop")
  # view(EnePov_HEP)
  
  EnePov_LIHC <- Ene_exp %>%
    # select("Ref","Seg","Y","ENEabs","pop") %>%
    left_join(Ene_share_med) %>%
    mutate(Exp_nonEne = Upper*(1-ENEshare),
           PovTH = 3.2*365,
           Ind1 = ENEshare - MedianShare,
           Ind2 = Exp_nonEne - PovTH) %>%
    filter(Ind1 > 0, Ind2 <= 0) %>%
    group_by(R, Ref,Y) %>%
    dplyr::summarize(LIHC = sum(pop), .groups = "drop")
  
  EneMedian <- Ene_abs_med %>%
    full_join(Ene_share_med) %>%
    ungroup()
  
  EnePov <- EnePov_2M %>%
    full_join(EnePov_HEP) %>%
    full_join(EnePov_LIHC) %>%
    ungroup()
  # view(EnePov)
  if(median){
    # print("T")
    return(EneMedian)
  }else{
    # print("F")
    return(EnePov)
  }
  
}


F_EnePov_global <- function(Demand_data,population,map_r_PHI2CGE,lis_R,lis_ref,lis_y,lis_Iene, median){
  # lis_y <- c("2020","2030", "2040", "2050")
  # Demand_data <- ConsumptionResults
  # lis_Iene <- lis_I_ene_full
  # lis_R <-c("CHN","IND","USA")
  # median <-  T
  lis_R <-  F
  if(lis_R == F){
    ExpNational_tmp <- Demand_data %>%
      rgdx.param("ExpNational") %>%
      rename.vars(from = colnames(.), to = c("R","Ref","VC","Seg","Y","I","ExpNational")) %>%
      filter( Ref %in% lis_ref,
              Y %in% lis_y,
              I %in% lis_Iene)%>%
      transform(Y = factor(Y, levels = lis_y))
  }else{
    ExpNational_tmp <- Demand_data %>%
      rgdx.param("ExpNational") %>%
      rename.vars(from = colnames(.), to = c("R","Ref","VC","Seg","Y","I","ExpNational")) %>%
      filter( Ref %in% lis_ref,
              Y %in% lis_y,
              I %in% lis_Iene,
              R %in% lis_R)  %>%
      transform(Y = factor(Y, levels = lis_y))
  }
  
  
  Seg_pop <- rgdx.param(Demand_data, "FreqSegall") %>%
    rename.vars(from = colnames(.), to = c("R",'Ref','Y','Seg','pop')) %>%
    filter(Y %in% lis_y,
           Ref %in% lis_ref) %>%
    SegNum() 
  # view(Seg_pop)
  
  
  
  Ene_exp_tmp <- ExpNational_tmp %>%
    filter( VC == "abs") %>%
    select(-"VC") %>%
    group_by(R,Ref,Seg,Y) %>%
    dplyr::summarise(ENEabs = sum(ExpNational),
                     .groups = "drop")  %>%
    left_join(Seg_pop) %>%
    left_join(population) %>%
    mutate(population_seg = Population * pop,
           ENEabs_seg = ENEabs * population_seg) %>%
    left_join(map_r_PHI2CGE) %>%
    group_by(R_CGE, Ref, Seg, Y) %>%
    dplyr::summarise(population_seg_tot = sum(population_seg),
                     ENEabs_seg_avg = sum(ENEabs_seg)/sum(population_seg),
                     .groups = "drop") %>%
    select(R_CGE,Ref,Seg,Y,population_seg_tot,ENEabs_seg_avg)
  # the total share of disposable income spent on residential energy use
  # view(Ene_exp_tmp)  
  
  population_PHI_17 <- Ene_exp_tmp %>%
    group_by(R_CGE, Ref, Y) %>%
    dplyr::summarise(population_tot = sum(population_seg_tot),
                     .groups = "drop")
  
  
  Ene_exp <- Ene_exp_tmp %>%
    SegNum() %>%
    left_join(population_PHI_17) %>%
    gdata::rename.vars(from = c("R_CGE", "ENEabs_seg_avg"),
                       to = c("R","ENEabs"))%>%
    mutate(pop = population_seg_tot/population_tot,
           ENEshare = ENEabs/Upper) %>%
    select(R,Ref,Seg,Y,pop,ENEabs,ENEshare,Upper) 
  
  
  Ene_share_med_tmp1 <- Ene_exp %>%
    group_by(R,Ref,Y) %>%
    dplyr::arrange(ENEshare, by_group = T) %>%
    dplyr::summarise(ENEshare = ENEshare, Seg = Seg, popcum = cumsum(pop), idx = abs(popcum-0.5))# %>%
  # # view(Ene_share_med_tmp1)
  
  Ene_share_med <-  Ene_exp %>%
    group_by(R,Ref,Y) %>%
    dplyr::arrange(ENEshare, by_group = T) %>%
    dplyr::summarise(ENEshare = ENEshare, Seg = Seg, popcum = cumsum(pop), idx = abs(popcum-0.5)) %>%
    filter(idx == min(idx)) %>%
    select("R","Ref","Y","ENEshare") %>%
    dplyr::rename(MedianShare = "ENEshare") 
  # view(Ene_share_med)
  
  
  Ene_abs_med <- Ene_exp %>%
    mutate(ENEabs = ENEshare*Upper) %>%
    group_by(R,Ref,Y) %>%
    dplyr::arrange(ENEabs, by_group = T) %>%
    dplyr::summarise(ENEabs = ENEabs, Seg = Seg, popcum = cumsum(pop), idx = abs(popcum-0.5)) %>%
    filter(idx == min(idx)) %>%
    select("R","Ref","Y","ENEabs") %>%
    dplyr::rename(MedianAbs = "ENEabs") 
  # view(Ene_abs_med)
  
  
  
  EnePov_2M <- Ene_exp %>%
    select("R","Ref","Seg","Y","ENEshare","pop") %>%
    left_join(Ene_share_med) %>%
    mutate(Ind = ENEshare-2*MedianShare) %>%
    filter(Ind >= 0) %>%
    group_by(R,Ref,Y) %>%
    dplyr::summarize(TwoM = sum(pop))
  # view(EnePov_2M)
  
  EnePov_HEP <- Ene_exp %>%
    select("R","Ref","Seg","Y","ENEabs","pop") %>%
    left_join(Ene_abs_med) %>%
    mutate(Ind = ENEabs-0.5*MedianAbs) %>%
    filter(Ind <= 0) %>%
    group_by(R,Ref,Y) %>%
    dplyr::summarize(HEP = sum(pop))
  # view(EnePov_HEP)
  
  EnePov_LIHC <- Ene_exp %>%
    # select("Ref","Seg","Y","ENEabs","pop") %>%
    left_join(Ene_share_med) %>%
    mutate(Exp_nonEne = Upper*(1-ENEshare),
           PovTH = 3.2*365,
           Ind1 = ENEshare - MedianShare,
           Ind2 = Exp_nonEne - PovTH) %>%
    filter(Ind1 > 0, Ind2 <= 0) %>%
    group_by(R, Ref,Y) %>%
    dplyr::summarize(LIHC = sum(pop))
  
  EneMedian <- Ene_abs_med %>%
    full_join(Ene_share_med) %>%
    ungroup()
  
  EnePov <- EnePov_2M %>%
    full_join(EnePov_HEP) %>%
    full_join(EnePov_LIHC) %>%
    ungroup()
  # view(EnePov)
  if(median == T){
    return(EneMedian)
  }else{
    return(EnePov)    
  }
  
}



# Food poverty ----
F_FoodPov <- function(df_FPL, dir_input){
  m_input <- Container$new(dir_input)
  
  # Food expenditure
  df_Exp_fd <- m_input["ExpNational"]$records %>% gdata::rename.vars(colnames(.), c("R", "Ref", "Seg", "Y", "I", "value")) %>% 
    filter(I == "Food and nonalcoholic beverages") %>% 
    mutate(value = value/365) 
  
  # Price change
  df_PQ <- m_input["PQchange"]$records %>% 
    gdata::rename.vars(colnames(.), c("R", "Ref", "Y", "R3", "I", "PQ")) %>% 
    filter(I == "Food and nonalcoholic beverages") 
  
  df_PQ_2010 <- df_PQ %>% filter(Y == 2010) %>% select(-Y, -I) %>% dplyr::rename("PQ_2010" = "PQ")
  
  # Future FPL
  df_FPL_scenario <- df_PQ %>% left_join(df_PQ_2010) %>% select(-R3) %>% left_join(df_FPL %>% select(-Y)) %>% 
    mutate(value_indicator = value_ind_2010 * PQ/PQ_2010) %>% 
    select("R", "Ref", "Y", "I", "Indicator", "Unit", "value_indicator") 
  
  # Mu_exp and Sigma_exp
  df_Mu_exp <- m_input["Mu_exp"]$records %>% gdata::rename.vars(colnames(.), c("R", "Ref", "Y", "para_Mu_exp")) 
  
  df_Sigma_exp <- m_input["Sigma_exp"]$records %>% gdata::rename.vars(colnames(.), c("R", "Ref", "Y", "para_Sigma_exp")) 
  
  
  df_Exp_fd_FPL <- df_Exp_fd %>% left_join(df_FPL_scenario)  %>% SegNum() %>% 
    mutate(FP = case_when(value <= value_indicator ~ 1, value > value_indicator ~ 0)) %>% 
    filter(FP == 1) %>% 
    dplyr::group_by(R, Ref, Y, I, Indicator) %>% dplyr::reframe(Income = max(Upper)) %>% 
    left_join(df_Mu_exp) %>% left_join(df_Sigma_exp) %>% 
    filter(!is.na(para_Mu_exp)) %>% 
    mutate(value = plnorm(Income, meanlog = para_Mu_exp, sdlog = para_Sigma_exp)) %>% 
    select(R, Ref, Y, Indicator, Income, value)
  
  return(df_Exp_fd_FPL)
  
}


# Nishiura-Matsui-Mori plots ----

F_p_area <- function(pdata,Value,ylab1,xlab1,palette_fill){
  plot <- ggplot() +
    geom_area(data=pdata,
              aes(x=as.numeric(as.character(Year)), 
                  y=Value, 
                  fill=Variable,group=Variable),
              stat="identity") + 
    ylab(ylab1) + xlab(xlab1) + 
    facet_wrap(~ SCENARIO,nrow=1) + 
    # MyThemeLine +
    theme(legend.position=c(0.5,1), text=element_text(size=12),  
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = 12)) +
    guides(fill=guide_legend(ncol=5,title=NULL)) + 
    scale_fill_manual(values = palette_fill) +
    scale_x_continuous(breaks=seq(min(as.numeric(as.character(pdata$Year))),
                                  max(as.numeric(as.character(pdata$Year))),
                                  10))
  return(plot)
}


# plot <- fin.1+fin_ind.2+fin_bui.2+fin_tra.2+plot_layout(ncol=4,widths=c(4,1.1,1,1))


F_p_bar_1 <- function(pdata,Value,ylab1, xlab1, palette_fill){
  plot <- ggplot() + 
    geom_bar(data=pdata,
             aes(x=SCENARIO, y = Value, 
                 fill=Variable, group=Variable),
             stat="identity") + 
    ylab(ylab1) + 
    xlab(xlab1) + 
    guides(fill=guide_legend(title=NULL)) + 
    MyThemeLine +
    facet_wrap(~Year, ncol = length(unique(pdata$Year))) +
    theme(legend.position=, text=element_text(size=12),  
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = 12)) + 
    scale_fill_manual(values=palette_fill,name="") +
    guides(fill=guide_legend(ncol=1,title=NULL))
  
  return(plot)
}


#-------------------------


plot.1 <- function(XX){
  plot <- ggplot() + 
    geom_area(data=XX,
              aes(x=Year, y=Value , fill=Variable, group=Variable), 
              stat="identity") + 
    guides(fill=guide_legend(title=NULL)) + 
    MyThemeLine +
    theme(legend.position=c(0.5,1), 
          text=element_text(size=12),  
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = 12)) +
    guides(fill=guide_legend(ncol=5,title=NULL)) #+ 
  # scale_x_continuous(breaks=seq(miny,maxy,10))
  
  plot2 <- plot +
    facet_wrap(~ SCENARIO,nrow=1) + 
    # scale_fill_manual(values=colorpal) + 
    theme(legend.position='bottom')
  plot3 <- plot2
  return(plot3)
}

plot.2 <- function(XX){
  plot <- ggplot() + 
    geom_bar(data=XX,
             aes(x=SCENARIO, y = Value, fill=Variable, group=Variable), stat='identity') + 
    # ylab("") + 
    # xlab(xlab1) + 
    guides(fill=guide_legend(title=NULL)) + 
    MyThemeLine +
    theme(legend.position=, 
          text=element_text(size=12),  
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = 12), 
          axis.text.y=element_blank(),
          axis.title.y = element_blank())+
    guides(fill=guide_legend(ncol=3,title=NULL))
  
  plot3 <- plot +
    facet_wrap(~ Year) #+ 
  # scale_fill_manual(values=colorpal)  
  
  return(plot3)
}

#linesplot function with dots
plot.3 <- function(XX){ 
  plot <- ggplot(XX,aes(x=Year,y=Value,colour=flag2,fill=flag)) +
    geom_line(size=0.5) +
    geom_point(aes(x=Year,y=Value,shape=flag),size=4,position=position_dodge(0)) +
    scale_shape_manual(values=c(19,4)) +
    ylab(ylab1) + xlab(xlab1) + guides(fill=guide_legend(title=NULL)) +
    MyThemeLine +
    theme(legend.position ="none", text=element_text(size=12),
          legend.title=element_blank(),
          axis.text.x=element_text(angle=45,vjust=1,hjust=1,size=12)) +
    guides(fill=guide_legend(ncol=5,title=NULL)) + scale_x_continuous(breaks=seq(miny,maxy,10))
  plot2 <- plot + facet_wrap(~Variable, nrow=1) + #scale_fill_manual(values=colorpal) +
    theme(legend.position="bottom")
  plot3 <- plot2
  return(plot3)
}

#plot.2 ver2
plot.2s <- function(XX){
  plot <- ggplot() + 
    geom_bar(data=XX,aes(x=SCENARIO, y = Value, fill=Variable, group=Variable), stat='identity') + 
    ylab(ylab1) + xlab(xlab1) + guides(fill=guide_legend(title=NULL)) + 
    MyThemeLine +
    theme(legend.position=, text=element_text(size=12),  
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = 12))+
    guides(fill=guide_legend(ncol=3,title=NULL))
  
  plot3 <- plot + scale_fill_manual(values=colorpal)  
  #    annotate("segment",x=miny,xend=maxy,y=0,yend=0,linetype="solid",color="grey") + theme(legend.position='bottom')
  return(plot3)
}

