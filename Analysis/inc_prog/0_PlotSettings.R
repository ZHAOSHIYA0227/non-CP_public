# This file defines the palettes and themes for AIM/PHI analysis 
# In the first feature paper and my thesis

# Shiya ZHAO, 2021/12/21

# directory ----
dir_fig <- paste0(prog_loc,"output/fig/")
dir.create(dir_fig)

dir_output <- paste0(prog_loc,"output/")
dir.create(paste0(prog_loc,"output/csv/"))


# Color palette ----
# palette_Com <- c("FNB" = "#FF3366",
#                  'HWE' = "#FF9966",
#                  "FHE" = "#CCFF00",
#                  "TRN" = "#33CC33",
#                  'OTHS' = "#999999")

palette_y <- c('2020' = '#184e77', #'#9999CC', 
               '2030' = '#1a759f', # '#1e6091',
               '2040' = '#34a0a4', # 
               '2050' = '#76c893')

palette_line_expt <- c("None" = "solid", "Direct tax exemption" = "dashed")



palette_line_cp <- c("Baseline non-CP" = "dotdash",
                     "1.5C non-CP" = "dotdash",
                     "Baseline" ="solid",
                     "1.5C CP" ="solid")

palette_line_SSP<- c("SSP1" = "dotdash",
                     "SSP2" = "solid",
                     "SSP3" = "dashed")

palette_point_SSP <- c("SSP1" = 0,
                      "SSP2" = 16,
                      "SSP3" = 2)


# alpha

palette_alpha_SSP <- c("SSP1" = .5,
                       "SSP2" = 1,
                       "SSP3" = .5)


palette_alpha_cp <- c("2C" = .2,"2C non-CP" = .2, 
                      "1.5C" = 1,"1.5C non-CP" = 1)

palette_alpha_target <- c("2C" = .5,"1.5C" = 1)

# color ----
# palette_color_Sc <- c('Baseline' ='grey30',
#                 '1.5C' = '#990000') 

palette_color_cp <- c('Baseline' ='grey30',
  '1.5C non-CP' ='#5ca1b6',
  '1.5C CP' = '#e76f51') 


palette_color_cp1 <-  c('Baseline' ='grey30',
                        '1.5C non-CP' ='#5ca1b6',
                        '1.5C CP' = '#e76f51',
                        '2C non-CP' ='#5ca1b6',
                        '2C CP' = '#e76f51') 


palette_color_SSP <- c('SSP1' ='#94d2bd',
                      'SSP2' ='#5ca1b6',
                      'SSP3' = '#e9c46a') 



palette_color_component <- c("Additional cost" = '#de786a', 
                             "Carbon tax" =  '#3D405B', 
                             "Indirect tax" = "#81B29A")

palette_eff <- c('Income' = "#E9C46A", # "#e9d8a6",
                 'Indirect price' = "#81b29a", #"#94d2bd",
                 'Direct tax' = "#3d405b")

palette_Prm <- c('Coal (without CCS)' = '#bb3e03', #'#9999CC',
                 'Coal (with CCS)' =  '#e76f51',#CCCCFF',
                 'Oil (without CCS)' = '#ff9100', #'#CC9966',
                 'Oil (with CCS)' = '#f4a261',
                 'Gas (without CCS)' = '#e9c46a',
                 'Gas (with CCS)' = '#ffcb69',
                 'Hydropower' = '#287271',
                 'Nuclear' = '#9fa0ff',
                 'Solar' = '#f6cacc',
                 'Wind' = '#9dcee2',
                 'Geothermal' = '#264653',
                 'Biomass (without CCS)' = '#8ab17d',
                 'Biomass (with CCS)' = '#94d2bd',
                 'Others' = '#001219'
)

palette_Sec <- c('Coal' = '#bb3e03', #'#9999CC',
                 'Oil' = '#ff9100', #'#CC9966',
                 'Gas' = '#e9c46a',
                 'Heat' = '#2a9d8f',
                 'Hydrogen' = '#287271',
                 "Electricity" = '#90dbf4',
                 "Biomass" = '#8ab17d',
                 'Others' = '#001219')


palette_Fin <- c('Others' = '#001219',
                 'Biomass' = '#8ab17d',
                 'Electricity'= '#90dbf4',
                 'Geothermal' = '#264653',
                 'Heat' = '#2a9d8f',
                 'Hydrogen' = '#287271', 
                 'Gas' = '#e9c46a',
                 'Liquid' = '#f4a261',
                 'Solids' = '#e76f51')

palette_FinRes <- c('Others' = '#001219',
                    'Biomass' = '#8ab17d',
                    'Electricity'= '#90dbf4',
                    'Heat' = '#2a9d8f',
                    'Hydrogen' = '#287271', 
                    'Gas' = '#e9c46a',
                    'Liquid' = '#f4a261',
                    'Solids' = '#e76f51')

palette_sector <- c("AFOLU" = "#073b4c",
                    "Building" = "#b8c0ff",
                    "Transport" = "#118ab2",
                    "Industry" = "#ffd166",
                    "Others" = "#ef476f")


palette_I_abb <- c('Food' = '#FF3366', #'#9999CC', 
                   'Alcohol&Others' = '#CC66FF', #CCCCFF',
                   'Clothing' = '#CC6699', # 
                   'Residentials' = '#FF9966', 
                   'Equipment' = '#CCFF00', 
                   'Health' = '#3399FF', 
                   'Transport' = '#33CC33', 
                   'Communication' = '#CC0000', 
                   'Recreation' = '#FFCC00', 
                   'Education' = '#66CCFF', 
                   'Restaurant&Hotel' = '#0099CC', 
                   'Miscellaneous' = '#999999')




palette_COM <- c('Food' = '#e07a5f', #'#9999CC', 
                   # 'Alcohol&Others' = '#CC66FF', #CCCCFF',
                   # 'Clothing' = '#CC6699', # 
                   'Energy' = '#81b29a', 
                   'Tranport' = '#577590'
                   # 'Health' = '#3399FF', 
                   # 'Transport' = '#33CC33', 
                   # 'Communication' = '#CC0000', 
                   # 'Recreation' = '#FFCC00', 
                   # 'Education' = '#66CCFF', 
                   # 'Restaurant&Hotel' = '#0099CC', 
                   # 'Others' = '#999999'
                 )

palette_TH <- c('1.9-threshold' = '#990000',
                '3.2-threshold' = '#CCCC33',
                '5.5-threshold' = '#CC99FF',
                'Relative poverty' ='#009966' )

palette_color_R5 <- c("R5ASIA" = "#F2CC8F", 
                      "R5MAF" = "#E07A5F",
                      "R5LAM" = "#81B29A",
                      "R5REF" = "#A1D6F1",
                      "R5OECD90+EU" = "#5B5F86")

palette_color_R17 <- c("BRA",
                       "CAN",
                       "CHN" ,
                       "CIS",
                       "IND",
                       "JPN",
                       "TUR",
                       "USA",
                       "XAF",
                       "XE25",
                       "XER",
                       "XLM",
                       "XME",
                       "XNF",
                       "XOC",
                       "XSA",
                       "XSE" )

# themes ----

MyTheme <- theme(
  axis.text.x = element_text(vjust = 0.5,
                             hjust = 0.5,
                             angle = 0,
                             size = 9),
  axis.text.y = element_text(vjust = 0.5,
                             hjust = 0.5,
                             angle = 0,
                             size = 9),
  axis.line = element_line(colour = "grey40"),
  panel.background =element_rect(fill = "white"),
  panel.spacing = unit(1, "lines")
) + ggthemes::theme_pander() + 
  theme(#text = element_text(size = 9), 
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 9, color = "grey40"),
        axis.title.x = element_text(size = 9, colour = "grey40"),
        axis.title.y = element_text(size = 9, colour = "grey40"),
        panel.grid.minor = element_blank(),
        legend.margin =margin(r=2,l=5,t=2,b=-2),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.key.spacing.y = unit(.002, "cm"),
        strip.text = element_text(size = 9),
        legend.box.background = element_rect(color = NA), 
        panel.border = element_rect(colour = "grey50"))


Theme_tran <- theme(
  panel.background = element_rect(fill='transparent', color = NA), #transparent panel bg
  plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
  panel.grid.major = element_blank(), #remove major gridlines
  panel.grid.minor = element_blank(), #remove minor gridlines
  legend.background = element_rect(fill='transparent', color = NA), #transparent legend bg
  legend.box.background = element_rect(fill='transparent', color = NA), #transparent legend panel
  panel.border = element_rect(colour = "grey50")
  # legend.box = element_line(color = 'transparent')
)
