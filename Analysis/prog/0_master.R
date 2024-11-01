# This is the master Rscript for the non-CPs
# Shiya ZHAO, 2024.02.21


#loading packages ----
require(gdxrrw)
require(ggplot2)
require(tidyverse)
require(gdata)
require(grid)



# switch ------------------------------------------------------------------

#------------------#
# to plot ribbon or line for baseline hunger and poverty in 2_main_GDP_consumption_poverty_inequality....R 
sw_base_ribbon <- F
#------------------#

#------------------#
# decomposition analysis in 3_main_factors.R 
# leave only world, R5ASIA, R5MAF, and R5OTHER
switch_oth <- T

# leave world and all R5 regions
switch_oth <- F

# leave only world
switch_oth <- "W"

# cross time dataset (T) or 2030 only (2030) or 2050 and 2030 (3050)
cross_time <- "2030"
#------------------#



#------------------#
# to read decile data from .Rdata or .gdx file in 3_EVGini.R
# sw_decile <-  "Rdata"
sw_decile <-  "gdx"
#------------------#


prog_loc <- "model"
dir_wd <- "/Users/shiyazhao/Desktop/Model/AIMPHI/VoluntaryAction/Analysis/exe"
setwd(dir_wd)
source(paste0("../", prog_loc, '/inc_prog/igdx_GAMS_PATH.r'))
getwd()

# output directory 

## Directory assignment and data loading 
# CGE output 
dir_cgeoutput <- "../../DataArchive/cgeoutput/240627/"
cge_ana <- paste0(dir_cgeoutput, "analysis.gdx")
cge_var <- paste0(dir_cgeoutput, "global_17_iamc.gdx")

# PHI output
AIDADSCalib <- 	"../../DataArchive/PHIoutput/gdx/AIDADSCalib/AIDADSCalibNationResults.gdx"
AnaExp <- "../../DataArchive/PHIoutput/gdx/AnalysisExpenditure.gdx"
AnaInc <- "../../DataArchive/PHIoutput/gdx/AnalysisIncome.gdx"
demand <- "../../DataArchive/PHIoutput/gdx/ConsumptionResults.gdx"
decile <- "../../DataArchive/PHIoutput/gdx/Decile.gdx"

source(paste0("../", prog_loc, '/inc_prog/0_PlotSettings.R'))
source(paste0("../", prog_loc, '/inc_prog/0_Functions.R'))
source(paste0("../", prog_loc, '/inc_prog/0_Directories.R'))
source(paste0("../", prog_loc, '/inc_prog/0_Maps.R'))



# 0. load data ------------------------------------------------------------
# cgeoutput

lis_R <- c("World", "R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")
lis_R17 <- c("USA", "XE25", "XER", "TUR", "XOC", "CHN", "IND", "JPN", "XSE", "XSA", "CAN", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
lis_Rphi <- unique(map_R17$R)
  
lis_Y <- c(seq(2020,2100,10))



##########################################################################
# 1. Macro ----------------------------------------------------------------


source(paste0("../", prog_loc, '/prog/1_Emission_tax_GDP.R'))

# source(paste0("../", prog_loc, '/prog/1_1_EnergySystem.R'))

source(paste0("../", prog_loc, '/prog/1_2_Prices.R'))

# 2. Poverty headcount ----------------------------------------------------------------
lis_eff <- c('Income','Indirect price', 'Direct tax')
# lis_ref <- lis_scenario
lis_ref <- c("Base","1p5C","1p5C_expt","1p5C_woc")

source(paste0("../", prog_loc, '/prog/2_headcounts.R'))

source(paste0("../", prog_loc, '/prog/2_1_hungerrisk.R'))



# 3. EV ----------------------------------------------------------------
a <- rgdx.param(AnaExp, "Gini_exp") 
lis_R_phi <- unique(a$R)
rm(a)
source(paste0("../", prog_loc, '/prog/3_EVGini.R'))


# 4. Expenditure ----------------------------------------------------------------
source(paste0("../", prog_loc, '/prog/4_Expenditure.R'))


# 6. Positioning ----------------------------------------------------------------
source(paste0("../", prog_loc, '/prog/8_Position.R'))




# 12. Price and consumption share ----------------------------------------------------------------
source(paste0("../", prog_loc, '/prog/12_Price_ConsumptionShare.R'))


##########################################################################

# 0. plots in the main text -------------------------------------------------
# World map
source(paste0("../", prog_loc, '/inc_prog/0_PlotWorldMap/0_PlotWorldMap.R'))


# Filter

F_filter1 <- function(x){
  y <- x %>% filter(R == "World", SSP == "SSP2", policy == "1.5C non-CP", tech == "None", Gini == "consistent")
  return(y)
}


lis_R <- c("World", "R5ASIA", "R5MAF", "R5LAM", "R5REF", "R5OECD90+EU")

dir_plot_main <- paste0(dir_fig, "/00_main/")
dir.create(dir_plot_main)

source(paste0("../", prog_loc, '/prog/maintext/1_main_energy_emissions_land.R'))

source(paste0("../", prog_loc, '/prog/maintext/2_main_GDP_consumption_poverty_food_inequality.R'))

source(paste0("../", prog_loc, '/prog/maintext/2_1_main_radarplot.R'))

source(paste0("../", prog_loc, '/prog/maintext/2_2_main_globalMap.R'))

source(paste0("../", prog_loc, '/prog/maintext/2_3_main_hunger risk country.R'))

source(paste0("../", prog_loc, '/prog/maintext/3_main_factors.R'))

source(paste0("../", prog_loc, '/prog/maintext/4_sensitivity_analysis.R'))
