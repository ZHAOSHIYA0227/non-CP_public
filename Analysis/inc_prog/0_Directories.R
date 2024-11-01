# This file stores the directory for PHI analysis
# The system directory are often different by user
# By Shiya Zhao, 2022/02/16

# prog_loc <- "E:/szhao/model/AIMPHI/analysis/R/"
# setwd(paste0(prog_loc,"carbon_tax_distribution/"))
# 

# input directory ----


# output directory ----
dir_fig <- paste0("../", prog_loc,"/output/fig/")
dir_csv <- paste0("../", prog_loc,"/output/csv/")
dir_output <- paste0("../", prog_loc,"/output/")
dir_def <- paste0("../", prog_loc,"/define/")
dir.create(dir_output)
dir.create(dir_fig)
dir.create(dir_csv)



# dir_phi <- "../../../data/phioutput/gdx"
# lis_gdxphi <- list.files(dir_phi,pattern = ".gdx", full.names = TRUE)
# #view(lis_gdxphi)
