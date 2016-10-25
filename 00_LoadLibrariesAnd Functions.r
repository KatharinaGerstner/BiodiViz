############################################################################
### 0. Set working directories
############################################################################
setwd("c:/Users/kg83hyby/Documents/2016_ScalingInMetaAnalyses/DataAnalysis/")

############################################################################
### 0. Load and install all needed libraries
############################################################################
needed_libs <- c("devtools", # download from github
                 "MoBspatial", # simulation of species communities
#                 "iNEXT", # computes diversity estimates for rarefied and extrapolated samples
                 "ggplot2",
                  "manipulate"
#                 "reshape2", # melt lists and dataframes
#                 "vegan" # for diversity indices
)
usePackage <- function(p) {
  if(p == "MoBspatial")    install_github('MoBiodiv/MoBspatial')    
#  if(p=="iNEXT")   install_github('JohnsonHsieh/iNEXT')
  if (!is.element(p, installed.packages()[,1]))    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
sapply(needed_libs,usePackage)

rm(needed_libs, usePackage)