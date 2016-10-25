############################################################################
### 0. Set working directories
############################################################################
.setwdntemp <- function(){
  cu <- Sys.info()["user"]
  cn <- Sys.info()["nodename"]
  
  if(cu==""){
    # PK
  }else {
    path2wd <- "C:/Users/kg83hyby/Documents/GitHub/BiodiViz/" #KG
    path2temp <- "C:/Users/kg83hyby/Documents/GitHub/BiodiViz/temp/" #KG 
  }  
  return(list(path2temp,path2wd))
}

set.list <-  .setwdntemp()
path2temp <- set.list[[1]]
path2wd <- set.list[[2]]

############################################################################
### 0. Load and install all needed libraries
############################################################################
needed_libs <- c("devtools", # download from github
                 "MoBspatial", # simulation of species communities
#                 "iNEXT", # computes diversity estimates for rarefied and extrapolated samples
                 "ggplot2",
                  "gridExtra", # for multiple plots using grid.arrange()
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