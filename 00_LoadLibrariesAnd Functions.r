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
                 "mobsim", # simulation of species communities
                 "ggplot2", # for plotting
                  "gridExtra", # for multiple plots using grid.arrange()
                  "manipulate", # interactive plotting
                  "raster", # creating a grid polygon
                  "broom" # converting spatial polygon to dataframe for plotting with ggplot
)
usePackage <- function(p) {
  if(p == "mobsim") {install_github('MoBiodiv/mobsim')}  
  if (!is.element(p, installed.packages()[,1]))    install.packages(p, dep = TRUE) # installs packages if not yet available
  require(p, character.only = TRUE) # load packages
}
sapply(needed_libs,usePackage)

rm(needed_libs, usePackage)