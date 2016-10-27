theme_biodiviz <- function (base_size = 12, base_family = "") {
  theme_bw() +
    theme(axis.text = element_text(size = rel(0.6)),
          axis.title = element_text(size = rel(0.6)),
          axis.text = element_text(size = rel(0.6)),
          legend.text = element_text(size = rel(0.6)),
          legend.position = "bottom",
          plot.title = element_text(size=rel(0.8)))
  }


plot.sim.com.grid <- function(S.pool, S.max=s.max, N.pool, spat.agg, evenness, resolution, cell.id){
  set.seed(229376)
  sim.com <- Sim.Thomas.Community(S = S.pool, N = N.pool, sigma=spat.agg, cv = evenness)[[1]]

  # create the grid
  grid.rast <- raster(extent(c(0,1,0,1)), nrows=resolution, ncols=resolution)
  grid.poly <- as(grid.rast, "SpatialPolygons")

  # extract the focal cell
  cell.poly <- as(grid.poly, "SpatialPolygons")[cell.id]
  
  # extract only the part of sim.com that falls into the focal grid
  is.in <- over(SpatialPoints(sim.com[,1:2]), cell.poly)
  is.in[is.na(is.in)] <- 0 
  is.in <- is.in == 1
  sim.com.cell <- sim.com[is.in,]
  
  # prepare the grid and the cell for ggplot
  grid.poly.gg = tidy(grid.poly)
  cell.poly.gg = tidy(cell.poly)

  SAD.global <- data.frame(specID = names(table(sim.com$Species)),
                    abundance = as.integer(table(sim.com$Species)))
  SAD.cell <-  data.frame(specID = names(table(sim.com.cell$Species)),
                    abundance = as.integer(table(sim.com.cell$Species)))
  SAD.cell <- SAD.cell[SAD.cell$abundance != 0,]
  
  SAD.plot.global <- ggplot(data=SAD.global, aes(abundance)) +
    geom_histogram(bins=ceiling(max(SAD.global$abundance/20))) +
    ggtitle("SAD_global") +
    geom_histogram(data=SAD.cell, bins=ceiling(max(SAD.global$abundance/20)), fill="red") +
    theme_biodiviz() 

  SAD.plot.cell <- ggplot(data=SAD.cell, aes(abundance)) +
    geom_histogram(fill="red", bins=ceiling(max(SAD.cell$abundance/20))) +
    ggtitle("SAD_cell") +
    theme_biodiviz()

  SAC.global <- data.frame(ind=1:sum(SAD.global$abundance),SR=SAC(SAD.global$abundance))
  SAC.cell <- data.frame(ind=1:sum(SAD.cell$abundance),SR=SAC(SAD.cell$abundance))
  
  SAC.plot.global <- ggplot(data=SAC.global,aes(x=ind,y=SR)) +
    geom_line() +
    ylim(0,S.max) +
    xlab("# individuals sampled") +
    ylab("Species richness") +
    ggtitle("SAC_global") +
    theme_biodiviz()
  
  SAC.plot.cell <- ggplot(data=SAC.cell,aes(x=ind,y=SR)) +
    geom_line() +
#   ylim(0,S.max) +
    xlab("# individuals sampled") +
    ylab("Species richness") +
    ggtitle("SAC_cell") +
    theme_biodiviz()
  
  ### SAR
  n <- 50
  prop.a <- seq(0.05, 0.5, by = 0.05) # size of area samples in proportions of total area
  SAR.global <- data.frame(DivAR(sim.com, prop.A=prop.a, nsamples = n)) # Vector with mean and standard deviation of the following diversity indices: (1)
  SAR.cell <- data.frame(DivAR(sim.com.cell, prop.A=prop.a, nsamples = n)) # Vector with mean and standard deviation of the following diversity indices: (1)
  
  SAR.plot.global <- ggplot(data=SAR.global,aes(x=propArea,y=meanSpec)) +
    geom_line() +
    geom_ribbon(aes(ymin=meanSpec-1.96*sdSpec,ymax=meanSpec+1.96*sdSpec),alpha=0.3) +
    ylim(0,S.max) +
    xlab("sampled area/total area") +
    ylab("Species richness") +
    ggtitle("SAR") +
    theme_biodiviz()
  
  SAR.plot.cell <- ggplot(data=SAR.cell,aes(x=propArea,y=meanSpec)) +
    geom_line() +
    geom_ribbon(aes(ymin=meanSpec-1.96*sdSpec,ymax=meanSpec+1.96*sdSpec),alpha=0.3) +
#    ylim(0,S.max) +
    xlab("sampled area/total area") +
    ylab("Species richness") +
    ggtitle("SAR") +
    theme_biodiviz()
  
  spat.plot <- ggplot(sim.com, aes(X,Y, color=Species)) +
    geom_point() +
    guides(color="none") +
    xlab("") +
    ylab("") +
    geom_polygon(data=grid.poly.gg, aes(y=lat, x=long, group=group), 
                 colour="grey", fill=NA, size=1.2) +
    geom_polygon(data=cell.poly.gg, aes(y=lat, x=long, group=group), 
                 colour="red", fill=NA, size=2) +
    theme_biodiviz()

    lay <- rbind(c(1,2),
                 c(3,4),
                 c(5,6),
                 c(7,7))
     grid.arrange(SAD.plot.global, SAD.plot.cell, 
                  SAC.plot.global, SAC.plot.cell,
                  SAR.plot.global, SAR.plot.cell,
                  spat.plot,
                  ncol=ncol(lay), nrow=nrow(lay), heights=c(1, 1, 1, 3), 
                  layout_matrix = lay)
}

#plot.sim.com.grid(S.pool=10, N.pool=500, spat.agg=0.02, evenness=1, resolution=10, cell.id=1)

# comment

S.max <- 500
manipulate(
  plot.sim.com.grid(S.pool=S, S.max=S.max, N.pool=N, spat.agg=spat.agg, evenness=evenness, 
                    resolution = res, cell.id=cell),
  S = slider(10,500,step=10),
  N = slider(5000,100000,step=1000),
  evenness = slider(0.5,3,step=0.5),
  spat.agg = slider(0.01,1),
  cell = slider(1, 400), # give error if cell > res^2 but manipulate doesn't work with dependent sliders, e.g cell=slider(1,res^2)
  res = slider(2, 20))


 # sim.com <- Sim.Thomas.Community(S = 10, N = 100, sigma=0.02, cv = 1)[[1]]
 # s.max <- 10
 # resolution <- 4
 # cell.id <- 2
