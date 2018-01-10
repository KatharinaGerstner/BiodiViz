theme_biodiviz <- function (base_size = 12, base_family = "") {
  theme_bw() +
    theme(axis.title = element_text(size = rel(0.6)),
          axis.text = element_text(size = rel(0.6)),
          legend.text = element_text(size = rel(0.6)),
          legend.position = "bottom",
          plot.title = element_text(size=rel(0.8)))
  }


plot.sim.com.grid <- function(S.pool, S.max=s.max, N.pool, spat.agg, evenness, resolution, cell.id){
  set.seed(229376)
  sim.com <- sim_thomas_community(s_pool = S.pool, n_sim = N.pool, sigma=spat.agg, sad_coef=list(cv_abund = evenness))

  # create the grid
  grid.rast <- raster(extent(c(0,1,0,1)), nrows=resolution, ncols=resolution)
  grid.poly <- as(grid.rast, "SpatialPolygons")

  # extract the focal cell
  cell.poly <- as(grid.poly, "SpatialPolygons")[cell.id]
  
  # extract only the part of sim.com that falls into the focal grid
  is.in <- over(SpatialPoints(sim.com$census[,1:2]), cell.poly)
  is.in[is.na(is.in)] <- 0 
  is.in <- is.in == 1
  sim.com.cell <- sim.com
  sim.com.cell$census <- sim.com.cell$census[is.in,]
  sim.com.cell$census$species <- factor(sim.com.cell$census$species)
  
  # prepare the grid and the cell for ggplot
  grid.poly.gg = tidy(grid.poly)
  cell.poly.gg = tidy(cell.poly)

  ### plot SAD
  SAD.global <- data.frame(specID = names(table(sim.com$census$species)),
                    abundance = as.integer(table(sim.com$census$species)))
  SAD.cell <-  data.frame(specID = names(table(sim.com.cell$census$species)),
                    abundance = as.integer(table(sim.com.cell$census$species)))

  SAD.plot.global <- ggplot(data=SAD.global, aes(abundance)) +
    geom_histogram(bins=ceiling(max(SAD.global$abundance/20))) +
    ggtitle("SAD_global") +
    geom_histogram(data=SAD.cell, bins=ceiling(max(SAD.global$abundance/20)), fill="red") +
    theme_biodiviz() 

  SAD.plot.cell <- ggplot(data=SAD.cell, aes(abundance)) +
    geom_histogram(fill="red") +
    ggtitle("SAD_cell") +
    theme_biodiviz()

  ### plot SAC
  SAC.global <- spec_sample_curve(sim.com, method="accumulation")
  SAC.cell <- spec_sample_curve(sim.com.cell, method="accumulation")
  
  SAC.plot.global <- ggplot(data=SAC.global,aes(x=n,y=spec_accum)) +
    geom_line() +
#    ylim(0,S.max) +
    xlab("# individuals sampled") +
    ylab("Species richness") +
    ggtitle("SAC_global") +
    theme_biodiviz()
  
  SAC.plot.cell <- ggplot(data=SAC.cell,aes(x=n,y=spec_accum)) +
    geom_line() +
#   ylim(0,S.max) +
    xlab("# individuals sampled") +
    ylab("Species richness") +
    ggtitle("SAC_cell") +
    theme_biodiviz()
  
  ### plot SAR
  n <- 50
  prop.a <- c(0.01,seq(0.1, 1, by = 0.1)) # size of area samples in proportions of total area
  SAR.global <- data.frame(divar(sim.com, prop_area=prop.a, n_samples = n)) # Vector with mean and standard deviation of the following diversity indices: (1)
  SAR.cell <- data.frame(divar(sim.com.cell, prop_area=prop.a, n_samples = n)) # Vector with mean and standard deviation of the following diversity indices: (1)
  
  SAR.plot.global <- ggplot(data=SAR.global,aes(x=prop_area,y=m_species)) +
    geom_line() +
    geom_line(data=SAR.cell,aes(x=prop_area/resolution^2,y=m_species),col="red", size=1.5) +
    geom_ribbon(aes(ymin=m_species-1.96*sd_species,ymax=m_species+1.96*sd_species),alpha=0.3) +
    xlab("sampled area/total area") +
    ylab("Species richness") +
    ggtitle("SAR_global") +
    theme_biodiviz()
  
  SAR.plot.cell <- ggplot(data=SAR.cell,aes(x=prop_area,y=m_species)) +
    geom_line() +
    xlab("sampled area/total area") +
    ylab("Species richness") +
    ggtitle("SAR_cell") +
    theme_biodiviz()
  
  # plot community
  spat.plot <- ggplot(sim.com$census, aes(x,y, color=species)) +
    geom_point() +
    guides(color="none") +
    xlab("") +
    ylab("") +
    geom_polygon(data=grid.poly.gg, aes(y=lat, x=long, group=group), 
                 colour="grey", fill=NA, size=1.2) +
    geom_polygon(data=cell.poly.gg, aes(y=lat, x=long, group=group), 
                 colour="red", fill=NA, size=2) +
    theme_biodiviz()
  
  # plot all at once
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

manipulate(
  plot.sim.com.grid(S.pool=S, S.max=500, N.pool=N, spat.agg=spat.agg, evenness=evenness, 
                    resolution = res, cell.id=cell),
  S = slider(10,500,step=10),
  N = slider(5000,100000,step=1000),
  evenness = slider(0.5,3,step=0.5),
  spat.agg = slider(0.01,1),
  cell = slider(1, 400), # give error if cell > res^2 but manipulate doesn't work with dependent sliders, e.g cell=slider(1,res^2)
  res = slider(2, 20))


#plot.sim.com.grid(S.pool=10, N.pool=500, spat.agg=0.02, evenness=1, resolution=10, cell.id=1)

# sim.com <- Sim.Thomas.Community(S = 10, N = 100, sigma=0.02, cv = 1)
# s.max <- 10
# resolution <- 4
# cell.id <- 2
