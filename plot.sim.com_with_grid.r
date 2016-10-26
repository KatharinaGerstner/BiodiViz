plot.sim.com.grid <- function(S.pool, N.pool, spat.agg, evenness, resolution, cell.id){
  set.seed(229376)
  sim.com <- Sim.Thomas.Community(S = S.pool, N = N.pool, sigma=spat.agg, cv = evenness)

  require(raster)
  require(broom)

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

  SAD.global <- data.frame(specID = names(table(sim.com$SpecID)),
                    abundance = as.integer(table(sim.com$SpecID)))
  SAD.cell <-  data.frame(specID = names(table(sim.com.cell$SpecID)),
                    abundance = as.integer(table(sim.com.cell$SpecID)))
  SAD.cell <- SAD.cell[SAD.cell$abundance != 0,]
  
  SAD.plot.global <- ggplot(data=SAD.global, aes(abundance)) +
    geom_histogram(binwidth=10) +
    ggtitle("SAD_global") +
    geom_histogram(data=SAD.cell, binwidth=10, fill="red") +
    theme_bw() +
    theme(axis.text = element_text(size = rel(0.6)),
          axis.title = element_text(size = rel(0.6)),
          axis.text = element_text(size = rel(0.6)),
          legend.text = element_text(size = rel(0.6)),
          legend.position = "bottom",
          plot.title = element_text(size=rel(0.8)))
  
  SAD.plot.cell <- ggplot(data=SAD.cell, aes(abundance)) +
    geom_histogram(fill="red") +
    ggtitle("SAD_cell") +
    theme_bw() +
    theme(axis.text = element_text(size = rel(0.6)),
          axis.title = element_text(size = rel(0.6)),
          axis.text = element_text(size = rel(0.6)),
          legend.text = element_text(size = rel(0.6)),
          legend.position = "bottom",
          plot.title = element_text(size=rel(0.8)))
  
  
  spat.plot <- ggplot(sim.com, aes(X,Y, color=SpecID)) +
    geom_point() +
    guides(color="none") +
    xlab("") +
    ylab("") +
    theme_classic() +
    geom_polygon(data=grid.poly.gg, aes(y=lat, x=long, group=group), 
                 colour="grey", fill=NA) +
    geom_polygon(data=cell.poly.gg, aes(y=lat, x=long, group=group), 
                 colour="red", fill=NA, size=2) +
    theme(axis.text = element_text(size = rel(0.6)),
          axis.title = element_text(size = rel(0.6)),
          axis.text = element_text(size = rel(0.6)),
          legend.text=element_text(size = rel(0.6)),
          legend.position="bottom",
          plot.title=element_text(size=rel(0.8)))

    lay <- rbind(c(1,2),
                 c(3,3))
     grid.arrange(SAD.plot.global, SAD.plot.cell, spat.plot,
                  ncol=2, nrow=2, heights=c(1, 3), 
                  layout_matrix = lay)
}

plot.sim.com.grid(S.pool=10, N.pool=500, spat.agg=0.02, evenness=1, resolution=10, cell.id=1)

# comment

manipulate(
  plot.sim.com.grid(S.pool=S, N.pool=N, spat.agg=spat.agg, evenness=evenness, 
                    resolution = res, cell.id=cell),
  S = slider(10,500,step=10),
  N = slider(500,10000,step=100),
  evenness = slider(1,3),
  spat.agg = slider(0.01,1),
  res = slider(2, 20),
  cell = slider(1, 100))



