plot.sim.com.grid <- function(S.pool, N.pool, spat.agg, evenness, resolution, cell.id){
  set.seed(2299)
  sim.com <- Sim.Thomas.Community(S = S.pool, N = N.pool, sigma=spat.agg, cv = evenness)

  require(raster)
  require(ggplot2)
  require(broom)
  require(plyr)
  require(rgeos)
  # create the grid
  grid.rast <- raster(extent(c(0,1,0,1)), nrows=resolution, ncols=resolution)
  grid.poly <- as(grid.rast, "SpatialPolygonsDataFrame")
  grid.poly@data$id = 1:nrow(grid.poly@data)

  # extract the focal cell
  cell.poly <- grid.poly[cell.id]
  
  # prepare the grid and the cell for ggplot
  grid.poly.gg = tidy(grid.poly)
  cell.poly.gg = tidy(cell.poly)


  spat.plot <- ggplot(sim.com, aes(X,Y, color=SpecID)) +
    geom_point() +
    guides(color="none") +
    xlab("") +
    ylab("") +
    theme_classic() +
    geom_polygon(data=grid.poly.gg, aes(y=lat, x=long, group=group), 
                 colour="grey", fill=NA) +
    theme(axis.text = element_text(size = rel(0.6)),
          axis.title = element_text(size = rel(0.6)),
          axis.text = element_text(size = rel(0.6)),
          legend.text=element_text(size = rel(0.6)),
          legend.position="bottom",
          plot.title=element_text(size=rel(0.8)))

   spat.plot
}

plot.sim.com.grid(S.pool=10, N.pool=500, spat.agg=0.02, evenness=1, resolution=10)

# comment

manipulate(
  plot.sim.com.grid(S.pool=S, N.pool=N, spat.agg=spat.agg, evenness=evenness, 
                    resolution = res, cell.id=cell),
  S = slider(10,50,step=10),
  N = slider(500,1000,step=100),
  evenness = slider(1,3),
  spat.agg = slider(0.02,1),
  res = slider(2, 20),
  cell = slider(1, 100))



