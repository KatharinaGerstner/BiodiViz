plot.sim.com.grid <- function(S.pool, N.pool, spat.agg, evenness, resolution, cell.id){
  set.seed(2299)
  sim.com <- Sim.Thomas.Community(S = S.pool, N = N.pool, sigma=spat.agg, cv = evenness)
  spec.cols <- rainbow(length(levels(sim.com$SpecID)))
  plot(Y ~ X, data =sim.com, col = spec.cols[sim.com$SpecID], cex=0.7,pch = 19,main="")
  
  
  require(raster)
  grid.rast <- raster(extent(c(0,1,0,1)), nrows=resolution, ncols=resolution)
  grid.poly <- as(grid.rast, "SpatialPolygons")
  plot(grid.poly, add=TRUE)
  plot(grid.poly[cell.id], add=TRUE, col="red")
}

plot.sim.com.grid(S.pool=10, N.pool=500, spat.agg=0.02, evenness=1, resolution=10)



manipulate(
  plot.sim.com.grid(S.pool=S, N.pool=N, spat.agg=0.02, evenness=1, 
                    resolution = res, cell.id=cell),
  S = slider(10,50,step=10),
  N = slider(500,1000,step=100),
  evenness = slider(1,3),
  spat.agg = slider(0.02,1),
  res = slider(2, 20),
  cell = slider(1, 100))



