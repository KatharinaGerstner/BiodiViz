plot.sim.com <- function(S.pool, N.pool, spat.agg, evenness){
  sim.com <- Sim.Thomas.Community(S = S.pool, N = N.pool, sigma=spat.agg, cv = evenness)
  spec.cols <- rainbow(length(levels(sim.com$SpecID)))
  plot(Y ~ X, data =sim.com, col = spec.cols[sim.com$SpecID], cex=0.7,pch = 19,main="")
}

manipulate(
  plot.sim.com(S.pool=S, N.pool=N, spat.agg=0.02, evenness=1),
  S = slider(10,50,step=10),
  N = slider(500,1000,step=100),
  evenness = slider(1,3),
  spat.agg = slider(0.02,1))
