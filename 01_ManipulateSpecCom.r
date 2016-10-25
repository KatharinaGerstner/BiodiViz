plot.sim.com <- function(S.pool, N.pool, spat.agg, evenness){
  sim.com <- Sim.Thomas.Community(S = S.pool, N = N.pool, sigma=spat.agg, cv = evenness)
  
  rel.size <- 0.6
  # spatial point pattern plot
  spat.plot <- ggplot(sim.com, aes(X,Y, color=SpecID)) + 
    geom_point() + 
    guides(color="none") +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(axis.text = element_text(size = rel(0.6)), 
          axis.title = element_text(size = rel(0.6)), 
          axis.text = element_text(size = rel(0.6)),
          legend.text=element_text(size = rel(0.6)),
          legend.position="bottom",
          plot.title=element_text(size=rel(0.8)))
  print(spat.plot)

  # # species-abundance distribution
  # SAD <- data.frame(specID=names(table(sim.com$SpecID)),abundance=as.integer(table(sim.com$SpecID)))
  # 
  # SAD.plot <- ggplot(data=SAD,aes(abundance)) + 
  #   geom_freqpoly(binwidth=10) +
  #   ggtitle("SAD") +
  #   theme_bw() +
  #   theme(axis.text = element_text(size = rel(0.6)), 
  #         axis.title = element_text(size = rel(0.6)), 
  #         axis.text = element_text(size = rel(0.6)),
  #         legend.text=element_text(size = rel(0.6)),
  #         legend.position="bottom",
  #         plot.title=element_text(size=rel(0.8)))
  # 
  # # species-accumulation-curves
  # SAC <- data.frame(ind=1:sum(SAD$abundance),SR=SAC.coleman(SAD$abundance))
  # SAC.plot <- ggplot(data=SAC,aes(x=ind,y=SR)) + 
  #   geom_line() + 
  #   xlab("# individuals sampled") +
  #   ylab("Species richness") +
  #   ggtitle("SAC") +
  #   theme_bw() +
  #   theme(axis.text = element_text(size = rel(0.6)), 
  #         axis.title = element_text(size = rel(0.6)), 
  #         axis.text = element_text(size = rel(0.6)),
  #         legend.text=element_text(size = rel(0.6)),
  #         legend.position="bottom",
  #         plot.title=element_text(size=rel(0.8)))
  # 
  # # species-area-relationship
  # n <- 100
  # prop.a <- seq(0.05, 0.5, by = 0.05) # size of area samples in proportions of total area 
  # SAR <- data.frame(DivAR(sim.com, prop.A=prop.a, nsamples = n)) # Vector with mean and standard deviation of the following diversity indices: (1) 
  # 
  # SAR.plot <- ggplot(data=SAR,aes(x=propArea,y=meanSpec)) +
  #   geom_line() +
  #   geom_ribbon(aes(ymin=meanSpec-1.96*sdSpec,ymax=meanSpec+1.96*sdSpec),alpha=0.3) +
  #   xlab("sampled area/total area") +
  #   ylab("Species richness") +
  #   ggtitle("SAR") +
  #   labs(linetype='') +
  #   theme_bw() +
  #   theme(axis.text = element_text(size = rel(0.6)), 
  #         axis.title = element_text(size = rel(0.6)), 
  #         axis.text = element_text(size = rel(0.6)),
  #         legend.text=element_text(size = rel(0.6)),
  #         legend.position="bottom",
  #         plot.title=element_text(size=rel(0.8)))
  # 
  # lay <- rbind(c(1,2,3),
  #              c(4,4,4))
  # grid.arrange(SAD.plot, SAC.plot, SAR.plot, spat.plot,ncol=3, nrow=2, heights=c(1, 3), layout_matrix = lay)
  # 
}

manipulate(
  plot.sim.com(S.pool=S, N.pool=N, spat.agg=0.02, evenness=1),
  S = slider(10,50,step=10),
  N = slider(500,1000,step=100),
  evenness = slider(1,3),
  spat.agg = slider(0.02,1))

#