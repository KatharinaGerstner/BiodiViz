
library(manipulate)

# the cars example
manipulate(
   plot(cars, xlim=c(0,x.max)),  
   x.max=slider(15,25))


# mouse click example
dat <- data.frame(x=0, y=0)[-1,]
manipulate({
   plot(c(0,1), c(0, 1), type="n")
   xy <- manipulatorMouseClick()
  if(!is.null(xy)) dat <- rbind(dat, c(xy$userX, xy$userY)); points(dat, pch=19)
   dat})