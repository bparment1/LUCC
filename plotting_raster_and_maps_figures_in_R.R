x <- seq(pi/4, 5 * pi, length.out = 100)
y <- seq(pi/4, 5 * pi, length.out = 100)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x=x, y=y)
grid$z <- cos(r^2) * exp(-r/(pi^3))
levelplot(z~x*y, grid, cuts = 50, 
          scales=list(log="e",x=list(cex=.3),y=list(cex=.3)), xlab=list(cex=2),
          ylab=list(cex=.25), main=list(label="Weird Function", cex=5), sub="with log scales",
          colorkey = FALSE, region = TRUE)

f <- system.file("external/test.grd", package="raster")
r <- raster(f)
levelplot(r)
s <- stack(r, r+500, r-500)
levelplot(s)
#myTheme=rasterTheme(region=sequential_hcl(10, power=2.2))
myTheme <- rasterTheme(axis.text = list(font = 2, cex = 1),region=rev(terrain.colors(15)))
myTheme <- Theme(axis.text = list(font = 2, cex = 1),region=rev(terrain.colors(15)))
levelplot(s,par.settings=myTheme)
          

levelplot(s,par.settings = list(axis.text = list(font = 2, cex = 1)),col.regions=terrain.colors(10))
temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
levelplot(s,par.settings = list(axis.text = list(font = 2, cex = 1)),col.regions=temp.colors(15))

levelplot(s,par.settings = list(axis.text = list(font = 2, cex = 1),
                                strip.background=list(col="green")),par.strip.text=list(cex=2),
          col.regions=temp.colors(15))

#strip=strip.custom(par.strip.text=list(cex=2), bg=NA))

#### ANother test
library(dichromat)
myTheme <- rasterTheme(region=dichromat(terrain.colors(15)))
levelplot(r, par.settings=myTheme)
names(trellis.par.get())  #get paramters list from trellis
trellis.par.get()[31] #show paramters 32
names(axis.text.get())
show.settings() #Show the current lattice settings 
trellis.par.get()[29] #show paramters 32

################# MULTIPLE PANNEL WITH DATA FRAME...

require(lattice)
data(barley)
xyplot(yield ~ year | site, data = barley)
xyplot(yield ~ year, data = barley) #Without mulitple pannel...

library("lattice")
x <- seq(pi/4, 5*pi, length.out=10)
y <- seq(pi/4, 5*pi, length.out=10)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x=x, y=y)
grid$z <- cos(r^2)*exp(-r/(pi^3))

p <- levelplot(z~x*y, grid, 
               panel=function(...) {
                 arg <- list(...)
                 panel.levelplot(...)
                 panel.text(arg$x, arg$y, round(arg$z,1))})
print(p)







