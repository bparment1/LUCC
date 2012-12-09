library(maptools)
library(sp)
library(rasterVis)
data(meuse)
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS("+init=epsg:28992")
data(meuse.riv)
river_polygon <- Polygons(list(Polygon(meuse.riv)), ID="meuse")
rivers <- SpatialPolygons(list(river_polygon))
proj4string(rivers) <- CRS("+init=epsg:28992")
rivers1 <- elide(rivers, reflect=c(TRUE, TRUE), scale=TRUE)
meuse1 <- elide(meuse, bb=bbox(rivers), reflect=c(TRUE, TRUE), scale=TRUE)
opar <- par(mfrow=c(1,2))
plot(rivers, axes=TRUE)
plot(meuse, add=TRUE)
plot(rivers1, axes=TRUE)
plot(meuse1, add=TRUE)
par(opar)
meuse1 <- elide(meuse, shift=c(10000, -10000))
bbox(meuse)
bbox(meuse1)
rivers1 <- elide(rivers, shift=c(10000, -10000))
bbox(rivers)
bbox(rivers1)
meuse1 <- elide(meuse, rotate=-30, center=apply(bbox(meuse), 1, mean))
bbox(meuse)
bbox(meuse1)
plot(meuse1, axes=TRUE)

This is the code:
  > p1<-spplot(Africa,c('var1'))
> p2<-spplot(Sao,c('var1'))
> print(p1,position=c(0,0,1,1),more=T)
> print(p2,position=c(0,0,0.3,0.3),more=T)
>
  > This results is a map of Africa with a second map of Sao in the left
> bottom corner.

#Using sp plot
p <- spplot(ecoreg) 
library(grid)
trellis.focus("panel",column=1,row=1)
scale_position<-as.numeric(grid.locator())
#ids<-panel.identify()
trellis.unfocus()
p+sp.layout("sp.text"
            #scale_position<-locator(type="p")
            #p + layer(sp.polygons(meuse.riv)) 
            #p + layer_(sp.polygons(meuse.riv)) 
            l2 = list("SpatialPolygonsRescale", layout.north.arrow(), offset = scale_position, 
                      scale = 400)
            p+ sp.layout(l2))
#l3 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(180500,329800), 

coordinates(meuse) <- ~x+y
#scale_position<-click(meuse.grid)
l2 = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(181300,329800), 
          scale = 400)
l3 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(180500,329800), 
          scale = 500, fill=c("transparent","black"))
l4 = list("sp.text", c(180500,329900), "0")
l5 = list("sp.text", c(181000,329900), "500 m")

spplot(meuse, c("ffreq"), sp.layout=list(l2,l3,l4,l5), col.regions= "black", 
       pch=c(1,2,3), key.space=list(x=0.1,y=.95,corner=c(0,1)))
spplot(meuse, c("zinc", "lead"), sp.layout=list(l2,l3,l4,l5, which = 2),
       key.space=list(x=0.1,y=.95,corner=c(0,1)))

#Using plot system

filename<-sub(".vct","",infile2b)                                             #Removing the extension from file.
reg_outline<-readOGR(".", filename)

plot(seg_id)
scale_position<-click(ecoreg,xy=TRUE)
scale_position<-scale_position[1,1:2]
arrow_position<-click(ecoreg,xy=TRUE)
arrow_position<-arrow_position[1,1:2]
SpatialPolygonsRescale(layout.scale.bar(), offset = scale_position, 
                       scale = 250000, fill=c("transparent","black"),plot.grid=FALSE)
text(scale_position,"0")
text(scale_position,"250,000")
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 125000, fill=c("transparent","black"),plot.grid=FALSE)
#or use scale bar...



#Now get the crosstab table
#names(lf_change_list_param)<-c("ALB_A0_c","ALB_A1_c","ALB_A2_c","LST_A0_c","LST_A1_c","LST_A2_c","NDVI_A0_c","NDVI_A1_c","NDVI_A2_c")
plot(seg_id)
#scalebar(250000, type = 'bar', divs=4)
#return(sta_change_obj)
#quartz(height=9, width=9)
#tmp <- nb_c_rast > 0
#plot(tmp, col=c("black","red"), legend=FALSE)
#plot_name<-paste(telindex, "and", mode_n,"lag analysis",sep="_")
#png(paste(plot_name,"_",out_prefix,".png", sep=""))
#grid(nx=12,ny=10)
legend("topright",legend=c("no change", "change"),pt.cex=0.9,fill=c("black","red"),bty="n")
scalebar(250000, type = 'bar', divs=4)
#no box around legend

box()
dev.off()

###### CREATE MAP

#Using plot system

filename<-sub(".vct","",infile2b)                                             #Removing the extension from file.
reg_outline<-readOGR(".", filename)

plot(seg_id)
scale_position<-click(ecoreg,xy=TRUE)
scale_position<-scale_position[1,1:2]
arrow_position<-click(ecoreg,xy=TRUE)
arrow_position<-arrow_position[1,1:2]
SpatialPolygonsRescale(layout.scale.bar(), offset = scale_position, 
                       scale = 250000, fill=c("transparent","black"),plot.grid=FALSE)
text(scale_position,"0")
text(scale_position,"250,000")
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 125000, fill=c("transparent","black"),plot.grid=FALSE)
#or use scale bar...

plot(seg_id)
scale_position<-click(ecoreg,xy=TRUE)
scale_position<-scale_position[1,1:2]
arrow_position<-click(ecoreg,xy=TRUE)
arrow_position<-arrow_position[1,1:2]
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=0.8)
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 125000, fill=c("transparent","black"),plot.grid=FALSE)
#Using sp plot
p <- spplot(ecoreg) 
library(grid)
trellis.focus("panel",column=1,row=1)
scale_position<-as.numeric(grid.locator())
#ids<-panel.identify()
trellis.unfocus()
p+sp.layout("sp.text"
            #scale_position<-locator(type="p")
            #p + layer(sp.polygons(meuse.riv)) 
            #p + layer_(sp.polygons(meuse.riv)) 
            l2 = list("SpatialPolygonsRescale", layout.north.arrow(), offset = scale_position, 
                      scale = 400)
            p+ sp.layout(l2))
#l3 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(180500,329800), 

coordinates(meuse) <- ~x+y
#scale_position<-click(meuse.grid)
l2 = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(181300,329800), 
          scale = 400)
l3 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(180500,329800), 
          scale = 500, fill=c("transparent","black"))
l4 = list("sp.text", c(180500,329900), "0")
l5 = list("sp.text", c(181000,329900), "500 m")

spplot(meuse, c("ffreq"), sp.layout=list(l2,l3,l4,l5), col.regions= "black", 
       pch=c(1,2,3), key.space=list(x=0.1,y=.95,corner=c(0,1)))
spplot(meuse, c("zinc", "lead"), sp.layout=list(l2,l3,l4,l5, which = 2),
       key.space=list(x=0.1,y=.95,corner=c(0,1)))

### FIND COLORS IN R
colors()[grep("yellow",colors())]

col_t<-"yelowgreen"
x<-c(1)
hist(x,col=col_t)

pie(rep(1,n), col=rainbow(n))
r<-raster("window_Anaktuvut_river_fire_12232011_rec.rst")
# Define the function
# Define the function
gdal_polygonizeR <- function(x, outshape=NULL, attname='DN', gdalformat = 'ESRI Shapefile', quiet=TRUE) { 
  py.c <- Sys.which('gdal_polygonize.py')
  if (!length(py.c)) stop("Can't find gdal_polygonize.py on your system.")
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbg'), sep='.'))
    if (any(f.exists)) stop(sprintf('File already exists: %s', toString(paste(outshape, c('shp', 'shx', 'dbg'), sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.asc')})
    rast.nm <- normalizePath(f)
  } else if (is.character(x)) {
    rast.nm <- normalizePath(x)
  } else stop('x must be either a file path (as a character string), or a Raster object.')
  full.c <- sprintf("%1$s %2$s -f '%3$s' %4$s.shp %4$s %5$s", py.c, rast.nm, gdalformat, outshape, attname)
  system(full.c)
  shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
  return(shp) 
}

# Time to polygonizeR the raster!
system.time(p <- gdal_polygonizeR(r))
p <- gdal_polygonizeR(r)
