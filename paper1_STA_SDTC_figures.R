###########  PLOTTING FIGURES FOR THE PAPER ##############
### FIRST READ IN ECOREGION INFORMATION
out_prefix <- "_05082013_multicomp_1"

infile_ecoreg<-"wwf_terr_ecos_Alaska.shp"
ecoreg_spdf<-readOGR(dsn=".",sub(".shp","",infile_ecoreg))
proj4string(ecoreg_spdf)
proj_ALB83<-"+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

ecoreg_spdf_ALB83<-spTransform(ecoreg_spdf,CRS(proj_ALB83))
outfile4<-paste("wwf_terr_ecos_Alaska_ALB83","_",out_prefix,".shp",sep="")
writeOGR(ecoreg_spdf_ALB83,dsn=".",layer= sub(".shp","",outfile4), driver="ESRI Shapefile",overwrite_layer=TRUE)
cat_names<-unique(ecoreg_spdf_ALB83$ECO_NAME)

nb_col<-length(unique(cat_names))
# Wrong order in terms of the categories of ecoreg so assign them
cat_names<-c("Alaska Peninsula montane taiga",
             "Alaska St Elias Range tundra",
             "Aleutian Islands tundra",
             "Arctic coastal tundra",
             "Arctic foothills tundra",
             "Beringia lowland tundra",
             "Beringia upland tundra",
             "Brooks-British Range tundra",
             "Cook Inlet taiga",
             "Copper Plateau taiga",
             "Interior Alaska-Yukon lowland taiga",
             "Interior Yukon-Alaska alpine tundra",
             "Northern Cordillera forests",
             "Northern Pacific coastal forests",
             "Ogilvie-Mackenzie alpine tundra",
             "Pacific Coastal Mountain icefields and tundra",
             "Rock and Ice")

infile<-"wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83.shp"
ecoreg_spdf<-readOGR(dsn=".",sub(".shp","",infile))
ecoreg_spdf$ECO_NAME<-cat_names
#problem with extent, ecoreg_spdf is not the same extent as raster images!!
LSTA1_change<-raster("A_avg_seg_change_Alaska__LST_A1_c_500_01222013b_multicomp_1.rst")
lf_eco<-list.files(pattern="ecoregion_map.rst$")
if (!file.exists(lf_eco[1])){
  ecoreg_rast<-rasterize(ecoreg_spdf,LSTA1_change,"DATA_VALUE")
  projection(ecoreg_rast)<-proj_ALB83
  data_name<-paste("ecoregion_map",sep="")
  raster_name<-paste(data_name,".rst", sep="")
  writeRaster(ecoreg_rast, filename=raster_name,NAflag=-999,overwrite=TRUE)  #Writing the data in a raster file format...
}
if (file.exists(lf_eco[1])){
  ecoreg_rast<-raster(lf_eco)
  projection(ecoreg_rast)<-proj_ALB83
}

###############################
##Figure 1: wwf ecoregion
res_pix<-960
col_mfrow<-1
row_mfrow<-1
png(filename=paste("Figure1_paper1_wwf_ecoreg_Alaska",out_prefix,".png",sep=""),
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)
#par(mfrow=c(1,2))
col_eco<-rainbow(nb_col)
col_eco[16]<-"brown"
col_eco[11]<-"darkgreen"
col_eco[7]<-"lightblue"
col_eco[6]<-"grey"
col_eco[12]<-"yellowgreen"

plot(ecoreg_rast,col=col_eco,legend=FALSE,axes="FALSE")
legend("topright",legend=cat_names,title="WWF ecoregions",
       pt.cex=1.1,cex=1.1,fill=col_eco,bty="n")
scale_position<-c(450000, 600000)
arrow_position<-c(900000, 600000)

label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=1.8)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)
dev.off()

###############
##Figure 2: worflow...

#Figure not created in R

###############
##Figure 3: amplitude image

amplitude_rast<-brick("ndvi_2001_2009_filling6__STA_Amplitudes.rst")
amp0<-raster("ndvi_2001_2009_filling6__STA_amplitude0_TS_slope.rst")
amp1<-raster("ndvi_2001_2009_filling6__STA_amplitude1_TS_slope.rst")
amp2<-raster("ndvi_2001_2009_filling6__STA_amplitude2_TS_slope.rst")
amplitude<-stack(amp0,amp1,amp2)
amplitude_rast<-mask(amplitude_rast,mask_alaska)

#plotRGB(amplitude, r=1, g=2, b=3) #not working because images are not bytes !!!
#plot(amplitude_rast) #multibands 3 amplitudes with A0,A1 and A2
#plotRGB(amplitude_rast, r=1, g=2, b=3,colNA=c("white"))

infile_window1<-"window1_rec.shp"
infile_window2<-"window2_rec.shp"
window2_rast <- brick("window2_ndvi_sta_amplitude_image.rst") #Use brick for multiband!!!!
window1<-readOGR(dsn=".",sub(".shp","",infile_window1))
window2<-readOGR(dsn=".",sub(".shp","",infile_window2))

#set up the output file to plot
res_pix<-960
col_mfrow<-1
row_mfrow<-1
png(filename=paste("Figure3_paper1_amplitude_image_Alaska",out_prefix,".png",sep=""),
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)
par(mfrow=c(1,1))

scale_position<-c(450000, 600000)
arrow_position<-c(900000, 600000)

plotRGB(amplitude_rast, r=1, g=2, b=3,stretch="lin",axes=FALSE,legend=FALSE)
plot(window1,add=TRUE,border="black",lwd=2) #border and lwd are options of graphics package polygon object
plot(window2,add=TRUE,border="black",lwd=2)
#legend("topright",legend=c(0:7),title="Number of change",
#       pt.cex=1.4,cex=2.1,fill=rev(terrain.colors(8)),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=1.8)

#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)
#note that scale in SpatialPolygonRescale sets the size of the north arrow!!
opar <- par(fig=c(0.7, 0.95, 0.5, 0.75), new=TRUE)
plotRGB(window2_rast, r=1, g=2, b=3,stretch="lin",axes=FALSE,legend=FALSE)

dev.off()

###############
##Figure 4: nubmer of change and change OR for 500 seg

#load images to plot
nb_c_500_seg<-raster("A_Change_image9_param_avg_seg_Alaska__500_01222013b_multicomp_1.rst")
nb_c_500_seg_OR<- nb_c_500_seg >0

#set up the output file to plot
res_pix<-960
col_mfrow<-2
row_mfrow<-1
png(filename=paste("Figure4_paper1_nb_c_OR_change_Alaska",out_prefix,".png",sep=""),
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)
par(mfrow=c(1,2))

#set up plotting parameters
#col_pal<-rev(terrain.colors(unique(nb_c_500_seg)))
col_pal<-rev(terrain.colors(8))
scale_position<-c(450000, 600000)
arrow_position<-c(900000, 600000)

##plot Fig 4a
#plot(nb_c_500_seg,legend=FALSE,col=rev(terrain.colors(8)))
plot(nb_c_500_seg,legend=FALSE,col=col_pal,axes="FALSE")
legend("topright",legend=c(0:7),title="Number of change",
       pt.cex=1.4,cex=2.1,fill=rev(terrain.colors(8)),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=1.8)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)
#note that scale in SpatialPolygonRescale sets the size of the north arrow!!

##plot Fig 4b
plot(nb_c_500_seg_OR,legend=FALSE,col=c("black","red"),axes="FALSE")
legend("topright",legend=c("no change","change"),title="Change category",
       pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=1.8)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)
dev.off()

###############################
##Figure 5: change 500 for LST
res_pix<-960
col_mfrow<-1
row_mfrow<-1
png(filename=paste("Figure5_paper1_LSA1_change_Alaska",out_prefix,".png",sep=""),
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)
#par(mfrow=c(1,2))

LSTA1_change<-raster("A_avg_seg_change_Alaska__LST_A1_c_500_01222013b_multicomp_1.rst")
nb_c_500_seg_OR
plot(LSTA1_change,legend=FALSE,col=c("black","red"),axes="FALSE")
legend("topright",legend=c("no change","change"),title="Change category",
       pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n")

label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=1.8)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)
dev.off()

###############################
##Figure 6: barplot land cover categories: change per number and shape parameters

#not created in R...

###############################
##Figure 7

infile_w_Anaktuvut<-"window_Anaktuvut_river_fire_12232011_rec.shp"
w_Anaktuvut<-readOGR(dsn=".",sub(".shp","",infile_w_Anaktuvut))
w_Anaktuvut_fire <-readOGR(dsn=".",sub(".shp","","window_MTBS_Anaktuvut.shp"))
#res_pix<-960
#col_mfrow<-1
#row_mfrow<-1
#png(filename=paste("Figure7_paper1_change_Alaska_fire_perimeters",out_prefix,".png",sep=""),
#    width=col_mfrow*res_pix,height=row_mfrow*res_pix)
#par(mfrow=c(row_mfrow,col_mfrow))

res_pix<-960
col_mfrow<-1
row_mfrow<-1
png(filename=paste("Figure7a_paper1_change_Alaska_fire_perimeters",out_prefix,".png",sep=""),
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)
par(mfrow=c(row_mfrow,col_mfrow))

##Figure 7a: change 500 with fire polygons???

#LSTA1_change<-raster("A_avg_seg_change_Alaska__LST_A1_c_500_01222013b_multicomp_1.rst")
infile_fire<-sub(".shp","","MTBS_AK_2001_2009_IDR_ID.shp")             #Removing the extension from file.
mtbs_pol <- readOGR(".",infile_fire)
nb_c_500_seg_OR
plot(nb_c_500_seg_OR,legend=FALSE,col=c("black","red"),axes="FALSE")
w1_rast<-crop(nb_c_500_seg_OR,w_Anaktuvut)
#legend("topright",legend=c("no change","change"),title="Change category",
#       pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n")
#lty=c( 1,-1,-1), pch=c(-1,15, 1)
legend("topright",legend=c("MTBS fire","no change","change"),
       #pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n") 
       cex=2.1, lwd(2.5,-1,-1),
       lty=c( 1,-1,-1), pch=c(-1,15, 15),col=c("yellowgreen","black","red"),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=1.8)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)
plot(mtbs_pol, add=TRUE,border="yellowgreen")
plot(w_Anaktuvut,add=TRUE,border="white")
opar <- par(fig=c(0.65, 0.9, 0.5, 0.75), new=TRUE)
plot(w1_rast,axes=FALSE,legend=FALSE,col=c("black","red"))
opar <- par(fig=c(0.65, 0.9, 0.5, 0.75), new=TRUE)
plot(w_Anaktuvut_fire,border="yellowgreen",add=TRUE)
dev.off()

##Figure 7b: change 500 with insect polygons???
#Insect_2001_2009_mask_alaska.shp
infile_insect<-sub(".shp","","insect_bool2.shp")             #Removing the extension from file.
insect_pol <- readOGR(".",infile_insect)

png(filename=paste("Figure7b_paper1_change_Alaska_insect_perimeters",out_prefix,".png",sep=""),
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)
par(mfrow=c(row_mfrow,col_mfrow))

plot(nb_c_500_seg_OR,legend=FALSE,col=c("black","red"),axes="FALSE")

#legend("topright",legend=c("no change","change"),title="Change category",
#       pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n")
#lty=c( 1,-1,-1), pch=c(-1,15, 1)
legend("topright",legend=c("Infestation","no change","change"),
       #pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n") 
       cex=2.1, lwd(2.5,-1,-1),
       lty=c( 1,-1,-1), pch=c(-1,15, 15),col=c("yellowgreen","black","red"),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=1.8)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)
plot(insect_pol, add=TRUE,border="yellowgreen")

dev.off()

###############################
##### Figure 7: comibined --change 500 with fire polygons???

res_pix<-960
col_mfrow<-2
row_mfrow<-1

##Figure 7a: change 500 with insect polygons???
#Insect_2001_2009_mask_alaska.shp
infile_insect<-sub(".shp","","insect_bool2.shp")             #Removing the extension from file.
insect_pol <- readOGR(".",infile_insect)

png(filename=paste("Figure7_paper1_change_Alaska_insect_fire_perimeters",out_prefix,".png",sep=""),
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)
par(mfrow=c(row_mfrow,col_mfrow))

plot(nb_c_500_seg_OR,legend=FALSE,col=c("black","red"),axes="FALSE")

#legend("topright",legend=c("no change","change"),title="Change category",
#       pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n")
#lty=c( 1,-1,-1), pch=c(-1,15, 1)
legend("topright",legend=c("Infestation","no change","change"),
       #pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n") 
       cex=2.1, lwd(2.5,-1,-1),
       lty=c( 1,-1,-1), pch=c(-1,15, 15),col=c("yellowgreen","black","red"),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=1.8)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)
plot(insect_pol, add=TRUE,border="yellowgreen")

infile_fire<-sub(".shp","","MTBS_AK_2001_2009_IDR_ID.shp")             #Removing the extension from file.
mtbs_pol <- readOGR(".",infile_fire)
nb_c_500_seg_OR
plot(nb_c_500_seg_OR,legend=FALSE,col=c("black","red"),axes="FALSE")
w1_rast<-crop(nb_c_500_seg_OR,w_Anaktuvut)
#legend("topright",legend=c("no change","change"),title="Change category",
#       pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n")
#lty=c( 1,-1,-1), pch=c(-1,15, 1)
legend("topright",legend=c("MTBS fire","no change","change"),
       #pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n") 
       cex=2.1, lwd(2.5,-1,-1),
       lty=c( 1,-1,-1), pch=c(-1,15, 15),col=c("yellowgreen","black","red"),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=1.8)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)
plot(mtbs_pol, add=TRUE,border="yellowgreen")
plot(w_Anaktuvut,add=TRUE,border="white")

#To make the horizontal dimensions of the graph smaller or to move the graph left or right, 
#adjust the starting and ending x coordinates, given by the first and second positions of 
#the fig value vector. To make the vertical dimensions of the graph smaller or to move 
#the graph up or down, adjust 
#the staring and ending y coordinates given in the third and fourth positions as below.
# place graph one in the bottom left
#par(fig=c(0, .25, 0, .25), mar=c(2,.5,1,.5), mgp=c(0, 1, 0))

#opar <- par(fig=c(0.8, 0.9, 0.6, 0.7), new=TRUE)
opar <- par(fig=c(0.87, 0.5, 0.65, 0.5), new=TRUE)
plot(w1_rast,axes=FALSE,legend=FALSE,col=c("black","red"),box=FALSE)
#opar <- par(fig=c(0.65, 0.9, 0.5, 0.75), new=TRUE)
#opar <- par(fig=c(0.8, 0.9, 0.6, 0.7), new=TRUE)
opar <- par(fig=c(0.87, 0.5, 0.65, 0.5), new=TRUE)

plot(w_Anaktuvut_fire,border="yellowgreen",add=TRUE,box=FALSE)
dev.off()

###############################
##Figure 8: combined-- change 500 for NDVI A0 pix and seg
res_pix<-960
col_mfrow<-2
row_mfrow<-1
png(filename=paste("Figure8_paper1_nb_c_OR_change_Alaska",out_prefix,".png",sep=""),
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)
par(mfrow=c(row_mfrow,col_mfrow))

##Figure 8a
NDVIA0_c_pix<-raster("A_change_pixels_Alaska_NDVI_A0_c_500_01222013_multicomp_1.rst")
plot(NDVIA0_c_pix,legend=FALSE,col=c("black","red"),axes="FALSE")
legend("topright",legend=c("no change","change"),title="Change category",
       pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=1.8)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)

##Figure 8b
NDVIA0_c_seg<-raster("A_avg_seg_change_Alaska__NDVI_A0_c_500_01222013_multicomp_1.rst")
plot(NDVIA0_c_seg,legend=FALSE,col=c("black","red"),axes="FALSE")
legend("topright",legend=c("no change","change"),title="Change category",
       pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=1.8)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)
dev.off()

#### End of script #####
