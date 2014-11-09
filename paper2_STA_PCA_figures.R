#This script reproduces and extend the analysis for paper 2 from the dissertation.
#Script created by Benoit Parmentier on May 5, 2012

###Loading r library and packages
library(raster)                                                                        # loading the raster package
library(gtools)                                                                        # loading ...
library(sp)
library(gplots)
library(rgdal)
library(RColorBrewer)
library(gdata)
library(plotrix)

###Parameters and arguments

infile1<-'ID_all_polygons_12232011_PCA_03182012c.csv'
#infile1<-'ID_all_polygons_12232011_PCA_03182012c.xlsx'
path<-'C:/Users/parmentier/Dropbox/Data/Dissertation_paper2_04142012'
path<-'//Users/benoitparmentier/Dropbox/Data/Dissertation_paper2_04142012'
infile2<-'ID_all_polygons_12232011_PCA_04082012c.csv'
out_prefix <-"_paper2_sta_11082014_"
setwd(path)

####Start of th script##

temp<-read.csv(paste(path,"/",infile1, sep=""), header=TRUE, na.strings="#NULL!")
data_extract<-read.csv(paste(path,"/",infile2,sep=""))
data2<-cbind(temp,data_extract) #combining both datasets...
rm(temp)
data<-data2
data<-subset(data2,unfilled==0)

#date<-paste(data2$year,data2$Month,data2$Day, sep="")
date<-ISOdate(data$year,data$Month,data$Day)
date2<-as.POSIXlt(as.Date(date))  #values of day=0 and month=1 are NA 
data$DOY<-date2$yday #This gives the DOY (day of year) any day of year above 160 is potentially high severity

data$dNBR_mean_NA<-data$dNBR_mean   #Creating a new variable dNBR with NA set for all values below -600
data$dNBR_mean_NA[data$dNBR_mean_NA< -600]<-NA   #This assign NA to all values below -600

data$dNBR_mean_NA<-data$dNBR_mean   #Creating a new variable dNBR with NA set for all values below -600
data$dNBR_mean_NA[data$dNBR_mean_NA< -600]<-NA   #This assign NA to all values below -600

### Adding variables for ANALYSIS OF SEVERITY

# another example: create 5 categories for 
attach(data)
data$Severity[DOY>=160 & area_ha>=60000] <- 1
data$Severity[DOY<160 & area_ha<60000] <- 0
detach(data)

attach(data)
data$Severity2[DOY>=160 & area_ha>=100000] <- 2
data$Severity2[DOY>=160 & area_ha>=60000 & area_ha<100000] <- 1
data$Severity2[DOY<160 | area_ha<60000] <- 0
detach(data)
#ghcn$Severity[is.na(ghcn$Severity)]<-0
unique(data$Severity) #This will show the unique values as well as if there are Na in the col.
x<-data$Severity
sum(is.na(x)) # Find out the number of na in a dataset...

data_B<-subset(data, BURNT==1)
mean_cat_PC1<-tapply(data_B$FAC1_1,data_B$PC1_catPC1_cat, mean, na.rm=TRUE)
mean_area_PC1<-tapply(data_B$area_ha,data_B$PC1_catPC1_cat, mean, na.rm=TRUE)

plot(mean_area_PC1)

#mean_area_PC1<-tapply(data_BURNT$area_ha,data_BURNT$PC1_catPC1_cat, mean, na.rm=TRUE)

lm5<-lm(FAC1_1~Severity, data=subset(data, BURNT==1))

mean_Severity_PC1<-tapply(data_B$FAC1_1, data_B$Severity, mean, na.rm=TRUE)
mean_Severity2_PC1<-tapply(data_B$FAC1_1, data_B$Severity2, mean, na.rm=TRUE)
range(data2$Severity, na.rm=TRUE)
lm5<-lm(FAC1_1~area_ha, data_BURNT)
lm5<-lm(FAC1_1~S, data_BURNT)
lm5<-lm(FAC1_1~Severity, data=subset)
index<-1:8
lm6<-lm(mean_area_PC1~index, data=data)

mean_Severity_PC1<-tapply(data$FAC1_1, data$Severity, mean, na.rm=TRUE)
range(data$Severity, na.rm=TRUE)
data_BURNT<-subset(data, BURNT==1)
hist(data_BURNT$area_ha)

# CREATING AVERAGEA PER POLYGONS !

Pol_ID<-tapply(data_BURNT$ID_POL,data_BURNT$ID_POL, max, na.rm=TRUE)
Pol_area<-tapply(data_BURNT$area_ha,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_logarea<-log(Pol_area)
Pol_PC1<-tapply(data_BURNT$FAC1_1,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_PC2<-tapply(data_BURNT$FAC2_1,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_dNBR<-tapply(data_BURNT$dNBR_mean,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_DOY<-tapply(data_BURNT$DOY,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_Severity<-tapply(data_BURNT$Severity,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_Severity<-tapply(data_BURNT$Severity,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_Ft7_prop<-tapply(data_BURNT$Ft7_prop,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_Ft6_prop<-tapply(data_BURNT$Ft6_prop,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_Ft5_prop<-tapply(data_BURNT$Ft6_prop,data_BURNT$ID_POL, mean, na.rm=TRUE)

#binding everything in a dataframe
data_pol<-as.data.frame(cbind(Pol_ID,Pol_PC1,Pol_PC2,Pol_dNBR,Pol_DOY,Pol_area,Pol_logarea,Pol_Severity,Pol_Severity,Pol_Ft7_prop,Pol_Ft6_prop,Pol_Ft5_prop))
#Save in a textfile
write.table(data_pol, file= paste(path,"/","data_pol_",out_prefix,".txt",sep=""), sep=",")

#To summarize data by polygon, one can also use "aggregate for all or the selected variables!!
#test2<-aggregate(cbind(dNBR_mean_NA, FAC1_1)~F_type, data=subset(data,BURNT==1), mean)
var_BURNT<-names(data_BURNT)

plot(Pol_Ft7_prop, Pol_PC1)

data_pol2<-aggregate(data_BURNT[,3]~ID_POL, data=data_BURNT, mean, na.rm=TRUE) #FOR dNBR you can have it in a loop.

attach(data_pol)
data_pol$Severity3[Pol_DOY>=160 & Pol_area>=60000] <- 1
data_pol$Severity3[Pol_DOY<160 | Pol_area<60000] <- 0
detach(data_pol)

table(data_pol$Severity3)
table(data_pol$Severity)
lm17<-lm(Pol_PC1~Severity3,data=data_pol)
plot(Pol_PC1~Pol_dNBR, subset(data_pol, Pol_dNBR>-600)) #Showing the relationship
lm13<-lm(Pol_PC1~Pol_dNBR, subset(data_pol, Pol_dNBR>-600)) #Showing the relationship

lm14<-lm(Pol_PC1~Pol_Ft7_prop)
lm7<-lm(Pol_area~Pol_PC1)
lm8<-lm(Pol_PC1~log(Pol_area))
lm8<-lm(Pol_PC1~Pol_dNBR, subset(data_pol, Pol_dNBR>-600))


###########  PLOTTING FIGURES FOR THE PAPER ##############
### FIRST READ IN ECOREGION INFORMATION
#out_prefix <- "_05132013_multicomp_1"
out_prefix <-"_paper2_sta_11082014_"

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
##Figure 1: wwf ecoregion and burned areas
#res_pix<-960


#infile2<-"wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83.rst"
#ecoreg<-raster(infile2)


res_pix<-960*2
col_mfrow<-1
row_mfrow<-1
png(filename=paste("Figure1_paper2_wwf_ecoreg_Alaska",out_prefix,".png",sep=""),
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)
#par(mfrow=c(1,2))
col_eco<-rainbow(nb_col)
col_eco[1:12] <- brewer.pal(12, "Set3")

col_eco[16]<-"brown"
#col_eco[11]<-
#col_eco[7]<-"lightblue"
col_eco[14]<-"darkgreen"
col_eco[15]<-"yellowgreen"

plot(ecoreg_rast,col=col_eco,legend=FALSE,axes="FALSE")


infile_fire<-sub(".shp","","MTBS_AK_2001_2009_IDR_ID.shp")             #Removing the extension from file.
mtbs_pol <- readOGR(".",infile_fire)
#nb_c_500_seg_OR
#plot(nb_c_500_seg_OR,legend=FALSE,col=c("black","red"),axes="FALSE")
#w1_rast<-crop(nb_c_500_seg_OR,w_Anaktuvut)
#legend("topright",legend=c("no change","change"),title="Change category",
#       pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n")
#lty=c( 1,-1,-1), pch=c(-1,15, 1)

plot(mtbs_pol, add=TRUE,border="black")

legend("topright",legend=cat_names,title="Ecoregions",
       pt.cex=3,cex=2.4,fill=col_eco,bty="n")
legend("topleft",legend=c("MTBS fire"),
       #pt.cex=1.4,cex=2.1,fill=c("black","red"),bty="n") 
       cex=2.1, lwd(2.5),
       lty=c(1), pch=c(-1),col=c("black"),bty="n")

scale_position<-c(450000, 600000)
arrow_position<-c(900000, 600000)

label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=3)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)
dev.off()

#############################
#### Figure 2

#Analysis workflow: in powerpoint

#############################
##### Figure 3: PCA loadings, scree plots, Mask

in_file_xlsx <- "PCA_analysis_rd_NBR_range_11092014.xlsx"
pc_dat_loadings <- read.xls(in_file_xlsx, 1)  
pc_dat_eigen <- read.xls(in_file_xlsx, 2)  

### Figure 3a
### PLOT AVERAGE MEAN PER CHANGE NO  CHANGE

plot(pc_dat_eigen$PC,pc_dat_eigen$pct_variance,
     type="b",
     col="blue",xlab="PRINCIPAL COMPONENTA",ylab="% VARIANCE")
legend("bottomleft",legend=c("a."),
       cex=1.2,bty="n")

#plot.new()
#par(mar=c(2,2,1,1))
#k <- 4
#f <- function() 
#  plot(rnorm(20),rnorm(20), xlab="", ylab="", main="", las=1)
#for(i in 1:k) {
#  par(fig=c(0,i/(k+1), (i-1)/k, i/k), new=TRUE)
#  f()
#  par(fig=c(i/(k+1),1, (i-1)/k, i/k), new=TRUE)
#  f()
#}

### Figure 3b
### PLOT AVERAGE MEAN PER CHANGE NO  CHANGE

mean_PC1 <- tapply(data$FAC1_1,data$BURNT, mean, na.rm=TRUE)
table(data$BURNT)
sd_PC1 <- tapply(data$FAC1_1,data$BURNT, sd, na.rm=TRUE)  

means <- mean_PC1
stdev <- sd_PC1
n     <- table(data$BURNT)[1]
ciw   <- qt(0.975, n) * stdev / sqrt(n)
ciw<-stdev    
plotCI(x=c(0,1),y=means, uiw=ciw, col="black", 
       scol="red",
       #labels=round(means,-3), 
       xaxt="n", 
       xlim=c(-0.2,1.2),ylim=c(-1.8,1.8), 
       xlab="BURNED",ylab="PC1 SCORES",font=2)
axis(side=1, at=c(0,1), labels=c(0,1), cex=0.7)
lines(c(0,1),means)
legend("bottomleft",legend=c("b."),
       cex=1.2,bty="n")
legend("topleft",legend=c("y = -0.589 x + 1.171","p<0.001"),
       cex=1.2,bty="n")

#boxplot(data$FAC1_1~data$BURNT)

### Figure 3c
### PLOT PCA LOADINGS

plot(pc_dat_loadings$PC1,pc_dat_loadings$PC2,xlim=c(-1,1),ylim=c(-1,1),asp=1,
     pch=20,col="blue",xlab="PC1",ylab="PC2",axes=FALSE)
axis(1, at=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) # "1' for side=below, the axis is drawned  on the right at location 0 and 1
axis(2, las=1,at=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) # "1' for side=below, the axis is drawned  on the right at location 0 and 1
#axis(2,las=1 ) # Draw axis on the left, with labels oriented perdendicular to axis.
box()    #This draws a box...
legend("bottomleft",legend=c("c."),
      cex=1.2,bty="n")

draw.circle(0,0,c(1.0,1),nv=200)#,border="purple",
text(pc_dat_loadings$PC1,pc_dat_loadings$PC2,pc_dat_loadings$VAR,pos=1,cex=0.8)            
grid(2,2)

#data
#plot(c(0,1), x, xlim=c(-.2, 1.2), ylim=c(-.4,1), type="l", axes=FALSE,
#     col="red", xlab="BURNED", ylab="MEAN PC1 SCORES")
#points(c(0,1), x, pch=1)
#axis(1, at=c(0,1)) # "1' for side=below, the axis is drawned  on the right at location 0 and 1
#axis(2,las=1 ) # Draw axis on the left, with labels oriented perdendicular to axis.
#box()    #This draws a box...

######################################
##### Figure 4: dNBR and PC1

#Done outside R


###################################
##### Figure 5: Severity and PC1

#Figure 5. Relationship between PC1 scores and severity: 
#(a)  High severity burn areas (category 1) has a high average PC1 score of about 1 
#(b) the average score (Pol_PC1) increases when the log of fire size (log(Pol_area)).

# SEVERITY AT THE POLYGON LEVEL: ANALYSIS FOR PAPER

mean_Pol_Severity_PC1<-tapply(data_pol$Pol_PC1, data_pol$Pol_Severity, mean, na.rm=TRUE)
x<- mean_Pol_Severity_PC1[1:2]
#X11(width=55,height=45)

##############
### Figure 5a

plot(c(0,1), x, xlim=c(-.2, 1.2), ylim=c(-.4,1), type="l", axes=FALSE,
     col="red", xlab="BOOLEAN SEVERITY", ylab="MEAN PC1 SCORES")
points(c(0,1), x, pch=1)
axis(1, at=c(0,1)) # "1' for side=below, the axis is drawned  on the right at location 0 and 1
axis(2,las=1 ) # Draw axis on the left, with labels oriented perdendicular to axis.
box()    #This draws a box...

savePlot(paste("Boolean_burnt_severity_",out_prefix,".tiff", sep=""), type="tiff")
tiff(filename=paste(path,"Boolean_burnt_severity_t_",out_prefix,".tiff", sep=""))

##############
### Figure 5b

## Plot ofr log of area...
plot(Pol_logarea, Pol_PC1)
plot(Pol_logarea, Pol_PC1, xlim=c(4, 13), ylim=c(-2,2), type="p", axes=FALSE,
     col="black", cex=0.7,xlab="FIRE SIZE (LOG OF AREA IN HA)", ylab="MEAN PC1 SCORES")
#points(c(0,1), x, pch=1) #Note that cex is used for hte size of the symbol to be used.
axis(1) # "1' for side=below, the axis is drawned  on the right at location 0 and 1
axis(2,las=1 ) # Draw axis on the left, with labels oriented perdendicular to axis.
box()    #This draws a box...
lm9<-lm(Pol_PC1~Pol_Severity)

##########################################
#### Figure 6: LAND COVER AND PC1

#LAND COVER           
# 7: B-W spruce (evergreen forest)
# 6: Deciduous forest
# 5: mixed forest
# 4: high shrub (shrub)
# 3: low shrub (dwarf shrub)
# 2: grassland and Non Woody vegetation
# 1: other
# 0: NA

lab<-c("NW","LSH", "HSH","MIX","DEC","EG")

###THIS PART O FHE CODE ANALYSIS THE RELATIONSHIP BETWEEN PC1 SCORES AND LAND COVER TYPE PROPORTIONS                                 
#Analysis based on the proportion of Fuel type
#lm9<-lm(FAC1_1~Ft7_prop, subset(data2, unfilled=0))

plot(data$FAC1_1,data$Ft7_prop)
mean_PC1_cat_prop7<-tapply(data2$Ft7_prop,data2$PC1_catPC1_cat, mean, na.rm=TRUE)
plot(mean_PC1_cat_prop7)

#mean_Severity_PC1<-tapply(Pol_PC1, Pol_Severity, mean, na.rm=TRUE)
#plot(Pol_PC1, Pol_Ft7_prop)    
#plot(log(Pol_Ft5_prop), Pol_PC1)
#lm9<-lm(Pol_PC1~log(Pol_Ft6_prop), data=data_pol)
#lm9<-lm(log(Pol_Ft5_prop)~Pol_PC1)
#lm10<-lm(mean_PC1_cat_prop7~index)

#Analysis based on the proportion of Fuel type
lm11<-lm(FAC1_1~Ft6_prop, subset(data2, unfilled=0))

plot(data2$FAC1_1,data2$Ft6_prop)
mean_PC1_cat_prop6<-tapply(data2$Ft6_prop,data2$PC1_catPC1_cat, mean, na.rm=TRUE)
plot(mean_PC1_cat_prop6)

#lm12<-lm(mean_PC1_cat_prop6~index)       
#Analysis based on the proportion of Fuel type
lm11<-lm(FAC1_1~Ft5_prop, subset(data2, unfilled=0))

plot(data2$FAC1_1,data2$Ft5_prop)
mean_PC1_cat_prop5<-tapply(data2$Ft5_prop,data2$PC1_catPC1_cat, mean, na.rm=TRUE)
plot(mean_PC1_cat_prop5)        
                
# another example: create 5 categories for 
attach(data)
data$F_t1[F_type.1==0] <- NA #Assigning NA to category 0
data$F_t1[F_type.1==1] <- 1
data$F_t1[F_type.1==2] <- 2
data$F_t1[F_type.1==3] <- 3
data$F_t1[F_type.1==4] <- 4
data$F_t1[F_type.1==5] <- 5
data$F_t1[F_type.1==6] <- 6
data$F_t1[F_type.1==7] <- 7      
detach(data)
        
# another example: create 5 categories for 
attach(data)
data$F_t2[F_type.1==1] <- NA #Assigning NA to category 1 (Other)
data$F_t2[F_type.1==2] <- 1 #Assigning NA to category 1 (Other)
data$F_t2[F_type.1==3] <- 2
data$F_t2[F_type.1==4] <- 3
data$F_t2[F_type.1==5] <- 4
data$F_t2[F_type.1==6] <- 5
data$F_t2[F_type.1==7] <- 6
detach(data)
        
attach(data)
data$F_t3[F_type.1<3] <- NA #Merging 2,1,0 and assigning 0
data$F_t3[F_type.1==3] <- 1 # low shrub (dwarf shrub) (LSH)
data$F_t3[F_type.1==4] <- 2 # high shrub (shrub) (HSH)
data$F_t3[F_type.1==5] <- 3 # mixed forest (MXF)
data$F_t3[F_type.1==6] <- 4 # Deciduous forest (DEC)
data$F_t3[F_type.1==7] <- 5 # B-W spruce (evergreen forest) (EGF)
detach(data)
        
data_BURNT<-subset(data, BURNT==1)
        
boxplot(data_BURNT$FAC1_1~data_BURNT$F_t1,outline=FALSE)
boxplot(data_BURNT$FAC1_1~data_BURNT$F_t3,outline=FALSE)
        
lm_F_t1<-lm(data_BURNT$FAC1_1~data_BURNT$F_t1)        
lm_F_t3<-lm(data_BURNT$FAC1_1~data_BURNT$F_t3)    
        
mean_PC1_t1<-tapply(data_BURNT$FAC1_1,data_BURNT$F_t1, mean, na.rm=TRUE)
mean_PC1_t2<-tapply(data_BURNT$FAC1_1,data_BURNT$F_t2, mean, na.rm=TRUE)
mean_dNBR_t2<-tapply(data_BURNT$dNBR_mean_NA,data_BURNT$F_t2, mean, na.rm=TRUE)
sd_dNBR_t2<-tapply(data_BURNT$dNBR_mean_NA,data_BURNT$F_t2, sd, na.rm=TRUE)
median_dNBR_t2<-tapply(data_BURNT$dNBR_mean_NA,data_BURNT$F_t2, median, na.rm=TRUE)
        
boxplot(data_BURNT$FAC1_1~data_BURNT$F_t2, outline=FALSE)
lmt<-lm(data_BURNT$FAC1_1~data_BURNT$F_t2)
        
mean_PC1_t3<-tapply(data_BURNT$FAC1_1,data_BURNT$F_t3, mean, na.rm=TRUE)
sd_PC1_t1<-tapply(data_BURNT$FAC1_1,data_BURNT$F_t1, sd, na.rm=TRUE) #This contains the standard deviation for


#### NOW PLOT FIGURE 6

#PC1 and the different land covers.
sd_PC1_t2<-tapply(data_BURNT$FAC1_1,data_BURNT$F_t2, sd, na.rm=TRUE)
sd_dNBR_t2<-tapply(data_BURNT$dNBR_mean_NA,data_BURNT$F_t2, sd, na.rm=TRUE)   
        
x_cat<-c("NWV","LSH","HSH", "MX", "DEC", "EGF")
data_BURNT$F_type_f<-factor(data_BURNT$F_t2, labels=x_cat, exclude="NULL")
means<- mean_PC1_t2
stdev<-sd_PC1_t2
        
#PLOT WITH STD_DEV AS  WIDTH FOR PC1
tmp   <- split(data_BURNT$FAC1_1, data_BURNT$F_t2) #This split the data into list for the 6 categories
means <- sapply(tmp, mean)
stdev <- sqrt(sapply(tmp, var))
n     <- sapply(tmp,length)
ciw   <- qt(0.975, n) * stdev / sqrt(n)
ciw<-stdev    
plotCI(x=means, uiw=ciw, col="black", barcol="blue",
      labels=round(means,-3), xaxt="n", xlim=c(1,6),ylim=c(-1,2), xlab="LAND COVER TYPES",ylab="PC1 SCORES")
      axis(side=1, at=1:6, labels=x_cat, cex=0.7)
        
####PLOT WITH STD_DEV AS  WIDTH FOR PC1
tmp   <- split(data_BURNT$FAC1_1, data_BURNT$F_t2) #This split the data into list for the 6 categories
means <- sapply(tmp, mean,na.rm=TRUE)
stdev <- sqrt(sapply(tmp, var,na.rm=TRUE))
n     <- sapply(tmp,length)
ciw   <- qt(0.975, n) * stdev / sqrt(n)
ciw<-stdev    
plotCI(x=means, uiw=ciw, col="black", scol="blue",
     #labels=round(means,-3), 
      xaxt="n", xlim=c(1,6),ylim=c(-1,2), xlab="LAND COVER TYPES",ylab="PC1 SCORES")
     axis(side=1, at=1:6, labels=x_cat, cex=0.7)

#To summarize data by polygon, one can also use "aggregate for all or the selected variables!!
#test2<-aggregate(cbind(dNBR_mean_NA, FAC1_1)~F_type, data=subset(data,BURNT==1), mean)
var_BURNT<-names(data_BURNT)

plot(Pol_Ft7_prop, Pol_PC1)

data_pol2<-aggregate(data_BURNT[,3]~ID_POL, data=data_BURNT, mean, na.rm=TRUE) #FOR dNBR you can have it in a loop.

###Figure 6 b!
## Land cover 7: proportion of evergreen forest
plot(Pol_Ft7_prop, Pol_PC1)
plot(Pol_Ft7_prop, Pol_PC1, xlim=c(0, 1), ylim=c(-2,2), type="p", axes=FALSE,
     col="black", cex=0.7,xlab="PROPORTION OF EVERGREEN FOREST", ylab="MEAN PC1 SCORES")
      #points(c(0,1), x, pch=1)
axis(1) # "1' for side=below, the axis is drawned  on the right at location 0 and 1
axis(2,las=1 ) # Draw axis on the left, with labels oriented perdendicular to axis.
box()    #This draws a box...
abline(Pol_PC1,Pol_Ft7_prop)  

#######################################
#### Now Figure 7
#Figure 7. The average PC1 score for the CHANGE variable indicates that 
#the mean scores for PC1 increase when the number of significant changes increases. 

######################################
#### Now Figure 8
#Figure 8. The average PC2 score increases when the age of the burned areas increases.

######################################
#### Now Figure 9
#Figure 9. Average trends for all change and no-change areas (burned and unburned pixels) 
#for the four variables that contribute the most to PC1: Note that with the exception of NDVI_A0, all Theil Sen slope increase in values in burned areas compared to unburned areas.

####################################
#### Add new figure: scores for fire and no fire areas...


####### End of script ########
