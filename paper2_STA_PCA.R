#This script reproduces and extend the analysis for paper 2 from the dissertation.
#Script created by Benoit Parmentier on May 5, 2012

###Loading r library and packages
library(raster)                                                                        # loading the raster package
library(gtools)                                                                        # loading ...
library(sp)
library(gplots)
library(rgdal)
library(RColorBrewer)
library(gdata) #contains read.xls function

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

#

# SEVERITY AT THE POLYGON LEVEL: ANALYSIS FOR PAPER

mean_Pol_Severity_PC1<-tapply(data_pol$Pol_PC1, data_pol$Pol_Severity, mean, na.rm=TRUE)
x<- mean_Pol_Severity_PC1[1:2]
#X11(width=55,height=45)

plot(c(0,1), x, xlim=c(-.2, 1.2), ylim=c(-.4,1), type="l", axes=FALSE,
     col="red", xlab="BOOLEAN SEVERITY", ylab="MEAN PC1 SCORES")
points(c(0,1), x, pch=1)
axis(1, at=c(0,1)) # "1' for side=below, the axis is drawned  on the right at location 0 and 1
axis(2,las=1 ) # Draw axis on the left, with labels oriented perdendicular to axis.
box()    #This draws a box...

savePlot(paste("Boolean_burnt_severity_",out_prefix,".tiff", sep=""), type="tiff")
tiff(filename=paste(path,"Boolean_burnt_severity_t_",out_prefix,".tiff", sep=""))

## Plot ofr log of area...
plot(Pol_logarea, Pol_PC1)
plot(Pol_logarea, Pol_PC1, xlim=c(4, 13), ylim=c(-2,2), type="p", axes=FALSE,
     col="black", cex=0.7,xlab="FIRE SIZE (LOG OF AREA IN HA)", ylab="MEAN PC1 SCORES")
#points(c(0,1), x, pch=1) #Note that cex is used for hte size of the symbol to be used.
axis(1) # "1' for side=below, the axis is drawned  on the right at location 0 and 1
axis(2,las=1 ) # Draw axis on the left, with labels oriented perdendicular to axis.
box()    #This draws a box...
lm9<-lm(Pol_PC1~Pol_Severity)

lab<-c("NW","LSH", "HSH","MIX","DEC","EG")

###THIS PART O FHE CODE ANALYSIS THE RELATIONSHIP BETWEEN PC1 SCORES AND LAND COVER TYPE PROPORTIONS                                 
#Analysis based on the proportion of Fuel type
#lm9<-lm(FAC1_1~Ft7_prop, subset(data2, unfilled=0))
                                 
plot(data$FAC1_1,data$Ft7_prop)
mean_PC1_cat_prop7<-tapply(data2$Ft7_prop,data2$PC1_catPC1_cat, mean, na.rm=TRUE)
plot(mean_PC1_cat_prop7)

mean_Severity_PC1<-tapply(Pol_PC1, Pol_Severity, mean, na.rm=TRUE)


plot(Pol_PC1, Pol_Ft7_prop)    
plot(log(Pol_Ft5_prop), Pol_PC1)
lm9<-lm(Pol_PC1~log(Pol_Ft6_prop), data=data_pol)
lm9<-lm(log(Pol_Ft5_prop)~Pol_PC1)
lm10<-lm(mean_PC1_cat_prop7~index)

#Analysis based on the proportion of Fuel type
lm11<-lm(FAC1_1~Ft6_prop, subset(data2, unfilled=0))
                                 
plot(data2$FAC1_1,data2$Ft6_prop)
mean_PC1_cat_prop6<-tapply(data2$Ft6_prop,data2$PC1_catPC1_cat, mean, na.rm=TRUE)
plot(mean_PC1_cat_prop6)
                                 
lm12<-lm(mean_PC1_cat_prop6~index)       
                                 
#Analysis based on the proportion of Fuel type
lm11<-lm(FAC1_1~Ft5_prop, subset(data2, unfilled=0))
                                 
plot(data2$FAC1_1,data2$Ft5_prop)
mean_PC1_cat_prop5<-tapply(data2$Ft5_prop,data2$PC1_catPC1_cat, mean, na.rm=TRUE)
plot(mean_PC1_cat_prop5)
                                 
lm12<-lm(mean_PC1_cat_prop5~index)
boxplot(data$FAC1_1~data$Severity
        
  
#LAND COVER           
# 7: B-W spruce (evergreen forest)
# 6: Deciduous forest
# 5: mixed forest
# 4: high shrub (shrub)
# 3: low shrub (dwarf shrub)
# 2: grassland and Non Woody vegetation
# 1: other
# 0: NA

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
tmp   <- split(data_BURNT$dNBR_mean_NA, data_BURNT$F_t2) #This split the data into list for the 6 categories
means <- sapply(tmp, mean,na.rm=TRUE)
stdev <- sqrt(sapply(tmp, var,na.rm=TRUE))
n     <- sapply(tmp,length)
ciw   <- qt(0.975, n) * stdev / sqrt(n)
ciw<-stdev    
plotCI(x=means, uiw=ciw, col="black", barcol="blue",
        labels=round(means,-3), xaxt="n", xlim=c(1,6),ylim=c(-100,500), xlab="LAND COVER TYPES",ylab="dNBR")
        axis(side=1, at=1:6, labels=x_cat, cex=0.7)
###
## Land cover 7: proportion of evergreen forest
plot(Pol_Ft7_prop, Pol_PC1)
plot(Pol_Ft7_prop, Pol_PC1, xlim=c(0, 1), ylim=c(-2,2), type="p", axes=FALSE,
col="black", cex=0.7,xlab="PROPORTION OF EVERGREEN FOREST", ylab="MEAN PC1 SCORES")
      #points(c(0,1), x, pch=1)
      axis(1) # "1' for side=below, the axis is drawned  on the right at location 0 and 1
      axis(2,las=1 ) # Draw axis on the left, with labels oriented perdendicular to axis.
box()    #This draws a box...
abline(Pol_PC1,Pol_Ft7_prop)  
          
mean_PC1_type3<-tapply(data$FAC1_1,data_BURNT$F_type3, mean, na.rm=TRUE)     
        
data$F_type_f<-as.factor(data$F_type, labels=c("NA","OTH","GRA","LSH","HSH","MXF","DEC","EGF"), exclude="NULL")
data$F_type_f<-factor(data$F_type, labels=c("NA","OTH","GRA","LSH","HSH","MXF","DEC","EGF"), exclude="NULL")

lm13<-lm(data$FAC1_1~data$F_type3, subset(data, BURNT==1))
lm13<-lm(data$FAC1_1~data$F_type_f, subset(data, BURNT==1))
lm13<-lm(Pol_PC1~data$F_type)
lm14<-lm(Pol_PC1~Pol_Ft7_prop)
mean_PC1_type2<-tapply(data$FAC1_1,data$F_type2, mean, na.rm=TRUE)

mean_PC1_type<-tapply(data$FAC1_1,data$F_type, mean, na.rm=TRUE)
mean_dNBRN_type<-tapply(data$dNBR_mean_NA,data$F_type, mean, na.rm=TRUE)

test<-aggregate(dNBR_mean_NA~F_type, data=data, mean) #This aggregates by F_type class in a data.frame 
test<-aggregate(dNBR_mean_NA~F_type, data=subset(data,BURNT==1), mean) #This aggregates by F_type class in a data.frame 
                                                                       #taking into account only the burnt pixel

test3<-aggregate(data, by=list(data$ID_POL), data=subset(data,BURNT==1), FUN=mean, na.rm=TRUE)
test2<-aggregate(cbind(dNBR_mean_NA, FAC1_1)~F_type, data=subset(data,BURNT==1), mean)

plot(dNBR_mean_NA~F_type, data=test2)
plot(FAC1_1~F_type, data=test2)
boxplot(FAC1_1~F_type, data=data)


boxplot(data_BURNT$FAC1_1~data_BURNT$F_t3)        
plotmeans(data_BURNT$FAC1_1~data_BURNT$F_t2)        

plot(mean_PC1_type2)
table(data$F_type2) # This gives the frequency per category
boxplot(data$FAC1_1~data$F_type2, subset(data, BURNT==1))     

#Checking the normality of FAC1_1
qqnorm(data2$FAC1_1)
qqline(data2$FAC1_1)
shapiro.test(data2$FAC1_1) #This test only works for sample size 3 to 5000
                                
library(nortest)
cvm.test(data$FAC1_1)
#Calculating the relative frequencies....
mean(abs(x-mean(data$FAC1_1))>2*sd(data$FAC1_1))
                     
###SELECTING LOW AND HIGH SCORES...BASED ON PC1 SCORES...
                     
mean(data_B$FAC1_1> 2) #This shows that there are about 10% of data with scores greaater than 2
mean(0.25>data_B$FAC1_1 & data_B$FAC1_1> 0)   #This shows that  there are about 10% date between 0.25 and 0              
mean(0.5>data_B$FAC1_1 & data_B$FAC1_1> 0) #This shows there are about 19.5% of data in that range
mean(data_B$FAC1_1> 1.5)  #This shows that there are about 19.2% data above 1.5  

attach(data)
data$PC1_HL1[FAC1_1>1.5] <- 2
data$PC1_HL1[FAC1_1>=0.5 & FAC1_1<=1.5] <- 0
data$PC1_HL1[FAC1_1>0 & FAC1_1<0.5] <- 1
data$PC1_HL1[FAC1_1<0] <- 0
detach(data)

#Ploting average scores for polygons...
plot(Pol_PC1,Pol_PC2,pch=4, cex=0.5)          
text(Pol_PC1,Pol_PC2,labels=Pol_ID,cex=0.7) #Adding text labels to each point
head(sort(Pol_PC1, decreasing=TRUE))
x<-(sort(Pol_PC1, decreasing=TRUE))       
                     
plot(data$FAC1_1~data$dNBR_mean, subset(data, PC1_HL1==2))
data3<-subset(data, PC1_HL1==2)  
plot(data3$FAC1_1~data3$dNBR_mean)      

lm13<-lm(Pol_PC1~Pol_Severity, data=data_pol) # problem data is deleted

####ANALYSIS WITH TEMPORAL TRENDS
d14<-read.table("STA_AMP_sloples_Intercepts_04142012.txt",header=TRUE)  #data extracted from IDRISI
d14b<-cbind(data2,d14)
attach(d14b)
d14b$PC1_HL1[FAC1_1>1.5] <- 2
d14b$PC1_HL1[FAC1_1>=0.5 & FAC1_1<=1.5] <- 0
d14b$PC1_HL1[FAC1_1>0 & FAC1_1<0.5] <- 1
d14b$PC1_HL1[FAC1_1<0] <- 0
detach(d14b)
                     
#Calculating scores for BURNT SCARS HIGH AND LOW
#First show average for burnt scars 
d14b_B<-subset(d14b, unfilled==0 & BURNT==1)
listn<-names(d14b_B)
                  
i=0
j=0  
y<-matrix(1,9,2)
for (j in 1:9) {
    temp<-d14b_B[[paste("NDVI.A0.y",j,sep="")]]
    meanHL1<-tapply(temp,d14b_B$PC1_HL1, mean, na.rm=TRUE)
    y[j,1]<-meanHL1[2]
    y[j,2]<-meanHL1[3]
}
plot(y[,2])  #This shows the
plot(y[,1])

                     
mean_PC1_HL1_NDVIA0_y1<-tapply(d14b_B$NDVI.A0.y1,d14b_B$PC1_HL1, mean, na.rm=TRUE)
mean_PC1_HL1_NDVIA0_y1<-tapply(d14b_B$NDVI.A0.y1,d14b_B$PC1_HL1, mean, na.rm=TRUE)
        
###END OF SCRIPT
        
# plot means and
#data(state)
#tmp   <- split(state.area, state.region)
#means <- sapply(tmp, mean)
#stdev <- sqrt(sapply(tmp, var))
#n     <- sapply(tmp,length)
#ciw   <- qt(0.975, n) * stdev / sqrt(n)
        
# plain
#plotCI(x=means, uiw=ciw)
        
# prettier
#plotCI(x=means, uiw=ciw, col="black", barcol="blue", lwd=1)
        
# give mean values
#plotCI(x=means, uiw=ciw, col="black", barcol="blue",
#labels=round(means,-3), xaxt="n", xlim=c(0,5) )
#axis(side=1, at=1:4, labels=names(tmp), cex=0.7)
        
# better yet, just use plotmeans ... #
#plotmeans( state.area ~ state.region )