#This script reproduces and extend the analysis for paper 2 from the dissertation.
#Script created by Benoit Parmentier on May 5, 2012

###Loading r library and packages
library(raster)                                                                        # loading the raster package
library(gtools)                                                                        # loading ...
library(sp)
library(mgcv)

###Parameters and arguments

infile1<-'ID_all_polygons_12232011_PCA_03182012c.csv'
#infile1<-'ID_all_polygons_12232011_PCA_03182012c.xlsx'
path<-'C:/Users/parmentier/Dropbox/Data/Dissertation_paper2_04142012'
path<-'//Users/benoitparmentier/Dropbox/Data/Dissertation_paper2_04142012'
infile2<-'ID_all_polygons_12232011_PCA_04082012c.csv'
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

# DO AVERAGE BY POLYGON!
Pol_ID<-tapply(data_BURNT$ID_POL,data_BURNT$ID_POL, max, na.rm=TRUE)
Pol_area<-tapply(data_BURNT$area_ha,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_PC1<-tapply(data_BURNT$FAC1_1,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_PC2<-tapply(data_BURNT$FAC2_1,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_dNBR<-tapply(data_BURNT$dNBR_mean,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_DOY<-tapply(data_BURNT$DOY,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_Severity<-tapply(data_BURNT$Severity,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_Severity<-tapply(data_BURNT$Severity,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_Ft7_prop<-tapply(data_BURNT$Ft7_prop,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_Ft6_prop<-tapply(data_BURNT$Ft6_prop,data_BURNT$ID_POL, mean, na.rm=TRUE)
Pol_Ft5_prop<-tapply(data_BURNT$Ft6_prop,data_BURNT$ID_POL, mean, na.rm=TRUE)

data_pol<-as.data.frame(cbind(Pol_ID,Pol_PC1,Pol_PC2,Pol_dNBR,Pol_DOY,Pol_area,Pol_Severity,Pol_Severity,Pol_Ft7_prop,Pol_Ft6_prop,Pol_Ft5_prop))

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
lm9<-lm(Pol_PC1~Pol_Severity)


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
lm9<-lm(Pol_PC1~Pol_Ft6_prop, data=data_pol)
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
# another example: create 5 categories for 
attach(data)
data$F_type2[F_type<3] <- 1
data$F_type2[F_type==3] <- 2
data$F_type2[F_type==4] <- 3
data$F_type2[F_type==5] <- 4
data$F_type2[F_type==6] <- 5
data$F_type2[F_type==7] <- 6
detach(data)
        
attach(data)
data$F_type3[F_type<3] <- 1
data$F_type3[F_type==3] <- 2
data$F_type3[F_type==4] <- 3
data$F_type3[F_type==5] <- 4
data$F_type3[F_type==6] <- 5
data$F_type3[F_type==7] <- 6
detach(data)

data$F_type_f<-as.factor(data$F_type, labels=c("NA","OTH","GRA","LSH","HSH","MXF","DEC","EGF"))

lm14<-lm(Pol_PC1~Pol_Ft7_prop)
mean_PC1_type2<-tapply(data$FAC1_1,data$F_type2, mean, na.rm=TRUE)
        
mean_PC1_type<-tapply(data$FAC1_1,data$F_type, mean, na.rm=TRUE)
mean_dNBR_type<-tapply(data$FAC1_1,data$dNBR_mean, mean, na.rm=TRUE)
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
  