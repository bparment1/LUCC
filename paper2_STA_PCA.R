
###Parameters and arguments

infile1<-'ID_all_polygons_12232011_PCA_03182012.csv'
path<-'C:/Users/parmentier/Dropbox/Data'
#path<-'/Users/benoitparmentier/Dropbox/Data/'
setwd(path)

ghcn<-read.csv(paste(path,"/",infile1, sep=""), header=TRUE)


plot(ghcn$Elev,ghcn$FAC1_1)

lm1<-lm(FAC1_1~Elev,data=ghcn)


ghcn2<-subset(ghcn, dNBR_mean>-600)
lm1<-lm(FAC1_1~dNBR_mean,data=ghcn2)
plot(ghcn2$dNBR_mean,ghcn2$FAC1_1)
plot(ghcn2$dNBR_mean,ghcn2$FAC1_1, pch=3,cex=0.14)

plot(ghcn2$F_type,ghcn2$FAC1_1, pch=3,cex=0.14)

mean_F_type_PC1<-tapply(ghcn2$FAC1_1, ghcn2$F_type, mean, na.rm=TRUE)
mean_BURNT_PC1<-tapply(ghcn2$FAC1_1, ghcn2$BURNT, mean, na.rm=TRUE)

plot( subset( ghcn2, BURNT = 1, select = c( dNBR_mean, FAC1_1 ) ) )
#plot( subset( ghcn2, BURNT = 0, select = c( dNBR_mean, FAC1_1 ) ) , color="red",add= TRUE)

# another example: create 3 age categories
attach(ghcn2)
ghcn2$Elev_cat[Elev > 750] <- "high"
ghcn2$Elev_cat[ 750 >Elev & Elev > 250] <- "mid"
ghcn2$Elev_cat[Elev <250] <- "low"
detach(ghcn2)
mean_Elev_cat_PC1<-tapply(ghcn2$FAC1_1, ghcn2$Elev_cat, mean, na.rm=TRUE)
ghcn2$Elev_cat<-as.factor(ghcn2$Elev_cat)
plot(table(ghcn2$Elev_cat), type="h")
plot(table(ghcn2$F_type), type="h")

#Comparing SouthEast and 
attach(ghcn2)
ghcn2$South_East[A_North < -0.5 & A_East>0.5 ] <- "SE"
ghcn2$South_East[A_North > 0.5 & A_East < -0.5 ] <- "NW"
detach(ghcn2)
mean_SE_cat<-tapply(ghcn2$FAC1_1, ghcn2$South_East, mean, na.rm=TRUE)
plot(mean_SE_cat)

mean_year_cat<-tapply(ghcn2$FAC1_1, ghcn2$year, mean, na.rm=TRUE)
mean_year_cat<-tapply(ghcn2$FAC2_1, ghcn2$year, mean, na.rm=TRUE)

X11()
plot( subset( ghcn2, BURNT == 1 & F_type==7, select = c( dNBR_mean, FAC1_1 ) ) )
abline()
X11()
plot( subset( ghcn2, BURNT == 0 & F_type==7, select = c( dNBR_mean, FAC1_1 ) ) )
#REGRESSION
lm2<-lm(FAC1_1~F_type, data=subset(ghcn2,F_type>2))
lm3<-lm(FAC1_1~F_type, data=subset(ghcn2,F_type>3))
lm4<-lm(FAC1_1~F_type, data=subset(ghcn2,F_type>3& BURNT==1))
lm5<-lm(FAC1_1~F_type, data=subset(ghcn2,F_type>3& BURNT==0))

plot( subset( ghcn2, BURNT ==1 & F_type>3, select = c( dNBR_mean, FAC1_1 ) ) )
plot(FAC1_1~F_type)


# another example: create 5 categories for 
attach(ghcn2)
ghcn2$F_type2[F_type<3] <- 1
ghcn2$F_type2[F_type==3] <- 2
ghcn2$F_type2[F_type==4] <- 3
ghcn2$F_type2[F_type==5] <- 4
ghcn2$F_type2[F_type==6] <- 5
ghcn2$F_type2[F_type==7] <- 6
detach(ghcn2)

table(ghcn2$F_type2) # This gives the frequency per category
plot(table(ghcn2$F_type)/length(ghcn2$F_type), type="h", lwd=10) #relative frequency
mean_F_type2<-tapply(ghcn2$FAC1_1, ghcn2$F_type2, mean, na.rm=TRUE)
F<-sort(unique(ghcn2$F_type2))
plot(mean_F_type2)

lm6<-lm(FAC1_1~F_type2, data=ghcn2)

lm7<-lm(FAC1_1~dNBR_mean, data=subset(ghcn2,year==2001))