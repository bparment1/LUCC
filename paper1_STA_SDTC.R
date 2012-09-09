##################    USING RASTER PACKAGE FOR ANALYSIS    #######################################
###########################  multi comparison tests  #############################################
#This script is a first attempt at using R for multi-comparison tests...                       #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 07/13/2012                                                                                 #
#PROJECT: NCEAS INPLAN: Environment and Organisms --TASK#364--                                   #
##################################################################################################

###Loading r library and packages                                                      # loading the raster package
library(gtools)                                                                        # loading ...
library(mgcv)
library(sp)
library(spdep)
library(rgdal)
library(gstat)
library(raster)
library(multcomp)
library(rasterVis)
###Parameters and arguments

infile1<-"seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0.shp"
#infile2<-"dates_interpolation_03052012_2dates_test.txt"
infile2<-"wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83.shp"                                          #Ecoregions...
infile3<-"Change_image9_parameters_CMK.rst"
infile4<-"seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0.rst" #Seg ID
#outfile
path<-"/Users/benoitparmentier/Dropbox/Data/Dissertation_paper1_07142012/Paper1_sta_revision_12202011"
#path<-"C:/Users/parmentier/Dropbox/Data/Dissertation_paper1_07142012/Paper1_sta_revision_12202011"
setwd(path)

thresholds<- c(0.05,0.025)                                                                           #Proportion of testing retained for validation   
seed_number<-100
out_prefix<-"_07132012_multicomp_1"

#######START OF THE SCRIPT #############
ref<-readGDAL(infile3)
l1<-list.files(pattern="_MK_P.rst") #RENAME ???
l3<-list.files(pattern=".rst") # all rst files

l2<-list.files(pattern="_CMK_P_reclassed.rst")
s1<-stack(l1) #Create a stack from the list of files/layers
s2<-stack(l2)

seg_id<-raster(infile4)
seg_id[seg_id==0] <- NA   #Reclassifying category 0 in
mask_alaska<-seg_id
mask_alaska[mask_alaska>0]<-1      #There are 3,286,057 NA pixels
s1<-mask(s1,mask_alaska)   #This assigns NA to all values from s1 stack that are NA in the mask_land_NA

# CREATE A MASK FROM NA??


fsgid<-as.data.frame(freq(seg_id)) #Note that fsgid already contains the count of number of pixels per seg
s1_1<-raster(s1, layer=1)  #Select layer 1 from the stack
s1_1b<-subset(s1,1)        #subset layer 1 from stack using subset command
s2_1<-raster(s2, layer=1)  #This is a boolean layer!!
dataType(s2_1)
dataType(s1_1)             #Should be FLT4S because it is an MK proportion...
id<-s2_1
values(id)<-1:ncell(s2_1)

s2_sgdf<-as(s2,"SpatialGridDataFrame") #Conversion to spatial grid data frame

#projection(silverdollar) <- "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#START CODE HERE

#Loop through the number of threshold desired

# PART 1 use 5% and 2.5% threshold for p
alpha_adj1<-0.05/nlayers(s1) #Bonferroni adjustment
alpha_adj2<-0.01
thresholds<-(c(thresholds,alpha_adj1,alpha_adj2))

#s1_t1<-s1<0.025
s1_t1<-s1<thresholds[1] #0.05

s1_t2<-s1<0.05
s1_t3<-s1<alpha_adj1

# PART 2 use Bonferroni ajdustement?
#sum everthing first??

test<-zonal(s1_t1,seg_id, stat=sum) #output is a matrix
test2<-zonal(s1_t1,seg_id,stat=mean)
colnames(test)<-c("SEGID","ALB_A0","ALB_A1","ALB_A2","LST_A0","LST_A1","LST_A2","NDVI_A0","NDVI_A1","NDVI_A2")
colnames(test2)<-c("SEGID","ALB_A0","ALB_A1","ALB_A2","LST_A0","LST_A1","LST_A2","NDVI_A0","NDVI_A1","NDVI_A2")

# PART 3 use multicomp
nb_change<-rowSums(test[,2:10],na.rm=FALSE)
#rownames(nb_change)<-NULL
nb_change<-t(as.matrix(nb_change))
colnames(nb_change)<-c("nb_c")
test<-cbind(test,nb_change)
data_change<-as.data.frame(test)
drow<-as.data.frame(t(c(NA, 0, 0,0,0,0,0,0,0,0,0))) #Not necessary?
names(drow)<-c("SEGID","ALB_A0","ALB_A1","ALB_A2","LST_A0","LST_A1","LST_A2","NDVI_A0","NDVI_A1","NDVI_A2","nb_c")

data_change<-rbind(data_change,drow)
data_change<-merge(data_change,fsgid,by.x="SEGID",by.y="value")
data_change$prop_c<-(data_change$nb_c/data_change$count)*100 # DO THIS FOR EVERY STA PARAM

tmp4<-as.numeric(data_change$nb_c>1)
data_change$c_OR<-tmp4  #Seg is considered as change if it contians at least one change parmater

change<-subs(seg_id,data_change,by=1, which=ncol(data_change), subsWithNA=FALSE) # Asssign values from column 1 based on seg ID 

## WRITING OUT RESULTS IN FILES
#CANHEIGHT[is.na(CANHEIGHT)]<-0
s1_t1_1<-subset(s1_t1,1)
rst_tmp<-s1_t1_1
rst_tmp[is.na(rst_tmp)]<--999 #Assigning -999 to NA values
data_name<-paste("change_","050","_",sep="")
raster_name<-paste("Alaska_",data_name,out_prefix,".rst", sep="")
writeRaster(rst_tmp, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)

mask_tmp<-mask_alaska
mask_tmp[is.na(mask_tmp)]<--999 #Assigning -999 to NA values

data_name<-paste("mask","Alaska_","_",sep="")
raster_name<-paste("A_",data_name,out_prefix,".rst", sep="")
writeRaster(mask_tmp, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)

# PART 5 use segid and count...threshold at 50%...

#### End of script #####
