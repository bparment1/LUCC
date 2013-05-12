##################    USING RASTER PACKAGE FOR ANALYSIS    #######################################
###########################  multi comparison tests  #############################################
#This script is a first attempt at using R for multi-comparison tests...                       #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 05/08/2013                                                                                 #
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
library(gdata)

#Functions used in the script

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

merge_multiple_df<-function(df_list,by_name){
  for (i in 1:(length(df_list)-1)){
    if (i==1){
      df1=df_list[[i]]
    }
    if (i!=1){
      df1=df_m
    }
    df2<-df_list[[i+1]]
    df_m<-merge(df1,df2,by=by_name,all=T)
  }
  return(df_m)
}

freq_r_stack<-function(r_stack){
  list_area_tab<-vector("list",nlayers(r_stack))
  for (j in 1:nlayers(r_stack)){
    tmp<-freq(subset(r_stack,j))
    tmp2<-na.omit(tmp)
    if (j!=1){
      list_area_tab[[j]]<-as.data.frame(tmp2[,2])
    }
    if (j==1){
      list_area_tab[[j]]<-as.data.frame(tmp2)
    }
  }
  table_freq<-do.call(cbindX,list_area_tab)
  return(table_freq)
}


###Parameters and arguments

infile1<-"seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0.shp"
infile2<-"wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83.rst"
infile2b<-"wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83.vct"                                          #Ecoregions...
infile3<-"Change_image9_parameters_CMK.rst"
infile4<-"seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0.rst" #Seg ID
infile5<-"mask_land2.rst"
infile6 <-"Alaska_glc2000_ALB83.rst"
#infile6b<-"Alaska_globcover2009_ALB83.rst"
f_fire<-"MTBS_AK_2001_2009_IDR_ID.rst"

#outfile
#path<-"/Users/benoitparmentier/Dropbox/Data/Dissertation_paper1_07142012/Paper1_sta_revision_12202011"
#path_in<-"/home/parmentier/Data/Paper1_sta_revision_01192013"
path_in<-"/Users/benoitparmentier/Dropbox/Data/Dissertation_paper1_07142012/Paper1_sta_revision_01192013"

setwd(path_in)

#thresholds<- c(0.05,0.025)  #which correspond to alpha 10% and 5% probability level since the test must from two side, do 1%??
t<-0.05/9
thresholds_input<- c(c(0.05,0.01),t)
#out_prefix<-"_01192013_multicomp_1"
#out_prefix<-"_01222013b_multicomp_1"
out_prefix<-"_03022013_multicomp_1"

prop<-0.5
correction=0 #if correction is 1 then carry out Bonferroni correction...

#######START OF THE SCRIPT #############
ref<-readGDAL(infile3)
l1<-list.files(pattern="_MK_P.rst") #RENAME ???
l3<-list.files(pattern=".rst") # all rst files

l2<-list.files(pattern="_CMK_P_reclassed.rst")
s1<-stack(l1) #Create a stack from the list of files/layers
#s2<-stack(l2)
ecoreg<-raster(infile2)
seg_id<-raster(infile4)  #Reading segment image in
seg_id[seg_id==0] <- NA   #Reclassifying category 0 in
mask_alaska<-seg_id       # creating a mask for the study area
mask_alaska[mask_alaska>0]<-1      #There are 3,286,057 NA pixels and 1,476,334 pixels in the study area.
s1<-mask(s1,mask_alaska)   #This assigns NA to all values from s1 stack that are NA in the mask_land_NA
ecoreg<-mask(ecoreg,mask_alaska)
#mask_alaska2<-raster(infile5)
# CREATE A MASK FROM NA??

fsgid<-as.data.frame(freq(seg_id)) #Note that fsgid already contains the count of number of pixels per seg
s1_1<-raster(s1, layer=1)  #Select layer 1 from the stack
dataType(s1_1)             #Should be FLT4S because it is an MK proportion...
id_rast<-s1_1
#values(id_rast)<-1:ncell(s1_1) #Assinging unique ID for every pixels
setValues(id_rast,1:ncell(s1_1)) #Assinging unique ID for every pixels

#projection(s1) <- "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#START CODE HERE

# PART 1 use 5% and 2.5% threshold for p
if (correction==1){
  alpha_adj<- thresholds_input
  for (i in 1:length(thresholds_input)){
    alpha_adj[i]<-thresholds_input[i]/nlayers(s1) #Bonferroni adjustment
  }
  thresholds<-(c(thresholds_input,alpha_adj))
}

if (correction==0){
  thresholds<-c(thresholds_input)
}

thresh_lab<-as.character(round(thresholds*10000))

list_table_change<-vector("list", length(thresholds)) #This will contian the summary table per seg for every threshold
list_xtab_change<-vector("list", length(thresholds)) #This will contian the summary table per seg for every threshold

for (i in 1:length(thresholds)){
#runChangeFunction <- function(i) {            # loop over dates
  #s1_t1<-s1<0.025
  s1_t1<-s1<thresholds[i] #0.05 if value less than threhsold then 1 else 0 (no change), this is at pixel value
  #names_layers<-c("ALB_A0","ALB_A1","ALB_A2","LST_A0","LST_A1","LST_A2","NDVI_A0","NDVI_A1","NDVI_A2")
  #data_name<-paste("change_pixel_","Alaska_","_",names(data_change)[k],"_",sep="")
  #raster_name<-paste("A_",data_name,(thresh_lab[i]),out_prefix,".rst", sep="")
  #writeRaster(s1_t1, filename=raster_name,NAflag=-999,overwrite=TRUE)  #Writing the data in a raster file format...
  
  # PART 2 use Bonferroni ajdustement? Next loop
  test<-zonal(s1_t1,seg_id, stat=mean) #output is a matrix containg average prop of change per segement
  colnames(test)<-c("SEGID","ALB_A0","ALB_A1","ALB_A2","LST_A0","LST_A1","LST_A2","NDVI_A0","NDVI_A1","NDVI_A2")
  
  test2<-matrix(1,nrow(test),10)
  for (k in 2:10){
    tmp<-test[,k]>=prop #if above threshold then change...this creates a logic var  
    test2[,k]<-tmp #logic var is 0-1
  }
  test2<-as.data.frame(test2[,2:10])
  names(test2)<-c("ALB_A0_c","ALB_A1_c","ALB_A2_c","LST_A0_c","LST_A1_c","LST_A2_c","NDVI_A0_c","NDVI_A1_c","NDVI_A2_c")
  data_change<-cbind(test,test2)  #This containthe average number of change pixels per segments and if a segment is considered
                                  # as change if it is has >50%pixels above teh 0.05 or other threshold
  # PART 3 use multicomp
  nb_change<-rowSums(data_change[,11:19],na.rm=FALSE) #nb of STA parameters changing
  nb_change<-as.data.frame(nb_change)
  names(nb_change)<-c("nb_c") #names
  data_change<-cbind(data_change,nb_change) # Adding the number of STA param changes per segments
  
  data_change<-merge(data_change,fsgid,by.x="SEGID",by.y="value")
  #data_change$prop_c<-(data_change$nb_c/data_change$count)*100 # DO THIS FOR EVERY STA PARAM
  tmp4<-as.numeric(data_change$nb_c>0)
  data_change$c_OR<-tmp4  #Seg is considered as change if it contians at least one change parmater
  
  list_xtab<-vector("list", nlayers(s1_t1))
  list_xtab2<-vector("list", nlayers(s1_t1))
  lf_change_list_param<-vector("list", nlayers(s1_t1))
  
  #This creates proportion images per segment. 
  j=0
  for (k in 2:10){
    j<-j+1 #counter var
    change_prop<-subs(seg_id,data_change,by=1, which=k) # Asssign values from column k based on seg ID in column 1 of data_change
    data_name<-paste("avg_seg_proportion_","Alaska_","_",names(data_change)[k],"_",sep="")
    raster_name<-paste("A_",data_name,(thresh_lab[i]),out_prefix,".rst", sep="")
    writeRaster(change_prop, filename=raster_name,NAflag=-999,overwrite=TRUE)  #Writing the data in a raster file format...
  }
  #This creates change images for each parameter from the matrix data_change...Pixels with semgent ID serve as reference.
  #Note a segment is considered as changing if it has 50% (prop var) or more pixels changing for given threshold.
  
  j=0
  for (k in 11:(ncol(data_change)-1)){
    j<-j+1 #counter var
    change<-subs(seg_id,data_change,by=1, which=k) # Asssign values from column k based on seg ID in column 1 of data_change
    data_name<-paste("avg_seg_change_","Alaska_","_",names(data_change)[k],"_",sep="")
    raster_name<-paste("A_",data_name,(thresh_lab[i]),out_prefix,".rst", sep="")
    writeRaster(change, filename=raster_name,NAflag=-999,overwrite=TRUE)  #Writing the data in a raster file format...
    lf_change_list_param[j]<-raster_name
  }  
    
  #Now crosstabe the change for each sta parameters and the number of change
  pos<-match("nb_c",names(data_change))
  nb_c_rast<-subs(seg_id,data_change,by=1, which=pos) # Asssign values from column 1 based on seg ID 
  
  data_name<-paste("Change_image9_param_avg_seg_","Alaska_","_",sep="")
  raster_name<-paste("A_",data_name,(thresh_lab[i]),out_prefix,".rst", sep="")
  writeRaster(nb_c_rast, filename=raster_name,NAflag=-999,overwrite=TRUE)  #Writing the data in a raster file format...
  
  ##Now built a table for the paper...
  
  lf_change_list_param<-lf_change_list_param[1:9]
  names(lf_change_list_param)<-c("ALB_A0_c","ALB_A1_c","ALB_A2_c","LST_A0_c","LST_A1_c","LST_A2_c","NDVI_A0_c","NDVI_A1_c","NDVI_A2_c")
  for (j in 1:length(lf_change_list_param)){
    param_rast<-raster(lf_change_list_param[[j]]) #STA param to crosstab
    xtab<-crosstab(nb_c_rast,param_rast) #This contains the crosstab table 
    list_xtab[[j]]<-xtab
    #need to change here on 11/13/2012: done on 01/19/2013
    if (ncol(xtab)==2){
      xtab2<-as.data.frame(xtab[,2])  #drop the first colum only if there are two columns!!! otherwise add a 0 column??
    }
    if (ncol(xtab)==1){
      colxtab<-rep(NA,length(lf_change_list_param)+1)
      xtab2<-as.data.frame(colxtab)  #drop the first colum only if there are two columns!!! otherwise add a 0 column??
    }
    names(xtab2)<-names(lf_change_list_param)[j]
    list_xtab2[[j]]<-xtab2   
  }
  #table_change<-do.call(cbind,list_xtab2)
  table_change<-do.call(cbindX,list_xtab2) #bind data frame wiith columns and add NA if different length
  names(table_change)<-c("ALB_A0_c","ALB_A1_c","ALB_A2_c","LST_A0_c","LST_A1_c","LST_A2_c","NDVI_A0_c","NDVI_A1_c","NDVI_A2_c")
  table_change_paper<-as.data.frame(t(as.matrix(table_change)))
  write.table(table_change_paper,file=paste("A_","table_change_paper_seg_",(thresh_lab[i]),out_prefix,".txt", sep=""),sep=",")
  #list_table_change[[i]]<-table_change_paper
  #list_xtab_change[[i]]<-list_xtab2 #?? change here
  sta_change_obj<-list(data_change,list_xtab2,table_change_paper)
  names(sta_change_obj)<-c("dat_change_seg","list_xtab_change","table_change")
  save(sta_change_obj, file=paste("paper1_sta_change_obj_",(thresh_lab[i]),out_prefix,".RData", sep=""))
  #return(sta_change_obj)
}

###### SAME ANALYSIS AT THE PIXEL LEVEL...
#Compare to threshold from pixels only...
list_table_change<-vector("list", length(thresholds)) #This will contian the summary table per seg for every threshold
list_xtab_change<-vector("list", length(thresholds)) #This will contian the summary table per seg for every threshold

i=2
for (i in 1:length(thresholds)){
  #plot(s1)
  s1_t1 <- s1 < thresholds[i]
  lf_change_list_param<-vector("list",nlayers(s1_t1))
  names_rast<-c("ALB_A0_c","ALB_A1_c","ALB_A2_c","LST_A0_c","LST_A1_c","LST_A2_c","NDVI_A0_c","NDVI_A1_c","NDVI_A2_c")
  list_xtab<-vector("list", nlayers(s1_t1))
  list_xtab2<-vector("list", nlayers(s1_t1))
  
  for (j in 1:nlayers(s1_t1)){
    data_name<-paste("change_pixels_","Alaska_",names_rast[j],"_",sep="")
    raster_name<-paste("A_",data_name,(thresh_lab[i]),out_prefix,".rst", sep="")
    writeRaster(subset(s1_t1,j), filename=raster_name,NAflag=-999,overwrite=TRUE)  #Writing the data in a raster file format...
    lf_change_list_param[[j]]<-raster_name
  }
  ##
  nb_c_rast<-stackApply(s1_t1, indices=rep(1,nlayers(s1_t1)),fun=sum,na.rm=TRUE)
  nb_c_rast<-mask(nb_c_rast,mask_alaska)
  data_name<-paste("Change_image9_param_pixels_","Alaska_","_",sep="")
  raster_name<-paste("A_",data_name,(thresh_lab[i]),out_prefix,".rst", sep="")
  writeRaster(nb_c_rast, filename=raster_name,NAflag=-999,overwrite=TRUE)  #Writing the data in a raster file format...
  
  ##Now create table...
  for (j in 1:length(lf_change_list_param)){
    param_rast<-raster(lf_change_list_param[[j]]) #STA param to crosstab
    xtab<-crosstab(nb_c_rast,param_rast) #This contains the crosstab table 
    list_xtab[[j]]<-xtab
    #need to change here on 11/13/2012: done on 01/19/2013
    if (ncol(xtab)==2){
      xtab2<-as.data.frame(xtab[,2])  #drop the first colum only if there are two columns!!! otherwise add a 0 column??
    }
    if (ncol(xtab)==1){
      colxtab<-rep(NA,length(lf_change_list_param)+1)
      xtab2<-as.data.frame(colxtab)  #drop the first colum only if there are two columns!!! otherwise add a 0 column??
    }
    names(xtab2)<-names(lf_change_list_param)[j]
    list_xtab2[[j]]<-xtab2   
  }
  #table_change<-do.call(cbind,list_xtab2)
  table_change<-do.call(cbindX,list_xtab2) #bind data frame wiith columns and add NA if different length
  names(table_change)<-c("ALB_A0_c","ALB_A1_c","ALB_A2_c","LST_A0_c","LST_A1_c","LST_A2_c","NDVI_A0_c","NDVI_A1_c","NDVI_A2_c")
  table_change_paper<-as.data.frame(t(as.matrix(table_change)))
  write.table(table_change_paper,file=paste("A_","pixels_table_change_paper_",(thresh_lab[i]),out_prefix,".txt", sep=""),sep=",")
  #list_table_change[[i]]<-table_change_paper
  #list_xtab_change[[i]]<-list_xtab2 #?? change here
  sta_change_obj<-list(list_xtab2,table_change_paper)
  names(sta_change_obj)<-c("list_xtab_change","table_change")
  save(sta_change_obj, file=paste("paper1_sta_change_pixels_obj_",(thresh_lab[i]),out_prefix,".RData", sep=""))
  
}

#mclapply(1:length(thresholds), runChangeFunction,mc.preschedule=FALSE,mc.cores = 9)

########## BEFORE EXAMINING RESULTS WRITE OUT CHANGES TO FILE
### Create table of %change for every level
#list_data_change<-vector("list", length(thresholds)) #This will contian the summary table per seg for every threshold
file_pat<-glob2rx(paste("*A_Change_image9_param_avg_seg_Alaska*",out_prefix,"*.rst",sep="")) #Search for files in relation to fusion                  
lfc<-mixedsort(list.files(pattern=file_pat))
s_change_seg<-stack(lfc)
#A_Change_image9_param_avg_seg_Alaska__500_01222013b_multicomp_1.rst

file_pat<-glob2rx(paste("*A_Change_image9_*pixels*_Alaska*",out_prefix,"*rst",sep="")) #Search for files in relation to fusion                  
lfc<-mixedsort(list.files(pattern=file_pat))
s_change_pix<-stack(lfc)
A_Change_image9_param_avg_seg_Alaska__500_01222013b_multicomp_1.rst
s_change<-stack(s_change_seg,s_change_pix)
s_change<-mask(s_change,mask_alaska)
s_change_OR<- s_change > 0

#Write out stack of number of change for each threshold level
data_name<-paste("change_nb_c_rast_all_pix_seg_","Alaska_","_",sep="")
raster_name<-paste("A_",data_name,out_prefix,".tif", sep="")
writeRaster(s_change, filename=raster_name,NAflag=-999,bylayer=False,
            bandorder ="BSQ",overwrite=TRUE)  #Writing the data in a raster file format...

####COMPARE RESULTS TO MTBS FIRE DATA SET AND ECOREGION########
## Create table 4 for paper

s_change<-brick("A_change_nb_c_rast_all_pix_seg_Alaska___01222013_multicomp_1.tif")

nb<-maxValue(ecoreg) #number of regions in the map
plot(ecoreg,col=rainbow(nb))
r_fire<-raster(f_fire)
r_fire<-mask(r_fire,mask_alaska)
r_fire<- r_fire >0
thresh_lab_tmp<-rev(thresh_lab)
labels<-c(paste(thresh_lab_tmp,"seg",sep="_"),paste(thresh_lab_tmp,"pix",sep="_"))
lc_rast <- raster(infile6) #land cover
temp_name <- sub("^([^.]*).*", "\\1", infile6) 
lc_rast_m <- mask(x=lc_rast,mask=mask_alaska,filename=paste(tmp_name,"_mask.rst",sep="")) #land cover

#Select the relevant change image?
#change_OR<-raster(s_change,layer=3)
list_xtab_eco<-vector("list",nlayers(s_change))
list_xtab_fire<-vector("list",nlayers(s_change))
list_xtab_lc<-vector("list",nlayers(s_change))

#Make this a function
for (i in 1:nlayers(s_change)){
  change_OR<-raster(s_change, layer=i)
  change_OR<- change_OR > 0
  xtab_eco<-crosstab(ecoreg,change_OR)
  #write.table(xtab_eco,file=paste("A_","xtab_tb_eco_",(labels[i]),out_prefix,".txt", sep=""),sep=",")
  xtab_fire<-crosstab(r_fire,change_OR) 
  xtab_lc <-crosstab(lc_rast_m,change_OR)
  
  #write.table(xtab_fire,file=paste("A_","xtab_tb_fire_",(labels[i]),out_prefix,".txt", sep=""),sep=",")
  list_xtab_fire[[i]]<-cbind(xtab_fire[,2])
  list_xtab_eco[[i]]<-cbind(xtab_eco[,2])  
  list_xtab_lc[[i]]<-cbind(xtab_lc[,2])  
}

tmp <- as.data.frame(do.call(cbind,list_xtab_eco))
names(tmp)<-labels
tmp2<-as.data.frame(do.call(cbind,list_xtab_fire))
names(tmp2)<-labels
tmp3<-as.data.frame(do.call(cbindX,list_xtab_lc))
#names(tmp3)<-labels

total2 <-freq(r_fire)[2,2]
total1 <-freq(ecoreg)[1:17,2]

#total<-c(total1,total2)
total<-c(total1,total2)

table_4_paper<-rbind(tmp,tmp2[2,]) #category 18 is fire!!!
table_4_paper<-rbind(tmp,tmp2[2,]) #category 18 is fire!!!

table_4_paper$total_area<-total
table_4_paper_percent<-(table_4_paper[,1:6]/total)*100
table_4_paper_percent$total_area<-total
write.table(table_4_paper,file=paste("A_","paper1_sta_table_4_paper_",out_prefix,".txt", sep=""),sep=",")
write.table(table_4_paper_percent,file=paste("A_","paper1_sta_table_4_paper_",out_prefix,".txt", sep=""),
            append=TRUE,sep=",")

#########################
## LAND COVER DATA

total_lc <-as.data.frame(freq(lc_rast))
list_lc_df <- vector("list",length(list_xtab_lc))

for (i in 1:length(list_xtab_lc)){
  tmp_df <-as.data.frame(list_xtab_lc[[i]])
  names(tmp_df)[1]<-labels[i]
  tmp_df$GLC_ID<-rownames(tmp_df)
  names(tmp_df)
  list_lc_df[[i]]<-tmp_df
}

### MERGE MULTIPLE df based on id
table_5_paper<-merge_multiple_df(list_lc_df,by_name="GLC_ID")
table_5_temp<- merge(table_5_paper,total_lc,by.x="GLC_ID",by.y="value")
table_5_paper_percent<-(table_5_temp[,2:7]/table_5_temp[,8])*100
#table_5_paper$total_area<- total_lc[c("count")]
table_5_paper_percent$GLC_ID <- table_5_temp$GLC_ID

lc_cat <- vector("list",25)
lc_cat[[1]]<- c(0, "No Data")
lc_cat[[2]]<- c(3,"Temperate or Sub-polar Broadleaved Deciduous Forest - Closed Canopy")
lc_cat[[3]]<- c(4,"Temperate or Sub-polar Needleleaved Evergreen Forest - Closed Canopy")
lc_cat[[4]]<- c(5,"Temperate or Sub-polar Needleleaved Evergreen Forest - Open Canopy")
lc_cat[[5]]<- c(6,"Temperate or Sub-polar Needleleaved Mixed Forest - Closed Canopy")
lc_cat[[6]]<- c(7,"Temperate or Sub-polar Mixed Broadleaved or Needleleaved Forest - Closed Canopy")
lc_cat[[7]]<- c(8,"Temperate or Sub-polar Mixed Broaddleleaved or Needleleaved Forest - Open Canopy")
lc_cat[[8]]<- c(10,"Temperate or Subpolar Broadleaved Deciduous Shrubland - Open Canopy")
lc_cat[[9]]<- c(11,"Temperate or Subpolar Needleleaved Evergreen Shrubland - Open Canopy")
lc_cat[[10]]<- c(13,"Temperate or Subpolar Grassland")
lc_cat[[11]]<- c(14,"Temperate or Subpolar Grassland with a Sparse Tree Layer")
lc_cat[[12]]<- c(15,"Temperate or Subpolar Grassland with a Sparse Shrub Layer")
lc_cat[[13]]<- c(16,"Polar Grassland with a Sparse Shrub Layer")
lc_cat[[14]]<- c(17,"Polar Grassland with a Dwarf-Sparse Shrub Layer")
lc_cat[[15]]<- c(18,"Cropland")
lc_cat[[16]]<- c(19,"Cropland and Shrubland/woodland")
lc_cat[[17]]<- c(20,"Subpolar Needleleaved Evergreen Forest Open Canopy -  lichen understory")
lc_cat[[18]]<- c(21,"Unconsolidated Material Sparse Vegetation (old burnt or other disturbance")
lc_cat[[19]]<- c(22,"Urban and Built-up")
lc_cat[[20]]<- c(23,"Consolidated Rock Sparse Vegetation")
lc_cat[[21]]<- c(24,"Water bodies")
lc_cat[[22]]<- c(25,"Burnt area (resent burnt area)")
lc_cat[[23]]<- c(26,"Snow and Ice")
lc_cat[[24]]<- c(27,"Wetlands")
lc_cat[[25]]<- c(28,"Herbaceous Wetlands")

lc_cat_df<-as.data.frame(do.call(rbind,lc_cat))
names(lc_cat_df)<-c("GLC_ID","lc_cat_name")

#merge(table_5_paper_percent,lc_cat_df,by="GLC_ID"))
table_5_paper_all<- merge(table_5_paper_percent,lc_cat_df,by="GLC_ID")


########### INSECT INFESTATION DATASET ############

insect_rast<-raster("Insect_2001_2009_mask.rst")
extent(insect_rast)<-extent(lc_rast)
insect_rast_m<-mask(insect_rast,change_OR,NAflag=-9999,filename="Insect_2001_2009_mask_alaska.rst",overwrite=TRUE)

#infile_insect_vct <-"akfordmg01.shp"
infile_insect_vct <- "akfordmg1989_2010.shp"
infile_insect_path <- "./"
#insect_spdf<-readOGR(dsn="/Users/benoitparmentier/Dropbox/Data/Dissertation_paper1_07142012/insect_infestation_2013/",sub(".shp","",infile_insect_vct))
insect_spdf<-readOGR(dsn="./insect_infestation_2013/",sub(".shp","",infile_insect_vct))

proj4string(insect_spdf) # ok same projection but with different string definition
proj_ALB83<-"+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#r2 <- rasterize(insect_spdf, id_rast, field="DAMAGE2001", update=TRUE, updateValue="NA")

names(insect_spdf)

insect_spdf_01<-subset(insect_spdf,!is.na(insect_spdf$DAMAGE2001))
insect_spdf_02<-subset(insect_spdf,!is.na(insect_spdf$DAMAGE2002))
insect_spdf_03<-subset(insect_spdf,!is.na(insect_spdf$DAMAGE2003))
insect_spdf_04<-subset(insect_spdf,!is.na(insect_spdf$DAMAGE2004))
insect_spdf_05<-subset(insect_spdf,!is.na(insect_spdf$DAMAGE2005))
insect_spdf_06<-subset(insect_spdf,!is.na(insect_spdf$Damage2006))
insect_spdf_07<-subset(insect_spdf,!is.na(insect_spdf$Damage2007))
insect_spdf_08<-subset(insect_spdf,!is.na(insect_spdf$Damage2008))
insect_spdf_09<-subset(insect_spdf,!is.na(insect_spdf$Damage2009))

insect_spdf_01$y2001 <- rep(1,length(insect_spdf_01))
insect_spdf_02$y2002 <- rep(1,length(insect_spdf_02))
insect_spdf_03$y2003 <- rep(1,length(insect_spdf_03))
insect_spdf_04$y2004 <- rep(1,length(insect_spdf_04))
insect_spdf_05$y2005 <- rep(1,length(insect_spdf_05))
insect_spdf_06$y2006 <- rep(1,length(insect_spdf_06))
insect_spdf_07$y2007 <- rep(1,length(insect_spdf_07))
insect_spdf_08$y2008 <- rep(1,length(insect_spdf_08))
insect_spdf_09$y2009 <- rep(1,length(insect_spdf_09))

r2001 <- rasterize(insect_spdf_01, id_rast, field="y2001")
r2002 <- rasterize(insect_spdf_02, id_rast, field="y2002")
r2003 <- rasterize(insect_spdf_03, id_rast, field="y2003")
r2004 <- rasterize(insect_spdf_04, id_rast, field="y2004")
r2005 <- rasterize(insect_spdf_05, id_rast, field="y2005")
r2006 <- rasterize(insect_spdf_06, id_rast, field="y2006")
r2007 <- rasterize(insect_spdf_07, id_rast, field="y2007")
r2008 <- rasterize(insect_spdf_08, id_rast, field="y2008")
r2009 <- rasterize(insect_spdf_09, id_rast, field="y2009")

r2001[is.na(r2001)] <-0
r2002[is.na(r2002)] <-0
r2003[is.na(r2003)] <-0
r2004[is.na(r2004)] <-0
r2005[is.na(r2005)] <-0
r2006[is.na(r2006)] <-0
r2007[is.na(r2007)] <-0
r2008[is.na(r2008)] <-0
r2009[is.na(r2009)] <-0

instect_s <-stack(r2001,r2002,r2003,r2004,r2005,r2006,r2007,r2008,r2009)
insect_rast<- r2001+r2002+r2003+r2004+r2005+r2006+r2007+r2008+r2009


nb_c_500_seg<-raster("A_Change_image9_param_avg_seg_Alaska__500_01222013b_multicomp_1.rst")
change_OR<- nb_c_500_seg >0
#load images to plot
insect_rast_m<-mask(insect_rast,change_OR,NAflag=-9999,filename="insect_rast_mask_alaska.rst",overwrite=TRUE)

xtab_insect_s <-crosstab(insect_rast_m,change_OR)
insect_bool <- insect_rast_m > 0
xtab_insect_bool <-crosstab(insect_bool,change_OR)
raster_name<-"insect_bool.rst"
writeRaster(insect_bool, filename=raster_name,NAflag=-999,overwrite=TRUE)  #Writing the data in a raster file format...

#######################################
#########AREA OF CHANGE FOR DIFFERENT THRESHOLD AND PIXEL-SEG METHOD...
#Now Create table 5 for paper...

#using freq_r_stack function

table_nc_pix<-freq_r_stack(s_change_pix)
table_nc_seg<-freq_r_stack(s_change_seg)
names(table_nc_pix)<-c("value",paste("t_",rev(thresh_lab),sep=""))  #rev to reverse the elements of a vector
names(table_nc_seg)<-c("value",paste("t_",rev(thresh_lab),sep=""))  #rev to reverse the elements of a vector
table_nc_pix[,1]<-0:8
table_nc_seg[,1]<-0:7
#write out table...
write.table(table_nc_pix,file=paste("A_","pixels_table_nc_",out_prefix,".txt", sep=""),sep=",")
write.table(table_nc_seg,file=paste("A_","seg_table_nc_",out_prefix,".txt", sep=""),sep=",")
total_area<-freq(mask_alaska)[1,2]
table_propnc_pix<-(100*table_nc_pix[,2:4]/total_area)
table_propnc_pix
### 

list_obj<-mixedsort(list.files(pattern=paste(".*pixels.*",out_prefix,".*RData",sep="")))
change_pix_56<-load_obj(list_obj[[1]])
change_pix_100<-load_obj(list_obj[[2]])
change_pix_500<-load_obj(list_obj[[3]])

list_data_change<-vector("list", length(thresholds)) #This will contian the summary table per seg for every threshold
file_pat<-glob2rx(paste("*A_Change_image9_*pixels*_Alaska*",out_prefix,"*rst",sep="")) #Search for files in relation to fusion                  
lfc<-mixedsort(list.files(pattern=file_pat))
s_change_OR<-stack(lfc)
plot(s_change_OR)
change_OR<-mask(s_change_OR,mask_alaska)
change_OR<- s_change_OR >0

## Plot specific results from pixels
i=1
list_data_change<-vector("list", length(thresholds)) #This will contian the summary table per seg for every threshold
file_pat<-glob2rx(paste("*A_change_*pixels*_Alaska*",thresh_lab[i],"*",out_prefix,"*rst",sep="")) #Search for files in relation to fusion                  
lfc<-mixedsort(list.files(pattern=file_pat))
s_change<-stack(lfc)
s_change<-mask(s_change,mask_alaska)
plot(s_change)

########################## CREATING FIGURES FOR THE PAPER ###############################

source("paper1_dissertation_05122013_change_modification1_figures.R")
