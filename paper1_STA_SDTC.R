##################    USING RASTER PACKAGE FOR ANALYSIS    #######################################
###########################  multi comparison tests  #############################################
#This script is a first attempt at using R for multi-comparison tests...                       #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 01/19/2013                                                                                 #
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
###Parameters and arguments

infile1<-"seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0.shp"
infile2<-"wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83.rst"
#infile2<-"wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83.shp"                                          #Ecoregions...
infile3<-"Change_image9_parameters_CMK.rst"
infile4<-"seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0.rst" #Seg ID
infile5<-"mask_land2.rst"
infile6<-"Alaska_globcover2009_ALB83_mask.rst"
infile6b<-"Alaska_globcover2009_ALB83.rst"
#outfile
#path<-"/Users/benoitparmentier/Dropbox/Data/Dissertation_paper1_07142012/Paper1_sta_revision_12202011"
path_in<-"/home/parmentier/Data/Paper1_sta_revision_01192013"
#path<-"C:/Users/parmentier/Dropbox/Data/Dissertation_paper1_07142012/Paper1_sta_revision_12202011"
setwd(path_in)

#thresholds<- c(0.05,0.025)  #which correspond to alpha 10% and 5% probability level since the test must from two side, do 1%??
thresholds_input<- c(0.05)
out_prefix<-"_01192013_multicomp_1"
prop<-0.5
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
#if (correction==1){...}
alpha_adj<- thresholds_input
for (i in 1:length(thresholds_input)){
  alpha_adj[i]<-thresholds_input[i]/nlayers(s1) #Bonferroni adjustment
}
thresholds<-(c(thresholds_input,alpha_adj))
thresh_lab<-as.character(round(thresholds*10000))


i=1
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
  write.table(table_change_paper,file=paste("A_","table_change_paper_",(thresh_lab[i]),out_prefix,".txt", sep=""),sep=",")
  list_table_change[[i]]<-table_change_paper
  list_xtab_change[[i]]<-list_xtab2 #?? change here
  sta_change_obj<-list(data_change,list_xtab2,list_table_change)
  names(sta_change_obj)<-c("dat_change_seg","list_xtab_change","table_change")
  save(sta_change_obj, file=paste("paper1_sta_change_obj_",(thresh_lab[i]),out_prefix,".RData", sep=""))
  #return(sta_change_obj)
}


#mclapply(1:length(thresholds), runChangeFunction,mc.preschedule=FALSE,mc.cores = 9)

save(list_xtab_change, file=paste("list_xtab_",(thresh_lab[i]),out_prefix,".RData",sep=""))
save(list_table_change, file=paste("list_data_change_",(thresh_lab[i]),out_prefix,".RData", sep=""))

#Plot change image for each sta parameter:
#lf<-list.files(pattern=paste("^A_avg_seg_change_Alaska.*.",out_prefix,".*.rst$",sep=""))
file_pat<-glob2rx(paste("*A_avg_seg_change_Alaska*","c_500",out_prefix,"*rst",sep="")) #Search for files in relation to fusion                  
lf<-mixedsort(list.files(pattern=file_pat))
lf<-list.files(pattern=file_pat)
#match("*nb*",lf)

r_stack<-stack(lf)


#file_pat<-glob2rx(paste(A_))
l_f<-list.files(pattern=paste("^A_change_Alaska__c_OR_.*.",out_prefix,".*.rst$",sep=""))

nb<-maxValue(ecoreg) #number of regions in the map
plot(ecoreg,col=rainbow(nb))

list_data_change<-vector("list", length(thresholds)) #This will contian the summary table per seg for every threshold
     
for (i in 1:length(thresholds)){
  change_OR<-raster(l_f[[i]])
  change_OR<-mask(change_OR,mask_alaska)
  xtab_eco<-crosstab(ecoreg,change_OR)
  write.table(xtab_eco,file=paste("A_","xtab_tb_eco_",(thresh_lab[i]),out_prefix,".txt", sep=""),sep=",")
  #list_xtab_eco[[i]]<-xtab_eco
  
}


# PART 5 use segid and count...threshold at 50%...

#### End of script #####
