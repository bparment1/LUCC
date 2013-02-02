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

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}
###Parameters and arguments

infile1<-"seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0.shp"
infile2<-"wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83.rst"
infile2b<-"wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83.vct"                                          #Ecoregions...
infile3<-"Change_image9_parameters_CMK.rst"
infile4<-"seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0.rst" #Seg ID
infile5<-"mask_land2.rst"
infile6<-"Alaska_globcover2009_ALB83_mask.rst"
infile6b<-"Alaska_globcover2009_ALB83.rst"
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
out_prefix<-"_01222013_multicomp_1"

prop<-0.5
correction=0 #if correction is 1 then doa 

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

nb<-maxValue(ecoreg) #number of regions in the map
plot(ecoreg,col=rainbow(nb))
f_fire<-"MTBS_AK_2001_2009_IDR_ID.rst"
r_fire<-raster(f_fire)
r_fire<-mask(r_fire,mask_alaska)
r_fire<- r_fire >0
thresh_lab_tmp<-rev(thresh_lab)
labels<-c(paste(thresh_lab_tmp,"seg",sep="_"),paste(thresh_lab_tmp,"pix",sep="_"))
#Select the relevant change image?
#change_OR<-raster(s_change,layer=3)
list_xtab_eco<-vector("list",nlayers(s_change))
list_xtab_fire<-vector("list",nlayers(s_change))

#Make this a function
for (i in 1:nlayers(s_change)){
  change_OR<-raster(s_change, layer=i)
  change_OR<- change_OR > 0
  xtab_eco<-crosstab(ecoreg,change_OR)
  #write.table(xtab_eco,file=paste("A_","xtab_tb_eco_",(labels[i]),out_prefix,".txt", sep=""),sep=",")
  xtab_fire<-crosstab(r_fire,change_OR) 
  #write.table(xtab_fire,file=paste("A_","xtab_tb_fire_",(labels[i]),out_prefix,".txt", sep=""),sep=",")
  list_xtab_fire[[i]]<-cbind(xtab_fire[,2])
  list_xtab_eco[[i]]<-cbind(xtab_eco[,2])  
}
tmp<-as.data.frame(do.call(cbind,list_xtab_eco))
names(tmp)<-labels
tmp2<-as.data.frame(do.call(cbind,list_xtab_fire))
names(tmp2)<-labels
total2<-freq(r_fire)[2,2]
total1<-freq(ecoreg)[1:17,2]
total<-c(total1,total2)
table_4_paper<-rbind(tmp,tmp2[2,]) #category 18 is fire!!!
table_4_paper$total_area<-total
table_4_paper_percent<-(table_4_paper[,1:6]/total)*100
table_4_paper_percent$total_area<-total
write.table(table_4_paper,file=paste("A_","paper1_sta_table_4_paper_",out_prefix,".txt", sep=""),sep=",")
write.table(table_4_paper_percent,file=paste("A_","paper1_sta_table_4_paper_",out_prefix,".txt", sep=""),
            append=TRUE,sep=",")

#######################################
#########AREA OF CHANGE FOR DIFFERENT THRESHOLD AND PIXEL-SEG METHOD...
#Now Create table 5 for paper...

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

### Creating figures
cat_names<-c("Alaska Peninsula montane Taiga",
             "Alaska St Elias Range Tundra",
             "Aleutian Islands tundra",
             "Arctic coastal tundra",
             "Arctic foothilss tundra",
             "Beringia lowland tundra",
             "Beringia upland tundra",
             "Brooks-British range tundra",
             "Cook Inlet taign",
             "Copper Plateau taiga",
             "Interior Alaska-Yukon lowland taiga",
             "Interior Alaska-Yukon alpine tundra",
             "Northern Cordillera forests",
             "Ogilvie Mackenzie alpine tundra",
             "Pacific Coastal Mountain icefiles and tundra")

#col_palette<-ecoreg@legend@colortable
nb<-length(unique(ecoreg))
ecoreg@legend@colortable<-rainbow(nb)
plot(ecoreg,col=rainbow(nb), colNA="transparent")
plot(seg_id)
#plot(RGB)...
pal<-rainbow(nb)
pal[11]<-"green"
pal[12]<-"darkgreen"
plot(ecoreg,col=c(colorRampPalette(pal)))

###############################
##Figure 1: wwf ecoregion

col_mfrow<-1
row_mfrow<-1
png(filename=paste("Figure5_paper1_LSA1_change_Alaska",out_prefix,".png",sep=""),
    width=col_mfrow*480,height=row_mfrow*480)
#par(mfrow=c(1,2))

LSTA1_change<-raster("A_avg_seg_change_Alaska__LST_A1_c_500_01222013b_multicomp_1.rst")
plot(LSTA1_change,legend=FALSE,col=c("black","red"))
legend("topright",legend=c("no change","change"),title="Change category",
       pt.cex=0.9,fill=c("black","red"),bty="n")

label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=0.7)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 125000, fill=c("transparent","black"),plot.grid=FALSE)
dev.off()


###############
##Figure 4: nubmer of change and change OR for 500 seg

#load images to plot
nb_c_500_seg<-raster("A_Change_image9_param_avg_seg_Alaska__500_01222013b_multicomp_1.rst")
nb_c_500_seg_OR<- nb_c_500_seg >0

#set up the output file to plot
col_mfrow<-2
row_mfrow<-1
png(filename=paste("Figure4_paper1_nb_c_OR_change_Alaska",out_prefix,".png",sep=""),
                   width=col_mfrow*480,height=row_mfrow*480)
par(mfrow=c(1,2))

#set up plotting parameters
#col_pal<-rev(terrain.colors(unique(nb_c_500_seg)))
col_pal<-rev(terrain.colors(8))
scale_position<-c(450000, 600000)
arrow_position<-c(900000, 600000)

##plot Fig 4a
#plot(nb_c_500_seg,legend=FALSE,col=rev(terrain.colors(8)))
plot(nb_c_500_seg,legend=FALSE,col=col_pal)
legend("topright",legend=c(0:7),title="Number of change",
       pt.cex=0.9,fill=rev(terrain.colors(8)),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=0.7)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 150000, fill=c("transparent","black"),plot.grid=FALSE)
##plot Fig 4b
plot(nb_c_500_seg_OR,legend=FALSE,col=c("black","red"))
legend("topright",legend=c("no change","change"),title="Change category",
       pt.cex=0.9,fill=c("black","red"),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=0.7)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 125000, fill=c("transparent","black"),plot.grid=FALSE)
dev.off()

###############################
##Figure 5: change 500 for LST

col_mfrow<-1
row_mfrow<-1
png(filename=paste("Figure5_paper1_LSA1_change_Alaska",out_prefix,".png",sep=""),
    width=col_mfrow*480,height=row_mfrow*480)
#par(mfrow=c(1,2))

LSTA1_change<-raster("A_avg_seg_change_Alaska__LST_A1_c_500_01222013b_multicomp_1.rst")
nb_c_500_seg_OR
plot(LSTA1_change,legend=FALSE,col=c("black","red"))
legend("topright",legend=c("no change","change"),title="Change category",
       pt.cex=0.9,fill=c("black","red"),bty="n")

label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=0.7)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 125000, fill=c("transparent","black"),plot.grid=FALSE)
dev.off()

###############################
##Figure 6: barplot land cover categories: change per number and shape parameters




###############################
##Figure 7: change 500 with fire polygons???

col_mfrow<-2
row_mfrow<-2
png(filename=paste("Figure7_paper1_change_Alaska_fire_perimeters",out_prefix,".png",sep=""),
    width=col_mfrow*480,height=row_mfrow*480)
#par(mfrow=c(1,2))

#LSTA1_change<-raster("A_avg_seg_change_Alaska__LST_A1_c_500_01222013b_multicomp_1.rst")
infile_fire<-sub(".shp","","MTBS_AK_2001_2009_IDR_ID.shp")             #Removing the extension from file.
mtbs_pol <- readOGR(".",infile_fire)
nb_c_500_seg_OR
plot(nb_c_500_seg_OR,legend=FALSE,col=c("black","red"))
legend("topright",legend=c("no change","change"),title="Change category",
       pt.cex=2,cex=2,fill=c("black","red"),bty="n")

label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=1)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 125000, fill=c("transparent","black"),plot.grid=FALSE)
#CRS_interp<-proj4string(interp_area)
#temp.colors <- colorRampPalette(c('blue', 'lightgoldenrodyellow', 'red'))
#plot(mtbs_pol, add=TRUE,border="lightgoldenrodyellow")
plot(mtbs_pol, add=TRUE,border="yellow")
#par(usr=c(-216, -66, 24, 144))   # you should be able to 'automate' this calculation 
par(usr=c(1000000,1500000,1500000,2000000))
plot(mtbs_pol, border="red",add=TRUE) #this plot without fill...
e <- drawExtent()
w_map<-crop(nb_c_500_seg_OR,e)
dev.off()

###############################
##Figure 8: combined-- change 500 for NDVI A0 pix and seg
col_mfrow<-1
row_mfrow<-2
png(filename=paste("Figure_8_paper1_nb_c_OR_change_Alaska",out_prefix,".png",sep=""),
    width=col_mfrow*960,height=row_mfrow*960)
par(mfrow=c(2,1))

##Figure 8a
NDVIA0_c_pix<-raster("A_change_pixels_Alaska_NDVI_A0_c_500_01222013_multicomp_1.rst")
plot(NDVIA0_c_pix,legend=FALSE,col=c("black","red"))
legend("topright",legend=c("no change","change"),title="Change category",
       pt.cex=0.9,fill=c("black","red"),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=0.7)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 125000, fill=c("transparent","black"),plot.grid=FALSE)

##Figure 8b
NDVIA0_c_seg<-raster("A_avg_seg_change_Alaska__NDVI_A0_c_500_01222013_multicomp_1.rst")
plot(NDVIA0_c_seg,legend=FALSE,col=c("black","red"))
legend("topright",legend=c("no change","change"),title="Change category",
       pt.cex=0.9,fill=c("black","red"),bty="n")
label_scalebar<-c("0","125","250")
scalebar(d=250000, xy=scale_position, type = 'bar', 
         divs=3,label=label_scalebar,below="kilometers",
         cex=0.7)
#this work on non sp plot too
SpatialPolygonsRescale(layout.north.arrow(), offset = arrow_position, 
                       scale = 125000, fill=c("transparent","black"),plot.grid=FALSE)
dev.off()

#### End of script #####

#
im_window = raster("/Users/benoitparmentier/Dropbox/Data/Dissertation_paper1_07142012/Figures_Dissertation_paper1/Figure1_Map_of_Study_area.png") 
im_window=as.matrix(w_map)
add.image(xpos=1000000, ypos=1500000,z=im_window, col=c("black","red"),
  image.width = 0.15) #points the left-bottom corner and the reative size of image 
par(new=TRUE, plt=c(0,1,0,1), mar=c(0,0,0,0), usr=c(0,1,0,1))
add.image(xpos=0,ypos=0,z=im_window, col=c("black","red"),
          image.width = 0.15)
#use par new with usr
#plot fire poly on the image inset