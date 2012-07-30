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

infile1<-"ghcn_or_tmax_b_04142012_OR83M.shp"
#infile2<-"dates_interpolation_03052012_2dates_test.txt"
infile2<-"dates_interpolation_03052012.txt"                                          #List of 10 dates for the regression
infile3<-"Change_image9_parameters_CMK.rst"
#outfile
path<-"/Users/benoitparmentier/Dropbox/Data/Dissertation_paper1_07142012/Paper1_sta_revision_12202011"
#path<-"C:/Users/parmentier/Dropbox/Data/Dissertation_paper1_07142012/Paper1_sta_revision_12202011"
setwd(path)

prop<-0.3                                                                           #Proportion of testing retained for validation   
seed_number<-100
out_prefix<-"_07132012_multicomp_1"

#######START OF THE SCRIPT #############
ref<-readGDAL(infile3)
l1<-list.files(pattern="_MK_P.rst")
l2<-list.files(pattern="_CMK_P_reclassed.rst")
s1<-stack(l1) #Create a stack from the list of files/layers
s2<-stack(l2)

s1_1<-raster(s1, layer=1)  #Select layer 1 from the stack
s1_1b<-subset(s1,1)        #subset layer 1 from stack using subset command
s2_1<-raster(s2, layer=1)  #This is a boolean layer!!
dataType(s2_1)
dataType(s1_1)             #Should be FLT4S because it is an MK proportion...
id<-s2_1
values(id)<-1:ncell(s2_1)

s2_sgdf<-as(s2,"SpatialGridDataFrame") #Conversion to spatial grid data frame

#projection(silverdollar) <- "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

freq_s2_1<-freq(s2_1)  #Calculate the number of pixels in each category in the raster...
f<-l1[1]  #first file with .rst extension
a1<-raster(f)   #Create a R object of the RasterLayer type
a1              #Print the summary
plot(a1, main="First test")        #Plot the image
plot(a1, xlab = "Easting (m)", ylab = "Northing (m)", main="Alaska")
inMemory(a1)                       #Check if the file is in memory...

#CONVERSION
rc<-raster(ref)
freq(rc)
rc[rc==0] <- NA   #Reclassifying category 0 in
rc1_2 <- rc==1 | rc==2 #Reclassify for category 1 and 2, note that this creates a boolean layer...
count(rc1_2,0)    #This counts the number of 0 values in the raster rc1_2

s1_1_p005<-s1_1>0.05
a1_g<-as(a1,"SpatialGridDataFrame") #Conversion to spatial grid data frame

a1_s<-raster(a1_g)
image(a1_g)
t1<-a1*2                           #Create new raster by multiplying values by 2
plot(t1, main="This is the second test")   #Plot the modified file

test<-values(t1)       #This extract values of the raster into a numeric object
t1<-clearValues(t1)     #Remove values from memory

levelplot(s1_1)  #Using rasterVis option
levelplot(s1)    #UPlots 9 images using rasterVis option
hist(s1_1)       #uses only certain percentage of image (sampling)
histogram(s1_1, breaks=100)  #Uses options from rasterVis

t1_d<-as.data.frame(t1)           #Convert the image in a data frame...Could use spatial grid data frame...

showOptions()
writeRaster(t1, filename="R_test_Alaska2.rst",overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
writeRaster(s1_1_p005, filename="R_test_Alaska3.rst")  #Writing the data in a raster file format...(IDRISI)

#### End of script #####
