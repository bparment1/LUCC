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
library(multicomp)
###Parameters and arguments

infile1<-"ghcn_or_tmax_b_04142012_OR83M.shp"
#infile2<-"dates_interpolation_03052012_2dates_test.txt"
infile2<-"dates_interpolation_03052012.txt"                                          #List of 10 dates for the regression

#outfile
#path<-"/Users/benoitparmentier/Dropbox/Data/Dissertation_paper1_04142012"
path<-"C:/Users/parmentier/Dropbox/Data/Dissertation_paper1_07142012/Paper1_sta_revision_12202011"
setwd(path)

prop<-0.3                                                                           #Proportion of testing retained for validation   
seed_number<-100
out_prefix<-"_07132012_multicomp_1"

#######START OF THE SCRIPT #############


l<-list.files(pattern="_MK_P.rst")
s1<-stack(l) #Create a stack from the list of files/layers

s1_1<-raster(s1, layer=1)


f<-l[1]  #first file with .rst extension
a1<-raster(f)   #Create a R object of the RasterLayer type
a1              #Print the summary
plot(a1, main="First test")        #Plot the image
inMemory(a1)                       #Check if the file is in memory...

a1_g<-as(a1,"SpatialGridDataFrame") #Conversion to spatial grid data frame
image(a1_g)
t1<-a1*2                           #Create new raster by multiplying values by 2
plot(t1, main="This is the second test")   #Plot the modified file

t1_d<-as.data.frame(t1)           #Convert the image in a data frame...Could use spatial grid data frame...

writeRaster(t1, filename="R_test_Alaska2.rst")  #Writing the data in a raster file format...(IDRISI)

#### End of script #####
