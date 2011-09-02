#This script was written by Benoit Parmentier on September 2, 2011.
#This script runs the CHAIN CLUSTER MODULE over a range of distance value. It necessitates pseudo-images as input.
#The maximum number of clusters and sequence parameter may be changed within this script.
#Results are translated back into pixels images.Note that input bands must also be pseudo images.
#The output also provides a textfile (ending with "_cluster_max.txt"). It containst the number of cluster for each distance.

import os, glob, sys
from osgeo import gdal, gdalconst
import win32com.client
import shutil
import numpy

#Arguments parameters for the script: variables, input parameters

#inputf1= 'ps_seg12_0_STDTS_9_avg.rgf'
#inputf3= 'ps_seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0_PIXID'
#inputf2= 'seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0'

inputf1= 'seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0_rd1_STDV__6_avg.rgf'
inputf3= 'ps_seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0rd1_STDV__PIXID' # do not include extension
inputf2= 'seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0' #do not include extension

output_prefix= 'seg0_6_bands_Chain_cluster_rd1seq1_STDV'  #names
#output_prefix= 'test_STDV'
#output_prefix= 'seg0_TESTChain_cluster_TEST'  #names

mask_image ='none'
#wd1= 'H:\Benoit_Backup\Paper3_STA_07202011\Segments_classifications\\' #Path
wd1= 'H:\Benoit_Backup\Paper3_STA_07202011\Segments_classifications\seg_classification_random\\' #Path

step = 0.5;  #If random is equal 1 then create a random pseudo image with ID at random places 
dist_max = 15;
dist_min = 3;
max_cluster=25500;
sequence = 1;

#START OF THE SCRIPT
api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
wd = api.GetWorkingDir();                                      #Is variable containing the path of the working directory
wd = 'H:\Benoit_Backup\Paper3_STA_07202011\\'

stop=int((dist_max-dist_min)/step);
distance=dist_min;

max = numpy.ones((stop+1)) #declaring an array of size stop+1 and initialized with number 1

i=0;

for i in range(1,stop+2,1):
    
    #RUN MODULE CHAINCLUSTER
    
    parameters = wd1+inputf1+'*'+wd1+'ps_'+output_prefix+'_'+str(i)+'.rst'+'*'+mask_image+'*'+str(distance)+'*'+'n'+'*'+str(max_cluster)+'*'+str(sequence)
    module = 'chaincluster'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
        print('Error running ' + module + '!')


    #RUN MODULE EXTRACT
    parameters = wd1+inputf3+'.rst'+'*'+wd1+'ps_'+output_prefix+'_'+str(i)+'.rst'+'*'+'1*1*'+wd1+'ps_'+output_prefix+'_'+str(i)+'.avl'
    module = 'extract'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
        print('Error running ' + module + '!')
   
    #RUN ASSIGN
    parameters = wd1+inputf2+'.rst'+'*'+wd1+output_prefix+'_'+str(i)+'.rst'+'*'+wd1+'ps_'+output_prefix+'_'+str(i)+'.avl'   
    module = 'assign'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
        print('Error running ' + module + '!')

    stat =api.CalculateRasterMinMax(wd1+'ps_'+output_prefix+'_'+str(i)+'.rst','1','2')
    tmp= stat[2]
    max[i-1]=tmp #this array contains the number of cluster in each image
    
    distance=distance+step

#Write the results in a textfile
outfile=wd1+output_prefix+'_cluster_max.txt'
numpy.savetxt(outfile, max, fmt='%-7.2f')


    
   

