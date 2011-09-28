#Learning to use python and IDRISI
#This script displays all the rst files in the working directory folder in IDRISI.

import os, glob, sys
from osgeo import gdal, gdalconst
import win32com.client
import shutil
import numpy

#variables
inputf1='seg0_9_b_STDV_rd_092311_Chain_cluster__f8_7' #input image containing the cluster map
inputf2='wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83'

wd= 'C:\Data\Benoit\Clark_University\Thesis_work_PhD\Dissertation_paper3\Jaccard_Index_Python\\'

api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
##wd = api.GetWorkingDir();                                      #Is variable containing the path of the working directory
                     
                                                            #Note that the list is in a unicode format hence the u' before the string



##BREAKOUT  1*C:\Data\Benoit\Clark_University\Thesis_work_PhD\Dissertation_paper3\Jaccard_Index_Python\seg0_9_b_STDV_rd_092311_Chain_cluster__f8_7.rst*C:\Data\Benoit\Clark_University\Thesis_work_PhD\Dissertation_paper3\Jaccard_Index_Python\test*1
parameters = '1*'+wd+inputf1+'*'+wd+inputf1+'*1'                          #Note that suffix "_bool_Class_1" will be added to category 1 and so on
module = 'breakout'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
   print('Error running ' + module + '!')

##BREAKOUT  1*C:\Data\Benoit\Clark_University\Thesis_work_PhD\Dissertation_paper3\Jaccard_Index_Python\seg0_9_b_STDV_rd_092311_Chain_cluster__f8_7.rst*C:\Data\Benoit\Clark_University\Thesis_work_PhD\Dissertation_paper3\Jaccard_Index_Python\test*1
parameters = '1*'+wd+inputf2+'*'+wd+inputf2+'*1'                          #Note that suffix "_bool_Class_1" will be added to category 1 and so on
module = 'breakout'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
   print('Error running ' + module + '!')
   
#BREAKOUT  1*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399.rst*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399*1

listfilesf1_wd = glob.glob(wd+inputf1+'_bool_Class_'+'*'+'.rst')         #This creates a list with all the rst files in the working directory
listfilesf2_wd = glob.glob(wd+inputf2+'_bool_Class_'+'*'+'.rst')         #This creates a list with all the rst files in the working directory

nb_f1 = len(listfilesf1_wd)
nb_f2 = len(listfilesf2_wd)

Jacard_table = numpy.ones((nb_f2+1,nb_f1+1)) #declaring an array of size stop+1 and initialized with number 1

###########################################LOOP TO DEFINE THE UNBURNT PIXELS##############
#processed_poylgon 

i = 0                                                                  #Note that the list is in a unicode format hence the u' before the string
filename2 = 'wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83_bool_Class_1.rst'

for filename in listfilesf1_wd:        #tThis is a loop through a list in which there is no update. To make update in a list use a copy 
    i = i + 1
    
    # Extract the IDRISI filename from the full path
    idrisi_filename = filename[filename.rfind('\\')+1:-4]        #This select the name within the string excluding the extension
    id_polygon_current = idrisi_filename[idrisi_filename.rfind('_')+1:]      # Extract cluster number from filename by finding the last '_'

    idrisi_filename2 = filename2[filename2.rfind('\\')+1:-4]  
    id_polygon_current2 = idrisi_filename2[idrisi_filename2.rfind('_')+1:]
    j=1
    ###OVERLAY MODULE###    
    parameters = '1*'+wd+idrisi_filename+'.rst'+'*'+wd+idrisi_filename2+'.rst'+'*'+wd+idrisi_filename+'_overlay_'+id_polygon_current+'_'+id_polygon_current2+'.rst'
    module = 'overlay'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
       print('Error running ' + module + '!')

##       ##BREAKOUT  1*C:\Data\Benoit\Clark_University\Thesis_work_PhD\Dissertation_paper3\Jaccard_Index_Python\seg0_9_b_STDV_rd_092311_Chain_cluster__f8_7.rst*C:\Data\Benoit\Clark_University\Thesis_work_PhD\Dissertation_paper3\Jaccard_Index_Python\test*1
##    parameters = '1*'+wd+inputf2+'*'+wd+inputf2+'*1'                          #Note that suffix "_bool_Class_1" will be added to category 1 and so on
##    module = 'breakout'
##    print('Running ' + module + ' module with parameters ' + parameters)
##    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
##    if not success:
##       print('Error running ' + module + '!')       
##           
    ###AREA###
    parameters = wd+idrisi_filename+'_overlay_'+id_polygon_current+'_'+id_polygon_current2+'.rst'+'*'+'2*1*'+wd+'area_selection.avl'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
    module = 'area'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
       print('Error running ' + module + '!')

    if id_polygon_current!=0 and id_polygon_current2!=0:
        f1 = open(wd+'area_selection.avl','r')
        linef1=f1.readline() #read the first line which pertains to category 0
        linef1=f1.readline()   # reading the first line (category 1)
        linef1_2=f1.readline() #reading the second line (category 2)
        if linef1 !='':
            id1, area_cat1_str =linef1.rsplit(' ')
        else:
            id1='1'
            area_cat1_str='0'
        if linef1_2 !='':
            id2, area_cat2_str =linef1_2.rsplit(' ')
        else:
            id1='2'
            area_cat2_str='0'            
        if area_cat1_str=='0'and area_cat2_str=='0':
             Jacard_Index = 0
        else:             
            Jacard_Index = (float(area_cat2_str)/(float(area_cat1_str)+float(area_cat2_str)))*100

        Jacard_table[j,i]= Jacard_Index
    
outfile1=wd+'Jacard_table.txt'
numpy.savetxt(outfile1, Jacard_table, fmt='%-7.6f')    



     
    #print 'Loop number is '+loop_number
#print('Loop number ' + module + '!')
#run    
