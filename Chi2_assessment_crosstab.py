#This script was written by Benoit Parmentier on December 21, 2011.
#This script runs the CROSSTAB module in IDRISI to compare two maps at the category level.
#Each category in the fisrt map is compared to the categories in teh other map using the
#Chi Square (Chi2_) and Cramer's V (CV) statistics. The output are tables for CV and Chi2
#stored in space delimeted text format.

import os, glob, sys
from osgeo import gdal, gdalconst
import win32com.client
import shutil
import numpy

#Arguments parameters for the script: variables, input parameters

inputf0= 'ctab1.id$'  # This is a list of group file to use in t
inputf2= 'seg0_9_b_STDV_rd_092311_Chain_cluster__f8_7' #do not include extension!!
inputf1= 'wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83' # do not include extension!!
inputf3='wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83' # This is the mask file.

output_prefix= 'Eco_ChainC_12212011_'  #names

wd1= 'C:\Data\Benoit\Clark_University\Thesis_work_PhD\Dissertation_paper3\Chi2_test_Python\\' 



api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api


#Chi2_table = numpy.ones((cat_map1,cat_map2)) #declaring an array of size stop+1 and nb_files initialized with number 1, it will hold Cramer's V stat

#RUN BREAKOUT FOR MAP1
parameters = '1*'+wd1+inputf1+'.rst'+'*'+wd1+inputf1+'*1'
module = 'breakout'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
   print('Error running ' + module + '!')

#RUN BREAKOUT FOR MAP2
parameters = '1*'+wd1+inputf2+'.rst'+'*'+wd1+inputf2+'*1'
module = 'breakout'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
   print('Error running ' + module + '!')

f1 = open( wd1+inputf1+'.rgf','r')
f2 = open( wd1+inputf2+'.rgf','r')

listlines_f1 = f1.readlines()  #Reading all the lines in the file
listlines_f2 = f2.readlines()  #Reading all the lines in the file
nb_f1 =len(listlines_f1)  #nb_f1 is the number of categories in the map including value 0 but should be deduced by -1 because rgf has one line with the number of files
nb_f2 =len(listlines_f2)
f1.close()
f2.close()
##nb_f1 = len(listfilesf1_wd)
##nb_f2 = len(listfilesf2_wd)

Chi2_table = numpy.ones((nb_f1-2,nb_f2-2)) #declaring an array of size stop+1 and nb_files initialized with number 1, it will hold Cramer's V stat
CV_table = numpy.ones((nb_f1-2,nb_f2-2)) #declaring an array of size stop+1 and nb_files initialized with number 1, it will hold Cramer's V stat



#RUN CROSSTAB IN A LOOP and extract Chi2 and Chramer's V (CV) statistic
#This is a nested loop
###########################################LOOP TO DEFINE THE UNBURNT PIXELS##############
#processed_poylgon 

j = 0                                                                  #Note that the list is in a unicode format hence the u' before the string
#filename2 = 'wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83_bool_Class_1'
i= 0

##for i in range(2,nb_f2):
##    filename2= listlines_f2[i]

filename3=inputf3 #This is the mask file.

#filename2='seg0_9_b_STDV_rd_092311_Chain_cluster__f8_7_bool_Class_1.rst'

for i in range(2,nb_f2):
    filename2= listlines_f2[i]
    for j in range(2,nb_f1):
    ##for filename in listfilesf1_wd:        #tThis is a loop through a list in which there is no update. To make update in a list use a copy 
        filename1= listlines_f1[j]
        #filename2= listlines_f2[j]
        idrisi_filename1= filename1.rstrip('\n') #Remove the LF character from the string
        idrisi_filename2= filename2.rstrip('\n') #Remove the LF character from the string
        id_polygon_current = idrisi_filename1[idrisi_filename1.rfind('_')+1:]      # Extract cluster number from filename by finding the last '_'

        id_polygon_current2 = idrisi_filename2[idrisi_filename2.rfind('_')+1:]
        #RUN CROSSTAB
        parameters = '1*'+wd1+idrisi_filename1+'.rst'+'*'+wd1+idrisi_filename2+'*NONE*'+wd1+filename3+'.rst'+'*2*none*N'
        #parameters = '1*'+wd1+idrisi_filename1+'.rst'+'*'+wd1+idrisi_filename2+'*NONE*C:\Data\Benoit\Clark_University\Thesis_work_PhD\Dissertation_paper3\Jaccard_Index_Python\wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83.rst*2*none*N'
        module = 'crosstab'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')

        f0 = open( wd1+inputf0,'r')
        listlines_f0 = f0.readlines()  #Reading all the lines in the file
        f0.close()
        
        temp1, Chi2_str= listlines_f0[10].rsplit('=')
        temp2, CV_str= listlines_f0[13].rsplit('=')
        
        Chi2=float(Chi2_str.rstrip('\n'))
        CV=float(CV_str.rstrip('\n'))
        Chi2_table[j-2,i-2]= Chi2            #In order to write the value in the first row and column we need to do minus 2
        CV_table[j-2,i-2]= CV
        #NOTE THAT J correspond to rows and i to columnns in the output table or matrix!!!!
        #NOTE THAT the index starts at 0
    
outfile1=wd1+output_prefix+'Chi2_table.txt'
outfile2=wd1+output_prefix+'CV_table.txt'
numpy.savetxt(outfile1, Chi2_table, fmt='%-7.6f')
numpy.savetxt(outfile2, CV_table, fmt='%-7.6f')

print('Running script completed')
