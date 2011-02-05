#This script was written by Benoit Parmentier on September 2, 2011.
#This script runs the CHAIN CLUSTER MODULE over a range of distance value. It necessitates pseudo-images as input.
#The maximum number of clusters and sequence parameter may be changed within this script.
#Results are translated back into pixels images.Note that input bands must also be pseudo images.
#The output also provides a textfile (ending with "_cluster_max.txt"). It containst the number of cluster for each distance.

import os, glob, sys
#from osgeo import gdal, gdalconst
import win32com.client
import shutil
import numpy

#Arguments parameters for the script: variables, input parameters

#inputf1= 'ps_seg12_0_STDTS_9_avg.rgf'
#inputf3= 'ps_seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0_PIXID'
#inputf2= 'seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0'

inputf0='list_groupfiles_random_09282011.txt'  # This is a list of group file to use in the processing
inputf4='list_ID_random_09282011.txt'                                     # This is a list of group file to use in the processing
inputf2= 'seg12_NDVI_LST_ALB_A0_A1_A2_TS_s_0_450__0' #do not include extension!!
inputf5= 'wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83' # do not include extension!!
inputf6='ps_wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83_mode.avl'

output_prefix= 'seg0_9_b_STDV_rd_092311_Chain_cluster_'  #names

mask_image ='none'
#wd1= 'H:\Benoit_Backup\Paper3_STA_07202011\Segments_classifications\\' #Path
#wd1= 'H:\Benoit_Backup\Paper3_STA_07202011\Segments_classifications\seg_classification_random9\\' #Path
#Problem with path!!! \bparmentier becuase of \b!!!! so added \\b 
#wd1='C:\\Users\\bparmentier\\Google Drive\\Dissertation_paper3_09262014\\seg_classification_random9\\'

wd1= 'J:\\Benoit\\Data\\Dissertation_paper3_09262014\\seg_classification_random9_02202015'

step = 0.5;  #If random is equal 1 then create a random pseudo image with ID at random places 
dist_max = 15;
dist_min = 3;
max_cluster=25500;
sequence = 1;

#START OF THE SCRIPT
api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
wd = api.GetWorkingDir();                                      #Is variable containing the path of the working directory
#wd = 'H:\Benoit_Backup\Paper3_STA_07202011\\'
#wd = 'C:\\Users\\bparmentier\\Google Drive\\Dissertation_paper3_09262014\\test_02172015'
wd = 'J:\\Benoit\Data\\Dissertation_paper3_09262014\\test_02202015'

stop=int((dist_max-dist_min)/step);
distance=dist_min;


##    #RUN MODULE EXTRACT TO GET THE MODE FOR EVERY WWF REGION
##    parameters = wd1+inputf2+'.rst'+'*'+wd1+inputf5+'.rst'+'*'+'1*5*'+wd1+'ps_'+inputf5+'_mode.avl'
##    module = 'extract'
##    print('Running ' + module + ' module with parameters ' + parameters)
##    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
##    if not success:
##        print('Error running ' + module + '!')
#wd1='C:\\Users\\bparmentier\\Google Drive\\Dissertation_paper3_09262014\\'
f1 = open( wd1+inputf0,'r')
f2 = open( wd1+inputf4,'r')

listlines_f1 = f1.readlines()  #Reading all the lines in the file
listlines_f2 = f2.readlines()  #Reading all the lines in the file
nb_files =len(listlines_f2)

f1.close()
f2.close()

max = numpy.ones((stop+1,nb_files+1)) #declaring an array of size stop+1 and initialized with number 1
crosstab = numpy.ones((stop+1,nb_files+1)) #declaring an array of size stop+1 and nb_files initialized with number 1, it will hold Cramer's V stat
                       
loop_number2=0;

for j in range(1,nb_files+1):
    loop_number2 = loop_number2 + 1
    filename1= listlines_f1[j-1]
    filename2= listlines_f2[j-1]
##    if j<>nb_files:
##        idrisi_filename1= filename1.rstrip('\n') #Remove the LF character from the string
##        idrisi_filename2= filename2.rstrip('\n') #Remove the LF character from the string
   
    idrisi_filename1= filename1.rstrip('\n') #Remove the LF character from the string
    idrisi_filename2= filename2.rstrip('\n') #Remove the LF character from the string
    inputf1=idrisi_filename1
    inputf3=idrisi_filename2

    #CREATING PSEUDO IMAGE FOR THE REFERENCE IMAGE
  
    #RUN ASSIGN
    #parameters = wd1+inputf3+'.rst'+'*'+wd1+'ps_'+inputf3+'reference_file_f'+str(j)+'_mode.rst'+'*'+wd1+'ps_'+inputf5+'_f'+str(j)+'_mode.avl'
    #parameters = wd1+inputf3+'.rst'+'*'+wd1+'ps_'+inputf3+'reference_file_f'+str(j)+'_mode.rst'+'*'+wd1+'ps_'+inputf5+'_mode.avl'
    parameters = wd1+inputf3+'.rst'+'*'+wd1+'ps_'+inputf3+'reference_file_f'+str(j)+'_mode.rst'+'*'+wd1+inputf6
    module = 'assign'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
        print('Error running ' + module + '!')

    #RUN CONVERT
    parameters = '1*'+wd1+'ps_'+inputf3+'reference_file_f'+str(j)+'_mode.rst'+'*'+wd1+'ps_'+inputf3+'reference_file_f'+str(j)+'_mode_byte.rst'+'*1*2*2'
    module = 'convert'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
        print('Error running ' + module + '!')
        
    i=0;
    distance=dist_min; # reinitialize the distance before running it on a the next file
    position=0;
    for i in range(1,stop+2,1):
        
        #RUN MODULE CHAINCLUSTER
        
        parameters = wd1+inputf1+'*'+wd1+'ps_'+output_prefix+'_f'+str(j)+'_'+str(i)+'.rst'+'*'+mask_image+'*'+str(distance)+'*'+'n'+'*'+str(max_cluster)+'*'+str(sequence)
        module = 'chaincluster'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')

        #RUN MODULE EXTRACT
        parameters = wd1+inputf3+'.rst'+'*'+wd1+'ps_'+output_prefix+'_f'+str(j)+'_'+str(i)+'.rst'+'*'+'1*1*'+wd1+'ps_'+output_prefix+'_f'+str(j)+'_'+str(i)+'.avl'
        module = 'extract'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')
       
        #RUN ASSIGN
        parameters = wd1+inputf2+'.rst'+'*'+wd1+output_prefix+'_f'+str(j)+'_'+str(i)+'.rst'+'*'+wd1+'ps_'+output_prefix+'_f'+str(j)+'_'+str(i)+'.avl'   
        module = 'assign'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')

        stat =api.CalculateRasterMinMax(wd1+'ps_'+output_prefix+'_f'+str(j)+'_'+str(i)+'.rst','1','2')
        tmp= stat[2]
        #max[i-1,j-1]=tmp #this array contains the number of cluster in each image
        max[i-1,j]=tmp #this array contains the number of cluster in each image

        #RUN CROSSTAB
        parameters = '1*'+wd1+'ps_'+inputf3+'reference_file_f'+str(j)+'_mode_byte.rst'+'*'+wd1+'ps_'+output_prefix+'_f'+str(j)+'_'+str(i)+'.rst'+'*NONE*NONE*11*'+wd1+'test.rst'
        module = 'crosstab'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')        
##
##        txt_output = 'CramersV_Output_TXT.id$'
##        idrisi_filename1= txt_output.rstrip('') #Remove the LF character from the string
##        idrisi_filename2= filename2.rstrip('\n') #Remove the LF character from the string

        #RECORDING CRAMER'S V FOR EVERY RUN IN TABLE
            
        os.rename(wd1+'CramersV_Output_TXT.id$',wd1+'ps_'+inputf3+'reference_file_f'+output_prefix+'_f'+str(j)+'_'+str(i)+'_CROSSTAB.txt')

        f3 = open( wd1+'ps_'+inputf3+'reference_file_f'+output_prefix+'_f'+str(j)+'_'+str(i)+'_CROSSTAB.txt','r')
        listlines_f3 = f3.readlines()  #Reading all the lines in the file
        f3.close()
        tmp=listlines_f3[1].split(',')
        line = tmp[0].split(' ')
        position=len(line)
        #crosstab[i-1,j-1]=float(line[position-1])
        crosstab[i-1,j]=float(line[position-1])
        
        distance=distance+step
        #end of loop for chain cluster
        
    #Write the results in a textfile
##    outfile1=wd1+output_prefix+'_f'+str(j)+'_cluster_max.txt'
##    numpy.savetxt(outfile1, max, fmt='%-7.2f')
    
    #end of loop for the list of file

#Write the results in a textfile
distance=dist_min
for i in range(1,stop+2,1):
    distance=distance+step
    crosstab[i-1,0]=distance
    max[i-1,0]=distance
    
outfile1=wd1+output_prefix+'_'+str(nb_files)+'_cluster_max.txt'
numpy.savetxt(outfile1, max, fmt='%-7.2f')
    
outfile2=wd1+'CramerV_crosstab_'+str(nb_files)+'.txt'
numpy.savetxt(outfile2, crosstab, fmt='%-7.5f')

#end of script

##    #RUN CROSSTAB
##    parameters = '1*'+wd1+'ps_'+inputf3+'reference_file_f'+str(j)+'_mode_byte.rst'+'*'+wd1+'ps_'+output_prefix+'_f'+str(j)+'_'+str(i)+'.rst'+'*NONE*NONE*11*'+wd1+'test.rst'
##    module = 'crosstab'
##    print('Running ' + module + ' module with parameters ' + parameters)
##    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
##    if not success:
##        print('Error running ' + module + '!')


    
   

