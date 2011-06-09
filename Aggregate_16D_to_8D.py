#This script aggreage MODIS data from 16Days to 8Days using the mean with valid range and background information taken into account.
#The script is divided in two parts: the first part creates the rgf. series for as input for TSTATS while the second part runs the
#TSTATS module.

import os, glob, sys

import win32com.client

import pickle

#variables, input parameters

#inputf1='Albedo_band_data_masked_6b.rgf' #idrisi groupfile containing the albedo band product to be converted into a 16D product
#inputf1= 'albedo_band_data_masked_414_filling2_rec_.rgf'
inputf1= 'fill4_.rgf'
outfilename= 'Albedo_16D_masked2_fill4_'
wd1='G:\Benoit_Backup\STA_PAPER2_02262011\\fill4_\\' #folder location of the groupfile and files

                 #number of band used in the formation of the albedo product
min_range = 0  #minimum threshold to consider a pixel as good quality in the BRDF band quality
max_range = 1  #minimum threshold to consider a pixel as good quality based on the sum of the overlays
background = 2
min_nb_obs = 1

#START OF THE SCRIPT

api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
wd = api.GetWorkingDir();
#wd = 'G:\Benoit_Backup\Filling\ALBEDO_MCD43B2\W_OUTPUT_11_tiles_MCD43B2_BRDF_Albedo_Band_Quality\\' #location of the input files in the group file

f1 = open(wd1+inputf1,'r') 
listfiles1_wd = f1.readlines()  #Reading all the lines in the file
nb_files = int(listfiles1_wd[0])#Number of files in the group
loop_number2=0
##
##for i in range(2,nb_files+1,2):
##        loop_number2 = loop_number2 + 1
##        filename1= listfiles1_wd[i]              #Contains the second file name and path from the rgf
##        filename0= listfiles1_wd[i-1]            #Contains the first file name and path from the rgf
##        idrisi_filename1= filename1.rstrip('\n') #Remove the LF character from the string
##        idrisi_filename0= filename0.rstrip('\n')
##        line='2\n'
##        rgf_name = 'rgf_'+str(i)+'.rgf'
##        rgf = open(wd1+rgf_name,'w')
##        rgf.write(line)
####        rgf.write(filename0)
####        rgf.write(filename1)
##        rgf.write(idrisi_filename0+'\n')
##        rgf.write(idrisi_filename1+'\n')
##        rgf.close
#f1.close                

##TStats  1*G:\Benoit_Backup\STA_PAPER2_02262011\tmpidr_division_2_year2001.rgf*G:\Benoit_Backup\STA_PAPER2_02262011\test5_4*-10000*10000*10000*2
##DISPLAY image file test5_4_Mean with symbol file quant.  Autoscale, Title, Legend.
##test5_4.rst (First input image) not found.
##Output file G:\Benoit_Backup\STA_PAPER2_02262011\test5.rst already exists. Overwrite?

###PART 1 IS NOT OK THE LAST FILE IS NOT WRITTEN...###

f1 = open(wd1+inputf1,'r') 
listfiles1_wd = f1.readlines()  #Reading all the lines in the file
nb_files = int(listfiles1_wd[0])#Number of fitles in the group
loop_number1=0

for i in range(2,nb_files+1,2):
        loop_number1 = loop_number1 + 1
        rgf_name = 'rgf_'+str(i)+'.rgf'
        #TSTAT     
        parameters ='1'+'*'+wd1+rgf_name+'*'+wd+outfilename+str(loop_number1)+'*'+str(min_range)+'*'+str(max_range)+'*'+str(background)+'*'+str(min_nb_obs)

        module = 'TStats'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
               print('Error running ' + module + '!')
       