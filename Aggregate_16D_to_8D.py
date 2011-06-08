#This script aggreage MODIS data from 16Days to 8Days using the mean with valid range and background information taken into account.
#The script is divided in two parts: the first part creates the rgf. series for as input for TSTATS while the second part runs the
#TSTATS module.

import os, glob, sys

import win32com.client

#variables, input parameters

inputf1='Albedo_band_data_masked_6.rgf' #idrisi groupfile containing the albedo band product to be converted into a 16D product
#inputf1= 'albedo_band_data_masked_414_filling2_rec_.rgf'

wd1='G:\Benoit_Backup\STA_PAPER2_02262011\\'

band_nb = 7;                                                 #number of band used in the formation of the albedo product
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

for i in range(2,nb_files+1):
        loop_number2 = loop_number2 + 1
        filename1= listfiles1_wd[i]              #Contains the second file name and path from the rgf
        filename0= listfiles1_wd[i-1]            #Contains the first file name and path from the rgf
        idrisi_filename1= filename1.rstrip('\n') #Remove the LF character from the string
        idrisi_filename0= filename0.rstrip('\n')
        rgf_name = 'rgf_'+i+'.rgf'
        rgf = open(rgf_name,'w')

                
                

###PART 1 IS OK###
                    