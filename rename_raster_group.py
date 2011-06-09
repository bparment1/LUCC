#This script runs MODISQC for Albedo band quality and renames files containing the albedo data.
#The script processes the series band per band and produces an aggregate series with a 8Day temporal resolution.

import os, glob, sys

import win32com.client

#variables, input parameters

inputf1='albedo_band_data_masked2_4_filling3_test.rgf'             #idrisi groupfile containing the list of files to be renamed
inputf2='Albedo_band_quality_414.rgf'     #idrisi groupfile containing the list of names 


#wd2='G:\Benoit_Backup\STA_PAPER2_02262011\W_OUTPUT_11_tiles_MCD43B2_BRDF_Albedo_Band_Quality\\'
wd1='G:\Benoit_Backup\STA_PAPER2_02262011\\aggregation\\'

nb_char = 50; #number of character to be croppedd in the formation of the albedo product
prefix = 'fill4_'  #minimum threshold to consider a pixel as good quality in the BRDF band quality

#START OF THE SCRIPT

api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
wd = api.GetWorkingDir();
#wd = 'G:\Benoit_Backup\Filling\ALBEDO_MCD43B2\W_OUTPUT_11_tiles_MCD43B2_BRDF_Albedo_Band_Quality\\' #location of the input files in the group file

f1 = open(wd1+inputf1,'r') 
listfiles1_wd = f1.readlines()  #Reading all the lines in the file
nb_files1 = int(listfiles1_wd[0])#Number of files in the group
nb_files = nb_files1
loop_number1=0

for i in range(1,nb_files+1):
        loop_number1 = loop_number1 + 1
        filename1= listfiles1_wd[i]
        idrisi_filename1= filename1.rstrip('\n') #Remove the LF character from the string
        crop_char = len(idrisi_filename1) - nb_char
        idrisi_filename2= prefix+idrisi_filename1[crop_char:len(idrisi_filename1)]   

        os.rename(wd1+idrisi_filename1+'.rst',wd1+idrisi_filename2+'.rst')
        os.rename(wd1+idrisi_filename1+'.rdc',wd1+idrisi_filename2+'.rdc')
        
##for i in range(1,nb_files+1):
##        loop_number1 = loop_number1 + 1
##        filename1= listfiles1_wd[i]
##        idrisi_filename1= filename1.rstrip('\n') #Remove the LF character from the string
##        filename2= listfiles2_wd[i]
##        idrisi_filename2= filename1.rstrip('\n') #Remove the LF character from the string
##
##        os.rename(wd+idrisi_filename1+'.rst',wd+idrisi_filename2+'.rst')
##        os.rename(wd+idrisi_filename1+'.rdc',wd+idrisi_filename2+'.rdc')
        