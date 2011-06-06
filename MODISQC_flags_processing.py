#This script runs MODISQC for Albedo band quality and renames files containing the albedo data.
#The script processes the series band per band and produces an aggregate series with a 16Day temporal resolution.

import os, glob, sys

import win32com.client

#variables, input parameters
#inputf1='unburnt_selection_' #list of input image containing the STA parameters
inputf1='albedo_band_quality_1.rgf'
#inputf1='Amp_3_slopes_NDVI_p'
api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
wd = api.GetWorkingDir();
#wd = 'G:\Benoit_Backup\Filling\ALBEDO_MCD43B2\W_OUTPUT_11_tiles_MCD43B2_BRDF_Albedo_Band_Quality\\'
band_nb = 7;

f2 = open(wd+inputf1,'r') 
listfiles2_wd = f2.readlines()  #Reading all the lines in the file
nb_files = int(listfiles2_wd[0])#Number of files in the group
loop_number2=0

for i in range(1,nb_files+1):
        loop_number2 = loop_number2 + 1
        filename2= listfiles2_wd[i]
        idrisi_filename2= filename2.rstrip('\n') #Remove the LF character from the string
        for b in range(1,band_nb+1):
                #MODISQC     
                parameters ='3'+'*'+wd+idrisi_filename2+'.rst'+'*'+wd+idrisi_filename2+'_qc_'+str(b)+'.rst'+'*'+str(b)  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
                #parameters = '2*G:\Benoit_Backup\STA_PAPER2_02262011\W_ALASKA_2001_025_MCD43B2_REPROJECTED_BRDF_ALBEDO_BAND_QUALITY.rst*G:\Benoit_Backup\STA_PAPER2_02262011\Albedo_quality_band_qc\W_ALASKA_2001_025_BRDF_ALBEDO_BAND_QUALITY_QC_5.rst*1'

                module = 'modisqc'
                print('Running ' + module + ' module with parameters ' + parameters)
                success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
                if not success:
                       print('Error running ' + module + '!')

       