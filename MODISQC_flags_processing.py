#This script runs MODISQC for Albedo band quality and rename files containing the albedo data.

import os, glob, sys

import win32com.client

#variables, input parameters
#inputf1='unburnt_selection_' #list of input image containing the STA parameters
inputf1='albedo_band_quality_1.rgf'
#inputf1='Amp_3_slopes_NDVI_p'
api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
wd = api.GetWorkingDir();
#wd = 'G:\Benoit_Backup\Filling\ALBEDO_MCD43B2\W_OUTPUT_11_tiles_MCD43B2_BRDF_Albedo_Band_Quality\\'

f2 = open(wd+inputf1,'r') 
listfiles2_wd = f2.readlines()  #Reading all the lines in the file
nb_files = int(listfiles2_wd[0])#Number of files in the group
loop_number2=0

for i in range(1,nb_files+1):
        loop_number2 = loop_number2 + 1
        filename2= listfiles2_wd[i]
        idrisi_filename2= filename2.rstrip('\n') #Remove the LF character from the string
        #MODISQC     
        parameters = '3'+'*'+wd+idrisi_filename2+'.rst'+'*'+wd+'W_ALASKA_2001_025_BRDF_ALBEDO_BAND_QUALITY'+'_qc_1'+'.rst'+'*1'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
        module = 'modisqc'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
               print('Error running ' + module + '!')

       