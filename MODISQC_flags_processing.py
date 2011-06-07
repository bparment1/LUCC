#This script runs MODISQC for Albedo band quality and renames files containing the albedo data.
#The script processes the series band per band and produces an aggregate series with a 16Day temporal resolution.

import os, glob, sys

import win32com.client

#variables, input parameters

inputf1='albedo_band_quality_1.rgf'     #idrisi groupfile containing the albedo band quality 
api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
wd = api.GetWorkingDir();
#wd = 'G:\Benoit_Backup\Filling\ALBEDO_MCD43B2\W_OUTPUT_11_tiles_MCD43B2_BRDF_Albedo_Band_Quality\\' #location of the input files in the group file
band_nb = 7;                                                 #number of band used in the formation of the albedo product
t1 = 3  #minimum threshold to consider a pixel as good quality in the BRDF band quality
t2 = 4  #minimum threshold to consider a pixel as good quality based on the sum of the overlays

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

                #RECLASS  I*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_allocating_bool_Class_83overlay_random.rst*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\selection_83.rst*3*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\idrtemp.rcl*1       
                parameters = 'I'+'*'+wd+idrisi_filename2+'_qc_'+str(b)+'.rst'+'*'+wd+idrisi_filename2+'_qc_'+str(b)+'_rec_'+str(t1)+'.rst'+'*2'+'*0*0*'+str(t1)+'*1*'+str(t1)+'*5'+'*-9999*1'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
                module = 'reclass'
                print('Running ' + module + ' module with parameters ' + parameters)
                success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
                if not success:
                    print('Error running ' + module + '!')


f2 = open(wd+inputf1,'r') 
listfiles2_wd = f2.readlines()  #Reading all the lines in the file
nb_files = int(listfiles2_wd[0])#Number of files in the group
loop_number2=0

filename2= listfiles2_wd[1]
idrisi_filename2= filename2.rstrip('\n')

#RUN MODULE INITIAL BEFORE THE OVERLAY LOOP
parameters = wd+'overlay_0'+'.rst'+'*1*1*0*1*'+wd+idrisi_filename2+'.rst'              #This creates an image file with value zero using the information from the first file in the list
module = 'initial'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
    print('Error running ' + module + '!')

api.DisplayFile(wd+'overlay_0'+'.rst', 'qual') #This displays the file just created by the module INITIAL: 'overlay_0.rst

loop_number3 = 0

for i in range(1,nb_files+1):
        loop_number2 = loop_number2 + 1
        filename2= listfiles2_wd[i]
        idrisi_filename2= filename2.rstrip('\n') #Remove the LF character from the string
        
        #listfiles2_wd = glob.glob(wd+'unburnt_selection_'+'*'+'.rst') #list the files of interest
        listfiles3_wd = glob.glob(wd+idrisi_filename2+'_qc_'+'*'+'_rec_'+str(t1)+'.rst') #list the files of interest
        nb_foverlay =len(listfiles3_wd)
        loop_number3=0
        for filename3 in listfiles3_wd:        #tThis is a loop through a list in which there is no update. To make update in a list use a copy 
            loop_number3 = loop_number3 + 1
                  
            ###OVERLAY MODULE###    
            parameters = '1*'+filename3+'*'+wd+'overlay_'+str(loop_number3-1)+'.rst'+'*'+wd+'overlay_'+str(loop_number3)+'.rst'
            module = 'overlay'
            print('Running ' + module + ' module with parameters ' + parameters)
            success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
            if not success:
               print('Error running ' + module + '!')
            os.remove(wd+'overlay_'+str(loop_number3-1)+'.rst')
            os.remove(wd+'overlay_'+str(loop_number3-1)+'.rdc')
            #os.remove(wd+'overlay_'+str(loop_number3-1)+'.r'+'*')

        os.rename(wd+'overlay_'+str(loop_number3)+'.rst',wd+idrisi_filename2+'_qc_'+'overlay_'+str(t1)+'.rst')
        os.rename(wd+'overlay_'+str(loop_number3)+'.rdc',wd+idrisi_filename2+'_qc_'+'overlay_'+str(t1)+'.rdc')


        #RECLASS  I*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_allocating_bool_Class_83overlay_random.rst*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\selection_83.rst*3*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\idrtemp.rcl*1       
        parameters = 'I'+'*'+wd+idrisi_filename2+'_qc_'+'overlay_'+str(t1)+'.rst'+'*'+wd+idrisi_filename2+'_qc_'+'masked_'+str(t1)+'_'+str(t2)+'.rst'+'*2'+'*0*0*'+str(t2)+'*1*'+str(t2)+'*'+str(band_nb+1)+'*-9999*1'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
        module = 'reclass'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')                
       