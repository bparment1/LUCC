#This script runs MODISQC for Albedo band quality and renames files containing the albedo data.
#The script processes the series band per band and produces an aggregate series with a 8Day temporal resolution.

import os, glob, sys

import win32com.client

#variables, input parameters

inputf1='albedo_data_414.rgf'
inputf2='Albedo_band_quality_414.rgf'     #idrisi groupfile containing the albedo band quality
inputf3='Albedo_BRDF_quality_414.rgf'     #idrisi groupfile containing the albedo BRDF ALBEDO quality

wd2='G:\Benoit_Backup\STA_PAPER2_02262011\W_OUTPUT_11_tiles_MCD43B2_BRDF_Albedo_Band_Quality\\'
wd3='G:\Benoit_Backup\STA_PAPER2_02262011\W_Alaska_2001_2009_MCD43B2_Reprojected_BRDF_Albedo_Quality\\'
wd1='G:\Benoit_Backup\STA_PAPER2_02262011\w_Albedo_2001_2009_BSA_SW\\'

band_nb = 7;                                                 #number of band used in the formation of the albedo product
t1 = 3  #minimum threshold to consider a pixel as good quality in the BRDF band quality
t2 = 3  #minimum threshold to consider a pixel as good quality based on the sum of the overlays

#START OF THE SCRIPT

api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
wd = api.GetWorkingDir();
#wd = 'G:\Benoit_Backup\Filling\ALBEDO_MCD43B2\W_OUTPUT_11_tiles_MCD43B2_BRDF_Albedo_Band_Quality\\' #location of the input files in the group file

f2 = open(wd2+inputf2,'r') 
listfiles2_wd = f2.readlines()  #Reading all the lines in the file
nb_files = int(listfiles2_wd[0])#Number of files in the group
loop_number2=0

for i in range(1,nb_files+1):
        loop_number2 = loop_number2 + 1
        filename2= listfiles2_wd[i]
        idrisi_filename2= filename2.rstrip('\n') #Remove the LF character from the string
        for b in range(1,band_nb+1):
                #MODISQC     
                parameters ='3'+'*'+wd2+idrisi_filename2+'.rst'+'*'+wd+idrisi_filename2+'_qc_'+str(b)+'.rst'+'*'+str(b)  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
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

###PART 1 IS OK###
                    
###PART2##########

f1 = open(wd1+inputf1,'r') 
listfiles1_wd = f1.readlines()  #Reading all the lines in the file
nb_files1 = int(listfiles1_wd[0])#Number of files in the group
loop_number1=0

filename1= listfiles1_wd[1]
idrisi_filename1= filename1.rstrip('\n')

f2 = open(wd2+inputf2,'r') 
listfiles2_wd = f2.readlines()  #Reading all the lines in the file
nb_files = int(listfiles2_wd[0])#Number of files in the group
loop_number2=0

filename2= listfiles2_wd[1]
idrisi_filename2= filename2.rstrip('\n')

#RUN MODULE INITIAL BEFORE THE OVERLAY LOOP
parameters = wd+'overlay_0'+'.rst'+'*1*1*0*1*'+wd1+idrisi_filename1+'.rst'              #This creates an image file with value zero using the information from the first file in the list
module = 'initial'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
    print('Error running ' + module + '!')

#api.DisplayFile(wd+'overlay_0'+'.rst', 'qual') #This displays the file just created by the module INITIAL: 'overlay_0.rst

loop_number3 = 0


for i in range(1,nb_files+1):
        loop_number2 = loop_number2 + 1
        filename2= listfiles2_wd[i]
        idrisi_filename2= filename2.rstrip('\n') #Remove the LF character from the string


        
        
        #MAKE SURE ALL THE FILES ARE CLEAR!!
        listfiles3_wd = glob.glob(wd+idrisi_filename2+'_qc_'+'*'+'_rec_'+str(t1)+'.rst') #list the files of interest
        nb_foverlay =len(listfiles3_wd)
        loop_number3=0

        parameters = wd+'overlay_0'+'.rst'+'*1*1*0*1*'+wd1+idrisi_filename1+'.rst'              #This creates an image file with value zero using the information from the first file in the list
        module = 'initial'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
           print('Error running ' + module + '!')
           
        for filename3 in listfiles3_wd:        #tThis is a loop through a list in which there is no update. To make update in a list use a copy 
            loop_number3 = loop_number3 + 1
                  
            ###OVERLAY MODULE###    
            parameters = '1*'+filename3+'*'+wd+'overlay_'+str(loop_number3-1)+'.rst'+'*'+wd+'overlay_'+str(loop_number3)+'.rst'
            module = 'overlay'
            print('Running ' + module + ' module with parameters ' + parameters)
            success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
            if not success:
               print('Error running ' + module + '!')
            #os.remove(wd+'overlay_'+str(loop_number3)+'.rst')
            os.remove(wd+'overlay_'+str(loop_number3-1)+'.rst')
            #os.remove(wd+'overlay_'+str(loop_number3-1)+'.rdc')
            os.remove(wd+'overlay_'+str(loop_number3-1)+'.rdc')
            #os.remove(wd+'overlay_'+str(loop_number3-1)+'.r'+'*')

        listfile_wd = glob.glob(wd+idrisi_filename2+'_qc_'+'overlay_'+str(t1)+'.r**') #list the files of interest
        if  len(listfile_wd)>0:
                os.remove(wd+idrisi_filename2+'_qc_'+'overlay_'+str(t1)+'.rdc') #list the files of interest
                os.remove(wd+idrisi_filename2+'_qc_'+'overlay_'+str(t1)+'.rst') #list the files of interest
        os.rename(wd+'overlay_'+str(loop_number3)+'.rst',wd+idrisi_filename2+'_qc_'+'overlay_'+str(t1)+'.rst')
        os.rename(wd+'overlay_'+str(loop_number3)+'.rdc',wd+idrisi_filename2+'_qc_'+'overlay_'+str(t1)+'.rdc')


        #RECLASS  I*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_allocating_bool_Class_83overlay_random.rst*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\selection_83.rst*3*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\idrtemp.rcl*1       
        parameters = 'I'+'*'+wd+idrisi_filename2+'_qc_'+'overlay_'+str(t1)+'.rst'+'*'+wd+idrisi_filename2+'_qc_'+'masked_'+str(t1)+'_'+str(t2)+'.rst'+'*2'+'*0*0*'+str(t2)+'*1*'+str(t2)+'*'+str(band_nb+1)+'*-9999*1'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
        module = 'reclass'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')                

#For instance: W_ALASKA_2001_025_MCD43B2_REPROJECTED_BRDF_ALBEDO_BAND_QUALITY_qc_masked_3_4.rst contains all the pixels that are considered as
#good quality if the individual band have more than (or equal) 50% fill value and 4 bands out of 7 bands are of good quality

###PART3##########

###NEED TO DO GLOB
            
#listfiles2_wd = glob.glob(wd+idrisi_filename2+'_qc_'+'masked_'+str(t1)+'_'+str(t2)+'.rst') #list the files of interest
listfiles2_wd = glob.glob(wd+'*'+'_qc_'+'masked_'+str(t1)+'_'+str(t2)+'.rst') #list the files of interest

f3 = open(wd3+inputf3,'r') 
listfiles3_wd = f3.readlines()  #Reading all the lines in the file
nb_files3 = int(listfiles3_wd[0])#Number of files in the group
loop_number3=0

filename3= listfiles3_wd[1]
idrisi_filename3= filename3.rstrip('\n')

f1 = open(wd1+inputf1,'r') 
listfiles1_wd = f1.readlines()  #Reading all the lines in the file
nb_files1 = int(listfiles1_wd[0])#Number of files in the group
loop_number1=0

filename1= listfiles1_wd[1]
idrisi_filename1= filename1.rstrip('\n')
##if nb_files!=nb_files3:
##        exit()

loop_number3 = 0;

for i in range(1,nb_files+1):
        
        loop_number3 = loop_number3 + 1
        filename2= listfiles2_wd[i-1]
        idrisi_filename2= filename2[filename2.rfind('\\')+1:-4] #Remove the path

        filename3= listfiles3_wd[i]
        idrisi_filename3= filename3.rstrip('\n') #Remove the LF character from the string

        filename1= listfiles1_wd[i]
        idrisi_filename1= filename1.rstrip('\n') #Remove the LF character from the string

        #RECLASS  I*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_allocating_bool_Class_83overlay_random.rst*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\selection_83.rst*3*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\idrtemp.rcl*1       
        #NOTE THAT WE ARE USING THE INPUT FILE PATH!!!
        parameters = 'I'+'*'+wd3+idrisi_filename3+'.rst'+'*'+wd+idrisi_filename3+'_qc_'+'rec'+'.rst'+'*2'+'*1*0*'+'1'+'*0*'+'1'+'*'+'256'+'*-9999*1'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
        module = 'reclass'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')    

        #overlay with option 7
        ###OVERLAY MODULE###    
        parameters = '7*'+wd+idrisi_filename3+'_qc_'+'rec'+'.rst'+'*'+wd+idrisi_filename2+'.rst'+'*'+wd+'overlay_mask_'+str(loop_number3)+'.rst'
        module = 'overlay'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')

        #RECLASS  I*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_allocating_bool_Class_83overlay_random.rst*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\selection_83.rst*3*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\idrtemp.rcl*1       
        parameters = 'I'+'*'+wd+'overlay_mask_'+str(loop_number3)+'.rst'+'*'+wd+'overlay_mask_reversed_'+str(loop_number3)+'.rst'+'*2'+'*1*0*'+'1'+'*0*'+'1'+'*'+'2'+'*-9999*1'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
        module = 'reclass'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')      
        
        #overlay with option 1
        ###OVERLAY MODULE###
        #NOTE THAT WE ARE USING THE INPUT FILE PATH!!!
        parameters = '1*'+wd1+idrisi_filename1+'.rst'+'*'+wd+'overlay_mask_reversed_'+str(loop_number3)+'.rst+'+'*'+wd+idrisi_filename1+'_masked_'+str(t1)+'_'+str(t2)+'.rst'
        module = 'overlay'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')        

