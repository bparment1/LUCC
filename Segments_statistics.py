#This script calculate statistic for segments or polygons in an image. It also creates pseudo images based on segments ID and input bands.
#The script is divided in two parts: the first part creates the rgf. series for as input for TSTATS while the second part runs the
#TSTATS module.

import os, glob, sys
from osgeo import gdal, gdalconst
import win32com.client
import shutil

#Arguments parameters for the script: variables, input parameters

inputf1= 'Amplitudes_2001_2009_filling6__9_STA_Slopes.rgf' #bands information
outfilename= 'Albedo_16D_masked2_fill4_'
wd1='H:\Benoit_Backup\Paper3_STA_07202011\\' #folder location of the groupfile and files
inputf3='seg10_NDVI_LST_ALB_A0_A1_A2_TS_slope_scale_0_450__30' #This is the segment level used or the feature definition image used

#inputf3= 'ID_burnt_unburnt_05232011'
dst_frmts    = 'rst' #data format driver for gdal
nb_f = 0             # this is the number of features in the feature image (excluding background)
stat_n2 = '8' #SD summary
stat_n1 = '4' #AVG summary
stst_n3 = '9' #all statistics

#START OF THE SCRIPT
api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
wd = api.GetWorkingDir();                                      #Is variable containing the path of the working directory
wd = 'H:\Benoit_Backup\Paper3_STA_07202011\\'

#H:\Benoit_Backup\Paper3_STA_07202011\seg10_NDVI_LST_ALB_A0_A1_A2_TS_slope_scale_0_450__30.rst*H:\Benoit_Backup\Paper3_STA_07202011\seg10_NDVI_LST_ALB_A0_A1_A2_TS_slope_scale_0_450__30.rst*1*1*G:\Benoit_Backup\STA_PAPER2_02262011\seg10_NDVI_LST_ALB_A0_A1_A2_TS_slope_scale_0_450__30_ID.avl

def file_len(fname):
    f=open(fname)
    for i, l in enumerate(f):
            pass
    return i + 1

#RUN MODULE EXTRACT
parameters = wd1+inputf3+'.rst'+'*'+wd1+inputf3+'.rst'+'*'+'1*1*'+wd1+inputf3+'_ID'+'.avl'
module = 'extract'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
    print('Error running ' + module + '!')

nb_f = file_len(wd1+inputf3+'_ID'+'.avl')

#RUN MODULE INITIAL
parameters = wd1+'ps_'+inputf3+'_initial.rst'+'*3*1*0*2*'+str(nb_f)+'*1*plane*m*1*'+str(nb_f)+'*1*'+str(nb_f)+'*1.0*undefined'   
module = 'initial'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
    print('Error running ' + module + '!')

#RUN THE RANK MODULE USING DESCENDING
parameters = wd1+'ps_'+inputf3+'_initial.rst'+'*none*'+wd1+'ps_'+inputf3+'_initial_PIXID.rst'+'*D'   
module = 'RANK'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
    print('Error running ' + module + '!')

#RUN MODULE EXTRACT
parameters = wd1+'ps_'+inputf3+'_initial_PIXID.rst'+'*'+wd1+'ps_'+inputf3+'_initial_PIXID.rst'+'*'+'1*1*'+wd1+'ps_'+inputf3+'_initial_PIXID'+'.avl'
module = 'extract'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
    print('Error running ' + module + '!')

f1 = open( wd1+'ps_'+inputf3+'_initial_PIXID'+'.avl','r')
f2 = open( wd1+inputf3+'_ID'+'.avl','r')

listlines_f1 = f1.readlines()  #Reading all the lines in the file
listlines_f2 = f2.readlines()  #Reading all the lines in the file
#listlines = list()            #This declares an empty list
listlines = listlines_f1 
loop_number2=0
           
i=0;
for i in range(1,nb_f+1,1):
        valf1 = listlines_f1[i-1].rsplit(' ')
        valf2 = listlines_f2[i-1].rsplit(' ')
        #listlines[i-1] = listlines.append(valf1[0]+' '+valf2[1])
        listlines[i-1] = valf1[0]+' '+valf2[1]

f3 = open( wd1+'ps_'+inputf3+'_PIXID'+'.avl','w')
for line in listlines:
    f3.write(line) 
f3.close()

nb_f3 = file_len(wd1+'ps_'+inputf3+'_PIXID'+'.avl')
if nb_f3!=nb_f:
    f3 = open( wd1+'ps_'+inputf3+'_PIXID'+'.avl','w')
    for line in listlines:
        f3.write(line)
f3.close()

shutil.copy(wd1+'ps_'+inputf3+'_initial_PIXID.adc',wd1+'ps_'+inputf3+'_PIXID'+'.adc')

f1.close()
f2.close()

#RUN ASSIGN
parameters = wd1+'ps_'+inputf3+'_initial_PIXID.rst'+'*'+wd1+'ps_'+inputf3+'_PIXID'+'.avl'+'*'+wd1+'ps_'+inputf3+'_PIXID'+'.rst'   
module = 'assign'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
    print('Error running ' + module + '!')

#___________#THIS CREATED THE PSEUDO_IMAGE FOR LEVEL_30__________________

f1 = open(wd1+inputf1,'r') 
listfiles1_wd = f1.readlines()  #Reading all the lines in the file
nb_files = int(listfiles1_wd[0])#Number of fitles in the group
loop_number1=0

for i in range(1,nb_files+1,1):
        loop_number1 = loop_number1 + 1
        filename1= listfiles1_wd[i] 
        idrisi_filename1= filename1.rstrip('\n')

        parameters = wd1+inputf3+'.rst'+'*'+wd1+idrisi_filename1+'.rst'+'*'+'1*'+stat_n2+'*'+wd1+inputf3+'_ID'+'_b_'+str(loop_number1)+'.avl'
        module = 'extract'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')
            
        #RUN ASSIGN
        parameters = wd1+'ps_'+inputf3+'_PIXID'+'.rst'+'*'+wd1+'ps_'+inputf3+'_ID'+'_b_'+str(loop_number1)+'.rst'+'*'+wd1+inputf3+'_ID'+'_b_'+str(loop_number1)+'.avl'  
        module = 'assign'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
            print('Error running ' + module + '!')

            
listfiles1_wd1 = glob.glob(wd1+'ps_seg10_NDVI_LST_ALB_A0_A1_A2_TS_slope_scale_0_450__30_ID_b_*.rst')
f4 = open( wd1+'ps_'+inputf3+str(nb_files)+'.rgf','w')
line0 = str(len(listfiles1_wd1))+'\n'
f4.write(line0)
for line in listfiles1_wd1:
    line = line[line.rfind('\\')+1:]
    #line = line[line.rfind('\\')+1:-4]
    line=line.replace('.rst','\n')
    f4.write(line) 
f4.close()

rgf_name =wd1+'ps_'+inputf3+str(nb_files)+'.rgf'
outfilename =wd1+'ps_'+inputf3+str(nb_files)

min_range =-1000
max_range =1000000000000000
background = -1000
min_nb_obs =1

#TSTAT     
parameters ='1'+'*'+rgf_name+'*'+outfilename+'*'+str(min_range)+'*'+str(max_range)+'*'+str(background)+'*'+str(min_nb_obs)
module = 'TStats'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
       print('Error running ' + module + '!')
               
### ==================================== #
### Load GDAL Drivers:
##dst_frmts    = 'rst' #data format driver for gdal
##dst_driver = gdal.GetDriverByName( dst_frmts )
##
##if dst_driver == None:
##    Usage()
##
###src_ds = gdal.Open( inputf1 )
##src_ds = gdal.Open( 'G:\\Benoit_Backup\\STA_PAPER2_02262011\\test5_ID.rst' )
##src_ds = gdal.Open('G:\Benoit_Backup\STA_PAPER2_02262011\GAP_ALB_07182011_filled_t3\\GAP_ALB_07182011_filled_t3_GAP_ALBEDO_16D_MASKED2_FILL4_6_MEAN.rst')
##src_rb = src_ds.GetRasterBand( 1 );
##
##x_size = src_rb.XSize  #nb_cols
##y_size = src_rb.YSize  #nb_rows
##
##src_rb.ReadAsArray(0,6,None,None,24,None)


