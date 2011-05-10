
#This script reclasses and overlays a series of boolean files from class signatures or polygons feature image.
#The boolean file must have an number identifier included in the name of the file.
#The reclassification utilizes the idenfifier and every layer is overlaid to create a unique file.

import os, glob, sys

import win32com.client

#variables
inputf1='unburnt_selection_' #input image containing the fire scars, note that this name does not correspond to an existing file
api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
wd = api.GetWorkingDir();                                      #Is variable containing the path of the working directory

#listfiles2_wd = glob.glob(wd+'unburnt_selection_'+'*'+'.rst') #list the files of interest
listfiles2_wd = glob.glob(wd+inputf1+'*'+'.rst') #list the files of interest
nb_files =len(listfiles2_wd)
loop_number2=0

for filename2 in listfiles2_wd:        #tThis is a loop through a list in which there is no update. To make update in a list use a copy 
    loop_number2 = loop_number2 + 1
    
    # Extract the IDRISI filename from the full path
    idrisi_filename2 = filename2[filename2.rfind('\\')+1:-4]        #This select the name within the string excluding the extension
    id_polygon_current2 = idrisi_filename2[idrisi_filename2.rfind('_')+1:]
    
    #RECLASS  I*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_allocating_bool_Class_83overlay_random.rst*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\selection_83.rst*3*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\idrtemp.rcl*1       
    parameters = 'I'+'*'+wd+inputf1+id_polygon_current2+'.rst'+'*'+wd+'pixels_unburnt'+'reclassed_'+id_polygon_current2+'.rst'+'*2'+'*0*0*1*'+id_polygon_current2+'*1*2*-9999*1'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
    module = 'reclass'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
        print('Error running ' + module + '!')

listfiles3_wd = glob.glob(wd+'pixels_unburnt'+'reclassed'+'*'+'.rst') #list the files of interest
nb_files =len(listfiles3_wd)
#Create an empty image for overlay

parameters = wd+'overlay_0'+'.rst'+'*1*1*0*1*'+listfiles3_wd[0]               #This creates an image file with value zero using the information from the first file in the list
module = 'initial'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
    print('Error running ' + module + '!')

api.DisplayFile(wd+'overlay_0'+'.rst', 'qual') #This displays the file just created by the module INITIAL: 'overlay_0.rst

#This loops will create the merged file

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

os.rename(wd+'overlay_'+str(loop_number3)+'.rst',wd+'ID_pixels_selection_unburnt_all_polygons.rst')
os.rename(wd+'overlay_'+str(loop_number3)+'.rdc',wd+'ID_pixels_selection_unburnt_all_polygons.rdc')

#end of script        

         