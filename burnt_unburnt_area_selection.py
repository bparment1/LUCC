#Learning to use python and IDRISI
#This script displays all the rst files in the working directory folder in IDRISI.

import os, glob, sys

import win32com.client

#variables
inputf='fire_2001_2007_POL_ID_03092011' #input image containing the fire scars
#inputf='OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399'

api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
wd = api.GetWorkingDir();                                      #Is variable containing the path of the working directory
                     
                                                                         #Note that the list is in a unicode format hence the u' before the string

###Running the Distance module around fire polygons########
##Note that distance takes as input two parameters
#input file name (fire polygons)
#outputfile name (distance image)

parameters= wd+inputf+'.rst'+'*'+wd+inputf+'_distance'+'.rst'
module ='distance'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)
if not success:
   print('Error running ' + module + '!')


###Running the allocate module around fire polygons########
##Note that distance takes as input three parameters
    #intputfile name (distance image)
    #input file name (fire polygons)
    #outputfile name (distance image)
 
parameters = wd+inputf+'_distance'+'.rst'+'*'+wd+inputf+'.rst'+'*'+wd+inputf+'_allocating'+'.rst'
module = 'allocate'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)
if not success:
   print('Error running ' + module + '!')
#BUFFER  C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399.rst*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_buffer20.rst*0*1*0*20000  

parameters = wd+inputf+'.rst'+'*'+wd+inputf+'_buffer20'+'.rst'+'*0*1*0*20000'      
module = 'buffer'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
   print('Error running ' + module + '!')

##AREA  C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399.rst*2*1*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\area_burnt_polygons.avl

parameters = wd+inputf+'.rst'+'*'+'2*1*'+wd+'area_burnt_polygons.avl'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
module = 'area'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
   print('Error running ' + module + '!')


#BREAKOUT  1*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399.rst*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399*1

parameters = '1*'+wd+inputf+'_allocating'+'.rst'+'*'+wd+inputf+'_allocating'+'*1'
module = 'breakout'
##parameters = wd+inputf+'.rst'+'*'+'2*1*'+wd+'area_burnt_polygons.avl'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
##module = 'area'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
   print('Error running ' + module + '!')

##RANDOM  C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\random.rst*1*2*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399.rst*1   
##DISPLAY image file C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_bool_Class_399.rst with symbol file C:\Program Files\IDRISI Taiga\Symbols\qual.smp.  Title, Legend.

parameters = wd+'random'+'.rst'+'*'+'1*2*'+ wd+inputf+'.rst'+'*1'
module = 'random'
print('Running ' + module + ' module with parameters ' + parameters)
success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
if not success:
   print('Error running ' + module + '!')

api.DisplayFile(wd+'random'+'.rst', 'quant')    # Displaying the random map using the quantitative palette.


###########################################LOOP TO DEFINE THE UNBURNT PIXELS##############
#processed_poylgon 

listfiles_wd = glob.glob(wd+inputf+'_allocating'+'_bool_Class_'+'*'+'.rst')         #This creates a list with all the rst files in the working directory
loop_number = 0                                                                  #Note that the list is in a unicode format hence the u' before the string

for filename in listfiles_wd:        #tThis is a loop through a list in which there is no update. To make update in a list use a copy 
    loop_number = loop_number + 1
    
    # Extract the IDRISI filename from the full path
    idrisi_filename = filename[filename.rfind('\\')+1:-4]        #This select the name within the string excluding the extension
    id_polygon_current = idrisi_filename[idrisi_filename.rfind('_')+1:]      # Extract fire polygon number from filename by finding the last '_'
    
        
    ###OVERLAY MODULE###    
    parameters = '3*'+wd+inputf+'_buffer20'+'.rst'+'*'+wd+idrisi_filename+'.rst'+'*'+wd+'buffer20'+'_overlay_'+id_polygon_current+'.rst'
    module = 'overlay'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
       print('Error running ' + module + '!')

    ###OVERLAY MODULE###     
    parameters = '3*'+wd+'random'+'.rst'+'*'+wd+'buffer20'+'_overlay_'+id_polygon_current+'.rst'+'*'+wd+'buffer20'+'_overlay_'+id_polygon_current+'_overlay_random'+'.rst'
    module = 'overlay'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
       print('Error running ' + module + '!')
       
    ###AREA###
    parameters = wd+'buffer20'+'_overlay_'+id_polygon_current+'.rst'+'*'+'2*1*'+wd+'area_unburnt_selection.avl'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
    module = 'area'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
       print('Error running ' + module + '!')

    f1 = open(wd+'area_unburnt_selection.avl','r')
    linef1=f1.readline() #read the first line which pertains to category 0
    linef1=f1.readline()
    if linef1 != '':
        id, area_un_selection_str =linef1.rsplit(' ')

        f2 = open(wd+'area_burnt_polygons.avl','r')     
        linef2=f2.readline()               #reading the first line and placing it in the string variable line
        id_polygon, area_burnt_str =linef2.rsplit(' ')
    ##    if int(float(id_polygon))==0:
    ##       linef2=f2.readline()
    ##       id_polygon, area_burnt_str =linef2.rsplit(' ')
        while int(float(id_polygon)) != int(id_polygon_current):
            linef2=f2.readline()
            id_polygon, area_burnt_str =linef2.rsplit(' ')

        id_polygon, area_burnt_str =linef2.rsplit(' ')    
        proportion = float(area_burnt_str)/float(area_un_selection_str)
        if proportion > 1:
            proportion = 1.1

        #RECLASS  I*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_allocating_bool_Class_83overlay_random.rst*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\selection_83.rst*3*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\idrtemp.rcl*1       
        parameters = 'I'+'*'+wd+'buffer20'+'_overlay_'+id_polygon_current+'_overlay_random'+'.rst'+'*'+wd+'selection_'+id_polygon_current+'.rst'+'*2'+'*1*0*'+str(proportion)+'*0*'+str(proportion)+'*1.3*-9999*1'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
        module = 'reclass'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
           print('Error running ' + module + '!')    

        ###OVERLAY MODULE###     
        parameters = '3*'+wd+'selection_'+id_polygon_current+'.rst'+'*'+wd+'buffer20'+'_overlay_'+id_polygon_current+'.rst'+'*'+wd+'unburnt_selection_'+id_polygon_current+'.rst'
        module = 'overlay'
        print('Running ' + module + ' module with parameters ' + parameters)
        success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
        if not success:
           print('Error running ' + module + '!')

                
    else:
        print('It is not possible to find unburnt areas for polygon'+id_polygon_current)
##        file = open(wd+'results_code.txt', 'a')
##        line = 'It is not possible to find unburnt areas for polygon '+id_polygon_current
##        file.write(line)
##        file.close
    #print 'Loop number is '+loop_number
#print('Loop number ' + module + '!')
#run    
