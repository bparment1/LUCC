#This script runs windows the Alaska region based on used based input.
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

nb_y=4
nb_x=5

band_nb = 7;                                                 #number of band used in the formation of the albedo product
t1 = 3  #minimum threshold to consider a pixel as good quality in the BRDF band quality
t2 = 3  #minimum threshold to consider a pixel as good quality based on the sum of the overlays

#START OF THE SCRIPT

api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
wd = api.GetWorkingDir();
#wd = 'G:\Benoit_Backup\Filling\ALBEDO_MCD43B2\W_OUTPUT_11_tiles_MCD43B2_BRDF_Albedo_Band_Quality\\' #location of the input files in the group file

t = api.GetFromDocFile('G:\Benoit_Backup\STA_PAPER2_02262011\\AGG_16D_ALBEDO_1_Mean.rdc',9)
minx = float(t[0])
t = api.GetFromDocFile('G:\Benoit_Backup\STA_PAPER2_02262011\\AGG_16D_ALBEDO_1_Mean.rdc',10)
maxx = float(t[0])
t = api.GetFromDocFile('G:\Benoit_Backup\STA_PAPER2_02262011\\AGG_16D_ALBEDO_1_Mean.rdc',11)
miny=float(t[0])
t = api.GetFromDocFile('G:\Benoit_Backup\STA_PAPER2_02262011\\AGG_16D_ALBEDO_1_Mean.rdc',12)
maxy=float(t[0])

t = api.GetFromDocFile('G:\Benoit_Backup\STA_PAPER2_02262011\\AGG_16D_ALBEDO_1_Mean.rdc',4)
nb_col=float(t[0])
t = api.GetFromDocFile('G:\Benoit_Backup\STA_PAPER2_02262011\\AGG_16D_ALBEDO_1_Mean.rdc',5)
nb_row=float(t[0])

cols=round(nb_cols/nb_y)
rows=round(nb_rows/nb_x)

window_widthx = (maxx-minx)/nb_x
window_widthy = (maxy-miny)/nb_y
nb_windows = nb_y*nb_x

coord(4,5)

##w_minx = 0;
##w_maxx = 0;
##
##for i in range(1,nb_x+1):
##    w_minx = w_minx+ minx
##    w_maxx = w_minx + window_widthx
    

