############### SESYNC Research Support: ocean colors datasets for environmental applications ########## 
## Importing and processing data about the water/ocean from MODIS sensor.
## 
## DATE CREATED: 11/01/2017
## DATE MODIFIED: 11/02/2017
## AUTHORS: Benoit Parmentier 
## PROJECT: Ocean colors data
## ISSUE: 
## TO DO:
##
## COMMIT: loading nc ocean color data with raster R 
##
## Links to investigate:
## backscattering bands: https://oceandata.sci.gsfc.nasa.gov/MODIS-Terra/Mapped/Monthly/4km/bb/
## refleance bands: https://oceandata.sci.gsfc.nasa.gov/MODIS-Terra/Mapped/Monthly/4km/Rrs/

###################################################
#

###### Library used

library(gtools)                              # loading some useful tools 
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(raster)
library(gdata)                               # various tools with xls reading, cbindX
library(rasterVis)                           # Raster plotting functions
library(parallel)                            # Parallelization of processes with multiple cores
library(maptools)                            # Tools and functions for sp and other spatial objects e.g. spCbind
library(maps)                                # Tools and data for spatial/geographic objects
library(plyr)                                # Various tools including rbind.fill
library(spgwr)                               # GWR method
library(rgeos)                               # Geometric, topologic library of functions
library(gridExtra)                           # Combining lattice plots
library(colorRamps)                          # Palette/color ramps for symbology
library(ggplot2)
library(lubridate)
library(dplyr)
library(car)
library(sf)

###### Functions used in this script and sourced from other files

create_dir_fun <- function(outDir,out_suffix=NULL){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

#Used to load RData object saved within the functions produced.
load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

### Other functions ####

#function_processing_data <- ".R" #PARAM 1
#script_path <- "/nfs/bparmentier-data/Data/projects/ocean_colors_data/scripts" #path to script #PARAM 
#source(file.path(script_path,function_processing_data)) #source all functions used in this script 1.

############################################################################
#####  Parameters and argument set up ###########

in_dir <- "/nfs/bparmentier-data/Data/projects/ocean_colors_data/data" #local bpy50 , param 1
out_dir <- "/nfs/bparmentier-data/Data/projects/ocean_colors_data/outputs" #param 2

num_cores <- 2 #param 8
create_out_dir_param=TRUE # param 9

out_suffix <-"ocean_colors_example_11022017" #output suffix for the files and ouptut folder #param 12

############## START SCRIPT ############################

######### PART 0: Set up the output dir ################

if(is.null(out_dir)){
  out_dir <- in_dir #output will be created in the input dir
  
}
#out_dir <- in_dir #output will be created in the input dir

out_suffix_s <- out_suffix #can modify name of output suffix
if(create_out_dir_param==TRUE){
  out_dir <- create_dir_fun(out_dir,out_suffix)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}

### PART I READ AND PREPARE DATA #######
#set up the working directory
#Create output directory

## band backscattering
lf_bb <- list.files(path=in_dir,
                    pattern="*.bb.*",
                    full.names=T) #this is the list of folder with RAW data information

r_stack_bb <- stack(lf_bb)
plot(r_stack_bb,y=1)

## Remote Sensing reflectance 

## band backscattering
lf_rrs <- list.files(path=in_dir,
                    pattern="*.RRS.*",
                    full.names=T) #this is the list of folder with RAW data information

r_stack_rrs <- stack(lf_rrs)
plot(r_stack_rrs,y=1)

NAvalue(r_stack_rrs) #find out NA values
animate(r_stack_rrs) #generate animation for specific bands

## Kd 490 

T20000322000060.L3m_MO_KD490_Kd_490_4km.nc

## band backscattering
lf_kd <- list.files(path=in_dir,
                     pattern="*.Kd.*",
                     full.names=T) #this is the list of folder with RAW data information

r_stack_kd <- stack(lf_kd)
plot(r_stack_KD,y=1)

NAvalue(r_stack_rrs) #find out NA values
animate(r_stack_rrs) #generate animation for specific bands

### create spectral profile for specific location


####################### END OF SCRIPT ###########################################