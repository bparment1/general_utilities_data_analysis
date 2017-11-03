############### SESYNC Research Support: ocean colors datasets for environmental applications ########## 
## Importing and processing data about the water/ocean from MODIS sensor.
## This is an example with Lake Malawi as a studya area.
##
## Data were downloaded from EARTHDATA NASA.
## https://oceandata.sci.gsfc.nasa.gov/MODIS-Terra/Mapped/Monthly/4km/Kd/
## An account is necessary if many files are downloaded.
## The products were downloaded manually but it is possible to write a script to automatically
## navigate the server directory structure and obtain the files.
##
## DATE CREATED: 11/01/2017
## DATE MODIFIED: 11/03/2017
## AUTHORS: Benoit Parmentier 
## PROJECT: Ocean colors data
## ISSUE: 
## TO DO:
##
## COMMIT: cleaning and producing example dataset 
##
## Links to investigate:
## Kd product: https://oceandata.sci.gsfc.nasa.gov/MODIS-Terra/Mapped/Monthly/4km/Kd/
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

###### Functions definitions/declarations used in this script and sourced from other files ##########

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

#function_processing_data <- ".R" #PARAM 1, not implemented right now
#script_path <- "/nfs/bparmentier-data/Data/projects/ocean_colors_data/scripts" #path to script #PARAM 
#source(file.path(script_path,function_processing_data)) #source all functions used in this script 1.

############################################################################
#####  Parameters and argument set up ###########

in_dir <- "/nfs/bparmentier-data/Data/projects/ocean_colors_data/data" #local bpy50 , param 1
out_dir <- "/nfs/bparmentier-data/Data/projects/ocean_colors_data/outputs" #param 2

num_cores <- 2 #param 8
create_out_dir_param=TRUE # param 9

out_suffix <-"ocean_colors_example_11032017" #output suffix for the files and ouptut folder #param 12

#Region study area from http://www.masdap.mw/layers/geonode%3Amalawi_lake
infile_reg_outline <- "malawi_lake.shp" #study area

file_format <- ".tif" #raster format used as output
NA_flag_val <- -9999 #pixels values for missing, backgroud or no-data

############## START SCRIPT ############################

#####################
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

#################
### PART I READ AND PREPARE DATA #######
#set up the working directory
#Create output directory

## Remote Sensing reflectance 

## band backscattering
lf_rrs <- list.files(path=in_dir,
                    pattern="*.RRS.*",
                    full.names=T) #this is the list of folder with RAW data information

r_stack_rrs <- stack(lf_rrs) #create a stack of raster images
#Composite reflectance from 2001001 to 2001031 (January 2001)
plot(r_stack_rrs,y=1) # plot the first image from the raster stack object

NAvalue(r_stack_rrs) #find out NA values
dataType(r_stack_rrs) #find out the data type, here FLT4S

## Kd 490nm attenuation

#e.g.: T2000032 2000060.L3m_MO_KD490_Kd_490_4km.nc
#
#      T2017244 2017273.L3m_MO_SST_sst_4km.nc
lf_kd <- list.files(path=in_dir,
                     pattern="*.Kd.*",
                     full.names=T) #this is the list of folder with RAW data information

r_stack_kd <- stack(lf_kd) #stack of Kd
plot(r_stack_kd,y=1)

#######################
###### PART 2: A quick exploration and extraction ###########

### Examine values across 12 months for 2001

NAvalue(r_stack_kd) #find out NA values
#animate(r_stack_kd) #generate animation for specific bands/product

### create temporal profile for specific location using monthly kd data for 2001 (monthly)

geog_loc <- c(-100,-10) #longitude and latitude, South America coast

geog_loc_mat <- matrix(geog_loc,nrow=1,ncol=2)
kd_df <- extract(r_stack_kd,geog_loc_mat)
plot(kd_df[1,],type="b")

### create spectral profile for specific location using monthly kd data from 2000 and 2001

rrs_df <- extract(r_stack_rrs,geog_loc_mat)

bands_names<- names(r_stack_rrs)
bands_char<- strsplit(bands_names,"[.]")
bands_labels <- unlist(lapply(bands_char,function(x){x[5]})) #get band values in nm

plot(rrs_df[1,],type="b",xaxt="n",xlab="reflectance band (nm)",ylab="Reflectance value")
axis(side=1, at=1:10, labels=bands_labels,las=2) # pos=, lty=, col=, las=, tck=, ...)
title("MODIS Terra Reflectance: Ocean color product")

pt_sf <- st_point(geog_loc)
plot(r_stack_rrs,y=1,
     main="location of extracted point")
plot(pt_sf,add=T)


#######################
###### PART 3: Extracting data for a specific study area ###########

#### get data out of the image for specific area
## The reference data for Lake Malawi was obtained here:
#http://www.masdap.mw/layers/geonode%3Amalawi_lake

reg_sf <- st_read(file.path(in_dir,infile_reg_outline)) # Read in as sf object

reg_sp <- as(reg_sf,"Spatial") # convert to Spatial object

r_kd_malawi <- crop(r_stack_kd,reg_sp) #Crop raster stack using region's extent
plot(r_kd_malawi,y=1:12) #Take a look at the time series for the variable of interest (kd here)

malawi_mean_df <- extract(r_stack_kd,reg_sp,fun=mean,df=T,na.rm=T) # Extract the average for the area of interest
plot(malawi_mean_df[1,],type="b")

### Save data in multiple format:
#Write out cropped data as raster:

out_raster_filename <- paste("study_area_cropped_pixels_",out_suffix,file_format,sep="")
writeRaster(r_kd_malawi, 
            file.path(out_dir,out_raster_filename),
            NAflag=NA_flag_val,
            bylayer=T,
            suffix=names(r_kd_malawi))
            
#Write out cropped data as shapefile and textfile

malawi_data_sp <- rasterToPoints(r_kd_malawi,spatial=T) #sp points object
dim(malawi_data_sp)

outfile <- paste0("study_area_values_pixels_",out_suffix)
writeOGR(malawi_data_sp,dsn= ".",layer= outfile, driver="ESRI Shapefile",overwrite_layer=TRUE)

### Use the new sf package to write out:
malawi_data_sf <- as(malawi_data_sp,"sf")
outfile_sf <- paste0("study_area_sf_values_pixels_",out_suffix)

st_write(malawi_data_sf,
         file.path(out_dir,outfile_sf),
         driver="ESRI Shapefile")

malawi_data_df <- as.data.frame(malawi_data_sp)
dim(malawi_data_df) # note x, y column added!!

out_filename_df <- paste0("study_area_values_pixels_",out_suffix,".txt")
write.table(malawi_data_df,
            file.path(out_dir,out_filename_df),
            sep=",")

####################### END OF SCRIPT ##################################################
