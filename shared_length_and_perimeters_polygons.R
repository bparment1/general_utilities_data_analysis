############### SESYNC Research Support: Urbanization Impact on Biodiversity ########## 
## Calculating shared length of boudaries of neighbors. This is useful for spatial analysis.
## 
## DATE CREATED: 05/30/2017
## DATE MODIFIED: 06/20/2017
## AUTHORS: Benoit Parmentier 
## Version: 1
## PROJECT: Urbanization impact on biodiversity
## ISSUE: 
## TO DO:
##
## COMMIT: get system env var to set the input
##
## Links to investigate:
#https://gis.stackexchange.com/questions/119993/convert-line-shapefile-to-raster-value-total-length-of-lines-within-cell
#https://edzer.github.io/sfr/articles/sfr.html
#https://geographicdatascience.com/2017/01/06/first-impressions-from-sf-the-simple-features-r-package/

###################################################
#

###### Library used

library(gtools)                              # loading some useful tools 
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
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

###### Functions used in this script

function_neighbors_calculations <- "shared_length_and_perimeters_polygons_functions_06202017.R" #PARAM 1
script_path <- "/nfs/bparmentier-data/Data/projects/urbanization_effects_on_biodiversity/scripts" #path to script #PARAM 

source(file.path(script_path,function_neighbors_calculations)) #source all functions used in this script 1.

##create an output directory
create_dir_fun <- function(out_dir,out_suffix){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    out_dir <- file.path(out_dir,out_name)
  }
  #create if does not exists
  if(!file.exists(out_dir)){
    dir.create(out_dir)
  }
  return(out_dir)
}

load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}


#####  Parameters and argument set up ###########

#ARGS 1
in_dir <- "/nfs/bparmentier-data/Data/projects/urbanization_effects_on_biodiversity/data"
#ARGS 2
#infile_name <- "Biomes_disolv.shp"
#infile_name <- "sids.shp"
INFILE_NAME <- Sys.getenv('INFILE_NAME')
infile_name <- as.character(INFILE_NAME)
#ARGS 3
num_cores <- 8
#ARGS 4

#ARGS 5
out_dir <- "/nfs/bparmentier-data/Data/projects/urbanization_effects_on_biodiversity/outputs" #parent directory where the new output directory is located
#ARGS 6
create_out_dir_param=TRUE #create a new ouput dir if TRUE
#ARGS 7
out_suffix <- "urbanization_effects_biodiversity_06202017"

################# START SCRIPT ###############################

### PART I READ AND PREPARE DATA #######
#set up the working directory
#Create output directory

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

if(is.null(num_cores)){
  num_cores<- detectCores()
}

## Step 1: read in the data and generate time stamps

poly_sp <- readOGR(in_dir,sub(".shp","",infile_name))
#plot(wwf_sp)

#test_poly <- poly_sp@polygons[[1]] #show first polygons 
#test_poly
#list_nb_wwf <- poly2nb(wwf_sp)

### let's first test on North Carolina data
#nc_file #path to the file provided by sp
#nc_sp <- readOGR(dirname(nc_file),sub(".shp","",basename(nc_file)))
#plot(nc_sp)

#dim(nc_sp) #100 polygons!

#nc_nb <- poly2nb(nc_sp)

#nc_nb[[1]]
#> nc_nb[[1]]
#[1] 17 19 41 68 76 79
#nc_nb[[11]]


### set up log file
if(file.exists("running_code.txt")){
  file.remove("running_code.txt")
}

sink("running_code.txt",append=TRUE,split=TRUE) #append to file and print to console
print(paste0("Computing shared boundaries using ",function_neighbors_calculations))
print(paste0("On ",date()))
sink()


#debug(calculate_shared_boundaries_polygons)

test_shared_boundaries <- calculate_shared_boundaries_polygons(poly_sp=poly_sp,
                                                               poly_nb=NULL,
                                                               edges=T,
                                                               num_cores=num_cores,
                                                               out_dir=out_dir,
                                                               out_suffix= out_suffix)



################## END OF SCRIPT #########################

