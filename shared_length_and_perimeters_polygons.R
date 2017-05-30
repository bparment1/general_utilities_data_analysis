############### SESYNC Research Support: Urbanization Impact on Biodiversity ########## 
##
## 
## DATE CREATED: 05/30/2017
## DATE MODIFIED: 05/30/2017
## AUTHORS: Benoit Parmentier 
## Version: 1
## PROJECT: Urbanization impact on biodiversity
## ISSUE: 
## TO DO:
##
## COMMIT: initial commit
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
#in_dir <- "/research-home/bparmentier/Data/projects/urbanization_effects_on_biodiversity"
in_dir <- "/nfs/bparmentier-data/Data/projects/urbanization_effects_on_biodiversity/data"
#ARGS 2
infile_name <- "wwf_terr_ecos.shp"
#ARGS 3
#ARGS 5
out_dir <- "/nfs/bparmentier-data/Data/projects/urbanization_effects_on_biodiversity/outputs" #parent directory where the new output directory is located
#ARGS 6
create_out_dir_param=TRUE #create a new ouput dir if TRUE
#ARGS 7
out_suffix <- "urbanization_effects_biodiversity_05302017"

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

## Step 1: read in the data and generate time stamps

wwf_sp <- readOGR(in_dir,sub(".shp","",infile_name))
plot(wwf_sp)

### let's first test on North Carolina data
nc_file <- system.file("etc/shapes/sids.shp", package = "spdep")[1]
nc_file #path to the file provided by sp
nc_sp <- readOGR(dirname(nc_file),sub(".shp","",basename(nc_file)))
plot(nc_sp)

#https://stackoverflow.com/questions/12196440/extract-feature-coordinates-from-spatialpolygons-and-other-sp-classes

dim(nc_sp) #100 polygons!
#use fortify from ggplot2 to convert the sp object spatial attributes into a data.frame
fortified_nc_df <- fortify(nc_sp)
class(fortified_nc_df)
View(fortified_nc_df)

#see location1599: makring polygons as holes with a logical (TRUE/FALSE) flag, the hole slot, and using ring 
#direction -- clockwise rings are taken as not being holes, anti clokcwise as being holes. This is needed because
#the non-topological representation of polygons has no easy way of knowing that a polygon represent internaal boudnary of
#an encosing polygon, a hole or lake.
 
require(ggplot2); theme_set(theme_bw())
ggplot(aes(x = long, y = lat, group = group), data = fortified_nc_df) + geom_path()

#https://stackoverflow.com/questions/26499010/finding-adjacent-polygons-in-r-neighbors
test_nb <- poly2nb(nc_sp)
poly2nb #show content of function

test_poly <- nc_sp@polygons[[1]] #show first polygons

test <- as(nc_sp, "SpatialPolygons")
test2 <- as(nc_sp, "SpatialLines")
str(test2,max.level = 2)

class(test2@lines[[1]])

#works fine; I modified the code such that
#xx1<-as(xx, "owin")

gDifference(test2[[1]])



station_nb <- nrow(data)
if(is.null(max_dist)){
  while(station_nb > target_min_nb){
    data <- remove.duplicates(data, zero = dist_val) #spatially sub sample...
    dist_val <- dist_val + step_val
    station_nb <- nrow(data)
}
  #setdiff(as.character(data$id),as.character(data_in$id))
  #ind.selected <-match(as.character(data$id),as.character(data_in$id)) #index of stations row selected
  #ind.removed  <- setdiff(1:nrow(data_in), ind.selected) #index of stations rows removed 
  id_selected <- as.character(data$id)
  id_removed <- setdiff(unique(as.character(data_in$id)),as.character(data$id))




## From fortified table one can calculate length object...
str(nc_sp,max.level=2)


debug(poly2nb_test)
test_nb <- poly2nb_test(nc_sp)

#http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS1_SpatialDataTypes_part1_vectorData.html
?gTouches

test <- gTouches(nc_sp,byid=T)
test_nb[[1]]
poly2nb

x = readWKT("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
y = readWKT("POLYGON ((3 3, 7 3, 7 7, 3 7, 3 3))")
d = gDifference(x,y)
plot(d,col="red",pbg="white")

# Empty geometry since y is completely contained with x
d2 = gDifference(y,x)

y <- as(test_poly, 'SpatialLines') 


# Intersect lines with raster "polygons" and add length to new lines segments
#rrst.poly <- rasterToPolygons(rrst)
#rp <- gIntersection(roads, rrst.poly, byid=TRUE)
#rp <- SpatialLinesDataFrame(rp, data.frame(row.names=sapply(slot(rp, "lines"), 
#                                                            function(x) slot(x, "ID")), ID=1:length(rp), 
#                                           length=SpatialLinesLengths(rp)/1000) ) 


