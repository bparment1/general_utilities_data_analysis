############### SESYNC Research Support: Urbanization Impact on Biodiversity ########## 
##
## Calculating shared length of boudaries of neighbors. This is useful for spatial analysis.
## 
## DATE CREATED: 06/02/2017
## DATE MODIFIED: 06/02/2017
## AUTHORS: Benoit Parmentier 
## Version: 1
## PROJECT: Urbanization impact on biodiversity
## ISSUE: 
## TO DO:
##
## COMMIT: modifying outputs of list to data.frame of connectivity and list spatial obj
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

intersect_poly_length_fun <- function(poly_sp1,poly_sp2){
  #Intersects two polygons and returns the shared boundary and length
  #
  ## To do add option for overlap? if polygons
  
  intersection_sp <- gIntersection(poly_sp1,poly_sp2)
  #note that sp object can be points or lines, if points then lenght is zero
  if(inherits(intersection_sp,"SpatialLines")){
    length_val <- SpatialLinesLengths(intersection_sp)
  }
  if(inherits(intersection_sp,"SpatialPoints")){
    length_val <- 0
  }
  
  ### Add overlap if polygon
  #length_val <- NA
  #area_val <- ...
  
  intersect_obj <- list(length_val,intersection_sp)
  
  return(intersect_obj)
}

calculate_shared_length <- function(i,poly_sp,poly_nb){
  #
  #
  
  poly_sp_ref_selected <- poly_sp[i,] # reference polygon under consideration for which neighbours are compared to
  poly_nb_selected <- poly_nb[[i]] #list of neighbour for the ref poly selected
  list_polygons_neighbors <- lapply(poly_nb_selected,FUN=function(j){poly_sp[j,]})
  names(list_polygons_neighbors) <- poly_nb_selected
  
  #debug(interesect_poly_length_fun)
  
  list_lines_shared <- lapply(list_polygons_neighbors,FUN=intersect_poly_length_fun,poly_sp2=poly_sp_ref_selected)
  #browser()
  
  #list_lines_shared
  
  #browser()
  ### make data.frame with length and id of neighbours
  nb_df <- lapply(list_lines_shared,function(x){x[[1]]})
  nb_df <- do.call(rbind,nb_df)
  nb_df <- as.data.frame(nb_df)
  names(nb_df) <- "length"
  nb_df$id <- rownames(nb_df)
  rownames(nb_df) <- NULL
  ### Extract sp object resulting from intersection
  list_nb_sp <- lapply(list_lines_shared,function(x){x[[2]]})
  
  shared_nb_obj <- list(nb_df=nb_df,list_nb_sp=list_nb_sp)
  return(shared_nb_obj)
}

calculate_shared_boundaries_polygons <- function(poly_sp,poly_nb=NULL,edges=F,num_cores=1,out_dir=".",out_suffix=""){
  #Calculate the length of shared edged between neighbours of a spatial polygons data frame object
  #
  ##INPUTS:
  #1) poly_sp
  #2) list_nb
  #3) num_cores
  #4) edges: if edges is TRUE keep spatialLines objects corresponding to share boundaries between polygons
  #4) out_dir
  #5) out_suffix
  
  ####### Begin function ####
  
  if(is.null(poly_nb)){
    poly_nb <- poly2nb(poly_sp)
  }
  
  #debug(calculate_shared_length)
  #test_lines_shared <- calculate_shared_length(11,
  #                                             poly_sp = poly_sp,
  #                                             poly_nb = poly_nb )
  
  no_poly <- length(poly_sp) #number of polygons features in the dataset
  list_shared_nb_obj <- mclapply(1:no_poly,
                                 FUN=calculate_shared_length,
                                 poly_sp = poly_sp,
                                 poly_nb = poly_nb,
                                 mc.preschedule=FALSE,
                                 mc.cores = num_cores)
  
  
  return(list_shared_nb_obj)
}



################## END OF SCRIPT #########################

