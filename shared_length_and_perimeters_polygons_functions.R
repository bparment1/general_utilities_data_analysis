############### SESYNC Research Support: Urbanization Impact on Biodiversity ########## 
##
## Calculating shared length of boudaries of neighbors. This is useful for spatial analysis.
## 
## DATE CREATED: 06/02/2017
## DATE MODIFIED: 06/20/2017
## AUTHORS: Benoit Parmentier 
## Version: 1
## PROJECT: Urbanization impact on biodiversity
## ISSUE: Support #22140
## TO DO:
##
## COMMIT: modifying outputs to save object for cluster run + documentation
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
  #This function compute share boundary length between a polyon and its neighbour.
  #It assumes that a collection of polygon feature (e.g. spatialPolygonDataFrame).
  # AUTHORS : Benoit Parmentier
  # CREATED : 06/02/2017
  # MODIFIED: 06/13/2017
  #
  #INPUTS
  #1)i : index selecting specific polygons and its neighbours
  #2)poly_sp : SpatialPolygonDataFrame object corresponding to a specific feature collection
  #3)poly_nb : neighbour object from spdep package
  #OUTPUTS
  #shared_nb_obj: object as list composed of
  #1)nb_df: data.frame with ID of neighbour and length of shared border, length is zero if point
  #2)list_nb_sp: list object corresponding to neighbours (spatial polygon or spatial point)
  
  ############ BEGIN SCRIPT ###############
  
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
  
  sink("running_code.txt",append=TRUE,split=TRUE)
  #ls -lsink("myfile.txt", append=TRUE, split=TRUE) 
  print(paste0("processed polygon number: ",i))
  sink()
  
  return(shared_nb_obj)
}

calculate_shared_boundaries_polygons <- function(poly_sp,poly_nb=NULL,edges=F,num_cores=1,out_dir=".",out_suffix=""){
  #
  #Calculate the length of shared edged between neighbours of a spatial polygons data frame object
  #
  ##INPUTS:
  #1) poly_sp: spatial object with polygons (SpatialPolygonsDataFrame)
  #2) list_nb: list of neighbours for the polygons/features, if NULL it is computed
  #3) num_cores: number of cores used in the analysis
  #4) edges: if edges is TRUE keep spatialLines objects corresponding to share boundaries between polygons
  #4) out_dir: output directory
  #5) out_suffix: output suffix added to all outputs
  ##OUTPUTS
  # object as list composed of the following items:
  # 1) nb_combined_df: data.frame with length of shared neighours and neibhour ID
  # 2) nb_combined_sp: shared neighbour objects (points or polygons)
  #
  
  ####### Begin function ####
  
  if(is.null(poly_nb)){
    poly_nb <- poly2nb(poly_sp)
    write.nb.gal(poly_nb,
                 file=file.path(out_dir,paste0("poly_nb_",out_suffix,".gal")))
  }
  
  #debug(calculate_shared_length)
  #test_lines_shared <- calculate_shared_length(11,
  #                                             poly_sp = poly_sp,
  #                                             poly_nb = poly_nb )
  
  no_poly <- length(poly_sp) #number of polygons features in the dataset
  
  sink("running_code.txt",append=TRUE,split=TRUE) #append to file and print to console
  print(paste0("Processing shared boundaries for ",no_poly," polygons"))
  sink()
  
  list_shared_nb_obj <- mclapply(1:no_poly,
                                 FUN=calculate_shared_length,
                                 poly_sp = poly_sp,
                                 poly_nb = poly_nb,
                                 mc.preschedule=FALSE,
                                 mc.cores = num_cores)
  
  
  ### Format output
  
  #### First extract shared sp object
  nb_combined_sp <- lapply(list_shared_nb_obj,function(x){x[[2]]})
  names(nb_combined_sp) <- 1:no_poly
  
  #### Now reformat and combine all data.frame with length of shared boundaries
  nb_combined_df <- lapply(list_shared_nb_obj,function(x){x[[1]]})
  names(nb_combined_df) <- 1:no_poly
  
  nb_combined_df <- do.call(rbind,nb_combined_df)
  nb_combined_df <- as.data.frame(nb_combined_df)
  nb_combined_df$ref_id <- rownames(nb_combined_df)
  
  rownames(nb_combined_df) <- NULL
  list_ref_id_tmp <- (strsplit(nb_combined_df$ref_id,"[.]")) #split is regexp so need bracket
  nb_combined_df$ref_id <- unlist(lapply(list_ref_id_tmp,function(x){x[1]}))
  
  
  shared_boundaries_obj <- list(nb_combined_df,nb_combined_sp)
  names(shared_boundaries_obj) <- c("nb_combined_df","nb_combined_sp")
  

  #save the information for later use (validation at monthly step!!)
  save(shared_boundaries_obj,file= file.path(out_dir,paste("shared_boundaries_obj",
                                                           out_suffix,
                                                           ".RData",sep="")))
  
  return(list_shared_nb_obj)
}



################## END OF SCRIPT #########################

