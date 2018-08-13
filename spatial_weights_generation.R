############### SESYNC Research Support: Neighborhood Desirability ########## 
##
##    This script goal is to merge spatial weights in spdep
##    ultimately the goal is to have weigths where:
##    each front yard (which has a parcel id, we can join back to parcel),
##     
##    a)  neighbors all other front yards on the same street segment, 
##    b)  neighbors all other front yards in that same block group, and
##    c)  neighbors parcels on eigther side [queen contiguity] and parcels across
##    the street based on distances derived from road widths
##
##  The goal is to generate three hierachical levels of the neighborhood structure
##  for the spatial regression.
##
##
## DATE CREATED: 0x/03/2018
## DATE MODIFIED: 08/10/2018
## AUTHORS: Dexter Locke, modified by Benoit Parmentier  
## Version: 1
## PROJECT: Neighborhood Desirability
## ISSUE: 
## TO DO:
##
## COMMIT: initial commit
##

###################################################
#

###### Library used
# load the libraries
#library(spdep) #spatial analyses operations, functions etc.
library(spdep)           # builds neighbors, and much, much more
library(sf)              # reads and handels spatial data
library(mapview)         # makes a fun web map, to vizualize the dat
library(dplyr)           # data wrangling

library(sp) # spatial/geographic objects and functions
library(rgdal) #GDAL/OGR binding for R with functionalities
library(gtools) # contains mixsort and other useful functions
library(maptools) # tools to manipulate spatial data
library(parallel) # parallel computation, part of base package no
library(rasterVis) # raster visualization operations
library(raster) # raster functionalities
library(forecast) #ARIMA forecasting
library(colorRamps) #contains matlab.like color palette
library(rgeos) #contains topological operations
library(sphet) #contains spreg, spatial regression modeling
library(BMS) #contains hex2bin and bin2hex, Bayesian methods
library(bitops) # function for bitwise operations
library(foreign) # import datasets from SAS, spss, stata and other sources
library(gdata) #read xls, dbf etc., not recently updated but useful
library(classInt) #methods to generate class limits
library(plyr) #data wrangling: various operations for splitting, combining data
#library(gstat) #spatial interpolation and kriging methods
library(readxl) #functionalities to read in excel type data
library(psych) #pca/eigenvector decomposition functionalities
library(snow)

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

#Benoit setup
script_path <- "/nfs/bparmentier-data/Data/projects/neighborhood_desirability_in_urban_environment/scripts"
#mosaicing_functions <- "weighted_mosaicing_functions_07252018.R"
#source(file.path(script_path,mosaicing_functions))

#########cd ###################################################################
#####  Parameters and argument set up ########### 

#ARGS 1
in_dir <- "/nfs/bparmentier-data/Data/projects/neighborhood_desirability_in_urban_environment/data"
#ARGS 2
out_dir <- "/nfs/bparmentier-data/Data/projects/neighborhood_desirability_in_urban_environment/outputs"
#ARGS 3:
#NA_flag <- -999999
NA_flag_val <- NULL
#ARGS 4:
file_format <- ".tif"
#ARGS 5:
create_out_dir_param=TRUE #create a new ouput dir if TRUE
#ARGS 7
out_suffix <-"spatiaLweights_pocessing_08092018" #output suffix for the files and ouptut folder
#ARGS 8
num_cores <- 2 # number of cores

in_filename <- "analysisShapes/bf_chm.shp"

################# START SCRIPT ###############################

######### PART 0: Set up the output dir ################

options(scipen=999)

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

#######################################
### PART 1: Read in DATA #######

#data_df <- read.table(file.path(in_dir,in_filename),
#                      sep=",",
#                      header=T)

#rm(list=ls())            # clear


# Benoit's ideas (paraphrased by Dexter)

# build global IDs (block group, road, etc - the larger grouping unit)
# subset for those IDs (subset.nb?)
# calculate a distance such that all units within that group are neighs
#    use the row.names (the global ID) to ensure uniqueness
# recombine the nb lists for each IDs (union.nb?)

#getwd()             # for Dexter this goes to "/nfs/dlocke-data/LMc/LMc_Bos"
#list.files("GIS/analysisShapes/")
list.files(path=file.path(in_dir,"analysisShapes"))

# yards data
#system.time(sf <- st_read('GIS/analysisShapes/bf_chm.shp', quiet = F))
system.time(sf <- st_read(file.path(in_dir,in_filename), quiet = F))

a <- object.size(sf); print(a, units='Mb')
dim(sf) #360846
#View(sf)
#assume that YARD_ID is unique?

sf_orig <- sf
# roads - not really needed
#system.time(rds <- st_read('GIS/analysisShapes/roads_ply.shp', quiet = F))
#a <- object.size(rds); print(a, units='Mb'); rm(a)

# filter down
sf   <- sf %>% filter(sample == 2) #yards
#rds  <- rds%>% filter(sample == 2) #roads
dim(sf)
length(unique(sf$YARD_ID)) #OK unique

# make an sp version of shp (ie not a simple feature), then map it
sp <- as(sf, "Spatial"); class(sp)
#rd <- as(rds,"Spatial"); class(rd)
par(mar=c(0,0,0,0), mfrow=c(1,2)); plot(sp)#; plot(rd)

# filter down again to just front yards
spFront <- sp[sp$YARD == 'Front', ];dim(spFront); dim(sp)/2 #0 

head(sf)
dim(sf)
plot(sf['YARD'])
#plot(st_centroid(sf)['YARD'])
#plot(st_centroid(rds)['STREET_NAM'])

# web map with centoids of yards
mapview(sf, zcol='YARD') + mapview(st_geometry(st_centroid(sf))) 
#+ mapview(rds['STREET_NAM'])

# view the sp vesion of the subest
par(mar=c(0,0,5,0), mfrow=c(1,4))
plot(spFront, main="Front Yards")

# loop through each block group (n=2 as a test, lster use street segment)
# and make all parcels in that block group (or later that segment) neighs
# but first get the front yard centroids
cents <- SpatialPointsDataFrame(coords = coordinates(spFront),
                                data = spFront@data,
                                proj4string=CRS("+proj=utm +zone=19 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

# viz
plot(cents, main="Front Yard Centroids")

# viz
plot(spFront, main='Front Yards in same \nblock group are neighbors')
i<- unique(cents$ID_CBG)[1]               # n = 2

### Make this a function that you can use with lapply or mclapply
ID_var_name <- "ID_CBG"
n_features <- unique(cents[[ID_var_name]])
ref_id_var_name <- "" #reference global ID

for(i in n_features){    # can replace "ID_CBG" with street segment later on
     print(i)
     
     # make all fronts in the same ID_CBG neighbors
     # Benoit's suggested appraoch requires global IDs
     # knn2nb doesn't allow for that, but the point is to make all grouped
     # features neighbors.
     #    dnearneigh does have a row.names argument for IDs, potential alternative
     
     dim(cents[cents$'ID_CBG' == i,])[1] -1 #this 341 elements
     ?knearneigh
     
     selected_parcels <- cents[cents$'ID_CBG' == i,]
     dim(selected_parcels)
     selected_parcels <- cents[cents[[ID_var_name]] == i,]
     
     k = dim(cents[cents$'ID_CBG' == i,])[1] - 1
     
     test <- knearneigh(selected_parcels,
                          k= nrow(selected_parcels)-1)
     test$np
     test$nn
     
     selected_parcels$ID_CBG
     
     test$nn
     nb_all <- knn2nb(knearneigh(cents[cents$'ID_CBG' == i,],
                                 k = dim(cents[cents$'ID_CBG' == i,])[1] - 1))
     print(nb_all)
     plot(nb_all, coordinates(cents[cents$'ID_CBG' == i,]),
          add=T, col='blue',  lwd=.5, pch="*") # add the network   
     
}


# now by street segment
plot(spFront, main='Front Yards on same\nstreet segment\nare neighbors')
table(droplevels(cents$street_nam)) # some segments only have 1 front yard
                                    # no neighbors

# get only the street segments with more than 1 front yard KLUDGEY!
valid_st <-    select(as.data.frame(sf), -geometry) %>%
               filter(YARD == 'Front') %>%
               select(street_nam) %>%
               group_by(street_nam) %>%
               mutate(count = n()) %>%
               filter(count > 1) %>%
               as.data.frame() %>%
               distinct() %>%
               arrange(count); valid_st

for(i in valid_st$street_nam){  # street segment
     print(i)
     
     # make all fronts in the same ID_CBG neighbors
     nb_knn1 <- knn2nb(knearneigh(cents[cents$'street_nam' == i,],
                                  k = dim(cents[cents$'street_nam' == i,])[1] - 1))
     print(nb_knn1)
     plot(nb_knn1, coordinates(cents[cents$'street_nam' == i,]),
          add=T, col="blue",  lwd=.5, pch="*") # add the network
     
}


# examine just the street segment map
par(mfrow=c(1,1))
start.time <- proc.time()[3]	  # start clock
plot(spFront, main='Front Yards on same\nstreet segment\nare neighbors')
for(i in valid_st$street_nam){  # street segment
     print(i)
     
     # make all fronts in the same ID_CBG neighbors
     nb_knn1 <- knn2nb(knearneigh(cents[cents$'street_nam' == i,],
                                  k = dim(cents[cents$'street_nam' == i,])[1] - 1))
     #print(nb_knn1)
     plot(nb_knn1, coordinates(cents[cents$'street_nam' == i,]),
          add=T, col="blue",  lwd=.5, pch="*") # add the network
     
}
elapsed.time <- proc.time()[3]; print(elapsed.time)/60		# clock out


# need to figure out how to re-combine nb's with different ids
test <- union.nb(nb_knn1, nb_knn2); test

image(as(nb2listw(nb_knn1, zero.policy=TRUE), "CsparseMatrix"))


# end June 25, 2018 DHLocke

# old experimental below


# example of aggregating nbs
# on queen and one K = 4

knn <- knn2nb(knearneigh(cents, k = 4)); summary(knn)
dnn <- dnearneigh(cents, d1=0, d2=100); summary(dnn)

plot(cents)

nb <- aggregate.nb(knn, dnn)

nb <- poly2nb(rd, row.names = rd$street_n_1, queen=TRUE); summary(nb)
#image(as(nb2listw(nb, style="B"), "CsparseMatrix"))

bnb <- nb2blocknb(nb, as.character(rd$group))



plot(nb_F, coordinates(spFront), add=T, col="red",  lwd=.75, pch='.') # add the network

system.time(nb_d<- dnearneigh(cents, d1=0, d2=myDist)); summary(nb_d)

plot(nb_d, coordinates(spFront), add=T, col="blue",  lwd=.75, pch="*") # add the network


# 0 trim down to just front yards
# 1 build a queen contiguity matrix (map it)
# 2 build a knn contiguity matrix (map it)
# 3 merge and map 1 and 2
# 4 combine front and back yards?
spFront <- sp[sp$YARD == 'Front', ];dim(spFront); dim(sp)/2 #0 
par(mar=c(0,0,5,0), mfrow=c(1,3))
plot(spFront, main="Queen Contiguity", col.main="red")

# 1 build queen contiguity matrix
system.time(nb_F <- poly2nb(spFront, queen=TRUE)); summary(nb_F)

#  make a lists object from the neighbor list
# system.time(listw_F <- nb2listw(nb_F, zero.policy=TRUE)); 
# summary(listw_F, zero.policy=TRUE)

# plots first order queen contiguity
plot(nb_F, coordinates(spFront), add=T, col="red",  lwd=.75, pch='.') # add the network


# 2 build a knn contiguity matrix (map it)
# (first need centroids)
cents <- SpatialPointsDataFrame(coords = coordinates(spFront),
                                data = spFront@data,
                                proj4string=CRS("+proj=utm +zone=19 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))




k <- 4
system.time(nb_knn <- knn2nb(knearneigh(cents, k = k))); summary(nb_knn)
plot(spFront,  main="K-Nearest Neigh Contiguity\nK=4", col.main="purple")
plot(nb_knn, coordinates(cents), add=T, col="purple",  lwd=.75, pch='.') # add the network

# layer on queen and knn
plot(spFront, main="both")
un <- union.nb(nb_knn,nb_F)


plot(nb_knn, coordinates(cents), add=T, col="purple",  lwd=.75, pch='.') # add the network
plot(nb_F, coordinates(spFront), add=T, col="red",  lwd=.75, pch='.') # add the network

head(nb_F); head(nb_knn) # seems like all KNN neighs are also in Queen, but not the other way around

# at what distances will all fronts have neighbors
system.time(nb_knn1 <- knn2nb(knearneigh(cents, k = 1))); nb_knn1
dists <- unlist(nbdists(nb_knn1, coords=coordinates(spFront))); summary(dists)

barplot(table(dists))

myDist <- max(dists) / 1.75; myDist
system.time(nb_d<- dnearneigh(cents, d1=0, d2=myDist)); summary(nb_d)

plot(nb_d, coordinates(spFront), add=T, col="blue",  lwd=.75, pch="*") # add the network



# 3 merge and map 1 and 2
# 4 combine front and back yards?












if(FALSE){
     # make a neighbor list, using queen contiguity (vertex or edge = neigh)
     system.time(nb <- poly2nb(sp, queen=TRUE)); summary(nb)
     
     # make a lists object from the neighbor list
     system.time(listw <- nb2listw(nb)); listw
     
     # map out the weights matrix 
     plot(nb, coordinates(sp), add=T, col="red", lwd=.5, pch=1) # add the network
     
     # the most connected feature is region.id = 688
     nb[[688]]
     nb2 <- droplinks(nb, 688); nb2[[688]] # practice manipulating links
     plot(nb2, coordinates(sp), add=T, col="gray", lwd=.5, pch=1) # add the network
     all.equal(nb, nb2, check.attributes=FALSE)
     
     # practice droppingn links 'manually' (aka without droplinks())
     summary(nb)
     nb3 <- nb
     nb3[[7]]; str(nb3)
     nb3[[7]][1] <- as.integer(9)
     nb3[[7]][2] <- as.integer(10)
     str(nb3)
     plot(sp)
     plot(nb2, coordinates(sp), add=T, col="gray", lwd=.5, pch=1) # add the network
     plot(nb3, coordinates(sp), add=T, col="red",  lwd=.5, pch=1) # add the network
     
     # we have successfully re-assigned neighbors
     # key is to make neighborh region.id 's neighbors as INTEGER
}



################################ End of script ############################