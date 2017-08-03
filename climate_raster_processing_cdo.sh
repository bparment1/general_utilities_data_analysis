#Shell sript to process *.nc files from C. Trisos
#The script call the cdo tools application:
#https://code.mpimet.mpg.de/projects/cdo/

#commit: initial commit

## list directory to process

listdir=`ls`

### example of set up loop:
dir_processed="bcc-csm1-1"
model_name="historical"

cdo mer bcc-csm1-1/*historical*.nc

for dir_processed in $listdir ; do
     echo the value of dir_processed is $dir_processed
     #add cdo command here
     #generate file name on the fly
done


##