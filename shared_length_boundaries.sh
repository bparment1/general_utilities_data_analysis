#!/bin/bash
#SBATCH --job-name=shared_boundaries_test    # Job name
#SBATCH --mail-type=ALL                          # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=bparmentier@sesync.org       # Where to send mail
#SBATCH --time=00:50:00                          # Time limit hrs:min:sec
#SBATCH --output=shared_boundaries_test_%A_%a.out   # Standard output from console
#SBATCH --error=shared_boundaries_test_%A_%a.err   # Error log 
#SBATCH --partition=sesyncshared        # queue name, this is for debugging and testing

pwd; hostname; date
 
echo "Running shared boundaries code for testing"
 
Rscript --vanilla /nfs/bparmentier-data/Data/projects/urbanization_effects_on_biodiversity/scripts/shared_length_and_perimeters_polygons_06072017.R
