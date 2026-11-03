#!/bin/bash -l

#SBATCH --job-name=fcip_inst
#SBATCH --time=0-20:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=28G
#SBATCH --array=1
#SBATCH --mail-user=ftsiboe@hotmail.com
#SBATCH --mail-type=ALL

module purge
module load RStudio-Server/2023.09.1+494-foss-2023a-Java-11-R-4.3.2
R --no-save -q < data-raw/replications/crop_insurance_instruments/generate_fcip_instruments.R

