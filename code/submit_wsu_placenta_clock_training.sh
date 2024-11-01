#!/bin/bash
#SBATCH --job-name=wsu_placental_clock_450k             # Name of your job
#SBATCH -q primary                                      # specify partition
#SBATCH --output=wsu_placental_clock_output_%j.log      # Standard output log file (%j expands to job ID)
#SBATCH --error=wsu_placental_clock_error_%j.log        # Standard error log file (%j expands to job ID)
#SBATCH --ntasks=1                                      # Number of tasks (commands) to run
#SBATCH --cpus-per-task=8                               # Number of CPU cores per task
#SBATCH --mem=200G                                      # Memory requirement per node
#SBATCH --time=12:00:00                                  # Time limit (4 hrs)
#SBATCH --mail-type=ALL                                 # Send email notifications for job events
#SBATCH --mail-user=dw1227@wayne.edu                    # Your email address

# Change to the directory 
cd /wsu/home/dw/dw12/dw1227/Documents/bhatti_dream_challenge_2024

module load r/4.4.0
Rscript code/wsu_placenta_clock_450k_bias_adj.R
