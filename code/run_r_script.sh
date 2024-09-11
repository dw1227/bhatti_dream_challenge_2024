#!/bin/bash

# Check if an argument is provided
if [ -z "$1" ]; then
  echo "Usage: ./run_script.sh <name_of_R_script>"
  exit 1
fi

# Load the R module
if [[ $(module list 2>&1 | grep -q "r/4.4.0"; echo $?) -ne 0 ]]  
         then 
             module load r/4.4.0 
         fi

# Run the provided R script
Rscript --vanilla "$1" "${@:2}"
