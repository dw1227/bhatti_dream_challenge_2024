#!/bin/bash

# Check if an argument is provided
if [ -z "$1" ]; then
  echo "Usage: ./run_python_script.sh <name_of_python_script>"
  exit 1
fi

# Load the R module
if [[ $(module list 2>&1 | grep -q "python/3.11"; echo $?) -ne 0 ]]  
         then 
             module load python/3.11 
         fi

# Run the provided R script
python "$1" "${@:2}"
