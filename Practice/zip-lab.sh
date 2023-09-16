#!/bin/bash

#===============================================================================
#
#          FILE:  zip-lab.sh
#        AUTHOR:  Edoardo Costantini
#       VERSION:  1.0
#       CREATED:  2023-01-18
#      REVISION:  2023-02-13
#   DESCRIPTION:  Bash script to create a zip file of a lab folde for uploading 
#                 to Canvas page
#
#         USAGE:  . zip-lab.sh $1
#
#     ARGUMENTS: $1  <week-#>
#       OPTIONS:  ---
#         NOTES:  ---
#===============================================================================

# Process arguments
WEEK=$1

# Print message introducing next task
tput bold; printf "Task: Compiling practice HTML document \n"; tput sgr0

# Knit practice script with no solutions so that it will be included in the zip
Rscript knit-practice.R FALSE $WEEK

# Print message introducing next task
tput bold; printf "Task: Compiling practice HTML document with solutions \n"; tput sgr0

# Knit solutions
Rscript knit-practice.R TRUE $WEEK

# Define which lab folder you want to opearte in
LAB=$WEEK-lab

# Create a copy with only the requested files
cp -r $LAB/ deliverables/$LAB

# Remove practice.Rmd and solutions
rm deliverables/$LAB/code/*.Rmd

# Remove solutions.html
rm deliverables/$LAB/docs/*solutions.html

# Remove any data generation script
rm deliverables/$LAB/code/*data-gen.R

# Remove .gitkeeps if any
find deliverables/$LAB/. -name ".gitkeep" -delete

# Remove bash trash if any
find deliverables/$LAB/. -name ".DS_Store" -delete

# Print message introducing next task
tput bold; printf "Task: Zipping solutions \n"; tput sgr0

# Zip solutions
zip -j deliverables/$WEEK-practice-solutions.zip $LAB/docs/$WEEK-practice-solutions.html

# Print message introducing next task
tput bold; printf "Task: Zipping lab folder \n"; tput sgr0

# Go to deliverables folder
cd deliverables

# Zip the copy
zip -r $LAB.zip $LAB

# Delete the temp folder
rm -r $LAB

# Go back to original folder
cd ../
