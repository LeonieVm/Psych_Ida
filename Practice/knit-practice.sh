#!/bin/bash

#===============================================================================
#
#          FILE:  knit-practice.sh
#        AUTHOR:  Edoardo Costantini
#       VERSION:  1.0
#       CREATED:  2023-01-18
#      REVISION:  2023-03-09
#   DESCRIPTION:  Bash script to compile *-practice.rmd html ouputs
#                 to Canvas page
#
#         USAGE:  . ziplab.sh $1
#
#     ARGUMENTS: $1  <week-#>
#       OPTIONS:  --solutions: should you knit with solutions? (default is without)
#         NOTES:  ---
#===============================================================================

# Process arguments
WEEK=$1
if [ "$2" = "--solutions" -o "$2" = "-s" ]
then
    SOL=TRUE
else
    SOL=FALSE
fi

# Make a temporary directory
Rscript knit-practice.R $SOL $WEEK