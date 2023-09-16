# Course:   Statistics and methodology
# Title:    Compiling script
# Author:   Edo
# Created:  2023-03-09
# Modified: 2023-03-09
# Descrip.: For a given week, this script compiles an html file out of the 
#           Rmd practice file

# Example input ----------------------------------------------------------------

# solutions <- TRUE
# week <- "week-6"

# Body -------------------------------------------------------------------------

# Store terminal commands
args <- commandArgs(trailingOnly = TRUE)

# Process provided arguments
solutions <- args[1]
week <- args[2]

# Define input file name
input <- paste0("./", week, "-lab/code/", week, "-practice.Rmd")

# Define output file name
output_file <- paste0(
    week,
    "-practice",
    ifelse(solutions, "-solutions", ""),
    ".html"
)

# Define output directory
output_dir <- paste0("./", week, "-lab/docs/")

# Call rendering function
rmarkdown::render(
    input = input,
    output_file = output_file,
    output_dir = output_dir,
    quiet = TRUE
)