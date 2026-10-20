## Code for reading in DataCite data files, returning only observations
## that list NIH as a funder. Update 'dirpath' to name of current
## directory, with DataCite data files stored in a folder (within this
## directory) called 'data'. Writes NIH data to the file specified by
## 'outfile'.

## Code written by Ashvin Narayan (divyanrn@umich.edu), modified 20 October 2026.
## DataCite files (version 4.1, updated 15 August 2025) were downloaded 8 
## October 2026. DataCite Corpus data files are available at
## https://zenodo.org/records/16901115.

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

#### FUNCTION DEFINITIONS ####

## FUNCTION: get_nih, reads in dataframe, perform basic preprocessing,
## return only data for NIH-funded datasets (based on fundersROR).
get_nih <- function(filename) {
  dat <- read_csv(file = filename, na = c("", "NA", "none"))
  
  # Prints column names and types for debugging. 
  c("Columns:", colnames(dat)) |> paste(collapse = ", ") |> print()
  c("Column types", sapply(dat, typeof) %>% unlist()) |> 
    paste(collapse = ", ") |> print()
  
  # Sets date and datetime columns to POSITX type.
  dat <- dat |> mutate(
    across(c(created, updated), as_datetime)
  )
  dat <- dat |> mutate(
    across(publishedDate, as.Date)
  )
  
  # Makes all character columns (excluding 'id', 'dataset', and 'publication') uppercase.
  dat <- dat |> mutate(
    across(c(repository:title, source:fundersROR), toupper)
  )
  
  # Returns data from NIH-funded datasets.
  return(dat[which(
    str_detect(dat$funders, pattern = "(NIH)|(NATIONAL INSTITUTES OF HEALTH)") | 
      str_detect(dat$fundersROR, pattern = "(NIH)|(NATIONAL INSTITUTES OF HEALTH)")
  ),]
  )
}

## FUNCTION: read_datacite - arguments dirpath (string)
## Argument dirpath takes a file path to the directory containing
## the DataCite Corpus files to be read. 
read_datacite <- function(filenames) {
  cl <- makeCluster(8)
  registerDoParallel(cl)
  
  # From each file, get data from NIH-funded datasets, bind rowwise.
  nih_df <- foreach(
    fn = filenames, 
    .combine = 'rbind',
    .export = 'get_nih', 
    .packages = 'tidyverse'
    ) %dopar% {
      get_nih(fn)
    }
  
  stopCluster(cl)
  
  # Return data from NIH-funded datasets only.
  return(nih_df)
}


#### READING IN DATA ####

## TODO: Modify dirpath to current directory
dirpath = "/Users/ashvin/Desktop/biomed-retractions/datacite"

## TODO: Specify name of output file
outfile = "nih_citations.csv"

# Gets the names of files to be read int, then call read_datacite.
files <- list.files(path = paste0(dirpath, "/data"), 
                    full.names = TRUE, recursive = TRUE)
df <- read_datacite(files)

# Writes data from NIH-funded datasets to outfile
write_csv(df, file = file = paste(dirpath, outfile, sep = "/"), quote = "none")
