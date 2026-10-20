## Code for merging DataCite Corpus data with data from the
## Retraction Watch Database. Merges both raw DataCite data 
## and NIH-funded datasets (as output by readdata.R).

## Ashvin Narayan (divyanrn@umich.edu), modified 20 October 2026.

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

do_rw_compare <- function(filename) {
  # Read in data from DataCite Corpus
  dc <- read_csv(file = filename, na = c("", "NA", "none")) %>% 
    mutate(across(c(created, updated), as_datetime)) %>%
    mutate(across(publishedDate, as.Date)) %>%
    mutate(across(c(repository:title, source:fundersROR), toupper))
  
  # Read in data from Retraction Watch
  rw <- read_csv(
    "https://raw.githubusercontent.com/ashnryn/article-retractions/refs/heads/main/retraction-watch/retraction_watch.csv",
    na = c("", "unavailable")
    )
    
  # Merge using DOI
  merge_dat <- inner_join(
    x = (dc %>% 
           filter(!is.na(publication)) %>%
           filter(str_detect(publication, pattern = "doi")) %>%
           mutate(publication = str_remove(publication, "https://doi.org/")) %>%
           select(id, repository, title, dataset, publication, source, 
                  funders, fundersROR) %>%
           unnest(publication)),
    y = (rw %>%
           filter(!is.na(OriginalPaperDOI)) %>%
           select(Title, Author, Journal, RetractionDate, OriginalPaperDOI, 
                  RetractionNature, Reason) %>% 
           unnest(OriginalPaperDOI)),
    by = join_by(publication == OriginalPaperDOI),
    relationship = "many-to-many"
  )
  
  return(merge_dat)
}



rw_merge <- function(filenames) {
  cl <- makeCluster(8)
  registerDoParallel(cl)
  
  mergedc <- foreach(
    fn = filenames,
    .combine = "rbind",
    .export = "do_rw_compare",
    .packages = "tidyverse"
  ) %dopar% {
    do_rw_compare(fn)
  }
  
  stopCluster(cl)
  
  return(mergedc)
}

## TODO: Modify dirpath to current directory
dirpath = "/Users/ashvin/Desktop/biomed-retractions/datacite"

## TODO: Specify name of output file
outfile = "merged.csv"

# Gets the names of files to be read int, then calls rw_merge.
files <- list.files(path = paste0(dirpath, "/data"), 
                    full.names = TRUE, recursive = TRUE)

merged <- rw_merge(files)

write_csv(merged, file = paste(dirpath, outfile, sep = "/"), quote = "none")

