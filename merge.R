## Code for merging DataCite Corpus data with data from the
## Retraction Watch Database. Merges both raw DataCite data 
## and NIH-funded datasets (as output by readdata.R).

## Ashvin Narayan (divyanrn@umich.edu), modified 20 October 2026.

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

do_rw_compare <- function(filename, rw) {
  # Read in data from DataCite Corpus
  dc <- read_csv(file = filename, na = c("", "NA", "none")) %>% 
    mutate(across(c(created, updated), as_datetime)) %>%
    mutate(across(publishedDate, as.Date)) %>%
    mutate(across(c(repository:title, source:fundersROR), toupper))
  
  # Merge using DOI
  merge_dat <- inner_join(
    x = (dc %>% 
           filter(!is.na(publication)) %>%
           filter(str_detect(publication, pattern = "doi")) %>%
           mutate(publication = str_remove(publication, "https://doi.org/")) %>%
           select(id, created, repository, title, dataset, publication, source, 
                  funders, fundersROR) %>%
           unnest(publication)),
    y = (rw %>%
           filter(!is.na(OriginalPaperDOI)) %>% 
           unnest(OriginalPaperDOI)),
    by = join_by(publication == OriginalPaperDOI),
    relationship = "many-to-many"
  )
  
  return(merge_dat)
}



rw_merge <- function(filenames) {
  # load retraction watch data
  retractions = data.frame(read.csv(
    "https://raw.githubusercontent.com/ashnryn/article-retractions/refs/heads/main/retraction-watch/retraction_watch.csv",
    na.strings = c("", "unavailable")
  ))
  
  # keep Record.ID, Title, Journal, Publisher, Author, URLs, ArticleType,
  # RetractionDate, RetractionDOI, OriginalPaperDate, OriginalPaperDOI,
  # RetractionNature, Reason, Notes
  retractions <- retractions %>%
    select(
      "Record.ID",
      "Title",
      "Author",
      "Journal",
      "Publisher",
      "OriginalPaperDate",
      "OriginalPaperDOI",
      "RetractionDate",
      "RetractionDOI",
      "RetractionNature",
      "Reason"
    )
  
  # replace '(&quot;)' with \" quotation marks
  retractions <- retractions %>% 
    mutate(across(c("Title") , ~ gsub('(&quot;)', '\"', .)))
  
  cl <- makeCluster(8)
  registerDoParallel(cl)
  
  mergedc <- foreach(
    fn = filenames,
    .combine = "rbind",
    .export = "do_rw_compare",
    .packages = "tidyverse",
    .verbose = TRUE
  ) %dopar% {
    do_rw_compare(fn, retractions)
  }
  
  stopCluster(cl)
  
  return(mergedc)
}

# Gets the names of files to be read int, then calls rw_merge.
files <- list.files(path = paste0(dirpath, "/data"), 
                    full.names = TRUE, recursive = TRUE)

merged <- rw_merge(files)

colnames(merged) <- c(
  "data.record.id",
  "data.created.date",
  "data.repository",
  "data.title",
  "data.id",
  "data.citation.id",
  "data.citation.source",
  "data.funders",
  "data.funders.ror",
  "pub.record.id",
  "pub.title",
  "pub.author",
  "pub.journal",
  "pub.publisher",
  "pub.date",
  "pub.retraction.date",
  "pub.retraction.doi",
  "pub.retraction.nature",
  "pub.retraction.reason"
)

merged <- merged %>%
  mutate(pub.year = mdy_hm(pub.date) %>% year() %>% as.integer(), 
         .after = pub.date) %>%
  mutate(pub.retraction.year = mdy_hm(pub.retraction.date) %>% year() %>% as.integer(), 
         .after = pub.retraction.date)


## TODO: Modify dirpath to current directory
dirpath = "/Users/ashvin/Desktop/biomed-retractions/datacite"

## TODO: Specify name of output file
outfile = "merged.csv"

write_csv(merged, file = paste(dirpath, outfile, sep = "/"), quote = "none")

