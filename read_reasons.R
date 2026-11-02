#### read_reasons.R ####

## Code for parsing retraction reasons as given in the Retraction 
## Watch Database. Includes a function to parse a single retraction 
## reasons string (count_reasons_row) and a function vectorizing
## count_reasons_row across a column.

#### FUNCTION: count_reasons_row - string str, optional dataframe rtab ####
## Takes string (semicolon-separated list) of retraction reasons, initializes/updates 
## rtab as a dataframe of reasons and the number of papers that list said reasons
## as reasons for retraction. Retraction reasons are not mutually exclusive.
count_reasons_row <- function(str, rtab = NULL) {
  rvec <- str_split(str, pattern = ';', simplify = TRUE)
  # Drop trailing whitespace
  rvec <- rvec[-length(rvec)]
  
  # Check whether each reason is present in the table,
  # if not, add to table.
  for (item in rvec) {
    if (is.null(rtab)) {
      rtab <- data.frame(reason = item, n = 1,
                         stringsAsFactors = FALSE)
    }
    else if (!any(str_equal(rtab$reason, item))) {
      rtab <- rbind(rtab, c("reason" = item, "n" = 1))
    }
    # If it is present, add 1 to count for that reason
    else {
      rtab$n[which(str_equal(rtab$reason, item))] = 
        as.integer(rtab$n[which(str_equal(rtab$reason, item))]) + 1
    }
  }
  colnames(rtab) = c("reason", "n")
  return(rtab)
}

#### FUNCTION: count_reasons - string vector strvec, optional dataframe reason_cts ####
## Vectorizes count_reasons_row, continuously modifying a table of retraction 
## reasons using count_reasons_row.
count_reasons <- function(strvec, reason_cts = NULL) {
  for (elt in strvec) {
    reason_cts <- count_reasons_row(elt, reason_cts)
  }
  
  # Update column type of 'n', arrange rows in alphabetical order
  reason_cts$n = sapply(reason_cts$n, as.integer)
  reason_cts <- reason_cts %>% arrange(reason)
  
  return(reason_cts)
}