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

#### FUNCTION: update_crosstab - string str, string vector reasons_list, ####
#### optional matrix rmat
## Assumes str is a semicolon-separated list of retraction reasons, and that
## reasons_list is the list of all occurring retraction reasons, and all 
## retraction reasons in str are contained in reasons_list. Optional matrix 
## rmat is a table of co-occurrences.
update_crosstab <- function(str, reasons_list, rmat = NULL) {
  rvec <- str_split(str, pattern = ';', simplify = TRUE)
  # Drop trailing whitespace
  rvec <- rvec[-length(rvec)]
  # Initialize rmat if null
  if (is.null(rmat)) {
    rmat <- matrix(0, 
                   nrow = length(reasons_list), 
                   ncol = length(reasons_list))
  }
  
  # Add 1 for each co-occurrence - includes co-occurrence w/ self
  for (elt1 in rvec) {
    for (elt2 in rvec) {
      rmat[which(reasons_list == elt1), which(reasons_list == elt2)] = 
        rmat[which(reasons_list == elt1), which(reasons_list == elt2)] + 1
    }
  }
  
  return(rmat)
}

#### FUNCTION: reasons_crosstab - string vector strvec, string vector reasons, ####
## Vectorizes update_crosstab. Assumes strings in strvec are semicolon-separated 
## lists of retraction reasons, and that all retraction reasons listed in strvec 
## strings are also listed in reasons.
reasons_crosstab <- function(strvec, reasons) {
  reason_ctab = NULL
  for (str in strvec) {
    reason_ctab <- update_crosstab(str, reasons, reason_ctab)
  }
  
  # Rename rows and columns
  reason_ctab <- data.frame(reason_ctab, row.names = reasons)
  colnames(reason_ctab) <- reasons
  
  return(reason_ctab)
}

#### FUNCTION: crosstab_proportions - matrix or dataframe arguments cross, totals ####
## Uses totals dataframe to convert counts to proportions. Assumes totals provides
## total/topline occurrence of retraction reasons, and cross provides co-occurrence
## of retraction reasons.
crosstab_proportions <- function(cross, totals) {
  cross_prop <- matrix(nrow = nrow(cross),
                       ncol = ncol(cross),
                       dimnames = list(totals$reason, totals$reason))
  
  for (elt in totals$reason) {
    cross_prop[,elt] <- 
      cross[,elt] / totals$n[which(str_equal(totals$reason, elt))]
  }
  cross_prop <- data.frame(cross_prop, row.names = rownames(totals))
  colnames(cross_prop) <- rownames(totals)
  
  return(cross_prop)
}







