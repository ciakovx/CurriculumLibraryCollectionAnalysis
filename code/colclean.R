library(stringr)
library(ggplot2)
library(RColorBrewer)
library(knitr)

colclean <- function(col, checkouts = 0){
  # The colclean function takes a list as exported from Sierra ILS and returns to the
  # global environment dataframes of 
  #
  # Arg:
  #   col: An exported list from Sierra including the following values at minimum:
  #       - loutdate 
  #       - location 
  #       - tot.chkout
  #
  # Returns: 
  #   To the global environment, returns as many dataframes as there are locations
  #   within the original data. The dataframes will be named after the location codes.
  #   Transformations to each dataframe include:
  #     - column names to lower case
  #     - a new column 'loutmdy' with the time extracted from the loutdate (the last date the item was checked out)
  #     - a new column 'loutyear' with the year extracted from the loutdate
  
  names(col) <- tolower(names(col))
  
  col <- col[col$tot.chkout >= checkouts, ]
  
  # Remove time from out date
  col$loutmdy <- str_sub(col$loutdate, start = 1L, end = 10L)  
  
  # extract year, erase whitespace, put NA values for blank spaces, and convert it to factors
  # alternatively, skip the last step and graph it with discrete values (I did this to play around with the gradient as year)
  col$loutyear <- str_sub(col$loutmdy, start = 7L)
  col$loutyear <- str_trim(col$loutyear)
  suppressWarnings(col$loutyear <- as.integer(col$loutyear))
  #for(i in 1:length(col$loutyear)) if(nchar(col$loutyear[i]) == 0) col$loutyear[i] <- NA
  
  # Convert location to factor
  col$location <- as.factor(col$location)  
  
  # Delete blank pub year
  col$x008.date.one <- str_trim(col$x008.date.one)
  col <- col[!grepl("[[:alpha:]]|[[:punct:]]", col$x008.date.one), ]  # Some dates have alphabetical characters. Get rid of them.
  col$x008.date.one <- as.numeric(col$x008.date.one)
    
    
  # Dealing with screwed up columns--this is a result of the horrendous "repeated field" that is created from Sierra
  # This is a temporary fix to just eliminate rows that were screwed up in the data read. I need to go back and write a function
  # to deal with them later.
  suppressWarnings(col$tot.chkout <- as.integer(col$tot.chkout))
  x <- which(nchar(col$tot.chkout) > 3)  # If there are more than 3 characters here, it's an indication the row is screwed up
  if(length(x) > 0) col <- col[-x, ]
  # Another way of doing it
  #q2 <- which(nchar(col$tot.chkout) > 3)
  #if(length(q2) != 0) col$tot.chkout <- col$tot.chkout[-q2, ]
  
  return(col)
}

split_loc <- function(col){
  # Split the dataframe into lists according to location
  x <- split(col, col$location)  
  y <- lapply(seq_along(x), 
              function(z) {
                x[[z]][order(x[[z]]$tot.chkout, decreasing = TRUE), ]
              })
  y <- lapply(seq_along(x), function(z) as.data.frame(x[[z]]))  # Turn each item in the list into a dataframe
  names(y) <- levels(col$location)  # change the names of each dataframe to the locations gathered from the levels
  list2env(y, envir = .GlobalEnv)  # put the dataframes into the global environment
}