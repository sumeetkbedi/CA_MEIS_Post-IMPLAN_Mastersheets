## Defines function "geo_list" for grabbing all the counties/districts from filenames and dropping it into a list ##

geo_list <- function(pattern, replacement, dataframe) {
  val <- gsub(pattern, replacement, dataframe)
}
