## Defines function "f_index" for defining indices for files which are either "regular" or "inverse" model results ##

f_index <- function(pattern, dataframe) {
  val <- grep(pattern, dataframe)
}
