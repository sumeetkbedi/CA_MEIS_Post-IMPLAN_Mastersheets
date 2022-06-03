## Defines function "f_list" for grabbing all the IMPLAN result files with certain data and dropping it into a list ##

f_list <- function(path, pattern) {
  val <- list.files(path, pattern)
}