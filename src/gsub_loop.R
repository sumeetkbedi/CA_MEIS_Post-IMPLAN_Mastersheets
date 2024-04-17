## Defines function "gsub_loop" for cleaning up IMPLAN results data ##

gsub_loop <- function(df) {
  for(i in 3:10){
    df[,i] <- gsub("\\$", "",
                   gsub(",", "",
                        gsub("[(]", "-",
                             gsub("[)]", "", as.character(df[,i])))))
  }
  return(df)
}
