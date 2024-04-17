## Defines function "gsub_loop" for cleaning up IMPLAN results data ##

gsub_loop <- function(df, col_range) {
  for(i in col_range){
    df[,i] <- gsub("\\$", "",
                   gsub(",", "",
                        gsub("[(]", "-",
                             gsub("[)]", "", as.character(df[,i])))))
  }
  return(df)
}
