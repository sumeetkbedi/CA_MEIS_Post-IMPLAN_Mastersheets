## Defines function "result_loop" for creating dataframes of the IMPLAN results ##

result_loop <- function(index, var, geo_ind, path1, path2, files) {
  for(i in index){
    var <- rbind(var, data.frame(geo = geo_ind[i], read.xlsx(file.path(path1, path2, files[i]))))
  }
  return(var)
}
