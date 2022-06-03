## Defines function "result_loop" for creating dataframes of the IMPLAN results in the counties data ##

result_loop <- function(index, var, geo, geo_ind, files) {
  for(i in index){
    var <- rbind(var, data.frame(geo = geo_ind[i], read_excel(files[i])))
  }
}
