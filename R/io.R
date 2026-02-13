#' Read a list of buoy info, and give the user the option to make it a spatial
#' object.
#' 
#' @export
#' @param form one of "table" or spatial table ("sf")
#' @return table or an sf table
read_buoy_list = function(form = c("table", "sf")[1]){
  
  x  = buoydata::buoy_data |>
    dplyr::as_tibble()
  
  if(tolower(form[1]) == "sf"){
    x = sf::st_as_sf(x,
                     coords = c("LON", "LAT"),
                     crs = 4326)
  }
  return(x)
}
