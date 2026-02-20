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

#' Retrieve buoy data for one or more buoys
#' 
#' @export
#' @param x a table of buoy metadata for one or more buoys
#' @param ... other arguments for the [buoydata::get_buoy_data()]
#' @return table
get_buoy = function(x = read_buoy_list() |> dplyr::slice(1:2),
                    ...){
  r = x |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        b = buoydata::get_buoy_data(row$ID, ...) |>
          dplyr::as_tibble() |>
          dplyr::mutate(ID = row$ID, .before = 1)
        return(b)
      }
    ) |>
    dplyr::bind_rows()
  
  return(r)
}
