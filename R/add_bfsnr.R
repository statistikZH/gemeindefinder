#' add bfsnr to geocoordinates
#'
#' @export
add_bfsnr <- function(df, crs, datum = "latest"){
  if(datum == "latest"){
    existing_dirs <- list.dirs("inst/swissboundaries", recursive = F)

    max_datum <-  max(as.Date(gsub("inst/swissboundaries/(.*)", "\\1", existing_dirs)))

    file_path <- paste0("inst/swissboundaries/", max_datum,"/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
  }else{
    file_path <- paste0("inst/swissboundaries/", datum,"/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
  }

  if(!("GKODE" %in% names(df) & "GKODN" %in% names(df))){
    stop("make sure, that the coordinate columns are named as GKODE and GKODN")
  }

  df_sf <- sf::st_as_sf(
      df,
      coords = c("GKODN", "GKODE"),
      crs = crs,
      agr = "constant"
    ) %>%
    sf::st_transform(crs = 2056)

  map <- sf::read_sf(file_path) %>%
    dplyr::select(NAME, BFS_NUMMER)

  df_bfsnr <- sf::st_intersection(df_sf, map) %>%
    sf::st_drop_geometry()

  return(df_bfsnr)
}
