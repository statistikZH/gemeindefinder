#' add bfsnr to geocoordinates
#'
#' @param df dataframe containing the coordinates which should be mapped to a bfsnr
#'
#' @param crs epsg number of the projection of the dataset
#'
#' @param datum datum as string, used to define which data should be used for the matching
#'
#' @export
add_bfsnr <- function(df, crs, datum = "latest"){
  if(datum == "latest"){
    existing_dirs <- list.dirs(system.file("swissboundaries", package = "gemeindefinder"), recursive = F)

    max_datum <-  max(as.Date(gsub(".*/swissboundaries/(.*)", "\\1", existing_dirs)))

    file_path <- paste0("swissboundaries/", max_datum,"/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
  }else{
    file_path <- paste0("swissboundaries/", datum,"/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
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

  map_path <- system.file(file_path, package = "gemeindefinder")

  map <- sf::read_sf(map_path) %>%
    dplyr::select(NAME, BFS_NUMMER)

  df_bfsnr <- sf::st_intersection(df_sf, map) %>%
    sf::st_drop_geometry()

  return(df_bfsnr)
}
