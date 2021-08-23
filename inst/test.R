dat <- jsonlite::fromJSON("https://data.geo.admin.ch/ch.bfe.ladestellen-elektromobilitaet/data/oicp/ch.bfe.ladestellen-elektromobilitaet.json")


dat_info <- dat$EVSEData$EVSEDataRecord


ladestationen_coords <- tibble::tibble(
  geocoordinates = purrr::map(dat_info, "GeoCoordinates")
) %>%
  tidyr::unnest("geocoordinates") %>%
  tidyr::separate(Google,c("GKODE","GKODN"),sep=" ",convert=TRUE) %>%
  tibble::rownames_to_column()

test <- gemeindefinder::add_bfsnr(ladestationen_coords, crs = 4326)
