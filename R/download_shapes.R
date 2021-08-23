#' Function to download the swissboundary shapes
#'
#' @export
download_gem_grenzen <- function(dest_dir, which_date = "latest"){
  download_links <- read.csv("https://ogd.swisstopo.admin.ch/resources/ch.swisstopo.swissboundaries3d-un5pTAlL.csv", header = F) %>%
    dplyr::filter(stringr::str_detect(V1, ".shp")) %>%
    rename(download_link = V1)

  parser_string <- "https://data.geo.admin.ch/ch.swisstopo.swissboundaries3d/swissboundaries3d_(.*)/swissboundaries3d_.*.shp.zip"

  file_dates <- download_links %>%
    dplyr::mutate(datum = as.Date(paste0(gsub(parser_string,"\\1",download_link), "-01"), format = "%Y-%m-%d")) %>%
    {if(which_date == "latest")
      dplyr::filter(., datum == max(datum)) else
        dplyr::filter(., datum == as.Date(which_date))
    }


  dest_dir <- paste0(dest_dir, file_dates$datum, "/")

  if(dir.exists(dest_dir)) stop("No new file is available")

  dir.create(dest_dir, recursive = T)

  zip_dir <- paste0(dest_dir,"data.zip")

  download.file(file_dates$download_link, zip_dir)
  unzip(zip_dir, exdir = dest_dir)
  unlink(zip_dir)
}


