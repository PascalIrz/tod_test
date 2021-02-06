library(tod)
req_api_geo_urba(couche = "wfs_du:prescription_lin")

req_api_geo_urba(couche = "wfs_du:document")

req_api_geo_urba(couche = "wfs_du:municipality")

req_api_geo_urba(couche = "wfs_du:zone_urba")

req_api_geo_urba(couche = "wfs_du:secteur_cc")

req_api_geo_urba(couche = "wfs_du:prescription_surf")

req_api_geo_urba(couche = "wfs_du:prescription_pct")

req_api_geo_urba(couche = "wfs_du:info_surf")

req_api_geo_urba(couche = "wfs_du:info_lin")

req_api_geo_urba(couche = "wfs_du:info_pct")

# assemblage ; cf idées sur https://www.r-bloggers.com/2019/03/open-and-merge-multiple-shapefiles/
# décompression dans des sous-répertoires
repo <- "raw_data/prescription_lin"
path_files_to_unzip <- list.files(path = repo,
                                  pattern = ".zip$",
                                  full.names = TRUE)

dest_repo_names <- paste0(repo, "/", list.files(path = repo,
                                                pattern = ".zip$")) %>%
  str_sub(end = -5)

# décompression
walk2(.x = path_files_to_unzip,
      files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE,
      .y = dest_repo_names,
      .f = unzip)

# suppression des répertoires de shapefiles vides (sinon ils bloquent l'étape suivante)
shapefiles <- list.files(repo,
                         pattern = ".shp$",
                         recursive = TRUE,
                         full.names = TRUE)

for (file in shapefiles)

{

  if(file.size(file) < 500) # shapefile vide

  {

    dir <- file %>%
      str_sub(start = 1, end = str_locate_all(., pattern = "/") %>%
                unlist() %>%
                max() %>%
                `-`(1))
    unlink(dir, recursive = TRUE)

  }

}

# liste des shapefiles
shapefiles <- list.files(repo,
                         pattern = ".shp$",
                         recursive = TRUE,
                         full.names = TRUE)

# lecture avec rgdal qui gère mieux l'encodage que sf
sf_list <- map(.x = shapefiles,
               .f = rgdal::readOGR,
               use_iconv = TRUE,
               encoding = "UTF-8")

# passage en sf
sf_list <- map(.x = sf_list,
               .f = sf::st_as_sf)

# essai de visualisation
mapview::mapview(sf_list[8])
ggplot(data=sf_list[1] %>% fortify()) + geom_sf()
