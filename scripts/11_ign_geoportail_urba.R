detach("package:tod", unload=TRUE)
library(tod)
library(tidyverse)

tod::ign_urba_tod(couche = "prescription_lin",
                  xmin = -7,
                  ymin = 47,
                  xmax = -4,
                  ymax = 48,
                  repertoire = "raw_data/prescription_lin")
# ign_urba_tod(couche = "wfs_du:document")
# ign_urba_tod(couche = "wfs_du:municipality")
# ign_urba_tod(couche = "wfs_du:zone_urba")
# ign_urba_tod(couche = "wfs_du:secteur_cc")
# ign_urba_tod(couche = "wfs_du:prescription_surf")
# ign_urba_tod(couche = "wfs_du:prescription_pct")
# ign_urba_tod(couche = "wfs_du:info_surf")
# ign_urba_tod(couche = "wfs_du:info_lin")
# ign_urba_tod(couche = "wfs_du:info_pct")

# repo <- "raw_data/prescription_surf"
ign_urba_dec(repertoire = "raw_data/prescription_lin")

ign_urba_net_rep(repertoire = "raw_data/prescription_lin",
                 seuil_ko = 2)

sf_liste <- ign_urba_lire_shapes(repertoire = "raw_data/prescription_lin")

# essai de visualisation
mapview::mapview(sf_liste[1])

assemblage <- ign_urba_assembler_sf(liste_sf = sf_liste)

mapview::mapview(assemblage)

ign_urba_sauver_shape(objet_sf = assemblage,
                      chemin = "processed_data/prescription_lin.shp")
