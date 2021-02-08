detach("package:tod", unload=TRUE)
library(tod)
library(tidyverse)

ma_couche <- "prescription_surf"
repertoire_donnees_brutes <- "raw_data/prescription_surf"
fichier_donnees_traitees <- "processed_data/prescription_surf.shp"

tod::ign_urba_tod(couche = ma_couche,
                  xmin = -7,
                  ymin = 47,
                  xmax = -4,
                  ymax = 48,
                  repertoire = rep_donnees_brutes)
# ign_urba_tod(couche = "document")
# ign_urba_tod(couche = "municipality")
# ign_urba_tod(couche = "zone_urba")
# ign_urba_tod(couche = "secteur_cc")
# ign_urba_tod(couche = "prescription_surf")
# ign_urba_tod(couche = "prescription_pct")
# ign_urba_tod(couche = "info_surf")
# ign_urba_tod(couche = "info_lin")
# ign_urba_tod(couche = "info_pct")

ign_urba_dec(repertoire = rep_donnees_brutes)

ign_urba_net_rep(repertoire = rep_donnees_brutes,
                 seuil_ko = 2)

sf_liste <- ign_urba_lire_shapes(repertoire = rep_donnees_brutes)

# essai de visualisation
mapview::mapview(sf_liste[2])

assemblage <- ign_urba_assembler_sf(liste_sf = sf_liste)

mapview::mapview(assemblage)

ign_urba_sauver_shape(objet_sf = assemblage,
                      chemin = fichier_donnees_traitees,
                      scr = 2154)

ma_couche <- "prescription_lin"
repertoire_donnees_brutes <- "raw_data/prescription_lin"
fichier_donnees_traitees <- "processed_data/prescription_lin.shp"

ign_urba_api_shp(couche = ma_couche,
                 xmin = -7,
                 ymin = 47,
                 xmax = -4,
                 ymax = 48,
                 repertoire = repertoire_donnees_brutes,
                 fichier_sortie = fichier_donnees_traitees,
                 scr = 2154)
