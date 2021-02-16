syntheses <- readLines(con = "raw_data/5590_4_synthese.csv")

liste <- scinder_syntheses(syntheses = syntheses)

liste <- nommer_liste(liste)

ma_synthese <- liste[1] %>%
  unlist()

# -------------------------------------------------------------
# ecoulements mensuels
# -------------------------------------------------------------

em_txt <- extraire_bloc(synthese = ma_synthese,
                        pattern_debut = "Ecoulements mensuels",
                        nb_lignes = 4)

noms_colonnes <- c('Jan', 'Fév', 'Mars', 'Avr', 'Mai', 'Juin', 'Juil', 'Aout', 'Sept', 'Oct', 'Nov', 'Dec')

noms_lignes <- map(.x = em_txt[2:4],
                   .f = function(x) x[1]) %>%
  unlist()

em <- parser_bloc(bloc = em_txt,
                  noms_colonnes = noms_colonnes,
                  noms_lignes = noms_lignes)

# -------------------------------------------------------------
# modules interannuels
# -------------------------------------------------------------

mi_txt <- extraire_bloc(synthese = ma_synthese,
                        pattern_debut = "Modules interannuels",
                        nb_lignes = 2)

# noms des colonnes. Nb il y a les intervalles de confiance => 3 valeurs / nom
noms_colonnes <- mi_txt[[1]] %>%
  .[.!=''] %>%
  suffixer_colonnes()# suppression des vides

noms_lignes <- map(.x = mi_txt[2:4],
                   .f = function(x) x[1]) %>%
  unlist()

mi <- parser_bloc(bloc = mi_txt,
                  noms_colonnes = noms_colonnes,
                  noms_lignes = noms_lignes)

# données module (moyenne)

mm_txt <- extraire_bloc(synthese = ma_synthese,
                        pattern_debut = "Module (moyenne)",
                        nb_lignes = 0)

noms_colonnes <- mm_txt[[2]][1] %>%
  .[.!=''] %>%
  suffixer_colonnes() # suppression des vides

noms_lignes <- "Débits (m3/s)"

mm <- parser_bloc(bloc = mm_txt,
                  noms_colonnes = noms_colonnes,
                  noms_lignes = noms_lignes)

# -------------------------------------------------------------
# Basses eaux
# -------------------------------------------------------------


