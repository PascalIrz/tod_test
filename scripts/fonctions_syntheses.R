# -------------------------------------------------------------
# fonction interne pour récupérer la synthèse d'une station
# -------------------------------------------------------------

get_1_sta <- function(syntheses, indexes_debut, indexes_fin, i, nom_liste = "liste") {

  sta <- list()

  sta_data <- syntheses[indexes_debut[i]:indexes_fin[i]] #%>%
  # as.list()

  sta[[1]] <- sta_data

  sta_id <- sta_data[[2]] %>%
    strsplit(";") %>%
    .[[1]] %>%
    .[[1]]

  sta[[2]] <- sta_id

  sta

}

yo <- get_1_sta(syntheses = syntheses,
                indexes_debut = indexes_debut,
                indexes_fin = indexes_fin,
                i = 1)
# -------------------------------------------------------------
# fonction pour scinder les synthèses par station
# -------------------------------------------------------------

scinder_syntheses <- function(syntheses) {

  indexes_debut <- syntheses %>%
    stringi::stri_detect_fixed(pattern = "Code station") %>%
    which()

  nb_stations <- length(indexes_debut)

  indexes_fin <- (indexes_debut[2:nb_stations] - 1) %>%
    c(length(syntheses))

  map(.x = 1:nb_stations,
      .f = get_1_sta,
      syntheses = syntheses,
      indexes_debut = indexes_debut,
      indexes_fin = indexes_fin)

}

liste <- scinder_syntheses(syntheses = syntheses)

# -------------------------------------------------------------
# fonction interne de nommage des synthèses par les codes stations
# -------------------------------------------------------------

nommer_liste <- function(liste) {

  noms_liste <- map(.x = liste,
                    .f = function(synthese) synthese[[2]]) %>%
    unlist()

  names(liste) <- noms_liste

  map(.x = liste,
      .f = function(synthese) synthese[1])

}

liste <- nommer_liste(liste)

# -------------------------------------------------------------
# Extraire un bloc de la synthèse d'une station
# Le nb de lignes s'entend ligne de titre déduite
# -------------------------------------------------------------

extraire_bloc <- function(synthese, pattern_debut, nb_lignes) {

  ligne_debut <- synthese %>%
    stringi::stri_detect_fixed(pattern = pattern_debut) %>%
    which()

  data <- synthese[(1 + ligne_debut):(nb_lignes + ligne_debut)] %>%
    str_split(pattern = ";")

}

# em_txt <- extraire_bloc(synthese = ma_synthese,
#                         pattern_debut = "Ecoulements mensuels",
#                         nb_lignes = 4)


# données


# em_data <- em_txt[2:4] %>%
#   str_replace(pattern = '\\s*\\([^\\)]+\\)',
#               replacement = '') %>% # supprimer les unités qui contiennent des chiffres
#   str_split(pattern = ";") %>%
#   str_match_all("[0-9.]+") %>%
#   map(.f = as.data.frame) %>% #,
#   # row.names = noms_colonnes) %>%
#   map(.f = slice,
#       n = -n()) %>%
#   map(.f = t) %>%
#   reduce(rbind) %>%
#   as.data.frame(row.name = FALSE) %>%
#   purrr::set_names(noms_colonnes) %>%
#   mutate(noms_lignes = noms_lignes) %>%
#   column_to_rownames(var = "noms_lignes") %>%
#   mutate_all(as.numeric)
#
#
#
# dm <- dm %>%
#   purrr::set_names(mois) %>%
#   mutate(variable = variables) %>%
#   column_to_rownames(var = "variable") %>%
#   mutate_all(as.numeric)

# -------------------------------------------------------------
# Parser un bloc en une liste d'un df par ligne (fonction interne)
# -------------------------------------------------------------
bloc_to_dfs <- function(bloc) {

  bloc[2:length(bloc)] %>% # récupération données moins la ligne de titres
    str_replace(pattern = '\\s*\\([^\\)]+\\)', # supprimer ce qui est entre parenthèses
                replacement = '') %>% # càd les unités qui contiennent des chiffres
    str_split(pattern = ";") %>% # scission
    str_match_all("[0-9.]+") %>% # récupération des chiffres
    map(.f = as.data.frame) # passage en dataframe

}



# --------------------------------------------------------------
parser_bloc <- function(bloc, noms_colonnes, noms_lignes)

{

  data <- bloc %>%
    bloc_to_dfs()

  if (nrow(data[[1]]) > 12) # cas des données mensuelles + annuelle à la fin

      {

      data <- map(.x = data,
                  .f = slice, # suppression du total annuel
                  n = -n())
      }

  data <- data %>%
    map(.f = t) %>%
    reduce(rbind) %>%
    as.data.frame(row.name = FALSE) %>%
    purrr::set_names(noms_colonnes) %>%
    mutate(nl = noms_lignes)


  data <- data %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    select(-rowname) %>%
    column_to_rownames(var = "nl") %>%
    mutate_all(as.numeric)

}




# -------------------------------------------------------------
# modules interannuels
# -------------------------------------------------------------

ligne_debut <- ma_synthese %>%
  stringi::stri_detect_fixed(pattern = "Modules interannuels") %>%
  which()

data <- ma_synthese[(1 + ligne_debut):(4 + ligne_debut)] %>%
  str_split(pattern = ";")

noms_colonnes <- data[[1]] %>%
  .[.!=''] # suppression des vides

suffixes <- c("est", "min", "max")

noms_colonnes <- map(.x = noms_colonnes,
                     .f = paste,
                     suffixes) %>%
  unlist()


# données mi

mi <- data[2] %>%
  str_replace(pattern = '\\s*\\([^\\)]+\\)',
              replacement = '') %>% # supprimer les unités qui contiennent des chiffres
  str_split(pattern = ";") %>%
  str_match_all("[0-9.]+") %>%
  as.data.frame(row.names = noms_colonnes) %>%
  purrr::set_names("Débit (m3/s)") %>%
  t()

# données module (moyenne)

mm <- data[4] %>%
  str_replace(pattern = '\\s*\\([^\\)]+\\)',
              replacement = '') %>% # supprimer les unités qui contiennent des chiffres
  str_split(pattern = ";") %>%
  str_match_all("[0-9.]+") %>%
  as.data.frame(row.names = c('module moyenne est', 'module moyenne min', 'module moyenne max')) %>%
  purrr::set_names("Débit (m3/s)") %>%
  t()

module <- cbind(mm, mi) %>%
  as.data.frame()

rm(mi, mm)

# -------------------------------------------------------------
# Basses eaux
# -------------------------------------------------------------

ligne_debut <- ma_synthese %>%
  stringi::stri_detect_fixed(pattern = "Ecoulements mensuels") %>%
  which()

data <- ma_synthese[(1 + ligne_debut):(4 + ligne_debut)] %>%
  str_split(pattern = ";")

# mois
# mois <- ma_synthese[1 + ligne_debut] %>%
#   str_split(pattern = ";")

index_mois <- seq(from = 2, to = 24, by = 2)

mois <- data[[1]][index_mois]
variables <- map(.x = data[2:4],
                 .f = function(x) x[1]) %>%
  unlist()

# données

dm <- data[2:4] %>%
  str_replace(pattern = '\\s*\\([^\\)]+\\)',
              replacement = '') %>% # supprimer les unités qui contiennent des chiffres
  str_split(pattern = ";") %>%
  str_match_all("[0-9.]+") %>%
  map(.f = as.data.frame,
      row.names = mois) %>%
  map(.f = slice,
      n = -n()) %>%
  map(.f = t) %>%
  reduce(rbind) %>%
  as.data.frame(row.name = FALSE)


dm <- dm %>%
  purrr::set_names(mois) %>%
  mutate(variable = variables) %>%
  column_to_rownames(var = "variable") %>%
  mutate_all(as.numeric)
