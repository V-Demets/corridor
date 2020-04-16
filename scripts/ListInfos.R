suppressMessages(library(tcltk))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))

#' Liste des couches sous format shp.
#'
#' @description Import des données brutes par département, fusion et découpe selon le périmètre de la zone d'étude.
#'
#' @return La fonction exporte un data frame contenant le liste des shapes avec leurs adresses ainsi que
#' l'adresse du répertoire contenant ces informations.
#'
#' @author Bruciamacchie Max, Demets Valentin
#'
#' @import tcltk
#' @import stringr
#' @import tools
#' @import dplyr
#' @import reshape2
#'
#' @export

############### Liste des couches vectorielles ###############
ListInfos <- function() {
  # -- choix du répertoire contenant les données vecteurs
  raw_data_dir <- tk_choose.dir(
    default = "", #getwd(), 
    caption = "Choix du r\u00E9pertoire contenant les donn\u00E9es vecteurs brutes"
  )

  # -- récupération des fichiers contenus dans raw_data_dir
  list_raw_files_tree <- list.files(
    path = raw_data_dir, 
    all.files = F, 
    recursive = T, 
    include.dirs = T
  ) 

  raw_files_tree <- data.frame( 
    dir_source = dirname(list_raw_files_tree), 
    source = list_raw_files_tree, 
    ext = str_sub(list_raw_files_tree, -4, -1), 
    stringsAsFactors = F
  ) %>%
    filter(ext == ".shp") %>%
    mutate(
      layer = basename(source), 
      layer = str_sub(layer, 1, -5)
    ) %>%
    arrange(layer) %>%
    mutate(
      Id = factor(source), 
      Id = as.numeric(Id), 
      Id = paste0("ID_", Id)
    )

  dir_source_list <- str_split(raw_files_tree$dir_source, "/") # List
  
  files_tree <- data.frame(
    Id = rep.int(raw_files_tree$Id, sapply(dir_source_list, length)), 
    source = rep.int(raw_files_tree$source, sapply(dir_source_list, length)), 
    dir = unlist(dir_source_list), 
    stringsAsFactors = F
  ) %>%
    group_by(source) %>%
    mutate(level = 1:length(source)) %>%
    ungroup() %>% 
    select(Id, dir, level) %>% 
    left_join(raw_files_tree, by = "Id")

  # reproduction et sauvegarde de la hiérarchie entre les fichiers/dossiers dans un data.frame (df3)
  levels <- unique(files_tree$level) # Niveaux = levels
  files_tree <- 
    files_tree %>% 
    mutate(dir_name = source) %>%
    group_by(source) %>%
    mutate(level_max = max(level)) %>% # Niveau_Max = level_max
    ungroup()
  for (niv in levels[length(levels):1]) {
    files_tree <- 
      files_tree %>% 
      mutate(
        dir_name = ifelse(niv <= level_max, dirname(dir_name), dir_name), 
        dir = ifelse(level == niv, dir_name, dir)
      )
  }
  
  # Création des dossiers nécessaires dans le dossier revised_data:
  # rep_data <- dirname(raw_data_dir) # remonter dans les dossiers pour créer revised_data
  revised_data_dir <- file.path( dirname(raw_data_dir) , "revised_data") # revised_data = DataProjet
  dir.create(revised_data_dir, showWarnings = F)

  # ---- N.B : Création des dossiers est reportée dans la fonction ReecritureShape (inutile de
  # -- créer des dossiers pour des shapes qui ne seront pas réécris)
  infos <- list(raw_data_dir, files_tree)
  names(infos) <- c("raw_data_dir", "files_tree") # TODO : voir si mise au format df vraiment nécessaire -> se contenter de la liste des fichiers ?
  return(infos)
}
