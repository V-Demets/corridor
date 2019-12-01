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
  # -- définition du répertoire de travail
  rep_Ilots <- tk_choose.dir( # TODO : rename rep_Ilots as rep
    default = getwd(), 
    caption = "Choix du r\u00E9pertoire de travail"
  )
  setwd(rep_Ilots)

  # -- choix du répertoire contenant les données vecteurs
  rep_SHP <- tk_choose.dir(
    default = "", #getwd(), 
    caption = "Choix du r\u00E9pertoire contenant les donn\u00E9es vecteurs brutes"
  )

  # -- récupération des fichiers contenus dans rep_SHP
  # List_DIRS <- list.dirs(path = rep_SHP)
  # List_FILES <- list.files(path = List_DIRS, full.names = T)
  # pos_rep <- str_locate(List_FILES, rep_SHP) # construction chemin relatif :
  # # supprime le chemin du répertoire de base dans le nom des fichiers
  # List_FILES <- str_sub(List_FILES, pos_rep[, 2] + 2, -1)
  list_raw_files_tree <- list.files(
    path = rep_SHP, 
    all.files = F, 
    recursive = T, 
    include.dirs = T
  ) 

  raw_files_tree <- data.frame( # df1
    dir_source = dirname(list_raw_files_tree), 
    source = list_raw_files_tree, 
    # Fichier = file_path_sans_ext(list_raw_files_tree), 
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
  
  df2 <- data.frame(
    Id = rep.int(raw_files_tree$Id, sapply(dir_source_list, length)), 
    # dir_source = rep.int(raw_files_tree$dir_source, sapply(dir_source_list, length)), # Dir_Source
    source = rep.int(raw_files_tree$source, sapply(dir_source_list, length)), # Source
    # ext = rep.int(raw_files_tree$ext, sapply(dir_source_list, length)), # Extent
    # layer = rep.int(raw_files_tree$layer, sapply(dir_source_list, length)), # Couche
    dir = unlist(dir_source_list), # Dossier
    # niveau = seq()
    stringsAsFactors = F
  ) %>%
    group_by(source) %>%
    mutate(level = 1:length(source)) %>%
    ungroup() %>% 
    select(Id, dir, level) %>% 
    left_join(raw_files_tree, by = "Id")

  # Reproduction et sauvegarde de la hiérarchie entre les fichiers/dossiers dans un data.frame (df3)
  levels <- unique(df2$level) # Niveaux = levels
  df3 <- 
    df2 %>% 
    mutate(DirName = source) %>%
    group_by(source) %>%
    mutate(level_max = max(level)) %>% # Niveau_Max = level_max
    ungroup()
  for (niv in levels[length(levels):1]) {
    df3 <- 
      df3 %>% 
      mutate(
        DirName = ifelse(niv <= level_max, dirname(DirName), DirName), 
        dir = ifelse(level == niv, DirName, dir)
      )
  }
  
  # Création des dossiers nécessaires dans le dossier revised_data:
  repDataProjet <- dirname(rep_SHP) # remonter dans les dossiers pour créer revised_data
  repDataProjet <- paste0(repDataProjet, "/revised_data") # revised_data = DataProjet
  dir.create(repDataProjet, showWarnings = F)

  # ---- N.B : Création des dossiers est reportée dans la fonction ReecritureShape (inutile de
  # -- créer des dossiers pour des shapes qui ne seront pas réécris)

  # setwd(repDataProjet)
  # for (niv in levels) {
  #   Dirs <- filter(df3, level == niv) %>%
  #     dplyr::select(Dossier) %>%
  #     distinct() %>% # idem unique mais plus rapide
  #     unlist()
  #   for (dir in Dirs) {
  #     dir.create(dir, showWarnings = F)
  #   }
  # }

  infos <- list(rep_Ilots, rep_SHP, df3)
  names(infos) <- c("repTravail", "repDataBrutes", "tableau") # TODO : voir si mise au format df vraiment nécessaire -> se contenter de la liste des fichiers ?
  return(infos)
}
