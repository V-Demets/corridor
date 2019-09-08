#' Fusion de vecteurs, reprojection et découpe au périmètre d'étude.
#'
#' @description Import des données SIG brutes (vecteurs), sélection, fusion et découpe selon le périmètre de la zone d'étude.
#' La sélection et la fusion des couches est paramétrée à travers le classeur 'Parametres_SIG.xlsx'.
#' Une fois les données ainsi triées, les couches reformatées sont réécrites dans le dossier DataProjet.
#'
#' @return La fonction sauvegarde les couches lues dans une archive R et réécrit les couches corrigées dans le dossier DataProjet.
#'
#' @author Bruciamacchie Max, Demets Valentin
#' @param Zone_SHP = shape définissant le périmètre de la zone d'étude. Sécurité si aucun shape choisi
#' @param Buffer_Width = taille du buffer à rajouter au périmètre de la zone d'étude.
#' Valeur par défaut = 1 km.
#' @param ParamSIG_DF = tableau paramétrant les shapes et attributs à découper, fusionner et réécrire.
#' @param Path_DF = tableau listant les chemins d'accès aux différents shapes du dossier de données brutes.
#' @param repDataBrutes = répertoire contenant les données brutes à traiter.
#'
#' @import sp
#' @import rgdal
#' @import raster
#' @import rgeos
#' @import tcltk
#'
#' @export

library(openxlsx)
library(sf)
library(stats) # TODO : à charger avant dplyr (fonction 'filter' et 'lag' masquées sinon)
library(dplyr)
library(tcltk)

# -- fonction de sauvegarde des shapes lus
save_read_shp <- function(shp, id_shp, read_shp) {
  # TODO : voir si possible de supprimer id_shp ?
  
  # separate sf object between id and values (+ geometry) - à voir si scinder en plus de morceaux ?
  values_sf <- 
    shp %>% 
    gather(variable, value, -geometry) %>% 
    mutate(id_sf = id_shp) %>% 
    select(id_sf, variable, value, geometry)# %>% 
  # # TODO : add id for sfg ?
  # group_by(id_sf, geometry) %>% 
  # mutate(id_sfg = 1:length(id_sfg)) %>% 
  # # Erreur : Column `geometry` can't be used as a grouping variable because it's a sfc_POLYGON/sfc
  # ungroup() %>% 
  # mutate(id_sfg = paste0(id_sf, id_sfg))
  id_sf <- 
    values_sf %>% 
    select(id_sf) %>% 
    st_drop_geometry() %>% 
    distinct()
  
  # -- sauvegarde des données dans la liste read_shp
  if (length(read_shp) == 0) {
    read_shp[[1]] <- id_sf
    read_shp[[2]] <- values_sf
  } else {
    read_shp[[1]] <- rbind(read_shp[[1]], id_sf)
    read_shp[[2]] <- rbind(read_shp[[2]], values_sf)
  }
  
  # -- noms de la liste read_shp
  names(read_shp)[1] <- "id_sf"
  names(read_shp)[2] <- "values_sf"
  
  # -- retour de la fonction read_shp
  return(read_shp)
}

rewrite_shp <- function(
  zone, parameter_wb_file, 
  rep
) {
  # zone <- zone # debug # zone = Zone_SHP
  # parameter_wb_file <- parameter_wb_file # debug # parameter_wb_file = ParamSIG_FILE
  # raw_data_rep = rep2 # debug # raw_data_rep = repDataBrutes
  # # buffer_width = 1000 # debug # buffer_width = Buffer_Width TODO : suppress param
  # # Zone_SHP <- Bbox_SHP
  # # ParamSIG_FILE <- df_BASE
  # # ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_bis_V2.xlsx"
  raw_data_rep = file.path(getwd(), "data/raw")
  compiled_data_rep <- sub("raw", "compiled", raw_data_rep) # debug # compiled_data_rep = repDataProjet
  
  # -- tableau pour système de projection manquant (ReecritureShape2.R) # TODO : mettre ces données dans une archive (comme les styles ?)
  EPSG_df <- data.frame(
    Label = c(
      "EPSG:2154 - RGF93/Lambert-93", 
      "EPSG:7421 - NTF(Paris)/Lambert Zone II", 
      "EPSG:4326 - WGS84 (gps)", 
      "EPSG:32632 - UTM 32N (gps téléphone)"
    ), 
    EPSG = c(2154, 7421, 4326, 32632), 
    stringsAsFactors = F
  )
  
  # -- création des dossiers nécessaires
  dir.create(compiled_data_rep, showWarnings = F, recursive = T)
  dir.create("tables", showWarnings = F) # TODO : dossier déjà créé dans write_wb_1

  # ----- Import des paramètres du classeur 'Parametres_SIG.xslx' -----
  parameter_df <- read.xlsx( # parameter_df = ParamSIG_DF
    parameter_wb_file, sheet = "Parametrage_SIG"
    ) %>% 
    filter(Reecrire_SHP == "Oui" | !is.na(Thematique_RAS))
  ########## TODO : changer Id en id, Source en source, etc. ##########
  # shp_df <- 
  #   ParamSIG_DF %>% 
  #   distinct(Id, shp)
  # ListShp_SHP <- shp_df$shp # TODO : étape déjà fait dans ListInfos.R (list_raw_shp)
  # names(ListShp_SHP) <- shp_df$Id
  # TODO : changer Id en id, Source en source, etc.
  ########## / \ ##########
  df <- parameter_df %>% select(Id, Source) %>% arrange(Id) %>% distinct()
  list_shp <- df$Source ; names(list_shp) <- df$Id
  list_shp <- list_shp[c(1:3, which(names(list_shp) == "ID_36"))] # debug

  # ----- Début chaîne travail sur shapes -----
  error_list <- c()
  error_list2 <- c()

  # -- ensemble de sauvegarde des shapes lus
  read_shp <- c()

  # -- barre de progression
  pb <- tkProgressBar(
    title = "Progression", 
    label = "Réécriture des shapes (%)", 
    min = 0, max = 100, width = 500
  )

  # -- boucle de lecture+réécriture des shapes
  for (i in 1:length(list_shp)) {
    # -- paramètres récupérés dans parameter_df :
    # i = 1 # debug
    # print(i) # debug
    name_shp <- unname(list_shp[i])
    name_shp <- sub(".dbf", ".shp", name_shp) # TODO : change .dbf en .shp ?
    name_shp <- basename(name_shp)
    id_shp <- names(list_shp)[i]
    # print(id_shp) # debug
    df <- parameter_df %>% filter(Id %in% id_shp)
    source_shp <- unique(df$Source)
    source_shp <- sub(".dbf", ".shp", source_shp)
    # encoding_shp <- unique(df$Encodage)
    df <- df %>% filter(Reecrire_SHP == "Oui")
    attrs_shp <- unique(df$Attributs)
    union_shp <- unique(df$Union_Champ) %>% na.omit()
    
    # -- sécurité # TODO : renforcer les tests (si id_shp, source ou encoding > 1 ?) -> faire une fonction pour détecter les erreurs ?
    # attrs_shp <- attrs_shp[1] # debug
    if (length(union_shp) > 1) {
      stop(
        "Plus d'un champ indiqué pour fusionner les polygones du shapes pour le fichier :\n\n", 
        source_shp
      )
    }

    ########## TODO : shape déjà lu ##########
    # -- possibilité que shp déjà lu pendant exécution de la fonction CreateRaster()
    # if (id_shp %in% names(ListShp_SHP2)) {
    #   shp <- ListShp_SHP2[[which(names(ListShp_SHP2) %in% id_shp)]]
    # } else {
      # print("Controle passage")
      # -- Lecture # TODO : voir s'il y a un problème possible avec l'encodage ?
    ########## / \ ##########
    shp <- st_read(
      file.path(raw_data_rep, "/", source_shp),
      stringsAsFactors = FALSE, quiet = T
    ) %>% 
      st_transform(crs = 2154) %>%   # TODO : à tester (que faire de la sécurité ci-dessous - Contrôle du système de projection)
      st_zm(drop = T, what = "ZM") # sécurité si 3D shapefile (not supported by st_write)  # TODO : Note : écriture peut bloquer si shapefile est 3D (XYZ)

    # -- crop du shape sur la zone d'emprise
    return <- tryCatch( # return = Return
      # shp[zone, ], # TODO : regarder fonction rapide pour crop (stars ?)
      st_intersection(zone, shp),
      error = function(cond) {
        error_list <- c(error_list, source_shp) # error_list = List_ERROR
        shp <- st_buffer(shp, dist = 0) # permet d'éliminer les erreurs typologiques +
        # argument byid = T permet de garder les données associés TODO : tester avec sf (= plus aucun argument byid)
        # shp <- shp[zone, ]
        st_intersection(zone, shp)
        return(list(shape = shp, error = error_list))
      }
    )

    ########## TODO : revoir sécurité ##########
    # TODO : revoir sécurité
    # if (class(return) == "list" & length(return) == 2) { # impératif de contrôler la classe
    #   shp <- return[[which(names(return) == "shape")]]
    #   error_list <- return[[which(names(return) == "error")]]
    # } else {
      # shp <- return
    # }
    ########## / \ ##########
    shp <- return

    # -- sauvegarde des shapes lus
    # read_shp <- c(read_shp, list(shp)) # read_shp = ListShp_DF # TODO : create a BD (ou GeoPackage ?)
    # names(read_shp)[i] <- id_shp
    read_shp <- save_read_shp(shp, id_shp, read_shp)

    # -- ecriture du shape corrigé/cropped
    # if (length(shp) != 0) { # Contrôle que la couche croise bien au moins 1 fois la zone d'étude
    if (dim(shp)[1] != 0) { 

      ########## TODO : utilité ? ##########
      # if (!is.null(attrs_shp) & length(attrs_shp)) {
      #   # -- Subset des attributs à conserver # TODO : utilité ?
      #   shp <- shp[, attrs_shp]
      # }
      # -- Fusion des shapes selon 1 champ (si indication existe)
      # if (length(union_shp) != 0) { # TODO : à tester
      #   shp_df <- shp@data %>%
      #     distinct_(union_shp, .keep_all = T)
      #   row.names(shp_df) <- as.character(shp_df[, union_shp]) # value sert d'identifiant
      #   # Encoding(shp@data[, union_shp])
      # 
      #   shp <- gUnaryUnion(shp, id = shp@data[, union_shp]) # nécessaire de récupérer le data.frame
      #   shp <- SpatialPolygonsDataFrame(shp, shp_df, match.ID = T) # match les row.names et les ID des polygones
      # }
      ########## / \ ##########

      # -- création des dossiers nécessaires
      save_path <- file.path(compiled_data_rep, dirname(source_shp)) # chemin
      dir.create(
        save_path, showWarnings = F, recursive = T
      )

      # -- Ecriture du shape
      # reptemp <- paste(compiled_data_rep, dir_OUT, sep = "/")
      st_write(
        shp, dsn = save_path, layer = name_shp, 
        driver = "ESRI Shapefile", quiet = T,
        update = TRUE, delete_layer = TRUE
      )
      # st_write(zone, dsn = file.path(save_path, 'nc.gpkg')) # TODO : tests avec objet gpkg
      # st_write(zone, dsn = file.path(save_path, 'nc2.gpkg'), layer = 'zone', quiet = TRUE)
    } else {
      error_list2 <- c(error_list2, source_shp)
    }

    info <- round(i / length(list_shp) * 100)
    setTkProgressBar(
      pb, info, paste0("Réécriture des shapes en cours : (", info, " %)"), 
      paste0(info, "% done")
    )
  }

  # -- bilan des erreurs topologiques (polygones vides ? - TODO : vérifier fonctionnement)
  if (length(error_list) > 0) {
    msg <- tk_messageBox(
      type = "ok", 
      message = paste0(
        "Au cours de la réécriture des shapes, une erreur topologique (polygone vide) a été détecté sur le(s) shape(s) :\n\n", 
        paste0(error_list, collapse = "\n"), 
        "\n\nA contrôler."
      )
    )
  }

  # -- bilan des erreurs d'emprise (polygones hors zone d'étude - TODO : vérifier fonctionnement)
  if (length(error_list2) > 0) {
    msg <- tk_messageBox(
      type = "ok", 
      message = paste0(
        "Erreur rencontrée pour le(s) fichier(s) :\n\n", 
        paste0(error_list2, collapse = "\n"), 
        "\n\nShape(s) situé(s) hors emprise (même après reprojection).\nA contrôler.\nShape(s) non réécrit(s)"
      )
    )
  }
  
  # -- message
  msg <- tk_messageBox(
    type = "ok", 
    message = "Réécriture des shapes terminée."
  )
  close(pb)

  # -- sauvegarde des shapes lus
  save(read_shp, file = "tables/read_shapes.Rdata")
}
