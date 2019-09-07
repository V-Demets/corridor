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


ReecritureShape <- function(
  Zone_SHP, 
  ParamSIG_FILE, 
  repDataBrutes = paste0(getwd(), "/Data/SIG/Vecteurs/DataBrutes"), 
  # Buffer_Width = 1000, 
  Path_DF
) {
  Zone_SHP <- zone # debug # zone = Zone_SHP
  ParamSIG_FILE <- parameter_wb_file # debug # parameter_wb_file = ParamSIG_FILE
  raw_data_rep = rep2 # debug # raw_data_rep = repDataBrutes
  # buffer_width = 1000 # debug # buffer_width = Buffer_Width TODO : suppress param
  # Zone_SHP <- Bbox_SHP
  # ParamSIG_FILE <- df_BASE
  # ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_bis_V2.xlsx"
  compiled_data_rep <- sub("raw", "compiled", rep2) # debug # compiled_data_rep = repDataProjet
  
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
  # ParamSIG_DF <- df_BASE
  parameter_df <- read.xlsx( # parameter_df = ParamSIG_DF
    parameter_wb_file, sheet = "Parametrage_SIG"
    ) %>% 
    filter(Reecrire_SHP == "Oui" | !is.na(Thematique_RAS))
  # shp_df <- 
  #   ParamSIG_DF %>% 
  #   distinct(Id, shp)
  # ListShp_SHP <- shp_df$shp # TODO : étape déjà fait dans ListInfos.R (list_raw_shp)
  # names(ListShp_SHP) <- shp_df$Id
  # TODO : changer Id en id, Source en source, etc.
  df <- parameter_df %>% select(Id, Source) %>% arrange(Id) %>% distinct()
  shp_list <- df$Source ; names(list_shp) <- df$Id
  list_shp <- list_shp[1:3] # debug
  # shp_list = ListShp_SHP

  # ----- Import des chemins relatifs des différentes couches SIG -----
  # Path_DF <- res[[3]]

  # ----- Début chaîne travail sur shapes -----
  error_list <- c()
  error_list2 <- c()
  # ListShp_SHP3 <- c() # List_SHP3 accueille tous les shapes lus (y compris ceux de ListShp_SHP2)

  read_shp <- c()

  pb <- tkProgressBar(
    title = "Progression", 
    label = "Réécriture des shapes (%)", 
    min = 0, max = 100, width = 500
  )

  for (i in 1:length(list_shp)) {
    # -- paramètres récupérés dans Path_DF et parameter_df :
    # Lecture du shape :
    # i = 1 # debug
    shp_name <- unname(list_shp[i])
    shp_name <- sub(".dbf", ".shp", shp_name)
    shp_name <- basename(shp_name)
    shp_id <- names(list_shp)[i]
    df0 <- parameter_df %>% filter(Id %in% shp_id)
    shp_source <- unique(df0$Source)
    shp_source <- sub(".dbf", ".shp", shp_source)
    shp_encoding <- unique(df0$Encodage)
    df0 <- df0 %>% filter(Reecrire_SHP == "Oui")
    shp_attrs <- unique(df0$Attributs)
    shp_union <- unique(df0$Union_Champ) %>% na.omit()
    
    # -- sécurité # TODO : renforcer les tests (si shp_id, source ou encoding > 1 ?) -> faire une fonction pour détecter les erreurs ?
    # shp_attrs <- shp_attrs[1] # debug
    if (length(shp_union) > 1) {
      stop(
        "Plus d'un champ indiqué pour fusionner les polygones du shapes pour le fichier :\n\n", 
        shp_source
      )
    }

    # -- Possibilité que shp déjà lu pendant exécution de la fonction CreateRaster()
    # if (shp_id %in% names(ListShp_SHP2)) {
    #   shp <- ListShp_SHP2[[which(names(ListShp_SHP2) %in% shp_id)]]
    # } else {
      # print("Controle passage")
      # -- Lecture # TODO : voir s'il y a un problème possible avec l'encodage ?
    shp <- st_read(
      file.path(raw_data_rep, "/", shp_source),
      stringsAsFactors = FALSE, quiet = T
    ) %>% 
      st_transform(crs = 2154) %>%   # TODO : à tester (que faire de la sécurité ci-dessous - Contrôle du système de projection)
      st_zm(drop = T, what = "ZM") # sécurité si 3D shapefile (not supported by st_write)  # TODO : Note : écriture peut bloquer si shapefile est 3D (XYZ)
      # if (is.na(shp_encoding)) {
      #   shp <- readOGR(dsn = paste0(repDataBrutes, "/", dirname(shp_source)),
      #                  layer = file_path_sans_ext(basename(shp_source)),
      #                  verbose = F,
      #                  stringsAsFactors = F)
      # } else {
      #   shp <- readOGR(dsn = paste0(repDataBrutes, "/", dirname(shp_source)), 
      #                  layer = file_path_sans_ext(basename(shp_source)), 
      #                  verbose = F, 
      #                  stringsAsFactors = F, 
      #                  use_iconv = TRUE, encoding = shp_encoding)
      # }


    # # -- Sauvegarde des tables attributaires
    # ListShp_DF <- c(ListShp_DF, list(shp@data))
    # names(ListShp_DF)[i] <- shp_id

      # -- Contrôle du système de projection (Lambert 93 demandé)
      # if (is.na( st_crs(zone)["proj4string"] )) {
      #   msg <- tk_messageBox(
      #     type = "ok", 
      #     message = paste0(
      #       "Aucun système de projection renseigné pour le shape :\n\n", 
      #       shp_source, 
      #       "\n\nCliquer sur OK et choisir le système de projection parmi la sélection proposée"
      #     )
      #   )
      #   answ <- tk_select.list(
      #     title = "Choix du système de projection", 
      #     choices = EPSG_df$Label,# EPSG_df = df_EPSG
      #     preselect = "EPSG:2154 - RGF93/Lambert-93", 
      #     multiple = F
      #   )
      #   EPSG <- with(EPSG_df, EPSG[match(answ, Label)] )
      #   # proj4string(shp) <- CRS(paste0("+init = epsg:", EPSG)) # Attribution d'un système de projection au shp
      #   shp <- shp %>% st_transform(crs = EPSG)
      # }
    # }

    # ----- Reprojection et crop -----
    # TODO : suppress
    # shp <- spTransform(shp, 
    #                    CRS("+init = epsg:2154"))

    # TODO : suppress
    # Return <- tryCatch(shp[zone, ], 
    #                    error = function(cond) {
    #                      List_ERROR <- c(List_ERROR, 
    #                                      shp_source)
    #                      shp <- gBuffer(shp, byid = TRUE, width = 0) # permet d'éliminer les erreurs typologique +
    #                      # argument byid = T permet de garder les données associés
    #                      shp <- shp[zone, ]
    #                      return(list(Shape = shp, 
    #                                  Error = List_ERROR))
    #                    })
    return <- tryCatch( # return = Return
      shp[zone, ], # TODO : regarder fonction rapide pour crop (stars ?)
      error = function(cond) {
        error_list <- c(error_list, shp_source) # error_list = List_ERROR
        shp <- st_buffer(shp, dist = 0) # permet d'éliminer les erreurs typologiques +
        # argument byid = T permet de garder les données associés TODO : tester avec sf (= plus aucun argument byid)
        shp <- shp[zone, ]
        return(list(shape = shp, error = error_list))
      }
    )

    # TODO : revoir sécurité
    # if (class(return) == "list" & length(return) == 2) { # impératif de contrôler la classe
    #   shp <- return[[which(names(return) == "shape")]]
    #   error_list <- return[[which(names(return) == "error")]]
    # } else {
      shp <- return
    # }

    # -- Sauvegarde des tables attributaires
    # ListShp_DF <- c(ListShp_DF, list(shp@data))
    # names(ListShp_DF)[i] <- shp_id
    read_shp <- c(read_shp, shp) # shp_list = ListShp_DF
    names(read_shp)[i] <- shp_id

    # ----- Ecriture du shape corrigé -----
    # if (length(shp) != 0) { # Contrôle que la couche croise bien au moins 1 fois la zone d'étude
    if (dim(shp) != 0) { 

      # if (!is.null(shp_attrs) & length(shp_attrs)) {
      #   # -- Subset des attributs à conserver # TODO : utilité ?
      #   shp <- shp[, shp_attrs]
      # }
      # -- Fusion des shapes selon 1 champ (si indication existe)
      # if (length(shp_union) != 0) { # TODO : à tester
      #   shp_df <- shp@data %>%
      #     distinct_(shp_union, .keep_all = T)
      #   row.names(shp_df) <- as.character(shp_df[, shp_union]) # value sert d'identifiant
      #   # Encoding(shp@data[, shp_union])
      # 
      #   shp <- gUnaryUnion(shp, id = shp@data[, shp_union]) # nécessaire de récupérer le data.frame
      #   shp <- SpatialPolygonsDataFrame(shp, shp_df, match.ID = T) # match les row.names et les ID des polygones
      # }



      # -- Création des dossiers nécessaires
      # # setwd(compiled_data_rep)
      # df <- Path_DF %>% filter(Id %in% shp_id)
      # Niveaux <- df$Niveau
      # dir_OUT <- unique(df$dir_source)
      # for (niv in Niveaux) {
      #   # niv <- Niveaux[1]
      #   Dirs <- filter(df, Niveau == niv) %>%
      #     dplyr::select(dir) %>%
      #     distinct() %>% # idem unique mais plus rapide
      #     unlist()
      #   for (dir in Dirs) {
      #     # dir <- Dirs[1]
      #     dir.create(paste0(compiled_data_rep, "/", dir), showWarnings = F)
      #   }
      # }
      # -- création des dossiers nécessaires
      save_path <- file.path(compiled_data_rep, dirname(shp_source)) # chemin
      dir.create(
        save_path, showWarnings = F, recursive = T
      )

      # -- Ecriture du shape
      # reptemp <- paste(compiled_data_rep, dir_OUT, sep = "/")
      st_write(
        shp, dsn = save_path, layer = shp_name, 
        driver = "ESRI Shapefile",
        delete_layer = T, delete_dsn = T
      )
      # writeOGR(shp, dsn = save_path, layer = shp_name, 
      #          driver = "ESRI Shapefile", encoding = "UTF-8", overwrite_layer = T)

      # Sauvegarde des shapes lus et validés :
      # ListShp_SHP3 <- c(ListShp_SHP3, shp) # TODO : à supprimer. déjà sauvegardé avec read_shp
      # names(ListShp_SHP3)[length(ListShp_SHP3)] <- shp_id # TODO : à supprimer. déjà sauvegardé avec read_shp
    } else {
      error_list2 <- c(error_list2, shp_source)
    }

    info <- round(i / length(shp_list) * 100)
    setTkProgressBar(
      pb, info, paste0("Réécriture des shapes en cours : (", info, " %)"), 
      paste0(info, "% done")
    )
  }

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
  msg <- tk_messageBox(
    type = "ok", 
    message = "Réécriture des shapes terminée."
  )
  close(pb)

  # # ----- Extraction et Sauvegarde des tables attributaires
  # ListShp_DF <- c()
  # for (i in 1:length(ListShp_SHP3)) {
  #   shp <- ListShp_SHP3[[i]]
  #   id_shp <- names(ListShp_SHP3)[i]
  #   print(id_shp)
  #   ListShp_DF <- c(ListShp_DF, list(shp@data))
  #   names(ListShp_DF)[i] <- id_shp
  # }

  # # Sauvegarde des shapes lus :
  # save(ListShp_SHP3, 
  #      file = "Tables/Shapes_ReecritureShape2.RData")
  # # Sauvegarde des tables attributaires des shapes lus :
  # save(ListShp_DF, 
  #      file = "Tables/ShapesDF_ReecritureShape2.RData")
  # -- sauvegarde des shapes lus
  save(read_shp, file = "tables/read_shapes.Rdata")
}
