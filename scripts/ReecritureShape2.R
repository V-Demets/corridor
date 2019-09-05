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


ReecritureShape <- function(Zone_SHP,
                            ParamSIG_FILE,
                            repDataBrutes=paste0(getwd(),"/Data/SIG/Vecteurs/DataBrutes"),
                            Buffer_Width=1000,
                            Path_DF) {
  # Zone_SHP <- Bbox_SHP
  # ParamSIG_FILE <- df_BASE
  # ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_bis_V2.xlsx"
  repDataProjet <- paste0(dirname(repDataBrutes),"/DataProjet")
  dir.create(repDataProjet,
             showWarnings=F)
  dir.create("Tables",
             showWarnings=F)


  # ----- Import des paramètres du classeur 'Parametres_SIG.xslx' -----
  # ParamSIG_DF <- df_BASE
  ParamSIG_DF <- read.xlsx(ParamSIG_FILE,sheet="Parametrage_SIG")
  ParamSIG_DF <- filter(ParamSIG_DF,
                        Reecrire_SHP=="Oui" | !is.na(Thematique_RAS))
  shp_df <- distinct(ParamSIG_DF,
                     Id,shp)
  ListShp_SHP <- shp_df$shp
  names(ListShp_SHP) <- shp_df$Id

  # ----- Import des chemins relatifs des différentes couches SIG -----
  # Path_DF <- res[[3]]

  # ----- Début chaîne travail sur shapes -----
  List_ERROR <- c()
  List_ERROR2 <- c()
  ListShp_SHP3 <- c() # List_SHP3 accueille tous les shapes lus (y compris ceux de ListShp_SHP2)

  ListShp_DF <- c()

  pb <- tkProgressBar("Progression", "Réécriture des shapes (%)",
                      0, 100, width=500)

  for (i in 1:length(ListShp_SHP)) {

    # Paramètres récupérés dans Path_DF et ParamSIG_DF :
    # Lecture du shape :
    # i=1
    shp_name <- unname(ListShp_SHP[i])
    shp_id <- names(ListShp_SHP)[i]
    shp_source <- unique(ParamSIG_DF$Source[ParamSIG_DF$Id %in% shp_id])
    print(shp_source)
    shp_encoding <- unique(ParamSIG_DF$Encodage[ParamSIG_DF$Id %in% shp_id])
    shp_attrs <- unique(ParamSIG_DF$Attributs[which(ParamSIG_DF$Id %in% shp_id &
                                                ParamSIG_DF$Reecrire_SHP=="Oui")])
    # shp_attrs <- shp_attrs[1]
    shp_union <- unique(ParamSIG_DF$Union_Champ[ParamSIG_DF$Id %in% shp_id &
                                                  ParamSIG_DF$Reecrire_SHP=="Oui" &
                                                  !is.na(ParamSIG_DF$Union_Champ)])
    if (length(shp_union) > 1) {
      stop(paste0("Plus d'un champ indiqué pour fusionner les polygones du shapes pour le fichier :\n\n",
                  shp_source))
    }

    # -- Possibilité que shp déjà lu pendant exécution de la fonction CreateRaster()
    # if (shp_id %in% names(ListShp_SHP2)) {
    #   shp <- ListShp_SHP2[[which(names(ListShp_SHP2) %in% shp_id)]]
    # } else {
      # print("Controle passage")
      # -- Lecture
      if (is.na(shp_encoding)) {
        shp <- readOGR(dsn=paste0(repDataBrutes,"/",dirname(shp_source)),
                       layer=file_path_sans_ext(basename(shp_source)),
                       verbose=F,
                       stringsAsFactors=F)
      } else {
        shp <- readOGR(dsn=paste0(repDataBrutes,"/",dirname(shp_source)),
                       layer=file_path_sans_ext(basename(shp_source)),
                       verbose=F,
                       stringsAsFactors=F,
                       use_iconv = TRUE, encoding = shp_encoding)
      }


    # # -- Sauvegarde des tables attributaires
    # ListShp_DF <- c(ListShp_DF,list(shp@data))
    # names(ListShp_DF)[i] <- shp_id

      # -- Contrôle du système de projection (Lambert 93 demandé)
      if (is.na(proj4string(shp))) {
        Msg1 <- tk_messageBox(type="ok",
                              message=paste0("Aucun système de projection renseigné pour le shape :\n\n",
                                             shp_source,
                                             "\n\nCliquer sur OK et choisir le système de projection parmi la sélection proposée"))
        Answer1 <- tk_select.list(title="Choix du système de projection",
                                  choices=df_EPSG$Label,
                                  preselect="EPSG:2154 - RGF93/Lambert-93",
                                  multiple=F)
        EPSG <- df_EPSG$EPSG[match(Answer1,df_EPSG$Label)]
        proj4string(shp) <- CRS(paste0("+init=epsg:",EPSG)) # Attribution d'un système de projection au shp
      }
    # }

    # ----- Reprojection et crop -----
    shp <- spTransform(shp,
                       CRS("+init=epsg:2154"))

    Return <- tryCatch(shp[zone,],
                       error=function(cond) {
                         List_ERROR <- c(List_ERROR,
                                         shp_source)
                         shp <- gBuffer(shp, byid=TRUE, width=0) # permet d'éliminer les erreurs typologique +
                         # argument byid=T permet de garder les données associés
                         shp <- shp[zone,]
                         return(list(Shape=shp,
                                     Error=List_ERROR))
                       })

    if (class(Return)=="list" & length(Return)==2) { # impératif de contrôler la classe
      shp <- Return[[which(names(Return)=="Shape")]]
      List_ERROR <- Return[[which(names(Return)=="Error")]]
    } else {
      shp <- Return
    }

    # -- Sauvegarde des tables attributaires
    ListShp_DF <- c(ListShp_DF,list(shp@data))
    names(ListShp_DF)[i] <- shp_id

    # ----- Ecriture du shape corrigé -----
    if (length(shp)!=0) { # Contrôle que la couche croise bien au moins 1 fois la zone d'étude

      if (!is.null(shp_attrs) & length(shp_attrs)) {
        # -- Subset des attributs à conserver
        shp <- shp[,shp_attrs]
      }
      # -- Fusion des shapes selon 1 champ (si indication existe)
      if (length(shp_union)!=0) {
        shp_df <- shp@data %>%
          distinct_(shp_union,.keep_all=T)
        row.names(shp_df) <- as.character(shp_df[,shp_union]) # value sert d'identifiant
        # Encoding(shp@data[,shp_union])

        shp <- gUnaryUnion(shp,id=shp@data[,shp_union]) # nécessaire de récupérer le data.frame
        shp <- SpatialPolygonsDataFrame(shp, shp_df, match.ID=T) # match les row.names et les ID des polygones
      }



      # -- Création des dossiers nécessaires
      # setwd(repDataProjet)
      df <- filter(Path_DF,
                   Id %in% shp_id)
      Niveaux <- df$Niveau
      dir_OUT <- unique(df$Dir_Source)
      for (niv in Niveaux) {
        # niv <- Niveaux[1]
        Dirs <- filter(df, Niveau==niv) %>%
          dplyr::select(Dossier) %>%
          distinct() %>% # idem unique mais plus rapide
          unlist()
        for (dir in Dirs) {
          # dir <- Dirs[1]
          dir.create(paste0(repDataProjet,"/",dir), showWarnings=F)
        }
      }

      # -- Ecriture du shape
      reptemp <- paste(repDataProjet, dir_OUT, sep="/")
      writeOGR(shp, dsn=reptemp, layer=shp_name,
               driver="ESRI Shapefile", encoding="UTF-8", overwrite_layer = T)

      # Sauvegarde des shapes lus et validés :
      ListShp_SHP3 <- c(ListShp_SHP3, shp)
      names(ListShp_SHP3)[length(ListShp_SHP3)] <- shp_id
    } else {
      List_ERROR2 <- c(List_ERROR2,
                       shp_source)
    }

    info <- round(i/length(ListShp_SHP)*100)
    setTkProgressBar(pb, info, paste0("Réécriture des shapes en cours : (",info," %)"),
                     paste0(info,"% done"))
  }

  if (length(List_ERROR) > 0) {
    tk_messageBox(type="ok",
                  message=paste0("Au cours de la réécriture des shapes, une erreur topologique (polygone vide) a été détecté sur le(s) shape(s) :\n\n",
                                 paste0("- ",couche," du dossier ",rep1,collapse="\n"),
                                 "\n\nA contrôler."))
  }

  if (length(List_ERROR2) > 0) {
    tk_messageBox(type="ok",
                  message=paste0("Erreur rencontrée pour le(s) fichier(s) :\n\n",
                                 paste0(List_ERROR2,collapse="\n"),
                                 "\n\nShape(s) situé(s) hors emprise (même après reprojection).\nA contrôler.\nShape(s) non réécrit(s)"))
  }


  tk_messageBox(type="ok",
                message="Réécriture des shapes terminée.")
  close(pb)

  # # ----- Extraction et Sauvegarde des tables attributaires
  # ListShp_DF <- c()
  # for (i in 1:length(ListShp_SHP3)) {
  #   shp <- ListShp_SHP3[[i]]
  #   id_shp <- names(ListShp_SHP3)[i]
  #   print(id_shp)
  #   ListShp_DF <- c(ListShp_DF,list(shp@data))
  #   names(ListShp_DF)[i] <- id_shp
  # }

  # Sauvegarde des shapes lus :
  save(ListShp_SHP3,
       file="Tables/Shapes_ReecritureShape2.RData")
  # Sauvegarde des tables attributaires des shapes lus :
  save(ListShp_DF,
       file="Tables/ShapesDF_ReecritureShape2.RData")
}
