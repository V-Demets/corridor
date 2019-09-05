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
#' @param ParamSHP_DF = tableau paramétrant les shapes et attributs à découper, fusionner et réécrire.
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
                            ParamSHP_DF,
                            repDataBrutes=paste0(getwd(),"/Data/SIG/Vecteurs/DataBrutes"),
                            Buffer_Width=1000,
                            Path_DF) {
  # Zone_SHP <- Bbox_SHP
  # ParamSHP_DF <- df_BASE
  repDataProjet <- paste0(dirname(repDataBrutes),"/DataProjet")
  dir.create(repDataProjet,
             showWarnings=F)
  dir.create("Tables",
             showWarnings=F)

  # ----- Choix du fichier d'emprise -----
  # SECURITE A SUPPRIMER ?
  if (is.null(Zone_SHP) | class(Zone_SHP)[1]!="SpatialPolygonsDataFrame") {
    Zone_FILE <- tk_choose.files(default="SIG/Vecteurs/Perimetre2014.shp", #getwd(),
                                 caption="Choix du shape d'emprise permettant de clipper et de reprojeter les différents shp",
                                 multi=F,
                                 filters=matrix(c(".shp",".shp"),

                                                nrow=1,ncol=2))
    Zone_SHP <- readOGR(dsn=dirname(Zone_FILE),
                        layer=file_path_sans_ext(basename(Zone_FILE)),
                        verbose=F,
                        stringsAsFactors=F) %>%
      spTransform(CRS("+init=epsg:2154")) # Forcer la couche d'emprise en L_93 ou laisser le choix ?
  }

  # ----- Rajout du buffer au périmètre d'étude -----
  zone <- gBuffer(Zone_SHP, width=Buffer_Width)

  # ----- Tableau si système de projection manquant -----
  df_EPSG <- data.frame(Label=c("EPSG:2154 - RGF93/Lambert-93",
                                "EPSG:7421 - NTF(Paris)/Lambert Zone II",
                                "EPSG:4326 - WGS84 (gps)",
                                "EPSG:32632 - UTM 32N (gps téléphone)"),
                        EPSG=c(2154, 7421, 4326, 32632),
                        stringsAsFactors=F)

  # ----- Import des paramètres du classeur 'Parametres_SIG.xslx' -----
  # ParamSHP_DF <- df_BASE
  ParamSHP_DF <- filter(ParamSHP_DF,
                        Inclure_SHP=="Oui")
  SHP_DF <- distinct(ParamSHP_DF,
                       Id,shp)
  List_SHP <- SHP_DF$shp
  names(List_SHP) <- SHP_DF$Id

  # ----- Import des chemins relatifs des différentes couches SIG -----
  # Path_DF <- res[[3]]

  # ----- Début chaîne travail sur shapes -----
  List_ERROR <- c()
  List_ERROR2 <- c()
  List_SHP3 <- c() # List_SHP3 accueille tous les shapes lus (y compris ceux de List_SHP2)

  pb <- tkProgressBar("Progression", "Réécriture des shapes (%)",
                      0, 100, width=500)

  for (i in 1:length(List_SHP)) {

    # Paramètres récupérés dans Path_DF et ParamSHP_DF :
    # Lecture du shape :
    # i=1
    shp_name <- unname(List_SHP[i])
    shp_Id <- names(List_SHP)[i]
    shp_source <- unique(ParamSHP_DF$Source[ParamSHP_DF$Id %in% shp_Id])
    print(shp_source)
    shp_encoding <- unique(ParamSHP_DF$Encodage[ParamSHP_DF$Id %in% shp_Id])
    shp_attrs <- unique(ParamSHP_DF$Attributs[ParamSHP_DF$Id %in% shp_Id &
                                                  ParamSHP_DF$Inclure_SHP=="Oui"])
    # shp_attrs <- shp_attrs[1]
    shp_union <- unique(ParamSHP_DF$Union_Champ[ParamSHP_DF$Id %in% shp_Id &
                                           ParamSHP_DF$Inclure_SHP=="Oui" &
                                           !is.na(ParamSHP_DF$Union_Champ)])
    if (length(shp_union) > 1) {
      stop(paste0("Plus d'un champ indiqué pour fusionner les polygones du shapes pour le fichier :\n\n",
                  shp_source))
    }

    # -- Possibilité que shp déjà lu pendant exécution de la fonction CreateRaster()
    if (shp_Id %in% names(List_SHP2)) {
      shp <- List_SHP2[[which(names(List_SHP2) %in% shp_Id)]]
    } else {
      # print("Controle passage")
    # -- Lecture
      if (is.na(shp_encoding)) {
        shp <- readOGR(dsn=paste0(repDataBrutes,"/",dirname(shp_source)),
                       layer=file_path_sans_ext(basename(shp_source)),
                       verbose=F,
                       stringsAsFactors=F,
                       use_iconv = TRUE, encoding = "CP1252")
      } else {
        shp <- readOGR(dsn=paste0(repDataBrutes,"/",dirname(shp_source)),
                       layer=file_path_sans_ext(basename(shp_source)),
                       verbose=F,
                       stringsAsFactors=F,
                       use_iconv = TRUE, encoding = shp_encoding)
      }

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
    }

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

      # ----- Ecriture du shape corrigé -----
    if (length(shp)!=0) { # Contrôle que la couche croise bien au moins 1 fois la zone d'étude

      if (!is.null(shp_attrs)) {
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
                    Id %in% shp_Id)
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
      List_SHP3 <- c(List_SHP3, shp)
      names(List_SHP3)[length(List_SHP3)] <- shp_Id
    } else {
      List_ERROR2 <- c(List_ERROR2,
                       shp_source)
    }

    info <- round(i/length(List_SHP)*100)
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


    # Sauvegarde des shapes lus (2):
  save(List_SHP3,
       file="Tables/Shapes_ReecritureShape.RData")
}
