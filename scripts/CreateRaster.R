#' Création raster
#'
#' @description Création d'un raster à destination d'un krigeage. La fonction utilise les données SIG
#' (vecteurs ou rasters) disponibles en entrée pour organiser les données sous la forme d'un raster
#' multicouches.
#' L'organisation des données est directement dictée par le contenu du classeur xlsx ..... "REMPLACER NOM 1"
#' respectant la forme du fichier produit par la fonction "REMPLACER NOM 2".
#'
#' @return La fonction retourne un RasterStack.
#'
#' @author Bruciamacchie Max, Demets Valentin
#'
#' @param Zone_SHP = shape définissant le périmètre de la zone d'étude. Sécurité si aucun shape choisi
#' @param Buffer_Width = taille du buffer à rajouter au périmètre de la zone d'étude.
#' Valeur par défaut = 1 km.
#' @param ParamRAS_DF = tableau paramétrant les shapes à prendre en compte dans le raster
#' @param Rasters = liste de raster à ajouter au raster composé des extraits de shapes.
#' @param res_tile = résolution du raster (valeur par défaut : 20 m)
#' @param repDataBrutes = répertoire contenant les données SIG (shapes) brutes.
#'
#' @import sp
#' @import tcltk
#' @import rgdal
#' @import rgeos
#' @import raster
#'
#' @export


CreateRaster <- function(Zone_SHP,
                         Buffer_Width=1000,
                         ParamRAS_DF,
                         res_tile=20,
                         repDataBrutes=paste0(getwd(),"/Data/SIG/Vecteurs/DataBrutes")) {
  # Zone_SHP <- Bbox_SHP
  # Buffer_Width <- 1000
  # Xlsx_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Table_Champs_SHP_VD3.xlsx"
  # # Xlsx <- read.xlsx(Xlsx_FILE, sheet="Feuil2")
  # ParamRAS_DF <- Xlsx
  dir.create("Tables",
             showWarnings=F)

  # ----- Rajout du buffer au périmètre d'étude -----
  zone <- gBuffer(Zone_SHP, width=Buffer_Width)

  # ----- Import des paramètres renseignés dans le classeur Excel -----
  # df <- rename_(ParamRAS_DF,
  #               "Couche"="Nom.de.la.couche",
  #               "Inclure_Couche"="Inclure.la.couche",
  #               "Champ_Couche"="Intitulé.du.champ",
  #               "Valeurs_pour_Champ"="Valeur.à.insérer.dans.shape") %>%
  df <- filter(ParamRAS_DF,
               Inclure_RASTER=="Oui")

  # ----- Initialisation du raster destiné à recevoir toutes les couches d'informations -----
  ext <- extent(zone)
  ras_GRD2 <- raster(ext)
  projection(ras_GRD2) <- proj4string(zone) # projection
  res(ras_GRD2)=res_tile

  # ----- Tableau si système de projection manquant -----
  df_EPSG <- data.frame(Label=c("EPSG:2154 - RGF93/Lambert-93",
                                "EPSG:7421 - NTF(Paris)/Lambert Zone II",
                                "EPSG:4326 - WGS84 (gps)",
                                "EPSG:32632 - UTM 32N (gps téléphone)"),
                        EPSG=c(2154, 7421, 4326, 32632),
                        stringsAsFactors=F)

  # ----- Initialisation listes qui contiendra les shapes lus, reprojetés et clippés -----
  List_SHP2 <- c()

  List_Champ <- unique(df$Intitule_ChampRaster) # Premiers test - BDCALLA exclu (problème avec shapes point)
  # List_Champ <- List_Champ[List_Champ %in% c("Aires_Protection","Geologie","Ilots_Sen","Peuplt_IFN",
  #                                            "SER","Station_Forestiere","Unites_Paysageres")]
  # List_Champ <- List_Champ[3]
  pb <- tkProgressBar("Progression", "Rasterisation des shapes (%)",
                      0, 100, width=500)
  for (chp in List_Champ) { # Pour faciliter la création des rasters, on travaille en considérant d'abord la
    # colonne qui sera utilisée dans le raster => on extrait ensuite les valeurs de tous les shp concernés

    # chp <- List_Champ[1]
    List_Shp_IN <- df$shp[df$Intitule_ChampRaster %in% chp]

    ras_GRD <- raster(ext)
    projection(ras_GRD) <- proj4string(zone) # projection
    res(ras_GRD)=res_tile # raster initiant la boucle -> où seront versées les valeurs des différentes couches

    for (name in List_Shp_IN) {
      # name <- List_Shp_IN[[1]]
      print(name)
      value <- df$Valeur_ChampRaster[which(df$shp %in% name & df$Intitule_ChampRaster %in% chp)] # Valeur à insérer
      # dans le raster pour ce shape (unique pour name et chp - même si 2 vecteurs (57 et 67) doivent être
      # utilisés pour construire le raster sur l'ensemble du périmètre)

      # -- Import des shp concernés
      shp_source <- df$Source[which(df$shp %in% name & df$Intitule_ChampRaster %in% chp)]

      # pos <- which(names(List_SHP3) %in% name)
      for (src in shp_source) { # on peut avoir plusieurs shapes du même nom (si séparation 57/67
                                # + nom identique (BD TOPO))
        # src <- shp_source[1]
      shp_encoding <- df$Encodage[which(df$shp %in% name &
                                          df$Intitule_ChampRaster %in% chp &
                                          df$Source %in% src)]
      shp_Id <- df$Id[which(df$shp %in% name &
                                          df$Intitule_ChampRaster %in% chp &
                                          df$Source %in% src)]
        # Lecture
      if (is.na(shp_encoding)) {
      shp <- readOGR(dsn=paste0(repDataBrutes,"/",dirname(src)),
                     layer=file_path_sans_ext(basename(src)),
                     verbose=F,
                     stringsAsFactors=F,
                     use_iconv = TRUE, encoding = "CP1252")
      } else {
        shp <- readOGR(dsn=paste0(repDataBrutes,"/",dirname(src)),
                       layer=file_path_sans_ext(basename(src)),
                       verbose=F,
                       stringsAsFactors=F,
                       use_iconv = TRUE, encoding = shp_encoding)
      }

      # Contrôle du système de projection (Lambert 93 demandé)
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

      # Reprojection et crop:
      shp <- spTransform(shp,
                         CRS("+init=epsg:2154"))
      shp <- shp[zone,]

      # Sauvegarde des shapes lus :
      List_SHP2 <- c(List_SHP2, shp)
      names(List_SHP2)[length(List_SHP2)] <- shp_Id
        # i <- pos[1]
        # shp <- List_SHP3[[i]]
        # plot(shp)
        if (value %in% names(shp)) {
          shp$value <- shp[[value]]
          shp$value <- as.factor(shp$value)
        } else {
          shp$value <- value
          shp$value <- as.numeric(shp$value)
        }
        shp <- shp["value"] # On ne conserve que la colonne "témoin" = valeur paramétrée ou valeurs de
        # la colonne désignée dans le classeur de paramètres.
        shp@data <- mutate(shp@data,
                           value=ifelse(is.na(value),0,value))

        # IMPORTANT : fusion des polygones du shape selon l'attribut "value"
        shp_df <- shp@data %>%
          distinct(value)
        row.names(shp_df) <- as.character(shp_df$value) # value sert d'identifiant

        shp <- gUnaryUnion(shp,id=shp@data$value) # nécessaire de récupérer le data.frame
        shp <- SpatialPolygonsDataFrame(shp, shp_df, match.ID=T) # match les row.names et les ID des polygones

        # -- Extraction des valeurs de la couche pour chaque point du grid :
        shp <- shp[zone,]
        ras_temp <- rasterize(shp,ras_GRD,
                              field="value") # remplissage des cellules du raster par les valeurs du shape)
        ras_GRD <- stack(ras_temp,ras_GRD)
      }
      # plot(shp)
      # head(shp)
    }
    ras_temp2 <- stackApply(ras_GRD, indices=rep(1,dim(ras_GRD)[3]),
                            fun=sum)
    names(ras_temp2) <- chp
    ras_GRD2 <- stack(ras_temp2,ras_GRD2)

    info <- round(match(chp,List_Champ)/length(List_Champ)*100)
    setTkProgressBar(pb, info, paste0("Rasterisation des shapes en cours : (",info," %)"),
                     paste0(info,"% done"))
  }
close(pb)
 # ----- Sauvegarde des shapes lus -----
  save(List_SHP2,
       file="Tables/Shapes_CreateRaster.RData")

  return(ras_GRD2)
}
