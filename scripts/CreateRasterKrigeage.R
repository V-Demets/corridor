#' Création raster
#'
#' @description Création d'un raster à destination d'un krigeage (reprend la fonction originale CreateRaster). La fonction utilise les données SIG
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
#'
#' @import sp
#' @import tcltk
#' @import rgdal
#' @import rgeos
#' @import raster
#'
#' @export


CreateRasterKrigeage <- function(Zone_SHP,
                         Buffer_Width=1000,
                         ParamSIG_FILE,
                         res_tile=20) {
  # repDataBrutes=paste0(getwd(),"/Data/SIG/Vecteurs/DataBrutes")
  # Zone_SHP <- Bbox_SHP
  # Buffer_Width <- 1000
  # Xlsx_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Table_Champs_SHP_VD3.xlsx"
  # # Xlsx <- read.xlsx(Xlsx_FILE, sheet="Feuil2")
  # ParamRAS_DF <- Xlsx
  dir.create("Tables",
             showWarnings=F)
  dir.create("Out",
             showWarnings=F)
  dir.create("Out/SIG",
             showWarnings=F)
  dir.create("Out/SIG/Raster",
             showWarnings=F)
  dir.create("Out/SIG/Raster/Krigeage",
             showWarnings=F)

  # ----- Rajout du buffer au périmètre d'étude -----
  zone <- gBuffer(Zone_SHP, width=Buffer_Width)

  # ----- Import des paramètres renseignés dans le classeur Excel -----
  # ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_Ter_V2.xlsx"
  ListSheets <- getSheetNames(ParamSIG_FILE)
  sheet <- "Krigeage"

  ParamSIG_DF <- read.xlsx(ParamSIG_FILE, # Changer nom ParamSIG_FILE
                           sheet=sheet) %>%
    select(Id,Source,shp,Encodage,Inclure_RASTER,Intitule_ChampRaster,Valeur_ChampRaster,Buffer)

  Krigeage_DF <- filter(ParamSIG_DF,
                      Inclure_RASTER=="Oui") %>%
    group_by(Intitule_ChampRaster) %>%
    summarise(Id=paste0(Id,collapse=";"),
              shp=paste0(shp,collapse=";"),
              Valeur_ChampRaster=paste0(Valeur_ChampRaster,collapse=";")) %>%
    ungroup()


  # ----- Initialisation du raster destiné à recevoir toutes les couches d'informations -----
  ext <- extent(zone)

  ras_GRD <- raster(ext) # couche contenant (possibilité existe) les couches de plusieurs shp du même nom
  projection(ras_GRD) <- proj4string(zone) # projection
  res(ras_GRD)=res_tile # raster initiant la boucle -> où seront versées les valeurs des différentes couches


  # ras_GRD2 <- raster(ext)
  # projection(ras_GRD2) <- proj4string(zone) # projection
  # res(ras_GRD2)=res_tile

  # ----- Initialisation listes qui contiendra les shapes lus, reprojetés et clippés -----
  # List_SHP2 <- c()

    load("Tables/Shapes_ReecritureShape2.RData") # Chargement des shapes lus et sauvegardés
  # List_Champ <- unique(df$Intitule_ChampRaster) # Premiers test - BDCALLA exclu (problème avec shapes point)
  # List_Champ <- List_Champ[List_Champ %in% c("Aires_Protection","Geologie","Ilots_Sen","Peuplt_IFN",
  #                                            "SER","Station_Forestiere","Unites_Paysageres")]
  # List_Champ <- List_Champ[3]
  pb <- tkProgressBar("Progression", "Rasterisation des shapes (%)",
                      0, 100, width=500)
  if (dim(Krigeage_DF)[1] > 0) {
    for (j in 1:dim(Krigeage_DF)[1]) {
  # for (chp in List_Champ) { # Pour faciliter la création des rasters, on travaille en considérant d'abord la
    # colonne qui sera utilisée dans le raster => on extrait ensuite les valeurs de tous les shp concernés

    # chp <- List_Champ[1]
    # List_Shp_IN <- df$shp[df$Intitule_ChampRaster %in% chp]
    Layer_name <- Krigeage_DF$Intitule_ChampRaster[j]

    print(paste0("##### Champ raster '",Layer_name,"' #####"))

    Layer_values <- Krigeage_DF$Valeur_ChampRaster[j]
    Layer_values <- str_split(Layer_values,";")
    Layer_values <- unlist(Layer_values,";")

    ListShp_id <- Krigeage_DF$Id[j]
    ListShp_id <- str_split(ListShp_id,";")
    ListShp_id <- unlist(ListShp_id,";")
    ListShp_name <- str_split(Krigeage_DF$shp[j],";")
    ListShp_name <- unlist(ListShp_name)
    # ListShp_id <- df$Id[df$Intitule_ChampRaster %in% chp]

    pos <- which(duplicated(ListShp_id))
    if (length(pos) > 0) {
      stop(paste0("Shape ",
                  paste0(ListShp_name[pos],collapse=", "),
                  " détecté plusieurs fois dans la feuille ",
                  sheet,". Risque d'erreur générée par des doublons de couches portant le même identifiant"))
    }

    # On récupère les shapes concernés (dans ListSHP3)
    # ListSHP <- ListShp_SHP3[names(ListShp_SHP3) %in% ListShp_id]
    #
    # # Sécurité si shape n'a pas été lu :
    # pos_Error <- which(!ListShp_id %in% names(ListShp_SHP3))
    # if (length(pos_Error) > 0) {
    #   warning(paste0("Le(s) shape(s) '",
    #                  paste0(ListShp_name[pos_Error],
    #                         "' (Id:",
    #                         ListShp_id[pos_Error],
    #                         ")",
    #                         collapse=", "),
    #                  " ne figure(nt) pas dans l'archive 'Shapes_ReecritureShape2.RData' : shape(s) non indiqué(s) dans la table 'Parametrage_SIG' lors de l'exécution de la fonction 'ReecritureShape()' ou shape(s) situé(s) hors emprise"))
    #   Layer_values <- Layer_values[-pos_Error]
    #   ListShp_id <- ListShp_id[-pos_Error]
    #   ListShp_name <- ListShp_name[-pos_Error]
    #   }

    Count=0
    for (i in 1:length(ListShp_id)) {
      #   shp <- ListSHP[[i]]
      #   shp_id <- names(ListSHP[i])
      #   shp_name <- ListShp_name[i]
      #   id_test <- ListShp_id[i]
      # if (id_test!=shp_id) {
      #   stop(paste0("Erreur de correspondance des Id des shapes (shape: ",
      #               shp_name," -- Id/Id_Test: ",shp_id,"/",id_test,")"))
      # }
      shp_id <- ListShp_id[i]
      shp_name <- ListShp_name[ListShp_id %in% shp_id]


      if (!shp_id %in% names(ListShp_SHP3)) {
        tk_messageBox(type="ok",
                      message=paste0("Le shape '",
                                     paste0(shp_name,
                                            "' (Id:",
                                            shp_id,
                                            ")",
                                            collapse=", "),
                                     " ne figure pas dans l'archive 'Shapes_ReecritureShape2.RData' : shape non indiqué dans la table 'Parametrage_SIG' lors de l'exécution de la fonction 'ReecritureShape()' OU shape situé hors emprise"))
      } else {

        shp <- ListShp_SHP3[[which(names(ListShp_SHP3) %in% shp_id)]]

        # shp_name <- ListShp_name[match(shp_id,ListShp_id)]
        value <- Layer_values[ListShp_id %in% shp_id]

        print(paste0("Shape: ",shp_name," -- Id: ",shp_id," -- Valeur: ",value))

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



        # Cas possibles : polygones = lignes ou points
        if (class(shp)=="SpatialLinesDataFrame" |
            class(shp)=="SpatialPointsDataFrame") {
          # Answer_Buffer <- tk_messageBox(type="yesno",
          #                                message=paste0("La couche SIG '",
          #                                               shp_name," (Id: ",shp_id,
          #                                               ")' est composée de lignes ou de points.\nRajouter un buffer pour les éléments du shapes ?\n(Si aucun buffer n'est ajouté, l'information contenue dans la couche ne sera pas prise en compte ici)"))
          # if (Answer_Buffer=="yes") {
          # Buffer <- tk_select.list(1:1000,
          #                          title=paste0("Valeur du buffer à ajouter pour les éléments de la couche ",
          #                                       shp_name),
          #                          multiple=F)
          Buffer <- 11
          if (shp_name %in% c("LIGNE_ELECTRIQUE","RoutesSecondaires")) {
            Buffer <- 20
          }
          shp <- buffer(shp,width=Buffer,
                        dissolve=F)
          # }


          # shp <- gUnaryUnion(shp)
          # shp <- disaggregate(shp)
          # shp <- disaggregate(shp)
          # df <- data.frame(Surface=gArea(shp, byid=T))
          # row.names(df) <- 1:dim(df)[1]
          # shp <- SpatialPolygonsDataFrame(shp,
          #                                        data=df)
        }
        if (class(shp)=="SpatialPolygonsDataFrame") {
          if (i==1) {
            shp_ToFill <- shp
          } else {
            shp_ToFill <- rbind(shp_ToFill,shp)
          }
        }
      }
    }

    # IMPORTANT : fusion des polygones du shape selon l'attribut "value"
    shp_ToFill_df <- shp_ToFill@data %>%
      distinct(value)
    row.names(shp_ToFill_df) <- as.character(shp_ToFill_df$value) # value sert d'identifiant

    shp_ToFill <- gUnaryUnion(shp_ToFill,id=shp_ToFill@data$value) # nécessaire de récupérer le data.frame
    shp_ToFill <- SpatialPolygonsDataFrame(shp_ToFill, shp_ToFill_df, match.ID=T) # match les row.names et les ID des polygones

    # writeOGR(shp_ToFill,
    #          dsn = "Out/Vecteurs/",
    #          layer=paste0(Layer_name,"_test"),
    #          driver="ESRI Shapefile",
    #          encoding="UTF-8",
    #          overwrite_layer = T)


    # -- Extraction des valeurs de la couche pour chaque point du grid :
    shp_ToFill <- shp_ToFill[zone,]
    ras_temp <- rasterize(shp_ToFill,ras_GRD,
                          field="value") # remplissage des cellules du raster par les valeurs du shape)
    names(ras_temp) <- Layer_name
    ras_GRD <- stack(ras_temp,ras_GRD)
    }

    # ras_temp2 <- stackApply(ras_GRD, indices=rep(1,dim(ras_GRD)[3]),
    #                         fun=sum)
    # names(ras_temp2) <- Layer_name
    # ras_GRD2 <- stack(ras_temp2,ras_GRD2)
    info <- round(j/dim(Krigeage_DF)[1]*100)
    setTkProgressBar(pb, info, paste0("Rasterisation des shapes pour le krigeage en cours : (",info," %)"),
                     paste0(info,"% done"))

    writeRaster(ras_GRD,
                file="Out/Raster/Krigeage/RasterKrigeage.tif",
                format="GTiff",
                overwrite=T)

  } else {
      Msg1 <- tk_messageBox(type="ok",
                            message="Aucun shape indiqué à destination du krigeage.")
    }
  # }

close(pb)
 # ----- Sauvegarde des shapes lus -----
  # save(List_SHP2,
  #      file="Tables/Shapes_CreateRaster.RData")

  return(ras_GRD)
}
