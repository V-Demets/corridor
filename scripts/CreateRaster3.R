#' Création raster
#'
#' @description Création des rasters. La fonction utilise les données SIG selon les indications fournies dans
#' le classeur 'Parametrage_SIG.xlsx'.
#' (vecteurs ou rasters) disponibles en entrée pour organiser les données sous la forme de rasters
#' multicouches en leur attribuant certaines valeurs...
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
#' @param ParamSIG_FILE = Nom du fichier contenant les éléments définissant les différents rasters.
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


CreateRaster3 <- function(Zone_SHP,
                          Buffer_Width=1000,
                          ParamSIG_FILE,
                          res_tile=20) {
  # repDataBrutes=paste0(getwd(),"/Data/SIG/Vecteurs/DataBrutes")
  dir.create("Out",showWarnings=F)
  dir.create("Out/SIG",showWarnings=F)
  dir.create("Out/SIG/Raster",showWarnings=F)

  # N.B : dans processus de rasterisation (buffer), voir si il est correct de changer la méthode par défaut de
  # gdal message :
  # Warning 1: organizePolygons() received a polygon with more than 100 parts. The processing may be really slow.
  # You can skip the processing by setting METHOD=SKIP, or only make it analyze counter-clock wise parts by
  # setting METHOD=ONLY_CCW if you can assume that the outline of holes is counter-clock wise defined


  # --- Chargement des shapes lus via ReecritureShape2 (liste : 'ListShp_SHP3') :
  load("Tables/Shapes_ReecritureShape2.RData")

  # --- Emprise :
  # Zone_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Perimetres/Perimetre2014.shp"
  # Zone_SHP <- readOGR(dsn=dirname(Zone_FILE),
  #                     layer=file_path_sans_ext(basename(Zone_FILE)),
  #                     verbose=F,
  #                     stringsAsFactors=F) %>%
  #   spTransform(CRS("+init=epsg:2154")) # Forcer la couche d'emprise en L_93 ou laisser le choix ?
  Zone_SHP <- spTransform(Zone_SHP,
                          CRS("+init=epsg:2154"))

  if (is.null(Zone_SHP) | class(Zone_SHP)[1]!="SpatialPolygonsDataFrame") {
    stop("Fichier d'emprise incorrect : (is.null(Zone_SHP) | class(Zone_SHP)[1]!='SpatialPolygonsDataFrame'")
  }
  # --- Rajout du buffer au périmètre d'étude --- Voir si cela reste utile ?
  zone <- buffer(Zone_SHP,width=Buffer_Width,dissolve=T)


  # ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_bis_V2.xlsx"
  # ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_Ter_V1.xlsx"
  # ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_Ter_V2.xlsx"
  # ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_Ter_V3.xlsx"
  ListSheets <- getSheetNames(ParamSIG_FILE)

  # -- Initialisation : raster à remplir
  ext <- extent(zone)
  rasExt_RAS <- raster(ext)
  projection(rasExt_RAS) <- proj4string(zone) # projection
  res(rasExt_RAS)=res_tile

  compteur=0
  Count_2=0
  for (sheet in ListSheets[2:(length(ListSheets)-1)]) { # 2:(length(ListSheets)-1) # :(length(ListSheets)-1)     2:(length(ListSheets)-1)  #c(2,3,5)
    dir.create(paste0("Out/SIG/Raster/",sheet),showWarnings=F)
    # sheet <- ListSheets[2]
  print("##### Buffer #####")
    print(sheet)
    ParamSIG_DF <- read.xlsx(ParamSIG_FILE, # Changer nom ParamSIG_FILE
                             sheet=sheet) %>%
      filter(!is.na(Buffer) | !is.na(Distance))

    # -- Liste des shapes à considérer :
    ListShp <- ParamSIG_DF$shp
    names(ListShp) <- ParamSIG_DF$Id


    # for (i in 1:dim(ParamSIG_DF)[1]) {
    #   shp_name <- unname(ListShp[i])
    #   shp_id <- names(ListShp[i])
    #   print(paste0("Raster: ",sheet," -- shape: ",shp_name," -- id: ",shp_id))
    #
    #   # -- Récupération du shape dans l'archive constituée via la fonction ReecritureShape2
    #   shp <- ListShp_SHP3[[which(names(ListShp_SHP3) %in% shp_id)]]

    ##### Buffer #####
    # -- Paramètres pour la constitution de la couche :
    # - Nbre de paramètres différents et identification des couches concernées
    Buffer_DF <- filter(ParamSIG_DF,
                        !is.na(Buffer)) %>%
      group_by(Nbre_Niveau,Buffer,Poids) %>%
      summarise(Id=paste0(Id,collapse=";"),
                shp=paste0(shp,collapse=";")) %>%
    # ,
    #             # Detail_Valeurs=ifelse(Detail_Valeurs %in% c("Non",NA),
    #             #                       NA,
    #             #                       paste0(Detail_Valeurs,collapse=";")),
    #             Detail_Valeurs=paste0(Detail_Valeurs,collapse=";"),
    #             Valeurs=paste0(Valeurs,collapse=";")) %>%
      ungroup()


    if (dim(Buffer_DF)[1] > 0) {
      # Count_Buff=0
      dir.create(paste0("Out/SIG/Vecteurs/Buffer"),showWarnings=F)
      dir.create(paste0("Out/SIG/Vecteurs/Buffer/",sheet),showWarnings=F)
    for (j in 1:dim(Buffer_DF)[1]) { #
      print(j)
      # Nombre de niveaux d'impact
      Niveaux <- Buffer_DF$Nbre_Niveau[j]
      # Buffers
      Buff_Width <- Buffer_DF$Buffer[j]
      Buff_Width <- str_split(Buff_Width,";")
      Buff_Width <- unlist(Buff_Width,";")
      # # Poids
      Buff_Poids <- Buffer_DF$Poids[j]
      Buff_Poids <- str_split(Buff_Poids,";")
      Buff_Poids <- unlist(Buff_Poids,";")

      if (length(Buff_Width) != Niveaux) { # (Sécurité temporaire)
        stop(paste0("Nombre de valeurs de la colonne 'Buffer' (",
                    paste0(Buff_Width,collapse=";"),
                    ") incohérentes avec le nombre de valeurs de la colonne 'Nbre_Niveau' (",
                    paste0(Niveaux,collapse=";"),")"))
      }

      ListShp_name <- str_split(Buffer_DF$shp[j],";")
      ListShp_name <- unlist(ListShp_name)
      ListShp_id <- str_split(Buffer_DF$Id[j],";")
      ListShp_id <- unlist(ListShp_id)

      # On récupère les shapes concernés (dans ListSHP3)
      ListSHP <- ListShp_SHP3[names(ListShp_SHP3) %in% ListShp_id]

      # Sécurité si shape n'a pas été lu :
      pos_Error <- which(!ListShp_id %in% names(ListShp_SHP3))
      if (length(pos_Error) > 0) {
        warning(paste0("Le(s) shape(s) '",
                       paste0(ListShp_name[pos_Error],
                              "' (Id:",
                              ListShp_id[pos_Error],
                              ")",
                              collapse=", "),
                       " ne figure(nt) pas dans l'archive 'Shapes_ReecritureShape2.RData' : shape(s) non indiqué(s) dans la table 'Parametrage_SIG' lors de l'exécution de la fonction 'ReecritureShape()' ou shape(s) situé(s) hors emprise"))
      }


      # Etape 1 : pour le nbre niveaux, les buffers et les poids (j) on fusionne tous les éléments (polygones)
      Count=0
      shp_ToFill <- c()
      # ListSHP <- ListSHP[3]
      for (i in 1:length(ListSHP)) {
        # On ne s'intéresse pas aux buffer pour l'instant
        shp <- ListSHP[[i]]
        shp_id <- names(ListSHP[i])
        shp_name <- ListShp_name[match(shp_id,ListShp_id)]

        print(paste0("Shape: ",shp_name," -- Id: ",shp_id))

        # Sélection éventuelle de certaines valeurs d'un des champs :
        detail_DF <- filter(ParamSIG_DF,
                       Id==shp_id &
                         shp==shp_name &
                         Poids==paste0(Buff_Poids,collapse=";") &
                         !Detail_Valeurs %in% c("Non",NA))
        if (dim(detail_DF)[1] > 0) {
          attrs_detail <- unique(detail_DF$Detail_Valeurs)
          val_detail <- unique(detail_DF$Valeurs)
          print(paste0("Détail: ",attrs_detail," (",val_detail,") - ",shp_name," (Id: ",shp_id,")"))
          pos_detail <- which(shp@data[,attrs_detail] %in% val_detail)
          if (length(pos_detail) > 0) { # Sécurité si impossible de retrouver la valeur dans la table
            # attributaire (à cause encodages par exemple
          shp <- shp[which(shp@data[,attrs_detail] %in% val_detail),]
          } else {
            Msg1 <- tk_messageBox(type="ok",
                                  message=paste0("Attention construction du buffer selon le détail des valeurs de la colonne ",
                                                 attrs_detail,
                                                 " du shape ",
                                                 shp_name," (Id: ",shp_id,
                                                 ") :\n\nImpossible de retrouver la correspondance entre les valeurs de la colonne et la valeur inscrite dans le classeur de paramétrage '",
                                                 val_detail,"'.\n\nChoisir la valeur dans la liste qui va suivre"))
            val_detailBis <- tk_select.list(unique(shp@data[,attrs_detail]),
                                      title=paste0("Choisir la valeur de la table attributaire du shape ",
                                                   shp_name," (Id: ",shp_id,
                                                   ")\ncorrespondant à la valeur '",
                                                   val_detail,"'"),
                                      multiple=F)
            val_detail <- val_detailBis

            shp <- shp[which(shp@data[,attrs_detail] %in% val_detail),]
          }
        }

        shp$Id <- (Count+1):(Count+dim(shp)[1])
        Count=Count+dim(shp)[1]
        shp <- shp[,"Id"]
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


        print("Repère")
      # # Ecriture du shape contenant tous les polygones à intégrer dans le layer (raster).
      # writeOGR(shp_ToFill,
      #          dsn = "Out/Vecteurs/",
      #          layer=paste0(layer,"_Buff0"),
      #          driver="ESRI Shapefile",
      #          encoding="UTF-8",
      #          overwrite_layer = T)
      # # maintenant que tous les éléments sont rassemblés, on passe ensuite à la création des buffers (selon les différents niveaux)


      # 1. On unit les polygones du shape
        # Tests vitesse fonctions gBuffer et buffer -> petit nombre de shapes, gBuffer est légèrement meilleur
        # system.time(
        # shp_ToFill2 <- buffer(shp_ToFill,width=0,dissolve=T) # ne pas changer pour raster::buffer => erreurs topologiques
        # )
        # system.time(
        #   shp_ToFill3 <- gBuffer(shp_ToFill,width=0) # ne pas changer pour raster::buffer => erreurs topologiques
        # )
        shp_ToFill <- buffer(shp_ToFill,width=0,dissolve=F)
      shp1 <- gUnaryUnion(shp_ToFill)

      # save(plot(shp1),file=paste0("Shape_",shp_name,"_Id_",shp_id,"_N_",compteur,"_.jpeg"))
           # Polygones
      dir.create("Out/Plot_Shp",showWarnings = F)
           png(file = paste0("Out/Plot_Shp/Shape_",shp_name,"_",shp_id,"_Num_",compteur,"_out.png"),
               width = 800, height = 700)
           plot(shp1)
           dev.off()
           compteur=compteur+1
      # tk_messageBox(type="ok",message="shp1. l.273")
      for (niv in 1:Niveaux) {
        # niv <- 2
        print(paste0("Buffer : ",niv))
        # 2. On applique le buffer
        # N.B : Si buffer basé sur la surface => expression => NA apparaîtront après conversion en numérique
        if (!is.na(as.numeric(Buff_Width[niv]))) { # On est sur une valeur numérique
          shp2 <- buffer(shp1,width=0,dissolve=F)
          shp2 <- buffer(shp2,width=as.numeric(Buff_Width[niv]),
                         dissolve=T)
        } else { # on est sur une expression
          if (str_detect(tolower(Buff_Width[niv]),"surface")) {
            Buff_Width[niv] <- gsub("surface","Surface",tolower(Buff_Width[niv])) # Sécurité : on s'assure que
            # dans l'expression du buffer, le mot (correspondant à la variable) 'Surface' est correctement écrit

            # 2 cas possibles : soit on a déjà un buffer construit au niveau précédent (-> on l'utilise pour
            # le calcul de la surface), soit on doit partir de shp1.
            if (niv > 1) { #Si on est à un niveau > 1, on est sûr d'avoir au moins une fois la surface.
              shp2 <- shp3
            } else {
            shp2 <- disaggregate(shp1)
            df <- data.frame(Surface=gArea(shp2, byid=T))
            row.names(df) <- 1:dim(df)[1]
            shp2 <- SpatialPolygonsDataFrame(shp2,
                                             data=df)
            }
            # shp5 <- shp2
            shp2@data <- mutate_(shp2@data,
                                .dots=setNames(list(Buff_Width[niv]),"Buffer"))
            # Buff_Width[niv] <- gsub("surface","shp2@data$Surface",tolower(Buff_Width[niv]))
            # shp2@data$Buffer <- NA
            # shp2@data$Buffer <- eval(parse(text=Buff_Width[niv]))

            # system.time(
            #   shp2_Test <- gBuffer(shp2,width=shp2@data$Buffer,byid=T)
            #   # shp2_Test <- buffer(shp2,width=shp2@data$Buffer,
            #   #                dissolve=F)
            # )
              # system.time(
                # shp5 <- gBuffer(shp2,width=shp2@data$Buffer,byid=T)
            # shp5 <- shp2
            # shp5 <- buffer(shp5,width=0,dissolve=F)
            shp2 <- buffer(shp2,width=0,dissolve=F)
                shp2 <- buffer(shp2,width=shp2@data$Buffer,dissolve=F)
            # )
            shp2 <- gUnaryUnion(shp2)
          } else {
            stop(paste0("Expression du buffer (chaîne de caractères détectée) non reconnue pour le buffer '",
                        Buff_Width[niv],
                        "' (orthographe de la variable 'Surface' non reconnue). Calcul du buffer impossible"))
          }
        }

        # 3. Récupérer les polygones formés par les buffer
        # slot(shp2, "polygons") <- lapply(slot(shp2, "polygons"), checkPolygonsHoles)
        shp2 <- buffer(shp2,width=0,dissolve=T) # sécurité rajoutée à cause erreur :
        # "Error in rgeos::createPolygonsComment(x@polygons[[i]]) :
        # rgeos_PolyCreateComment: orphaned hole, cannot find containing polygon for hole at index 21482"
        shp3 <- disaggregate(shp2)
        df <- data.frame(Surface=gArea(shp3, byid=T))
        row.names(df) <- 1:dim(df)[1]
        shp3 <- SpatialPolygonsDataFrame(shp3,
                                         data=df)
        shp3@data <- mutate(shp3@data,
                            Poids=as.numeric(Buff_Poids[niv]),
                            Buffer=paste0("niv_",niv))
        if (niv==1 & j==1) {
          shp_Impact <- shp3
        } else {
          shp_Impact <- rbind(shp_Impact,shp3)
        }
        dir.create(paste0("Out/SIG/Vecteurs/Buffer/",sheet,"/Niveau",niv),showWarnings=F)
        shp3 <- spTransform(shp3,
                            CRS("+init=epsg:2154"))
      writeOGR(shp3,
               dsn = paste0("Out/SIG/Vecteurs/Buffer/",sheet,"/Niveau",niv),
               layer=paste0(sheet,"_Buff",niv,"_",j),
               driver="ESRI Shapefile",
               encoding="UTF-8",
               overwrite_layer = T)
      }
    # shp_Sav2 <- shp_Impact
    # shp_Sav3 <- shp_Impact
    # shp_Impact <- shp_Sav3

    # Rasterisation
    rasBuff_temp <- rasterize(shp_Impact,rasExt_RAS,
                              field="Poids",fun=sum)

    # pos <- which(is.na(values(rasBuff_temp)))
    # if (length(pos) > 0) {
    #   values(rasBuff_temp)[pos] <- 0 # on remplace les vides par 0. NON
    # }
    names(rasBuff_temp) <- paste0(sheet,"_Buff")

    # Raster tampon pour repérer valeurs vides
    rasTampon_temp <- rasBuff_temp
    pos_NA <- which(!is.na(values(rasTampon_temp)))
    values(rasTampon_temp)[pos_NA] <- 1
    values(rasTampon_temp)[-pos_NA] <- 0
    # # Possibilité d'écrire le raster obtenu :
    # writeRaster(rasBuff_temp,
    #             file=paste0("Out/SIG/Raster/",sheet,"/",sheet,"_Buff"),
    #             format="GTiff",
    #             overwrite=T)
    # Count_Buff=Count_Buff+1
      if (j > 1) { # j=numéro de la ligne dans Buffer_DF
      rasBuff_RAS <- stack(rasBuff_RAS,rasBuff_temp) # rasBuff_RAS concentre tous les buffers
      rasTampon_RAS <- stack(rasTampon_RAS,rasTampon_temp) # raster tampon pour repère valeurs vides

      rasBuff_RAS <- sum(rasBuff_RAS,na.rm=T)
      rasTampon_RAS <- sum(rasTampon_RAS,na.rm=T)
    } else {
      rasBuff_RAS <- rasBuff_temp
      rasTampon_RAS <- rasTampon_temp
    }
    }

      # Utilisation du tampon pour localiser les pixels vides :
      pos_NA <- which(values(rasTampon_RAS)==0)
      values(rasTampon_RAS)[pos_NA] <- NA
      values(rasTampon_RAS)[-pos_NA] <- 0

      rasBuff_RAS <- rasBuff_RAS + rasTampon_RAS

    # Ecriture du raster obtenu :
      # rasBuff_RAS <- projectRaster(rasBuff_RAS,
      #                             crs=CRS("+init=epsg:2154"))
      names(rasBuff_RAS) <- paste0(sheet,"_Buff")
    writeRaster(rasBuff_RAS,
                file=paste0("Out/SIG/Raster/",sheet,"/",sheet,"_Buff"),
                format="GTiff",
                overwrite=T)
    if (Count_2 > 1) {
      rasFin_RAS <- stack(rasFin_RAS,rasBuff_RAS) # rasFin_RAS est le raster rendu qui
      # concentre tous les layers
    } else {
      rasFin_RAS <- rasBuff_RAS
    }
}

    # Conclusion : on rassemble tous les raster dans 1 seul
    # if (match(sheet,ListSheets)==2) {


  ##### Distance #####
    print("##### Distance #####")
  # for (sheet in ListSheets) {
  # sheet <- ListSheets[6]
    print(sheet)
  ParamSIG_DF <- read.xlsx(ParamSIG_FILE, # Changer nom ParamSIG_FILE
                           sheet=sheet) %>%
    filter(!is.na(Buffer) | !is.na(Distance))


  Distance_DF <- filter(ParamSIG_DF,
                        !is.na(Distance)) %>%
    group_by(Distance) %>%
    summarise(Id=paste0(Id,collapse=";"),
              shp=paste0(shp,collapse=";")) %>%
    ungroup()
if (dim(Distance_DF)[1] > 0) {
  for (j in 1:dim(Distance_DF)[1]) {
    # j=1
    # # Poids
    Poids <- Distance_DF$Distance[j]

    ListShp_name <- str_split(Distance_DF$shp[j],";")
    ListShp_name <- unlist(ListShp_name)
    ListShp_id <- str_split(Distance_DF$Id[j],";")
    ListShp_id <- unlist(ListShp_id)


    pos <- which(duplicated(ListShp_id))
    if (length(pos) > 0) {
      stop(paste0("Shape ",
                  paste0(ListShp_name[pos],collapse=", "),
                  " détecté plusieurs fois dans la feuille ",
                  sheet,". Risque d'erreur générée par des doublons de couches portant le même identifiant"))
    }




    # # On récupère les shapes concernés (dans ListSHP3)
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
    #   ListShp_id <- ListShp_id[-pos_Error]
    #   ListShp_name <- ListShp_name[-pos_Error]
    #   }


    # Etape 1 : pour le nbre niveaux, les buffers et les poids (j) on fusionne tous les éléments (polygones)
    Count=0
    for (i in 1:length(ListShp_id)) {

      # i=1
      # On ne s'intéresse pas aux buffer pour l'instant
      # shp <- ListSHP[[i]]
      # shp_id <- names(ListSHP[i])
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
        # shp_name <- ListShp_name[i]
        # id_test <- ListShp_id[i]
        # if (id_test!=shp_id) {
        #   stop(paste0("Erreur de correspondance des Id des shapes (shape: ",
        #               shp_name," -- Id/Id_Test: ",shp_id,"/",id_test,")"))
        # }


        print(paste0("Shape: ",shp_name," -- Id: ",shp_id))

        shp$Id <- (Count+1):(Count+dim(shp)[1])
        Count=Count+dim(shp)[1]
        shp <- shp[,"Id"]
        if (class(shp)=="SpatialLinesDataFrame" |
            class(shp)=="SpatialPointsDataFrame") {
          # Answer_Buffer <- tk_messageBox(type="yesno",
          #                                message=paste0("La couche SIG '",
          #                                               shp_name," (Id: ",shp_id,
          #                                               ")' est composée de lignes ou de points.\nRajouter un buffer pour les éléments du shapes ?\n(Si aucun buffer n'est ajouté, l'information contenue dans la couche ne sera pas prise en compte ici)"))
          # if (Answer_Buffer=="yes") {
          #   Buffer <- tk_select.list(1:1000,
          #                            title=paste0("Valeur du buffer à ajouter pour les éléments de la couche ",
          #                                         shp_name),
          #                            multiple=F)
          # Buffer <- 11
          # if (shp_name %in% c("LIGNE_ELECTRIQUE","RoutesSecondaires")) {
            Buffer <- 2.5*res_tile
          # }
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


    # # Ecriture du shape contenant tous les polygones à intégrer dans le layer (raster).
    # # maintenant que tous les éléments sont rassemblés, on passe ensuite à la création des buffers (selon les différents niveaux)

print("Repère")
    # 1. On unit les polygones du shape
    system.time(
      shp1 <- gUnaryUnion(shp_ToFill)
    )
    # writeOGR(shp_ToFill,
    #          dsn = "Out/Vecteurs/",
    #          layer=paste0(sheet,"_BuffTest"),
    #          driver="ESRI Shapefile",
    #          encoding="UTF-8",
    #          overwrite_layer = T)

  # }
  # shp_Sav2 <- shp_Impact
  # shp_Sav3 <- shp_Impact
  # shp_Impact <- shp_Sav3

  # Rasterisation
  # Raster Distance :
  rasDist_temp <- rasExt_RAS
  rasDist_temp <- setValues(rasDist_temp,0)


  # rasBuff_temp <- rasterize(shp_ToFill,rasExt_RAS,
  #                           field="Id",fun=sum)
  # rasDist_temp2 <- projectRaster(rasDist_temp,res=20,
  #                  crs=CRS("+init=epsg:2154"))
  # make values NA where polygon intesects raster
  system.time(
    rasDist_temp <- mask(rasDist_temp,shp1)
  )
  # plot(rasDist_temp)
  # run distance check
  system.time(
    rasDist_temp <- distance(rasDist_temp)
  )

  # Changement des valeurs de distance 0 en 1 pour éviter valeurs vides si Poids comporte inverse distance
  pos <- which(values(rasDist_temp)==0)
  if (length(pos) > 0) {
    values(rasDist_temp)[pos] <- 1 # on remplace les vides par 0
  }

  # Détection de l'expression 'distance' dans la variable poids (sans distinction majuscule ou minuscule) :
  Poids <- tolower(Poids)
  if (!str_detect(Poids,"distance")) {
    stop("Aucune expression impliquant la distance n'a été détectée dans la valeur du poids (classeur de paramétrage).\n
         Correction nécessaire (non pris en charge dans le programme")
  }
  Poids <- str_replace(Poids,"distance","rasDist_temp")

  rasDist_temp <- eval(parse(text=Poids))

  if (j==1) {
    rasDist_temp2 <- rasDist_temp
  } else {
  rasDist_temp2 <- rasDist_temp2 + rasDist_temp
  }
  # ras <- mask(ras,shp_Impact)

  # rE <- rD*(-1)

  # unique(values(rasBuff_RAS))
}

  Count_2=Count_2+1
  rasDist_RAS <- mask(rasDist_temp2,zone)
  # rasDist_RAS <- projectRaster(rasDist_RAS,
  #                             crs=CRS("+init=epsg:2154"))
  names(rasDist_RAS) <- paste0(sheet,"_Dist")
  dir.create(paste0("Out/SIG/Raster/",sheet),showWarnings=F)
  writeRaster(rasDist_RAS,
              file=paste0(rep_Ilots,"/Out/SIG/Raster/",sheet,"/",sheet,"_Dist"),
              format="GTiff",
              overwrite=T)
  # Voir plus bas pourquoi lignes ci-dessous sont en commentaire
  if (Count_2 > 1) {
    rasFin_RAS <- stack(rasFin_RAS,rasDist_RAS)
  } else {
    rasFin_RAS <- rasDist_RAS
  }
}

    # Conclusion : on rassemble tous les raster dans 1 seul
  # -> pour l'instant pas en place parce qu'on ne mélange pas le calcul des distances avec
  # les buffers
  }
  return(rasFin_RAS)
}
