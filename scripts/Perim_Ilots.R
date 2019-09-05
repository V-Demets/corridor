library(rgeos)
library(rgdal)
library(tidyverse)
library(raster)
library(xtable)
library(readxl)
library(tools)
library(tcltk)
library(openxlsx)
library(dplyr)
library(broom)
library(stringr)
library(ggmap)
library(ggplot2)
library(gdata)
library(gstat)
library(ggsn)
library(dplyr)
library(maptools)
library(sp)
library(easypackages)
libraries("tcltk", "rgdal", "openxlsx", "tools", "stringr", "dplyr", "reshape2", "Corridor", "raster", "rgeos")



rep_Ilots <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN"
setwd(rep_Ilots)


# -- Choix du fichier d'emprise :
setwd(rep_Ilots)
Bbox_FILE <- tk_choose.files(default=paste0(rep_Ilots,"/Data/SIG/Vecteurs/DataBrutes/Perimetres/Perimetre2014.shp"), #getwd(),
                             caption="Choix du shape d'emprise permettant de clipper et de reprojeter les différents shp",
                             multi=F,
                             filters=matrix(c(".shp",".shp"),
                                            nrow=1,ncol=2))
Bbox_SHP <- readOGR(dsn=dirname(Bbox_FILE),
                    layer=file_path_sans_ext(basename(Bbox_FILE)),
                    verbose=F,
                    stringsAsFactors=F) %>%
  spTransform(CRS("+init=epsg:2154")) # Forcer la couche d'emprise en L_93 ou laisser le choix ?

perim <- Bbox_SHP
zone <- gBuffer(perim, width=1000)



# Script règles de décision pour le dessin d'îlots de sénescence :
df_BASE3 <- rename_(ParamSHP_DF,
              "Thematique"="Thématique") %>%
  filter(Inclure_SHP=="Oui") %>%
  dplyr::select(Id,Source,shp,Inclure_SHP,Thematique,Encodage) %>%
  distinct() %>%

  mutate(Buffer=NA,
         Poids=NA,
         Nom_Layer=NA,
         Nbre_Niveau=NA,
         Inclure_Ilots="Oui",
         Detail_Valeurs="Non",
         Valeurs=NA)

# df_BASE_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/Excel/Table_Champs_SHP_VD4.xlsx"


# Création du classeur contenant les critères de décision pour la définition des ilots.
wb <- createWorkbook()
addWorksheet(wb,"CriteresIlots")

# Création des styles de cellules
Style1 <- createStyle(fontName="Arial", fontColour="black", border= "TopBottomLeftRight",
                      fgFill="lightskyblue", textDecoration="bold", wrapText=F,
                      valign="center",halign="center",
                      textRotation=0)
Style2 <- createStyle(fontName="Arial", wrapText=T,
                      valign="center", halign="center")

# Ecriture des données dans les feuilles correspondantes
addStyle(wb,
         sheet="CriteresIlots", Style1, rows=1, cols=1:dim(df_BASE3)[2],
         gridExpand=T)
addStyle(wb,
         sheet="CriteresIlots", Style2, rows=1+(1:dim(df_BASE3)[1]), cols=1:dim(df_BASE3)[2],
         gridExpand=T)
writeData(wb, "CriteresIlots", df_BASE3)
removeColWidths(wb, sheet = "CriteresIlots", cols = 1:dim(df_BASE3)[2])
setColWidths(wb, sheet = "CriteresIlots",
             cols = 1:dim(df_BASE3)[2], widths=rep("auto",dim(df_BASE3)[2]))

# Sauvegarde du classeur
# setwd(rep1)
saveWorkbook(wb,
             "Out/Excel/DecisionIlots.xlsx",
             overwrite=T)

# ----- Attribution poids par valeurs dans 1 colonne -----
# N.B : dans le classeur de paramètres, laisser choix possible entre valeurs d'attributs ? (ex : stations forestières)
# Il faut réimporter le classeur dans lequel ces éléments auront été précisés.
CriteresIlots_FILE <- tk_choose.files(default=paste0(rep_Ilots,"/Out/Excel/DecisionIlots.xlsx"),
                                caption="Choix du classeur 'DecisionIlots.xlsx' contenant les paramètres définissant les îlots de sénescence.",
                                multi=F,
                                filters=matrix(c(".xlsx",".xlsx"),
                                               nrow=1,ncol=2))
CriteresIlots_DF <- read.xlsx(CriteresIlots_FILE,
                              sheet="CriteresIlots")
CriteresIlots_DF2 <- filter(CriteresIlots_DF,
                            Detail_Valeurs!="Non")
  # ListSHP <- List_SHP2[names(List_SHP2) %in% CriteresIlots_DF$shp]

# Initialisation :
df <- data.frame(Valeurs=character(),
                 shp=character(),
                 Detail_Valeurs=character())

# A supprimer après réglage des problèmes d'encodage :
  # ListShp_FILES <- file.choose()
  ListShp_FILES <- c("/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Milieux/geol_zon_etud.shp",
                     "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Peuplt/PeupltIFN.shp",
                     "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Autres/ser100union.shp",
                     "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Stations/StationsONF.shp")
for (i in 1:length(CriteresIlots_DF2$shp)) {
  name_shp <- CriteresIlots_DF2$shp[i]
  print(name_shp)
  pos <- which(names(List_SHP2) %in% name_shp) # Par la suite supprimer pos car travail avec Id
  # shp <- List_SHP2[[pos[1]]]

  attrs <- CriteresIlots_DF2$Detail_Valeurs[i]

  # shp <- ListSHP[[2]]
  shp_FILE <- ListShp_FILES[i]
  shp <- readOGR(dsn=dirname(shp_FILE),
          layer=file_path_sans_ext(basename(shp_FILE)),
          verbose=F,
          stringsAsFactors=F)
  shp <- spTransform(shp,
                     CRS("+init=epsg:2154"))
  shp <- shp[zone,]

  df_temp <- select(shp@data,
                one_of(attrs)) %>%
    distinct() %>%
  # Penser à changer pour mettre l'ID de la couche et non pas juste le nom... !!!!
    mutate(shp=name_shp,
           Detail_Valeurs=attrs) %>%
    rename_("Valeurs"=attrs)
  df <- rbind(df,df_temp)
}
CriteresIlots_DF <- mutate(CriteresIlots_DF,
                           Valeurs=NULL) %>%
  left_join(df)

# ----- Réécriture du classeur de paramètres dans un version 2.0 -----
# Création du classeur contenant les critères de décision pour la définition des ilots.
wb <- createWorkbook()
addWorksheet(wb,"CriteresIlots")

# Création des styles de cellules
Style1 <- createStyle(fontName="Arial", fontColour="black", border= "TopBottomLeftRight",
                      fgFill="lightskyblue", textDecoration="bold", wrapText=F,
                      valign="center",halign="center",
                      textRotation=0)
Style2 <- createStyle(fontName="Arial", wrapText=T,
                      valign="center", halign="center")

# Ecriture des données dans les feuilles correspondantes
addStyle(wb,
         sheet="CriteresIlots", Style1, rows=1, cols=1:dim(CriteresIlots_DF)[2],
         gridExpand=T)
addStyle(wb,
         sheet="CriteresIlots", Style2, rows=1+(1:dim(CriteresIlots_DF)[1]), cols=1:dim(CriteresIlots_DF)[2],
         gridExpand=T)
writeData(wb, "CriteresIlots", CriteresIlots_DF)
removeColWidths(wb, sheet = "CriteresIlots", cols = 1:dim(CriteresIlots_DF)[2])
setColWidths(wb, sheet = "CriteresIlots",
             cols = 1:dim(CriteresIlots_DF)[2], widths=rep("auto",dim(CriteresIlots_DF)[2]))

# Sauvegarde du classeur
# setwd(rep1)
saveWorkbook(wb,
             "Out/Excel/DecisionIlots2.xlsx",
             overwrite=T)




# ----- Import des critères de choix pour les ilots : -----
CriteresIlots_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/DecisionIlots.xlsx"
CriteresIlots_DF <- read.xlsx(CriteresIlots_FILE,
                              sheet="CriteresIlots")

# Import des shapes déjà lus : (ATTENTION : s'assurer qu'à ce stade on a bien tous les shapes qui ont déjà été lus !)
# + s'assurer que les shapes ont bien été clippés.
load("Tables/Shapes_Rewrite.RData")

# -- Liste des shapes à considérer :
CriteresIlots_DF <- filter(CriteresIlots_DF,
                           Inclure_Ilots=="Oui") %>%
  arrange(Id)
ListLayer <- unique(CriteresIlots_DF$Nom_Layer)


# Initialisation : raster à remplir
ext <- extent(zone)
rasExt_RAS <- raster(ext)
projection(rasExt_RAS) <- proj4string(zone) # projection
res(rasExt_RAS)=20

for (layer in ListLayer) { # Au final : il faut 1 valeur de niveaux et 1 définition des buffer pour chaque layer !
  # layer <- ListLayer[1]
  # layer <- ListLayer[2]
  # layer <- ListLayer[3]
  ListSHP_Name <- CriteresIlots_DF$shp[which(CriteresIlots_DF$Nom_Layer %in% layer)]
  # --- Paramètres du layer :
  # Nombre de niveaux d'impact
  Niveaux <- unique(CriteresIlots_DF$Nbre_Niveau[which(CriteresIlots_DF$Nom_Layer %in% layer)])
  # Buffers
  Buff_Width <- unique(CriteresIlots_DF$Buffer[which(CriteresIlots_DF$Nom_Layer %in% layer)]) # Virer les unique après avoir résolu le pblme d'ID des shapes.
  Buff_Width <- str_split(Buff_Width,";")
  Buff_Width <- unlist(Buff_Width,";")
  # Poids
  Poids <- unique(CriteresIlots_DF$Poids[which(CriteresIlots_DF$Nom_Layer %in% layer)])
  Poids <- str_split(Poids,";")
  Poids <- unlist(Poids,";")

  if (length(Buff_Width) != Niveaux) { # (Sécurité temporaire)
    stop("Nombre de valeurs de buffer incohérentes avec le nombre de niveaux à construire.
         Valeurs de buffers différentes pour des shapes de même nom. A régler")
  }

  # On récupère les shapes concernés (dans ListSHP2)
  ListSHP <- List_SHP2[names(List_SHP2) %in% ListSHP_Name]


Count=0
  for (i in 1:length(ListSHP)) { # Etape 1 : on fusionne tous les éléments (polygones) qui font partie du layer
                         # On ne s'intéresse pas aux buffer pour l'instant
    shp <- ListSHP[[i]]
    name_shp <- names(ListSHP[i])
    # shp <- ListSHP[[2]]
    # shp <- ListSHP[[Count]]
    shp <- shp[zone,] # Sécurité pour vérifier l'emprise.
    print(i)
    shp$Id <- (Count+1):(Count+dim(shp)[1])
    Count=Count+dim(shp)[1]
    shp <- shp[,"Id"]
    if (class(shp)=="SpatialLinesDataFrame" |
        class(shp)=="SpatialPointsDataFrame") {
      Answer_Buffer <- tk_messageBox(type="yesno",
                                     message=paste0("La couche SIG '",
                                                    name_shp,
                                                    "' est composée de lignes ou de points.\nRajouter un buffer pour les éléments du shapes ?\n(Si aucun buffer n'est ajouté, l'information contenue dans la couche ne sera pas prise en compte ici)"))
      if (Answer_Buffer=="yes") {
        Buffer <- tk_select.list(1:1000,
                                 title=paste0("Valeur du buffer à ajouter pour les éléments de la couche ",
                                              name_shp),
                                 multiple=F)
      }
      shp <- buffer(shp,width=Buffer,
                    dissolve=F)
      # shp <- gUnaryUnion(shp)
      # shp <- disaggregate(shp)
      # shp <- disaggregate(shp)
      # df <- data.frame(Surface=gArea(shp, byid=T))
      # row.names(df) <- 1:dim(df)[1]
      # shp <- SpatialPolygonsDataFrame(shp,
      #                                        data=df)
    }
    if (i==1) {
      shp_ToFill <- shp
    } else {
      shp_ToFill <- rbind(shp_ToFill,shp)
    }
  }
 # Ecriture du shape contenant tous les polygones à intégrer dans le layer (raster).
   writeOGR(shp_ToFill,
           dsn = "Out/Vecteurs/",
           layer=paste0(layer,"_Buff0"),
           driver="ESRI Shapefile",
           encoding="UTF-8",
           overwrite_layer = T)
  # maintenant que tous les éléments sont rassemblés, on passe ensuite à la création des buffers (selon les différents niveaux)


   # 1. On unit les polygones du shape
   shp1 <- gUnaryUnion(shp_ToFill)
   for (niv in 1:Niveaux) {
     # niv <- 1
     print(paste0("Buffer : ",niv))
     # 2. On applique le buffer
     # N.B : Si buffer basé sur la surface => expression => NA apparaîtront après conversion en numérique
     if (!is.na(as.numeric(Buff_Width[niv]))) { # On est sur une valeur numérique
       shp2 <- buffer(shp1,width=as.numeric(Buff_Width[niv]),
                      dissolve=T)
     } else { # on est sur une expression
       shp2 <- shp_Impact
       shp2@data <- mutate(shp2@data,
                           Buffer=Surface^0.35)
       system.time(
         shp2 <- buffer(shp2,width=shp2@data$Buffer,
                        dissolve=F)
       )
       shp2 <- gUnaryUnion(shp2)
     }

     # 3. Récupérer les polygones formés par les buffer
     shp_Impact <- disaggregate(shp2)
     df <- data.frame(Surface=gArea(shp_Impact, byid=T))
     row.names(df) <- 1:dim(df)[1]
     shp_Impact <- SpatialPolygonsDataFrame(shp_Impact,
                                            data=df)
     shp_Impact@data <- mutate(shp_Impact@data,
                               Poids=as.numeric(Poids[niv]))
     # shp_Sav1 <- shp_Impact
     # shp_Sav2 <- shp_Impact
     # shp_Sav3 <- shp_Impact
     # shp_Impact <- shp_Sav3
     writeOGR(shp_Impact,
              dsn = "Out/Vecteurs/",
              layer=paste0(layer,"_Buff",niv),
              driver="ESRI Shapefile",
              encoding="UTF-8",
              overwrite_layer = T)


     # Raster Distance :
     ras <- rasExt_RAS
     ras <- setValues(ras,0)

     # make values NA where polygon intesects raster
     ras <- mask(ras,shp_Impact)

     # run distance check
     rD <- distance(ras)

     ras <- mask(ras,shp_Impact)

     writeRaster(rD,
                 file=paste0(rep_Ilots,"/Out/Raster/TestConnexion_Ilots.tif"),
                 format="GTiff",
                 overwrite=T)
























     # Rasterisation
     rasBuff_temp <- rasterize(shp_Impact,rasExt_RAS,
                               field="Poids",fun=sum)

     pos <- which(is.na(values(rasBuff_temp)))
     if (length(pos) > 0) {
       values(rasBuff_temp)[pos] <- 0 # on remplace les vides par 0
     }
     names(rasBuff_temp) <- paste0(layer,"_Buff",niv)

     if (niv==1 & match(layer,ListLayer)==1) {
       rasBuff_RAS <- rasBuff_temp
     } else {
       rasBuff_RAS <- rasBuff_RAS + rasBuff_temp
     }
     # unique(values(rasBuff_RAS))
   }

}
   writeRaster(rasBuff_RAS,
               file=paste0(rep_Ilots,"/Out/Raster/TestBufferFIN_Ilots.tif"),
               format="GTiff",
               overwrite=T)






















###### ----- BROUILLON ----- #####
   # ----- Deuxième partie Brouillon -----
   # 2ème type d'influence à traduire: proximité avec les axes de communication (routes, lignes électriques, cours d'eau)
   # Idée = construiredes buffers autour des éléments concernés.


   # ----- Import des critères de choix pour les ilots : -----
   CriteresIlots_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/DecisionIlots.xlsx"
   CriteresIlots_DF <- read.xlsx(CriteresIlots_FILE,
                                 sheet="CriteresIlots")


   ListSHP_Name <- CriteresIlots_DF$shp[which(CriteresIlots_DF$Nom_Layer %in% layer)]
   # --- Paramètres du layer :
   # Nombre de niveaux d'impact
   Niveaux <- unique(CriteresIlots_DF$Nbre_Niveau[which(CriteresIlots_DF$Nom_Layer %in% layer)])
   # Buffers
   Buff_Width <- unique(CriteresIlots_DF$Buffer[which(CriteresIlots_DF$Nom_Layer %in% layer)]) # Virer les unique après avoir résolu le pblme d'ID des shapes.
   Buff_Width <- str_split(Buff_Width,";")
   Buff_Width <- unlist(Buff_Width,";")
   # Poids
   Poids <- unique(CriteresIlots_DF$Poids[which(CriteresIlots_DF$Nom_Layer %in% layer)])
   Poids <- str_split(Poids,";")
   Poids <- unlist(Poids,";")

   if (length(Buff_Width) != Niveaux) { # (Sécurité temporaire)
     stop("Nombre de valeurs de buffer incohérentes avec le nombre de niveaux à construire.
          Valeurs de buffers différentes pour des shapes de même nom. A régler")
   }

   # On récupère les shapes concernés (dans ListSHP2)
   ListSHP <- List_SHP2[names(List_SHP2) %in% ListSHP_Name]


   Count=0
   for (i in 1:length(ListSHP)) { # Etape 1 : on fusionne tous les éléments (polygones) qui font partie du layer
     # On ne s'intéresse pas aux buffer pour l'instant
     shp <- ListSHP[[i]]
     name_shp <- names(ListSHP[i])
     # shp <- ListSHP[[2]]
     # shp <- ListSHP[[Count]]
     shp <- shp[zone,] # Sécurité pour vérifier l'emprise.
     print(i)
     shp$Id <- (Count+1):(Count+dim(shp)[1])
     Count=Count+dim(shp)[1]
     shp <- shp[,"Id"]
     if (class(shp)=="SpatialLinesDataFrame" |
         class(shp)=="SpatialPointsDataFrame") {
       Answer_Buffer <- tk_messageBox(type="yesno",
                                      message=paste0("La couche SIG '",
                                                     name_shp,
                                                     "' est composée de lignes ou de points.\nRajouter un buffer pour les éléments du shapes ?\n(Si aucun buffer n'est ajouté, l'information contenue dans la couche ne sera pas prise en compte ici)"))
       if (Answer_Buffer=="yes") {
         Buffer <- tk_select.list(1:1000,
                                  title=paste0("Valeur du buffer à ajouter pour les éléments de la couche ",
                                               name_shp),
                                  multiple=F)
       }
       shp <- buffer(shp,width=Buffer,
                     dissolve=F)
       # shp <- gUnaryUnion(shp)
       # shp <- disaggregate(shp)
       # shp <- disaggregate(shp)
       # df <- data.frame(Surface=gArea(shp, byid=T))
       # row.names(df) <- 1:dim(df)[1]
       # shp <- SpatialPolygonsDataFrame(shp,
       #                                        data=df)
     }
     if (i==1) {
       shp_ToFill <- shp
     } else {
       shp_ToFill <- rbind(shp_ToFill,shp)
     }
   }























   # ----- Première partie Brouillon -----
  # for (name in ListSHP_Name) {
  #   # name <- ListSHP_Name[1]
  #   ListSHP_ID <- CriteresIlots_DF$Id[which(CriteresIlots_DF$Nom_Niveau %in% layer &
  #                                             CriteresIlots_DF$shp %in% name)]
  #   ListSHP <- List_SHP2[names(List_SHP2) %in% name]
  #   # Initialisation : création du shape conteneur (vide) :
  #   shp_ToFill <- c()
  #   for (shp in ListSHP) { # Souci sur l'ID des shapes lus -> pas de distinction possible depuis le classeur de Critères (que faire si 2 buffer différents pour un même nom de couche ?)
  #     # shp <- ListSHP[[1]]
  #
  #
  #     for (niv in 1:Niveaux) {
  #       # niv <- 1
  #     # 1. On unit les polygones du shape
  #     shp1 <- gUnaryUnion(shp)
  #     # 2. On applique le buffer
  #     # N.B : Si buffer basé sur la surface => expression => NA apparaîtront après conversion en numérique
  #     if (!is.na(as.numeric(Buff_Width[niv]))) { # On est sur une valeur numérique
  #       shp2 <- buffer(shp1,width=as.numeric(Buff_Width[niv]),
  #                      dissolve=T)
  #     } else { # on est sur un expression
  #       shp2 <- shp_Impact
  #       shp2@data <- mutate(shp2@data,
  #                           Buffer=Surface^0.35)
  #       system.time(
  #       shp2 <- buffer(shp2,width=shp2@data$Buffer,
  #                      dissolve=F)
  #       )
  #       shp2 <- gUnaryUnion(shp2)
  #     }
  #
  #     # 3. Récupérer les polygones formés par les buffer
  #     shp_Impact <- disaggregate(shp2)
  #     df <- data.frame(Surface=gArea(shp_Impact, byid=T))
  #     row.names(df) <- 1:dim(df)[1]
  #     shp_Impact <- SpatialPolygonsDataFrame(shp_Impact,
  #                                            data=df)
  #     shp_Impact@data <- mutate(shp_Impact@data,
  #                               Poids=as.numeric(Poids[niv]))
  #     shp_Sav3 <- shp_Impact
  #     # shp_Sav2 <- shp_Impact
  #     # shp_Sav <- shp_Impact
  #
  #     # Ecriture pour vérification :
  #     writeOGR(shp_Sav,
  #              dsn = "Out/Vecteurs/",
  #              layer="shp_Sav",
  #              driver="ESRI Shapefile",
  #              encoding="UTF-8",
  #              overwrite_layer = T)
  #     writeOGR(shp_Sav2,
  #              dsn = "Out/Vecteurs/",
  #              layer="shp_Sav2",
  #              driver="ESRI Shapefile",
  #              encoding="UTF-8",
  #              overwrite_layer = T)
  #     writeOGR(shp_Sav3,
  #              dsn = "Out/Vecteurs/",
  #              layer="shp_Sav3",
  #              driver="ESRI Shapefile",
  #              encoding="UTF-8",
  #              overwrite_layer = T)
  #     }
  #   }
  # }
}
ListSHP_Ilots <- unique(CriteresIlots_DF$shp)

# Test sur shapes et buffer :
load("Tables/Shapes_Rewrite.RData")
unique(ParamSHP_DF$shp[which(ParamSHP_DF$Id %in% names(List_SHP2))]) # Archives contenant les shapes déjà
                                                                     # lus à revoir
# list <- c("TERRAIN_SPORT","RESERVOIR","RESERVOIR_EAU","POSTE_TRANSFORMATION","PISTE_AERODROME","GARE",
#           "CONSTRUCTION_LEGERE","CIMETIERE","BATI_INDUSTRIEL","BATI_REMARQUABLE","BATI_INDIFFERENCIE",
#           "AIRE_TRIAGE")

listshp <- List_SHP2[sort(names(List_SHP2)) %in% list]

# shp1 <- listshp[[1]]
shp2 <- listshp[[2]]
# shp22 <- listshp[[2]]


# Emprise plus petite pour travail
TestExtent_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DefIlotsTest_Emprise.shp"
TestExtent_SHP <- readOGR(dsn=dirname(TestExtent_FILE),
                          layer=file_path_sans_ext(basename(TestExtent_FILE)),
                          verbose=F,
                          stringsAsFactors=F)
TestExtent_SHP <- spTransform(TestExtent_SHP,
                              CRS("+init=epsg:2154"))
TestExtent2_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DefIlotsTest_Emprise2.shp"
TestExtent2_SHP <- readOGR(dsn=dirname(TestExtent2_FILE),
                          layer=file_path_sans_ext(basename(TestExtent2_FILE)),
                          verbose=F,
                          stringsAsFactors=F)
TestExtent2_SHP <- spTransform(TestExtent2_SHP,
                              CRS("+init=epsg:2154"))


# Affichage des résultats (attention : technique alternative à crop ne semble pas fonctionner)
# shp1 <- crop(shp1,TestExtent_SHP)
shp2 <- crop(shp2,TestExtent_SHP)
# shp2 <- crop(shp2,TestExtent2_SHP)

# par(mfrow=c(1,1),
#     mai=c(0,0,0,0))
# plot(shp2)
# plot(shp1,add=T)

# library(broom)
# shp2_DF1 <- data.frame(id=row.names(shp2),
#                             shp2@data,
#                             stringsAsFactors=F)
# shp2_DF2 <- tidy(shp2)
# shp2_DF <- left_join(shp2_DF1,shp2_DF2)


# Ajout Buffer
# Buff_shp2 <- gBuffer(shp2,
#                      width=100,byid=T)
# Buff_shp2_DF1 <- data.frame(id=row.names(Buff_shp2),
#                        Buff_shp2@data,
#                        stringsAsFactors=F)
# Buff_shp2_DF2 <- tidy(Buff_shp2)
# Buff_shp2_DF <- left_join(Buff_shp2_DF1,Buff_shp2_DF2)
# Buff2_shp2 <- gBuffer(shp2,
#                      width=100)
# Buff_shp2_DF1 <- data.frame(id=row.names(Buff2_shp2),
#                             Buff_shp2@data,
#                             stringsAsFactors=F)
# # Buff_shp2_DF1 <- data.frame(id=row.names(Buff_shp2),
# #                             Buff_shp2@data,
# #                             stringsAsFactors=F)
# Buff2_shp2_DF <- tidy(Buff2_shp2)
# # Buff_shp2_DF <- left_join(Buff_shp2_DF1,Buff_shp2_DF2)
# ggplot() +
#   geom_polygon(Buff2_shp2_DF,
#                mapping=aes(long,lat,group=group),
#                colour="gold",
#                fill=NA) +
#   geom_polygon(Buff_shp2_DF,
#                mapping=aes(long,lat,group=group),
#                colour="red",
#                fill=NA) +
#   geom_polygon(shp2_DF,
#                mapping=aes(long,lat,group=group),
#                colour="black")



# shp2Bis <- gUnaryUnion(shp22)
shp2Bis <- gUnaryUnion(shp2)
system.time(
Buff2_shp2 <- buffer(shp2Bis,
                     width=20,
                      dissolve=T)
)
# plot(Buff2_shp2)
urbainImpact1 <- disaggregate(Buff2_shp2)
tab <- data.frame(Impact = gArea(urbainImpact1, byid=T)^0.35) %>%
  mutate(Impact = replace(Impact, Impact>250, 250))
row.names(tab) <- 1:dim(tab)[1]
urbainImpact1 <- SpatialPolygonsDataFrame(urbainImpact1, data=tab)

urbainImpact2 <- gBuffer(urbainImpact1, width=urbainImpact1$Impact, byid=T)
urbainImpact2 <- gUnaryUnion(urbainImpact2)
plot(urbainImpact2)
plot(urbainImpact1,add=T,col="green")
# plot(shp2Bis,add=T)
# shp3 <- gUnaryUnion(Buff_shp2)
shp3 <- Buff2_shp2

polys <- shp3@polygons[[1]]@Polygons
pl <- vector("list", length(polys))
for (i in 1:length(polys)) {
  pl[i] <- Polygons(list(polys[[i]]), i)
  }
b.spolys <- SpatialPolygons(pl)
# plot(Buff2_shp2)
# for (i in 1:length(b.spolys)) {
#   plot(b.spolys[i],
#        add=T, col="red")
# }
row.ids <- sapply(slot(b.spolys, "polygons"), function(i) slot(i, "ID"))
b.exploded <- SpatialPolygonsDataFrame(b.spolys, data.frame(FID=as.numeric(row.ids)))
b.exploded@data <- mutate(b.exploded@data,
                          Surface_BUFFER=gArea(b.exploded,byid=T)/10000)
b.exploded@proj4string <- CRS("+init=epsg:2154")
b.exploded@proj4string <- shp2@proj4string
shp4 <- over(shp2,b.exploded)
shp2@data <- mutate(shp2@data,
               Surface_Poly=gArea(shp2,byid=T)/10000,
               Id_Poly=row.names(shp2))
shp6 <- cbind(shp2@data,shp4) %>%
  group_by(FID) %>%
  mutate(Count=length(FID),
         Density=Surface_Poly/Surface_BUFFER,
         Surface_PolyCum=sum(Surface_Poly)) %>%
  ungroup() %>%
  mutate(Buffer2=sqrt(Surface_PolyCum*10000)^1.25) # Voir avec max pour trouver le bon facteur multiplicateur
max(shp6$Buffer2)
min(shp6$Buffer2)
# row.names(shp6) <- shp6$Id_Poly

shp2@data <- mutate(shp2@data,
                    Surface_Poly=NULL)
shp7 <- merge(shp2,shp6)
shp8 <- gBuffer(shp7,
                width=shp7$Buffer2,
                byid=T)
shp9 <- gUnaryUnion(shp8) # shape buffer final

# shp11 <- shp9
# plot(shp10)
# plot(shp11,add=T)

# save(shp10,shp11,
#      file="Tables/TestShapes_Ilots.RData")
# plot(Buff2_shp2,add=T,col="green")
# plot(shp2,add=T,col="red")


# ----- Transformation du shape buffer final pour écriture -----
Buff_Polygons <- shp9@polygons[[1]]@Polygons
ListBuff_Polygons <- vector("list", length(Buff_Polygons))
for (i in 1:length(Buff_Polygons)) {
  ListBuff_Polygons[i] <- Polygons(list(Buff_Polygons[[i]]), i)
}
Buff_SP <- SpatialPolygons(ListBuff_Polygons)
# plot(Buff2_shp2)
# for (i in 1:length(b.spolys)) {
#   plot(b.spolys[i],
#        add=T, col="red")
# }
Buff_Ids <- sapply(slot(Buff_SP, "polygons"), function(i) slot(i, "ID"))
Buff_SPDF <- SpatialPolygonsDataFrame(Buff_SP, data.frame(FID=as.numeric(Buff_Ids)))
Buff_SPDF@data <- mutate(Buff_SPDF@data,
                          Surface_BUFFER=gArea(Buff_SPDF,byid=T)/10000,
                         Poids=-100) # Attribution du poids souhaité.
Buff_SPDF@proj4string <- CRS("+init=epsg:2154")
Buff_SPDF@proj4string <- shp9@proj4string
Buff_SHP <- Buff_SPDF

# dir.create("Out/Vecteurs/")
# writeOGR(Buff_SHP,
#          dsn = "Out/Vecteurs/",
#          layer="Buff_SHP2",
#          driver="ESRI Shapefile",
#          encoding="UTF-8",
#          overwrite_layer = T)



# Bbox_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Perimetres/Perimetre2014.shp"
# Bbox_SHP <- readOGR(dsn=dirname(Bbox_FILE),
#                     layer=file_path_sans_ext(basename(Bbox_FILE)),
#                     verbose=F,
#                     stringsAsFactors=F) %>%
#   spTransform(CRS("+init=epsg:2154"))
# zone <- gBuffer(Bbox_SHP, width=1000)
ext <- extent(zone)
rasBuff_RAS <- raster(ext)
projection(rasBuff_RAS) <- proj4string(zone) # projection
res(rasBuff_RAS)=20
rasBuff_temp <- rasterize(Buff_SHP,rasBuff_RAS,
                          field="Poids")
names(rasBuff_temp) <- "BATI_INDUSTRIEL"
# plot(rasBuff_temp)
# plot(Buff_SHP,add=T)
# writeRaster(rasBuff_temp,
#             file=paste0(rep_Ilots,"/Out/Raster/TestBuffer2_Ilots.tif"),
#             format="GTiff",
#             overwrite=T)

# rasBuff_temp1 <- rasBuff_temp
rasBuff_temp2 <- rasBuff_temp

save(rasBuff_temp1,rasBuff_temp1,
     file="Tables/TestRasterIlots.RData")

# rasBuff_temp1 <- raster(paste0(rep_Ilots,"/Out/Raster/TestBuffer_Ilots.tif"))
# names(rasBuff_temp1) <- "BATI_INDUSTRIEL"
# rasBuff_temp1 <- projectRaster(rasBuff_temp1,
#                                CRS("+init=epsg:2154"))
#
# rasBuff_temp2 <- raster(paste0(rep_Ilots,"/Out/Raster/TestBuffer2_Ilots.tif"))
# names(rasBuff_temp2) <- "BATI_INDUSTRIEL"
# rasBuff_temp2 <- spTransform(rasBuff_temp2,
#                              res=20,
#                              crs=CRS("+init=epsg:2154"))

# Test si somme possible entre rasters :
TestExtent3_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DefIlotsTest_EmpriseFIN.shp"
TestExtent3_SHP <- readOGR(dsn=dirname(TestExtent3_FILE),
                           layer=file_path_sans_ext(basename(TestExtent3_FILE)),
                           verbose=F,
                           stringsAsFactors=F)
TestExtent3_SHP <- spTransform(TestExtent3_SHP,
                               CRS("+init=epsg:2154"))


load("Tables/TestRasterIlots.RData")

rasBuff_temp1 <- crop(rasBuff_temp1,TestExtent3_SHP)
pos <- which(is.na(values(rasBuff_temp1)))
# a1 <- values(rasBuff_temp1)[pos]
# a2 <- values(rasBuff_temp1)[-pos]
# length(rasBuff_temp1)
# length(a1)+length(a2)
if (length(pos) > 0) {
  values(rasBuff_temp1)[pos] <- 0 # on remplace les vides par 0
}
# rasBuff_temp2 <- rasBuff_temp2[TestExtent3_SHP,]
# rasBuff_temp2 <- crop(rasBuff_temp2,TestExtent3_SHP)
rasBuff_temp2 <- projectRaster(rasBuff_temp2,rasBuff_temp1)
pos <- which(is.na(values(rasBuff_temp2)))

if (length(pos) > 0) {
  values(rasBuff_temp2)[pos] <- 0 # on remplace les vides par 0
}

# rasBuff_temp1
# rasBuff_temp2
# TestExtent3_SHP@proj4string
# plot(TestExtent3_SHP)
# plot(rasBuff_temp1,add=T)
# plot(rasBuff_temp2,add=T)
# extent(rasBuff_temp1)
# extent(rasBuff_temp2)
ras_Buff_FIN <- rasBuff_temp1 + rasBuff_temp2
# ras_Buff_FIN <- addLayer(rasBuff_temp1,rasBuff_temp2)
# ras_Buff_FIN <- rasBuff_temp1
# ras_Buff_FIN <- ras_Buff_FIN + rasBuff_temp2 # Attention si on somme avec des valeurs vides => valeurs vides

# plot(rasBuff_temp1)
# plot(rasBuff_temp2,add=T)
# plot(ras_Buff_FIN)
# plot(rasBuff_temp1 + rasBuff_temp2)






writeRaster(ras_Buff_FIN,
            file=paste0(rep_Ilots,"/Out/Raster/TestBufferFIN_Ilots.tif"),
            format="GTiff",
            overwrite=T)

# BALEEEEEEEZEE !!!!!
Ensuite passer shp9 à la moulinette pour avoir un SPDF, puis lui attribuer la valeur prévue dans le classeur
Aussi, rendre accessible les paramètres pour la définition du buffer (ici on a pris 100m puis racine de (la
                                                                     surface cumulée (ha)*10000) puissance
                                                                     1.25)


# lire les shapes tour à tour, rajouter un buffer si indiqué, construire au final un zonage (vecteur/raster ?)
# des zones à éviter plus ou moins (poids entre les éléments à éviter ou au contraire à atteindre)


# 1 Lire les shapes

# 2 Ajouter les buffers

# 3 Ajuster la valeur interdite (négatif = à éviter, positif = à atteindre) -> Voir si intéressant de
# dissocier en 2 rasters les caractères "négatif" et "positif" ?

# 4 Rasterizer cette info nouvellement construite


# Travail sur résultat krigeage :
# Trouver une valeur de surface min à partir des estimateurs tirés de la couche Ilots_Sen déjà existants ?

# 1 Sélectionner une classe de hauteur suffisamment intéressante -> Attribuer une valeur positive.
# 2 A partir de cette sélection, éliminer un ensemble de zones trop peu denses.
# 3 Devrait rester un nombre limité de zones. NON. Au final faire la somme des rasters => zones > 0 sont les
# plus intéressantes.

Comment prendre en compte connectivité ? -> Buffer ?
