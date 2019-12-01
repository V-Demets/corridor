

# (Mise au propre et surtout dans l'ordre du script Ordre_SHP.R)


########## Initialisation ##########
# ----- Chargement des packages nécessaires
library(easypackages)
# libraries(
#   "tcltk", "openxlsx", "tools", "stringr", "dplyr", "reshape2", "Corridor", 
#   
#   "raster", "rgeos", "maptools", "ggplot2", "gdata", "gstat", "sf", "rgdal", "gstat", 
#   "PermGF", "PermPSDRF"
# )


# ----- 1. Liste des couches disponibles (shapes) et de leur chemin -----
res <- ListInfos()
rep_Ilots <- res[[1]] # repTravail
repDataBrutes <- res[[2]] # repDataBrutes
Path_DF <- res[[3]] # tableau # TODO : simplifier résultat fonction ListInfos -> nécessité de transformer list_raw_files_tree en data frame ?
setwd(rep_Ilots)
# df3 <- res[[1]]

# ----- 2. Création classeur listant les champs des tables attributaires des différents shapes -----
# Le classeur ne contient à la base qu'une feuille. Il doit ensuite être modifié pour indiquer les shapes
# à réécrire et surtout la répartition des shapes entre différents rasters
# res1 <- ListChamps1(res[[1]], res[[2]], res[[3]])
res1 <- ListChamps2(res[[1]], res[[2]], res[[3]])

# df_BASE_FILE <- tk_choose.files(
#   default = file.path(rep_Ilots, "/out/excel/"), 
#   caption = "Choix du classeur listant les champs des tables attributaires \u00E0 conserver dans la r\u00E9\u00E9criture des shapes.", 
#   multi = F, 
#   filters = matrix(c(".xlsx", ".xlsx"), nrow = 1, ncol = 2)
# )
# 
# df_BASE_FILE <- sub(
#   paste0(rep_Ilots, "/"), "", df_BASE_FILE
# )
# df_BASE <- read.xlsx(df_BASE_FILE, sheet = "Parametrage_SIG")
# ParamSHP_DF <- df_BASE # TODO : supprimer intermédiaire

# # Ensuite l'opérateur modifie les champs à inclure ou non. Créer une table à inclure par défaut.
# # Answer3 <- tk_messageBox(type = "yesno", 
# #                          message = "Existe-t-il une version modifiée du classeur 'Parametres_SIG.xlsx' à intégrer ?")
# # if (Answer3 == "yes") {
# # df_BASE_FILE <- tk_choose.files(default = paste0(rep_Ilots, "/Data/Excel/"), 
# #                                 caption = "Choix du classeur listant les champs des tables atributaires à conserver dans la réécriture des shapes.", 
# #                                 multi = F, 
# #                                 filters = matrix(c(".xlsx", ".xlsx"), 
# #                                                nrow = 1, ncol = 2))
# df_BASE_FILE <- tk_choose.files(
#   default = file.path(rep_Ilots, "/out/excel/"), # TODO : gérer le default (vide ou mettre le bon nom de fichier)
#   caption = "Choix du classeur listant les champs des tables atributaires à conserver dans la réécriture des shapes.", 
#   multi = F, 
#   filters = matrix(c(".xlsx", ".xlsx"), nrow = 1, ncol = 2)
# )
# 
# df_BASE <- read.xlsx(df_BASE_FILE, sheet = "Parametrage_SIG")
# # } else {
# #   df_BASE <- res1[[1]]
# #   df_BASE2 <- res1[[2]]
# # }

# ParamSHP_DF <- df_BASE
# ParamRAS_DF <- df_BASE2


  
#################### styles excel (ListChamps2) - checked till 5. ####################
# Création des styles de cellules # TODO : rassembler les styles -> faire 1 fonction pour écriture des classeurs (rassemble les différentes versions de classeur à écrire + les différents styles)
Style1 <- createStyle(
  fontName = "Arial", fontColour = "black", border = "TopBottomLeftRight", 
  fgFill = "lightskyblue", textDecoration = "bold", wrapText = F, 
  valign = "center", halign = "center", 
  textRotation = 0
)
Style2 <- createStyle(
  fontName = "Arial", wrapText = T, 
  valign = "center", halign = "center"
)

#################### / \ ####################
# TODO : faire une fonction pour 3.
# ----- fonction de nettoyage des noms
clean_names <- function(string) {
  string <- gsub(" ", "_", string, fixed = T)
  string <- gsub("'", "", string, fixed = T)
  string <- gsub("\u00EA", "e", string, fixed = T)
  string <- gsub("\u00E2", "a", string, fixed = T)
  string <- gsub("\u00E9", "e", string, fixed = T)
  string <- gsub("\u00E8", "e", string, fixed = T)
  string <- gsub("\u00FB", "u", string, fixed = T)
  string <- gsub("\u00EE", "i", string, fixed = T)
  string <- gsub("\u00F4", "o", string, fixed = T)
  
  # retour de la fonction clean_names
  return(string)
}
# ----- fonction d'écriture du classeur Parametrage_SIG_bis.xlsx
write_wb_2 <- function(
  df, sheet_list, 
  output_dir = "out/excel"
) {
  # -- création des dossiers nécessaires
  dir.create(output_dir, showWarnings = F, recursive = T)
  
  # -- création du classeur
  wb <- createWorkbook()
  
  for (sheet in sheet_list) {
    # sheet <- sheet_list[2] # debug
    # df <- df1 # debug
    sheet_name <- clean_names(sheet)
    
    wb_df <- df
    if (sheet != "Parametrage_SIG") {
      wb_df <- 
        df %>% 
        filter(str_detect(Thematique_RAS, sheet)) %>%
        select(Id, Source, shp, Encodage) %>% # dplyr:: # Commentaires supprimés -> génèrent des doublons
        distinct()
      
      col_names <- if (sheet == "Krigeage") {c(
        "Inclure_RASTER", "Intitule_ChampRaster", 
        "Valeur_ChampRaster", "Buffer"
      )} else {c(
        "Buffer", "Distance", "Nbre_Niveau",
        "Poids", "Detail_Valeurs", "Valeurs"
      )}
      wb_df[, col_names] <- NA
    }
    
    # -- création des feuilles nécessaires
    addWorksheet(wb, sheet)
    
    # -- écriture des données
    # writeData(wb, sheet, wb_df)
    writeData(wb, sheet, wb_df)
    
    # -- styles et mise en forme
    # styles
    addStyle(
      wb, 
      sheet = sheet, 
      Style1,
      rows = 1, cols = 1:dim(wb_df)[2],
      gridExpand = T
    )
    addStyle(
      wb, 
      sheet = sheet, 
      Style2,
      rows = 1 + (1:dim(wb_df)[1]), cols = 1:dim(wb_df)[2],
      gridExpand = T
    )
    # mise en forme
    removeColWidths(
      wb, 
      sheet = sheet, 
      cols = 1:dim(wb_df)[2]
    )
    setColWidths(
      wb, 
      sheet = sheet, 
      cols = 1:dim(wb_df)[2],
      widths = rep("auto", dim(wb_df)[2])
    )
  }
  
  # -- sauvegarde du classeur
  saveWorkbook(
    wb,
    file.path(output_dir, "Parametres_SIG_bis.xlsx"), # TODO : automatiser le nom des classeurs
    overwrite = T
  )
  
  # -- message
  wb_name <- "Parametres_SIG_bis"
  msg <- tk_messageBox(
    type = "ok", 
    message = paste0(
      "Le classeur '", wb_name, ".xlsx' a \u00E9t\u00E9 \u00E9crit \u00E0 l'emplacement : \n\n", 
      output_dir
    )
  )
}

# ----- 3. Création du classeur Parametrage (bis) avec une feuille par grand type de raster -----
# Une fois les feuilles de chaque raster créé, il est possible d'obtenir des détails sur certains attributs
# pour attribuer des poids différents à des valeurs différentes d'un même attribut. Mais pour cela, pour des
# question d'encodage, il faut importer les shapes, voire corriger l'encodage à utiliser.
parameter_wb_file <- tk_choose.files(
  default = file.path(rep_Ilots, "/out/excel/"), 
  caption = "Choix du classeur listant les champs des tables attributaires \u00E0 conserver dans la r\u00E9\u00E9criture des shapes.", 
  multi = F, 
  filters = matrix(c(".xlsx", ".xlsx"), nrow = 1, ncol = 2)
)

parameter_wb_step2 <- function(
  rep, wb_name
) {
  # rep <- rep1 # debug
  # -- extraction du chemin relatif
  parameter_wb_file <- sub(
    paste0(rep, "/"), "", parameter_wb_file
  )
  
  # -- lecture du classeur excel
  parameter_df <- 
    read.xlsx(
      parameter_wb_file, 
      sheet = "Parametrage_SIG"
    ) %>% 
    filter(!is.na(Thematique_RAS)) %>% 
    select(
      Id, Source, shp, Attributs, Encodage, 
      Reecrire_SHP, Union_Champ, Thematique_RAS, Commentaires
    )
  
  # -- sécurité pour détecter et mentionner les shapes qui ne sont pas rangés dans une thématique
  empty_thematique <- parameter_df %>% filter(is.na(Thematique_RAS))
  if (dim(empty_thematique)[1] > 0) {
    empty_nb <- length(unique(empty_thematique$Source))
    warning("il y a ", empty_nb, " couches .shp sans 'Thematique_RAS'" )
  }
  
  # -- liste des thématiques créées par l'opérateur dans le classeur Parametrage_SIG.xlsx (wb_name)
  sheet_list <- 
    unique(df1$Thematique_RAS) %>% 
    str_split("/") %>% 
    unlist() %>% 
    unique()
  sheet_list <- c("Parametrage_SIG", sheet_list) # TODO : replace Parametrage_SIG by argument
  
  # -- message de validation sur les thématiques détectées
  msg <- tk_messageBox(
    type = "yesno", 
    message = paste0(
      "A CONFIRMER :\nil y a ", 
      length(sheet_list), 
      " thématiques à traiter qui ont été détectées dans le classeur '",
      file, "'"
    ),
    icon = "warning"
  )
  if (msg == "no") stop("Confirmer les thématiques détectées pour continuer")
  
  # -- écriture du classeur "Parametrage_SIG_bis.xlsx" (paste0(wb_name, "_bis.xlsx"))
  write_wb_2(df1, sheet_list)
}















# ----- 4. Lecture des shapes et réécriture (réécriture à mettre en option ?) -----
##### Création des rasters nécessaire au krigeage des données ###

# --- Choix du fichier d'emprise ---
zone_file <- tk_choose.files( # zone_file = Zone_FILE
  default = "", #getwd(),
  caption = "Choix du shape d'emprise permettant de clipper et de reprojeter les diff\u00E9rents shp",
  multi = F,
  filters = matrix(c(".shp", ".shp"), nrow = 1, ncol = 2)
)
zone_file <- "data/raw/Perimetres/Perimetre2014.shp" # debug

zone <- 
  zone_file %>% 
  st_read(stringsAsFactors = FALSE, quiet = T)
# -- sécurités
if (is.na( st_crs(zone)["proj4string"] )) {
  stop("Aucune projection disponible pour le shape du périmètre (zone)")
}
if (is.na( st_crs(zone)["epsg"] )) {
  zone <- st_transform(zone, crs = 2154)
}
  
# zone <- readOGR( # zone = Zone_SHP
#   dsn = dirname(zone_file), 
#   layer = file_path_sans_ext(basename(zone_file)), 
#   verbose = F, 
#   stringsAsFactors = F
# ) %>%
#   spTransform(CRS("+init = epsg:2154")) # Forcer la couche d'emprise en L_93 ou laisser le choix ?

# if (is.null(zone) | class(zone)[1] != "SpatialPolygonsDataFrame") {
#   stop("Fichier d'emprise incorrect : (is.null(zone) | class(zone)[1] != 'SpatialPolygonsDataFrame'")
# }
if (is.null(zone) | class(zone)[1] != "sf") {
  stop("Fichier d'emprise incorrect : (is.null(zone) | class(zone)[1] != 'sf'")
}
# --- Rajout du buffer au périmètre d'étude ---
buffer_width = 1000 # buffer_width = Buffer_Width
# zone <- gBuffer(zone_shp, width = Buffer_Width)
zone_expanded <- st_buffer(zone, dist = buffer_width) # zone_expanded = zone

# ----- tableau pour système de projection manquant (ReecritureShape2.R)
# EPSG_df <- data.frame(
#   Label = c(
#     "EPSG:2154 - RGF93/Lambert-93", 
#     "EPSG:7421 - NTF(Paris)/Lambert Zone II", 
#     "EPSG:4326 - WGS84 (gps)", 
#     "EPSG:32632 - UTM 32N (gps téléphone)"
#   ), 
#   EPSG = c(2154, 7421, 4326, 32632), 
#   stringsAsFactors = F
# )

# ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_bis_V3.xlsx"
parameter_wb_file <- "out/excel/Parametres_SIG_bis_V3.xlsx" # debug

# --- Lecture des shapes et réécriture (réécriture à mettre en option ?) ---
# system.time(
#   ReecritureShape(
#     zone_shp = Bbox_SHP, 
#     ParamSIG_FILE, 
#     repDataBrutes = repDataBrutes, 
#     Buffer_Width = 1000, 
#     Path_DF
#   )
# )

# Bbox_SHP = zone
# system.time(
#   ReecritureShape(
#     zone_shp = zone, 
#     parameter_wb_file, 
#     repDataBrutes = repDataBrutes, 
#     Buffer_Width = 1000, 
#     Path_DF
#   )
# )
system.time(
  rewrite_shp(
    zone, parameter_wb_file, 
    rep = rep1
  )
)







# ----- 5. Réécriture du classeur ParamSIG_FILE si besoin de détails sur certains attributs -----
# Possibilité de refaire tourner l'édition du classeur indéfiniment
# N.B : dplyr moins performant que merge ici !!
# load("Tables/ShapesDF_ReecritureShape2.RData")
# TODO : faire en sorte d'updater l'archive read_shapes.Rdata (évite de gérer un objet (read_shp) trop gros en ajoutant au fur et à mesure des shapes)
# TODO : mettre une option pour savoir si on veut vraiment réécrire les shapes (pas utile si on peut y accéder sous une autre forme)
load("tables/read_shapes.Rdata")

# --- Fichier contenant les paramètres (vrai nom dansla fonction = Parametrage_SIG) ---
# ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_bis_V2.xlsx"
# ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_Ter_V2.xlsx"
# ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_bis_V3.xlsx"
# ListSheets <- getSheetNames(ParamSIG_FILE) # Liste des feuilles du classeur (<=> rasters)
parameter_wb_file <- tk_choose.files(
  default = file.path(rep_Ilots, "/out/excel/"), 
  caption = "Choix du classeur listant les champs des tables attributaires \u00E0 conserver dans la r\u00E9\u00E9criture des shapes.", 
  multi = F, 
  filters = matrix(c(".xlsx", ".xlsx"), nrow = 1, ncol = 2)
)
parameter_wb_file <- "/Users/Valentin/Travail/Outils/GitHub/corridor/out/excel/Parametres_SIG_Ter_V3.xlsx" # debug

parameter_wb_step3 <- function(
  rep, wb_name
) {
  # rep <- rep1 <- getwd()# debug
  # -- extraction du chemin relatif
  parameter_wb_file <- sub(
    paste0(rep, "/"), "", parameter_wb_file
  )
  # -- liste des feuilles du classeur excel
  list_sheets <- getSheetNames(parameter_wb_file) # list_sheets = ListSheets
  
  for (sheet in list_sheets) {
    # sheet <- list_sheets[2] # debug
    # print(sheet) # debug
    
    # -- Import des paramètres renseignés dans le classeur Excel ---
    # -- lecture du classeur excel - feuille !!sheet
    parameter_df <- # parameter_df = df_BASE
      read.xlsx(parameter_wb_file, sheet = sheet) #%>% 
      # filter(!is.na(Thematique_RAS)) %>% 
      # select(
      #   Id, Source, shp, Attributs, Encodage, 
      #   Reecrire_SHP, Union_Champ, Thematique_RAS, Commentaires
      # )
    # df_BASE <- read.xlsx(ParamSIG_FILE, sheet = sheet)
    # %>%
    #   mutate(Mark = 1)
    if (!is.element(sheet, c("Parametrage_SIG", "Krigeage"))) {
      shp_to_detail <- 
        parameter_df %>% 
        filter(!is.element(Detail_Valeurs, c("Non", NA))) %>%
        distinct(
          Id, Source, shp, Encodage, Detail_Valeurs, 
          .keep_all = T
        )
      
      # -- Initialisation :
      df <- data.frame(
        Id = character(), 
        Source = character(), 
        Valeurs = character(), 
        # Valeurs2 = character(), 
        shp = character(), 
        Detail_Valeurs = character()
      )
      
      if (dim(shp_to_detail)[1] > 0) {
        # ListShp_FILES <- c(
        #   "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Milieux/geol_zon_etud.shp",
        #   "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Peuplt/PeupltIFN.shp",
        #   "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Autres/ser100union.shp",
        #   "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Stations/StationsONF.shp"
        # )
        list_id_shp <- shp_to_detail$Id
        id_sf <- 
          read_shp[[1]] %>% 
          filter(id_sf %in% list_id_shp)
        attrs_sf <- 
          read_shp[[1]] %>% 
          left_join(read_shp[[2]], by = "id_sf")
        values_sf <- 
          id_sf %>% 
          left_join(read_shp[[3]], by = "id_sf")
        for (i in 1:length(shp_to_detail$shp)) {
          i = 1 # debug
          id_shp <- shp_to_detail$Id[i]
          src_shp <- shp_to_detail$Source[i]
          name_shp <- shp_to_detail$shp[i]
          print(paste0("Couche : ", name_shp, " --- Id = ", id_shp)) # debug
          # pos <- which(names(List_SHP2) %in% name_shp) # Par la suite supprimer pos car travail avec Id
          # shp <- List_SHP2[[pos[1]]]
          id_sf <- 
            read_shp[[1]] %>% 
            filter(id_sf == id_shp)
          values_sf <- 
            read_shp[[2]] %>% 
            right_join(id_sf)
          shp_df <- read_shp[][[1]] %>% 
            data_frame()
          
          attrs <- shp_to_detail$Detail_Valeurs[i]
          
          # # shp <- ListSHP[[2]]
          # shp_FILE <- ListShp_FILES[i]
          # shp <- readOGR(dsn = dirname(shp_FILE), 
          #                layer = file_path_sans_ext(basename(shp_FILE)), 
          #                verbose = F, 
          #                stringsAsFactors = F)
          # shp <- spTransform(shp, 
          #                    CRS("+init = epsg:2154"))
          # shp <- shp[zone, ]
          
          # df_temp <- select(shp@data, 
          #                   one_of(attrs)) %>%
          df_temp <- 
            shp_df %>% 
            # select(one_of(attrs)) %>% # dplyr::
            # distinct() %>%
            select(NATURE) %>% # dplyr::
            st_drop_geometry() %>% 
            distinct(NATURE, .keep_all = T) %>%
            # Penser à changer pour mettre l'ID de la couche et non pas juste le nom... !!!!
            mutate(
              Id = id_shp, 
              Source = src_shp, 
              shp = name_shp, 
              Detail_Valeurs = attrs
            ) %>%
            rename_("Valeurs" = attrs) %>%
            
            # mutate(Valeurs2 = Valeurs) %>%
            
            select(Id, Source, Valeurs, shp, Detail_Valeurs) # Valeurs2, # dplyr::
          df <- rbind(df, df_temp)
        }
        # df_BASE3 <- mutate(parameter_df, 
        #                   Valeurs = NULL) %>%
        #   left_join(df)
        parameter_df <- merge(parameter_df, df, all = T) %>% # ATTENTION IMPORTANT : défaillance de dplyr : full_join
          #   incapable de faire la correspondance entre certaines chaînes de charactères (trop longues ou ... ?)
          # . Pas de problème si chaîne comparées en dehors du tableau.
          
          # df_BASE4 <- full_join(parameter_df, df)
          # df_BASE5 <- inner_join(parameter_df, df)
          filter(!(!is.element(Detail_Valeurs, c("Non", NA)) & is.na(Valeurs))) %>%
          # filter(is.element(Detail_Valeurs, c("Non", NA)) | !is.na(Valeurs))) # notation équivalente
          filter(!is.element(Valeurs, c("#N/A"))) %>%
          select(one_of(
            "Id", "Source", "shp", "Encodage", 
            names(parameter_df)[!names(parameter_df) %in% c("Id", "Source", "shp", "Encodage")]
          )) # dplyr::
        # %>%
        #   df_BASE3 <- mutate(parameter_df, 
        #                      Valeurs = as.character(Valeurs), 
        #                      Valeurs = gsub(" ", "_", parameter_df$Valeurs, fixed = T)), 
        # Valeurs = gsub("'", "", Valeurs, fixed = T), 
        # Valeurs = gsub("\u00EA", "e", Valeurs, fixed = T), 
        # Valeurs = gsub("\u00E2", "a", Valeurs, fixed = T), 
        # Valeurs = gsub("\u00E9", "e", Valeurs, fixed = T), 
        # Valeurs = gsub("\u00E8", "e", Valeurs, fixed = T), 
        # Valeurs = gsub("\u00FB", "u", Valeurs, fixed = T), 
        # Valeurs = gsub("\u00EE", "i", Valeurs, fixed = T), 
        # Valeurs = gsub("\u00F4", "o", Valeurs, fixed = T))
      }
    }
    
    # -- Réécriture des tables dans le classeur de paramètres dans une version 2.0 --
    
    addWorksheet(wb, sheet)
    
    # Ecriture des données dans les feuilles correspondantes
    addStyle(
      wb, 
      sheet = sheet, 
      Style1, 
      rows = 1, cols = 1:dim(parameter_df)[2], 
      gridExpand = T
    )
    addStyle(
      wb, 
      sheet = sheet, 
      Style2, 
      rows = 1 + (1:dim(parameter_df)[1]), cols = 1:dim(parameter_df)[2], 
      gridExpand = T
    )
    
    writeData(wb, sheet, parameter_df)
    
    removeColWidths(wb, sheet = sheet, cols = 1:dim(parameter_df)[2])
    setColWidths(
      wb, sheet = sheet, 
      cols = 1:dim(parameter_df)[2], 
      widths = rep("auto", dim(parameter_df)[2])
    )
    
  }
  
  # -- Sauvegarde du classeur
  saveWorkbook(
    wb, 
    "Out/Excel/Parametres_SIG_Ter.xlsx", 
    overwrite = T
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  # -- lecture du classeur excel
  parameter_df <- 
    read.xlsx(
      parameter_wb_file, 
      sheet = "Parametrage_SIG"
    ) %>% 
    filter(!is.na(Thematique_RAS)) %>% 
    select(
      Id, Source, shp, Attributs, Encodage, 
      Reecrire_SHP, Union_Champ, Thematique_RAS, Commentaires
    )
  
  # -- liste des thématiques créées par l'opérateur dans le classeur Parametrage_SIG.xlsx (wb_name)
  sheet_list <- 
    unique(df1$Thematique_RAS) %>% 
    str_split("/") %>% 
    unlist() %>% 
    unique()
  sheet_list <- c("Parametrage_SIG", sheet_list) # TODO : replace Parametrage_SIG by argument
  
  # -- message de validation sur les thématiques détectées
  msg <- tk_messageBox(
    type = "yesno", 
    message = paste0(
      "A CONFIRMER :\nil y a ", 
      length(sheet_list), 
      " thématiques à traiter qui ont été détectées dans le classeur '",
      file, "'"
    ),
    icon = "warning"
  )
  if (msg == "no") stop("Confirmer les thématiques détectées pour continuer")
  
  # -- écriture du classeur "Parametrage_SIG_bis.xlsx" (paste0(wb_name, "_bis.xlsx"))
  write_wb_2(df1, sheet_list)
}


# --- Création du classeur contenant les critères de décision pour la définition des ilots ---
wb <- createWorkbook()

for (sheet in ListSheets) {
  # # --- Nom de la feuille ---
  # sheet = ListSheets[5]
  

##### / #####








##### 6. Création des rasters correspondant aux critères de décision #####
# ----- Utilise les données shapes (selon les indications du classeur Excel) + les autres
# ----- rasters (mnh, rayonnement) ###

# -- Répertoire de travail
setwd(rep_Ilots)

# -- Fonction CreateRaster
# raster <- CreateRaster(Zone_SHP = NULL, 
#              ParamRAS_DF = df_BASE2)
Bbox_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Perimetres/Perimetre2014.shp"
Bbox_SHP <- readOGR(
  dsn = dirname(Bbox_FILE), 
  layer = file_path_sans_ext(basename(Bbox_FILE)), 
  verbose = F, 
  stringsAsFactors = F
) %>%
  spTransform(CRS("+init = epsg:2154"))

raster <- CreateRaster3(
  Zone_SHP = Bbox_SHP, 
  ParamSIG_FILE = ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_Ter_V3.xlsx"
)
# raster <- ras_GRD2
save(
  raster, 
  file = "Tables/Raster_CreateRaster.RData"
)
load("Tables/Raster_CreateRaster.RData")








# ----- 7. Création du raster supportant le krigeage -----
rep_Ilots <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN"
setwd(rep_Ilots)
Bbox_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Perimetres/Perimetre2014.shp"
Bbox_SHP <- readOGR(
  dsn = dirname(Bbox_FILE), 
  layer = file_path_sans_ext(basename(Bbox_FILE)), 
  verbose = F, 
  stringsAsFactors = F
) %>%
  spTransform(CRS("+init = epsg:2154"))
Zone_SHP <- Bbox_SHP
raster <- CreateRasterKrigeage(
  Zone_SHP = Bbox_SHP, 
  # ParamRAS_DF = df_BASE2, 
  ParamSIG_FILE = ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_Ter_V3.xlsx"
)
# raster_FILE <- paste0(rep_Ilots, "/Out/Raster/Krigeage/RasterKrigeage.tif")
# raster <- brick(raster_FILE)
# raster <- ras_GRD
# raster <- ras_GRD2
save(
  raster, 
  file = "Tables/Raster_CreateRasterKrigeage.RData"
)
load("Tables/Raster_CreateRasterKrigeage.RData")









# ----- 8. Rajout des couches du MNH et du rayonnement -----

# - MNH :
mnh_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Rasters/MNH/mnh.tif"
mnh_RAS0 <- raster(mnh_FILE)

mnh_RAS1 <- projectRaster(
  mnh_RAS0, raster, 
  crs = CRS("+init = epsg:2154")
)
raster2 <- stack(mnh_RAS1, raster)

# - Rayonnement :
rayon_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Rasters/Milieux/radneb61_6/hdr.adf"
rayon_RAS <- raster(rayon_FILE)

rayon_RAS1 <- projectRaster(
  rayon_RAS, raster2, 
  crs = CRS("+init = epsg:2154")
)
# plot(rayon_RAS1)
raster2 <- stack(rayon_RAS1, raster2)

# Remplissage des valeurs vides (NA bloquent la fonction variogram du krigeage)
pos_NA <- which(is.na(values(raster2)))

# ListVal <- unique(values(raster))
NA_VALUE <- -10
if (!is.element(NA_VALUE, values(raster2))){
  values(raster2)[pos_NA] <- NA_VALUE
} else {
  tk_messageBox(
    type = "ok", 
    message = paste0(
      "Attention : la valeur renseignée pour les valeurs vides (", 
      NA_VALUE, 
      ") figure déjà dans les valeurs du rasterBrick. A modifier."
    ), 
    icon = "warning"
  )
}
plot(raster2)

# - Sauvegarde du raster final
save(
  raster2, 
  file = "Tables/Raster_FIN.RData"
)
load("Tables/Raster_FIN.RData")









# Autres éléments ?









########## Traitement des données d'inventaire ##########

##### / #####
# ----- 9. Préparation des résultats d'inventaire par placettes - A terme cf fonction 'TraitementInventaires()' !!-----
# --- Définition du répertoire de travail
# rep_Ilots <- tk_choose.dir(default = getwd(), 
#                            caption = "Choix du répertoire de travail")
rep_Ilots <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN"
setwd(rep_Ilots)

# Choix du dossier contenant les fichiers d'inventaire
# rep_Data <- tk_choose.dir(default = "Data/Excel/Inventaires", #getwd(), 
#                           caption = "Choix du répertoire contenant les données vecteurs")
# rep_Data <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data"

# définir les tables à créer pour les besoins du krigeage
# ----- Traitement des données PSDRF -----
# --- Définition du répertoire contenant les classeurs d'inventaire :
rep_InvPSDRF <- tk_choose.dir(
  default = "Data/Excel/PSDRF", #getwd(), 
  caption = "Choix du répertoire contenant les classeurs d'inventaire PSDRF."
)
# --- Import des classeurs d'inventaire :
psdrf_Xls2Rdata(repPSDRF = rep_Ilots, repData = rep_InvPSDRF)

# --- Chargement des données administrateurs :
# PsdrfListes_FILE <- file.choose()
# psdrf_Codes(rep_Ilots, PsdrfListes_FILE)
load("Tables/psdrfCodes.Rdata")
# --- Vérification des données :
# psdrf_Verif(rep_Ilots, 
#             rep_InvPSDRF, 
#             "pdf")

# --- Calcul des variables par arbres
psdrf_Calculs(rep_Ilots)

# --- Agrégation des résultats par placettes
# -- Table TabCombi :
tBase <- c(
  "psdrfDispBM_", "psdrfDispBM_Essence", "psdrfDispBM_EssenceClasse", 
  "psdrfDispBM_StadeD", "psdrfDispBM_StadeDStadeE", "psdrfDispBM_StadeE", 
  "psdrfDispBMP_Classe", "psdrfDispBMP_ClasseType", "psdrfDispBMS_Classe", 
  "psdrfDispCodes_", "psdrfDispCodes_CodeEcolo", "psdrfDispCodes_CatCodeEcolo", 
  "psdrfDispDen_", "psdrfDispDen_Essence", "psdrfDispFpied_", 
  "psdrfDispFpied_Cat", "psdrfDispFpied_Classe", "psdrfDispFpied_Essence", "psdrfDispFpied_EssRegCat", 
  "psdrfDispFpied_EssenceCat", "psdrfDispFpied_EssRegParCat", 
  "psdrfDispPer_", "psdrfDispPer_Essence", "psdrfDispPer_EssReg", 
  "psdrfDispHabitatBM_", "psdrfDispHabitatBM_StadeD", "psdrfDispHabitatBMP_", 
  "psdrfDispHabitatBMS_", "psdrfDispHabitatFpied_", 
  "psdrfDispHabitatFpied_Classe", "psdrfDispHabitatTaillis_", 
  "psdrfDispHabitatTaillis_Classe", "psdrfDispRege_Essence", 
  "psdrfDispRege_EssRegPar", "psdrfDispTaillis_", "psdrfDispTaillis_Classe", 
  "psdrfDispTaillis_Essence", "psdrfDispTot_", "psdrfDispTot_Cat", 
  "psdrfDispTot_CatCodeEcolo", "psdrfDispTot_Essence", "psdrfDispTot_Classe", 
  "psdrfDispTot_Coupe", "psdrfDispTot_ClasseCoupe", "psdrfDispTot_CatCoupe", 
  "psdrfDispTot_EssenceClasse", "psdrfDispTot_EssRegParCat", "psdrfPlaFpied_", 
  "psdrfPlaFpied_Cat ", " psdrfPlaTaillis_Cat", "psdrfPlaTaillis_", 
  "psdrfPlaBM_", "psdrfPlaRege_", "psdrfPlaTot_", "psdrfPlaTot_EssReg", "psdrfPlaTot_Cat", 
  "psdrfDispPFutaie_", "psdrfDispPFutaie_Essence", "psdrfDispPFutaie_Classe", "psdrfDispPFutaie_Cat", 
  "psdrfDispExploit_", "psdrfDispExploit_Essence", "psdrfDispExploit_Classe", "psdrfDispExploit_Cat"
)

tBaseCarnet <- data.frame(
  var = tBase, 
  stringsAsFactors = F
)
tBaseCarnet <- 
  tBaseCarnet %>% 
  mutate( # dplyr::
    Var1 = ifelse(str_detect(var, "Essence"), "Essence", NA), 
    Var1 = ifelse(str_detect(var, "EssReg"), "EssReg", Var1), 
    Var1 = ifelse(str_detect(var, "EssRegPar"), "EssRegPar", Var1), 
    Var2 = ifelse(str_detect(var, "Classe"), "Classe", NA), 
    Var2 = ifelse(str_detect(var, "Cat"), "Cat", Var2), 
    Var3 = ifelse(str_detect(var, "CodeEcolo"), "CodeEcolo", NA), 
    Var4 = ifelse(str_detect(var, "Coupe"), "Coupe", NA), 
    Var5 = ifelse(str_detect(var, "StadeD"), "StadeD", NA), 
    Var6 = ifelse(str_detect(var, "StadeE"), "StadeE", NA), 
    Var7 = ifelse(str_detect(var, "Type"), "Type", NA), 
    Data = ifelse(str_detect(var, "BM"), "BM", NA), 
    Data = ifelse(str_detect(var, "BMP"), "BMP", Data), 
    Data = ifelse(str_detect(var, "BMS"), "BMS", Data), 
    Data = ifelse(str_detect(var, "Tot"), "Tot", Data), 
    Data = ifelse(str_detect(var, "Fpied"), "Fpied", Data), 
    Data = ifelse(str_detect(var, "Per"), "Per", Data), 
    Data = ifelse(str_detect(var, "Taillis"), "Taillis", Data), 
    Data = ifelse(str_detect(var, "Den"), "Den", Data), 
    Data = ifelse(str_detect(var, "PFutaie"), "PFutaie", Data), 
    Data = ifelse(str_detect(var, "Exploit"), "Exploit", Data), 
    Data = ifelse(str_detect(var, "Codes"), "Codes", Data), 
    Data = ifelse(str_detect(var, "Rege"), "Rege", Data), 
    var = NULL
  )
TabCombi_PSDRF <- distinct(tBaseCarnet)


psdrf_AgregArbres(rep_Ilots, TabCombi_PSDRF)
# for(i in 1:length(TabPla)) {assign(names(TabPla)[i], TabPla[[i]])}
# # -- Table résultats par placettes - Fpied :
# assign(names(TabPla)[43], TabPla[[43]])

# --- Ecriture des résultats sous la forme de shape :
psdrf_ShapesPlac(rep_Ilots)





# ----- Traitement des données GF -----
# --- Choix des classeurs d'inventaire :
ListFile_InvGF <- tk_choose.files(
  default = "", #getwd(), 
  caption = "Choix des classeurs contenant les données d'inventaire GF.", 
  filter = matrix(c(".xlsx", ".xlsx"), 1, 2)
)
# --- Import des classeurs d'inventaire :
gf_Xls2Rdata(rep_Ilots, ListFile_InvGF)

# --- Calcul des variables par arbres
gf_Calculs(rep_Ilots)

# --- Agrégation des résultats par placettes
# -- Table TabCombi :
tBase <- c(
  "gfForetBMP_", "gfForetBMP_Classe", "gfForetBMP_ClasseStadeD", 
  "gfForetBMP_StadeD", "gfForetBMP_StadeE", 
  "gfForetBMP_ClasseType", 
  "gfForetBMP_StadeDStadeE", "gfForetBMS_", "gfForetBMS_Classe", 
  "gfForetBMS_StadeDStadeE", 
  "gfForetBMS_ClasseStadeD", "gfForetBMS_StadeD", "gfForetBMS_StadeE", 
  
  "gfForetCodes_CatCodeEcolo", "gfForetCodes_CodeEcolo", 
  
  "gfForetDen_", "gfForetDen_EssReg", "gfForetDen_EssRegClasse", 
  "gfForetDen_Cat", "gfForetDen_Reg1", "gfForetDen_CatReg1", 
  "gfForetDen_Essence", "gfForetPer_Essence", "gfForetDen_EssenceCat", 
  
  "gfForetFpied_CatCodeEcolo", "gfForetFpied_", "gfForetFpied_Cat", 
  "gfForetFpied_CatReg1", "gfForetFpied_Classe", "gfForetFpied_ClasseQual", "gfForetFpied_ClasseReg1", 
  "gfForetFpied_Essence", "gfForetFpied_EssenceCat", "gfForetFpied_EssRegCat", 
  "gfForetFpied_Reg2", "gfForetPer_Classe", "gfForetFpied_ClasseCodeEcolo", 
  
  "gfForetPer_ClasseReg1", "gfForetPer_EssRegClasse", "gfPPTetrasPer_EssReg", 
  "gfForetPer_", 
  "gfForetRege_Essence", "gfForetRege_EssReg", "gfForetRege_", 
  "gfForetTaillis_", "gfForetTaillis_Essence", 
  "gfForetTaillis_EssReg", "gfForetTaillis_Classe", "gfForetTaillis_EssRegClasse", 
  
  "gfPlaBMP_", "gfPlaBMS_", "gfPlaDen_", "gfPlaFpied_", "gfPlaFpied_EssReg", 
  "gfPlaPerches_EssRegClasseReg1", "gfPlaDen_Cat", 
  "gfPlaRege_EssReg", "gfPlaTaillis_", "gfPlaTaillis_EssReg", 
  
  "gfPlaFpied_EssenceCatRep\u00E9r\u00E9", "gfPlaBMP_EssenceCat", 
  "gfForetRege_Essence", 
  "gfForetDen_EssenceReg1", "gfForetDen_EssenceCatReg1", 
  "gfForetFpied_EssenceCatRep\u00E9r\u00E9PerchoirAbri", 
  "gfForetFpied_EssenceCatPerchoirAbri", 
  "gfForetFpied_EssenceClassePerchoirAbri", 
  "gfForetFpied_EssenceClasseReg1", "gfForetFpied_Reg1", 
  "gfForetFpied_EssRegReg1", "gfForetFpied_EssenceReg1"
)

tBaseCarnet <- data.frame(
    var = tBase, 
    stringsAsFactors = F
  ) %>% 
  mutate(
    Var1 = ifelse(str_detect(var, "Essence"), "Essence", NA), 
    Var1 = ifelse(str_detect(var, "EssReg"), "EssReg", Var1), 
    Var1 = ifelse(str_detect(var, "EssRegInd"), "EssRegPar", Var1), 
    Var2 = ifelse(str_detect(var, "Classe"), "Classe", NA), 
    Var2 = ifelse(str_detect(var, "Cat"), "Cat", Var2), 
    Var3 = ifelse(str_detect(var, "StadeD"), "StadeD", NA), 
    Var4 = ifelse(str_detect(var, "StadeE"), "StadeE", NA), 
    Var5 = ifelse(str_detect(var, "Qual"), "Qual", NA), 
    Var5 = ifelse(str_detect(var, "Reg1"), "Reg1", Var5), 
    Var5 = ifelse(str_detect(var, "Reg2"), "Reg2", Var5), 
    Var6 = ifelse(str_detect(var, "Martel\u00E9"), "Martel\u00E9", NA), 
    Var7 = ifelse(str_detect(var, "Rep\u00E9r\u00E9"), "Rep\u00E9r\u00E9", NA), 
    Var8 = ifelse(str_detect(var, "Perchoir"), "Perchoir", NA), 
    Var9 = ifelse(str_detect(var, "Abri"), "Abri", NA), 
    Var10 = ifelse(str_detect(var, "Type"), "Type", NA), 
    Var10 = ifelse(str_detect(var, "CodeEcolo"), "CodeEcolo", Var10), 
    Var11 = ifelse(str_detect(var, "Coupe"), "Coupe", NA), 
    Data = ifelse(str_detect(var, "BM"), "BM", NA), 
    Data = ifelse(str_detect(var, "BMP"), "BMP", Data), 
    Data = ifelse(str_detect(var, "BMS"), "BMS", Data), 
    Data = ifelse(str_detect(var, "Tot"), "Tot", Data), 
    Data = ifelse(str_detect(var, "Fpied"), "Fpied", Data), 
    Data = ifelse(
      is.na(Data), 
      ifelse(str_detect(var, "Per"), "Per", Data), 
      Data
    ), 
    Data = ifelse(str_detect(var, "Taillis"), "Taillis", Data), 
    Data = ifelse(str_detect(var, "Den"), "Den", Data), 
    Data = ifelse(str_detect(var, "Codes"), "Codes", Data), 
    Data = ifelse(str_detect(var, "Rege"), "Rege", Data), 
    var = NULL
  )
# TabCombi <- rbind(TabCombi, tBaseCarnet)
TabCombi_GF <- unique(tBaseCarnet)

gf_AgregArbres(rep_Ilots, TabCombi_GF)
load("Tables/gfDonneesBrutes.RData")
load("Tables/gfTablesBrutes.RData")
load("Tables/gfTablesElaboreesPlacTest.RData")
# for(i in 1:length(TabPla)) {assign(names(TabPla)[i], TabPla[[i]])}
# -- Table résultats par placettes - Fpied :
assign(names(TabPla)[35], TabPla[[35]])

# -- Agrégations par ensemble (échelle forêt)

gf_AgregPlacettes(
  rep_Ilots, 
  data.frame(
    V1 = "Foret", V2 = NA, V3 = NA, 
    stringsAsFactors = F
  )
)
load("Tables/gfTablesElaborees.RData")
assign(names(Tableaux)[1], Tableaux[[1]])
assign(names(Tableaux)[35], Tableaux[[35]])

# --- Ecriture des résultats sous la forme de shape :
gf_ShapesPlac(rep_Ilots)


# ----- Sélection des placettes inventoriées il y a moins de 3 ans :
load("Tables/psdrfDonneesBrutes.RData")
CyclesPSDRF_DF <- 
  Cycles %>% 
  filter(Annee >= as.numeric(format(Sys.Date(), "%Y")) - 5) %>%
  select(NumDisp, NumPlac, Cycle) # dplyr::
load("Tables/gfDonneesBrutes.RData")

CyclesGF_DF <- 
  Cycles %>% 
  filter(Annee >= as.numeric(format(Sys.Date(), "%Y")) - 5) %>%
  select(NumForet, Cycle) %>% # dplyr::
  left_join(Placettes[, c("NumForet", "NumPlac", "Cycle")])


















##### / #####



# ----- 10. Construction des données placettes pour le krigeage Vha du HET : -----
  # --- Récupération des résultats sur le volume de GB et TGB
  # Résultats d'analyse (data.frame) :
  load("Tables/psdrfTablesElaboreesPlacTest.RData")
TabPla_PSDRF <- TabPla
load("Tables/gfTablesElaboreesPlacTest.RData")

TabPla_GF <- TabPla
pos1 <- which(names(TabPla_PSDRF) == "psdrfPlaFpied_EssRegCat")
assign(names(TabPla_PSDRF)[pos1], TabPla_PSDRF[[pos1]])

psdrfPlaFpied_EssRegCat <- CyclesPSDRF_DF %>% left_join(psdrfPlaFpied_EssRegCat)


pos2 <- which(names(TabPla_GF) == "gfPlaFpied_EssRegCat")
assign(names(TabPla_GF)[pos2], TabPla_GF[[pos2]])

gfPlaFpied_EssRegCat <- left_join(CyclesGF_DF, gfPlaFpied_EssRegCat)

# Attention : si on prend que les placettes avec du GB/TGB de HET, krigeage ne saura pas
# comment modéliser les zones sans GB/TGB de HET ? cf essai2
# PlacGF_DF <- left_join(CyclesGF_DF, gfPlaFpied_EssRegCat) %>%
#   filter(is.element(EssReg, c("Hêtre")) &
#            # is.element(EssReg, c("Hêtre", "Pin S", "Chêne", "Epicéa", "Sapin P", "Douglas")) &
#
#            is.element(Cat, c("GB", "TGB"))) %>%
#   group_by(NumForet) %>% # pas vraiment nécessaire
#   filter(Cycle == max(Cycle)) %>% # pas vraiment nécessaire
#   group_by(NumForet, NumPlac, Cycle, EssReg) %>%
#   summarise(Vha = sum(Vha)) %>%
#   ungroup() %>%
#   mutate(Num = paste0("GF-", NumForet, "-", NumPlac), 
#          Cat = "GBTGB") %>%
#   select(Num, NumForet, NumPlac, Cycle, EssReg, Cat, Vha)
# essai2
PlacGF_DF <- 
  gfPlaFpied_EssRegCat %>% 
  filter(
    is.element(EssReg, c("Hêtre")) &
      # is.element(EssReg, c("Hêtre", "Pin S", "Chêne", "Epicéa", "Sapin P", "Douglas")) &
      
      is.element(Cat, c("GB", "TGB"))
  ) %>%
  # group_by(NumForet) %>% # pas vraiment nécessaire
  # filter(Cycle == max(Cycle)) %>% # pas vraiment nécessaire
  group_by(NumForet, NumPlac, Cycle, EssReg) %>%
  summarise(Vha = sum(Vha)) %>%
  ungroup() %>%
  # right_join(CyclesGF_DF) %>%
  mutate(
    EssReg = ifelse(is.na(EssReg), "Hêtre", EssReg), 
    Vha = ifelse(is.na(Vha), 0, Vha)
  ) %>%
  mutate(
    Num = paste0("GF-", NumForet, "-", NumPlac), 
    Cat = "GBTGB"
  ) %>%
  select(Num, NumForet, NumPlac, Cycle, EssReg, Cat, Vha) # dplyr::

# a <- sort(PlacGF_DF$Num[which(PlacGF_DF$Vha > 0)])
# Autre méthode : ne pas faire la distinction entre les placettes ("Hêtre + GB/TGB")

# -> %%%%%%% Marche PAS !! %%%%%%%%

# PlacGF_DF <- group_by(gfPlaFpied_EssRegCat, 
#                       NumForet) %>%
#   filter(Cycle == max(Cycle)) %>% # pas nécessaire
#   ungroup() %>%
# PlacGF_DF <- mutate(gfPlaFpied_EssRegCat, 
#          GBTGB = ifelse(Cat == "GB" | Cat == "TGB", 
#                       "GBTGB", NA), 
#          Essence2Get = ifelse(EssReg == "Hêtre", 
#                         EssReg, NA)) %>%
#   group_by(NumForet, NumPlac, Cycle, GBTGB, Essence2Get) %>%
#   summarise(Vha = sum(Vha)) %>%
#   ungroup() %>%
#   mutate(Vha = ifelse(is.na(GBTGB) | is.na(Essence2Get), 
#                     0, Vha), 
#          GBTGB = "GBTGB", 
#          Essence2Get = "Hêtre") %>%
#   group_by(NumForet, NumPlac, Cycle, GBTGB, Essence2Get) %>%
#   summarise(Vha = sum(Vha)) %>%
#   ungroup() %>%
#   # right_join(CyclesGF_DF) %>% # pas nécessaire
#   mutate(Num = paste0("GF-", NumForet, "-", NumPlac), 
#          Cat = "GBTGB") %>%
#   rename(EssReg = Essence2Get) %>%
#   dplyr::select(Num, NumForet, NumPlac, Cycle, EssReg, Cat, Vha)
# b <- sort(PlacGF_DF$Num[which(PlacGF_DF$Vha > 0)])
# setdiff(a, b)

# pos_GB <- which(PlacGF_DF$Vha > 0)
# pos_GB2 <- c(1:dim(PlacGF_DF)[1])[-pos_GB][1:length(pos_GB)] # limite le nombre de placettes = 0
# PlacGF_DF <- PlacGF_DF[c(pos_GB, 
#                          pos_GB2), ]


# %>%
#   dcast(Num + NumForet + NumPlac + Cycle + Cat ~ EssReg, value.var = "Vha") %>%
#   melt(id = c("Num", "NumForet", "NumPlac", "Cycle", "Cat"), 
#        variable.name = "EssReg", value.name = "Vha") %>%
#   mutate(Vha = ifelse(is.na(Vha), 0, Vha)) %>%
#   dcast(Num + NumForet + NumPlac + Cycle + Cat ~ EssReg, value.var = "Vha")

Ess_DF <- data.frame(
  Ess_PSDRF = c("HET", "PIN", "CHE", "EPC", "SAP", "DOU"), 
  Ess_GF = c("Hêtre", "Pin S", "Chêne", "Epicéa", "Sapin P", "Douglas"), 
  stringsAsFactors = F
)

# PlacPSDRF_DF <- filter(psdrfPlaFpied_EssRegCat, 
#                        is.element(EssReg, c("HET", "PIN", "CHE", "EPC", "SAP", "DOU")) &
#                          is.element(Cat, c("GB", "TGB"))) %>%
#   group_by(NumDisp) %>%
#   filter(Cycle == max(Cycle)) %>%
#   group_by(NumDisp, NumPlac, Cycle, EssReg) %>%
#   summarise(Vha = sum(Vha)) %>%
#   ungroup() %>%
#   mutate(Num = paste0("PSDRF-", NumDisp, "-", NumPlac), 
#          Cat = "GBTGB") %>%
#   left_join(Ess_DF, by = c("EssReg" = "Ess_PSDRF")) %>%
#   mutate(EssReg = Ess_GF, 
#          Ess_GF = NULL) %>%
#   rename(NumForet = NumDisp) %>%
#   select(Num, NumForet, NumPlac, Cycle, EssReg, Cat, Vha)




# PlacPSDRF_DF <- filter(psdrfPlaFpied_EssRegCat, 
#                        is.element(EssReg, c("HET")) &
#                          is.element(Cat, c("GB", "TGB"))) %>%
#   group_by(NumDisp) %>%
#   filter(Cycle == max(Cycle)) %>%
#   group_by(NumDisp, NumPlac, Cycle, EssReg) %>%
#   summarise(Vha = sum(Vha)) %>%
#   ungroup() %>%
#   mutate(Num = paste0("PSDRF-", NumDisp, "-", NumPlac), 
#          Cat = "GBTGB") %>%
#   # left_join(Ess_DF, by = c("EssReg" = "Ess_PSDRF")) %>%
#   # mutate(EssReg = Ess_GF, 
#   #        Ess_GF = NULL) %>%
#   rename(NumForet = NumDisp) %>%
#   select(Num, NumForet, NumPlac, Cycle, EssReg, Cat, Vha)

PlacPSDRF_DF <- 
  psdrfPlaFpied_EssRegCat %>% 
  filter(
    is.element(EssReg, c("HET")) & 
      is.element(Cat, c("GB", "TGB"))
  ) %>%
  # group_by(NumDisp) %>% # pas nécessaire
  # filter(Cycle == max(Cycle)) %>% # pas nécessaire
  group_by(NumDisp, NumPlac, Cycle, EssReg) %>%
  summarise(Vha = sum(Vha)) %>%
  ungroup() %>%
  left_join(Ess_DF, by = c("EssReg" = "Ess_PSDRF")) %>%
  mutate(
    EssReg = Ess_GF, 
    Ess_GF = NULL,
    # right_join(CyclesPSDRF_DF) %>% # pas nécessaire
    
    EssReg = ifelse(is.na(EssReg), "Hêtre", EssReg), 
    Vha = ifelse(is.na(Vha), 0, Vha),
    
    Num = paste0("PSDRF-", NumDisp, "-", NumPlac), 
    Cat = "GBTGB"
  ) %>%
  # left_join(Ess_DF, by = c("EssReg" = "Ess_PSDRF")) %>%
  # mutate(EssReg = Ess_GF, 
  #        Ess_GF = NULL) %>%
  rename(NumForet = NumDisp) %>%
  select(Num, NumForet, NumPlac, Cycle, EssReg, Cat, Vha) # dplyr::


# Autre méthode : ne pas faire la distinction entre les placettes ("Hêtre + GB/TGB")
# PlacPSDRF_DF <- group_by(psdrfPlaFpied_EssRegCat, 
#                          NumDisp) %>%
#   filter(Cycle == max(Cycle)) %>% # pas nécessaire
#   ungroup() %>%
# PlacPSDRF_DF <- mutate(psdrfPlaFpied_EssRegCat, 
#                     GBTGB = ifelse(Cat == "GB" | Cat == "TGB", 
#                       "GBTGB", NA), 
#          Essence2Get = ifelse(EssReg == "HET", 
#                             EssReg, NA)) %>%
#   group_by(NumDisp, NumPlac, Cycle, GBTGB, Essence2Get) %>%
#   summarise(Vha = sum(Vha)) %>%
#   ungroup() %>%
#   mutate(Vha = ifelse(is.na(GBTGB) | is.na(Essence2Get), 
#                     0, Vha), 
#          GBTGB = "GBTGB", 
#          Essence2Get = "Hêtre") %>%
#   group_by(NumDisp, NumPlac, Cycle, GBTGB, Essence2Get) %>%
#   summarise(Vha = sum(Vha)) %>%
#   ungroup() %>%
#   # right_join(CyclesPSDRF_DF) %>% # pas nécessaire
#   mutate(Num = paste0("PSDRF-", NumDisp, "-", NumPlac), 
#          Cat = "GBTGB") %>%
#   rename(EssReg = Essence2Get, 
#          NumForet = NumDisp) %>%
#   dplyr::select(Num, NumForet, NumPlac, Cycle, EssReg, Cat, Vha)
# pos_GB <- which(PlacPSDRF_DF$Vha > 0)
# pos_GB2 <- c(1:dim(PlacPSDRF_DF)[1])[-pos_GB][1:length(pos_GB)] # limite le nombre de placettes = 0
# PlacPSDRF_DF <- PlacPSDRF_DF[c(pos_GB, 
#                                pos_GB2), ]

# length(grep("GF-12-", PlacGF_DF$Num))
Plac_DF <- rbind(PlacGF_DF, PlacPSDRF_DF)
# a <- Plac_DF[which(is.na(Plac_DF$EssReg)), ]

# Autre méthode nécessite de limiter le nombre de valeurs nulles :
# pos_GB <- which(Plac_DF$Vha > 0)
# pos_nonGB <- which(Plac_DF$Vha = = 0)
# pos_nonGB <- pos_nonGB[1:round(length(pos_GB)/3, 0)]
# Plac_DF <- Plac_DF[c(pos_GB, pos_nonGB), ]
# a <- Plac_DF[which(duplicated(Plac_DF[, c("Num", "NumForet", "NumPlac")]) |
#                      duplicated(Plac_DF[, c("Num", "NumForet", "NumPlac")], fromLast = T)), ]

ListEss <- unique(Plac_DF$EssReg)
# Plac_DF <- mutate(Plac_DF, 
#                   EssReg = paste0("Vha_", EssReg)) %>%
#   dcast(Num + NumForet + NumPlac + Cycle + Cat ~ EssReg, value.var = "Vha") %>%
#   melt(id = c("Num", "NumForet", "NumPlac", "Cycle", "Cat"), 
#        variable.name = "EssReg", value.name = "Vha") %>%
#   mutate(Vha = ifelse(is.na(Vha), 0, Vha)) %>%
#   dcast(Num + NumForet + NumPlac + Cycle + Cat ~ EssReg, value.var = "Vha")


# Localisation des placettes d'inventaire :
repDataBrutes <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes"
# Fusion des shapes de placettes :
repGF <- paste0(dirname(repDataBrutes), "/GF/")
ListGF_SHP4 <- c(
  "Plac_Grossenwald_L93.shp", 
  "Plac_Hardt_L93.shp", 
  "Plac_Hohenfels_L93.shp", 
  "Plac_MILITAIB_L93.shp", 
  "Plac_PNRVN_L93.shp", 
  "Plac_SchoeneckI_L93.shp", 
  "Plac_SchoeneckII_GFVN_L93.shp", 
  "Plac_SchoeneckIII_L93.shp", 
  "Plac_SturzelbronnI_L93.shp", 
  "Plac_SturzelbronnII_L93.shp", 
  "Plac_Windstein_L93.shp", 
  "Plac_Zittersheim_L93.shp"
)

repPSDRF <- paste0(dirname(repDataBrutes), "/PSDRF/")
ListPSDRF_SHP4 <- c(
  "Plac_ForetduLangenberg_L93.shp", 
  "Plac_ForetIrregulieredelaPetitePierreSud_L93.shp", 
  "Plac_Hengstberg_L93.shp", 
  "Plac_Lutzelhardt_L93.shp", 
  "Plac_Nonnenthal_L93.shp", 
  "Plac_RNBitche_L93.shp"
)

List_SHP4 <- c(
  paste0(repGF, ListGF_SHP4), 
  paste0(repPSDRF, ListPSDRF_SHP4)
)



# --- Changement : on pioche 85% des placette de chaque réseau pour calibration
# Choix du pourcentage de calibration
p <- 0.85

Plac_SHP <- c()
# ListEchant <- list()
for (file in List_SHP4) {
  print(file)
  # file <- List_SHP4[1]
  shp_temp <- readOGR(
    dsn = dirname(file), 
    layer = file_path_sans_ext(basename(file)), 
    verbose = F, 
    stringsAsFactors = F
  )
  # # shp_temp <- shp_temp[, c(match("NumFort", names(shp_temp)), 
  # #                         match("NumPlac", names(shp_temp)), 
  # #                         which(names(shp_temp) %in% c("Vha", "Hêtr_Vh")))]
  # # names(shp_temp) <- c("NumForet", "NumPlac", "Vha")
  # shp_temp <- shp_temp[, c("NumForet", "NumPlac", "Vha")]
  if (is.element("NumDisp", names(shp_temp))) {
    shp_temp@data <- 
      shp_temp@data %>% 
      mutate(
        Num = paste0("PSDRF-", NumDisp, "-", NumPlac), 
        NumDisp = as.numeric(NumDisp)
      ) %>%
      rename(NumForet = NumDisp) %>%
      select(Num, NumForet, NumPlac) # dplyr::

  } else {
    shp_temp@data <- 
      shp_temp@data %>% 
      mutate( 
        Num = paste0("GF-", NumForet, "-", NumPlac), 
        NumForet = as.numeric(NumForet)
      ) %>%
      select(Num, NumForet, NumPlac) # dplyr::
  }
  shp_temp <- shp_temp[which(shp_temp$Num %in% Plac_DF$Num), ]

  # echant_temp <- sample(dim(shp_temp@data)[1], 
  #                       floor(p*dim(shp_temp@data)[1]), 
  #                       replace = FALSE)
  # echant_temp <- list(echant_temp)
  # names(echant_temp) <- paste0("PSDRF-", unique(shp_temp@data$NumForet))
  # ListEchant <- c(ListEchant, echant_temp)


  echant_temp <- sample(
    dim(shp_temp@data)[1], 
    floor(p*dim(shp_temp@data)[1]), 
    replace = FALSE
  )
  PlacCalib_temp_SHP <- shp_temp[echant_temp, ]
  PlacValid_temp_SHP <- shp_temp[-echant_temp, ]


  if (match(file, List_SHP4) == 1) {
    Plac_SHP <- shp_temp
    PlacCalib_SHP <- PlacCalib_temp_SHP
    PlacValid_SHP <- PlacValid_temp_SHP
  } else {
    Plac_SHP <- rbind(Plac_SHP, shp_temp)
    PlacCalib_SHP <- rbind(PlacCalib_SHP, PlacCalib_temp_SHP)
    PlacValid_SHP <- rbind(PlacValid_SHP, PlacValid_temp_SHP)
  }
}

Plac_SHP <- 
  spTransform(Plac_SHP, CRS("+init = epsg:2154"))

PlacCalib_SHP <- 
  spTransform(PlacCalib_SHP, CRS("+init = epsg:2154"))

# PlacCalib_SHPsav <- PlacCalib_SHP
PlacValid_SHP <- 
  spTransform(PlacValid_SHP, CRS("+init = epsg:2154"))
# PlacValid_SHPsav <- PlacValid_SHP

# a <- Plac_DF[which(!Plac_DF$Num %in% Plac_SHP$Num), ] Placettes manquantes dans le shape
# PlacSave_SHP <- Plac_SHP
# Plac_SHP <- PlacSave_SHP
# Plac_SHP <- Plac_SHP[which(Plac_SHP$Num %in% Plac_DF$Num), ]
Plac_SHP@data <- left_join(Plac_SHP@data, Plac_DF)

PlacCalib_SHP@data <- left_join(PlacCalib_SHP@data, Plac_DF)

PlacValid_SHP@data <- left_join(PlacValid_SHP@data, Plac_DF)

# df <- Plac_SHP@data
# a <- PlacCalib_SHP@data[which(duplicated(PlacCalib_SHP@data$Num) |
#                                 duplicated(PlacCalib_SHP@data$Num, fromLast = T)), ]














# ----- 13. Krigeage Vha HET (construction du raster) -----
# ----- Calibrage
# Choix du nombre de répétitions de la validation
# n <- 10

# Choix du pourcentage de calibration
p <- 0.85

# Choix de la variable objet du krigeage
# Objet <- "Vha"

# Initialisation des tableaux de stockage des p_values et des erreurs relatives
p_value <- data.frame()
err <- data.frame()


# --- Sélection des placettes de validation et de calibration
# Vecteur contenant les numéros de placettes à mettre dans le jeu de calibration
# echant <- sample(dim(Plac_SHP)[1], floor(p*dim(Plac_SHP)[1]), replace = FALSE)
# Sélection des placettes de calibration
# PlacCalib_SHP <- Plac_SHP[echant, ]



# Sélection des placettes de validation
# PlacPNRVN_SHP <- Plac_SHP[grep("GF-12-", Plac_SHP@data$Num), ] # sélection des placettes du
# PNRVN pour validation (assure représentativité à l'échelle du territoire)
# dim(PlacPNRVN_SHP)
# echant <- which(!is.element(PlacPNRVN_SHP@data$Num, PlacCalib_SHP@data$Num))
# if (length(echant) < 30) {
if (dim(PlacValid_SHP)[1] < 30) {
  tk_messageBox(
    type = "ok", 
    message = "Attention : nombre de placettes de validation insuffisant. Relancer le tirage", 
    icon = "warning"
  )
}
# PlacValid_SHP <- PlacPNRVN_SHP[echant, ]

# ----- Modification du shape délimitant le domaine forestier ---
# Raccourcir -> supposer que le shape a déjà été lu auparavant
# shp_NAME <- file.choose()
shpForet_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataProjet_Garde/Autres/PeupltIFN.shp"
shpForet_SHP <- readOGR(
  dsn = dirname(shpForet_FILE), 
  layer = file_path_sans_ext(basename(shpForet_FILE)), 
  verbose = F, 
  stringsAsFactors = F
) %>%
  spTransform(CRS("+init = epsg:2154"))
# shpForest_SHP <- gUnaryUnion(shpForet_SHP)
# save(shpForest_SHP, 
#      file = "Tables/Perim_Foret.RData")
load("Tables/Perim_Foret.RData")


# Récupération des données du raster (clip) contenant les infos de la feuille Krigeage :
# load("Tables/Raster_FIN.RData")
# raster3 <- mask(raster2, shpForest_SHP)
# # plot(raster3)
# save(raster3, 
#      file = "Tables/Raster3_FIN.RData")
load("Tables/Raster3_FIN.RData")
# load("Tables/Raster_FIN.RData")

# Clip sur le shape des placettes
Plac_SHP <- Plac_SHP[shpForest_SHP, ]

# Extraction des données du mnh au niveau des placettes (raster)
extract_raster3 <- raster::extract(raster3, PlacCalib_SHP@coords, df = TRUE)
# Jonction des données extraites avec la couche spatialisée placettes
# a <-
PlacCalib_SHP@data <- PlacCalib_SHP@data %>% cbind(extract_raster3)
PlacCalib_SHP <- spTransform(PlacCalib_SHP, CRS("+init = epsg:2154"))

# Sécurité si valeurs vides :
pos_NA <- c()
for (col in names(PlacCalib_SHP@data)) {
  pos_NA <- c(pos_NA, which(is.na(PlacCalib_SHP@data[, col])))
}
if (length(pos_NA) > 0) {
  PlacCalib_SHP <- PlacCalib_SHP[-pos_NA, ]
}

# colSums(is.na(PlacCalib_SHP@data))
# a <- PlacCalib_SHP@data

# pos_NA <- c()
# for (col in names(PlacCalib_SHP@data)) {
#   pos_NA <- c(pos_NA, which(is.na(PlacCalib_SHP@data[, col]))) # on exclut les placettes où il y a des valeurs
#   # vides. A ce stade, vu les corrections opérées, cela ne devrait pas être
# }
# pos_NA <- unique(pos_NA)
# PlacCalib_SHP <- PlacCalib_SHP[-pos_NA, ]
# dim(PlacCalib_SHP)
# # Il semble qu'il y ait encore des NA dans le shapes des placettes de calibrages. Pourquoi ? Loups éliminés avec corrections ci-dessus

Objet <- "Vha"

# Version sans boucle (uniquement 'Vha' considéré)
PlacCalib_SHP <- 
  PlacCalib_SHP %>% 
  select(Vha, mnh, hdr, Peuplt_IFN, SER, Geologie) # , Peuplt_IFN, Station_Forestiere #, Station_Forestiere, SER, Geologie
# PlacCalib_SHP <- PlacCalib_SHP[!is.na(PlacCalib_SHP$Vha), c("Vha", "mnh", "hdr", "Peuplt_IFN", "Station_Forestiere", "SER", "Geologie")]

PlacCalib_SHP <- spTransform(PlacCalib_SHP, CRS("+init = epsg:2154"))

# Sécurité si valeurs vides :
pos_NA <- c()
for (col in names(PlacCalib_SHP@data)) {
  pos_NA <- c(pos_NA, which(is.na(PlacCalib_SHP@data[, col])))
}
if (length(pos_NA) > 0) {
  PlacCalib_SHP <- PlacCalib_SHP[-pos_NA, ]
}

# PlacCalib_SHP@data <- mutate(PlacCalib_SHP@data, 
#                              Vha = ifelse(is.na(Vha), 
#                                         0, Vha), 
#                              mnh = ifelse(is.na(mnh), 
#                                         0, mnh), 
#                              hdr = ifelse(is.na(hdr), 
#                                         0, hdr), 
#                              Peuplt_IFN = ifelse(is.na(Peuplt_IFN), 
#                                                0, Peuplt_IFN), 
#                              Station_Forestiere = ifelse(is.na(Station_Forestiere), 
#                                                0, Station_Forestiere), 
#                              SER = ifelse(is.na(SER), 
#                                                0, SER), 
#                              Geologie = ifelse(is.na(Geologie), 
#                                                0, Geologie))
# Aires_Protection = ifelse(is.na(Aires_Protection), 
#                   0, Aires_Protection))

formula <- paste(
  Objet, 
  " ~  Peuplt_IFN + mnh + hdr + SER + Geologie", # + Aires_Protection   #+ SER + Geologie
  sep = ""
) #  + Peuplt_IFN + Station_Forestiere
# PlacCalib_SHP2 <- PlacCalib_SHP[, c("Vha", "mnh", "hdr", "Peuplt_IFN", "SER", "Geologie")] # , "Peuplt_IFN", "Station_Forestiere", "Aires_Protection"



# ----- Préparation du grid
res <- 20

ext <- extent(Bbox_SHP)

x_min <- floor(ext[1])
x_max <- x_min + floor((ext[2] - ext[1]) / res) * res
y_min <- floor(ext[3])
y_max <- y_min + floor((ext[4] - ext[3]) / res) * res
#Spatialisation du grid
grd <- expand.grid(x = seq(x_min, x_max, by = res), y = seq(y_min, y_max, by = res))
coordinates(grd) <- ~ x + y
grd@proj4string <- CRS("+init = epsg:2154")

extract_grd <- raster::extract(raster3, grd@coords, df = TRUE)
# %>%
#   mutate(mnh = ifelse(is.na(mnh), # Sécurité pour remplacer les NA
#                     0, mnh), 
#          hdr = ifelse(is.na(hdr), # Sécurité pour remplacer les NA
#                     0, hdr), 
#          Peuplt_IFN = ifelse(is.na(Peuplt_IFN), # Sécurité pour remplacer les NA
#                            0, Peuplt_IFN))
grd <- cbind(coordinates(grd), extract_grd)
coordinates(grd) <- ~ x + y
gridded(grd) <- TRUE
grd@proj4string <- CRS("+init = epsg:2154")

# Paramétrage du krigeage (variogramme)
# vr <- variogram(as.formula(formula), data = PlacCalib_SHP2, cutoff = 2000, width = 100)
# PlacCalib_SHP2 <- PlacCalib_SHP[, c("Vha", "mnh", "hdr", "Peuplt_IFN", "SER", "Geologie")]
vr <- variogram(as.formula(formula), data = PlacCalib_SHP, cutoff = 800, width = 70)
plot(vr)
vrmf <- fit.variogram(
  vr, 
  vgm(
    psill = 300, model = "Sph", 
    # range = 6000, 
    nugget = 0
  )
)
plot(vr, vrmf)
# ----- Interpolation
kr <- krige(as.formula(formula), locations = PlacCalib_SHP, newdata = grd, model = vrmf)
kr@data$var1.pred[which(kr@data$var1.pred < 0)] <- 0
#Transformation d'un SpatialPointDataFrame en SpatialGridDataFrame
gridded(kr) <- TRUE
#Rasterisation
kr_rast <- SpatialPixelsDataFrame(
  points = kr@coords, data = kr@data, 
  proj4string = CRS("+init = epsg:2154")
)
kr_rast <- raster(kr_rast)
krVha_rast <- kr_rast
writeRaster(
  krVha_rast, 
  file = "Out/SIG/Raster/Krigeage/Vha_HET", 
  format = "GTiff", 
  overwrite = T
)

# ----- 15. Test vérification krigeage Vha de HET : -----
#Extraction des données du krigeage au niveau des placettes (raster)
extract_kr <- raster::extract(
  krVha_rast, PlacValid_SHP@coords, df = TRUE, buffer = 30, fun = mean
)
extract_kr <- extract_kr[, 2]
# Simplification des donn?es des placettes de validation
# PlacValid_SHP <- Plac_SHP[echant, ]
PlacValid_DF <- subset(PlacValid_SHP@data, select = Objet)
# Fusion des deux ?chantillons
student <- cbind(extract_kr, PlacValid_DF)
# Calcul des erreurs relatives
temp <- student
temp$err <- abs(student[, 1]-student[, 2])/student[, 2]
# err <- rbind(err, temp)

# test de Student
test <- t.test(student[, 1], student[, 2], paired = TRUE)
test_VhaHET <- test
test
save(test_VhaHET, file = "Tables/TestStudent_VhaHET.RData")



































































##### / #####
# ----- 17. Construction des données placettes pour le krigeage Vha des GB/TGB toutes essences confondues : -----
# --- Récupération des résultats sur le volume de GB et TGB
# Résultats d'analyse (data.frame) :
load("Tables/psdrfTablesElaboreesPlacTest.RData")
TabPla_PSDRF <- TabPla
load("Tables/gfTablesElaboreesPlacTest.RData")
TabPla_GF <- TabPla

# pos1 <- which(names(TabPla_PSDRF) == "psdrfPlaFpied_Cat")
# assign(names(TabPla_PSDRF)[pos1], TabPla_PSDRF[[pos1]])
# pos2 <- which(names(TabPla_GF) == "gfPlaFpied_Cat")
# assign(names(TabPla_GF)[pos2], TabPla_GF[[pos2]])

pos1 <- which(names(TabPla_PSDRF) == "psdrfPlaFpied_Cat")
assign(names(TabPla_PSDRF)[pos1], TabPla_PSDRF[[pos1]])
psdrfPlaFpied_Cat <- left_join(CyclesPSDRF_DF, psdrfPlaFpied_Cat)
pos2 <- which(names(TabPla_GF) == "gfPlaFpied_Cat")
assign(names(TabPla_GF)[pos2], TabPla_GF[[pos2]])
gfPlaFpied_Cat <- left_join(CyclesGF_DF, gfPlaFpied_Cat)

# Attention : si on prend que les placettes avec du GB/TGB de HET, krigeage ne saura pas
# comment modéliser les zones sans GB/TGB de HET ? cf essai2
# PlacGF_DF <- left_join(CyclesGF_DF, gfPlaFpied_EssRegCat) %>%
#   filter(is.element(EssReg, c("Hêtre")) &
#            # is.element(EssReg, c("Hêtre", "Pin S", "Chêne", "Epicéa", "Sapin P", "Douglas")) &
#
#            is.element(Cat, c("GB", "TGB"))) %>%
#   group_by(NumForet) %>% # pas vraiment nécessaire
#   filter(Cycle == max(Cycle)) %>% # pas vraiment nécessaire
#   group_by(NumForet, NumPlac, Cycle, EssReg) %>%
#   summarise(Vha = sum(Vha)) %>%
#   ungroup() %>%
#   mutate(Num = paste0("GF-", NumForet, "-", NumPlac), 
#          Cat = "GBTGB") %>%
#   select(Num, NumForet, NumPlac, Cycle, EssReg, Cat, Vha)
# essai2
  PlacGF_DF <- filter(gfPlaFpied_Cat, 
                      is.element(Cat, c("GB", "TGB"))) %>%
  # group_by(NumForet) %>% # pas vraiment nécessaire
  # filter(Cycle == max(Cycle)) %>% # pas vraiment nécessaire
  group_by(NumForet, NumPlac, Cycle) %>%
  summarise(Vha = sum(Vha)) %>%
  ungroup() %>%
  # right_join(CyclesGF_DF) %>%
  mutate(Vha = ifelse(is.na(Vha), 0, Vha), 
         Num = paste0("GF-", NumForet, "-", NumPlac), 
         Cat = "GBTGB") %>%
  dplyr::select(Num, NumForet, NumPlac, Cycle, Cat, Vha)
# pos_GB <- which(PlacGF_DF$Vha > 0)
# pos_GB2 <- c(1:dim(PlacGF_DF)[1])[-pos_GB][1:length(pos_GB)] # limite le nombre de placettes = 0
# PlacGF_DF <- PlacGF_DF[c(pos_GB, 
#                          pos_GB2), ]


# %>%
#   dcast(Num + NumForet + NumPlac + Cycle + Cat ~ EssReg, value.var = "Vha") %>%
#   melt(id = c("Num", "NumForet", "NumPlac", "Cycle", "Cat"), 
#        variable.name = "EssReg", value.name = "Vha") %>%
#   mutate(Vha = ifelse(is.na(Vha), 0, Vha)) %>%
#   dcast(Num + NumForet + NumPlac + Cycle + Cat ~ EssReg, value.var = "Vha")

# Ess_DF <- data.frame(Ess_PSDRF = c("HET", "PIN", "CHE", "EPC", "SAP", "DOU"), 
#                      Ess_GF = c("Hêtre", "Pin S", "Chêne", "Epicéa", "Sapin P", "Douglas"), 
#                      stringsAsFactors = F)

# PlacPSDRF_DF <- filter(psdrfPlaFpied_EssRegCat, 
#                        is.element(EssReg, c("HET", "PIN", "CHE", "EPC", "SAP", "DOU")) &
#                          is.element(Cat, c("GB", "TGB"))) %>%
#   group_by(NumDisp) %>%
#   filter(Cycle == max(Cycle)) %>%
#   group_by(NumDisp, NumPlac, Cycle, EssReg) %>%
#   summarise(Vha = sum(Vha)) %>%
#   ungroup() %>%
#   mutate(Num = paste0("PSDRF-", NumDisp, "-", NumPlac), 
#          Cat = "GBTGB") %>%
#   left_join(Ess_DF, by = c("EssReg" = "Ess_PSDRF")) %>%
#   mutate(EssReg = Ess_GF, 
#          Ess_GF = NULL) %>%
#   rename(NumForet = NumDisp) %>%
#   select(Num, NumForet, NumPlac, Cycle, EssReg, Cat, Vha)




# PlacPSDRF_DF <- filter(psdrfPlaFpied_EssRegCat, 
#                        is.element(EssReg, c("HET")) &
#                          is.element(Cat, c("GB", "TGB"))) %>%
#   group_by(NumDisp) %>%
#   filter(Cycle == max(Cycle)) %>%
#   group_by(NumDisp, NumPlac, Cycle, EssReg) %>%
#   summarise(Vha = sum(Vha)) %>%
#   ungroup() %>%
#   mutate(Num = paste0("PSDRF-", NumDisp, "-", NumPlac), 
#          Cat = "GBTGB") %>%
#   # left_join(Ess_DF, by = c("EssReg" = "Ess_PSDRF")) %>%
#   # mutate(EssReg = Ess_GF, 
#   #        Ess_GF = NULL) %>%
#   rename(NumForet = NumDisp) %>%
#   select(Num, NumForet, NumPlac, Cycle, EssReg, Cat, Vha)

PlacPSDRF_DF <- filter(psdrfPlaFpied_Cat, 
                         is.element(Cat, c("GB", "TGB"))) %>%
  # group_by(NumDisp) %>% # pas nécessaire
  # filter(Cycle == max(Cycle)) %>% # pas nécessaire
  group_by(NumDisp, NumPlac, Cycle) %>%
  summarise(Vha = sum(Vha)) %>%
  ungroup() %>%
  # right_join(CyclesPSDRF_DF) %>% # pas nécessaire
  mutate(Vha = ifelse(is.na(Vha), 0, Vha), 
         Num = paste0("PSDRF-", NumDisp, "-", NumPlac), 
         Cat = "GBTGB") %>%
  # left_join(Ess_DF, by = c("EssReg" = "Ess_PSDRF")) %>%
  # mutate(EssReg = Ess_GF, 
  #        Ess_GF = NULL) %>%
  rename(NumForet = NumDisp) %>%
  dplyr::select(Num, NumForet, NumPlac, Cycle, Cat, Vha)
# pos_GB <- which(PlacPSDRF_DF$Vha > 0)
# pos_GB2 <- c(1:dim(PlacPSDRF_DF)[1])[-pos_GB][1:length(pos_GB)] # limite le nombre de placettes = 0
# PlacPSDRF_DF <- PlacPSDRF_DF[c(pos_GB, 
#                                pos_GB2), ]

# length(grep("GF-12-", PlacGF_DF$Num))
Plac_DF <- rbind(PlacGF_DF, PlacPSDRF_DF)

# ListCat <- unique(Plac_DF$Cat)
# Plac_DF <- mutate(Plac_DF, 
#                   EssReg = paste0("Vha_", EssReg)) %>%
#   dcast(Num + NumForet + NumPlac + Cycle + Cat ~ EssReg, value.var = "Vha") %>%
#   melt(id = c("Num", "NumForet", "NumPlac", "Cycle", "Cat"), 
#        variable.name = "EssReg", value.name = "Vha") %>%
#   mutate(Vha = ifelse(is.na(Vha), 0, Vha)) %>%
#   dcast(Num + NumForet + NumPlac + Cycle + Cat ~ EssReg, value.var = "Vha")


# Localisation des placettes d'inventaire :
repDataBrutes <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes"
# Fusion des shapes de placettes :
repGF <- paste0(dirname(repDataBrutes), 
                "/GF/")
ListGF_SHP4 <- c("Plac_Grossenwald_L93.shp", 
                 "Plac_Hardt_L93.shp", 
                 "Plac_Hohenfels_L93.shp", 
                 "Plac_MILITAIB_L93.shp", 
                 "Plac_PNRVN_L93.shp", 
                 "Plac_SchoeneckI_L93.shp", 
                 "Plac_SchoeneckII_GFVN_L93.shp", 
                 "Plac_SchoeneckIII_L93.shp", 
                 "Plac_SturzelbronnI_L93.shp", 
                 "Plac_SturzelbronnII_L93.shp", 
                 "Plac_Windstein_L93.shp", 
                 "Plac_Zittersheim_L93.shp")

repPSDRF <- paste0(dirname(repDataBrutes), 
                   "/PSDRF/")
ListPSDRF_SHP4 <- c("Plac_ForetduLangenberg_L93.shp", 
                    "Plac_ForetIrregulieredelaPetitePierreSud_L93.shp", 
                    "Plac_Hengstberg_L93.shp", 
                    "Plac_Lutzelhardt_L93.shp", 
                    "Plac_Nonnenthal_L93.shp", 
                    "Plac_RNBitche_L93.shp")

List_SHP4 <- c(paste0(repGF, ListGF_SHP4), 
               paste0(repPSDRF, ListPSDRF_SHP4))



# --- Changement : on pioche 85% des placette de chaque réseau pour calibration
# Choix du pourcentage de calibration
p <- 0.85

Plac_SHP <- c()
# ListEchant <- list()
for (file in List_SHP4) {
  print(file)
  # file <- List_SHP4[1]
  shp_temp <- readOGR(dsn = dirname(file), 
                      layer = file_path_sans_ext(basename(file)), 
                      verbose = F, 
                      stringsAsFactors = F)
  # # shp_temp <- shp_temp[, c(match("NumFort", names(shp_temp)), 
  # #                         match("NumPlac", names(shp_temp)), 
  # #                         which(names(shp_temp) %in% c("Vha", "Hêtr_Vh")))]
  # # names(shp_temp) <- c("NumForet", "NumPlac", "Vha")
  # shp_temp <- shp_temp[, c("NumForet", "NumPlac", "Vha")]
  if (is.element("NumDisp", names(shp_temp))) {
    shp_temp@data <- mutate(shp_temp@data, 
                            Num = paste0("PSDRF-", NumDisp, "-", NumPlac), 
                            NumDisp = as.numeric(NumDisp)) %>%
      rename(NumForet = NumDisp) %>%
      dplyr::select(Num, NumForet, NumPlac)

  } else {
    shp_temp@data <- mutate(shp_temp@data, 
                            Num = paste0("GF-", NumForet, "-", NumPlac), 
                            NumForet = as.numeric(NumForet)) %>%
      dplyr::select(Num, NumForet, NumPlac)
  }
  shp_temp <- shp_temp[which(shp_temp$Num %in% Plac_DF$Num), ]

  # echant_temp <- sample(dim(shp_temp@data)[1], 
  #                       floor(p*dim(shp_temp@data)[1]), 
  #                       replace = FALSE)
  # echant_temp <- list(echant_temp)
  # names(echant_temp) <- paste0("PSDRF-", unique(shp_temp@data$NumForet))
  # ListEchant <- c(ListEchant, echant_temp)


  echant_temp <- sample(dim(shp_temp@data)[1], 
                        floor(p*dim(shp_temp@data)[1]), 
                        replace = FALSE)
  PlacCalib_temp_SHP <- shp_temp[echant_temp, ]
  PlacValid_temp_SHP <- shp_temp[-echant_temp, ]


  if (match(file, List_SHP4) == 1) {
    Plac_SHP <- shp_temp
    PlacCalib_SHP <- PlacCalib_temp_SHP
    PlacValid_SHP <- PlacValid_temp_SHP
  } else {
    Plac_SHP <- rbind(Plac_SHP, 
                      shp_temp)
    PlacCalib_SHP <- rbind(PlacCalib_SHP, PlacCalib_temp_SHP)
    PlacValid_SHP <- rbind(PlacValid_SHP, PlacValid_temp_SHP)
  }
}

Plac_SHP <- spTransform(Plac_SHP, 
                        CRS("+init = epsg:2154"))
PlacCalib_SHP <- spTransform(PlacCalib_SHP, 
                             CRS("+init = epsg:2154"))
PlacValid_SHP <- spTransform(PlacValid_SHP, 
                             CRS("+init = epsg:2154"))

# a <- Plac_DF[which(!Plac_DF$Num %in% Plac_SHP$Num), ] Placettes manquantes dans le shape
PlacSave_SHP <- Plac_SHP
# Plac_SHP <- PlacSave_SHP
# Plac_SHP <- Plac_SHP[which(Plac_SHP$Num %in% Plac_DF$Num), ]
Plac_SHP@data <- left_join(Plac_SHP@data, Plac_DF)
PlacCalib_SHP@data <- left_join(PlacCalib_SHP@data, Plac_DF)
PlacValid_SHP@data <- left_join(PlacValid_SHP@data, Plac_DF)

# df <- Plac_SHP@data














# ----- 18. Krigeage Vha HET (construction du raster) -----
# ----- Calibrage
# Choix du nombre de répétitions de la validation
# n <- 10

# Choix du pourcentage de calibration
p <- 0.85

# Choix de la variable objet du krigeage
# Objet <- "Vha"

# Initialisation des tableaux de stockage des p_values et des erreurs relatives
p_value <- data.frame()
err <- data.frame()


# --- Sélection des placettes de validation et de calibration
# Vecteur contenant les numéros de placettes à mettre dans le jeu de calibration
# echant <- sample(dim(Plac_SHP)[1], floor(p*dim(Plac_SHP)[1]), replace = FALSE)
# Sélection des placettes de calibration
# PlacCalib_SHP <- Plac_SHP[echant, ]



# Sélection des placettes de validation
# PlacPNRVN_SHP <- Plac_SHP[grep("GF-12-", Plac_SHP@data$Num), ] # sélection des placettes du
# PNRVN pour validation (assure représentativité à l'échelle du territoire)
# dim(PlacPNRVN_SHP)
# echant <- which(!is.element(PlacPNRVN_SHP@data$Num, PlacCalib_SHP@data$Num))
# if (length(echant) < 30) {
if (dim(PlacValid_SHP)[1] < 30) {
  tk_messageBox(type = "ok", 
                message = "Attention : nombre de placettes de validation insuffisant. Relancer le tirage", 
                icon = "warning")
}
# PlacValid_SHP <- PlacPNRVN_SHP[echant, ]

# ----- Modification du shape délimitant le domaine forestier ---
# Raccourcir -> supposer que le shape a déjà été lu auparavant
# shp_NAME <- file.choose()
shpForet_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataProjet_Garde/Autres/PeupltIFN.shp"
shpForet_SHP <- readOGR(dsn = dirname(shpForet_FILE), 
                        layer = file_path_sans_ext(basename(shpForet_FILE)), 
                        verbose = F, 
                        stringsAsFactors = F) %>%
  spTransform(CRS("+init = epsg:2154"))
# shpForest_SHP <- gUnaryUnion(shpForet_SHP)
# save(shpForest_SHP, 
#      file = "Tables/Perim_Foret.RData")
load("Tables/Perim_Foret.RData")


# Récupération des données du raster (clip) contenant les infos de la feuille Krigeage :
load("Tables/Raster_FIN.RData")
# raster3 <- mask(raster2, shpForest_SHP)
# # plot(raster3)
# save(raster3, 
#      file = "Tables/Raster3_FIN.RData")
load("Tables/Raster3_FIN.RData")
# load("Tables/Raster_FIN.RData")

# Clip sur le shape des placettes
Plac_SHP <- Plac_SHP[shpForest_SHP, ]

# Extraction des données du mnh au niveau des placettes (raster)
extract_raster3 <- raster::extract(raster3, PlacCalib_SHP@coords, df = TRUE)
# Jonction des données extraites avec la couche spatialisée placettes
# a <-
PlacCalib_SHP@data <- cbind(PlacCalib_SHP@data, 
                            extract_raster3)
PlacCalib_SHP <- spTransform(PlacCalib_SHP, 
                             CRS("+init = epsg:2154"))

# Sécurité si valeurs vides :
pos_NA <- c()
for (col in names(PlacCalib_SHP@data)) {
  pos_NA <- c(pos_NA, which(is.na(PlacCalib_SHP@data[, col])))
}
if (length(pos_NA) > 0) {
  PlacCalib_SHP <- PlacCalib_SHP[-pos_NA, ]
}

# colSums(is.na(PlacCalib_SHP@data))
# a <- PlacCalib_SHP@data

# pos_NA <- c()
# for (col in names(PlacCalib_SHP@data)) {
#   pos_NA <- c(pos_NA, which(is.na(PlacCalib_SHP@data[, col]))) # on exclut les placettes où il y a des valeurs
#   # vides. A ce stade, vu les corrections opérées, cela ne devrait pas être
# }
# pos_NA <- unique(pos_NA)
# PlacCalib_SHP <- PlacCalib_SHP[-pos_NA, ]
# dim(PlacCalib_SHP)
# # Il semble qu'il y ait encore des NA dans le shapes des placettes de calibrages. Pourquoi ? Loups éliminés avec corrections ci-dessus

Objet <- "Vha"

# Version sans boucle (uniquement 'Vha' considéré)
PlacCalib_SHP <- PlacCalib_SHP[, c("Vha", "mnh", "hdr", "Peuplt_IFN", "SER", "Geologie")] # , "Peuplt_IFN", "Station_Forestiere" #, "Station_Forestiere", "SER", "Geologie"
# PlacCalib_SHP <- PlacCalib_SHP[!is.na(PlacCalib_SHP$Vha), c("Vha", "mnh", "hdr", "Peuplt_IFN", "Station_Forestiere", "SER", "Geologie")]

PlacCalib_SHP <- spTransform(PlacCalib_SHP, 
                             CRS("+init = epsg:2154"))

# Sécurité si valeurs vides :
pos_NA <- c()
for (col in names(PlacCalib_SHP@data)) {
  pos_NA <- c(pos_NA, which(is.na(PlacCalib_SHP@data[, col])))
}
if (length(pos_NA) > 0) {
  PlacCalib_SHP <- PlacCalib_SHP[-pos_NA, ]
}

# PlacCalib_SHP@data <- mutate(PlacCalib_SHP@data, 
#                              Vha = ifelse(is.na(Vha), 
#                                         0, Vha), 
#                              mnh = ifelse(is.na(mnh), 
#                                         0, mnh), 
#                              hdr = ifelse(is.na(hdr), 
#                                         0, hdr), 
#                              Peuplt_IFN = ifelse(is.na(Peuplt_IFN), 
#                                                0, Peuplt_IFN), 
#                              Station_Forestiere = ifelse(is.na(Station_Forestiere), 
#                                                0, Station_Forestiere), 
#                              SER = ifelse(is.na(SER), 
#                                                0, SER), 
#                              Geologie = ifelse(is.na(Geologie), 
#                                                0, Geologie))
# Aires_Protection = ifelse(is.na(Aires_Protection), 
#                   0, Aires_Protection))

formula <- paste(Objet, " ~  Peuplt_IFN + mnh + hdr + SER + Geologie", # + Aires_Protection   #+ SER + Geologie
                 sep = "") #  + Peuplt_IFN + Station_Forestiere
# PlacCalib_SHP2 <- PlacCalib_SHP[, c("Vha", "mnh", "hdr", "Peuplt_IFN", "SER", "Geologie")] # , "Peuplt_IFN", "Station_Forestiere", "Aires_Protection"



# ----- Préparation du grid
res <- 20

ext <- extent(Bbox_SHP)

x_min <- floor(ext[1])
x_max <- x_min + floor((ext[2] - ext[1])/res)*res
y_min <- floor(ext[3])
y_max <- y_min + floor((ext[4] - ext[3])/res)*res
#Spatialisation du grid
grd <- expand.grid(x = seq(x_min, x_max, by = res), y = seq(y_min, y_max, by = res))
coordinates(grd) <- ~ x + y
grd@proj4string <- CRS("+init = epsg:2154")

extract_grd <- raster::extract(raster3, grd@coords, df = TRUE)
# %>%
#   mutate(mnh = ifelse(is.na(mnh), # Sécurité pour remplacer les NA
#                     0, mnh), 
#          hdr = ifelse(is.na(hdr), # Sécurité pour remplacer les NA
#                     0, hdr), 
#          Peuplt_IFN = ifelse(is.na(Peuplt_IFN), # Sécurité pour remplacer les NA
#                            0, Peuplt_IFN))
grd <- cbind(coordinates(grd), extract_grd)
coordinates(grd) <- ~ x + y
gridded(grd) <- TRUE
grd@proj4string <- CRS("+init = epsg:2154")

# Paramétrage du krigeage (variogramme)
# vr <- variogram(as.formula(formula), data = PlacCalib_SHP2, cutoff = 2000, width = 100)
# PlacCalib_SHP2 <- PlacCalib_SHP[, c("Vha", "mnh", "hdr", "Peuplt_IFN", "SER", "Geologie")]
vr <- variogram(as.formula(formula), data = PlacCalib_SHP, cutoff = 800, width = 70)
plot(vr)
vrmf <- fit.variogram(vr, 
                      vgm(psill = 300, 
                          model = "Sph", 
                          # range = 6000, 
                          nugget = 0))
# vr <- variogram(as.formula(formula), data = PlacCalib_SHP, cutoff = 2000, width = 100)
# plot(vr)
# vrmf <- fit.variogram(vr, 
#                       vgm(psill = 3000, 
#                           model = "Sph", 
#                           # range = 6000, 
#                           nugget = 0))
plot(vr, vrmf)
# ----- Interpolation
kr <- krige(as.formula(formula), locations = PlacCalib_SHP, newdata = grd, model = vrmf)
kr@data$var1.pred[which(kr@data$var1.pred < 0)] <- 0
#Transformation d'un SpatialPointDataFrame en SpatialGridDataFrame
gridded(kr) <- TRUE
#Rasterisation
kr_rast <- SpatialPixelsDataFrame(points = kr@coords, data = kr@data, 
                                  proj4string = CRS("+init = epsg:2154"))
kr_rast <- raster(kr_rast)
krVhaGBTGB_rast <- kr_rast
writeRaster(krVhaGBTGB_rast, 
            file = "Out/SIG/Raster/Krigeage/Vha_GBTGB", 
            format = "GTiff", 
            overwrite = T)

# ----- 19. Test vérification krigeage Vha de HET : -----
#Extraction des données du krigeage au niveau des placettes (raster)
extract_kr <- raster::extract(krVhaGBTGB_rast, PlacValid_SHP@coords, df = TRUE, buffer = 30, fun = mean)
extract_kr <- extract_kr[, 2]
# Simplification des donn?es des placettes de validation
# PlacValid_SHP <- Plac_SHP[echant, ]
PlacValid_DF <- subset(PlacValid_SHP@data, select = Objet)
# Fusion des deux ?chantillons
student <- cbind(extract_kr, PlacValid_DF)
# Calcul des erreurs relatives
temp <- student
temp$err <- abs(student[, 1] - student[, 2]) / student[, 2]
# err <- rbind(err, temp)

# test de Student
test <- t.test(student[, 1], student[, 2], paired = TRUE)
test_VhaGBTGB <- test
test
save(test_VhaHET, file = "Tables/TestStudent_VhaGBTGB.RData")
##### / #####


































































##### / #####


































































##### / #####
# ----- 11. Construction des données placettes pour le krigeage VcHa : -----
# --- Récupération des résultats sur le volume de GB et TGB
# --- Récupération des résultats sur le volume de GB et TGB
# Résultats d'analyse (data.frame) :

load("Tables/gfTablesElaboreesPlacTest.RData")
TabPla_GF <- TabPla

pos2 <- which(names(TabPla_GF) == "gfPlaFpied_")
assign(names(TabPla_GF)[pos2], TabPla_GF[[pos2]])
gfPlaFpied_ <- left_join(CyclesGF_DF, gfPlaFpied_)


# Résultats d'analyse (data.frame) :
# load("Tables/gfTablesElaboreesPlacTest.RData")
# TabPla_GF <- TabPla
#
# pos2 <- which(names(TabPla_GF) == "gfPlaFpied_")
# assign(names(TabPla_GF)[pos2], TabPla_GF[[pos2]])

# PlacGF_DF <- group_by(gfPlaFpied_, 
#                       NumForet) %>% # pas vraiment nécessaire
#   filter(Cycle == max(Cycle)) %>% # pas vraiment nécessaire
#   # group_by(NumForet, NumPlac, Cycle) %>%
#   # summarise(VcHa = sum(VcHa)) %>%
#   # ungroup() %>%
#   right_join(CyclesGF_DF) %>%
#   mutate(VcHa = ifelse(is.na(VcHa), 0, VcHa)) %>%
#   mutate(Num = paste0("GF-", NumForet, "-", NumPlac)) %>%
#   dplyr::select(Num, NumForet, NumPlac, Cycle, VcHa)

PlacGF_DF <- mutate(gfPlaFpied_, 
                    VcHa = ifelse(is.na(VcHa), 0, VcHa)) %>%
  mutate(Num = paste0("GF-", NumForet, "-", NumPlac)) %>%
  dplyr::select(Num, NumForet, NumPlac, Cycle, VcHa)


# pos_GB <- which(PlacGF_DF$Vha > 0)
# pos_GB2 <- c(1:dim(PlacGF_DF)[1])[-pos_GB][1:length(pos_GB)] # limite le nombre de placettes = 0
# PlacGF_DF <- PlacGF_DF[c(pos_GB, 
#                          pos_GB2), ]

Plac_DF <- PlacGF_DF


# Localisation des placettes d'inventaire :
repDataBrutes <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes"
# Fusion des shapes de placettes :
repGF <- paste0(dirname(repDataBrutes), 
                "/GF/")
ListGF_SHP4 <- c("Plac_Grossenwald_L93.shp", 
                 "Plac_Hardt_L93.shp", 
                 "Plac_Hohenfels_L93.shp", 
                 "Plac_MILITAIB_L93.shp", 
                 "Plac_PNRVN_L93.shp", 
                 "Plac_SchoeneckI_L93.shp", 
                 "Plac_SchoeneckII_GFVN_L93.shp", 
                 "Plac_SchoeneckIII_L93.shp", 
                 "Plac_SturzelbronnI_L93.shp", 
                 "Plac_SturzelbronnII_L93.shp", 
                 "Plac_Windstein_L93.shp", 
                 "Plac_Zittersheim_L93.shp")

List_SHP4 <- paste0(repGF, ListGF_SHP4)


Plac_SHP <- c()
# ListEchant <- list()
for (file in List_SHP4) {
  print(file)
  # file <- List_SHP4[1]
  shp_temp <- readOGR(dsn = dirname(file), 
                      layer = file_path_sans_ext(basename(file)), 
                      verbose = F, 
                      stringsAsFactors = F)
  # # shp_temp <- shp_temp[, c(match("NumFort", names(shp_temp)), 
  # #                         match("NumPlac", names(shp_temp)), 
  # #                         which(names(shp_temp) %in% c("Vha", "Hêtr_Vh")))]
  # # names(shp_temp) <- c("NumForet", "NumPlac", "Vha")
  # shp_temp <- shp_temp[, c("NumForet", "NumPlac", "Vha")]
  if (is.element("NumDisp", names(shp_temp))) {
    shp_temp@data <- mutate(shp_temp@data, 
                            Num = paste0("PSDRF-", NumDisp, "-", NumPlac), 
                            NumDisp = as.numeric(NumDisp)) %>%
      rename(NumForet = NumDisp) %>%
      dplyr::select(Num, NumForet, NumPlac)

  } else {
    shp_temp@data <- mutate(shp_temp@data, 
                            Num = paste0("GF-", NumForet, "-", NumPlac), 
                            NumForet = as.numeric(NumForet)) %>%
      dplyr::select(Num, NumForet, NumPlac)
  }
  shp_temp <- shp_temp[which(shp_temp$Num %in% Plac_DF$Num), ]

  # echant_temp <- sample(dim(shp_temp@data)[1], 
  #                       floor(p*dim(shp_temp@data)[1]), 
  #                       replace = FALSE)
  # echant_temp <- list(echant_temp)
  # names(echant_temp) <- paste0("PSDRF-", unique(shp_temp@data$NumForet))
  # ListEchant <- c(ListEchant, echant_temp)


  echant_temp <- sample(dim(shp_temp@data)[1], 
                        floor(p*dim(shp_temp@data)[1]), 
                        replace = FALSE)
  PlacCalib_temp_SHP <- shp_temp[echant_temp, ]
  PlacValid_temp_SHP <- shp_temp[-echant_temp, ]


  if (match(file, List_SHP4) == 1) {
    Plac_SHP <- shp_temp
    PlacCalib_SHP <- PlacCalib_temp_SHP
    PlacValid_SHP <- PlacValid_temp_SHP
  } else {
    Plac_SHP <- rbind(Plac_SHP, 
                      shp_temp)
    PlacCalib_SHP <- rbind(PlacCalib_SHP, PlacCalib_temp_SHP)
    PlacValid_SHP <- rbind(PlacValid_SHP, PlacValid_temp_SHP)
  }
}

Plac_SHP <- spTransform(Plac_SHP, 
                        CRS("+init = epsg:2154"))
PlacCalib_SHP <- spTransform(PlacCalib_SHP, 
                             CRS("+init = epsg:2154"))
PlacValid_SHP <- spTransform(PlacValid_SHP, 
                             CRS("+init = epsg:2154"))

# a <- Plac_DF[which(!Plac_DF$Num %in% Plac_SHP$Num), ] Placettes manquantes dans le shape
PlacSave_SHP <- Plac_SHP
# Plac_SHP <- Plac_SHP[which(Plac_SHP$Num %in% Plac_DF$Num), ]
Plac_SHP@data <- left_join(Plac_SHP@data, Plac_DF)
PlacCalib_SHP@data <- left_join(PlacCalib_SHP@data, Plac_DF)
PlacValid_SHP@data <- left_join(PlacValid_SHP@data, Plac_DF)

# ----- 14. Krigeage VcHa -----
# ----- Calibrage
# Choix du nombre de répétitions de la validation
# n <- 10

# Choix du pourcentage de calibration
p <- 0.85

# Choix de la variable objet du krigeage
# Objet <- "Vha"

# Initialisation des tableaux de stockage des p_values et des erreurs relatives
# p_value <- data.frame()
# err <- data.frame()


# --- Sélection des placettes de validation et de calibration
# # Vecteur contenant les numéros de placettes à mettre dans le jeu de calibration
# # while (Length_test <= 60) {
# echant <- sample(dim(Plac_SHP)[1], floor(p*dim(Plac_SHP)[1]), replace = FALSE)
# # Sélection des placettes de calibration
# PlacCalib_SHP <- Plac_SHP[echant, ]
#
# # Sélection des placettes de validation
# PlacPNRVN_SHP <- Plac_SHP[grep("GF-12-", Plac_SHP@data$Num), ] # sélection des placettes du
# # PNRVN pour validation (assure représentativité à l'échelle du territoire)
# echant <- which(!is.element(PlacPNRVN_SHP@data$Num, PlacCalib_SHP@data$Num))
# if (length(echant) < 30) {
if (dim(PlacValid_SHP)[1] < 30) {
  tk_messageBox(type = "ok", 
                message = "Attention : nombre de placettes de validation insuffisant. Relancer le tirage", 
                icon = "warning")
}
# PlacValid_SHP <- PlacPNRVN_SHP[echant, ]
# Length_test <- dim(PlacValid_SHP)[1]
# }

# ----- Modification du shape délimitant le domaine forestier ---
# Raccourcir -> supposer que le shape a déjà été lu auparavant
# shp_NAME <- file.choose()
# shpForet_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataProjet_Garde/Autres/PeupltIFN.shp"
# shpForet_SHP <- readOGR(dsn = dirname(shpForet_FILE), 
#                         layer = file_path_sans_ext(basename(shpForet_FILE)), 
#                         verbose = F, 
#                         stringsAsFactors = F) %>%
#   spTransform(CRS("+init = epsg:2154"))
# shpForest_SHP <- gUnaryUnion(shpForet_SHP)
# save(shpForest_SHP, 
#      file = "Tables/Perim_Foret.RData")
load("Tables/Perim_Foret.RData")


# Récupération des données du raster :
load("Tables/Raster_FIN.RData")
# raster3 <- mask(raster2, shpForest_SHP)
# plot(raster3)
# save(raster3, 
#      file = "Tables/Raster3_FIN.RData")
load("Tables/Raster3_FIN.RData")
# load("Tables/Raster_FIN.RData")

# Clip sur le shape des placettes
# Plac_SHP <- Plac_SHP[shpForest_SHP, ]

# Extraction des données du mnh au niveau des placettes (raster)
extract_raster3 <- raster::extract(raster3, PlacCalib_SHP@coords, df = TRUE)
# Jonction des données extraites avec la couche spatialisée placettes
# a <-
PlacCalib_SHP@data <- cbind(PlacCalib_SHP@data, 
                            extract_raster3)
PlacCalib_SHP <- spTransform(PlacCalib_SHP, 
                             CRS("+init = epsg:2154"))

# Sécurité si valeurs vides :
pos_NA <- c()
for (col in names(PlacCalib_SHP@data)) {
  pos_NA <- c(pos_NA, which(is.na(PlacCalib_SHP@data[, col])))
}
if (length(pos_NA) > 0) {
  PlacCalib_SHP <- PlacCalib_SHP[-pos_NA, ]
}

Objet <- "VcHa"

# Version sans boucle (uniquement 'Vha' considéré)
PlacCalib_SHP <- PlacCalib_SHP[, c("VcHa", "mnh", "hdr", "Peuplt_IFN", "Station_Forestiere", "SER", "Geologie")] # , "Peuplt_IFN", "Station_Forestiere"
# PlacCalib_SHP <- PlacCalib_SHP[!is.na(PlacCalib_SHP$Vha), c("Vha", "mnh", "hdr", "Peuplt_IFN", "Station_Forestiere", "SER", "Geologie")]

PlacCalib_SHP <- spTransform(PlacCalib_SHP, 
                             CRS("+init = epsg:2154"))

formula <- paste(Objet, " ~  Peuplt_IFN + mnh + hdr + SER + Geologie", # + Aires_Protection
                 sep = "") #  + Peuplt_IFN + Station_Forestiere
# PlacCalib_SHP2 <- PlacCalib_SHP[, c("Vha", "mnh", "hdr", "Peuplt_IFN", "SER", "Geologie")] # , "Peuplt_IFN", "Station_Forestiere", "Aires_Protection"



# ----- Préparation du grid
res <- 20

ext <- extent(Bbox_SHP)

x_min <- floor(ext[1])
x_max <- x_min + floor((ext[2]-ext[1])/res)*res
y_min <- floor(ext[3])
y_max <- y_min + floor((ext[4]-ext[3])/res)*res
#Spatialisation du grid
grd <- expand.grid(x = seq(x_min, x_max, by = res), y = seq(y_min, y_max, by = res))
coordinates(grd) <- ~ x+y
grd@proj4string <- CRS("+init = epsg:2154")

extract_grd <- raster::extract(raster3, grd@coords, df = TRUE)

grd <- cbind(coordinates(grd), extract_grd)
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
grd@proj4string <- CRS("+init = epsg:2154")

# Paramétrage du krigeage (variogramme)
# vr <- variogram(as.formula(formula), data = PlacCalib_SHP2, cutoff = 2000, width = 100)
# PlacCalib_SHP2 <- PlacCalib_SHP[, c("VcHa", "mnh", "hdr", "Peuplt_IFN", "SER", "Geologie")]
vr <- variogram(as.formula(formula), data = PlacCalib_SHP, cutoff = 2300, width = 210)
plot(vr)
vrmf <- fit.variogram(vr, 
                      vgm(psill = 150000000, 
                          model = "Sph", 
                          # range = 6000, 
                          nugget = 10000000))
# vr <- variogram(as.formula(formula), data = PlacCalib_SHP, cutoff = 2000, width = 100)
# plot(vr)
# vrmf <- fit.variogram(vr, 
#                       vgm(psill = 20000000, 
#                           model = "Sph"))
# range = 6000, 
# nugget = 0))
# nugget = 5000000))
plot(vr, vrmf)
# ----- Interpolation
kr <- krige(as.formula(formula), locations = PlacCalib_SHP, newdata = grd, model = vrmf)
kr@data$var1.pred[which(kr@data$var1.pred < 0)] <- 0
#Transformation d'un SpatialPointDataFrame en SpatialGridDataFrame
gridded(kr) <- TRUE
#Rasterisation
kr_rast <- SpatialPixelsDataFrame(points = kr@coords, data = kr@data, 
                                  proj4string = CRS("+init = epsg:2154"))
kr_rast <- raster(kr_rast)
krVcHa_rast <- kr_rast
writeRaster(krVcHa_rast, 
            file = "Out/SIG/Raster/Krigeage/VcHa", 
            format = "GTiff", 
            overwrite = T)

# ----- 16. Test vérification krigeage VcHa : -----
#Extraction des données du krigeage au niveau des placettes (raster)
extract_kr <- raster::extract(krVcHa_rast, PlacValid_SHP@coords, df = TRUE, buffer = 30, fun = mean)
extract_kr <- extract_kr[, 2]
# Simplification des donn?es des placettes de validation
# PlacValid_SHP <- Plac_SHP[echant, ]
PlacValid_SHP <- subset(PlacValid_SHP@data, select = Objet)
# Fusion des deux ?chantillons
student <- cbind(extract_kr, PlacValid_SHP)
# Calcul des erreurs relatives
temp <- student
temp$err <- abs(student[, 1]-student[, 2])/student[, 2]
# err <- rbind(err, temp)

# test de Student
test_VcHa <- t.test(student[, 1], student[, 2], paired = TRUE)
test_VcHa
save(test_VcHa, file = "Tables/TestStudent_VcHa.RData")
# load("Tables/TestStudent_VcHa.RData")



























##### / #####
##### 20. Electre #####
# ----- Etape Préliminaire : préparation des rasters sortants:
# Chargement raster2 (pour emprise) :
load("Tables/Raster_FIN.Rdata") # raster 2 = rayonnement + mnh

# Occupation du territoire :
# raster1_FILE <- file.choose()
raster1_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/SIG/Raster/Occupations_du_territoire/Occupations_du_territoire_Buff.tif"
raster1_RAS <- raster(raster1_FILE)

# Statuts de protection :
# raster2_FILE <- file.choose()
raster2_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/SIG/Raster/Statuts_de_protection/Statuts_de_protection_Buff.tif"
raster2_RAS <- raster(raster2_FILE)

# Etat du peuplement forestier :
# raster3_FILE <- file.choose()
# raster3_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/SIG/Raster/Etats_du_peuplement_forestier/Etats_du_peuplement_forestier_Buff.tif"
# raster3_RAS <- raster(raster3_FILE) # A supprimer

# Milieux :
# raster4_FILE <- file.choose()
raster3_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/SIG/Raster/Milieux/Milieux_Buff.tif"
raster3_RAS <- raster(raster3_FILE)

# Fragmentation :
# raster5_FILE <- file.choose()
raster4_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/SIG/Raster/Fragmentation/Fragmentation_Dist.tif"
raster4_RAS <- raster(raster4_FILE) # A changer en fragmentation

# Krigeage Vha GB HET:
# raster6_FILE <- file.choose()
raster5_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/SIG/Raster/Krigeage/Vha_HET.tif"
raster5_RAS <- raster(raster5_FILE)
raster5_RAS <- projectRaster(raster5_RAS, raster2, 
                             crs = CRS("+init = epsg:2154")) # Problème d'emprise



# Krigeage VcHa:
# raster6_FILE <- file.choose()
raster6_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/SIG/Raster/Krigeage/VcHa.tif"
raster6_RAS <- raster(raster6_FILE)
raster6_RAS <- projectRaster(raster6_RAS, raster2, 
                             crs = CRS("+init = epsg:2154")) # Problème d'emprise

raster7_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/SIG/Raster/Krigeage/Vha_GBTGB.tif"
raster7_RAS <- raster(raster7_FILE)
raster7_RAS <- projectRaster(raster7_RAS, raster2, 
                             crs = CRS("+init = epsg:2154"))

# rasterTot_RAS <- raster1_RAS + raster2_RAS + raster3_RAS + raster4_RAS + raster5_RAS + raster6_RAS
# rasterTot_RAS <- raster1_RAS + raster2_RAS + raster4_RAS + raster5_RAS +
#   raster6_RAS + raster7_RAS



# Raster Valeur de consommation nette (inclusion du coût d'exploitation) :
# - MNT :
mnt_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Rasters/MNT/MNT_PNRVN_L93.tif"
mnt_RAS0 <- raster(mnt_FILE)

mnt_RAS1 <- projectRaster(mnt_RAS0, raster2, 
                          crs = CRS("+init = epsg:2154"))

pente_RAS <- terrain(mnt_RAS1, 
                     opt = "slope", 
                     unit = "tangent")

# Pente monte jusque 110 %. Il faut un seuil au delà duquel la valeur ne monte plus
# Ou alors diviser par 4 -> au max l'augmentation de prix est de 25 %.
# writeRaster(pente_RAS, 
#             file = "Out/SIG/Raster/Pente_PNRVN_L93.tif", 
#             format = "GTiff", 
#             overwrite = T)

CoeffExploit_RAS <- pente_RAS
pos_70sup <- which(values(CoeffExploit_RAS) >= 0.7) # si pente est supérieure à 70%
# alors on considère qu'on est au taquet du prix max
values(CoeffExploit_RAS)[pos_70sup] <- 0.7 # on met en place la valuer taquet de 0.7

# CoeffExploit_RAS2 <- CoeffExploit_RAS
values(CoeffExploit_RAS) <- scales::rescale(values(CoeffExploit_RAS), 
                                            to = c(0, 0.25))
# raster6_RAS
raster61_RAS <- raster6_RAS*(1-CoeffExploit_RAS)

# plot(raster6_RAS)
# plot(raster61_RAS)

writeRaster(raster61_RAS, 
            file = "Out/SIG/Raster/Krigeage/VcHa_Net.tif", 
            format = "GTiff", 
            overwrite = T)
raster6_RAS <- raster61_RAS


# ---- Couche Maturité :
load("Tables/Shapes_Rewrite.RData")
KrigeageVha_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/SIG/Raster/Krigeage/Vha_GBTGB.tif"
KrigeageVha_RAS <- raster(KrigeageVha_FILE)
KrigeageVha_RAS <- projectRaster(KrigeageVha_RAS, raster2, 
                                 crs = CRS("+init = epsg:2154"))
maturite_RAS <- KrigeageVha_RAS
values(maturite_RAS) <- scales::rescale(values(maturite_RAS), 
                                        to = c(0, 100))
shp <- List_SHP2[[which(names(List_SHP2) == c("BOISMURS_DANS_FEUILLUS_A_FAVORISER"))]]
boismur_RAS <- mask(maturite_RAS, shp)
values(boismur_RAS) <- values(boismur_RAS)*0.1 # On va prendre 10% de la valeur et la
# rajouter au raster maturite_RAS  => +10% aux emplacements des
# "BOISMURS_DANS_FEUILLUS_A_FAVORISER"
# test <- projectRaster(boismur_RAS, maturite_RAS, 
#                       crs = CRS("+init = epsg:2154"))
pos_NA <- which(is.na(values(boismur_RAS)))
if (length(pos_NA) > 0) {
  values(boismur_RAS)[pos_NA] <- 0
}

maturite_RAS <- maturite_RAS + boismur_RAS
# values(maturite_RAS) <- scales::rescale(values(maturite_RAS), 
#                                         to = c(0, 100))

# min <- abs(min(values(maturite_RAS), na.rm = T))
# # pos_NA <- which(is.na(values(maturite_RAS)))
#
# values(maturite_RAS) <- values(maturite_RAS) + min # De cette façon, on est sûr
# # d'éliminer les valeurs négatives du raster et on peut commencer à rescaler.
#
# # pos_NA2 <- which(is.na(values(maturite_RAS)))
# # pos_NA == pos_NA2
#
# max <- max(values(maturite_RAS), na.rm = T)
# values(maturite_RAS) <- values(maturite_RAS)/max*100

writeRaster(maturite_RAS, 
            file = "Out/SIG/Raster/Krigeage/Vha_GBTGB_Maturite", 
            format = "GTiff", 
            overwrite = T)
raster7_RAS <- maturite_RAS


# ----- Recalage des valeurs des 8 rasters entre 0 et 20 (valeur neutre = 10)
for (i in 1:7) {
  print(i)
  ras <- get(paste0("raster", i, "_RAS"))
# plot(ras)
values(ras) <- scales::rescale(values(ras), 
                                to = c(0, 20))
# print(ras)
assign(paste0("raster", i, "_RAS"), ras)
}


# ----- Premier scénario : Somme des rasters et sélection des meilleures valeurs du
# raster (Viser un total de 100/150 îlots et épurer par rapport à la surface
# minimale (fixée à 1ha))
raster_Fin1 <- raster1_RAS + raster2_RAS + raster3_RAS + raster4_RAS +
  raster5_RAS - raster6_RAS + raster7_RAS # + raster8_RAS # Attention : minimiser Vcha_Net
plot(raster1_RAS)
plot(raster2_RAS)
plot(raster3_RAS)
plot(raster4_RAS)
plot(raster5_RAS)
plot(raster6_RAS)
plot(raster7_RAS)
plot(raster_Fin1)
raster_Fin1 <- stack(raster1_RAS, 
                     raster2_RAS, 
                     raster3_RAS, 
                     raster4_RAS, 
                     raster5_RAS, 
                     raster6_RAS, 
                     raster7_RAS)
raster_Fin1 <- sum(raster_Fin1, na.rm = T)
plot(raster_Fin1)
Nbre_Ilots <- 0
# taux <- 73 # Réponse pour avoir 100 îlots (sans contrôle sur la surface) : 79.2. Pblme -> une fois tri des surfaces  => 6 îlots
taux <- 81.6 # Réponse pour avoir 100 îlots entre 1 et 5 ha -> 72.1
rasterTemp_RAS <- raster_Fin1
while(Nbre_Ilots < 100) {
taux <- taux-0.1
print(taux)
# 1. Extraire les valeurs les plus hautes (taux variable - revoir valeur max du krigeage)
# raster_max <- max(getValues(rasterTemp_RAS), na.rm = T)
# seuil <- quantile(raster5_RAS, probs = 0.97) # on vise le quantile à 97% pour atteindre
# 50-60 m3. Si on est dans du feuillu, on doit avoir autour de 100-120 m3/ha.
# Au pire c'est de l'irrégulier -> on vise alors 50% du volume en GB/TGB donc 50-60m3

# seuil <- 60 # valeur en m3/ha à avoir pour les zones d'intérêt
# df <- data.frame(values = getValues(rasterTemp_RAS), 
#                  stringsAsFactors = F)
# ggplot() +
#   geom_boxplot(df, 
#                mapping = aes(x = "", y = values))

raster_max <- max(getValues(rasterTemp_RAS), na.rm = T)
# taux <- taux_DF$taux[match(i, taux_DF$raster)]
# taux <- 70

# seuil <- (100-taux)/100*raster_max
seuil <- taux/100*raster_max
rasterTemp_RAS1 <- rasterTemp_RAS >= seuil
# rasterTemp_RAS1 <- rasterTemp_RAS <= seuil
rasterTemp_RAS1 <- mask(rasterTemp_RAS1, PerimPNRVN_SHP)
# plot(PerimPNRVN_SHP, col = "red", type = "l")
# plot(rasterTemp_RAS1, add = T)

rasterTemp_RAS2 <- rasterTemp_RAS*rasterTemp_RAS1
# max(getValues(rasterTemp_RAS2), na.rm = T)
# plot(rasterTemp_RAS2)

pos <- which(values(rasterTemp_RAS2) == 0)
if (length(pos) > 0) {
  values(rasterTemp_RAS2)[pos] <- NA # on remplace les vides par 0
}
# plot(rasterTemp_RAS2)


# # -- Autre méthode : récupérer les zones où Vha GB HET > 50 m3/ha
# seuil <- 50 # valeur en m3/ha à avoir pour les zones d'intérêt
# pos_Zones <- which(values(rasterTemp_RAS) > 50)
# rasterTemp_RAS2 <- rasterTemp_RAS
# rasterTemp_RAS2 <- rasterTemp_RAS2[pos_Zones]

# 2. Vectoriser le raster
convert_STEP1 <- rasterToPolygons(rasterTemp_RAS2, dissolve = T)
# plot(convert_STEP1)
# convert_STEP1 <- mask(convert_STEP1, PerimPNRVN_SHP)
convert_STEP2 <- gBuffer(convert_STEP1, width = T)
convert_STEP3 <- disaggregate(convert_STEP2)
df <- data.frame(Surface = gArea(convert_STEP3, byid = T)/10000)
row.names(df) <- 1:dim(df)[1]
Ilots_SHP <- SpatialPolygonsDataFrame(convert_STEP3, 
                                      data = df)
# Nbre_Ilots <- dim(Ilots_SHP)[1]
# IlotsSave_SHP <- Ilots_SHP
Ilots_SHP <- spTransform(Ilots_SHP, 
                         CRS("+init = epsg:2154"))


# Subset selon un seuil de surface :
# surf_SEUIL <- 5000 # 0.5 ha
# Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface > surf_SEUIL), ]
# ListShp_SHP5 <- Ilots_SHP
# Subset selon un seuil de surface :
surf_SEUIL1 <- 1 # 1 ha
surf_SEUIL2 <- 9
Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface >= surf_SEUIL1), ]
# Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface <= surf_SEUIL2), ] # pas nécessaire -> si trop gros, piocher dedans pour avoir plus petit
Nbre_Ilots <- dim(Ilots_SHP)[1]
# if (i == 1) {
# ListShp_SHP5 <- Ilots_SHP
# } else {
#   ListShp_SHP5 <- rbind(ListShp_SHP5, Ilots_SHP)
# }
}

# 3. Ecriture du shape (Proposition Electre)
dir.create("Out", showWarnings = F)
dir.create("Out/SIG", showWarnings = F)
dir.create("Out/SIG/Vecteurs", showWarnings = F)
dir.create("Out/SIG/Vecteurs/Ilots", showWarnings = F)
writeOGR(Ilots_SHP, 
         dsn = "Out/SIG/Vecteurs/Ilots", 
         layer = "Ilots_Results1_Ancien", 
         driver = "ESRI Shapefile", 
         encoding = "UTF-8", 
         overwrite_layer = T)

Nbre_Ilots <- 0
taux <- 81.6
rasterTemp_RAS <- raster_Fin1
while(Nbre_Ilots < 100) {
  taux <- taux-0.1
  print(taux)
  # 1. Extraire les valeurs les plus hautes (taux variable - revoir valeur max du krigeage)
  # raster_max <- max(getValues(rasterTemp_RAS), na.rm = T)
  # seuil <- quantile(raster5_RAS, probs = 0.97) # on vise le quantile à 97% pour atteindre
  # 50-60 m3. Si on est dans du feuillu, on doit avoir autour de 100-120 m3/ha.
  # Au pire c'est de l'irrégulier -> on vise alors 50% du volume en GB/TGB donc 50-60m3

  # seuil <- 60 # valeur en m3/ha à avoir pour les zones d'intérêt
  # df <- data.frame(values = getValues(rasterTemp_RAS), 
  #                  stringsAsFactors = F)
  # ggplot() +
  #   geom_boxplot(df, 
  #                mapping = aes(x = "", y = values))

  raster_max <- max(getValues(rasterTemp_RAS), na.rm = T)
  # taux <- taux_DF$taux[match(i, taux_DF$raster)]
  # taux <- 70

  # seuil <- (100-taux)/100*raster_max
  seuil <- taux/100*raster_max
  rasterTemp_RAS1 <- rasterTemp_RAS >= seuil
  # rasterTemp_RAS1 <- rasterTemp_RAS <= seuil
  # plot(PerimPNRVN_SHP, col = "red", type = "l")
  # plot(rasterTemp_RAS1, add = T)

  rasterTemp_RAS2 <- rasterTemp_RAS*rasterTemp_RAS1
  # max(getValues(rasterTemp_RAS2), na.rm = T)
  # plot(rasterTemp_RAS2)

  pos <- which(values(rasterTemp_RAS2) == 0)
  if (length(pos) > 0) {
    values(rasterTemp_RAS2)[pos] <- NA # on remplace les vides par 0
  }
  # plot(rasterTemp_RAS2)
  rasterTemp_RAS2 <- mask(rasterTemp_RAS2, shpForest_SHP)


  # # -- Autre méthode : récupérer les zones où Vha GB HET > 50 m3/ha
  # seuil <- 50 # valeur en m3/ha à avoir pour les zones d'intérêt
  # pos_Zones <- which(values(rasterTemp_RAS) > 50)
  # rasterTemp_RAS2 <- rasterTemp_RAS
  # rasterTemp_RAS2 <- rasterTemp_RAS2[pos_Zones]

  # 2. Vectoriser le raster
  convert_STEP1 <- rasterToPolygons(rasterTemp_RAS2, dissolve = T)
  # plot(convert_STEP1)
  # convert_STEP1 <- mask(convert_STEP1, PerimPNRVN_SHP)
  convert_STEP2 <- gBuffer(convert_STEP1, width = T)
  convert_STEP3 <- disaggregate(convert_STEP2)
  df <- data.frame(Surface = gArea(convert_STEP3, byid = T)/10000)
  row.names(df) <- 1:dim(df)[1]
  Ilots_SHP <- SpatialPolygonsDataFrame(convert_STEP3, 
                                        data = df)
  # Nbre_Ilots <- dim(Ilots_SHP)[1]
  # IlotsSave_SHP <- Ilots_SHP
  Ilots_SHP <- spTransform(Ilots_SHP, 
                           CRS("+init = epsg:2154"))


  # Subset selon un seuil de surface :
  # surf_SEUIL <- 5000 # 0.5 ha
  # Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface > surf_SEUIL), ]
  # ListShp_SHP5 <- Ilots_SHP
  # Subset selon un seuil de surface :
  surf_SEUIL1 <- 1 # 1 ha
  surf_SEUIL2 <- 9
  Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface >= surf_SEUIL1), ]
  # Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface <= surf_SEUIL2), ] # pas nécessaire -> si trop gros, piocher dedans pour avoir plus petit
  Nbre_Ilots <- dim(Ilots_SHP)[1]
  # if (i == 1) {
  # ListShp_SHP5 <- Ilots_SHP
  # } else {
  #   ListShp_SHP5 <- rbind(ListShp_SHP5, Ilots_SHP)
  # }
}

dir.create("Out", showWarnings = F)
dir.create("Out/SIG", showWarnings = F)
dir.create("Out/SIG/Vecteurs", showWarnings = F)
dir.create("Out/SIG/Vecteurs/Ilots", showWarnings = F)
writeOGR(Ilots_SHP, 
         dsn = "Out/SIG/Vecteurs/Ilots", 
         layer = "Ilots_Results1", 
         driver = "ESRI Shapefile", 
         encoding = "UTF-8", 
         overwrite_layer = T)





# ----- Deuxième et troisième scénarios :
# + sélection des meilleures valeurs du raster "krigeage du volume de GBTGB de Hêtre"
# (Viser un total de 100/150 îlots et épurer par rapport à la surface minimale
# (fixée à 1ha)).
Nbre_Ilots <- 0
# Krigeage Vha GB HET:
# raster6_FILE <- file.choose()
rasterTemp_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/SIG/Raster/Krigeage/Vha_HET.tif"
rasterTemp_RAS <- raster(rasterTemp_FILE)
rasterTemp_RAS <- projectRaster(rasterTemp_RAS, raster2, 
                             crs = CRS("+init = epsg:2154"))
# taux <- 73 # Réponse pour avoir 100 îlots (sans contrôle sur la surface) : 79.2. Pblme -> une fois tri des surfaces  => 6 îlots
taux <- 50 # Réponse pour avoir 100 îlots entre 1 et 5 ha -> 72.1
seuil <- 140 #seuil de 60 m3/ha
# rasterTemp_RAS <- raster5_RAS
# while(Nbre_Ilots < 100) {
  # taux <- taux-0.1
  # seuil <- seuil-1
  # print(taux)
  print(seuil)
  # 1. Extraire les valeurs les plus hautes (taux variable - revoir valeur max du krigeage)
  # raster_max <- max(getValues(rasterTemp_RAS), na.rm = T)
  # seuil <- quantile(raster5_RAS, probs = 0.97) # on vise le quantile à 97% pour atteindre
  # 50-60 m3. Si on est dans du feuillu, on doit avoir autour de 100-120 m3/ha.
  # Au pire c'est de l'irrégulier -> on vise alors 50% du volume en GB/TGB donc 50-60m3

  # seuil <- 60 # valeur en m3/ha à avoir pour les zones d'intérêt
  # df <- data.frame(values = getValues(rasterTemp_RAS), 
  #                  stringsAsFactors = F)
  # ggplot() +
  #   geom_boxplot(df, 
  #                mapping = aes(x = "", y = values))

  # raster_max <- mean(getValues(rasterTemp_RAS), na.rm = T)
  # taux <- taux_DF$taux[match(i, taux_DF$raster)]
  # taux <- 70

  # seuil <- (100-taux)/100*raster_max
  # seuil <- taux/100*raster_max
  rasterTemp_RAS1 <- rasterTemp_RAS >= seuil
  plot(rasterTemp_RAS1)
  # rasterTemp_RAS1 <- rasterTemp_RAS <= seuil
  rasterTemp_RAS1 <- mask(rasterTemp_RAS1, PerimPNRVN_SHP)
  # plot(PerimPNRVN_SHP, col = "red", type = "l")
  # plot(rasterTemp_RAS1, add = T)

  rasterTemp_RAS2 <- rasterTemp_RAS*rasterTemp_RAS1
  # max(getValues(rasterTemp_RAS2), na.rm = T)
  # plot(rasterTemp_RAS2)

  pos <- which(values(rasterTemp_RAS2) == 0)
  if (length(pos) > 0) {
    values(rasterTemp_RAS2)[pos] <- NA # on remplace les vides par 0
  }
  # plot(rasterTemp_RAS2)


  # # -- Autre méthode : récupérer les zones où Vha GB HET > 50 m3/ha
  # seuil <- 50 # valeur en m3/ha à avoir pour les zones d'intérêt
  # pos_Zones <- which(values(rasterTemp_RAS) > 50)
  # rasterTemp_RAS2 <- rasterTemp_RAS
  # rasterTemp_RAS2 <- rasterTemp_RAS2[pos_Zones]

  # 2. Vectoriser le raster
  convert_STEP1 <- rasterToPolygons(rasterTemp_RAS2, dissolve = T)
  # plot(convert_STEP1)
  # convert_STEP1 <- mask(convert_STEP1, PerimPNRVN_SHP)
  convert_STEP2 <- gBuffer(convert_STEP1, width = T)
  convert_STEP3 <- disaggregate(convert_STEP2)
  df <- data.frame(Surface = gArea(convert_STEP3, byid = T)/10000)
  row.names(df) <- 1:dim(df)[1]
  Ilots_SHP <- SpatialPolygonsDataFrame(convert_STEP3, 
                                        data = df)
  Nbre_Ilots <- dim(Ilots_SHP)[1]
  # IlotsSave_SHP <- Ilots_SHP
  Ilots_SHP <- spTransform(Ilots_SHP, 
                           CRS("+init = epsg:2154"))


  # Subset selon un seuil de surface :
  # surf_SEUIL <- 5000 # 0.5 ha
  # Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface > surf_SEUIL), ]
  # ListShp_SHP5 <- Ilots_SHP
  # Subset selon un seuil de surface :
  surf_SEUIL1 <- 1 # 1 ha
  surf_SEUIL2 <- 9
  Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface >= surf_SEUIL1), ]
  # Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface <= surf_SEUIL2), ] # pas nécessaire -> si trop gros, piocher dedans pour avoir plus petit
  Nbre_Ilots <- dim(Ilots_SHP)[1]
  # if (i == 1) {
  # ListShp_SHP5 <- Ilots_SHP
  # } else {
  #   ListShp_SHP5 <- rbind(ListShp_SHP5, Ilots_SHP)
  # }
# }

# 3. Ecriture du shape (Proposition Electre)
dir.create("Out", showWarnings = F)
dir.create("Out/SIG", showWarnings = F)
dir.create("Out/SIG/Vecteurs", showWarnings = F)
dir.create("Out/SIG/Vecteurs/Ilots", showWarnings = F)
Ilots_SHP$Num <- row.names(Ilots_SHP)
writeOGR(Ilots_SHP, 
         dsn = "Out/SIG/Vecteurs/Ilots", 
         layer = "Ilots_Results2", 
         driver = "ESRI Shapefile", 
         encoding = "UTF-8", 
         overwrite_layer = T)
# test <- Ilots_SHP
# test$Num <- row.names(test)
# test2 <- Ilots_SHP[20:25, ]

# + sortir un shape des îlots ainsi déssinés

# + Récupérer les valeurs pour les différents critères sur ces zones (valeurs calibrées
# entre 0 et 20)


# ----- Vectorisation des valeurs hautes des rasters : création des alternatives
# taux_DF <- data.frame(raster = c(1, 2, 3, 4, 5, 6, 7, 8), 
#                       taux = c(40, 20, 30, 60, 75, 70), 
#                       stringsAsFactors = F)
# ListShp_SHP5 <- c() # RAJOUTER : mask périmètre
# for (i in 1) {
# # 1. Extraire les valeurs les plus hautes (taux variable - revoir valeur max du krigeage)
#   # i = 6
# rasterTemp_RAS <- get(paste0("raster", i, "_RAS"))
# raster_max <- max(getValues(rasterTemp_RAS), na.rm = T)
# # raster_max <- min(getValues(rasterTemp_RAS), na.rm = T)
# taux <- taux_DF$taux[match(i, taux_DF$raster)]
# # taux <- 0
#
# seuil <- (100-taux)/100*raster_max
# rasterTemp_RAS1 <- rasterTemp_RAS >= seuil
# # rasterTemp_RAS1 <- rasterTemp_RAS <= seuil
# plot(rasterTemp_RAS1)
# rasterTemp_RAS1 <- mask(rasterTemp_RAS1, shpForest_SHP))
#
# rasterTemp_RAS2 <- rasterTemp_RAS*rasterTemp_RAS1
# # max(getValues(rasterTemp_RAS2), na.rm = T)
# plot(rasterTemp_RAS2)
#
# pos <- which(values(rasterTemp_RAS2) == 0)
# if (length(pos) > 0) {
#   values(rasterTemp_RAS2)[pos] <- NA # on remplace les vides par 0
# }
# plot(rasterTemp_RAS2)
#
# # 2. Vectoriser le raster
# convert_STEP1 <- rasterToPolygons(rasterTemp_RAS2, dissolve = T)
# convert_STEP2 <- gBuffer(convert_STEP1, width = T)
# convert_STEP3 <- disaggregate(convert_STEP2)
# df <- data.frame(Surface = gArea(convert_STEP3, byid = T)/10000)
# row.names(df) <- 1:dim(df)[1]
# Ilots_SHP <- SpatialPolygonsDataFrame(convert_STEP3, 
#                                       data = df)
# Ilots_SHP <- spTransform(Ilots_SHP, 
#                          CRS("+init = epsg:2154"))
#
#
# # Subset selon un seuil de surface :
# # surf_SEUIL <- 5000 # 5 ha
# # Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface > surf_SEUIL), ]
# # ListShp_SHP5 <- Ilots_SHP
# if (i == 1) {
#   ListShp_SHP5 <- Ilots_SHP
# } else {
#   ListShp_SHP5 <- rbind(ListShp_SHP5, Ilots_SHP)
# }
#
# # 3. Ecriture du shape (Proposition Electre)
# dir.create("Out", showWarnings = F)
# dir.create("Out/SIG", showWarnings = F)
# dir.create("Out/SIG/Vecteurs", showWarnings = F)
# dir.create("Out/SIG/Vecteurs/Ilots", showWarnings = F)
# writeOGR(Ilots_SHP, 
#          dsn = "Out/SIG/Vecteurs/Ilots", 
#          layer = paste0("raster", i, "_Alternative"), 
#          driver = "ESRI Shapefile", 
#          encoding = "UTF-8", 
#          overwrite_layer = T)
# }









# # Périmètre du parc :
# PerimPNRVN_FILE <- paste0(rep_Ilots, "/Data/SIG/Vecteurs/DataBrutes/Perimetres/Perimetre2014.shp")
# # PerimPNRVN_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Stations/StationsONF.shp"
# PerimPNRVN_SHP <- readOGR(dsn = dirname(PerimPNRVN_FILE), 
#                           layer = file_path_sans_ext(basename(PerimPNRVN_FILE)), 
#                           verbose = F, 
#                           stringsAsFactors = F)
# PerimPNRVN_SHP <- spTransform(PerimPNRVN_SHP, 
#                               CRS("+init = epsg:2154"))
# i = 5
# rasterTemp_RAS <- get(paste0("raster", i, "_RAS"))
# seuil <- 74 # -> avec boucle while, on arrive à un seuil de 82 m3/ha pour avoir 100 îlots
# # Finalemnet, on a préféré 150 ILS car sur les 100, 30 % tombaient chez Evrard.
#
# # Nbre_Ilots <- 10000
# # while(Nbre_Ilots > 100) {
# seuil <- seuil+1
# print(seuil)
# # for (i in 1) {
# # 1. Extraire les valeurs les plus hautes (taux variable - revoir valeur max du krigeage)
# # raster_max <- max(getValues(rasterTemp_RAS), na.rm = T)
# # seuil <- quantile(raster5_RAS, probs = 0.97) # on vise le quantile à 97% pour atteindre
# # 50-60 m3. Si on est dans du feuillu, on doit avoir autour de 100-120 m3/ha.
# # Au pire c'est de l'irrégulier -> on vise alors 50% du volume en GB/TGB donc 50-60m3
#
# # seuil <- 60 # valeur en m3/ha à avoir pour les zones d'intérêt
# # df <- data.frame(values = getValues(rasterTemp_RAS), 
# #                  stringsAsFactors = F)
# # ggplot() +
# #   geom_boxplot(df, 
# #                mapping = aes(x = "", y = values))
#
# # raster_max <- min(getValues(rasterTemp_RAS), na.rm = T)
# # taux <- taux_DF$taux[match(i, taux_DF$raster)]
# # taux <- 70
#
# # seuil <- (100-taux)/100*raster_max
# rasterTemp_RAS1 <- rasterTemp_RAS >= seuil
# # rasterTemp_RAS1 <- rasterTemp_RAS <= seuil
# rasterTemp_RAS1 <- mask(rasterTemp_RAS1, PerimPNRVN_SHP)
# # plot(PerimPNRVN_SHP, col = "red", type = "l")
# # plot(rasterTemp_RAS1, add = T)
#
# rasterTemp_RAS2 <- rasterTemp_RAS*rasterTemp_RAS1
# # max(getValues(rasterTemp_RAS2), na.rm = T)
# # plot(rasterTemp_RAS2)
#
# pos <- which(values(rasterTemp_RAS2) == 0)
# if (length(pos) > 0) {
#   values(rasterTemp_RAS2)[pos] <- NA # on remplace les vides par 0
# }
# # plot(rasterTemp_RAS2)
#
#
# # # -- Autre méthode : récupérer les zones où Vha GB HET > 50 m3/ha
# # seuil <- 50 # valeur en m3/ha à avoir pour les zones d'intérêt
# # pos_Zones <- which(values(rasterTemp_RAS) > 50)
# # rasterTemp_RAS2 <- rasterTemp_RAS
# # rasterTemp_RAS2 <- rasterTemp_RAS2[pos_Zones]
#
# # 2. Vectoriser le raster
# convert_STEP1 <- rasterToPolygons(rasterTemp_RAS2, dissolve = T)
# # plot(convert_STEP1)
# # convert_STEP1 <- mask(convert_STEP1, PerimPNRVN_SHP)
# convert_STEP2 <- gBuffer(convert_STEP1, width = T)
# convert_STEP3 <- disaggregate(convert_STEP2)
# df <- data.frame(Surface = gArea(convert_STEP3, byid = T)/10000)
# row.names(df) <- 1:dim(df)[1]
# Ilots_SHP <- SpatialPolygonsDataFrame(convert_STEP3, 
#                                       data = df)
# Nbre_Ilots <- dim(Ilots_SHP)[1]
# # }
# Ilots_SHP <- spTransform(Ilots_SHP, 
#                          CRS("+init = epsg:2154"))
#
#
# # Subset selon un seuil de surface :
# # surf_SEUIL <- 5000 # 0.5 ha
# # Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface > surf_SEUIL), ]
# # ListShp_SHP5 <- Ilots_SHP
# # Subset selon un seuil de surface :
# surf_SEUIL1 <- 1 # 1 ha
# surf_SEUIL2 <- 9
# Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface >= surf_SEUIL1), ]
# # Ilots_SHP <- Ilots_SHP[which(Ilots_SHP@data$Surface <= surf_SEUIL2), ] # pas nécessaire -> si trop gros, piocher dedans pour avoir plus petit
# Nbre_Ilots <- dim(Ilots_SHP)[1]
# # if (i == 1) {
# ListShp_SHP5 <- Ilots_SHP
# # } else {
# #   ListShp_SHP5 <- rbind(ListShp_SHP5, Ilots_SHP)
# # }
#
# # 3. Ecriture du shape (Proposition Electre)
# dir.create("Out", showWarnings = F)
# dir.create("Out/SIG", showWarnings = F)
# dir.create("Out/SIG/Vecteurs", showWarnings = F)
# dir.create("Out/SIG/Vecteurs/Ilots", showWarnings = F)
writeOGR(Ilots_SHP, 
         dsn = "Out/SIG/Vecteurs/Ilots", 
         layer = "Ilots_Results", 
         driver = "ESRI Shapefile", 
         encoding = "UTF-8", 
         overwrite_layer = T)
# # }
# plot(Ilots_SHP)


# ----- Autre critère = évaluation de la répartition des ILS :
# --- Distance entre les îlots :
DistanceIlots_DF <- as.data.frame(gDistance(Ilots_SHP, byid = T)) %>%
  mutate(DistMin = NA)
# DistanceIlots_DF$NumIlot <- row.names(DistanceIlots_DF)
DistanceIlots_DF$NumIlot <- Ilots_SHP$Num
for (row in 1:dim(DistanceIlots_DF)[1]) {
  DistanceIlots_DF$DistMin[row] <- min(DistanceIlots_DF[row, 
                                                        -c(which(is.element(names(DistanceIlots_DF), c("NumIlot", "DistMin"))), row)])
}

DistanceIlots_DF <- dplyr::select(DistanceIlots_DF, 
                                  NumIlot, DistMin)
Question : pourquoi il y a des distances min égale à ?

Surface_Foret <- gArea(shpForest_SHP)/10000 # Surface du domaine forestier
Densite_Optimale <- Surface_Foret/Nbre_Ilots # Densité optimale (1 ILS par la surface
# "Densite_Optimale")
DistanceIlots_DF <- mutate(DistanceIlots_DF, 
                           Ecart = abs(Densite_Optimale-DistMin))


DistanceIlots_DF <- mutate(DistanceIlots_DF, 
               Densite = rescale(Ecart, 
                            to = c(20, 0)))













# ----- 20.5 Création du tableau d'entrée pour Electre 3 : -----
#
# Ilots_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/SIG/Vecteurs/Ilots/Ilots_Results2.shp"
# Ilots_SHP <- readOGR(dsn = dirname(Ilots_FILE), 
#                      layer = file_path_sans_ext(basename(Ilots_FILE)), 
#                      verbose = F, 
#                      stringsAsFactors = F) %>%
#   spTransform(CRS("+init = epsg:2154"))
#  = récupération des valeurs des alternatives pour les différents critères
# N.B : rajouter impact financier (+1 raster) et rajouter critère coût déplacement
# ListShp_SHP5 <- ListShp_SHP5[1:10, ]
# names(ListShp_SHP5) <- paste0("ALT_", 1:length(ListShp_SHP5))
ListShp_SHP5 <- Ilots_SHP
ListShp_SHP6 <- c()
df <- data.frame(raster1 = numeric(), 
                 raster2 = numeric(), 
                 raster3 = numeric(), 
                 raster4 = numeric(), 
                 # raster5 = numeric(), 
                 raster6 = numeric(), 
                 # raster7 = numeric(), 
                 raster8 = numeric(), 
                 NumIlot = character(), 
                 stringsAsFactors = F) #, raster7 = numeric()
for (k in 1:length(ListShp_SHP5)) {
  shp <- ListShp_SHP5[k, ]
  df[k, 1] <- raster::extract(raster1_RAS, shp, fun = mean, na.rm = T)
  df[k, 2] <- raster::extract(raster2_RAS, shp, fun = mean, na.rm = T)
  df[k, 3] <- raster::extract(raster3_RAS, shp, fun = mean, na.rm = T)
  df[k, 4] <- raster::extract(raster4_RAS, shp, fun = mean, na.rm = T)
  # df[k, 5] <- raster::extract(raster5_RAS, shp, fun = mean)
  df[k, 5] <- raster::extract(raster6_RAS, shp, fun = mean, na.rm = T)
  df[k, 6] <- raster::extract(raster7_RAS, shp, fun = mean, na.rm = T)
  # print(row.names(ListShp_SHP5[k, ]))
  df[k, "NumIlot"] <- row.names(ListShp_SHP5[k, ])
  # df[k, 5] <- raster::extract(raster8_RAS, shp, fun = mean)
  # df[k, 7] <- extract(raster7_RAS, shp, fun = mean)
  row.names(df)[k] <- paste0("ALT_", row.names(ListShp_SHP5[k, ]))
  ListShp_SHP6 <- c(ListShp_SHP6, list(shp))
  names(ListShp_SHP6)[length(ListShp_SHP6)] <- paste0("ALT_", k)
}
df1 <- rename(df, 
             Occupations_du_territoire = raster1, 
             Statuts_de_Protection = raster2, 
             Milieux = raster3, 
             Fragmentation = raster4, 
             Vcha = raster6, 
             Maturite = raster8) %>%
  left_join(DistanceIlots_DF) %>%
  select(NumIlot, 
         Occupations_du_territoire, 
         Statuts_de_Protection, 
         Milieux, 
         Fragmentation, 
         Vcha, 
         Maturite, 
         Densite)
save(df1, file = "Tables/ElectreDF_Fin.RData")
write.xlsx(df1, 
           file = "Out/Excel/ForElectre.xlsx")
# colnames(df) <- c("Occupations_du_territoire", "Statuts_de_protection", 
#                   "Etats_du_peuplement_forestier", 
#                   "Milieux", "Connexions", "Krigeage_Vha", "Krigeage_VcHa")
colnames(df) <- c(names(raster1_RAS), 
                  names(raster2_RAS), 
                  names(raster3_RAS), 
                  names(raster4_RAS), 
                  # names(raster5_RAS), 
                  names(raster6_RAS), 
                  # names(raster7_RAS), 
                  names(raster8_RAS))

# # Question : garder les îlots avec des valeurs vides pour Vha et VcHa (ne sont-ils pas hors forêt ?)
# pos_NA <- which(is.na(df[, 6]))
# if (length(pos_NA) > 0) {
#   df[pos_NA, 6] <- 0
# }
# pos_NA <- which(is.na(df[, 7]))
# if (length(pos_NA) > 0) {
#   df[pos_NA, 7] <- 0
# }

## the performance table
performanceMatrix <- as.matrix(df[1:100, ])
# Vector containing names of alternatives
alternatives <- row.names(df[1:100, ])
# Vector containing names of criteria
criteria <- colnames(df[1:100, ])
#  vector indicating the direction of the criteria evaluation .
minmaxcriteria <- rep("max", dim(df[1:100, ])[2])
# criteriaWeights vector
criteriaWeights <- rep(1, dim(df[1:100, ])[2])
# thresholds vector

IndifferenceThresholds <- rep(10, dim(df[1:100, ])[2])
PreferenceThresholds <- rep(40, dim(df[1:100, ])[2])
VetoThresholds <- rep(90, dim(df[1:100, ])[2])

# performanceMatrix <- performanceMatrix[1:100, ]




# ----- 21. Ecriture des résultats d'Electre : -----


# Création des styles de cellules
Style1 <- createStyle(fontName = "Arial", fontColour = "black", border = "TopBottomLeftRight", 
                      fgFill = "lightskyblue", textDecoration = "bold", wrapText = F, 
                      valign = "center", halign = "center", 
                      textRotation = 0)
Style2 <- createStyle(fontName = "Arial", wrapText = T, 
                      valign = "center", halign = "center")

wb <- createWorkbook()
for (i in 1:length(Electre_List)) {
  # sheet <- ListSheet2[5]
  sheet <- names(Electre_List[i])
  print(sheet)
  df <- as.data.frame(Electre_List[[i]])
  name_sheet <- gsub(" ", "_", sheet, fixed = T)
  sheet <- gsub("'", "", sheet, fixed = T)
  sheet <- gsub("\u00EA", "e", sheet, fixed = T)
  sheet <- gsub("\u00E2", "a", sheet, fixed = T)
  sheet <- gsub("\u00E9", "e", sheet, fixed = T)
  sheet <- gsub("\u00E8", "e", sheet, fixed = T)
  sheet <- gsub("\u00FB", "u", sheet, fixed = T)
  sheet <- gsub("\u00EE", "i", sheet, fixed = T)
  sheet <- gsub("\u00F4", "o", sheet, fixed = T)


  addWorksheet(wb, name_sheet)

  # Ecriture des données dans les feuilles correspondantes
  addStyle(wb, 
           sheet = name_sheet, Style1, rows = 1, cols = 1:dim(df_temp)[2], 
           gridExpand = T)
  addStyle(wb, 
           sheet = name_sheet, Style2, rows = 1+(1:dim(df_temp)[1]), cols = 1:dim(df_temp)[2], 
           gridExpand = T)

  writeData(wb, name_sheet, df)
  removeColWidths(wb, sheet = name_sheet, cols = 1:dim(df)[2])
  setColWidths(wb, sheet = name_sheet, 
               cols = 1:dim(df)[2], widths = rep("auto", dim(df)[2]))
}
saveWorkbook(wb, 
             "Out/Excel/Electre.xlsx", 
             overwrite = T)


# Sortie des shapes des îlots avec leur rang
IlotsFin_SHP <- c()
for (i in 1:length(ListShp_SHP6)) {
  shp <- ListShp_SHP6[[i]]
  Id <- names(ListShp_SHP6[i])
  shp@data$Id <- Id
  if (i == 1) {
    IlotsFin_SHP <- shp
    IlotsFin_SHP@data$Id <- Id
  } else {
    IlotsFin_SHP <- rbind(IlotsFin_SHP, shp)
  }
}


# Rangement des shapes selon la table "Final Ranking"
FinalRanking_DF <- Electre_List[[which(names(Electre_List) == "Final Ranking Matrix")]]
IlotsFin_SHP <- IlotsFin_SHP[match(FinalRanking_DF$alternative, IlotsFin_SHP@data$Id), ]

writeOGR(IlotsFin_SHP, 
         dsn = "Out/Vecteurs/Ilots_TriElectre_L93", 
         layer = "Ilots_TriElectre_L93", 
         driver = "ESRI Shapefile", 
         encoding = "UTF-8", 
         overwrite_layer = T)






# Objectif Mercredi 22/11 :
  Récupérer le shape des îlots possibles et leurs notes pour tous ces critères. Ensuite : à mouliner dans classeur Excel
