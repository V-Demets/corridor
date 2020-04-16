#' Première étape du traitement de données permettant de délimiter les îlots de sénescence au sein
#' d'un territoire.
#'
#' @description Liste des champs dans les couches recensées par la fonction ListInfos. A cette liste
#' sont ajoutés des colonnes permettant de paramétrer la suite du traitement de données, et
#' notamment la sélection et la transformation des données au format raster.
#'
#' @return La fonction exporte un fichier Excel contenant le liste des couches avec leurs tables
#' attributaires.
#'
#' @author Bruciamacchie Max, Demets Valentin
#'
#' @import tcltk
#' @import stringr
#' @import tools
#' @import dplyr
#' @import reshape2
#' @import openxlsx
#' @import foreign
#'
#' @examples
#' \dontrun{
#' res <- ListInfos()
#' tab <- ListChamps(res$rep, res$tableau)
#'}
#'
#' @export

  library(foreign) # TODO : à intégrer dans NAMESPACE -> pour read.dbf

ListChamps2 <- function(
  rep1, rep2, 
  df_Infos
  ) {
  rep1 <- "/Users/Valentin/Travail/Outils/GitHub/corridor" # debug
  rep2 <- file.path(rep1, "data/raw") # debug
  df_Infos <- Path_DF # debug
  # rep1<- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN"
  # setwd(rep1)
  #
  # rep2 <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes"
  # df_Infos <- Path_DF
  
  
  setwd(rep2) # TODO : mieux gérer les différents répertoires
  
  # -- ancienne écriture
  # -- autre écriture
  df <- raw_files_tree %>% select(Id, source) %>% distinct()
  list_raw_shp <- df$source ; names(list_raw_shp) <- df$Id # list_raw_shp = Shp_FILES
  
  # Initialisation boucle
  shp_schema <- data.frame( 
    Id = character(), 
    shp = character(), 
    Attributs = character(), 
    stringsAsFactors = F
  )
  
  # espace de sauvegarde des tables attributaires déjà lues
  shp_tab <- c() # shp_tab = ListShp_DF 
  
  # barre de progression
  pb <- tkProgressBar(
    title = "Progression", 
    label = "Importation des shapes (%)", 
    min = 0, max = 100, width = 500
  )
  
  for (i in 1:length(list_raw_shp)) {
    # i <- 1 # debug
    file_name <- names(list_raw_shp)[i]
    # TODO : gérer le cas où les fichiers sont au format mapinfo (.TAB, ...)
    # file <- paste0(str_sub(list_raw_shp[[i]], 1, -4), "dbf")
    file <- sub(pattern = ".shp", replacement = ".dbf", list_raw_shp[i])
    
    if (is.element(file, list_raw_files_tree)) { # sécurité si pas de .dbf présent
      tab <- read.dbf(file, as.is = T)
    } else {
      stop("fichier ", file, " introuvable dans le dossier ", rep2)
    }
    
    # sauvegarde de la table attributaire lue :
    shp_tab <- c(shp_tab, list(tab))
    names(shp_tab)[i] <- file_name
    col_names <- names(tab)
    # col_nb <- length(col_names)
    layer <- file_path_sans_ext(basename(file))
    
    # création de la table du classeur 'Parametres_SIG'
    df1 <- data.frame(
      Id = file_name, 
      source = file, 
      shp = layer, 
      Attributs = col_names, 
      stringsAsFactors = F
    )
    shp_schema <- rbind(shp_schema, df1)
    
    info <- round(i / length(list_raw_shp) * 100)
    print(file)
    setTkProgressBar(
      pb = pb, 
      value = info, 
      title = paste0("Lecture des tables attributaires en cours : (", info, " %)"), 
      label = paste0(info, "% done")
    )
  } # end of loop 1:length(list_raw_shp)
  
  close(pb)
  
  
  # Tableau paramétrage pour la réécriture des shape et la construction des raster:
  shp_schema <- 
    shp_schema %>% 
    mutate(
      Encodage = NA, 
      Reecrire_SHP = NA, 
      Union_Champ = NA, 
      Thematique_RAS = NA, # Voir si à supprimer par la suite
      Commentaires = NA
    )
  
  # ----- Ecriture du tableau permettant de paramétrer les shapes à réécrire ou à intégrer dans le raster
  answ <- tk_messageBox(
    type = "yesno", 
    message = "Enregistrer le classeur listant les champs des tables attributaires\n(permet de choisir les champs \u00E0 inclure)?"
  )
  if (answ == "yes") {
    # Création des dossiers nécessaires
    output_dir <- "out/excel" # TODO : à définir comme argument d'entrée ?
    dir.create(output_dir, showWarnings = F, recursive = T)
    
    # Création du classeur et des feuilles nécessaires
    wb <- createWorkbook()
    addWorksheet(wb, "Parametrage_SIG")
    
    # Création des styles de cellules # TODO : rassembler les styles -> faire 1 fonction pour écriture des classeurs (rassemble les différentes versions de classeur à écrire + les différents styles)
    Style1 <- createStyle(
      fontName = "Arial", fontColour = "black", 
      border =  "TopBottomLeftRight", 
      fgFill = "lightskyblue", textDecoration = "bold", wrapText = F, 
      valign = "center", halign = "center", 
      textRotation = 0
    )
    Style2 <- createStyle(
      fontName = "Arial", wrapText = T, 
      valign = "center", halign = "center"
    )
    
    # Ecriture des données dans les feuilles correspondantes
    addStyle(
      wb, 
      sheet = "Parametrage_SIG", 
      Style1, 
      rows = 1, cols = 1:dim(shp_schema)[2], 
      gridExpand = T
    )
    addStyle(
      wb, 
      sheet = "Parametrage_SIG", 
      Style2, 
      rows = 1 + (1:dim(shp_schema)[1]), cols = 1:dim(shp_schema)[2], 
      gridExpand = T
    )
    
    writeData(wb, "Parametrage_SIG", shp_schema)
    removeColWidths(wb, sheet = "Parametrage_SIG", cols = 1:dim(shp_schema)[2])
    setColWidths(
      wb, 
      sheet = "Parametrage_SIG", 
      cols = 1:dim(shp_schema)[2], 
      widths = rep("auto", dim(shp_schema)[2])
    )
    
    # Sauvegarde du classeur
    setwd(rep1)
    saveWorkbook(
      wb, 
      file = file.path(output_dir, "Parametres_SIG.xlsx"), 
      overwrite = T
    )
    
    # -- message de fin
    msg <- tk_messageBox(
      type = "ok", 
      message = paste0(
        "Le classeur 'Parametres_SIG.xlsx' a \u00E9t\u00E9 \u00E9crit \u00E0 l'emplacement : \n\n", 
        file.path(rep1, output_dir)
      )
    )
  }
  out <- list(shp_schema)
  names(out) <- c("Parametrage_SIG")
  return(out)
  
  
  # -- save attribute tables
  setwd(rep1)
  dir.create("tables")
  save(
    shp_tab, 
    file = "tables/shp_attrs_tables.Rdata" # TablesAttributaires_SHP.RData
  )
}



#################### styles excel (ListChamps2) - 01 ####################
# Création des styles de cellules # TODO : rassembler les styles -> faire 1 fonction pour écriture des classeurs (rassemble les différentes versions de classeur à écrire + les différents styles)
Style1 <- createStyle(
  fontName = "Arial", fontColour = "black", 
  border =  "TopBottomLeftRight", fgFill = "lightskyblue", 
  textDecoration = "bold", wrapText = F, 
  valign = "center", halign = "center", 
  textRotation = 0
)
Style2 <- createStyle(
  fontName = "Arial", wrapText = T, 
  valign = "center", halign = "center"
)
#################### / \ ####################

# ----- fonction d'écriture du classeur Parametrage_SIG.xlsx
write_wb_1 <- function(
  df, sheet, output_dir = "out/excel"
) {
  # df <- shp_schema # debug
  # sheet <- "Parametrage_SIG" # debug
  
  # -- création des dossiers nécessaires
  dir.create(output_dir, showWarnings = F, recursive = T)
  
  # -- création du classeur et des feuilles nécessaires
  wb <- createWorkbook()
  addWorksheet(wb, sheet)
  
  # -- écriture des données
  writeData(wb, sheet, df)
  
  # -- styles et mise en forme
  # styles
  addStyle(
    wb, 
    sheet = sheet, 
    Style1, 
    rows = 1, cols = 1:dim(df)[2], 
    gridExpand = T
  )
  addStyle(
    wb, 
    sheet = sheet, 
    Style2, 
    rows = 1 + (1:dim(df)[1]), cols = 1:dim(df)[2], 
    gridExpand = T
  )
  # mise en forme
  removeColWidths(wb, sheet = sheet, cols = 1:dim(df)[2])
  setColWidths(
    wb, 
    sheet = sheet, 
    cols = 1:dim(df)[2], 
    widths = rep("auto", dim(df)[2])
  )
  
  # -- sauvegarde du classeur
  setwd(rep1)
  saveWorkbook(
    wb, 
    file = file.path(output_dir, paste0(sheet, ".xlsx")), 
    overwrite = T
  )

  # -- message
  msg <- tk_messageBox(
    type = "ok", 
    message = paste0(
      "Le classeur '", sheet, ".xlsx' a \u00E9t\u00E9 \u00E9crit \u00E0 l'emplacement : \n\n", 
      paste0(rep1, output_dir)
    )
  )
}
  
