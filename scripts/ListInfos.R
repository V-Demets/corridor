#' Liste des couches sous format shp.
#'
#' @description Import des données brutes par département, fusion et découpe selon le périmètre de la zone d'étude.
#'
#' @return La fonction exporte un data frame contenant le liste des shapes avec leurs adresses ainsi que
#' l'adresse du répertoire contenant ces informations.
#'
#' @author Bruciamacchie Max, Demets Valentin
#'
#' @import tcltk
#' @import stringr
#' @import tools
#' @import dplyr
#' @import reshape2
#'
#' @export

############### Liste des couches vectorielles ###############
ListInfos <- function() {
  # -- Définition du répertoire de travail
  rep_Ilots <- tk_choose.dir(default=getwd(),
                             caption="Choix du répertoire de travail")
  setwd(rep_Ilots)

  # -- Choix du répertoire contenant les données vecteurs
  rep_SHP <- tk_choose.dir(default="SIG/Vecteurs/DataBrutes", #getwd(),
                           caption="Choix du répertoire contenant les données vecteurs brutes")

  # -- Récupération des fichiers contenus dans rep_SHP
  List_DIRS <- list.dirs(path=rep_SHP)
  List_FILES <- list.files(path=List_DIRS, full.names=T)
  pos_rep <- str_locate(List_FILES, rep_SHP) # construction chemin relatif :
  # supprime le chemin du répertoire de base dans le nom des fichiers
  List_FILES <- str_sub(List_FILES, pos_rep[,2]+2,-1)

  df1 <- data.frame(Dir_Source=dirname(List_FILES),
                    Source=List_FILES,
                    # Fichier=file_path_sans_ext(List_FILES),
                    Extent=str_sub(List_FILES,-4,-1),
                    stringsAsFactors=F) %>%
    filter(Extent==".shp") %>%
    mutate(Couche=basename(Source),
           Couche=str_sub(Couche,1,-5)) %>%
    arrange(Couche) %>%
    mutate(Id=factor(Source),
           Id=as.numeric(Id),
           Id=paste0("ID_",Id))

  List <- str_split(df1$Dir_Source,"/")
  df2 <- data.frame(Id=rep.int(df1$Id, sapply(List, length)),
                    Dir_Source=rep.int(df1$Dir_Source, sapply(List, length)),
                    Source=rep.int(df1$Source, sapply(List, length)),
                    Extent=rep.int(df1$Extent, sapply(List, length)),
                    Couche=rep.int(df1$Couche, sapply(List, length)),
                    Dossier=unlist(List),
                    stringsAsFactors=F) %>%
    group_by(Source) %>%
    mutate(Niveau=1:length(Source)) %>%
    ungroup()

  # Reproduction et sauvegarde de la hiérarchie entre les fichiers/dossiers dans un data.frame (df3)
  Niveaux <- unique(df2$Niveau)
  df3 <- mutate(df2, DirName=Source) %>%
    group_by(Source) %>%
    mutate(Niveau_Max=max(Niveau)) %>%
    ungroup()
  for (niv in Niveaux[length(Niveaux):1]) {
    df3 <- mutate(df3,
                  DirName=ifelse(niv <= Niveau_Max,
                                 dirname(DirName),
                                 DirName),
                  Dossier=ifelse(Niveau==niv,
                                 DirName,
                                 Dossier))
  }
  # Création des dossiers nécessaires dans le dossier DataProjet:
  repDataProjet <- dirname(rep_SHP) # remonter dans les dossiers pour créer DataProjet
  repDataProjet <- paste0(repDataProjet,"/DataProjet")
  dir.create(repDataProjet,
             showWarnings=F)

  # ---- N.B : Création des dossiers est reportée dans la fonction ReecritureShape (inutile de
  # -- créer des dossiers pour des shapes qui ne seront pas réécris)

  # setwd(repDataProjet)
  # for (niv in Niveaux) {
  #   Dirs <- filter(df3, Niveau==niv) %>%
  #     dplyr::select(Dossier) %>%
  #     distinct() %>% # idem unique mais plus rapide
  #     unlist()
  #   for (dir in Dirs) {
  #     dir.create(dir, showWarnings=F)
  #   }
  # }

  infos <- list(rep_Ilots, rep_SHP, df3)
  names(infos) <- c("repTravail","repDataBrutes","tableau")
  return(infos)
}
