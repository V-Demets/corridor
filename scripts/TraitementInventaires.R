#' Traitement des données d'inventaire.
#'
#' @description Traitement des données d'inventaires par placettes permanentes (PSDRF et GF).
#'
#' @return La fonction exporte les résultats d'inventaire sous la forme de shapes par placettes.
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
#' @import PermGF
#' @import PermPSDRF
#' @import knitr
#'
#' @export

TraitementInventaires <- function() {

# Choix du dossier contenant les fichiers d'inventaire
  rep_Data <- tk_choose.dir(
    default = "Data/Excel/Inventaires", #getwd(), 
    caption = "Choix du répertoire contenant les données vecteurs"
  )

# ----- Traitement des données d'inventaire PSDRF -----
# définir les tables à créer pour les besoins du krigeage -> tBase_GF, tBase_PSDRF
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
psdrf_Verif(rep_Ilots, rep_InvPSDRF, "pdf")

# --- Calcul des variables par arbres
psdrf_Calculs(rep_Ilots)

# # --- Agrégation des résultats par placettes
# # -- Table TabCombi :
# tBase <- c("psdrfDispBM_", "psdrfDispBM_Essence", "psdrfDispBM_EssenceClasse", 
#            "psdrfDispBM_StadeD", "psdrfDispBM_StadeDStadeE", "psdrfDispBM_StadeE", 
#            "psdrfDispBMP_Classe", "psdrfDispBMP_ClasseType", "psdrfDispBMS_Classe", 
#            "psdrfDispCodes_", "psdrfDispCodes_CodeEcolo", "psdrfDispCodes_CatCodeEcolo", 
#            "psdrfDispDen_", "psdrfDispDen_Essence", "psdrfDispFpied_", 
#            "psdrfDispFpied_Cat", "psdrfDispFpied_Classe", "psdrfDispFpied_Essence", 
#            "psdrfDispFpied_EssenceCat", "psdrfDispFpied_EssRegParCat", 
#            "psdrfDispPer_", "psdrfDispPer_Essence", "psdrfDispPer_EssReg", 
#            "psdrfDispHabitatBM_", "psdrfDispHabitatBM_StadeD", "psdrfDispHabitatBMP_", 
#            "psdrfDispHabitatBMS_", "psdrfDispHabitatFpied_", 
#            "psdrfDispHabitatFpied_Classe", "psdrfDispHabitatTaillis_", 
#            "psdrfDispHabitatTaillis_Classe", "psdrfDispRege_Essence", 
#            "psdrfDispRege_EssRegPar", "psdrfDispTaillis_", "psdrfDispTaillis_Classe", 
#            "psdrfDispTaillis_Essence", "psdrfDispTot_", "psdrfDispTot_Cat", 
#            "psdrfDispTot_CatCodeEcolo", "psdrfDispTot_Essence", "psdrfDispTot_Classe", 
#            "psdrfDispTot_Coupe", "psdrfDispTot_ClasseCoupe", "psdrfDispTot_CatCoupe", 
#            "psdrfDispTot_EssenceClasse", "psdrfDispTot_EssRegParCat", "psdrfPlaFpied_", 
#            "psdrfPlaFpied_Cat ", " psdrfPlaTaillis_Cat", "psdrfPlaTaillis_", 
#            "psdrfPlaBM_", "psdrfPlaRege_", "psdrfPlaTot_", "psdrfPlaTot_EssReg", "psdrfPlaTot_Cat", 
#            "psdrfDispPFutaie_", "psdrfDispPFutaie_Essence", "psdrfDispPFutaie_Classe", "psdrfDispPFutaie_Cat", 
#            "psdrfDispExploit_", "psdrfDispExploit_Essence", "psdrfDispExploit_Classe", "psdrfDispExploit_Cat")
#
# tBaseCarnet <- data.frame(var = tBase, 
#                           stringsAsFactors = F)
# tBaseCarnet <- dplyr::mutate(tBaseCarnet, 
#                              Var1 = ifelse(str_detect(var, "Essence"), 
#                                          "Essence", NA), 
#                              Var1 = ifelse(str_detect(var, "EssReg"), 
#                                          "EssReg", Var1), 
#                              Var1 = ifelse(str_detect(var, "EssRegPar"), 
#                                          "EssRegPar", Var1), 
#                              Var2 = ifelse(str_detect(var, "Classe"), 
#                                          "Classe", NA), 
#                              Var2 = ifelse(str_detect(var, "Cat"), 
#                                          "Cat", Var2), 
#                              Var3 = ifelse(str_detect(var, "CodeEcolo"), 
#                                          "CodeEcolo", NA), 
#                              Var4 = ifelse(str_detect(var, "Coupe"), 
#                                          "Coupe", NA), 
#                              Var5 = ifelse(str_detect(var, "StadeD"), 
#                                          "StadeD", NA), 
#                              Var6 = ifelse(str_detect(var, "StadeE"), 
#                                          "StadeE", NA), 
#                              Var7 = ifelse(str_detect(var, "Type"), 
#                                          "Type", NA), 
#                              Data = ifelse(str_detect(var, "BM"), 
#                                          "BM", NA), 
#                              Data = ifelse(str_detect(var, "BMP"), 
#                                          "BMP", Data), 
#                              Data = ifelse(str_detect(var, "BMS"), 
#                                          "BMS", Data), 
#                              Data = ifelse(str_detect(var, "Tot"), 
#                                          "Tot", Data), 
#                              Data = ifelse(str_detect(var, "Fpied"), 
#                                          "Fpied", Data), 
#                              Data = ifelse(str_detect(var, "Per"), 
#                                          "Per", Data), 
#                              Data = ifelse(str_detect(var, "Taillis"), 
#                                          "Taillis", Data), 
#                              Data = ifelse(str_detect(var, "Den"), 
#                                          "Den", Data), 
#                              Data = ifelse(str_detect(var, "PFutaie"), 
#                                          "PFutaie", Data), 
#                              Data = ifelse(str_detect(var, "Exploit"), 
#                                          "Exploit", Data), 
#                              Data = ifelse(str_detect(var, "Codes"), 
#                                          "Codes", Data), 
#                              Data = ifelse(str_detect(var, "Rege"), 
#                                          "Rege", Data), 
#                              var = NULL)
# TabCombi <- tBaseCarnet

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
  caption = "Choix du répertoire contenant les classeurs d'inventaire PSDRF.", 
  filter = matrix(c(".xlsx", ".xlsx"), 1, 2)
)
# --- Import des classeurs d'inventaire :
gf_Xls2Rdata(rep_Ilots, ListFile_InvGF)

# --- Calcul des variables par arbres
gf_Calculs(rep_Ilots)

# # --- Agrégation des résultats par placettes
# # -- Table TabCombi :
# tBase <- c("gfForetBMP_", "gfForetBMP_Classe", "gfForetBMP_ClasseStadeD", 
#            "gfForetBMP_StadeD", "gfForetBMP_StadeE", 
#            "gfForetBMP_ClasseType", 
#            "gfForetBMP_StadeDStadeE", "gfForetBMS_", "gfForetBMS_Classe", 
#            "gfForetBMS_StadeDStadeE", 
#            "gfForetBMS_ClasseStadeD", "gfForetBMS_StadeD", "gfForetBMS_StadeE", 
#
#            "gfForetCodes_CatCodeEcolo", "gfForetCodes_CodeEcolo", 
#
#            "gfForetDen_", "gfForetDen_EssReg", "gfForetDen_EssRegClasse", 
#            "gfForetDen_Cat", "gfForetDen_Reg1", "gfForetDen_CatReg1", 
#            "gfForetDen_Essence", "gfForetPer_Essence", "gfForetDen_EssenceCat", 
#
#            "gfForetFpied_CatCodeEcolo", "gfForetFpied_", "gfForetFpied_Cat", 
#            "gfForetFpied_CatReg1", "gfForetFpied_Classe", "gfForetFpied_ClasseQual", "gfForetFpied_ClasseReg1", 
#            "gfForetFpied_Essence", "gfForetFpied_EssenceCat", "gfForetFpied_EssRegCat", 
#            "gfForetFpied_Reg2", "gfForetPer_Classe", "gfForetFpied_ClasseCodeEcolo", 
#
#            "gfForetPer_ClasseReg1", "gfForetPer_EssRegClasse", "gfPPTetrasPer_EssReg", 
#            "gfForetPer_", 
#            "gfForetRege_Essence", "gfForetRege_EssReg", "gfForetRege_", 
#            "gfForetTaillis_", "gfForetTaillis_Essence", 
#            "gfForetTaillis_EssReg", "gfForetTaillis_Classe", "gfForetTaillis_EssRegClasse", 
#
#            "gfPlaBMP_", "gfPlaBMS_", "gfPlaDen_", "gfPlaFpied_", "gfPlaFpied_EssReg", 
#            "gfPlaPerches_EssRegClasseReg1", "gfPlaDen_Cat", 
#            "gfPlaRege_EssReg", "gfPlaTaillis_", "gfPlaTaillis_EssReg", 
#
#            "gfPlaFpied_EssenceCatRep\u00E9r\u00E9", "gfPlaBMP_EssenceCat", 
#            "gfForetRege_Essence", 
#            "gfForetDen_EssenceReg1", "gfForetDen_EssenceCatReg1", 
#            "gfForetFpied_EssenceCatRep\u00E9r\u00E9PerchoirAbri", 
#            "gfForetFpied_EssenceCatPerchoirAbri", 
#            "gfForetFpied_EssenceClassePerchoirAbri", 
#            "gfForetFpied_EssenceClasseReg1", "gfForetFpied_Reg1", 
#            "gfForetFpied_EssRegReg1", "gfForetFpied_EssenceReg1")
#
# tBaseCarnet <- data.frame(var = tBase, 
#                           stringsAsFactors = F)
# tBaseCarnet <- mutate(tBaseCarnet, 
#                       Var1 = ifelse(str_detect(var, "Essence"), 
#                                   "Essence", NA), 
#                       Var1 = ifelse(str_detect(var, "EssReg"), 
#                                   "EssReg", Var1), 
#                       Var1 = ifelse(str_detect(var, "EssRegInd"), 
#                                   "EssRegPar", Var1), 
#                       Var2 = ifelse(str_detect(var, "Classe"), 
#                                   "Classe", NA), 
#                       Var2 = ifelse(str_detect(var, "Cat"), 
#                                   "Cat", Var2), 
#                       Var3 = ifelse(str_detect(var, "StadeD"), 
#                                   "StadeD", NA), 
#                       Var4 = ifelse(str_detect(var, "StadeE"), 
#                                   "StadeE", NA), 
#                       Var5 = ifelse(str_detect(var, "Qual"), 
#                                   "Qual", NA), 
#                       Var5 = ifelse(str_detect(var, "Reg1"), 
#                                   "Reg1", Var5), 
#                       Var5 = ifelse(str_detect(var, "Reg2"), 
#                                   "Reg2", Var5), 
#                       Var6 = ifelse(str_detect(var, "Martel\u00E9"), 
#                                   "Martel\u00E9", NA), 
#                       Var7 = ifelse(str_detect(var, "Rep\u00E9r\u00E9"), 
#                                   "Rep\u00E9r\u00E9", NA), 
#                       Var8 = ifelse(str_detect(var, "Perchoir"), 
#                                   "Perchoir", NA), 
#                       Var9 = ifelse(str_detect(var, "Abri"), 
#                                   "Abri", NA), 
#                       Var10 = ifelse(str_detect(var, "Type"), 
#                                    "Type", NA), 
#                       Var10 = ifelse(str_detect(var, "CodeEcolo"), 
#                                    "CodeEcolo", Var10), 
#                       Var11 = ifelse(str_detect(var, "Coupe"), 
#                                    "Coupe", NA), 
#                       Data = ifelse(str_detect(var, "BM"), 
#                                   "BM", NA), 
#                       Data = ifelse(str_detect(var, "BMP"), 
#                                   "BMP", Data), 
#                       Data = ifelse(str_detect(var, "BMS"), 
#                                   "BMS", Data), 
#                       Data = ifelse(str_detect(var, "Tot"), 
#                                   "Tot", Data), 
#                       Data = ifelse(str_detect(var, "Fpied"), 
#                                   "Fpied", Data), 
#                       Data = ifelse(is.na(Data), 
#                                   ifelse(str_detect(var, "Per"), 
#                                          "Per", Data), Data), 
#                       Data = ifelse(str_detect(var, "Taillis"), 
#                                   "Taillis", Data), 
#                       Data = ifelse(str_detect(var, "Den"), 
#                                   "Den", Data), 
#                       Data = ifelse(str_detect(var, "Codes"), 
#                                   "Codes", Data), 
#                       Data = ifelse(str_detect(var, "Rege"), 
#                                   "Rege", Data), 
#                       var = NULL)
# # TabCombi <- rbind(TabCombi, tBaseCarnet)
# TabCombi_GF <- tBaseCarnet

gf_AgregArbres(rep_Ilots, TabCombi_GF)
load("Tables/gfDonneesBrutes.RData")
load("Tables/gfTablesBrutes.RData")
load("Tables/gfTablesElaboreesPlacTest.RData")
# for(i in 1:length(TabPla)) {assign(names(TabPla)[i], TabPla[[i]])}
# -- Table résultats par placettes - Fpied :
assign(names(TabPla)[39], TabPla[[39]])

# -- Agrégations par ensemble (échelle forêt)
gf_AgregPlacettes(rep_Ilots)
load("Tables/gfTablesElaborees.RData")
assign(names(Tableaux)[39], Tableaux[[39]])

# --- Ecriture des résultats sous la forme de shape :
gf_ShapesPlac(rep_Ilots)
}



