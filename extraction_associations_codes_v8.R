###############################################################################
# extraction_associations_codes_v8.R
#
# Fusion de extraction_associations_codes_v7.1.2.R (chirurgie ambulatoire,
# séjours courts) et extraction_associations_codes_v7.2.R (séjours longs).
# Spécification : SPEC_V8.md. Journal des écarts : MODIFICATIONS_V8.md.
#
# RÈGLE D'OR (SPEC §0) : toutes les chaînes dbplyr (de pRatihque::atihble() à
# compute()/collect()) sont des copies de v7.1.2 / v7.2. Chaque caractère
# modifié est tracé dans MODIFICATIONS_V8.md avec la référence §5 ou config
# qui l'autorise. Ne pas « améliorer » ces chaînes.
#
# Point d'entrée unique. Sections :
#   0. Config            5. Branche chirurgie ambulatoire
#   1. Sources/connexion 6. Branche séjours courts
#   2. prep_data(an)     7. Branche séjours longs
#   3. Tables de réf.    8. Exports
#   4. Helpers purs      9. Rapport de contrôle
###############################################################################

## ---- 0. Config ----
# Toutes les constantes paramétrables. Aucun nombre magique plus bas.

PATH_PROJET <- Sys.getenv("SCENARIOS_PMSI_PATH",
                          unset = "~/commun/projets_communs/DIM_siege/divers_projets/Scenario_crh_fictifs/")
if(!grepl("/$", PATH_PROJET)) PATH_PROJET <- paste0(PATH_PROJET, "/")
PATH_RESULTS        <- paste0(PATH_PROJET, "results/")
PATH_PAIRES_EXCLUES <- paste0(PATH_PROJET, "referentiels/exclusions_paires.yaml")

AN_REF         <- 26L               # année de référence (tables de référence, courts, chir ambu)
ANS_HISTORIQUE <- 17:26             # années agrégées pour le catalogue des séjours longs
SEED           <- 20260907

SEUIL_PIVOT        <- 10            # divulgation : nb > SEUIL_PIVOT au niveau des pivots (§2.2)
SEUIL_REF_DAS      <- 20            # effectif min de codes candidats d'une strate de référence (§6.2)
SEUIL_REF_IMPRECIS <- 20            # export §7.5 : nb >= seuil
SEUIL_REF_PAIRES   <- 50            # export §7.6 : nb >= seuil

DUREE_COURTS    <- 0:2              # séjours courts : durée < 3
DUREE_LONGS     <- 3:100            # séjours longs
DUREE_MIN_REF   <- 3                # tables de référence DAS : séjours de durée > DUREE_MIN_REF (v7.2 l.445, §6.2)
NBDA_MAX        <- 25               # nbda %in% 1:NBDA_MAX pour les séjours longs
K_GRAINE_LONGS  <- 2                # nb de DAS réels en graine (§2.1)

NB_TIRAGES_COURTS         <- 3      # nb de variantes de DAS par pivot (séjours courts, ex-boucle nb_min:nb_max)
NB_TIRAGES_LONGS          <- 1      # nb de complétions par combinaison (v7.2 : 1)
NB_VARIANTES_ADMIN_COURTS <- 2      # variantes d'habillage admin par scénario court (ex slice(1:2))
NB_VARIANTES_ADMIN_LONGS  <- NA     # NA = toutes les variantes (comportement v7.2)
MAX_SCENARIOS_LONGS       <- NA     # NA = tirage sur tout le catalogue ; sinon slice_sample(weight_by = poids)
AGE_MAX_OUVERT            <- 95     # borne haute de la classe ouverte "[80-[" pour le tirage d'âge

# Cible dégradée du nombre de DAS chroniques par classe d'âge (bornes incluses),
# utilisée quand la strate (cage, sexe) de ref_nb_chroniques est vide (§6.2).
CIBLES_NB_CHRONIQUES <- list(
  "[0-1["   = c(0, 0), "[1-5["   = c(0, 0), "[5-10["  = c(0, 0),
  "[10-15[" = c(0, 0), "[15-18[" = c(0, 0),
  "[18-30[" = c(0, 1), "[30-40[" = c(0, 1),
  "[40-50[" = c(1, 2), "[50-60[" = c(1, 2),
  "[60-70[" = c(2, 3),
  "[70-80[" = c(3, 5), "[80-["   = c(3, 5)
)

# Établissements
TYPES_ETBS_LONGS      <- c("CHR/U", "CH")   # ordre d'agrégation v7.2 : CHR/U puis CH
TYPE_ETBS_REF_DIABETE <- "CHR/U"            # v7.1.2 l.196 / v7.2 l.457

# GHM — listes obstétriques v7.2 l.518-524 : reprises telles quelles. Non utilisées dans le
# tirage v8 (le filtre de test nb>5000 / sample_n(3000) disparaît, §2.5) ; conservées
# pour l'allocation en aval.
GHM_ACC_NORMAL    <- c("14C03A", "14C07A", "14C08A", "14Z11A", "14Z12A",
                       "14Z13A", "14Z13T", "14Z14A", "14Z14T")
RACINES_ACC_PATHO <- c("14C07", "14C08", "14Z10", "14Z11", "14Z12", "14Z13", "14Z14")
GHM_BB_NORMAL     <- c("15M05A", "15M06A", "15M07A", "15M08A", "15M09A",
                       "15M10A", "15M11A", "15M13A", "15M14A")
RACINES_BB_MED    <- c("15M05", "15M06", "15M07", "15M08", "15M09",
                       "15M10", "15M11", "15M13", "15M14")

# Types d'autorisation d'unité (v7.2 l.20-22 ; les définitions l.16-19 étaient écrasées)
TYPEAUT_UHCD <- c("07A", "07B")
TYPEAUT_SC   <- c("01A", "01B", "13A", "13B", "03A", "03B")
TYPEAUT_USI  <- c("02E", "02A")             # non utilisé (repris de v7.2)

# Priorité des unités pour réduire prep_data à UNE ligne par séjour (écart B1-10, Q5 résolue).
# Documente l'ordre codé en littéral dans le case_when de prep_data (chaîne dbplyr).
# UHCD impérativement dernier : sinon un séjour multi-RUM passé par l'UHCD serait réduit à sa
# ligne UHCD puis supprimé par le filtre (nbrum == 1 & type_unite == "UHCD") | type_unite != "UHCD".
PRIORITE_TYPE_UNITE <- c("SC" = 1L, "SC-NEONAT" = 2L, "NEONAT" = 3L, "GERIATRIE" = 4L,
                         "HC" = 5L, "HP" = 6L, "UHCD" = 7L)

# Pénalisation des effectifs des codes diabète .9 (v7.1.2 l.208-213)
CAGE_PED          <- c("[1-5[", "[10-15[", "[5-10[", "[0-1[")
CAGE_AGES         <- c("[50-60[", "[60-70[", "[70-80[", "[80-[")
PENALITE_9_AGES   <- 0.2
PENALITE_9_AUTRES <- 0.5

# Pivots par branche
PIVOTS_COURTS      <- c("mode_hospit", "sexe", "cage", "ghm2", "diag2", "duree")        # v7.1.2 l.217
PIVOTS_LONGS       <- c("mode_hospit", "sexe", "age", "cage", "racine", "ghm2", "diabete", "hta",
                        "diag2", "nbda", "type_unite", "prep_sc")                       # v7.2 l.483 + §2.8
PIVOTS_LONGS_SEUIL <- setdiff(PIVOTS_LONGS, "nbda")                                     # v7.2 l.528-529
COLS_ADMIN         <- c("mode_entree", "mode_sortie", "mdp")                            # colonnes d'habillage

# Export §7.5 : motif de repérage des libellés « sans précision »
MOTIF_IMPRECIS <- "sans précision|non précisé"

# Millésime de la table des niveaux de CMA (mco_diag_niveau, colonnes v20xx) selon
# l'année de données (v7.2 l.280-282). Utilisé partout à la place de v2025 (§5.8).
anseqta_de <- function(an){
  dplyr::case_when(an <= 17 ~ "21",
                   an > 17 & an <= 22 ~ "23",
                   TRUE ~ "25")
}
ANSEQTA_REF <- anseqta_de(AN_REF)

DATE_TAG <- format(Sys.Date(), "%Y%m%d")

set.seed(SEED)

## ---- 1. Sources et connexion ----
source(paste0(PATH_PROJET, "utils.R"))
path_projet <- PATH_PROJET     # alias attendu par referentiels.R (et write_xlsx de utils.R)
outfile     <- PATH_RESULTS    # alias historique

conn <- pRatihque::connection_database()

source(paste0(PATH_PROJET, "exclusions.R"))
source(paste0(PATH_PROJET, "referentiels.R"))   # définit neo_codes_diabete, codes_diab, hta_autres, cim, ...

# Paires de préfixes exclues (§6.4) : liste de vecteurs c(prefixeA, prefixeB)
PAIRES_EXCLUES <- list()
if(file.exists(PATH_PAIRES_EXCLUES)){
  PAIRES_EXCLUES <- lapply(yaml::read_yaml(PATH_PAIRES_EXCLUES), as.character)
} else {
  warning("Fichier absent : " %+% PATH_PAIRES_EXCLUES %+% " ; aucune paire exclue.")
}

## ---- 2. prep_data(an) ----
# Source : v7.2 l.24-275 (version riche : type_unite, prep_sc, flags diabete/hta,
# branches an<=17 / 18-22 / >22). Écarts tracés dans MODIFICATIONS_V8.md (bloc B1) :
#   §5.9a : distinct(.keep_all=TRUE) -> règle d'ordre explicite (window row_number)
#   B1-10 : grain réduit à une ligne par séjour (ident), unité la plus prioritaire
#           (PRIORITE_TYPE_UNITE), prep_sc = max par séjour.
#   §3.2/§6.1 : ajout de raac (v7.1.2 l.30) et de cage2 (règle v7.1.2 l.68), initialement
#               pour la branche chirurgie ambulatoire ; branche supprimée, colonnes inertes
#               conservées (pas de réédition de la chaîne, cf. MODIFICATIONS_V8.md).
prep_data<-function(an){
  
  if(an>22){
      
    pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.um') |>
      dplyr::filter(!is.na(finessgeo)) |> 
        dplyr::mutate(mode_hospit = dplyr::case_when(type_hospum_1 == "P" ~"HP",
                                                     TRUE ~ "HC"),
                      type_unite = dplyr::case_when(type_rum_1%in%TYPEAUT_SC~"SC",
                                                    type_rum_1=="06"~"SC-NEONAT",
                                                    type_rum_1=="04"~"NEONAT",
                                                    type_rum_1=="27"~"GERIATRIE",
                                                    type_rum_1%in%TYPEAUT_UHCD~"UHCD",
                                                    type_hospum_1 == "P" ~"HP",
                                                    TRUE ~ "HC"),
                      prep_sc= ifelse(type_rum_1 %in%c(TYPEAUT_SC,"06"),1,0)) |> 
      #Ajouter soins critiques adu - ped - neonat / gériatrie
        dplyr::select(ident,finessgeo,mode_hospit,type_unite,prep_sc) |> 
        dplyr::group_by(ident,type_unite) |> dplyr::filter(dplyr::row_number(mode_hospit) == 1L) |> dplyr::ungroup() |>   # §5.9a (ex distinct(ident,type_unite,.keep_all=TRUE))
        dplyr::mutate(prep_sc = max(prep_sc), .by = ident) |>   # écart B1-10, une ligne par séjour, SC prioritaire
        dplyr::mutate(rang_unite = dplyr::case_when(
          type_unite == "SC" ~ 1L, type_unite == "SC-NEONAT" ~ 2L,
          type_unite == "NEONAT" ~ 3L, type_unite == "GERIATRIE" ~ 4L,
          type_unite == "HC" ~ 5L, type_unite == "HP" ~ 6L, TRUE ~ 7L)) |>   # écart B1-10 (ordre : PRIORITE_TYPE_UNITE)
        dplyr::group_by(ident) |>
        dplyr::filter(dplyr::row_number(rang_unite) == 1L) |>   # écart B1-10
        dplyr::ungroup() |> dplyr::select(-rang_unite) |>   # écart B1-10
        dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                           dplyr::distinct(finessgeo,categ_pmsi)) |> 

        dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.fixe') |>
                            dplyr::select(anonyme,ident,dp,dr,age,sexe,provenance,modesortie,
                                          destination,duree,rumdudp,nbda,ghm2,passage_urg,nbrum,raac)  |>   # + raac (§6.1, v7.1.2 l.30)
                            dplyr::group_by(anonyme,ghm2) |> dplyr::filter(dplyr::row_number(ident) == 1L) |> dplyr::ungroup() |>   # §5.9a (ex distinct(anonyme,ghm2,.keep_all=TRUE))
                            dplyr::mutate(
                              
                              mode_entree =dplyr::case_when(passage_urg %in% c("5","U","V") ~  "URGENCES" ,
                                                            TRUE  ~ "DOMICILE"),
                              mode_sortie = dplyr::case_when(
                                modesortie %in% ("9")   ~ "DECES",
                                destination %in% ("2")  ~ "SMR",
                                TRUE ~ "DOMICILE"),
                              
                              diag2 = dplyr::case_when( is.na(dr) ~ dp,
                                                       substr(dp,1,1)=="Z" ~ dr,
                                                       TRUE ~ dp ) ,
                              
                              mdp = dplyr::case_when( is.na(dr) ~ "DP",
                                                      substr(dp,1,1)=="Z" ~ dp,
                                                      TRUE ~ "DP" ) ,
                              age = ifelse(is.na(age),0,age),
                              cage = dplyr::case_when(
                                age < 1 ~ "[0-1[",
                                age < 5 ~ "[1-5[",
                                age < 10 ~ "[5-10[",
                                age < 15 ~ "[10-15[",
                                age < 18 ~ "[15-18[",
                                age < 30 ~ "[18-30[",
                                age < 40 ~ "[30-40[",
                                age < 50 ~ "[40-50[",
                                age < 60 ~ "[50-60[",
                                age < 70 ~ "[60-70[",
                                age < 80 ~ "[70-80[",
                                TRUE ~  "[80-["
                              ),
                              cage3 = ifelse(age>=18,"ge_18","lt_18"),
                              racine =substr(ghm2,1,5)
                            ) 
        ) |> 
      dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                         dplyr::filter(typ_diag==5,diag%in%c(code_dnid_ins,code_dnid,code_did)) |> 
                         dplyr::mutate(diabete = dplyr::case_when(diag %in% code_dnid_ins ~ "E11i",
                                                                  diag %in% code_dnid ~ "E11ni",
                                                                  diag %in%code_did  ~ "E10",
                                                                  TRUE~NA)) |> 
                         dplyr::filter(!is.na(diabete)) |> 
                         dplyr::select(ident,diabete) |> 
                         dplyr::group_by(ident) |> dplyr::filter(dplyr::row_number(diabete) == 1L) |> dplyr::ungroup())  |>   # §5.9a (ex distinct(ident,.keep_all=TRUE))
      dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                         dplyr::filter(typ_diag==5,diag%in%c("I10")) |> 
                         dplyr::mutate(hta = "I10") |>
                         dplyr::select(ident,hta) |> 
                         dplyr::distinct(ident,.keep_all = TRUE) )-> query   # déterministe : hta constant ("I10"), §5.9a sans objet
    
 
  }
  
  if(an>17 & an<=22){
    pRatihque::atihble(conn,"PRD_VUE_MCOBL_20" %+% an %+%  '.um') |>
      dplyr::filter(!is.na(finessgeo)) |> 
      dplyr::mutate(mode_hospit = dplyr::case_when(type_hospum_1 == "P" ~"HP",
                                                   TRUE ~ "HC"),
                    type_unite = dplyr::case_when(type_rum_1%in%TYPEAUT_SC~"SC",
                                                  type_rum_1=="06"~"SC-NEONAT",
                                                  type_rum_1=="04"~"NEONAT",
                                                  type_rum_1=="27"~"GERIATRIE",
                                                  type_rum_1%in%TYPEAUT_UHCD~"UHCD",
                                                  type_hospum_1 == "P" ~"HP",
                                                  TRUE ~ "HC"),
                    prep_sc= ifelse(type_rum_1 %in%c(TYPEAUT_SC,"06"),1,0)) |> 
      dplyr::select(ident,finessgeo,mode_hospit,type_unite,prep_sc) |> 
      dplyr::group_by(ident,type_unite) |> dplyr::filter(dplyr::row_number(mode_hospit) == 1L) |> dplyr::ungroup() |>   # §5.9a (ex distinct(ident,type_unite,.keep_all=TRUE))
      dplyr::mutate(prep_sc = max(prep_sc), .by = ident) |>   # écart B1-10, une ligne par séjour, SC prioritaire
      dplyr::mutate(rang_unite = dplyr::case_when(
        type_unite == "SC" ~ 1L, type_unite == "SC-NEONAT" ~ 2L,
        type_unite == "NEONAT" ~ 3L, type_unite == "GERIATRIE" ~ 4L,
        type_unite == "HC" ~ 5L, type_unite == "HP" ~ 6L, TRUE ~ 7L)) |>   # écart B1-10 (ordre : PRIORITE_TYPE_UNITE)
      dplyr::group_by(ident) |>
      dplyr::filter(dplyr::row_number(rang_unite) == 1L) |>   # écart B1-10
      dplyr::ungroup() |> dplyr::select(-rang_unite) |>   # écart B1-10
      dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                         dplyr::distinct(finessgeo,categ_pmsi)) |> 
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.fixe') |>
                          dplyr::filter(substr(ghm2,1,2)!="90") |> 
                          dplyr::group_by(anonyme,ghm2) |> dplyr::filter(dplyr::row_number(ident) == 1L) |> dplyr::ungroup() |>   # §5.9a (ex distinct(anonyme,ghm2,.keep_all=TRUE))
                          dplyr::select(anonyme,ident,dp,dr,age,sexe,provenance,modesortie,
                                        destination,duree,rumdudp,nbda,nbrum)  |> 
                          
                          dplyr::mutate(
                            mode_entree =dplyr::case_when(provenance =="5"  ~  "URGENCES" ,
                                                          TRUE  ~ "DOMICILE"),
                            mode_sortie = dplyr::case_when(
                              modesortie %in% ("9")   ~ "DECES",
                              destination %in% ("2")  ~ "SMR",
                              TRUE ~ "DOMICILE"),
                            
                            diag2 = dplyr::case_when( is.na(dr) ~ dp,
                                                      substr(dp,1,1)=="Z" ~ dr,
                                                      TRUE ~ dp ) ,
                            
                            mdp = dplyr::case_when( is.na(dr) ~ "DP",
                                                    substr(dp,1,1)=="Z" ~ dp,
                                                    TRUE ~ "DP" ) ,
                            age = ifelse(is.na(age),0,age),
                            cage = dplyr::case_when(
                              age < 1 ~ "[0-1[",
                              age < 5 ~ "[1-5[",
                              age < 10 ~ "[5-10[",
                              age < 15 ~ "[10-15[",
                              age < 18 ~ "[15-18[",
                              age < 30 ~ "[18-30[",
                              age < 40 ~ "[30-40[",
                              age < 50 ~ "[40-50[",
                              age < 60 ~ "[50-60[",
                              age < 70 ~ "[60-70[",
                              age < 80 ~ "[70-80[",
                              TRUE ~  "[80-["
                            ),
                            cage3 = ifelse(age>=18,"ge_18","lt_18"),
                            
                          ) 
      )|> 
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.rgp') |>
                          dplyr::distinct(ident,ghmv2023) |> 
                          dplyr::rename(ghm2=ghmv2023) |> 
                          dplyr::mutate(racine =substr(ghm2,1,5))) |> 
      dplyr::mutate(raac = NA) |>   # raac absent des millésimes <= 22 (colonne inerte depuis la suppression de la branche chir ambu)
      dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                         dplyr::filter(typ_diag==5,diag%in%c(code_dnid_ins,code_dnid,code_did)) |> 
                         dplyr::mutate(diabete = dplyr::case_when(diag %in% code_dnid_ins ~ "E11i",
                                                                  diag %in% code_dnid ~ "E11ni",
                                                                  diag %in%code_did  ~ "E10",
                                                                  TRUE~NA)) |> 
                         dplyr::filter(!is.na(diabete)) |> 
                         dplyr::select(ident,diabete) |> 
                         dplyr::group_by(ident) |> dplyr::filter(dplyr::row_number(diabete) == 1L) |> dplyr::ungroup())  |>   # §5.9a (ex distinct(ident,.keep_all=TRUE))
      dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                         dplyr::filter(typ_diag==5,diag%in%c("I10")) |> 
                         dplyr::mutate(hta = "I10") |>
                         dplyr::select(ident,hta) |> 
                         dplyr::distinct(ident,.keep_all = TRUE) )-> query   # déterministe : hta constant ("I10"), §5.9a sans objet
    
  }
  
  if(an<=17){
    
    pRatihque::atihble(conn,"PRD_VUE_MCOBL_20" %+% an %+%  '.um') |>
      dplyr::filter(!is.na(finessgeo)) |> 
      dplyr::mutate(mode_hospit = dplyr::case_when(type_hospum_1 == "P" ~"HP",
                                                   TRUE ~ "HC"),
                    type_unite = dplyr::case_when(type_rum_1%in%TYPEAUT_SC~"SC",
                                                  type_rum_1=="06"~"SC-NEONAT",
                                                  type_rum_1=="04"~"NEONAT",
                                                  type_rum_1=="27"~"GERIATRIE",
                                                  type_rum_1%in%TYPEAUT_UHCD~"UHCD",
                                                  type_hospum_1 == "P" ~"HP",
                                                  TRUE ~ "HC"),
                    prep_sc= ifelse(type_rum_1 %in%c(TYPEAUT_SC,"06"),1,0)) |> 
      dplyr::select(ident,finessgeo,mode_hospit,type_unite,prep_sc) |> 
      dplyr::group_by(ident,type_unite) |> dplyr::filter(dplyr::row_number(mode_hospit) == 1L) |> dplyr::ungroup() |>   # §5.9a (ex distinct(ident,type_unite,.keep_all=TRUE))
      dplyr::mutate(prep_sc = max(prep_sc), .by = ident) |>   # écart B1-10, une ligne par séjour, SC prioritaire
      dplyr::mutate(rang_unite = dplyr::case_when(
        type_unite == "SC" ~ 1L, type_unite == "SC-NEONAT" ~ 2L,
        type_unite == "NEONAT" ~ 3L, type_unite == "GERIATRIE" ~ 4L,
        type_unite == "HC" ~ 5L, type_unite == "HP" ~ 6L, TRUE ~ 7L)) |>   # écart B1-10 (ordre : PRIORITE_TYPE_UNITE)
      dplyr::group_by(ident) |>
      dplyr::filter(dplyr::row_number(rang_unite) == 1L) |>   # écart B1-10
      dplyr::ungroup() |> dplyr::select(-rang_unite) |>   # écart B1-10
      dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                         dplyr::distinct(finessgeo,categ_pmsi)) |> 
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.fixe') |>
                          dplyr::filter(substr(ghm2,1,2)!="90") |> 
                          dplyr::group_by(anonyme,ghm2) |> dplyr::filter(dplyr::row_number(ident) == 1L) |> dplyr::ungroup() |>   # §5.9a (ex distinct(anonyme,ghm2,.keep_all=TRUE))
                          dplyr::select(anonyme,ident,dp,dr,age,sexe,provenance,modesortie,
                                        destination,duree,rumdudp,nbda,nbrum)  |> 
                         
                          dplyr::mutate(
                            mode_entree =dplyr::case_when(provenance =="5"  ~  "URGENCES" ,
                                                          TRUE  ~ "DOMICILE"),
                            mode_sortie = dplyr::case_when(
                              modesortie %in% ("9")   ~ "DECES",
                              destination %in% ("2")  ~ "SMR",
                              TRUE ~ "DOMICILE"),
                            
                            diag2 = dplyr::case_when( is.na(dr) ~ dp,
                                                      substr(dp,1,1)=="Z" ~ dr,
                                                      TRUE ~ dp ) ,
                            
                            mdp = dplyr::case_when( is.na(dr) ~ "DP",
                                                    substr(dp,1,1)=="Z" ~ dp,
                                                    TRUE ~ "DP" ) ,
                            age = ifelse(is.na(age),0,age),
                            cage = dplyr::case_when(
                              age < 1 ~ "[0-1[",
                              age < 5 ~ "[1-5[",
                              age < 10 ~ "[5-10[",
                              age < 15 ~ "[10-15[",
                              age < 18 ~ "[15-18[",
                              age < 30 ~ "[18-30[",
                              age < 40 ~ "[30-40[",
                              age < 50 ~ "[40-50[",
                              age < 60 ~ "[50-60[",
                              age < 70 ~ "[60-70[",
                              age < 80 ~ "[70-80[",
                              TRUE ~  "[80-["
                            ),
                            cage3 = ifelse(age>=18,"ge_18","lt_18"),
                            
                          ) 
      )|>  
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.rgp') |>
                          dplyr::distinct(ident,ghmv2021) |> 
                          dplyr::rename(ghm2=ghmv2021) |> 
                          dplyr::mutate(racine =substr(ghm2,1,5))) |>
      dplyr::mutate(raac = NA) |>   # raac absent des millésimes <= 22 (colonne inerte depuis la suppression de la branche chir ambu)
      dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                         dplyr::filter(typ_diag==5,diag%in%c(code_dnid_ins,code_dnid,code_did)) |> 
                         dplyr::mutate(diabete = dplyr::case_when(diag %in% code_dnid_ins ~ "E11i",  
                                                                  diag %in% code_dnid ~ "E11ni",
                                                                  diag %in%code_did  ~ "E10",
                                                                  TRUE~NA)) |> 
                         dplyr::filter(!is.na(diabete)) |> 
                         dplyr::select(ident,diabete) |> 
                         dplyr::group_by(ident) |> dplyr::filter(dplyr::row_number(diabete) == 1L) |> dplyr::ungroup())  |>   # §5.9a (ex distinct(ident,.keep_all=TRUE))
      dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                         dplyr::filter(typ_diag==5,diag%in%c("I10")) |> 
                         dplyr::mutate(hta = "I10") |>
                         dplyr::select(ident,hta) |> 
                         dplyr::distinct(ident,.keep_all = TRUE) )-> query   # déterministe : hta constant ("I10"), §5.9a sans objet
    
    
    
  }

  query |> 
    dplyr::filter( (nbrum == 1 & type_unite == "UHCD" ) | type_unite != "UHCD" )  |> 
    dplyr::mutate(cage2 = ifelse(cage3=="lt_18" & substr(ghm2,3,3)=="C" & age>14,"ge_18",cage3)) |>   # règle cage2 v7.1.2 l.68 (§4, §6.1)
    dplyr::select(anonyme,ident,mode_hospit,mode_entree,mode_sortie,sexe,categ_pmsi,cage3,cage2,cage,racine,ghm2,
                  diabete,hta,diag2,mdp,rumdudp,nbda,duree,type_unite,prep_sc,raac)  |>   # + cage2, raac (§6.1)
    dplyr::filter(substr(ghm2,1,2)!="90") |> 
    dplyr::rename(age = cage3) |> 
    dplyr::mutate(diabete = ifelse(is.na(diabete),"N",diabete),
                  hta = ifelse(is.na(hta),"N",hta)) |> 
    dplyr::compute("prep_data_" %+% an,temporary=TRUE,overwrite=TRUE)
  
  
 
  
}

## ---- 3. Tables de référence ----

# 3a. ref_das_aigu(an) : DAS par strate pour la complétion des séjours longs.
# Source : v7.2 l.444-453 (df_das_ref). Écarts : bloc B2 de MODIFICATIONS_V8.md.
ref_das_aigu <- function(an){
  pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
    dplyr::filter(duree>DUREE_MIN_REF) |> 
    dplyr::rename(rum =  rumdudp) |> 
    dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                        dplyr::filter(typ_diag==5,!diag%in%c(comp_sat_diab,code_dnid_ins,code_dnid,code_did,
                                                             codes_astrisques_diabete,"I10")) |> 
                        dplyr::rename(das = diag) ) |> 
    
    dplyr::summarise(n =dplyr::n(),.by=dplyr::all_of(c("mode_hospit","sexe","cage","racine","ghm2","diag2","das"))) |> 
    dplyr::collect()
}

# 3b. prep_das_chronique(an) / ref_das_chronique(an) : DAS chroniques des séjours longs
# (référentiel all_cim10_caract_patient, type_liste == "Patho_chro", néo-codes diabète).
# Source : v7.1.2 l.88-112 (prep_das). Écarts : bloc B3 de MODIFICATIONS_V8.md
# (filtre duree > DUREE_MIN_REF §6.2 ; millésime anseqta §5.8 ; la summarise finale est
# déportée dans ref_das_chronique pour conserver le niveau séjour, réutilisé par 3d et §7.6).
prep_das_chronique<-function(an){
  
  anseqta = anseqta_de(an)
  
   pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
    dplyr::filter(duree>DUREE_MIN_REF) |>   # §6.2 : prévalence estimée sur les séjours longs
    dplyr::rename(rum =  rumdudp) |> 
    dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> dplyr::filter(typ_diag==5) |> 
                       dplyr::rename(das = diag) ) |> 
    dplyr::distinct_at(c("ident","diag2","das","cage","sexe")) |> 
    dplyr::left_join( pRatihque::atihble(conn, 'prd_vue_nompmsi.mco_diag_niveau') |> dplyr::filter(!!dplyr::sym("v20"%+% anseqta)>1) |>   # §5.8 (ex v2025>1)
                        dplyr::select(dplyr::all_of(c("code","v20"%+% anseqta))) |> 
                        dplyr::rename(das = code,niveau = !!dplyr::sym("v20"%+% anseqta))
    ) |> 
    dplyr::left_join( pRatihque::atihble(conn, 'prd_vue_nompmsi.all_cim10_caract_patient') |> 
                        dplyr::rename(das = code)
    ) |> 
    dplyr::filter(type_liste=="Patho_chro",!das%in%codes_astrisques_diabete) |> 
    dplyr::mutate(niveau = ifelse(is.na(niveau),"1",niveau),
                  das = dplyr::case_when(das %in% code_dnid_ins ~ "E11i",
                                         das %in% code_dnid ~ "E11ni",
                                         das %in%code_did  ~ "E10",
                                             TRUE~das)) |> 
    dplyr::compute("prep_das_chro_" %+% an,temporary=TRUE,overwrite=TRUE) |>
    invisible()

  
}

ref_das_chronique <- function(an){
  pRatihque::atihble(conn, "prep_das_chro_" %+% an) |>
    dplyr::summarise(nb_das = dplyr::n(),.by= c(diag2,das,sexe,cage,niveau,type_liste,caract)) |>   # v7.1.2 l.108
    dplyr::collect()
}

# 3c. ref_comp_diabete(an) : distribution des complications (4e caractère) des codes
# diabète par (cage, diabete), CHR/U. Source : v7.1.2 l.195-205 (chaîne valide ;
# la version v7.2 l.456-467 au pipe cassé est abandonnée, §5.3 ; le mutate(sc = ...)
# orphelin, vestigial, est supprimé).
ref_comp_diabete <- function(an){
  pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
    dplyr::filter(categ_pmsi==TYPE_ETBS_REF_DIABETE) |> 
    dplyr::rename(rum =  rumdudp) |>   dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                                                           dplyr::filter(typ_diag==5,diag%in%c(code_dnid_ins,code_dnid,code_did))) |> 
    dplyr::mutate(diabete = dplyr::case_when(diag %in% code_dnid_ins ~ "E11i",  
                                             diag %in% code_dnid ~ "E11ni",
                                             diag %in%code_did  ~ "E10",
                                             TRUE~NA),
                  comp = substr(diag,4,4)) |> 
    dplyr::summarise(nb = dplyr::n(),.by=c(cage,diabete,comp)) |> 
    dplyr::collect()
}

# 3d. ref_nb_chroniques(an) : distribution du nombre de DAS chroniques distincts par
# séjour long, par (cage, sexe), zéros inclus. Nouveau bloc (§3.3d, §6.2) construit sur
# prep_das_chro_<an> (3b) — bloc B4 de MODIFICATIONS_V8.md.
ref_nb_chroniques <- function(an){
  pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
    dplyr::filter(duree>DUREE_MIN_REF) |>
    dplyr::distinct(ident,cage,sexe) |>
    dplyr::left_join(pRatihque::atihble(conn, "prep_das_chro_" %+% an) |>
                       dplyr::distinct(ident,das) |>
                       dplyr::summarise(nb_chro = dplyr::n(),.by=ident)) |>
    dplyr::mutate(nb_chro = ifelse(is.na(nb_chro),0,nb_chro)) |>
    dplyr::summarise(nb = dplyr::n(),.by=c(cage,sexe,nb_chro)) |>
    dplyr::collect()
}

# 3e. Référentiels exportés (§7.5, §7.6) — nouveaux blocs B5, B6 de MODIFICATIONS_V8.md.
# §7.5 : effectifs de tous les codes de .diag par (cat, code, cage, sexe) ; le filtre sur
# les catégories contenant un code « sans précision » se fait après collect() (pas de
# liste IN volumineuse côté base).
ref_substitution_imprecis <- function(an, codes_imprecis){
  anseqta = anseqta_de(an)
  pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
    dplyr::distinct(ident,cage,sexe) |>
    dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |>
                        dplyr::select(ident,diag)) |>
    dplyr::mutate(cat = substr(diag,1,3)) |>
    dplyr::rename(code = diag) |>
    dplyr::summarise(nb = dplyr::n(),.by=c(cat,code,cage,sexe)) |>
    dplyr::filter(nb>=SEUIL_REF_IMPRECIS) |>
    dplyr::left_join( pRatihque::atihble(conn, 'prd_vue_nompmsi.mco_diag_niveau') |> dplyr::filter(!!dplyr::sym("v20"%+% anseqta)>1) |> 
                        dplyr::select(dplyr::all_of(c("code","v20"%+% anseqta))) |> 
                        dplyr::rename(niveau = !!dplyr::sym("v20"%+% anseqta))
    ) |>
    dplyr::collect() |>
    dplyr::filter(cat %in% unique(substr(codes_imprecis,1,3))) |>
    dplyr::mutate(imprecis = code %in% codes_imprecis) |>
    dplyr::arrange(cat,code,cage,sexe)
}

# §7.6 : co-occurrences par séjour des paires de DAS chroniques (das_a < das_b).
ref_paires_chroniques <- function(an){
  das_sejour <- pRatihque::atihble(conn, "prep_das_chro_" %+% an) |>
    dplyr::distinct(ident,cage,sexe,das)
  das_sejour |>
    dplyr::rename(das_a = das) |>
    dplyr::inner_join(das_sejour |> dplyr::rename(das_b = das), by = c("ident","cage","sexe")) |>
    dplyr::filter(das_a < das_b) |>
    dplyr::summarise(nb = dplyr::n(),.by=c(das_a,das_b,cage,sexe)) |>
    dplyr::filter(nb>=SEUIL_REF_PAIRES) |>
    dplyr::collect()
}

# 3f. Exécution des préparations (base). Ordre v7.2 l.439 : prep_data pour toutes les
# années, puis tables de référence sur AN_REF.
for(an_ in ANS_HISTORIQUE) prep_data(an_)
gc()

df_das_ref       <- ref_das_aigu(AN_REF)
prep_das_chronique(AN_REF)
df_das_chronique <- ref_das_chronique(AN_REF)
df_nb_chroniques <- ref_nb_chroniques(AN_REF)

# Correction des effectifs .9 (v7.1.2 l.207-214 ; constantes en config)
df_res_epi_comp_diabete <- ref_comp_diabete(AN_REF) |>
  dplyr::mutate(tot = sum(nb,na.rm=TRUE),.by=c(cage,diabete)) |> 
  dplyr::mutate(nb = dplyr::case_when(comp=="9"&cage%in%CAGE_AGES~tot*PENALITE_9_AGES,
                                      comp=="9"&! cage%in%CAGE_PED~tot*PENALITE_9_AUTRES,
                                      TRUE~nb)) |> 
  dplyr::select(-tot)

## ---- 4. Helpers purs (testables hors base) ----
# Règles : aucune variable globale implicite (toute table de référence est un argument),
# aucun `<<-`, dépendances limitées à dplyr/tibble/stringr/base. Ces fonctions masquent
# les versions homonymes de utils.R (retro_code_diabete, get_codes_diabete_from_neo).
# tests/test_helpers.R charge uniquement cette section (balises "## ---- 4." / "## ---- 5.").

# Classes d'âge (même découpage que prep_data, v7.1.2 l.53-66 / l.238-251)
decoupe_cage <- function(age){
  dplyr::case_when(
    age < 1 ~ "[0-1[",
    age < 5 ~ "[1-5[",
    age < 10 ~ "[5-10[",
    age < 15 ~ "[10-15[",
    age < 18 ~ "[15-18[",
    age < 30 ~ "[18-30[",
    age < 40 ~ "[30-40[",
    age < 50 ~ "[40-50[",
    age < 60 ~ "[50-60[",
    age < 70 ~ "[60-70[",
    age < 80 ~ "[70-80[",
    TRUE ~ "[80-["
  )
}

# Tirage d'un âge (scalaire) dans une classe semi-ouverte "[a-b[" -> a:(b-1) ;
# classe ouverte "[a-[" -> a:age_max. Libellé inconnu -> NA (§5.6).
sample_age_ligne <- function(cage, age_max = 95){
  cage <- as.character(cage)
  if(length(cage) != 1 || is.na(cage)) return(NA_integer_)
  bornes <- as.integer(unlist(regmatches(cage, gregexpr("[0-9]+", cage))))
  if(length(bornes) == 0) return(NA_integer_)
  if(length(bornes) == 1) bornes <- c(bornes, as.integer(age_max) + 1L)
  if(bornes[2] <= bornes[1]) return(bornes[1])
  v <- seq.int(bornes[1], bornes[2] - 1L)
  v[sample.int(length(v), 1)]
}

# Version vectorisée : un tirage indépendant par ligne (§5.6)
sample_age <- function(cage, age_max = 95){
  vapply(as.character(cage), sample_age_ligne, integer(1), age_max = age_max, USE.NAMES = FALSE)
}

# Dédoublonnage à la catégorie 3 caractères (au plus un code par substr(code,1,3), premier
# arrivé conservé) + exclusions par paires de préfixes (§6.4). La graine et les codes
# doctrine doivent être passés en tête de `codes`.
dedup_categorie <- function(codes, paires_exclues = list()){
  codes <- as.character(codes)
  codes <- codes[!is.na(codes) & nzchar(codes)]
  gardes <- character(0)
  for(x in codes){
    if(substr(x, 1, 3) %in% substr(gardes, 1, 3)) next
    exclu <- FALSE
    for(p in paires_exclues){
      p <- as.character(p)
      if(length(p) != 2) stop("exclusions_paires : chaque entrée doit être une paire [prefixeA, prefixeB]")
      if((startsWith(x, p[1]) && any(startsWith(gardes, p[2]))) ||
         (startsWith(x, p[2]) && any(startsWith(gardes, p[1])))){
        exclu <- TRUE
        break
      }
    }
    if(exclu) next
    gardes <- c(gardes, x)
  }
  gardes
}

# Néo-code diabète à partir d'un code (DP ou DAS) ; `defaut` sinon (v7.2 l.341-344)
neo_code_de_diag <- function(diag, code_did, code_dnid_ins, code_dnid, defaut = "N"){
  dplyr::case_when(diag %in% code_dnid_ins ~ "E11i",
                   diag %in% code_dnid ~ "E11ni",
                   diag %in% code_did ~ "E10",
                   TRUE ~ defaut)
}

# Rétro-codage néo-code + complication -> code CIM-10 (utils.R l.336-342).
# 5e caractère : "0" = insulinotraité (E11i, cf. code_dnid_ins = E1120...),
# "8" = non insulinotraité ou sans précision (E11ni, cf. code_dnid = E1128...).
# utils.R inversait 0/8 : corrigé ici (voir MODIFICATIONS_V8.md, écart H1).
retro_code_diabete <- function(neocode, comp){
  comp <- as.character(comp)
  dplyr::case_when(neocode == "E10" ~ paste0("E10", comp),
                   neocode == "E11i" ~ paste0("E11", comp, "0"),
                   neocode == "E11ni" ~ paste0("E11", comp, "8"))
}

# Chemins (codes_diabete.yaml) des codes astérisques obligatoires par type de complication
CHEMINS_ASTERISQUES_DIABETE <- c(
  "2" = "renal/asterisques_obligatoires",
  "3" = "oculaire/asterisques_obligatoires",
  "4" = "neurologique/asterisques_obligatoires",
  "5" = "vasculaire_peripherique/asterisques_obligatoires",
  "6" = "autres_precisees/asterisques_obligatoires"
)

# Tirage de la complication (4e caractère) d'un diabète : distribution empirique
# ref_comp_diabete (cage, diabete, comp, nb), repli toutes classes d'âge si strate vide,
# repli "9" si aucune information. comp "8" (non précisées) -> "9" ; comp "7" (multiples)
# -> 3 à 4 complications distinctes parmi 2:6 (v7 : utils.R l.350-358, v7.2 l.373-385).
tirer_comp_diabete <- function(diabete_, cage_, ref_comp_diabete, comp_forcee = NULL){
  if(!is.null(comp_forcee)){
    comp <- as.character(comp_forcee)
  } else {
    ok <- !is.na(ref_comp_diabete$nb) & ref_comp_diabete$nb > 0 & ref_comp_diabete$diabete == diabete_
    prep_diab <- ref_comp_diabete[ok & ref_comp_diabete$cage == cage_, ]
    if(nrow(prep_diab) == 0) prep_diab <- ref_comp_diabete[ok, ]
    if(nrow(prep_diab) == 0) return(list(comp = "9", comps = "9"))
    comp <- as.character(prep_diab$comp[sample.int(nrow(prep_diab), 1, prob = prep_diab$nb)])
  }
  if(comp == "8") comp <- "9"
  comps <- comp
  if(comp == "7") comps <- sample(c("2", "3", "4", "5", "6"), sample(3:4, 1))
  list(comp = comp, comps = comps)
}

# Codes CIM-10 à insérer pour un néo-code diabète : code E10x / E11xx + astérisques
# obligatoires pour chaque complication 2:6 (utils.R l.345-382, signature explicite §5.2).
# codes_diab : tibble (code, chemin) issu de lire_codes_diabete().
get_codes_diabete_from_neo <- function(diabete_, cage_, ref_comp_diabete, codes_diab, comp_forcee = NULL){
  tc <- tirer_comp_diabete(diabete_, cage_, ref_comp_diabete, comp_forcee)
  code_diabete_sample <- retro_code_diabete(diabete_, tc$comp)
  comp_diag <- character(0)
  for(c_ in tc$comps){
    if(!c_ %in% names(CHEMINS_ASTERISQUES_DIABETE)) next
    candidats <- codes_diab$code[grepl(CHEMINS_ASTERISQUES_DIABETE[[c_]], codes_diab$chemin)]
    if(length(candidats) == 0) next
    comp_diag <- c(comp_diag, candidats[sample.int(length(candidats), 1)])
  }
  c(comp_diag, code_diabete_sample)
}

# Insertion de I10 quand le patient est hypertendu et qu'aucun code hta_autres n'est
# présent (v7.2 l.365). I10 n'est jamais gardé en plus d'un code hta_autres.
ajoute_hta <- function(das, hta_flag, hta_autres){
  das <- das[das != "I10"]
  if(!is.na(hta_flag) && hta_flag != "N" && length(intersect(hta_autres, das)) == 0) das <- c("I10", das)
  das
}

# Regroupement des objets de référence passés aux fonctions de tirage (§5.2 : plus de
# variable globale implicite).
construire_refs <- function(comp_diabete, codes_diab, codes_comp_sat_diab, hta_autres,
                            code_did, code_dnid_ins, code_dnid, neo_codes, paires_exclues = list()){
  stopifnot(is.data.frame(comp_diabete), all(c("cage", "diabete", "comp", "nb") %in% names(comp_diabete)),
            is.data.frame(codes_diab), all(c("code", "chemin") %in% names(codes_diab)),
            is.character(hta_autres), is.character(neo_codes), is.list(paires_exclues))
  list(comp_diabete = comp_diabete, codes_diab = codes_diab, codes_comp_sat_diab = codes_comp_sat_diab,
       hta_autres = hta_autres, code_did = code_did, code_dnid_ins = code_dnid_ins, code_dnid = code_dnid,
       neo_codes = neo_codes, paires_exclues = paires_exclues)
}

# Table de prévalence chronique en deux niveaux : strate (diag2, cage, sexe) et repli (cage, sexe) (§6.2)
prep_ref_chronique <- function(ref_das_chronique){
  list(
    strate = ref_das_chronique |> dplyr::summarise(nb_das = sum(nb_das), .by = c(diag2, das, sexe, cage)),
    repli  = ref_das_chronique |> dplyr::summarise(nb_das = sum(nb_das), .by = c(das, sexe, cage))
  )
}

# Codes candidats d'une strate, avec repli si moins de seuil_ref codes (§6.2)
candidats_chroniques <- function(diag, sexe_, cage_, ref_chro, seuil_ref){
  tmp <- ref_chro$strate |> dplyr::filter(diag2 == diag, sexe == sexe_, cage == cage_)
  source <- "strate"
  if(nrow(tmp) < seuil_ref){
    tmp <- ref_chro$repli |> dplyr::filter(sexe == sexe_, cage == cage_)
    source <- "repli"
  }
  list(tmp = tmp, source = source)
}

# Nombre cible de DAS chroniques : distribution empirique (cage, sexe, nb_chro, nb) sinon
# cible dégradée uniforme dans cibles_defaut[[cage]] (§6.2)
tirer_nb_chroniques <- function(cage_, sexe_, ref_nb_chro, cibles_defaut = list()){
  tmp <- ref_nb_chro[ref_nb_chro$cage == cage_ & ref_nb_chro$sexe == sexe_ & !is.na(ref_nb_chro$nb) & ref_nb_chro$nb > 0, ]
  if(nrow(tmp) > 0) return(as.integer(tmp$nb_chro[sample.int(nrow(tmp), 1, prob = tmp$nb)]))
  b <- cibles_defaut[[cage_]]
  if(is.null(b)) return(0L)
  v <- seq.int(b[1], b[2])
  as.integer(v[sample.int(length(v), 1)])
}

# Filtre GHM en C (v7.1.2 l.131-133) : exclut R2630, F0x et F1x sauf F17.
# v7.1.2 écrivait substr(das,1,2)!="F10" (2 caractères comparés à 3 : toujours vrai, F1x
# jamais exclu) ; corrigé en substr(das,1,2)!="F1" (MODIFICATIONS_V8.md, écart H2).
filtre_das_ghm_c <- function(tmp, ghm2_){
  if(substr(ghm2_,3,3)=="C"){
    tmp<-tmp |> dplyr::filter(das !="R2630", substr(das,1,2)!="F0", ( substr(das,1,2)!="F1"  | substr(das,1,3)=="F17") )
  }
  tmp
}

# Cœur de tirage des séjours courts (§6.2). Source : v7.1.2 l.117-164 (sample_das), refondu :
# nb de DAS tiré dans ref_nb_chro (plus de boucle 2:4), repli de strate, dedup_categorie,
# diabète/HTA, nb_tirages variantes. Retourne NULL si la strate est vide.
sample_das_court <- function(mode_hospit, sexe, cage, ghm2, diag2, duree, nb = NA,
                             ref_chro, ref_nb_chro, refs,
                             nb_tirages = 1, seuil_ref = 20, cibles_defaut = list(), age_max = 95){
  mode_hospit_ = as.character(mode_hospit)
  sexe_ = as.character(sexe)
  cage_ = as.character(cage)
  ghm2_ = as.character(ghm2)
  diag = as.character(diag2)
  duree_ = as.integer(duree)
  poids_ = as.numeric(nb)
  
  cand <- candidats_chroniques(diag, sexe_, cage_, ref_chro, seuil_ref)
  tmp <- filtre_das_ghm_c(cand$tmp, ghm2_)
  if(nrow(tmp) < 1) return(NULL)
  
  diabete_dp <- neo_code_de_diag(diag, refs$code_did, refs$code_dnid_ins, refs$code_dnid)
  
  df_tmp <- NULL
  for(i in seq_len(nb_tirages)){
    nb_cible <- tirer_nb_chroniques(cage_, sexe_, ref_nb_chro, cibles_defaut)
    age_i <- sample_age_ligne(cage_, age_max)
    das_samples <- character(0)
    if(nb_cible > 0){
      das_samples <- sample(x = tmp$das, prob = tmp$nb_das, size = min(nb_cible, nrow(tmp)))
    }
    das_samples <- dedup_categorie(das_samples, refs$paires_exclues)
    
    # Diabète : flag issu du DP, sinon d'un néo-code tiré ; néo-codes remplacés par les codes réels
    diabete_ <- diabete_dp
    neo_tires <- intersect(refs$neo_codes, das_samples)
    if(diabete_ == "N" && length(neo_tires) > 0) diabete_ <- neo_tires[1]
    das_samples <- das_samples[!das_samples %in% refs$neo_codes]
    codes_diabete <- character(0)
    if(diabete_ != "N") codes_diabete <- get_codes_diabete_from_neo(diabete_, cage_, refs$comp_diabete, refs$codes_diab)
    
    # HTA : flag porté par un I10 tiré ; I10 retiré si un code hta_autres est présent
    hta_ <- if("I10" %in% das_samples) "I10" else "N"
    das_final <- dedup_categorie(c(codes_diabete, ajoute_hta(das_samples, hta_, refs$hta_autres)), refs$paires_exclues)
    
    tibble::tibble(mode_hospit = mode_hospit_, sexe = sexe_, cage = cage_, ghm2 = ghm2_, diag2 = diag,
                   duree = duree_, poids = poids_, variante = i, age = age_i,
                   source_ref = cand$source, nb_cible = nb_cible, nb_das = length(das_final),
                   diabete_scenario = diabete_, hta_scenario = hta_,
                   diagnostic_associes = paste(das_final, collapse = " ")) |>
      dplyr::bind_rows(df_tmp) -> df_tmp
  }
  
  return(df_tmp)
}

# Cœur de tirage des séjours longs (§6.3). Source : v7.2 l.330-422 (sample_das), corrigé :
# §5.1 sexe == sexe_ ; §5.2 age en argument ; §5.4 dedup_categorie ; §5.5 codes_diab ;
# tables de référence en argument. Retourne NULL si la strate est vide.
sample_das_long <- function(mode_hospit, sexe, age, cage, racine, ghm2, diabete, hta, diag2, nbda,
                            diagnostic_associes, type_unite = NA, prep_sc = NA, poids = NA,
                            ref_das_aigu, refs, nb_tirage = 1){
  
  mode_hospit_ = as.character(mode_hospit)
  sexe_ = as.character(sexe)
  age_ = as.character(age)
  cage_ = as.character(cage)
  racine_ = as.character(racine)
  ghm2_ = as.character(ghm2)
  type_unite_ = as.character(type_unite)
  prep_sc_ = as.numeric(prep_sc)
  poids_ = as.numeric(poids)
  
  da = unlist(stringr::str_split(diagnostic_associes," "))
  da = da[!is.na(da) & nzchar(da)]
  diag = as.character(diag2)
  diabete_ = as.character(diabete)
  diabete_ = neo_code_de_diag(diag, refs$code_did, refs$code_dnid_ins, refs$code_dnid, defaut = diabete_)
  hta_ = as.character(hta)
  
  nbda_ = as.integer(nbda)
  
  ref_das_aigu |> dplyr::filter(diag2 == diag,mode_hospit == mode_hospit_, sexe == sexe_, cage== cage_,ghm2==ghm2_,!das%in%da )  -> tmp   # §5.1 (ex sexe_ ==sexe_)
  
  if(nrow(tmp)<1) return(NULL)
  
  df_tmp<-NULL
  
  nb_max = min(nbda_,nrow(tmp))
  
  for(i in seq_len(nb_tirage)){
    
    sample(x= tmp$das,prob = tmp$n,size = nb_max)->das_samples
    
    das_samples <- dedup_categorie(c(da, das_samples), refs$paires_exclues)   # §5.4 (ex filter_chap) ; la graine passe en premier
    
    das_samples <- ajoute_hta(das_samples, hta_, refs$hta_autres)
    
    #Pour le diabète :
    # - Vérification des DAS finalement choisis :
    #   * Si complication en lien avec diabète = complication multiples
    #   * Sinon : répartition en fonction de l'âge et du diabète
    if(diabete_ != "N"){
      comp_forcee <- if(length(intersect(c(diag, das_samples), refs$codes_comp_sat_diab)) != 0) "7" else NULL
      codes_diabete <- get_codes_diabete_from_neo(diabete_, cage_, refs$comp_diabete, refs$codes_diab, comp_forcee)
      das_samples <- dedup_categorie(c(da, codes_diabete, das_samples), refs$paires_exclues)
    }
    
    tibble::tibble(mode_hospit = mode_hospit_, sexe = sexe_, age = age_, cage = cage_, racine = racine_,
                   ghm2 = ghm2_, diabete = as.character(diabete), hta = hta_, diag2 = diag, nbda = nbda_,
                   type_unite = type_unite_, prep_sc = prep_sc_, poids = poids_, variante = i,
                   graine = paste(da, collapse = " "), diabete_scenario = diabete_,
                   nb_das = length(das_samples),
                   diagnostic_associes = paste(das_samples, collapse = " ")) |>
      dplyr::bind_rows(df_tmp) -> df_tmp
    
  }
  
  return(df_tmp)
  
}

# Codes CIM-10 dont le libellé indique « sans précision » (§7.5) ; codes sans point.
codes_imprecis_de_cim <- function(cim, motif = "sans précision|non précisé"){
  stopifnot(all(c("code", "libelle") %in% names(cim)))
  code <- gsub(".", "", as.character(cim$code), fixed = TRUE)
  unique(code[grepl(motif, enc2utf8(as.character(cim$libelle)), ignore.case = TRUE, perl = TRUE)])
}

# --- Helpers du rapport de contrôle (§8.2) ---
split_das <- function(x){
  x <- ifelse(is.na(x), "", as.character(x))
  lapply(strsplit(x, " ", fixed = TRUE), function(v) v[nzchar(v)])
}

# Vérifications programmatiques d'un jeu de scénarios. Retourne une liste de compteurs
# (0 attendu partout). NA si la colonne nécessaire est absente.
controler_scenarios <- function(df, hta_autres, seuil_pivot){
  n <- nrow(df)
  res <- list(n = n, doublons_categorie = NA, diabete_hors_flag = NA, i10_avec_hta_autres = NA, poids_sous_seuil = NA)
  if("poids" %in% names(df)) res$poids_sous_seuil <- sum(!(df$poids > seuil_pivot), na.rm = TRUE)
  if(!"diagnostic_associes" %in% names(df) || n == 0) return(res)
  das <- split_das(df$diagnostic_associes)
  res$doublons_categorie <- sum(vapply(das, function(v) any(duplicated(substr(v, 1, 3))), logical(1)))
  res$i10_avec_hta_autres <- sum(vapply(das, function(v) "I10" %in% v && length(intersect(v, hta_autres)) > 0, logical(1)))
  if("diabete_scenario" %in% names(df)){
    a_diab <- vapply(das, function(v) any(substr(v, 1, 3) %in% c("E10", "E11")), logical(1))
    res$diabete_hors_flag <- sum(a_diab & df$diabete_scenario == "N")
  }
  res
}

# Distribution du nombre de DAS par classe d'âge (comptes)
distribution_nb_das <- function(df){
  if(!"diagnostic_associes" %in% names(df) || nrow(df) == 0) return(NULL)
  df |>
    dplyr::mutate(nb_das = lengths(split_das(diagnostic_associes))) |>
    dplyr::summarise(n = dplyr::n(), moy = round(mean(nb_das), 2), min = min(nb_das),
                     q50 = stats::median(nb_das), max = max(nb_das), .by = cage) |>
    dplyr::arrange(cage)
}

# Taux de DAS « sans précision » parmi les DAS de sortie (mesuré, pas corrigé)
taux_imprecis <- function(df, codes_imprecis){
  if(!"diagnostic_associes" %in% names(df) || nrow(df) == 0) return(NA_real_)
  v <- unlist(split_das(df$diagnostic_associes))
  if(length(v) == 0) return(NA_real_)
  round(mean(v %in% codes_imprecis), 4)
}

## ---- 5. Branche chirurgie ambulatoire (supprimée) ----
# La branche chirurgie ambulatoire (v7.1.2 l.259-272, durée 0, jointure df_dp_das et
# df_ref_specialite) n'est pas reprise : partie obsolète et seul consommateur de df_dp_das,
# référentiel absent du dépôt (MODIFICATIONS_V8.md, section 5 et Q1). Les colonnes raac et
# cage2 de prep_data, ajoutées pour ses pivots, restent en place (inertes).

# Objets de référence communs aux branches 6 et 7 (§5.2 : plus de variable globale implicite)
REFS <- construire_refs(comp_diabete = df_res_epi_comp_diabete, codes_diab = codes_diab,
                        codes_comp_sat_diab = codes_comp_sat_diab, hta_autres = hta_autres,
                        code_did = code_did, code_dnid_ins = code_dnid_ins, code_dnid = code_dnid,
                        neo_codes = neo_codes_diabete, paires_exclues = PAIRES_EXCLUES)

## ---- 6. Branche séjours courts ----
# Durée < 3, saturation en DAS chroniques (§6.2). Sources : v7.1.2 l.217-221 (pivots),
# l.232-234 (df_v_admin), l.228 / l.238-256 (tirage, habillage) — bloc B8.

df_cases_courts <- pRatihque::atihble(conn, 'prep_data_' %+% AN_REF ) |>
  dplyr::filter(duree%in%DUREE_COURTS) |> dplyr::summarise(nb=dplyr::n(),.by=dplyr::all_of(PIVOTS_COURTS)) |> dplyr::filter(nb>SEUIL_PIVOT) |> 
  dplyr::collect()

df_v_admin_courts <- pRatihque::atihble(conn, 'prep_data_' %+% AN_REF ) |> 
  dplyr::distinct(mode_hospit,mode_entree,mode_sortie,sexe,cage,ghm2,diag2,mdp,duree) |> 
  dplyr::collect()

ref_chro <- prep_ref_chronique(df_das_chronique)

df_courts_tirage <- purrr::pmap(df_cases_courts[, c(PIVOTS_COURTS, "nb")], sample_das_court,
                                ref_chro = ref_chro, ref_nb_chro = df_nb_chroniques, refs = REFS,
                                nb_tirages = NB_TIRAGES_COURTS, seuil_ref = SEUIL_REF_DAS,
                                cibles_defaut = CIBLES_NB_CHRONIQUES, age_max = AGE_MAX_OUVERT) |>
  purrr::list_rbind()

# Habillage admin : NB_VARIANTES_ADMIN_COURTS variantes tirées au sort par scénario (§5.9b)
df_courts <- df_courts_tirage |> 
  dplyr::left_join(df_v_admin_courts,relationship = "many-to-many") |> 
  dplyr::group_by(dplyr::across(-dplyr::any_of(COLS_ADMIN))) |> 
  dplyr::slice_sample(n = NB_VARIANTES_ADMIN_COURTS) |> 
  dplyr::ungroup()

print("- Nombre de lignes séjours courts (tirage) = " %+% nrow(df_courts_tirage))
print("- Nombre de lignes séjours courts (final) = " %+% nrow(df_courts))

## ---- 7. Branche séjours longs ----
# Durée 3-100, graine de K_GRAINE_LONGS DAS réels, agrégation ANS_HISTORIQUE × TYPES_ETBS_LONGS.
# Sources : v7.2 l.278-327 (prep_scenarios2), l.483-534 (catalogue), l.542-558 (tirage,
# habillage) — blocs B9, B10.

#----------------------------- Prépa DAS  -------------------------------#
prep_scenarios2<-function(an,type_etbs,nb_journees_aut,nbda_aut,nb_assoc_das,pivots){
  
  anseqta = anseqta_de(an)
  
  
  pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
    dplyr::filter(prep_sc==1) |> 
    dplyr::distinct(ident,prep_sc) |> dplyr::rename(sc= prep_sc) |> 
    dplyr::full_join(pRatihque::atihble(conn, 'prep_data_' %+% an )) |>
    dplyr::mutate(sc = ifelse(is.na(sc),0,1)) |> 
    dplyr::filter(! (prep_sc==0 & sc==1)) |>
    dplyr::filter(categ_pmsi %in% type_etbs,nbda%in%1:nbda_aut,duree %in%nb_journees_aut) |> 
    dplyr::rename(rum =  rumdudp) |> 
    dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                       dplyr::filter(typ_diag==5,!diag%in%c(code_dnid_ins,code_dnid,code_did,
                                                            codes_astrisques_diabete,"I10")) |> 
                       dplyr::rename(das = diag) ) |> 
    dplyr::distinct_at(c("ident",pivots,"das")) |>
    
                    
    dplyr::left_join( pRatihque::atihble(conn, 'prd_vue_nompmsi.mco_diag_niveau') |> dplyr::filter(!!dplyr::sym("v20"%+% anseqta)>1) |>   # §5.8 (ex v2025>1)
                        dplyr::select(dplyr::all_of(c("code","v20"%+% anseqta))) |> 
                        dplyr::rename(das = code,niveau = !!dplyr::sym("v20"%+% anseqta))
    ) |> 
    
    dplyr::collect() -> df_das
  
  
  df_das |> 
    dplyr::mutate(niveau = ifelse(is.na(niveau),"0",niveau)) |> 
    dplyr::mutate(nb_das = dplyr::n(),.by= dplyr::all_of(c(pivots,"das"))) -> df_das
  
  df_das |> 
    dplyr::arrange(ident,dplyr::desc(niveau),dplyr::desc(nb_das),das) |>   # §5.9 : `das` en dernier critère (ordre total)
    dplyr::group_by(ident) |> 
    dplyr::slice(1:nb_assoc_das) -> df_das
  
  df_das |> 
    dplyr::group_by_at(c("ident",pivots)) |> 
    dplyr::arrange(das) |> 
    dplyr::summarise(diagnostic_associes = paste0(das,collapse = " "),.groups="drop") |> 
    dplyr::ungroup() |> 
    dplyr::summarise(n = dplyr::n(),.by=dplyr::all_of(c(pivots,"diagnostic_associes"))) -> df_cases
  
  return(df_cases)
  
  
}

# Catalogue : agrégation v7.2 l.489-510 (CHR/U pour AN_REF puis 17:(AN_REF-1), puis CH pour
# 17:AN_REF). Ordre reproduit : TYPES_ETBS_LONGS × ANS_HISTORIQUE (l'ordre des années n'a
# pas d'effet, la somme est commutative).
construire_catalogue_longs <- function(){
  df_cases <- NULL
  for(type_etbs in TYPES_ETBS_LONGS){
    for(an_ in ANS_HISTORIQUE){
      df_cases_tmp<-prep_scenarios2(an_,type_etbs,DUREE_LONGS,NBDA_MAX,K_GRAINE_LONGS,PIVOTS_LONGS)
      gc()
      df_cases <- dplyr::bind_rows(df_cases,df_cases_tmp) |> 
        dplyr::summarise(n =sum(n),.by=dplyr::all_of(c(PIVOTS_LONGS,"diagnostic_associes")))
      rm(df_cases_tmp)
    }
  }
  df_cases
}

df_catalogue_brut <- construire_catalogue_longs()
print("- Nombre de lignes catalogue brut = " %+% nrow(df_catalogue_brut))

# Seuil de divulgation au niveau des pivots (v7.2 l.526-530 ; §2.2 : nb > SEUIL_PIVOT,
# le n par combinaison est sommé puis abandonné)
df_catalogue_longs <- df_catalogue_brut |>  dplyr::inner_join(df_catalogue_brut |> 
                                                                dplyr::summarise(nb=sum(n),
                                                                                 .by =dplyr::all_of(PIVOTS_LONGS_SEUIL) )  ) |> 
  dplyr::filter(nb>SEUIL_PIVOT) |> dplyr::select(-n) |>
  dplyr::rename(poids = nb)
rm(df_catalogue_brut)

print("- Nombre de lignes catalogue éligible = " %+% nrow(df_catalogue_longs))

# Tirage : tout le catalogue (§6.3), ou MAX_SCENARIOS_LONGS tirés au poids (pas de seuil dur)
df_cases_longs <- df_catalogue_longs
if(!is.na(MAX_SCENARIOS_LONGS) && nrow(df_cases_longs) > MAX_SCENARIOS_LONGS){
  df_cases_longs <- df_cases_longs |> dplyr::slice_sample(n = MAX_SCENARIOS_LONGS, weight_by = poids)
}

df_longs_tirage <- purrr::pmap(df_cases_longs |> dplyr::select(dplyr::all_of(c(PIVOTS_LONGS, "diagnostic_associes", "poids"))),
                               sample_das_long, ref_das_aigu = df_das_ref, refs = REFS, nb_tirage = NB_TIRAGES_LONGS) |>
  purrr::list_rbind()

#Ajout des modes entrée/sortie (v7.2 l.550-555)
df_v_admin_longs <- pRatihque::atihble(conn, 'prep_data_' %+% AN_REF ) |> 
  dplyr::distinct(mode_hospit,mode_entree,mode_sortie,sexe,age,cage,ghm2,diag2,mdp,nbda,duree) |> 
  dplyr::collect()

df_longs <- df_longs_tirage |> dplyr::left_join(df_v_admin_longs,relationship = "many-to-many")
if(!is.na(NB_VARIANTES_ADMIN_LONGS)){
  df_longs <- df_longs |>
    dplyr::group_by(dplyr::across(-dplyr::any_of(c(COLS_ADMIN, "duree")))) |>
    dplyr::slice_sample(n = NB_VARIANTES_ADMIN_LONGS) |>
    dplyr::ungroup()
}

print("- Nombre de lignes séjours longs (tirage) = " %+% nrow(df_longs_tirage))
print("- Nombre de lignes séjours longs (final) = " %+% nrow(df_longs))

## ---- 8. Exports ----
if(!dir.exists(PATH_RESULTS)) dir.create(PATH_RESULTS, recursive = TRUE)
chemin_export <- function(nom) PATH_RESULTS %+% nom %+% "_v8_" %+% DATE_TAG %+% ".parquet"

arrow::write_parquet(df_courts,          chemin_export("scenarios_courts"))
arrow::write_parquet(df_catalogue_longs, chemin_export("scenarios_longs_catalogue"))
arrow::write_parquet(df_longs,           chemin_export("scenarios_longs_tirage"))

# §7.5 : référentiel de substitution des codes « sans précision » (aucune substitution ici)
codes_imprecis <- codes_imprecis_de_cim(cim, MOTIF_IMPRECIS)
df_ref_imprecis <- ref_substitution_imprecis(AN_REF, codes_imprecis)
arrow::write_parquet(df_ref_imprecis, chemin_export("referentiel_substitution_imprecis"))

# §7.6 : paires de DAS chroniques co-occurrentes (mesure a posteriori, aucun usage dans le tirage)
df_ref_paires <- ref_paires_chroniques(AN_REF)
arrow::write_parquet(df_ref_paires, chemin_export("referentiel_paires_chroniques"))

## ---- 9. Rapport de contrôle ----
branches <- list("sejours_courts" = df_courts,
                 "sejours_longs_catalogue" = df_catalogue_longs,
                 "sejours_longs_tirage" = df_longs)
pivots_branches <- list("sejours_courts" = PIVOTS_COURTS,
                        "sejours_longs_catalogue" = PIVOTS_LONGS,
                        "sejours_longs_tirage" = PIVOTS_LONGS)

lignes <- c("RAPPORT DE CONTROLE — extraction_associations_codes_v8.R — " %+% DATE_TAG,
            "SEED = " %+% SEED %+% " ; AN_REF = " %+% AN_REF %+% " ; ANS_HISTORIQUE = " %+% paste(range(ANS_HISTORIQUE), collapse = "-"),
            "SEUIL_PIVOT = " %+% SEUIL_PIVOT %+% " ; SEUIL_REF_DAS = " %+% SEUIL_REF_DAS %+% " ; K_GRAINE_LONGS = " %+% K_GRAINE_LONGS,
            "")

lignes <- c(lignes, "== 1. Volumétrie par branche ==")
for(b in names(branches)){
  df_b <- branches[[b]]
  piv <- intersect(pivots_branches[[b]], names(df_b))
  n_piv <- if(length(piv) > 0) nrow(dplyr::distinct(df_b[, piv])) else NA
  lignes <- c(lignes, sprintf("%-26s lignes = %8d ; pivots distincts = %8s", b, nrow(df_b), format(n_piv)))
}
lignes <- c(lignes, "catalogue longs brut -> éligible : voir prints de la section 7", "")

lignes <- c(lignes, "== 2. Distribution du nombre de DAS par classe d'âge (à comparer aux cibles de saturation) ==")
for(b in names(branches)){
  d <- distribution_nb_das(branches[[b]])
  lignes <- c(lignes, "-- " %+% b)
  if(is.null(d)) lignes <- c(lignes, "   (pas de colonne diagnostic_associes)") else lignes <- c(lignes, "   " %+% utils::capture.output(print(as.data.frame(d), row.names = FALSE)))
}
lignes <- c(lignes, "-- cibles dégradées CIBLES_NB_CHRONIQUES :",
            "   " %+% names(CIBLES_NB_CHRONIQUES) %+% " : " %+% vapply(CIBLES_NB_CHRONIQUES, function(x) paste(x, collapse = "-"), character(1)), "")

lignes <- c(lignes, "== 3. Taux de codes « sans précision » parmi les DAS de sortie (mesuré, non corrigé) ==")
for(b in names(branches)){
  lignes <- c(lignes, sprintf("%-26s taux = %s", b, format(taux_imprecis(branches[[b]], codes_imprecis))))
}
lignes <- c(lignes, "")

lignes <- c(lignes, "== 4. Vérifications programmatiques (0 attendu ; NA = non applicable) ==")
controles <- lapply(branches, controler_scenarios, hta_autres = hta_autres, seuil_pivot = SEUIL_PIVOT)
for(b in names(controles)){
  cc <- controles[[b]]
  lignes <- c(lignes, sprintf("%-26s doublons_categorie = %s ; diabete_hors_flag = %s ; i10_avec_hta_autres = %s ; poids_sous_seuil = %s",
                              b, format(cc$doublons_categorie), format(cc$diabete_hors_flag),
                              format(cc$i10_avec_hta_autres), format(cc$poids_sous_seuil)))
}
anomalies <- sum(unlist(lapply(controles, function(cc) unlist(cc[c("doublons_categorie", "diabete_hors_flag", "i10_avec_hta_autres", "poids_sous_seuil")]))), na.rm = TRUE)
lignes <- c(lignes, "TOTAL anomalies = " %+% anomalies, "")

writeLines(lignes, PATH_RESULTS %+% "rapport_v8_" %+% DATE_TAG %+% ".txt")
cat(lignes, sep = "\n")
