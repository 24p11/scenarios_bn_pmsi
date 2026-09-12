###############################################################################
# extraction_associations_codes_v8.R — EXTRACTION (base -> agrégats parquet)
#
# Pipeline scenarios_bn_pmsi v8, industrialisation. Fichiers :
#   config_v8.R  : configuration et profils        helpers_v8.R : helpers purs
#   ce script    : requêtes base, partiels, refs   tirage_scenarios_v8.R : tirage (sans base)
# Spécification : SPEC_V8.md ; journal des écarts : MODIFICATIONS_V8.md.
#
# RÈGLE D'OR (SPEC §0) : les chaînes dbplyr (prep_data, tables de référence, prep_scenarios2,
# pivots courts, v_admin) sont des blocs DÉPLACÉS tels quels depuis le v8 mono-fichier
# (diff vide attendu bloc à bloc, cf. MODIFICATIONS_V8.md « industrialisation »). Les
# compute(temporary = TRUE) restent temporaires : rien n'est persisté en base.
#
# Déroulé : 0. bootstrap  1. résolution des besoins  2. prep_data (déf.)  3. tables de
# référence (déf.)  4. prep_scenarios2 (déf.)  5. exécution : prep_data pour les seules
# années nécessaires, refs manquantes, catalogue longs par partiels (checkpoint, cache
# inter-profils, instrumentation)  6. agrégation + seuil + exports (agrégats uniquement,
# JAMAIS de niveau séjour).
###############################################################################

## ---- 0. Bootstrap : config, sources, connexion ----
PATH_PROJET <- Sys.getenv("SCENARIOS_PMSI_PATH",
                          unset = "~/commun/projets_communs/DIM_siege/divers_projets/Scenario_crh_fictifs/")
source(file.path(PATH_PROJET, "config_v8.R"))

source(file.path(PATH_PROJET, "utils.R"))
path_projet <- PATH_PROJET     # alias attendu par referentiels.R (et write_xlsx de utils.R)
outfile     <- PATH_RESULTS    # alias historique

conn <- pRatihque::connection_database()

source(file.path(PATH_PROJET, "exclusions.R"))
source(file.path(PATH_PROJET, "referentiels.R"))   # définit neo_codes_diabete, codes_diab, hta_autres, cim, ...
source(file.path(PATH_PROJET, "helpers_v8.R"))

for(d in c(PATH_RESULTS, EXPORTS_DIR, PARTIELS_DIR)) if(!dir.exists(d)) dir.create(d, recursive = TRUE)

cat("PROFIL = ", PROFIL, " ; AN_REF = ", AN_REF, " ; ANS_HISTORIQUE = ", paste(ANS_HISTORIQUE, collapse = ","),
    " ; TYPES_ETBS_LONGS = ", paste(TYPES_ETBS_LONGS, collapse = ","), "\n", sep = "")
cat("EXPORTS_DIR = ", EXPORTS_DIR, "\nPARTIELS_DIR = ", PARTIELS_DIR, "\n", sep = "")

## ---- 1. Résolution des besoins (avant toute requête) ----
# Garde-fou des partiels : ils dépendent de K_GRAINE_LONGS et de la logique amont, pas du
# seuil ni du périmètre d'années (cf. RUN.md, règles de cache).
FICHIER_PARTIELS_META <- file.path(PARTIELS_DIR, "partiels_meta.yaml")
meta_partiels <- meta_partiels_courant(K_GRAINE_LONGS, NBDA_MAX, DUREE_LONGS, PIVOTS_LONGS, VERSION_SCRIPT)
if(file.exists(FICHIER_PARTIELS_META)){
  verif <- verifier_partiels_meta(yaml::read_yaml(FICHIER_PARTIELS_META), meta_partiels)
  if(!is.null(verif$erreur)) stop(verif$erreur)
  for(a in verif$avertissements) warning(a, call. = FALSE)
} else {
  yaml::write_yaml(meta_partiels, FICHIER_PARTIELS_META)
}

plan <- resoudre_besoins(TYPES_ETBS_LONGS, ANS_HISTORIQUE, AN_REF,
                         fichiers_partiels = list.files(PARTIELS_DIR, pattern = "^catalogue_partiel_.*\\.parquet$"),
                         fichiers_exports  = list.files(EXPORTS_DIR, pattern = "\\.parquet$"),
                         forcer_refs = FORCER_REFS, noms_refs = NOMS_REFS, refs_chroniques = REFS_CHRONIQUES)
imprimer_plan(plan)

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

## ---- 4. prep_scenarios2 (définition) ----
# Source : v7.2 l.278-327 (bloc B8 de MODIFICATIONS_V8.md), déplacé tel quel.
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

## ---- 5. Exécution ----
# 5a. prep_data UNIQUEMENT pour les années nécessaires (itérations manquantes + AN_REF si une
#     ref manque) ; prep_das_chronique UNIQUEMENT si une ref chronique manque. Toute table
#     temporaire créée ici n'est consommée que par un produit dont l'absence l'a exigée.
for(an_ in plan$annees_a_preparer){
  cat("prep_data(", an_, ")\n", sep = "")
  prep_data(an_)
  gc()
}
if(plan$prep_das_chronique) prep_das_chronique(AN_REF)

# 5b. Tables de référence : chacune SAUTÉE si son parquet existe (sauf FORCER_REFS).
#     pivots_courts / v_admin_* : chaînes v7.1.2 l.219-221, l.232-234 / v7.2 l.551-553
#     (blocs B6, B7, B10), déplacées telles quelles dans des fabriques sans argument.
#     Conversion E669 -> E660 (CONVERSION_E669) : post-collect, dans les fabriques, JAMAIS dans
#     les chaînes. Ordre impératif : conversion -> ré-agrégation -> seuil. La distribution E660x
#     de référence est calculée sur les comptes BRUTS de ref_das_chronique (avant sa conversion)
#     et exportée (distribution_e660.parquet) pour les autres refs et le catalogue.
CACHE_E669 <- new.env()
charger_dist_e660 <- function(){
  f <- file.path(EXPORTS_DIR, "distribution_e660.parquet")
  if(!file.exists(f)) stop("distribution_e660.parquet absent de " %+% EXPORTS_DIR %+% " (calculé avec ref_das_chronique ; FORCER_REFS <- TRUE)")
  arrow::read_parquet(f)
}
ref_das_chronique_brute <- function(){
  if(!exists("brute", envir = CACHE_E669)) assign("brute", ref_das_chronique(AN_REF), envir = CACHE_E669)
  get("brute", envir = CACHE_E669)
}
codes_imprecis_extraction <- function(){
  codes <- codes_imprecis_de_cim(cim, MOTIF_IMPRECIS)
  if(CONVERSION_E669) codes <- codes[!grepl("^E669", codes)]   # E669 traité en amont par la conversion
  codes
}
fabrique_pivots_courts <- function(){
  df_cases_courts <- pRatihque::atihble(conn, 'prep_data_' %+% AN_REF ) |>
    dplyr::filter(duree%in%DUREE_COURTS) |> dplyr::summarise(nb=dplyr::n(),.by=dplyr::all_of(PIVOTS_COURTS)) |> dplyr::filter(nb>SEUIL_PIVOT) |> 
    dplyr::collect()
  df_cases_courts
}
fabrique_v_admin_courts <- function(){
  df_v_admin_courts <- pRatihque::atihble(conn, 'prep_data_' %+% AN_REF ) |> 
    dplyr::distinct(mode_hospit,mode_entree,mode_sortie,sexe,cage,ghm2,diag2,mdp,duree) |> 
    dplyr::collect()
  df_v_admin_courts
}
fabrique_v_admin_longs <- function(){
  df_v_admin_longs <- pRatihque::atihble(conn, 'prep_data_' %+% AN_REF ) |> 
    dplyr::distinct(mode_hospit,mode_entree,mode_sortie,sexe,age,cage,ghm2,diag2,mdp,nbda,duree) |> 
    dplyr::collect()
  df_v_admin_longs
}
FABRIQUES_REFS <- list(
  ref_das_chronique = function(){
    brute <- ref_das_chronique_brute()
    if(!CONVERSION_E669) return(brute)
    dist <- distribution_e660(brute, "das", "nb_das")
    brute |>
      convertir_e669_comptes("diag2", c("das", "sexe", "cage", "niveau", "type_liste", "caract"), "nb_das", dist, BARE_E669_DEFAUT) |>
      convertir_e669_comptes("das", c("diag2", "sexe", "cage", "niveau", "type_liste", "caract"), "nb_das", dist, BARE_E669_DEFAUT)
  },
  distribution_e660 = function() distribution_e660(ref_das_chronique_brute(), "das", "nb_das"),
  ref_das_aigu = function(){
    df <- ref_das_aigu(AN_REF)
    if(!CONVERSION_E669) return(df)
    dist <- charger_dist_e660()
    df |>
      convertir_e669_comptes("diag2", c("mode_hospit", "sexe", "cage", "racine", "ghm2", "das"), "n", dist, BARE_E669_DEFAUT) |>
      convertir_e669_comptes("das", c("mode_hospit", "sexe", "cage", "racine", "ghm2", "diag2"), "n", dist, BARE_E669_DEFAUT)
  },
  ref_nb_chroniques = function() ref_nb_chroniques(AN_REF),                 # aucun code : intact
  ref_comp_diabete  = function() ref_comp_diabete(AN_REF),                  # effectifs bruts ; pénalisation .9 côté tirage
  pivots_courts = function(){
    # Le seuil nb > SEUIL_PIVOT est appliqué EN BASE par la chaîne v7.1.2 (non modifiée) :
    # conversion + ré-agrégation sur le collecté, sans re-seuil (perte conservatrice : classes
    # E669 sous le seuil individuellement ne sont jamais vues ; cf. MODIFICATIONS_V8.md §12).
    df <- fabrique_pivots_courts()
    if(!CONVERSION_E669) return(df)
    convertir_e669_comptes(df, "diag2", setdiff(PIVOTS_COURTS, "diag2"), "nb", charger_dist_e660(), BARE_E669_DEFAUT)
  },
  v_admin_courts = function(){
    df <- fabrique_v_admin_courts()
    if(!CONVERSION_E669) return(df)
    convertir_e669_distinct(df, "diag2", charger_dist_e660(), BARE_E669_DEFAUT)
  },
  v_admin_longs = function(){
    df <- fabrique_v_admin_longs()
    if(!CONVERSION_E669) return(df)
    convertir_e669_distinct(df, "diag2", charger_dist_e660(), BARE_E669_DEFAUT)
  },
  referentiel_substitution_imprecis = function(){
    codes_imprecis <- codes_imprecis_extraction()
    df <- ref_substitution_imprecis(AN_REF, codes_imprecis)
    if(!CONVERSION_E669) return(df)
    # niveau conservé comme attribut du code brut (strate) ; imprecis recalculé après conversion
    df |>
      dplyr::select(-imprecis) |>
      convertir_e669_comptes("code", c("cat", "cage", "sexe", "niveau"), "nb", charger_dist_e660(), BARE_E669_DEFAUT) |>
      dplyr::mutate(imprecis = code %in% codes_imprecis) |>
      dplyr::arrange(cat, code, cage, sexe)
  },
  referentiel_paires_chroniques = function(){
    df <- ref_paires_chroniques(AN_REF)
    if(!CONVERSION_E669) return(df)
    dist <- charger_dist_e660()
    df |>
      convertir_e669_comptes("das_a", c("das_b", "cage", "sexe"), "nb", dist, BARE_E669_DEFAUT) |>
      convertir_e669_comptes("das_b", c("das_a", "cage", "sexe"), "nb", dist, BARE_E669_DEFAUT) |>
      dplyr::mutate(a = pmin(das_a, das_b), b = pmax(das_a, das_b)) |>
      dplyr::filter(a != b) |>
      dplyr::select(-das_a, -das_b) |> dplyr::rename(das_a = a, das_b = b) |>
      reagreger(c("das_a", "das_b", "cage", "sexe"), "nb") |>
      dplyr::filter(nb >= SEUIL_REF_PAIRES)
  }
)
stopifnot(setequal(names(FABRIQUES_REFS), NOMS_REFS))
for(i in seq_len(nrow(plan$refs))){
  nom <- plan$refs$nom[i]
  fichier <- file.path(EXPORTS_DIR, plan$refs$fichier[i])
  if(!plan$refs$a_faire[i]){ cat("ref ", nom, " : présente, sautée\n", sep = ""); next }
  cat("ref ", nom, " : calcul\n", sep = "")
  df_ref <- FABRIQUES_REFS[[nom]]()
  arrow::write_parquet(df_ref, fichier)
  cat("  -> ", nrow(df_ref), " lignes -> ", fichier, "\n", sep = "")
  rm(df_ref); gc()
}

# 5c. Catalogue longs par partiels : chaque itération (etbs, an) écrit
#     PARTIELS_DIR/catalogue_partiel_<etbs>_<an>.parquet et est sautée si le fichier existe.
#     Chaîne base de l'itération (prep_scenarios2) inchangée ; seul l'enrobage R bouge.
#     Instrumentation : apports marginaux imprimés + diagnostic_apports.csv (EXPORTS_DIR).
construire_catalogue_longs <- function(plan){
  df_cases <- NULL
  apports <- NULL
  it <- plan$iterations
  for(i in seq_len(nrow(it))){
    type_etbs <- it$etbs[i]; an_ <- it$an[i]
    fichier <- file.path(PARTIELS_DIR, it$fichier[i])
    if(file.exists(fichier)){
      df_cases_tmp <- arrow::read_parquet(fichier)
      statut <- "relu"
    } else {
      df_cases_tmp<-prep_scenarios2(an_,type_etbs,DUREE_LONGS,NBDA_MAX,K_GRAINE_LONGS,PIVOTS_LONGS)
      gc()
      arrow::write_parquet(df_cases_tmp, fichier)
      statut <- "calculé"
    }
    diag2_avant <- if(is.null(df_cases)) character(0) else unique(df_cases$diag2)
    df_cases <- dplyr::bind_rows(df_cases,df_cases_tmp) |> 
      dplyr::summarise(n =sum(n),.by=dplyr::all_of(c(PIVOTS_LONGS,"diagnostic_associes")))
    ligne <- apports_iteration(type_etbs, an_, statut, df_cases_tmp, df_cases, diag2_avant)
    apports <- rbind(apports, ligne)
    cat(sprintf("  %-6s %s [%-7s] partiel = %8d lignes ; cumul = %9d lignes ; diag2 cumul = %5d (+%d nouveaux)\n",
                type_etbs, an_, statut, ligne$nb_lignes_partiel, ligne$nb_lignes_cumul, ligne$nb_diag2_cumul, ligne$nb_diag2_nouveaux))
    rm(df_cases_tmp)
  }
  utils::write.csv(apports, file.path(EXPORTS_DIR, "diagnostic_apports.csv"), row.names = FALSE)
  df_cases
}

cat("== Catalogue longs (partiels) ==\n")
df_prep_scenarios <- construire_catalogue_longs(plan)
cat("- Nombre de lignes catalogue brut (df_prep_scenarios) = ", nrow(df_prep_scenarios), "\n", sep = "")

# 5d. Conversion E669 -> E660 du catalogue agrégé (partiels relus en codes BRUTS) : diag2 puis
#     graines, ré-agrégation sum(n), PUIS seuil (section 6). Mesure d'impact avant/après.
impact_e669 <- NULL
if(CONVERSION_E669){
  dist_e660 <- charger_dist_e660()
  avant <- df_prep_scenarios
  df_prep_scenarios <- df_prep_scenarios |>
    convertir_e669_comptes("diag2", c(setdiff(PIVOTS_LONGS, "diag2"), "diagnostic_associes"), "n", dist_e660, BARE_E669_DEFAUT) |>
    convertir_e669_combo("diagnostic_associes", PIVOTS_LONGS, "n", dist_e660, BARE_E669_DEFAUT)
  impact_e669 <- impact_conversion_catalogue(avant, df_prep_scenarios, PIVOTS_LONGS_SEUIL, SEUIL_PIVOT, "n")
  # niveau CMA : mesurable seulement si ref_das_chronique a été calculée dans cette exécution (comptes bruts en cache)
  impact_e669$niveau_cma <- if(exists("brute", envir = CACHE_E669)) impact_niveau_cma(get("brute", envir = CACHE_E669), "das", "niveau", "nb_das") else
    list(effectif_e669 = "non mesuré (ref_das_chronique relue, déjà convertie)", niveau_change = NA, cible_inconnue = NA)
  impact_e669$e660_resultant <- effectifs_e660(df_prep_scenarios |> dplyr::mutate(n = as.integer(n)), c("diag2", "diagnostic_associes"))
  rm(avant)
  cat("- Conversion E669 : ", impact_e669$lignes_avant, " -> ", impact_e669$lignes_apres, " lignes (", impact_e669$lignes_fusionnees,
      " fusionnées) ; sum(n) ", impact_e669$n_total_avant, " -> ", impact_e669$n_total_apres, "\n", sep = "")
}

## ---- 6. Agrégation + seuil + exports ----
# Seuil de divulgation au niveau des pivots (v7.2 l.526-530 ; §2.2 : nb > SEUIL_PIVOT,
# le n par combinaison est sommé puis abandonné). Relit UNIQUEMENT les partiels du profil
# courant (ANS_HISTORIQUE × TYPES_ETBS_LONGS) : la production ré-agrège sans requête base.
df_prep_scenarios_seuil <- df_prep_scenarios |>  dplyr::inner_join(df_prep_scenarios |> 
                                                                      dplyr::summarise(nb=sum(n),
                                                                                       .by =dplyr::all_of(PIVOTS_LONGS_SEUIL) )  ) |> 
  dplyr::filter(nb>SEUIL_PIVOT) |> dplyr::select(-n) |>
  dplyr::rename(poids = nb)
rm(df_prep_scenarios); gc()
cat("- Nombre de lignes catalogue éligible (df_prep_scenarios_seuil) = ", nrow(df_prep_scenarios_seuil), "\n", sep = "")

arrow::write_parquet(df_prep_scenarios_seuil, file.path(EXPORTS_DIR, "catalogue_longs_seuil.parquet"))
meta_catalogue <- c(list(produit = "catalogue_longs_seuil", date = as.character(Sys.Date()),
                         nb_lignes = nrow(df_prep_scenarios_seuil),
                         nb_diag2_distincts = length(unique(df_prep_scenarios_seuil$diag2)),
                         plan_annees_preparees = as.list(plan$annees_a_preparer),
                         plan_iterations_calculees = sum(plan$iterations$a_faire),
                         plan_refs_calculees = sum(plan$refs$a_faire),
                         conversion_e669_lignes_fusionnees = if(is.null(impact_e669)) NA else impact_e669$lignes_fusionnees,
                         conversion_e669_profils_entres = if(is.null(impact_e669)) NA else impact_e669$profils_entres),
                    valeurs_effectives_config())
yaml::write_yaml(meta_catalogue, file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml"))

# Rapport d'extraction : plan, apports, mesure d'impact de la conversion E669 (§6 du chantier)
fmt_df <- function(d) if(is.null(d) || nrow(d) == 0) "   (vide)" else "   " %+% utils::capture.output(print(as.data.frame(d), row.names = FALSE))
lignes_rx <- c("RAPPORT D'EXTRACTION — extraction_associations_codes_v8.R — " %+% DATE_TAG %+% " — PROFIL = " %+% PROFIL,
               "", "== 1. Plan ==", utils::capture.output(imprimer_plan(plan)),
               "", "== 2. Catalogue ==",
               sprintf("catalogue éligible : %d lignes ; %d diag2 distincts ; SEUIL_PIVOT = %s", nrow(df_prep_scenarios_seuil), length(unique(df_prep_scenarios_seuil$diag2)), SEUIL_PIVOT),
               "", "== 3. Conversion E669 -> E660 (CONVERSION_E669 = " %+% CONVERSION_E669 %+% ") ==")
if(CONVERSION_E669){
  lignes_rx <- c(lignes_rx,
    "-- distribution E660x de référence (comptes bruts de ref_das_chronique, AN_REF) :", fmt_df(dist_e660),
    "-- catalogue agrégé (avant seuil) :",
    sprintf("   effectif E669 converti : diag2 suffixé = %s ; diag2 nu = %s ; graine suffixé = %s ; graine nu = %s",
            impact_e669$e669_diag2_suffixe, impact_e669$e669_diag2_nu, impact_e669$e669_graine_suffixe, impact_e669$e669_graine_nu),
    sprintf("   sum(n) avant = %s ; après = %s (conservé : %s)", impact_e669$n_total_avant, impact_e669$n_total_apres, impact_e669$n_total_avant == impact_e669$n_total_apres),
    sprintf("   lignes avant = %d ; après = %d ; fusionnées = %d", impact_e669$lignes_avant, impact_e669$lignes_apres, impact_e669$lignes_fusionnees),
    sprintf("   profils (pivots) > seuil : avant = %d ; après = %d ; ENTRÉS par fusion = %d ; sortis = %d  [écart de volumétrie assumé par doctrine]",
            impact_e669$profils_seuil_avant, impact_e669$profils_seuil_apres, impact_e669$profils_entres, impact_e669$profils_sortis),
    sprintf("   niveau CMA (ref_das_chronique brute) : effectif E669x = %s ; conversions changeant le niveau = %s ; cible E660x non observée = %s",
            impact_e669$niveau_cma$effectif_e669, impact_e669$niveau_cma$niveau_change, impact_e669$niveau_cma$cible_inconnue),
    "-- distribution E660x résultante dans le catalogue (diag2 + graines, en lignes) :", fmt_df(impact_e669$e660_resultant))
}
FICHIER_RAPPORT_EXTRACTION <- file.path(EXPORTS_DIR, "rapport_extraction_v8_" %+% DATE_TAG %+% ".txt")
writeLines(lignes_rx, FICHIER_RAPPORT_EXTRACTION)
cat("Rapport d'extraction : ", FICHIER_RAPPORT_EXTRACTION, "\n", sep = "")
cat("Exports écrits dans ", EXPORTS_DIR, " : catalogue_longs_seuil.parquet (+ meta.yaml), diagnostic_apports.csv, refs.\n", sep = "")
cat("Extraction terminée. Étape suivante : tirage_scenarios_v8.R (aucune connexion base).\n")
