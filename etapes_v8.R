###############################################################################
# etapes_v8.R — ORCHESTRATION PAR ÉTAPES du pipeline scenarios_bn_pmsi v8
#
# Sourcé par extraction_associations_codes_v8.R et tirage_scenarios_v8.R (après config,
# utils, exclusions, referentiels, helpers), et par RUN.Rmd. Contenu :
#   0. Chaînes base (définitions DÉPLACÉES telles quelles depuis le script d'extraction :
#      prep_data, tables de référence, prep_scenarios2, fabriques ; diff vide, voir
#      MODIFICATIONS_V8.md section 14). Aucune chaîne n'est exécutée au sourçage.
#   1. Infrastructure : bannières, prérequis, environnement de session ETAPES_ENV.
#   2. Famille EXTRACTION (exige `conn`) : etape_prep_data, etape_refs, etape_partiels_longs,
#      etape_catalogue (cette dernière ne touche pas la base : partiels + refs parquet).
#   3. Famille TIRAGE (aucun appel pRatihque) : etape_tirage_courts, etape_selection_longs,
#      etape_tirage_das_longs, etape_habillage_longs, etape_finalisation.
#   4. etat_pipeline() : tableau de bord FAIT / PARTIEL / À FAIRE, fichiers seulement.
# Chaque étape : bannière début/fin avec durée ; IDEMPOTENTE (saute ce qui existe selon les
# règles de cache, réaffiche ce qui est sauté) ; décisions en arguments explicites avec la
# config en défaut ; retourne (invisible) les fichiers produits ; vérifie ses prérequis et
# échoue avec un message actionnable si appelée hors ordre.
###############################################################################

## ---- 0. Chaînes base (définitions déplacées telles quelles) ----

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
# Source : v7.2 l.278-327 (bloc B8 de MODIFICATIONS_V8.md). ÉCART P1 (chantier mémoire 15 GiB,
# MODIFICATIONS_V8.md section 13) : la chaîne ne collecte plus le grain séjour × DAS. Le top-k
# par séjour (tri desc(niveau), desc(nb_das), das — §5.9 tiebreak compris) est calculé EN BASE
# par fenêtres (COUNT/ROW_NUMBER OVER, dialecte déjà validé par prep_data), matérialisé dans une
# table temporaire UNIQUE `prep_topk_tmp` écrasée à chaque itération (jamais d'empilement, pas
# de DROP nécessaire : temporaire de session, jamais de parquet pour ce grain), puis collectée
# en colonnes étroites, par morceaux de cage (collect_par_morceaux). Le collapse en graine est
# vectorisé en R (collapse_graine). Équivalence avec l'ancienne version prouvée par
# tests/test_chaines_sqlite.R (§P1.5) : les partiels antérieurs restent valides.
#----------------------------- Prépa DAS  -------------------------------#
prep_scenarios2<-function(an,type_etbs,nb_journees_aut,nbda_aut,nb_assoc_das,pivots,collect_par_morceaux = TRUE,noter = NULL){
  
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
    
    dplyr::mutate(niveau = ifelse(is.na(niveau), "0", niveau)) |>                                   # écart P1 (ex post-collect)
    dplyr::mutate(nb_das = dplyr::n(), .by = dplyr::all_of(c(pivots, "das"))) |>                    # écart P1 (ex post-collect ; COUNT OVER)
    dplyr::group_by(ident) |>                                                                       # écart P1
    dbplyr::window_order(desc(niveau), desc(nb_das), das) |>                                        # écart P1 (= arrange §5.9 ; `desc` non namespacé : dbplyr ne traduit pas `dplyr::desc` dans window_order)
    dplyr::filter(dplyr::row_number() <= nb_assoc_das) |>                                           # écart P1 (= slice(1:k) ; ROW_NUMBER OVER)
    dplyr::ungroup() |>                                                                             # écart P1
    dplyr::select(dplyr::all_of(c("ident", pivots, "das"))) |>                                      # écart P1 : colonnes étroites
    dplyr::compute("prep_topk_tmp", temporary = TRUE, overwrite = TRUE)                            # écart P1 : table temporaire unique, écrasée
  
  # COLLECT minimal depuis prep_topk_tmp (k lignes max par séjour). Par morceaux de cage : filtre
  # en lecture seule sur la table figée (aucun recalcul des fenêtres), chaque morceau collapsé
  # puis libéré ; les df_cases partiels (agrégés, petits) sont concaténés — leurs clés sont
  # disjointes car cage est un pivot et un ident n'a qu'une cage (jamais coupé par le morcelage).
  topk <- pRatihque::atihble(conn, "prep_topk_tmp")
  if(isTRUE(collect_par_morceaux)){
    cages <- topk |> dplyr::distinct(cage) |> dplyr::collect() |> dplyr::pull(cage)
    df_cases <- NULL
    for(cg in cages){
      morceau <- if(is.na(cg)) topk |> dplyr::filter(is.na(cage)) |> dplyr::collect() else topk |> dplyr::filter(cage == cg) |> dplyr::collect()
      if(!is.null(noter)) noter("prep_scenarios2 " %+% type_etbs %+% " " %+% an %+% " morceau " %+% cg, morceau)
      df_cases <- dplyr::bind_rows(df_cases, collapse_graine(morceau, pivots, nb_assoc_das))
      rm(morceau); gc()
    }
    if(is.null(df_cases)) df_cases <- collapse_graine(topk |> dplyr::collect(), pivots, nb_assoc_das)   # itération sans séjour
  } else {
    df_das <- topk |> dplyr::collect()
    if(!is.null(noter)) noter("prep_scenarios2 " %+% type_etbs %+% " " %+% an %+% " collect unique", df_das)
    df_cases <- collapse_graine(df_das, pivots, nb_assoc_das)
    rm(df_das); gc()
  }
  
  return(df_cases)
  
  
}

## ---- 0b. Infrastructure de l'extraction, fabriques de refs (déplacées telles quelles) ----
CACHE_E669 <- new.env()
# Instrumentation mémoire (chantier 15 GiB) : journal accumulé dans un environnement (pas de
# `<<-`), exporté en diagnostic_memoire.csv. noter_memoire() après chaque collect notable.
MEMOIRE_ENV <- new.env(); assign("journal", NULL, envir = MEMOIRE_ENV)
noter_memoire <- function(etiquette, objet = NULL){
  assign("journal", mesurer_memoire(etiquette, objet, get("journal", envir = MEMOIRE_ENV), SEUIL_ALERTE_GO), envir = MEMOIRE_ENV)
  invisible(NULL)
}
purger_cache_e669 <- function(){ if(exists("brute", envir = CACHE_E669)) rm("brute", envir = CACHE_E669); gc(); invisible(NULL) }
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
  distribution_e660 = function(){
    brute <- ref_das_chronique_brute()
    dist <- distribution_e660(brute, "das", "nb_das")
    assign("niveau_cma", impact_niveau_cma(brute, "das", "niveau", "nb_das"), envir = CACHE_E669)   # mesure §6 conservée (petite)
    rm(brute); purger_cache_e669()                                                                   # P3.2 : comptes bruts libérés aussitôt
    dist
  },
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

## ---- 1. Infrastructure des étapes ----
ETAPES_ENV <- new.env()   # état de session : plan, impact_e669, rapport, revue, sélection, df longs

banniere_debut <- function(nom, detail = ""){
  cat("\n==== [", nom, "] début ", format(Sys.time(), "%H:%M:%S"), if(nzchar(detail)) " — " %+% detail else "", "\n", sep = "")
  Sys.time()
}
banniere_fin <- function(nom, t0, fichiers = character(0)){
  d <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat("==== [", nom, "] fin — durée ", sprintf("%.1f s", d), " — ", length(fichiers), " fichier(s) produit(s)",
      if(length(fichiers)) " : " %+% paste(basename(fichiers), collapse = ", ") else "", "\n", sep = "")
  invisible(fichiers)
}
exiger_conn <- function(etape){
  if(!exists("conn", envir = globalenv()) || is.null(get("conn", envir = globalenv())))
    stop(etape %+% " : connexion base absente. Exécuter d'abord : conn <- pRatihque::connection_database() (famille extraction).", call. = FALSE)
  invisible(TRUE)
}
exiger_fichiers <- function(fichiers, etape, etape_amont){
  manquants <- fichiers[!file.exists(fichiers)]
  if(length(manquants) > 0)
    stop(etape %+% " : fichier(s) manquant(s) : " %+% paste(basename(manquants), collapse = ", ") %+% ". Lancez " %+% etape_amont %+% " d'abord.", call. = FALSE)
  invisible(TRUE)
}
table_temporaire_existe <- function(nom){
  ok <- tryCatch({ pRatihque::atihble(get("conn", envir = globalenv()), nom); TRUE }, error = function(e) FALSE)
  isTRUE(ok)
}
exiger_table <- function(nom, etape, conseil){
  if(!table_temporaire_existe(nom)) stop(etape %+% " : table temporaire `" %+% nom %+% "` absente de cette session. Lancez " %+% conseil %+% " d'abord.", call. = FALSE)
  invisible(TRUE)
}
fmt_df <- function(d) if(is.null(d) || nrow(d) == 0) "   (vide)" else "   " %+% utils::capture.output(print(as.data.frame(d), row.names = FALSE))
chemin_export <- function(nom) file.path(EXPORTS_DIR, nom %+% "_v8_" %+% DATE_TAG %+% ".parquet")
FICHIER_PARTIELS_META <- function() file.path(PARTIELS_DIR, "partiels_meta.yaml")
plan_courant <- function(forcer_refs = FORCER_REFS){
  for(d in c(PATH_RESULTS, EXPORTS_DIR, PARTIELS_DIR)) if(!dir.exists(d)) dir.create(d, recursive = TRUE)
  resoudre_besoins(TYPES_ETBS_LONGS, ANS_HISTORIQUE, AN_REF,
                   fichiers_partiels = list.files(PARTIELS_DIR, pattern = "^catalogue_partiel_.*\\.parquet$"),
                   fichiers_exports  = list.files(EXPORTS_DIR, pattern = "\\.parquet$"),
                   forcer_refs = forcer_refs, noms_refs = NOMS_REFS, refs_chroniques = REFS_CHRONIQUES)
}

## ---- 2. Famille EXTRACTION ----

# Étape 1 — résolution des besoins puis prep_data des années nécessaires (ans = NULL -> déduites
# du plan ; sinon forcées) ; prep_das_chronique(AN_REF) si une ref chronique manque. Garde-fou
# partiels_meta.yaml. Tables temporaires uniquement : rien n'est persisté en base.
etape_prep_data <- function(ans = NULL){
  t0 <- banniere_debut("etape_prep_data", "PROFIL = " %+% PROFIL %+% " ; AN_REF = " %+% AN_REF)
  exiger_conn("etape_prep_data")
  meta_partiels <- meta_partiels_courant(K_GRAINE_LONGS, NBDA_MAX, DUREE_LONGS, PIVOTS_LONGS, VERSION_SCRIPT)
  plan <- plan_courant()
  if(file.exists(FICHIER_PARTIELS_META())){
    verif <- verifier_partiels_meta(yaml::read_yaml(FICHIER_PARTIELS_META()), meta_partiels)
    if(!is.null(verif$erreur)) stop(verif$erreur)
    for(a in verif$avertissements) warning(a, call. = FALSE)
  } else {
    yaml::write_yaml(meta_partiels, FICHIER_PARTIELS_META())
  }
  imprimer_plan(plan)
  assign("plan", plan, envir = ETAPES_ENV)
  annees <- if(is.null(ans)) plan$annees_a_preparer else as.integer(ans)
  if(!is.null(ans)) cat("Années forcées par l'argument ans : ", paste(annees, collapse = ", "), "\n", sep = "")
  for(an_ in annees){
    cat("prep_data(", an_, ")\n", sep = "")
    prep_data(an_)
    gc()
  }
  if(plan$prep_das_chronique){ cat("prep_das_chronique(", AN_REF, ")\n", sep = ""); prep_das_chronique(AN_REF) }
  banniere_fin("etape_prep_data", t0, FICHIER_PARTIELS_META())
}

# Étape 2 — tables de référence : chaque fabrique, export parquet, libération (boucle déplacée
# telle quelle) ; sautée si son parquet existe (sauf forcer). Prérequis : prep_data_<AN_REF>.
etape_refs <- function(forcer = FORCER_REFS){
  t0 <- banniere_debut("etape_refs", "forcer = " %+% forcer %+% " ; EXPORTS_DIR = " %+% EXPORTS_DIR)
  plan <- plan_courant(forcer_refs = forcer)
  fichiers <- character(0)
  if(any(plan$refs$a_faire)){
    exiger_conn("etape_refs")
    exiger_table("prep_data_" %+% AN_REF, "etape_refs", "etape_prep_data()")
    if(any(plan$refs$a_faire & plan$refs$nom %in% REFS_CHRONIQUES) && !table_temporaire_existe("prep_das_chro_" %+% AN_REF)){
      cat("prep_das_chronique(", AN_REF, ") (ref chronique à calculer)\n", sep = ""); prep_das_chronique(AN_REF)
    }
  }
  for(i in seq_len(nrow(plan$refs))){
    nom <- plan$refs$nom[i]
    fichier <- file.path(EXPORTS_DIR, plan$refs$fichier[i])
    if(!plan$refs$a_faire[i]){ cat("ref ", nom, " : présente, sautée\n", sep = ""); next }
    cat("ref ", nom, " : calcul\n", sep = "")
    df_ref <- FABRIQUES_REFS[[nom]]()
    arrow::write_parquet(df_ref, fichier)
    cat("  -> ", nrow(df_ref), " lignes -> ", fichier, "\n", sep = "")
    noter_memoire("ref " %+% nom, df_ref)
    rm(df_ref); gc()   # P3.1 : aucune ref ne reste liée à l'environnement global après son écriture
    fichiers <- c(fichiers, fichier)
  }
  purger_cache_e669()  # P3.2 : au cas où ref_das_chronique a été calculée sans distribution_e660
  banniere_fin("etape_refs", t0, fichiers)
}

# Étape 3 — partiels des séjours longs : calcul/écriture des partiels manquants (boucle sans
# accumulateur, stats du partiel seul -> diagnostic_apports.csv) + recouvrement.csv. SANS
# agrégation finale. iterations = NULL -> plan complet du profil ; sinon data.frame(etbs, an).
etape_partiels_longs <- function(iterations = NULL){
  t0 <- banniere_debut("etape_partiels_longs", "PARTIELS_DIR = " %+% PARTIELS_DIR)
  plan <- plan_courant()
  it <- if(is.null(iterations)) plan$iterations else {
    d <- data.frame(etbs = as.character(iterations$etbs), an = as.integer(iterations$an), stringsAsFactors = FALSE)
    d$fichier <- nom_partiel(d$etbs, d$an); d$a_faire <- !file.exists(file.path(PARTIELS_DIR, d$fichier)); d
  }
  if(any(it$a_faire)){
    exiger_conn("etape_partiels_longs")
    for(an_ in unique(it$an[it$a_faire])) exiger_table("prep_data_" %+% an_, "etape_partiels_longs", "etape_prep_data(ans = " %+% an_ %+% ")")
  }
  apports <- NULL
  diag2_vus <- character(0)
  fichiers <- character(0)
  for(i in seq_len(nrow(it))){
    type_etbs <- it$etbs[i]; an_ <- it$an[i]
    fichier <- file.path(PARTIELS_DIR, it$fichier[i])
    if(file.exists(fichier)){
      df_cases_tmp <- arrow::read_parquet(fichier)
      statut <- "relu"
    } else {
      df_cases_tmp<-prep_scenarios2(an_,type_etbs,DUREE_LONGS,NBDA_MAX,K_GRAINE_LONGS,PIVOTS_LONGS,COLLECT_PAR_MORCEAUX,noter_memoire)
      gc()
      arrow::write_parquet(df_cases_tmp, fichier)
      statut <- "calculé"
      fichiers <- c(fichiers, fichier)
    }
    ligne <- apports_partiel(type_etbs, an_, statut, df_cases_tmp, diag2_vus)
    diag2_vus <- union(diag2_vus, unique(df_cases_tmp$diag2))
    apports <- rbind(apports, ligne)
    cat(sprintf("  %-6s %s [%-7s] partiel = %8d lignes ; sum(n) = %9d séjours ; diag2 = %5d (+%d nouveaux)\n",
                type_etbs, an_, statut, ligne$nb_lignes_partiel, ligne$sum_n_partiel, ligne$nb_diag2_partiel, ligne$nb_diag2_nouveaux))
    noter_memoire("partiel " %+% type_etbs %+% " " %+% an_ %+% " (" %+% statut %+% ")", df_cases_tmp)
    rm(df_cases_tmp); gc()
  }
  f_apports <- file.path(EXPORTS_DIR, "diagnostic_apports.csv")
  utils::write.csv(apports, f_apports, row.names = FALSE)
  cat("== Recouvrement entre partiels ==\n")
  recouvrement <- mesurer_recouvrement(PAIRES_RECOUVREMENT)
  f_rec <- file.path(EXPORTS_DIR, "recouvrement.csv")
  banniere_fin("etape_partiels_longs", t0, c(fichiers, f_apports, if(file.exists(f_rec)) f_rec))
}

mesurer_recouvrement <- function(paires){
  res <- NULL
  for(p in paires){
    etbs <- p[[1]]; anA <- as.integer(p[[2]]); anB <- as.integer(p[[3]])
    fA <- file.path(PARTIELS_DIR, nom_partiel(etbs, anA)); fB <- file.path(PARTIELS_DIR, nom_partiel(etbs, anB))
    if(!file.exists(fA) || !file.exists(fB)){
      res <- dplyr::bind_rows(res, data.frame(etbs = etbs, anA = anA, anB = anB, statut = "non calculable (partiel manquant)"))
      cat(sprintf("  recouvrement %s %s -> %s : non calculable (partiel manquant)\n", etbs, anA, anB)); next
    }
    A <- arrow::read_parquet(fA); B <- arrow::read_parquet(fB)
    r <- recouvrement_partiels(A, B, PIVOTS_LONGS, "diagnostic_associes", "n")
    rm(A, B); gc()
    res <- dplyr::bind_rows(res, cbind(data.frame(etbs = etbs, anA = anA, anB = anB, statut = "ok"), r))
    cat(sprintf("  ajouter %s aux %s : %.1f %% des combinaisons de %s déjà vues en %s (%.1f %% des séjours), %d cas uniques nouveaux ; pivots : %.1f %% déjà vus ; %d diag2 nouveaux\n",
                anB, etbs, 100 * r$part_combos_B_vues, anB, anA, 100 * r$part_sejours_B_vus, r$sejours_B_uniques_nouveaux, 100 * r$part_pivots_B_vus, r$nb_diag2_B_nouveaux))
  }
  if(!is.null(res)) utils::write.csv(res, file.path(EXPORTS_DIR, "recouvrement.csv"), row.names = FALSE)
  res
}

# Étape 4 — catalogue final en deux étages depuis les partiels du PÉRIMÈTRE PASSÉ EN ARGUMENT
# (ans, etbs : la décision de périmètre, tracée dans le meta.yaml), conversion E669, seuil,
# export catalogue_longs_seuil.parquet (+ meta.yaml) et rapport d'extraction. Ne touche pas la
# base. C'est le « fichier parquet sans les DAS ».
etape_catalogue <- function(ans = ANS_HISTORIQUE, etbs = TYPES_ETBS_LONGS){
  t0 <- banniere_debut("etape_catalogue", "périmètre : " %+% paste(etbs, collapse = ",") %+% " × " %+% paste(range(ans), collapse = "-") %+% " ; SEUIL_PIVOT = " %+% SEUIL_PIVOT)
  ans <- as.integer(ans); etbs <- as.character(etbs)
  for(d in c(PATH_RESULTS, EXPORTS_DIR, PARTIELS_DIR)) if(!dir.exists(d)) dir.create(d, recursive = TRUE)
  fichiers_partiels <- file.path(PARTIELS_DIR, nom_partiel(rep(etbs, each = length(ans)), rep(ans, times = length(etbs))))
  exiger_fichiers(fichiers_partiels, "etape_catalogue", "etape_partiels_longs()")
  if(CONVERSION_E669) exiger_fichiers(file.path(EXPORTS_DIR, "distribution_e660.parquet"), "etape_catalogue", "etape_refs()")
  # Étage 1 (petit) : agrégation au niveau PIVOTS (PIVOTS_LONGS_SEUIL), conversion E669 des
  #   comptes pivots, seuil > SEUIL_PIVOT -> pivots retenus (convertis).
  # Étage 2 (borné) : clés pivots BRUTES contribuant à un pivot retenu (identité ; E669 suffixé ->
  #   sa cible ; E669 nu -> retenu si au moins une cible est retenue), semi-jointure sur les
  #   partiels, agrégation (pivots bruts × graine), PUIS pipeline existant conversion -> ré-agrégation
  #   -> seuil RE-APPLIQUÉ exactement (l'étage 2 sur-matérialise légèrement ; le seuil final fait foi,
  #   fusions sous-seuil incluses). Équivalence avec l'ancien flux (bind_rows global) prouvée en test.
  cat("== Catalogue final : étage 1 (pivots) ==\n")
  pivots_bruts <- agreger_partiels(fichiers_partiels, PIVOTS_LONGS_SEUIL, "n")
  noter_memoire("catalogue étage 1 : pivots bruts", pivots_bruts)
  impact_e669 <- NULL
  if(CONVERSION_E669){
    dist_e660 <- charger_dist_e660()
    pivots_convertis <- convertir_e669_comptes(pivots_bruts, "diag2", setdiff(PIVOTS_LONGS_SEUIL, "diag2"), "n", dist_e660, BARE_E669_DEFAUT)
    impact_e669 <- impact_conversion_pivots(pivots_bruts, pivots_convertis, PIVOTS_LONGS_SEUIL, SEUIL_PIVOT, "n")
    impact_e669$niveau_cma <- if(exists("niveau_cma", envir = CACHE_E669)) get("niveau_cma", envir = CACHE_E669) else
      list(effectif_e669 = "non mesuré (distribution_e660 relue, ref_das_chronique non recalculée)", niveau_change = NA, cible_inconnue = NA)
  } else pivots_convertis <- pivots_bruts
  pivots_retenus <- pivots_convertis[pivots_convertis$n > SEUIL_PIVOT, PIVOTS_LONGS_SEUIL, drop = FALSE]
  cat("- pivots bruts = ", nrow(pivots_bruts), " ; pivots convertis = ", nrow(pivots_convertis), " ; retenus (> ", SEUIL_PIVOT, ") = ", nrow(pivots_retenus), "\n", sep = "")

  cat("== Catalogue final : étage 2 (combinaisons des clés retenues) ==\n")
  cles_brutes <- if(CONVERSION_E669) cles_brutes_retenues(pivots_bruts, pivots_retenus, PIVOTS_LONGS_SEUIL, dist_e660, BARE_E669_DEFAUT) else pivots_retenus
  rm(pivots_bruts, pivots_convertis); gc()
  combos <- agreger_partiels(fichiers_partiels, c(PIVOTS_LONGS, "diagnostic_associes"), "n", filtre_cles = cles_brutes)
  noter_memoire("catalogue étage 2 : combinaisons brutes", combos)
  if(CONVERSION_E669){
    e669_graine <- effectif_e669_combos(combos, "diagnostic_associes", "n")
    impact_e669$e669_graine_suffixe <- e669_graine$suffixe; impact_e669$e669_graine_nu <- e669_graine$nu
    impact_e669$lignes_avant <- nrow(combos)
    combos <- combos |>
      convertir_e669_comptes("diag2", c(setdiff(PIVOTS_LONGS, "diag2"), "diagnostic_associes"), "n", dist_e660, BARE_E669_DEFAUT) |>
      convertir_e669_combo("diagnostic_associes", PIVOTS_LONGS, "n", dist_e660, BARE_E669_DEFAUT)
    impact_e669$lignes_apres <- nrow(combos); impact_e669$lignes_fusionnees <- impact_e669$lignes_avant - impact_e669$lignes_apres
    noter_memoire("catalogue étage 2 : combinaisons converties", combos)
  }
  # Seuil de divulgation au niveau des pivots (v7.2 l.526-530 ; §2.2 : nb > SEUIL_PIVOT,
  # le n par combinaison est sommé puis abandonné) — RE-APPLIQUÉ exactement sur l'étage 2.
  df_prep_scenarios_seuil <- combos |>  dplyr::inner_join(combos |> 
                                                            dplyr::summarise(nb=sum(n),
                                                                             .by =dplyr::all_of(PIVOTS_LONGS_SEUIL) )  ) |> 
    dplyr::filter(nb>SEUIL_PIVOT) |> dplyr::select(-n) |>
    dplyr::rename(poids = nb)
  rm(combos, cles_brutes, pivots_retenus); gc()
  if(CONVERSION_E669) impact_e669$e660_resultant <- effectifs_e660(df_prep_scenarios_seuil, c("diag2", "diagnostic_associes"))
  noter_memoire("catalogue final (seuil)", df_prep_scenarios_seuil)
  cat("- Nombre de lignes catalogue éligible (df_prep_scenarios_seuil) = ", nrow(df_prep_scenarios_seuil), "\n", sep = "")
  assign("impact_e669", impact_e669, envir = ETAPES_ENV)

  f_cat <- file.path(EXPORTS_DIR, "catalogue_longs_seuil.parquet")
  arrow::write_parquet(df_prep_scenarios_seuil, f_cat)
  plan <- if(exists("plan", envir = ETAPES_ENV)) get("plan", envir = ETAPES_ENV) else NULL
  meta_catalogue <- c(list(produit = "catalogue_longs_seuil", date = as.character(Sys.Date()),
                           nb_lignes = nrow(df_prep_scenarios_seuil),
                           nb_diag2_distincts = length(unique(df_prep_scenarios_seuil$diag2)),
                           perimetre_ans = as.list(ans), perimetre_etbs = as.list(etbs),
                           plan_annees_preparees = if(is.null(plan)) NA else as.list(plan$annees_a_preparer),
                           plan_iterations_calculees = if(is.null(plan)) NA else sum(plan$iterations$a_faire),
                           plan_refs_calculees = if(is.null(plan)) NA else sum(plan$refs$a_faire),
                           conversion_e669_lignes_fusionnees = if(is.null(impact_e669)) NA else impact_e669$lignes_fusionnees,
                           conversion_e669_profils_entres = if(is.null(impact_e669)) NA else impact_e669$profils_entres),
                      valeurs_effectives_config())
  meta_catalogue$ANS_HISTORIQUE <- as.integer(ans); meta_catalogue$TYPES_ETBS_LONGS <- etbs   # trace de la décision de périmètre
  f_meta <- file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml")
  yaml::write_yaml(meta_catalogue, f_meta)

  # Rapport d'extraction : périmètre, catalogue, impact E669, apports, recouvrement, mémoire
  lire_csv <- function(f) if(file.exists(f)) utils::read.csv(f) else NULL
  lignes_rx <- c("RAPPORT D'EXTRACTION — etape_catalogue — " %+% DATE_TAG %+% " — PROFIL = " %+% PROFIL,
                 "", "== 1. Périmètre ==",
                 "etbs = " %+% paste(etbs, collapse = ", ") %+% " ; ans = " %+% paste(ans, collapse = ", "),
                 "partiels : " %+% paste(basename(fichiers_partiels), collapse = ", "),
                 if(!is.null(plan)) c("plan de la session :", utils::capture.output(imprimer_plan(plan))) else "plan : (etape_prep_data non exécutée dans cette session)",
                 "", "== 2. Catalogue ==",
                 sprintf("catalogue éligible : %d lignes ; %d diag2 distincts ; SEUIL_PIVOT = %s", nrow(df_prep_scenarios_seuil), length(unique(df_prep_scenarios_seuil$diag2)), SEUIL_PIVOT),
                 "", "== 3. Conversion E669 -> E660 (CONVERSION_E669 = " %+% CONVERSION_E669 %+% ") ==")
  if(CONVERSION_E669){
    lignes_rx <- c(lignes_rx,
      "-- distribution E660x de référence (comptes bruts de ref_das_chronique, AN_REF) :", fmt_df(dist_e660),
      "-- catalogue agrégé (avant seuil) :",
      sprintf("   effectif E669 converti : diag2 suffixé = %s ; diag2 nu = %s ; graine suffixé = %s ; graine nu = %s",
              impact_e669$e669_diag2_suffixe, impact_e669$e669_diag2_nu, impact_e669$e669_graine_suffixe, impact_e669$e669_graine_nu),
      sprintf("   sum(n) pivots avant = %s ; après = %s (conservé : %s)", impact_e669$n_total_avant, impact_e669$n_total_apres, impact_e669$n_total_avant == impact_e669$n_total_apres),
      sprintf("   combinaisons (étage 2, clés retenues) avant = %d ; après = %d ; fusionnées = %d", impact_e669$lignes_avant, impact_e669$lignes_apres, impact_e669$lignes_fusionnees),
      sprintf("   profils (pivots) > seuil : avant = %d ; après = %d ; ENTRÉS par fusion = %d ; sortis = %d  [écart de volumétrie assumé par doctrine]",
              impact_e669$profils_seuil_avant, impact_e669$profils_seuil_apres, impact_e669$profils_entres, impact_e669$profils_sortis),
      sprintf("   niveau CMA (ref_das_chronique brute) : effectif E669x = %s ; conversions changeant le niveau = %s ; cible E660x non observée = %s",
              impact_e669$niveau_cma$effectif_e669, impact_e669$niveau_cma$niveau_change, impact_e669$niveau_cma$cible_inconnue),
      "-- distribution E660x résultante dans le catalogue (diag2 + graines, en lignes) :", fmt_df(impact_e669$e660_resultant))
  }
  lignes_rx <- c(lignes_rx, "", "== 4. Apports par partiel (diagnostic_apports.csv) ==", fmt_df(lire_csv(file.path(EXPORTS_DIR, "diagnostic_apports.csv"))),
                 "", "== 5. Recouvrement entre partiels (recouvrement.csv) ==", fmt_df(lire_csv(file.path(EXPORTS_DIR, "recouvrement.csv"))),
                 "", "== 6. Mémoire (diagnostic_memoire.csv ; SEUIL_ALERTE_GO = " %+% SEUIL_ALERTE_GO %+% ") ==", fmt_df(get("journal", envir = MEMOIRE_ENV)))
  f_mem <- file.path(EXPORTS_DIR, "diagnostic_memoire.csv")
  utils::write.csv(get("journal", envir = MEMOIRE_ENV), f_mem, row.names = FALSE)
  f_rx <- file.path(EXPORTS_DIR, "rapport_extraction_v8_" %+% DATE_TAG %+% ".txt")
  writeLines(lignes_rx, f_rx)
  cat("Rapport d'extraction : ", f_rx, "\n", sep = "")
  rm(df_prep_scenarios_seuil); gc()
  banniere_fin("etape_catalogue", t0, c(f_cat, f_meta, f_rx, f_mem))
}

## ---- 3. Famille TIRAGE (aucun appel pRatihque) ----

# Contexte de tirage chargé une fois par session (refs parquet, REFS, libellés) ; `requis` =
# produits à exiger pour l'étape appelante (message listant les manquants).
charger_contexte_tirage <- function(requis, etape){
  exiger_fichiers(file.path(EXPORTS_DIR, requis), etape, "extraction (etape_refs / etape_catalogue)")
  if(!exists("ctx", envir = ETAPES_ENV)){
    if(!dir.exists(CHUNKS_DIR)) dir.create(CHUNKS_DIR, recursive = TRUE)
    f_meta <- file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml")
    meta_catalogue <- if(file.exists(f_meta)) yaml::read_yaml(f_meta) else NULL
    if(!is.null(meta_catalogue) && !identical(meta_catalogue$PROFIL, PROFIL)) warning("Le catalogue a été extrait avec le profil " %+% meta_catalogue$PROFIL %+% ", tirage en profil " %+% PROFIL)
    conv <- if(is.null(meta_catalogue)) CONVERSION_E669 else isTRUE(meta_catalogue$CONVERSION_E669)
    codes_imprecis <- codes_imprecis_de_cim(cim, MOTIF_IMPRECIS)
    if(conv) codes_imprecis <- codes_imprecis[!grepl("^E669", codes_imprecis)]
    paires <- list()
    if(file.exists(PATH_PAIRES_EXCLUES)) paires <- lapply(yaml::read_yaml(PATH_PAIRES_EXCLUES), as.character) else warning("Fichier absent : " %+% PATH_PAIRES_EXCLUES %+% " ; aucune paire exclue.")
    assign("ctx", list(meta_catalogue = meta_catalogue, conversion_e669 = conv, codes_imprecis = codes_imprecis, lib_cim = libelles_cim(cim), paires_exclues = paires), envir = ETAPES_ENV)
  }
  ctx <- get("ctx", envir = ETAPES_ENV)
  lire <- function(nom) arrow::read_parquet(file.path(EXPORTS_DIR, nom_ref(nom)))
  if(is.null(ctx$REFS) && all(nom_ref("ref_comp_diabete") %in% requis)){
    ctx$REFS <- construire_refs(comp_diabete = penaliser_comp_diabete(lire("ref_comp_diabete"), CAGE_PED, CAGE_AGES, PENALITE_9_AGES, PENALITE_9_AUTRES),
                                codes_diab = codes_diab, codes_comp_sat_diab = codes_comp_sat_diab, hta_autres = hta_autres,
                                code_did = code_did, code_dnid_ins = code_dnid_ins, code_dnid = code_dnid,
                                neo_codes = neo_codes_diabete, paires_exclues = ctx$paires_exclues)
    assign("ctx", ctx, envir = ETAPES_ENV)
  }
  ctx
}
etat_tirage <- function(nom, defaut = NULL) if(exists(nom, envir = ETAPES_ENV)) get(nom, envir = ETAPES_ENV) else defaut
poser_tirage <- function(nom, valeur) assign(nom, valeur, envir = ETAPES_ENV)
stats_branche <- function(df, pivots, codes_imprecis, cols_e669){
  list(n = nrow(df), pivots = nrow(dplyr::distinct(df[, pivots])),
       distribution = distribution_nb_das(df), top_das = top_das_par_cmd(df, 30),
       taux_imprecis = taux_imprecis(df, codes_imprecis),
       controles = controler_scenarios(df, hta_autres, SEUIL_PIVOT),
       e669_residuels = compter_e669(df, cols_e669),
       e660 = effectifs_e660(df, c("diag2", "diagnostic_associes")))
}

# Étape T1 — séjours courts : tirage par chunks (AN_REF UNIQUEMENT : les pivots courts sont
# extraits sur l'année de référence), habillage admin depuis v_admin_courts.parquet, contrôles,
# export scenarios_courts. Idempotent : chunks présents sautés ; export réécrit à l'identique.
etape_tirage_courts <- function(){
  t0 <- banniere_debut("etape_tirage_courts", "AN_REF = " %+% AN_REF %+% " uniquement (pivots courts de l'année de référence) ; chunking dynamique (NB_CHUNKS_MAX = " %+% NB_CHUNKS_MAX %+% ", CHUNK_SIZE_MIN = " %+% CHUNK_SIZE_MIN %+% ", CHUNK_SIZE_FIXE = " %+% CHUNK_SIZE_FIXE %+% ")")
  ctx <- charger_contexte_tirage(nom_ref(c("pivots_courts", "ref_das_chronique", "ref_nb_chroniques", "ref_comp_diabete", "v_admin_courts")), "etape_tirage_courts")
  lire <- function(nom) arrow::read_parquet(file.path(EXPORTS_DIR, nom_ref(nom)))
  df_pivots_courts <- lire("pivots_courts"); df_nb_chroniques <- lire("ref_nb_chroniques")
  ref_chro <- prep_ref_chronique(lire("ref_das_chronique")); df_v_admin_courts <- lire("v_admin_courts")
  cat("== Séjours courts : tirage par chunks (", nrow(df_pivots_courts), " pivots) ==\n", sep = "")
  df_tirage <- pmap_chunks(df_pivots_courts[, c(PIVOTS_COURTS, "nb")], sample_das_court,
                           chunk_size = NULL, dossier = CHUNKS_DIR, prefixe = "courts", seed_base = SEED,
                           garder_chunks = GARDER_CHUNKS,
                           ref_chro = ref_chro, ref_nb_chro = df_nb_chroniques, refs = ctx$REFS,
                           nb_tirages = NB_TIRAGES_COURTS, seuil_ref = SEUIL_REF_DAS,
                           cibles_defaut = CIBLES_NB_CHRONIQUES, age_max = AGE_MAX_OUVERT)
  rapport <- etat_tirage("rapport", list()); revue <- etat_tirage("revue", list())
  rapport$courts_tirage_n <- nrow(df_tirage)
  # Habillage admin : NB_VARIANTES_ADMIN_COURTS variantes tirées au sort par scénario (§5.9b)
  set.seed(SEED + 1e6)
  df_scenarios <- df_tirage |> 
    dplyr::left_join(df_v_admin_courts,relationship = "many-to-many") |> 
    dplyr::group_by(dplyr::across(-dplyr::any_of(COLS_ADMIN))) |> 
    dplyr::slice_sample(n = NB_VARIANTES_ADMIN_COURTS) |> 
    dplyr::ungroup()
  rm(df_tirage)
  f_out <- chemin_export("scenarios_courts")
  arrow::write_parquet(df_scenarios, f_out)
  cat("- Séjours courts : ", nrow(df_scenarios), " lignes -> ", f_out, "\n", sep = "")
  rapport$courts <- stats_branche(df_scenarios, PIVOTS_COURTS, ctx$codes_imprecis, c("diag2", "diagnostic_associes"))
  set.seed(SEED + 2e6)
  revue$courts <- formater_revue(echantillonner_revue(df_scenarios |> dplyr::mutate(cmd = substr(ghm2, 1, 2)), 25), "courts", ctx$lib_cim)
  poser_tirage("rapport", rapport); poser_tirage("revue", revue)
  rm(df_scenarios); gc()
  banniere_fin("etape_tirage_courts", t0, f_out)
}

# Étape T2 — sélection des séjours longs, figée sous le seed global AVANT le premier chunk :
# selection_longs.parquet (quota_dp) / meta_tirage.yaml ; la reprise relit le fichier et ne
# re-tire jamais la sélection (garde-fou meta_tirage.yaml sur les paramètres).
etape_selection_longs <- function(budget = BUDGET_TOTAL_LONGS, mode = MODE_SELECTION){
  t0 <- banniere_debut("etape_selection_longs", "mode = " %+% mode %+% " ; budget = " %+% budget)
  if(!mode %in% c("catalogue_complet", "quota_dp")) stop("etape_selection_longs : mode inconnu : " %+% mode)
  ctx <- charger_contexte_tirage(c("catalogue_longs_seuil.parquet", "catalogue_longs_seuil_meta.yaml"), "etape_selection_longs")
  df_prep_scenarios_seuil <- arrow::read_parquet(file.path(EXPORTS_DIR, "catalogue_longs_seuil.parquet"))
  FICHIER_META_TIRAGE <- file.path(EXPORTS_DIR, "meta_tirage.yaml")
  FICHIER_SELECTION   <- file.path(EXPORTS_DIR, "selection_longs.parquet")
  meta_tirage <- list(MODE_SELECTION = mode, BUDGET_TOTAL_LONGS = as.integer(budget),
                      QUOTA_MIN_PAR_UNITE = as.integer(QUOTA_MIN_PAR_UNITE), NB_CHUNKS_MAX = as.integer(NB_CHUNKS_MAX),
                      CHUNK_SIZE_MIN = as.integer(CHUNK_SIZE_MIN), CHUNK_SIZE_FIXE = as.integer(CHUNK_SIZE_FIXE),
                      SEED = as.integer(SEED), nrow_catalogue = nrow(df_prep_scenarios_seuil))
  CLES_META_TIRAGE <- c("MODE_SELECTION", "BUDGET_TOTAL_LONGS", "QUOTA_MIN_PAR_UNITE", "NB_CHUNKS_MAX", "CHUNK_SIZE_MIN", "CHUNK_SIZE_FIXE", "SEED", "nrow_catalogue")
  meta_existant <- if(file.exists(FICHIER_META_TIRAGE)) yaml::read_yaml(FICHIER_META_TIRAGE) else NULL
  msg <- verifier_meta_tirage(meta_existant, meta_tirage, CLES_META_TIRAGE)
  if(!is.null(msg)) stop(msg)
  fichiers <- character(0)
  rapport <- etat_tirage("rapport", list())
  set.seed(SEED + 3e6)
  if(mode == "quota_dp"){
    if(file.exists(FICHIER_SELECTION)){
      df_selection <- arrow::read_parquet(FICHIER_SELECTION)
      cat("== Sélection longs (quota_dp) : relue depuis ", FICHIER_SELECTION, " (", nrow(df_selection), " lignes)\n", sep = "")
      meta_tirage$quota_par_dp <- meta_existant$quota_par_dp
      meta_tirage$nb_dp        <- meta_existant$nb_dp
    } else {
      sel <- selection_quota_dp(df_prep_scenarios_seuil, budget, QUOTA_MIN_PAR_UNITE)
      df_selection <- sel$selection
      meta_tirage$quota_par_dp <- sel$quota_par_dp
      meta_tirage$nb_dp        <- sel$nb_dp
      arrow::write_parquet(df_selection, FICHIER_SELECTION)
      cat("== Sélection longs (quota_dp) : ", sel$nb_dp, " diag2, quota par DP = ", sel$quota_par_dp,
          ", ", nrow(df_selection), " lignes -> ", FICHIER_SELECTION, "\n", sep = "")
      fichiers <- c(fichiers, FICHIER_SELECTION)
    }
    nb_tirage_longs <- 1L
    meta_tirage$NB_VARIANTES <- 1L
    meta_tirage$volume_attendu <- nrow(df_selection)
    rapport$selection <- effectifs_selection(df_selection)
    f_eff <- file.path(EXPORTS_DIR, "selection_longs_effectifs.csv")
    utils::write.csv(rapport$selection, f_eff, row.names = FALSE); fichiers <- c(fichiers, f_eff)
  } else {
    sel <- selection_catalogue_complet(df_prep_scenarios_seuil, budget)
    df_selection <- df_prep_scenarios_seuil
    nb_tirage_longs <- sel$nb_variantes
    meta_tirage$NB_VARIANTES   <- sel$nb_variantes
    meta_tirage$volume_attendu <- sel$volume_attendu
    cat("== Sélection longs (catalogue_complet) : nrow = ", sel$nrow, " ; NB_VARIANTES = ", sel$nb_variantes,
        " ; volume attendu = ", sel$volume_attendu, "\n", sep = "")
  }
  meta_tirage$PROFIL <- PROFIL; meta_tirage$date <- as.character(Sys.Date())
  yaml::write_yaml(meta_tirage, FICHIER_META_TIRAGE); fichiers <- c(fichiers, FICHIER_META_TIRAGE)
  rm(df_prep_scenarios_seuil)
  poser_tirage("selection", df_selection); poser_tirage("nb_tirage_longs", nb_tirage_longs); poser_tirage("meta_tirage", meta_tirage); poser_tirage("rapport", rapport)
  banniere_fin("etape_selection_longs", t0, fichiers)
}

# Relecture de la sélection figée (reprise de session) : meta_tirage.yaml + selection_longs.parquet
# (quota_dp) ou catalogue + NB_VARIANTES (catalogue_complet).
relire_selection_longs <- function(etape){
  f_meta <- file.path(EXPORTS_DIR, "meta_tirage.yaml")
  exiger_fichiers(f_meta, etape, "etape_selection_longs()")
  meta_tirage <- yaml::read_yaml(f_meta)
  if(identical(meta_tirage$MODE_SELECTION, "quota_dp")){
    f_sel <- file.path(EXPORTS_DIR, "selection_longs.parquet")
    exiger_fichiers(f_sel, etape, "etape_selection_longs()")
    df_selection <- arrow::read_parquet(f_sel)
    rapport <- etat_tirage("rapport", list()); rapport$selection <- effectifs_selection(df_selection); poser_tirage("rapport", rapport)
  } else {
    exiger_fichiers(file.path(EXPORTS_DIR, "catalogue_longs_seuil.parquet"), etape, "etape_catalogue()")
    df_selection <- arrow::read_parquet(file.path(EXPORTS_DIR, "catalogue_longs_seuil.parquet"))
  }
  cat("Sélection relue depuis ", f_meta, " (", nrow(df_selection), " lignes, NB_VARIANTES = ", meta_tirage$NB_VARIANTES, ")\n", sep = "")
  poser_tirage("selection", df_selection); poser_tirage("nb_tirage_longs", as.integer(meta_tirage$NB_VARIANTES)); poser_tirage("meta_tirage", meta_tirage)
  invisible(NULL)
}

# Étape T3 — tirage des DAS des séjours longs par chunks (sauvegardés / repris), SANS habillage.
# L'assemblé reste en mémoire de session (ETAPES_ENV) ; les chunks présents sont relus.
etape_tirage_das_longs <- function(){
  t0 <- banniere_debut("etape_tirage_das_longs", "CHUNKS_DIR = " %+% CHUNKS_DIR %+% " ; chunking dynamique (NB_CHUNKS_MAX = " %+% NB_CHUNKS_MAX %+% ", CHUNK_SIZE_MIN = " %+% CHUNK_SIZE_MIN %+% ", CHUNK_SIZE_FIXE = " %+% CHUNK_SIZE_FIXE %+% ")")
  ctx <- charger_contexte_tirage(nom_ref(c("ref_das_aigu", "ref_comp_diabete")), "etape_tirage_das_longs")
  if(is.null(etat_tirage("selection"))) relire_selection_longs("etape_tirage_das_longs")
  df_selection <- etat_tirage("selection"); nb_tirage_longs <- etat_tirage("nb_tirage_longs")
  df_das_ref <- arrow::read_parquet(file.path(EXPORTS_DIR, nom_ref("ref_das_aigu")))
  cat("== Séjours longs : tirage par chunks (", nrow(df_selection), " lignes × ", nb_tirage_longs, " variante(s)) ==\n", sep = "")
  df_tirage <- pmap_chunks(df_selection |> dplyr::select(dplyr::all_of(c(PIVOTS_LONGS, "diagnostic_associes", "poids"))),
                           sample_das_long, chunk_size = NULL, dossier = CHUNKS_DIR, prefixe = "longs",
                           seed_base = SEED + 1e5, garder_chunks = GARDER_CHUNKS,
                           ref_das_aigu = df_das_ref, refs = ctx$REFS, nb_tirage = nb_tirage_longs)
  rapport <- etat_tirage("rapport", list()); rapport$longs_tirage_n <- nrow(df_tirage); poser_tirage("rapport", rapport)
  poser_tirage("df_tirage_longs", df_tirage)
  rm(df_das_ref); gc()
  banniere_fin("etape_tirage_das_longs", t0, list.files(CHUNKS_DIR, pattern = "^longs_chunk_", full.names = TRUE))
}

# Étape T4 — habillage admin des séjours longs depuis v_admin_longs.parquet (relu, jamais
# prep_data en direct : frontière tirage / base) + slice_sample des variantes. Résultat en
# mémoire de session ; relit les chunks si l'étape T3 n'a pas tourné dans cette session.
etape_habillage_longs <- function(){
  t0 <- banniere_debut("etape_habillage_longs", "NB_VARIANTES_ADMIN_LONGS = " %+% NB_VARIANTES_ADMIN_LONGS)
  charger_contexte_tirage(nom_ref("v_admin_longs"), "etape_habillage_longs")
  if(is.null(etat_tirage("df_tirage_longs"))){ cat("df_tirage_longs absent de la session : relecture des chunks via etape_tirage_das_longs()\n"); etape_tirage_das_longs() }
  df_tirage <- etat_tirage("df_tirage_longs")
  df_v_admin_longs <- arrow::read_parquet(file.path(EXPORTS_DIR, nom_ref("v_admin_longs")))
  # Habillage admin (v7.2 l.555)
  set.seed(SEED + 4e6)
  df_scenarios <- df_tirage |> dplyr::left_join(df_v_admin_longs,relationship = "many-to-many")
  if(!is.na(NB_VARIANTES_ADMIN_LONGS)){
    df_scenarios <- df_scenarios |>
      dplyr::group_by(dplyr::across(-dplyr::any_of(c(COLS_ADMIN, "duree")))) |>
      dplyr::slice_sample(n = NB_VARIANTES_ADMIN_LONGS) |>
      dplyr::ungroup()
  }
  rm(df_tirage, df_v_admin_longs)
  poser_tirage("df_scenarios_longs", df_scenarios)
  if(exists("df_tirage_longs", envir = ETAPES_ENV)) rm("df_tirage_longs", envir = ETAPES_ENV)
  cat("- Séjours longs habillés : ", nrow(df_scenarios), " lignes (en mémoire de session)\n", sep = "")
  rm(df_scenarios); gc()
  banniere_fin("etape_habillage_longs", t0, character(0))
}

# Étape T5 — finalisation : contrôles §8.2, rapport, echantillon_revue.csv, top30, export
# scenarios_longs_tirage définitif. Reconstruit ce qui manque en session (chunks relus via
# etape_habillage_longs ; stats des courts recalculées depuis leur parquet).
etape_finalisation <- function(){
  t0 <- banniere_debut("etape_finalisation", "EXPORTS_DIR = " %+% EXPORTS_DIR)
  ctx <- charger_contexte_tirage(character(0), "etape_finalisation")
  if(is.null(etat_tirage("df_scenarios_longs"))){ cat("df_scenarios_longs absent de la session : reconstruction via etape_habillage_longs()\n"); etape_habillage_longs() }
  rapport <- etat_tirage("rapport", list()); revue <- etat_tirage("revue", list())
  if(is.null(rapport$courts)){
    f_courts <- chemin_export("scenarios_courts")
    exiger_fichiers(f_courts, "etape_finalisation", "etape_tirage_courts()")
    cat("stats des séjours courts recalculées depuis ", f_courts, "\n", sep = "")
    df_c <- arrow::read_parquet(f_courts)
    rapport$courts_tirage_n <- NA
    rapport$courts <- stats_branche(df_c, PIVOTS_COURTS, ctx$codes_imprecis, c("diag2", "diagnostic_associes"))
    set.seed(SEED + 2e6)
    revue$courts <- formater_revue(echantillonner_revue(df_c |> dplyr::mutate(cmd = substr(ghm2, 1, 2)), 25), "courts", ctx$lib_cim)
    rm(df_c); gc()
  }
  if(is.null(rapport$longs_tirage_n)) rapport$longs_tirage_n <- NA
  meta_tirage <- etat_tirage("meta_tirage"); if(is.null(meta_tirage)) meta_tirage <- yaml::read_yaml(file.path(EXPORTS_DIR, "meta_tirage.yaml"))
  df_scenarios <- etat_tirage("df_scenarios_longs")
  f_longs <- chemin_export("scenarios_longs_tirage")
  arrow::write_parquet(df_scenarios, f_longs)
  cat("- Séjours longs : ", nrow(df_scenarios), " lignes -> ", f_longs, "\n", sep = "")
  rapport$longs <- stats_branche(df_scenarios, PIVOTS_LONGS, ctx$codes_imprecis, c("diag2", "graine", "diagnostic_associes"))
  set.seed(SEED + 5e6)
  revue$longs <- formater_revue(echantillonner_revue(df_scenarios |> dplyr::mutate(cmd = substr(ghm2, 1, 2)), 25), "longs", ctx$lib_cim)
  rm(df_scenarios); rm("df_scenarios_longs", envir = ETAPES_ENV); gc()
  poser_tirage("rapport", rapport); poser_tirage("revue", revue)

  ## Livrables de validation
  df_revue <- dplyr::bind_rows(revue$courts, revue$longs)
  f_revue <- file.path(EXPORTS_DIR, "echantillon_revue.csv"); readr::write_excel_csv2(df_revue, f_revue)
  f_top <- file.path(EXPORTS_DIR, "top30_das_par_cmd.csv")
  utils::write.csv(dplyr::bind_rows(courts = rapport$courts$top_das, longs = rapport$longs$top_das, .id = "branche"), f_top, row.names = FALSE)
  mode <- meta_tirage$MODE_SELECTION; conv <- ctx$conversion_e669
  lignes <- c("RAPPORT DE CONTROLE — tirage_scenarios_v8.R — " %+% DATE_TAG %+% " — PROFIL = " %+% PROFIL,
              "", "== 0. Meta du catalogue (catalogue_longs_seuil_meta.yaml) ==",
              "   " %+% strsplit(yaml::as.yaml(ctx$meta_catalogue), "\n")[[1]],
              "", "== 0b. Meta du tirage (meta_tirage.yaml) ==",
              "   " %+% strsplit(yaml::as.yaml(meta_tirage), "\n")[[1]], "")
  lignes <- c(lignes, "== 1. Volumétrie ==",
              sprintf("sejours_courts : tirage = %s ; final = %d ; pivots distincts = %d", format(rapport$courts_tirage_n), rapport$courts$n, rapport$courts$pivots),
              sprintf("sejours_longs  : tirage = %s ; final = %d ; pivots distincts = %d", format(rapport$longs_tirage_n), rapport$longs$n, rapport$longs$pivots))
  if(mode == "catalogue_complet"){
    lignes <- c(lignes, sprintf("mode catalogue_complet : nrow catalogue = %d ; NB_VARIANTES = %d ; volume attendu = %d ; volume tiré = %s",
                                meta_tirage$nrow_catalogue, meta_tirage$NB_VARIANTES, meta_tirage$volume_attendu, format(rapport$longs_tirage_n)))
  } else {
    lignes <- c(lignes, sprintf("mode quota_dp : %d diag2 ; quota par DP = %d ; lignes sélectionnées = %d", meta_tirage$nb_dp, meta_tirage$quota_par_dp, meta_tirage$volume_attendu),
                "-- effectifs sélectionnés diag2 × type_unite (50 premières lignes ; table complète : selection_longs_effectifs.csv)",
                fmt_df(utils::head(rapport$selection, 50)),
                "-- totaux par type_unite :",
                fmt_df(rapport$selection |> dplyr::select(-diag2, -total) |> dplyr::summarise(dplyr::across(dplyr::everything(), sum))))
  }
  lignes <- c(lignes, "", "== 2. Distribution du nombre de DAS par classe d'âge (à comparer aux cibles de saturation) ==",
              "-- sejours_courts", fmt_df(rapport$courts$distribution), "-- sejours_longs", fmt_df(rapport$longs$distribution),
              "-- cibles dégradées CIBLES_NB_CHRONIQUES :",
              "   " %+% names(CIBLES_NB_CHRONIQUES) %+% " : " %+% vapply(CIBLES_NB_CHRONIQUES, function(x) paste(x, collapse = "-"), character(1)), "")
  lignes <- c(lignes, "== 3. Top 30 DAS par CMD (fichier complet : top30_das_par_cmd.csv) ==",
              "-- sejours_courts", fmt_df(rapport$courts$top_das), "-- sejours_longs", fmt_df(rapport$longs$top_das), "")
  lignes <- c(lignes, "== 4. Taux de codes « sans précision » parmi les DAS de sortie (mesuré, non corrigé) ==",
              sprintf("sejours_courts : %s", format(rapport$courts$taux_imprecis)),
              sprintf("sejours_longs  : %s", format(rapport$longs$taux_imprecis)), "")
  lignes <- c(lignes, "== 5. Vérifications programmatiques §8.2 (0 attendu ; NA = non applicable) ==")
  anomalies <- 0
  for(b in c("courts", "longs")){
    cc <- rapport[[b]]$controles
    lignes <- c(lignes, sprintf("sejours_%-7s doublons_categorie = %s ; diabete_hors_flag = %s ; i10_avec_hta_autres = %s ; poids_sous_seuil = %s",
                                b, format(cc$doublons_categorie), format(cc$diabete_hors_flag), format(cc$i10_avec_hta_autres), format(cc$poids_sous_seuil)))
    anomalies <- anomalies + sum(unlist(cc[c("doublons_categorie", "diabete_hors_flag", "i10_avec_hta_autres", "poids_sous_seuil")]), na.rm = TRUE)
  }
  lignes <- c(lignes, "-- conversion E669 (meta.yaml CONVERSION_E669 = " %+% conv %+% ") : ^E669 résiduels attendus = 0",
              sprintf("sejours_courts  e669_residuels = %d%s", rapport$courts$e669_residuels, if(conv && rapport$courts$e669_residuels > 0) "  <- ANOMALIE" else ""),
              sprintf("sejours_longs   e669_residuels = %d%s", rapport$longs$e669_residuels, if(conv && rapport$longs$e669_residuels > 0) "  <- ANOMALIE" else ""))
  if(conv) anomalies <- anomalies + rapport$courts$e669_residuels + rapport$longs$e669_residuels
  lignes <- c(lignes, "-- effectifs E660x par classe (diag2 + DAS) :", "   courts :", fmt_df(rapport$courts$e660), "   longs :", fmt_df(rapport$longs$e660))
  lignes <- c(lignes, "TOTAL anomalies = " %+% anomalies, "",
              "Livrables : echantillon_revue.csv (" %+% nrow(df_revue) %+% " scénarios), top30_das_par_cmd.csv, meta_tirage.yaml" %+%
                if(mode == "quota_dp") ", selection_longs.parquet, selection_longs_effectifs.csv" else "")
  f_rapport <- file.path(EXPORTS_DIR, "rapport_v8_" %+% DATE_TAG %+% ".txt")
  writeLines(lignes, f_rapport)
  cat(lignes, sep = "\n")
  cat("Tirage terminé. Rapport : ", f_rapport, "\n", sep = "")
  banniere_fin("etape_finalisation", t0, c(f_longs, f_revue, f_top, f_rapport))
}

## ---- 4. Tableau de bord ----
# Statut FAIT / PARTIEL / À FAIRE par étape et pour le profil courant, avec preuves (fichiers
# seulement : aucune connexion requise ; les tables temporaires sont « inconnu hors connexion »).
etat_pipeline <- function(){
  plan <- resoudre_besoins(TYPES_ETBS_LONGS, ANS_HISTORIQUE, AN_REF,
                           fichiers_partiels = if(dir.exists(PARTIELS_DIR)) list.files(PARTIELS_DIR, pattern = "^catalogue_partiel_.*\\.parquet$") else character(0),
                           fichiers_exports = if(dir.exists(EXPORTS_DIR)) list.files(EXPORTS_DIR, pattern = "\\.parquet$") else character(0),
                           forcer_refs = FALSE, noms_refs = NOMS_REFS, refs_chroniques = REFS_CHRONIQUES)
  statut3 <- function(n, total) if(total == 0) "À FAIRE" else if(n >= total) "FAIT" else if(n > 0) "PARTIEL" else "À FAIRE"
  lire_yaml <- function(f) if(file.exists(f)) yaml::read_yaml(f) else NULL
  rows <- list()
  ajouter <- function(etape, statut, preuve) rows[[length(rows) + 1]] <<- data.frame(etape = etape, statut = statut, preuve = preuve, stringsAsFactors = FALSE)
  # 1. prep_data
  tt <- if(exists("conn", envir = globalenv()) && !is.null(get("conn", envir = globalenv()))){
    ok <- vapply(c("prep_data_" %+% AN_REF, "prep_das_chro_" %+% AN_REF), table_temporaire_existe, logical(1))
    paste0(names(ok), " : ", ifelse(ok, "présente", "absente"), collapse = " ; ")
  } else "tables temporaires : inconnu hors connexion"
  ajouter("etape_prep_data", "(session)", tt %+% " ; partiels_meta.yaml : " %+% if(file.exists(FICHIER_PARTIELS_META())) "présent" else "absent")
  # 2. refs
  n_refs <- sum(!plan$refs$a_faire)
  ajouter("etape_refs", statut3(n_refs, nrow(plan$refs)), sprintf("refs présentes %d / %d dans %s%s", n_refs, nrow(plan$refs), EXPORTS_DIR,
                                                                   if(n_refs < nrow(plan$refs)) " ; manquantes : " %+% paste(plan$refs$nom[plan$refs$a_faire], collapse = ", ") else ""))
  # 3. partiels
  n_p <- sum(!plan$iterations$a_faire)
  ajouter("etape_partiels_longs", statut3(n_p, nrow(plan$iterations)), sprintf("partiels présents %d / %d attendus du plan (%s × %s)%s", n_p, nrow(plan$iterations),
          paste(TYPES_ETBS_LONGS, collapse = ","), paste(range(ANS_HISTORIQUE), collapse = "-"),
          if(n_p < nrow(plan$iterations)) " ; manquants : " %+% paste(plan$iterations$fichier[plan$iterations$a_faire], collapse = ", ") else ""))
  # 4. catalogue
  m <- lire_yaml(file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml"))
  ajouter("etape_catalogue", if(!is.null(m) && file.exists(file.path(EXPORTS_DIR, "catalogue_longs_seuil.parquet"))) "FAIT" else "À FAIRE",
          if(is.null(m)) "catalogue_longs_seuil.parquet absent" else sprintf("catalogue du %s : %s lignes ; périmètre %s × %s ; CONVERSION_E669 = %s", m$date, m$nb_lignes,
                                                                             paste(unlist(m$TYPES_ETBS_LONGS), collapse = ","), paste(range(unlist(m$ANS_HISTORIQUE)), collapse = "-"), m$CONVERSION_E669))
  # 5. courts
  f_pc <- file.path(EXPORTS_DIR, "pivots_courts.parquet")
  n_courts_att <- if(file.exists(f_pc)){ nc <- nrow(arrow::read_parquet(f_pc)); as.integer(ceiling(nc / taille_chunk(nc))) } else NA
  n_courts <- if(dir.exists(CHUNKS_DIR)) length(list.files(CHUNKS_DIR, pattern = "^courts_chunk_")) else 0L
  f_sc <- if(dir.exists(EXPORTS_DIR)) list.files(EXPORTS_DIR, pattern = "^scenarios_courts_v8_.*\\.parquet$") else character(0)
  ajouter("etape_tirage_courts", if(length(f_sc)) "FAIT" else if(n_courts > 0) "PARTIEL" else "À FAIRE",
          sprintf("chunks courts %d / %s ; export : %s", n_courts, format(n_courts_att), if(length(f_sc)) paste(f_sc, collapse = ", ") else "absent"))
  # 6. sélection
  mt <- lire_yaml(file.path(EXPORTS_DIR, "meta_tirage.yaml"))
  ajouter("etape_selection_longs", if(is.null(mt)) "À FAIRE" else "FAIT",
          if(is.null(mt)) "meta_tirage.yaml absent" else sprintf("mode %s ; budget %s ; NB_VARIANTES %s ; volume attendu %s ; date %s", mt$MODE_SELECTION, mt$BUDGET_TOTAL_LONGS, mt$NB_VARIANTES, mt$volume_attendu, mt$date))
  # 7. chunks longs
  n_longs_att <- if(is.null(mt)) NA else { nl <- if(identical(mt$MODE_SELECTION, "quota_dp")) mt$volume_attendu else mt$nrow_catalogue; as.integer(ceiling(nl / taille_chunk(nl))) }
  n_longs <- if(dir.exists(CHUNKS_DIR)) length(list.files(CHUNKS_DIR, pattern = "^longs_chunk_")) else 0L
  ajouter("etape_tirage_das_longs", if(!is.na(n_longs_att)) statut3(n_longs, n_longs_att) else if(n_longs > 0) "PARTIEL" else "À FAIRE",
          sprintf("chunks longs %d / %s", n_longs, format(n_longs_att)))
  ajouter("etape_habillage_longs", if(!is.null(etat_tirage("df_scenarios_longs"))) "FAIT (session)" else "(session)", "résultat en mémoire de session uniquement")
  # 8. finalisation
  f_sl <- if(dir.exists(EXPORTS_DIR)) list.files(EXPORTS_DIR, pattern = "^scenarios_longs_tirage_v8_.*\\.parquet$") else character(0)
  f_rp <- if(dir.exists(EXPORTS_DIR)) list.files(EXPORTS_DIR, pattern = "^rapport_v8_.*\\.txt$") else character(0)
  ajouter("etape_finalisation", if(length(f_sl) && length(f_rp)) "FAIT" else "À FAIRE",
          sprintf("exports finaux : %s ; rapport : %s ; echantillon_revue.csv : %s", if(length(f_sl)) paste(f_sl, collapse = ", ") else "absent",
                  if(length(f_rp)) paste(f_rp, collapse = ", ") else "absent", if(file.exists(file.path(EXPORTS_DIR, "echantillon_revue.csv"))) "présent" else "absent"))
  etat <- do.call(rbind, rows)
  cat("== ÉTAT DU PIPELINE — PROFIL = ", PROFIL, " — ", format(Sys.time(), "%Y-%m-%d %H:%M"), " ==\n", sep = "")
  for(i in seq_len(nrow(etat))) cat(sprintf("  %-24s %-14s %s\n", etat$etape[i], etat$statut[i], etat$preuve[i]))
  invisible(etat)
}
