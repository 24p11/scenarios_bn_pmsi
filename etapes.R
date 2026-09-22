###############################################################################
# etapes.R — ORCHESTRATION PAR ÉTAPES du pipeline scenarios_bn_pmsi v8
#
# Sourcé par extraction.R et tirage.R (après config,
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
  f <- file.path(ns(DIR_REFERENCES), nom_ref("ref_distribution_e660"))
  if(!file.exists(f)) stop("ref_distribution_e660.parquet absent de " %+% DIR_REFERENCES %+% " (calculé avec ref_das_chronique ; FORCER_REFS <- TRUE)")
  arrow::read_parquet(f)
}
ref_das_chronique_brute <- function(){
  if(!exists("brute", envir = CACHE_E669)) assign("brute", if(un_seul_an_ref(ANS_COURTS)) ref_das_chronique(AN_REF) else ref_das_chronique_cumul(ANS_COURTS), envir = CACHE_E669)
  get("brute", envir = CACHE_E669)
}
codes_imprecis_extraction <- function(){
  codes <- codes_imprecis_de_cim(cim, MOTIF_IMPRECIS)
  if(CONVERSION_E669) codes <- codes[!grepl("^E669", codes)]   # E669 traité en amont par la conversion
  codes
}
fabrique_pivots_courts <- function(an = AN_REF){   # chantier courts en campagnes : `an` paramétré (défaut AN_REF, chaîne B6 intacte)
  df_cases_courts <- pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
    dplyr::filter(duree%in%DUREE_COURTS) |> dplyr::summarise(nb=dplyr::n(),.by=dplyr::all_of(PIVOTS_COURTS)) |> dplyr::filter(nb>SEUIL_PIVOT) |> 
    dplyr::collect()
  df_cases_courts
}
# Micro-lot « v_admin : périmètre de durée + pondération » (Q72, Q70 actées, journal §24.11) : chaque photographie admin est
# FILTRÉE sur le périmètre de durée de sa branche (DUREE_COURTS / DUREE_LONGS : un scénario ne peut plus recevoir la durée
# de l'autre branche) et passe de distinct() à un COMPTE par combinaison (colonne n : agrégat seuillable, aucune donnée au
# grain séjour) ; l'habillage tire les variantes admin pondérées par n (helpers K2). Écarts B7 / B10 consignés.
fabrique_v_admin_courts <- function(an = AN_REF){   # `an` paramétré (défaut AN_REF)
  df_v_admin_courts <- pRatihque::atihble(conn, 'prep_data_' %+% an ) |> 
    dplyr::filter(duree%in%DUREE_COURTS) |> 
    dplyr::summarise(n=dplyr::n(),.by=c(mode_hospit,mode_entree,mode_sortie,sexe,cage,ghm2,diag2,mdp,duree)) |> 
    dplyr::collect()
  df_v_admin_courts
}
# DÉCISION UTILISATEUR (chantier « habillage robuste », défaut trouvé en revue clinique) : nbda SORT des clés de la
# photographie admin des longs (écart B10 consigné) — la table rétrécit, les variantes se cumulent entre valeurs de nbda ;
# la jointure d'habillage porte alors 6 clés (CLES_ADMIN_LONGS) avec repli hiérarchique (helpers K2).
fabrique_v_admin_longs <- function(){
  df_v_admin_longs <- pRatihque::atihble(conn, 'prep_data_' %+% AN_REF ) |> 
    dplyr::filter(duree%in%DUREE_LONGS) |> 
    dplyr::summarise(n=dplyr::n(),.by=c(mode_hospit,mode_entree,mode_sortie,sexe,age,cage,ghm2,diag2,mdp,duree)) |> 
    dplyr::collect()
  df_v_admin_longs
}
# TIRABLE COURTS MULTI-ANNÉES (chantier « courts en campagnes » §1bis, symétrie avec le catalogue longs) : ANS_COURTS = AN_REF
# (défaut) -> chaînes v7 ci-dessus, inchangées ; sinon les pivots sont cumulés EN BASE (comptes des années additionnés par
# union_all, seuil SEUIL_PIVOT rejugé sur le cumul), les refs de saturation courts (ref_das_chronique -> distribution_e660,
# ref_nb_chroniques) et v_admin_courts = chaînes v7 par année puis addition / distinct en R (tables agrégées : résultat
# identique à une addition en base). Les ids des pivots sont des hash de contenu : stables sous extension.
un_seul_an_ref <- function(ans) length(ans) == 1 && ans[1] == AN_REF
fabrique_pivots_courts_cumul <- function(ans){
  parts <- lapply(ans, function(an) pRatihque::atihble(conn, 'prep_data_' %+% an) |> dplyr::filter(duree%in%DUREE_COURTS) |> dplyr::summarise(nb=dplyr::n(),.by=dplyr::all_of(PIVOTS_COURTS)))
  Reduce(dplyr::union_all, parts) |> dplyr::summarise(nb = sum(nb, na.rm = TRUE), .by = dplyr::all_of(PIVOTS_COURTS)) |> dplyr::filter(nb>SEUIL_PIVOT) |> dplyr::collect()
}
fabrique_v_admin_courts_cumul <- function(ans) purrr::map(ans, fabrique_v_admin_courts) |> purrr::list_rbind() |> reagreger(c("mode_hospit", "mode_entree", "mode_sortie", "sexe", "cage", "ghm2", "diag2", "mdp", "duree"), "n")
ref_das_chronique_cumul <- function(ans) purrr::map(ans, ref_das_chronique) |> purrr::list_rbind() |> reagreger(c("diag2", "das", "sexe", "cage", "niveau", "type_liste", "caract"), "nb_das")
ref_nb_chroniques_cumul <- function(ans) purrr::map(ans, ref_nb_chroniques) |> purrr::list_rbind() |> reagreger(c("cage", "sexe", "nb_chro"), "nb")
FABRIQUES_REFS <- list(
  ref_das_chronique = function(){
    brute <- ref_das_chronique_brute()
    if(!CONVERSION_E669) return(brute)
    dist <- distribution_e660(brute, "das", "nb_das")
    brute |>
      convertir_e669_comptes("diag2", c("das", "sexe", "cage", "niveau", "type_liste", "caract"), "nb_das", dist, BARE_E669_DEFAUT) |>
      convertir_e669_comptes("das", c("diag2", "sexe", "cage", "niveau", "type_liste", "caract"), "nb_das", dist, BARE_E669_DEFAUT)
  },
  ref_distribution_e660 = function(){
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
  ref_nb_chroniques = function() if(un_seul_an_ref(ANS_COURTS)) ref_nb_chroniques(AN_REF) else ref_nb_chroniques_cumul(ANS_COURTS),   # aucun code : intact ; cumul ANS_COURTS
  ref_comp_diabete  = function() ref_comp_diabete(AN_REF),                  # effectifs bruts ; pénalisation .9 côté tirage
  ref_pivots_courts = function(){
    # Le seuil nb > SEUIL_PIVOT est appliqué EN BASE par la chaîne v7.1.2 (non modifiée) :
    # conversion + ré-agrégation sur le collecté, sans re-seuil (perte conservatrice : classes
    # E669 sous le seuil individuellement ne sont jamais vues ; cf. MODIFICATIONS_V8.md §12).
    df <- if(un_seul_an_ref(ANS_COURTS)) fabrique_pivots_courts() else fabrique_pivots_courts_cumul(ANS_COURTS)
    if(!CONVERSION_E669) return(df)
    convertir_e669_comptes(df, "diag2", setdiff(PIVOTS_COURTS, "diag2"), "nb", charger_dist_e660(), BARE_E669_DEFAUT)
  },
  # photographies admin avec effectifs (micro-lot §24.11) : conversion E669 par COMPTES (n répartis aux plus forts restes, ré-agrégés), plus par distinct
  ref_v_admin_courts = function(){
    df <- if(un_seul_an_ref(ANS_COURTS)) fabrique_v_admin_courts() else fabrique_v_admin_courts_cumul(ANS_COURTS)
    if(!CONVERSION_E669) return(df)
    convertir_e669_comptes(df, "diag2", setdiff(names(df), c("diag2", "n")), "n", charger_dist_e660(), BARE_E669_DEFAUT)
  },
  ref_v_admin_longs = function(){
    df <- fabrique_v_admin_longs()
    if(!CONVERSION_E669) return(df)
    convertir_e669_comptes(df, "diag2", setdiff(names(df), c("diag2", "n")), "n", charger_dist_e660(), BARE_E669_DEFAUT)
  },
  ref_substitution_imprecis = function(){
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
  ref_paires_chroniques = function(){
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
# ---- Chemins dérivés du bloc UNIQUE de config (arborescence par étapes ; aucune autre concaténation de chemin) ----
# Magasins PARTAGÉS (config : DIR_PARTIELS, DIR_REFERENCES, DIR_CATALOGUE_M, DIR_COURTS, DIR_DIAGNOSTICS) ; PAR PROFIL :
# DIR_CAMPAGNES/<CAMPAGNE>/ (transitoires), DIR_REGISTRE_M (permanent), DIR_EXPORT_FINAL (livrables). Convention : un
# _meta.yaml colocalisé dans chaque magasin / dossier de campagne ; <objet>_meta.yaml à côté d'un fichier.
ns <- function(p) sub("/+$", "", p)   # les DIR_* de config se terminent par "/" ; file.path() les veut sans (chemins propres, sans "//")
FICHIER_PARTIELS_META       <- function() file.path(ns(DIR_PARTIELS), "_meta.yaml")
FICHIER_REFERENCES_META     <- function() file.path(ns(DIR_REFERENCES), "_meta.yaml")
DIR_CATALOGUE               <- function() file.path(ns(DIR_CATALOGUE_M), "catalogue_longs_seuil")           # parts par lettre + _meta.yaml (le magasin)
FICHIER_CATALOGUE_META      <- function() file.path(DIR_CATALOGUE(), "_meta.yaml")                       # ex _sidecar.yaml, + clés du magasin
MONO_CATALOGUE              <- function() file.path(ns(DIR_CATALOGUE_M), "catalogue_longs_seuil.parquet")   # monofichier transitoire (etape_catalogue -> repartitionner)
FICHIER_CATALOGUE_MONO_META <- function() file.path(ns(DIR_CATALOGUE_M), "catalogue_longs_seuil_meta.yaml")
FICHIER_RAPPORT_EXTRACTION  <- function() file.path(ns(DIR_CATALOGUE_M), "rapport_extraction.txt")
# Refs : toutes dans 10_references, SAUF le tirable courts (ref_pivots_courts = le catalogue des courts) dans 30_courts
FICHIER_REF                 <- function(nom) file.path(ns(if(nom == "ref_pivots_courts") DIR_COURTS else DIR_REFERENCES), nom_ref(nom))
FICHIER_PIVOTS_COURTS       <- function() FICHIER_REF("ref_pivots_courts")
FICHIER_COURTS_META         <- function() file.path(ns(DIR_COURTS), "_meta.yaml")                       # méta du TIRABLE (ANS_COURTS, seuils)
FICHIER_COURTS_HISTORIQUE   <- function() file.path(ns(DIR_COURTS), "scenarios_courts.parquet")        # corpus courts FIXE historique (adopté en C1)
FICHIER_COURTS_HISTORIQUE_META <- function() file.path(ns(DIR_COURTS), "scenarios_courts_meta.yaml")
FICHIER_APPORTS             <- function() file.path(ns(DIR_DIAGNOSTICS), "diagnostic_apports.csv")
FICHIER_RECOUVREMENT        <- function() file.path(ns(DIR_DIAGNOSTICS), "recouvrement.csv")
FICHIER_DIAG_MEMOIRE        <- function() file.path(ns(DIR_DIAGNOSTICS), "diagnostic_memoire_" %+% PROFIL %+% ".csv")
DIR_CAMPAGNE                <- function(campagne = CAMPAGNE) file.path(ns(DIR_CAMPAGNES), campagne)
# Courts PAR CAMPAGNE (chantier « courts en campagnes ») : chunks transitoires + scénarios habillés de la campagne
DIR_CHUNKS_COURTS           <- function(campagne = CAMPAGNE) file.path(DIR_CAMPAGNE(campagne), "chunks_courts")
DIR_HABILLE_COURTS          <- function(campagne = CAMPAGNE) file.path(DIR_CAMPAGNE(campagne), "habille", "courts")
FICHIER_COURTS_CAMPAGNE     <- function(campagne = CAMPAGNE) file.path(DIR_HABILLE_COURTS(campagne), "scenarios_courts.parquet")
FICHIER_COURTS_CAMPAGNE_META <- function(campagne = CAMPAGNE) file.path(DIR_HABILLE_COURTS(campagne), "_meta.yaml")
DIR_SELECTION               <- function(pop = NULL) if(is.null(pop)) file.path(DIR_CAMPAGNE(), "selection") else file.path(DIR_CAMPAGNE(), "selection", pop)
FICHIER_SELECTION_META      <- function() file.path(DIR_SELECTION(), "_meta.yaml")                       # ex meta_tirage.yaml
FICHIER_SELECTION_POP_META  <- function(pop) file.path(DIR_SELECTION(pop), "_meta.yaml")
DIR_CHUNKS_LONGS            <- function() file.path(DIR_CAMPAGNE(), "chunks")                            # modes historiques : chunks à plat
DIR_CHUNKS_POP              <- function(pop) file.path(DIR_CHUNKS_LONGS(), pop)                          # quota_dp_fixe : par population
DIR_HABILLE                 <- function(pop) file.path(DIR_CAMPAGNE(), "habille", pop)
DIR_REGISTRE                <- function() file.path(ns(DIR_REGISTRE_M), "registre_tirages")
FICHIER_LIVRABLE            <- function(campagne = CAMPAGNE) file.path(ns(DIR_EXPORT_FINAL), "scenarios_" %+% campagne %+% ".parquet")
DIR_LIVRABLE_PARTS          <- function(campagne = CAMPAGNE) file.path(ns(DIR_EXPORT_FINAL), "scenarios_" %+% campagne)         # repli au-delà de SEUIL_MONOFICHIER
FICHIER_LIVRABLE_META       <- function(campagne = CAMPAGNE) file.path(ns(DIR_EXPORT_FINAL), "scenarios_" %+% campagne %+% "_meta.yaml")
FICHIER_RAPPORT             <- function(campagne = CAMPAGNE) file.path(ns(DIR_EXPORT_FINAL), "rapport_" %+% campagne %+% ".txt")
FICHIER_REVUE               <- function(campagne = CAMPAGNE) file.path(ns(DIR_EXPORT_FINAL), "echantillon_revue_" %+% campagne %+% ".csv")
FICHIER_TOP30               <- function(campagne = CAMPAGNE) file.path(ns(DIR_EXPORT_FINAL), "top30_das_par_cmd_" %+% campagne %+% ".csv")
creer_dossiers <- function(...) for(d in c(...)) if(!dir.exists(d)) dir.create(d, recursive = TRUE)
# Seeds : sans registre, base = SEED (comportement antérieur) ; en campagne (REGISTRE_ACTIF), base =
# seed_campagne(SEED, CAMPAGNE) — deux campagnes de même config et même registre initial sont
# reproductibles indépendamment.
seed_base_campagne <- function() if(isTRUE(REGISTRE_ACTIF)) seed_campagne(SEED, CAMPAGNE) else SEED
seed_population <- function(pop) as.integer(seed_base_campagne() + 1e5 + 1e4 * match(pop, names(POPULATIONS)))   # seed de tirage dérivé par population
seed_courts     <- function() as.integer(seed_base_campagne() + 2e5)   # seed des courts de la campagne (déterminisme par (campagne, plage))
# diagnostic_memoire_<profil>.csv : journal mémoire de la session, écrit (idempotent, mêmes lignes) en fin
# d'etape_refs, d'etape_partiels_longs et d'etape_catalogue ; character(0) si aucune mesure.
ecrire_diagnostic_memoire <- function(){
  j <- get("journal", envir = MEMOIRE_ENV)
  if(is.null(j)) return(character(0))
  creer_dossiers(DIR_DIAGNOSTICS); f <- FICHIER_DIAG_MEMOIRE(); utils::write.csv(j, f, row.names = FALSE); f
}
charger_typo <- function(){ if(!exists("typo", envir = ETAPES_ENV, inherits = FALSE)) assign("typo", charger_typologie(PATH_TYPOLOGIE), envir = ETAPES_ENV); get("typo", envir = ETAPES_ENV, inherits = FALSE) }
# Surcharge de configuration active (SCENARIOS_PMSI_SURCHARGE) et marqueur de palier (PALIER_ACTIF posé par palier.R)
palier_actif <- function() nzchar(SURCHARGE_CONFIG) && isTRUE(get0("PALIER_ACTIF", envir = globalenv(), inherits = FALSE))
campagne_active <- function() nzchar(SURCHARGE_CONFIG) && isTRUE(get0("SURCHARGE_CAMPAGNE_ACTIVE", envir = globalenv(), inherits = FALSE))
surcharge_active <- function() if(!nzchar(SURCHARGE_CONFIG)) "aucune" else SURCHARGE_CONFIG %+% (if(palier_actif()) " (PALIER de mesure : budget de palier, PAS la campagne)" else if(campagne_active()) " (CAMPAGNE : décision d'exploitation de campagne.R)" else "")
# Trois niveaux de paramètres (helpers J) : écriture des surcharges campagne.R / palier.R depuis les notebooks, avec exclusivité.
lignes_surcharge_env <- function(){ f <- Sys.getenv("SCENARIOS_PMSI_SURCHARGE", ""); if(nzchar(f) && file.exists(f)) readLines(f, warn = FALSE) else character(0) }
ecrire_surcharge <- function(type, lignes, fichier){
  actif <- Sys.getenv("SCENARIOS_PMSI_SURCHARGE", "")
  meme_fichier <- nzchar(actif) && file.exists(fichier) && normalizePath(actif, mustWork = FALSE) == normalizePath(fichier, mustWork = FALSE)
  if(!meme_fichier){ v <- verifier_exclusivite_surcharges(type, actif, lignes_surcharge_env()); if(!v$ok) stop(v$message, call. = FALSE) }
  writeLines(lignes, fichier); Sys.setenv(SCENARIOS_PMSI_SURCHARGE = normalizePath(fichier))
  cat("surcharge ", type, " écrite : ", normalizePath(fichier), " — Restart R puis chunk `session` (la session affiche la source de chaque paramètre).\n", sep = "")
  invisible(normalizePath(fichier))
}
ecrire_surcharge_campagne <- function(campagne, nb_crh_cible, nb_lignes_par_dp = 1L, registre_actif = TRUE, plafonds_dpec = NULL, fichier = "campagne.R", nb_crh_cible_courts = NULL, ratio_courts = NULL, nb_variantes_admin = NULL)
  ecrire_surcharge("campagne", contenu_surcharge_campagne(campagne, nb_crh_cible, nb_lignes_par_dp, registre_actif, plafonds_dpec, nb_crh_cible_courts, ratio_courts, nb_variantes_admin), fichier)
ecrire_surcharge_palier <- function(nb_crh_cible = 100000L, fichier = "palier.R") ecrire_surcharge("palier", contenu_surcharge_palier(nb_crh_cible), fichier)
# Affichage session : valeur effective et SOURCE de chaque paramètre de campagne (défaut config / campagne.R / palier.R)
afficher_sources_campagne <- function(){
  src <- sources_parametres(PARAMETRES_CAMPAGNE, if(nzchar(SURCHARGE_CONFIG) && file.exists(SURCHARGE_CONFIG)) readLines(SURCHARGE_CONFIG, warn = FALSE) else NULL, SURCHARGE_CONFIG)
  cat("Paramètres de campagne (valeur effective [source]) :\n")
  for(n in PARAMETRES_CAMPAGNE){ v <- get0(n, envir = globalenv(), inherits = FALSE); cat(sprintf("  %-18s = %-40s [%s]\n", n, if(is.list(v)) paste(names(v), unlist(v), sep = " = ", collapse = " ; ") else paste(v, collapse = ","), src[[n]])) }
  invisible(src)
}
# Garde d'un magasin partagé au chargement : méta absent -> ok ; écart -> stop (verifier_magasin, helpers I1)
lire_meta_si_present <- function(f) if(file.exists(f)) yaml::read_yaml(f) else NULL
garder_magasin <- function(magasin, f_meta, etape){
  v <- verifier_magasin(magasin, lire_meta_si_present(f_meta), valeurs_effectives_config())
  if(!v$ok) stop(etape %+% " : " %+% v$message, call. = FALSE)
  invisible(v)
}
fichiers_refs_presents <- function() c(if(dir.exists(DIR_REFERENCES)) list.files(DIR_REFERENCES, pattern = "\\.parquet$"), if(dir.exists(DIR_COURTS)) list.files(DIR_COURTS, pattern = "^ref_pivots_courts\\.parquet$"))
plan_courant <- function(forcer_refs = FORCER_REFS){
  creer_dossiers(PATH_RESULTS, DIR_PARTIELS, DIR_REFERENCES, DIR_CATALOGUE_M, DIR_COURTS, DIR_DIAGNOSTICS)
  resoudre_besoins(TYPES_ETBS_LONGS, ANS_HISTORIQUE, AN_REF,
                   fichiers_partiels = list.files(DIR_PARTIELS, pattern = "^catalogue_partiel_.*\\.parquet$"),
                   fichiers_exports  = fichiers_refs_presents(),
                   forcer_refs = forcer_refs, noms_refs = NOMS_REFS, refs_chroniques = REFS_CHRONIQUES, ans_courts = ANS_COURTS, refs_courts = REFS_COURTS)
}

## ---- 2. Famille EXTRACTION ----

# Étape 1 — résolution des besoins puis prep_data des années nécessaires (ans = NULL -> déduites
# du plan ; sinon forcées) ; prep_das_chronique(AN_REF) si une ref chronique manque. Garde-fou
# du magasin 00_partiels (_meta.yaml, FORCER_PARTIELS). Tables temporaires uniquement : rien n'est persisté en base.
etape_prep_data <- function(ans = NULL){
  t0 <- banniere_debut("etape_prep_data", "PROFIL = " %+% PROFIL %+% " ; AN_REF = " %+% AN_REF)
  exiger_conn("etape_prep_data")
  meta_partiels <- meta_partiels_courant(K_GRAINE_LONGS, NBDA_MAX, DUREE_LONGS, PIVOTS_LONGS, VERSION_SCRIPT)
  if(isTRUE(FORCER_PARTIELS) && dir.exists(DIR_PARTIELS)){ cat("FORCER_PARTIELS : magasin partagé ", DIR_PARTIELS, " vidé (il sert tous les profils)\n", sep = ""); unlink(DIR_PARTIELS, recursive = TRUE) }
  plan <- plan_courant()
  if(file.exists(FICHIER_PARTIELS_META())){
    verif <- verifier_partiels_meta(yaml::read_yaml(FICHIER_PARTIELS_META()), meta_partiels)
    if(!is.null(verif$erreur)) stop(verif$erreur, call. = FALSE)
    for(a in verif$avertissements) warning(a, call. = FALSE)
  } else {
    yaml::write_yaml(c(list(magasin = "partiels"), meta_partiels), FICHIER_PARTIELS_META())
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
  for(an_ in plan$annees_das_chronique){ cat("prep_das_chronique(", an_, ")\n", sep = ""); prep_das_chronique(an_) }
  banniere_fin("etape_prep_data", t0, FICHIER_PARTIELS_META())
}

# Étape 2 — tables de référence : chaque fabrique, export parquet, libération (boucle déplacée
# telle quelle) ; sautée si son parquet existe (sauf forcer). Prérequis : prep_data_<AN_REF>.
etape_refs <- function(forcer = FORCER_REFS){
  t0 <- banniere_debut("etape_refs", "forcer = " %+% forcer %+% " ; magasin partagé = " %+% DIR_REFERENCES)
  plan <- plan_courant(forcer_refs = forcer)
  # Garde du magasin partagé : méta en écart -> stop sauf forcer (alors le magasin est régénéré et son méta réécrit)
  v <- verifier_magasin("references", lire_meta_si_present(FICHIER_REFERENCES_META()), valeurs_effectives_config())
  if(!v$ok && !isTRUE(forcer)) stop("etape_refs : " %+% v$message, call. = FALSE)
  if(!v$ok) cat("FORCER_REFS : magasin partagé régénéré malgré l'écart (", paste(v$differences, collapse = ", "), ")\n", sep = "")
  # Garde du TIRABLE courts (magasin 30_courts : ANS_COURTS, seuils) : écart -> stop sauf forcer / FORCER_COURTS (pivots régénérés)
  v_c <- verifier_magasin("courts", lire_meta_si_present(FICHIER_COURTS_META()), valeurs_effectives_config())
  if(!v_c$ok && !isTRUE(forcer) && !isTRUE(FORCER_COURTS)) stop("etape_refs : " %+% v_c$message, call. = FALSE)
  if(!v_c$ok || isTRUE(FORCER_COURTS)){ cat("tirable courts (30_courts) régénéré", if(!v_c$ok) " malgré l'écart (" %+% paste(v_c$differences, collapse = ", ") %+% ")" else " (FORCER_COURTS)", "\n", sep = ""); plan$refs$a_faire[plan$refs$nom == "ref_pivots_courts"] <- TRUE }
  fichiers <- character(0)
  if(any(plan$refs$a_faire)){
    exiger_conn("etape_refs")
    for(an_ in sort(unique(c(AN_REF, if(any(plan$refs$a_faire & plan$refs$nom %in% REFS_COURTS)) ANS_COURTS)))) exiger_table("prep_data_" %+% an_, "etape_refs", "etape_prep_data()")
    if(any(plan$refs$a_faire & plan$refs$nom %in% REFS_CHRONIQUES)){
      for(an_ in sort(unique(c(if(any(plan$refs$a_faire & plan$refs$nom %in% REFS_CHRONIQUES & !plan$refs$nom %in% REFS_COURTS)) AN_REF, if(any(plan$refs$a_faire & plan$refs$nom %in% REFS_CHRONIQUES & plan$refs$nom %in% REFS_COURTS)) ANS_COURTS))))
        if(!table_temporaire_existe("prep_das_chro_" %+% an_)){ cat("prep_das_chronique(", an_, ") (ref chronique à calculer)\n", sep = ""); prep_das_chronique(an_) }
    }
  }
  n_pivots <- NA_integer_
  for(i in seq_len(nrow(plan$refs))){
    nom <- plan$refs$nom[i]
    fichier <- FICHIER_REF(nom)   # ref_pivots_courts (le tirable) -> 30_courts ; les autres -> 10_references
    if(!plan$refs$a_faire[i]){ cat("ref ", nom, " : présente, sautée\n", sep = ""); next }
    cat("ref ", nom, " : calcul", if(nom %in% REFS_COURTS && !un_seul_an_ref(ANS_COURTS)) " (cumul ANS_COURTS = " %+% paste(ANS_COURTS, collapse = ",") %+% ")" else "", "\n", sep = "")
    df_ref <- FABRIQUES_REFS[[nom]]()
    arrow::write_parquet(df_ref, fichier)
    if(nom == "ref_pivots_courts") n_pivots <- nrow(df_ref)
    cat("  -> ", nrow(df_ref), " lignes -> ", fichier, "\n", sep = "")
    noter_memoire("ref " %+% nom, df_ref)
    rm(df_ref); gc()   # P3.1 : aucune ref ne reste liée à l'environnement global après son écriture
    fichiers <- c(fichiers, fichier)
  }
  purger_cache_e669()  # P3.2 : au cas où ref_das_chronique a été calculée sans distribution_e660
  if(length(setdiff(fichiers, FICHIER_PIVOTS_COURTS())) > 0 || !file.exists(FICHIER_REFERENCES_META())){
    yaml::write_yaml(meta_magasin("references", valeurs_effectives_config(), PROFIL_producteur = PROFIL, refs = as.list(setdiff(NOMS_REFS, "ref_pivots_courts"))), FICHIER_REFERENCES_META())
    fichiers <- c(fichiers, FICHIER_REFERENCES_META())
  }
  if((FICHIER_PIVOTS_COURTS() %in% fichiers || !file.exists(FICHIER_COURTS_META())) && file.exists(FICHIER_PIVOTS_COURTS())){
    if(is.na(n_pivots)) n_pivots <- nrow(arrow::read_parquet(FICHIER_PIVOTS_COURTS()))
    yaml::write_yaml(meta_magasin("courts", valeurs_effectives_config(), PROFIL_producteur = PROFIL, n_pivots = as.integer(n_pivots), fichier = basename(FICHIER_PIVOTS_COURTS()),
                                  note = "le TIRABLE courts (pivots = catalogue des courts), cumul des années ANS_COURTS ; le tirage se fait par campagne (40_campagnes/<C>/chunks_courts)"), FICHIER_COURTS_META())
    fichiers <- c(fichiers, FICHIER_COURTS_META())
  }
  f_mem <- ecrire_diagnostic_memoire()
  banniere_fin("etape_refs", t0, c(fichiers, f_mem))
}

# Étape 3 — partiels des séjours longs : calcul/écriture des partiels manquants (boucle sans
# accumulateur, stats du partiel seul -> diagnostic_apports.csv) + recouvrement.csv. SANS
# agrégation finale. iterations = NULL -> plan complet du profil ; sinon data.frame(etbs, an).
etape_partiels_longs <- function(iterations = NULL){
  t0 <- banniere_debut("etape_partiels_longs", "magasin partagé = " %+% DIR_PARTIELS)
  plan <- plan_courant()
  it <- if(is.null(iterations)) plan$iterations else {
    d <- data.frame(etbs = as.character(iterations$etbs), an = as.integer(iterations$an), stringsAsFactors = FALSE)
    d$fichier <- nom_partiel(d$etbs, d$an); d$a_faire <- !file.exists(file.path(ns(DIR_PARTIELS), d$fichier)); d
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
    fichier <- file.path(ns(DIR_PARTIELS), it$fichier[i])
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
  creer_dossiers(DIR_DIAGNOSTICS); f_apports <- FICHIER_APPORTS()
  utils::write.csv(apports, f_apports, row.names = FALSE)
  cat("== Recouvrement entre partiels ==\n")
  recouvrement <- mesurer_recouvrement(PAIRES_RECOUVREMENT)
  f_rec <- FICHIER_RECOUVREMENT()
  f_mem <- ecrire_diagnostic_memoire()
  banniere_fin("etape_partiels_longs", t0, c(fichiers, f_apports, if(file.exists(f_rec)) f_rec, f_mem))
}

mesurer_recouvrement <- function(paires){
  res <- NULL
  for(p in paires){
    etbs <- p[[1]]; anA <- as.integer(p[[2]]); anB <- as.integer(p[[3]])
    fA <- file.path(ns(DIR_PARTIELS), nom_partiel(etbs, anA)); fB <- file.path(ns(DIR_PARTIELS), nom_partiel(etbs, anB))
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
  if(!is.null(res)){ creer_dossiers(DIR_DIAGNOSTICS); utils::write.csv(res, FICHIER_RECOUVREMENT(), row.names = FALSE) }
  res
}

# Étape 4 — catalogue final en deux étages depuis les partiels du PÉRIMÈTRE PASSÉ EN ARGUMENT
# (ans, etbs : la décision de périmètre, tracée dans le meta.yaml), conversion E669, seuil,
# export catalogue_longs_seuil.parquet (+ meta.yaml) et rapport d'extraction. Ne touche pas la
# base. C'est le « fichier parquet sans les DAS ».
etape_catalogue <- function(ans = ANS_HISTORIQUE, etbs = TYPES_ETBS_LONGS){
  t0 <- banniere_debut("etape_catalogue", "périmètre : " %+% paste(etbs, collapse = ",") %+% " × " %+% paste(range(ans), collapse = "-") %+% " ; SEUIL_PIVOT = " %+% SEUIL_PIVOT)
  ans <- as.integer(ans); etbs <- as.character(etbs)
  creer_dossiers(PATH_RESULTS, DIR_PARTIELS, DIR_REFERENCES, DIR_CATALOGUE_M, DIR_DIAGNOSTICS)
  # Garde du magasin partagé 20_catalogue : méta existant (parts, sinon monofichier) comparé aux paramètres qui le
  # définissent (périmètre demandé, seuil, conversion, clés amont) -> identique : sauté (sauf FORCER_CATALOGUE) ;
  # en écart : stop sauf FORCER_CATALOGUE (alors parts, monofichier et .ancien sont supprimés puis régénérés).
  courant <- valeurs_effectives_config(); courant$ANS_HISTORIQUE <- ans; courant$TYPES_ETBS_LONGS <- etbs
  if(!identical(as.integer(ANS_HISTORIQUE), ans) || !identical(as.character(TYPES_ETBS_LONGS), etbs))
    cat("ATTENTION : périmètre demandé (", paste(etbs, collapse = ","), " × ", paste(ans, collapse = ","), ") différent de la config (ANS_HISTORIQUE / TYPES_ETBS_LONGS) : ",
        "le catalogue produit ne sera chargeable qu'avec une config alignée sur cette décision (garde du magasin).\n", sep = "")
  meta_exist <- lire_meta_si_present(FICHIER_CATALOGUE_META()); if(is.null(meta_exist)) meta_exist <- lire_meta_si_present(FICHIER_CATALOGUE_MONO_META())
  if(!is.null(meta_exist)){
    v <- verifier_magasin("catalogue", meta_exist, courant)
    if(v$ok && !isTRUE(FORCER_CATALOGUE) && (file.exists(MONO_CATALOGUE()) || dir.exists(DIR_CATALOGUE()))){
      cat("catalogue déjà à jour dans le magasin partagé (", DIR_CATALOGUE_M, ", mêmes paramètres) : sauté (FORCER_CATALOGUE <- TRUE pour régénérer)\n", sep = "")
      return(banniere_fin("etape_catalogue", t0, character(0)))
    }
    if(!v$ok && !isTRUE(FORCER_CATALOGUE)) stop("etape_catalogue : " %+% v$message, call. = FALSE)
    if(isTRUE(FORCER_CATALOGUE)){
      cat("FORCER_CATALOGUE : magasin partagé régénéré (parts, monofichier, .ancien supprimés ; relancer etape_repartitionner_catalogue() ensuite", if(!v$ok) " ; écart : " %+% paste(v$differences, collapse = ", ") else "", ")\n", sep = "")
      unlink(DIR_CATALOGUE(), recursive = TRUE); unlink(c(MONO_CATALOGUE(), MONO_CATALOGUE() %+% ".ancien", FICHIER_CATALOGUE_MONO_META()))
    }
  }
  fichiers_partiels <- file.path(ns(DIR_PARTIELS), nom_partiel(rep(etbs, each = length(ans)), rep(ans, times = length(etbs))))
  exiger_fichiers(fichiers_partiels, "etape_catalogue", "etape_partiels_longs()")
  if(CONVERSION_E669){ exiger_fichiers(file.path(ns(DIR_REFERENCES), nom_ref("ref_distribution_e660")), "etape_catalogue", "etape_refs()"); garder_magasin("references", FICHIER_REFERENCES_META(), "etape_catalogue") }
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

  f_cat <- MONO_CATALOGUE()
  arrow::write_parquet(df_prep_scenarios_seuil, f_cat)
  plan <- if(exists("plan", envir = ETAPES_ENV, inherits = FALSE)) get("plan", envir = ETAPES_ENV, inherits = FALSE) else NULL
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
  meta_catalogue$ANS_HISTORIQUE <- as.integer(ans); meta_catalogue$TYPES_ETBS_LONGS <- etbs   # trace de la décision de périmètre (clé du magasin)
  meta_catalogue$magasin <- "catalogue"
  f_meta <- FICHIER_CATALOGUE_MONO_META()
  yaml::write_yaml(meta_catalogue, f_meta)

  # Rapport d'extraction : périmètre, catalogue, impact E669, apports, recouvrement, mémoire
  lire_csv <- function(f) if(file.exists(f)) utils::read.csv(f) else NULL
  lignes_rx <- c("RAPPORT D'EXTRACTION — etape_catalogue — " %+% as.character(Sys.Date()) %+% " — PROFIL = " %+% PROFIL,
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
  lignes_rx <- c(lignes_rx, "", "== 4. Apports par partiel (90_diagnostics/diagnostic_apports.csv) ==", fmt_df(lire_csv(FICHIER_APPORTS())),
                 "", "== 5. Recouvrement entre partiels (90_diagnostics/recouvrement.csv) ==", fmt_df(lire_csv(FICHIER_RECOUVREMENT())),
                 "", "== 6. Mémoire (" %+% basename(FICHIER_DIAG_MEMOIRE()) %+% " ; SEUIL_ALERTE_GO = " %+% SEUIL_ALERTE_GO %+% ") ==", fmt_df(get("journal", envir = MEMOIRE_ENV)))
  f_mem <- ecrire_diagnostic_memoire()
  f_rx <- FICHIER_RAPPORT_EXTRACTION()
  writeLines(lignes_rx, f_rx)
  cat("Rapport d'extraction : ", f_rx, "\n", sep = "")
  rm(df_prep_scenarios_seuil); gc()
  banniere_fin("etape_catalogue", t0, c(f_cat, f_meta, f_rx, f_mem))
}

# Étape 4b — REPARTITIONNEMENT + ENRICHISSEMENT one-shot du catalogue longs : lit le monofichier
# par morceaux de lettres (jamais tout en RAM), ajoute lettre / DPEC / TPEC (typologie versionnée),
# écrit 20_catalogue/catalogue_longs_seuil/part_<L>.parquet + _meta.yaml (clés du magasin), vérifie nb_lignes ==
# méta, renomme le monofichier en .ancien (jamais supprimé). Idempotente ; version de typologie
# différente du sidecar -> stop proposant de re-repartitionner. Sans base.
etape_repartitionner_catalogue <- function(){
  t0 <- banniere_debut("etape_repartitionner_catalogue", "catalogue -> parts par lettre + DPEC/TPEC (typologie " %+% charger_typo()$version %+% ")")
  typo <- charger_typo(); dir_ds <- DIR_CATALOGUE(); mono <- MONO_CATALOGUE(); f_side <- FICHIER_CATALOGUE_META()
  if(file.exists(f_side)){
    side <- yaml::read_yaml(f_side)
    garder_magasin("catalogue", f_side, "etape_repartitionner_catalogue")
    if(!identical(as.character(side$version_typologie), as.character(typo$version)))
      stop("etape_repartitionner_catalogue : le catalogue partitionné a été typé avec la typologie " %+% side$version_typologie %+% " (courante : " %+% typo$version %+%
           "). Supprimez " %+% dir_ds %+% " (le monofichier .ancien est conservé : renommez-le en catalogue_longs_seuil.parquet) et relancez pour re-repartitionner.", call. = FALSE)
    if(!identical(as.character(side$version_recette_id), RECETTE_ID))
      stop("etape_repartitionner_catalogue : le catalogue partitionné ne porte pas id_profil en recette " %+% RECETTE_ID %+% " (sidecar : " %+% paste(side$version_recette_id, collapse = "") %+%
           "). Supprimez " %+% dir_ds %+% ", renommez le .ancien en catalogue_longs_seuil.parquet et relancez (chantier campagnes : id_profil requis). Si un registre existe déjà, le recalculer par rétro-inscription.", call. = FALSE)
    cat("catalogue partitionné déjà présent (", length(side$nb_lignes_par_part), " parts, typologie ", side$version_typologie, ") : sauté\n", sep = "")
    return(banniere_fin("etape_repartitionner_catalogue", t0, character(0)))
  }
  if(!file.exists(mono)) stop(message_catalogue_absent("etape_repartitionner_catalogue", DIR_CATALOGUE_M), call. = FALSE)
  f_meta <- FICHIER_CATALOGUE_MONO_META()
  meta <- if(file.exists(f_meta)) yaml::read_yaml(f_meta) else NULL
  if(!is.null(meta)) garder_magasin("catalogue", f_meta, "etape_repartitionner_catalogue")
  if(!dir.exists(dir_ds)) dir.create(dir_ds, recursive = TRUE)
  # lettres présentes (colonne légère) puis lecture par lettre
  lettres <- if(arrow_dataset_disponible()) sort(unique(lettre_de((arrow::open_dataset(mono) |> dplyr::select(diag2) |> dplyr::collect())$diag2))) else sort(unique(lettre_de(arrow::read_parquet(mono)$diag2)))
  nb_par_part <- list(); poids_par_part <- list(); eff_dpec <- NULL; fichiers <- character(0); ids_vus <- character(0)
  for(L in lettres){
    d <- if(arrow_dataset_disponible()) tibble::as_tibble(arrow::open_dataset(mono) |> dplyr::collect()) else tibble::as_tibble(arrow::read_parquet(mono))
    d <- d[lettre_de(d$diag2) == L, , drop = FALSE]
    d$lettre <- L
    d <- typologie_sejour(d, typo, col_age = "age", col_duree = "duree", duree_defaut = 3)
    d$id_profil <- id_profil_de(d)                                     # recette RECETTE_ID (helpers G1)
    if(anyDuplicated(d$id_profil)) stop("etape_repartitionner_catalogue : id_profil dupliqué dans la lettre " %+% L %+% " (collision de hash tronqué ou clé pivots × graine non unique).", call. = FALSE)
    ids_vus <- c(ids_vus, d$id_profil)
    f <- file.path(dir_ds, nom_part_lettre(L)); arrow::write_parquet(d, f); fichiers <- c(fichiers, f)
    nb_par_part[[L]] <- nrow(d); poids_par_part[[L]] <- sum(d$poids)
    eff_dpec <- dplyr::bind_rows(eff_dpec, dplyr::count(d, DPEC, name = "n")) |> dplyr::summarise(n = sum(n), .by = DPEC)
    noter_memoire("repartitionnement lettre " %+% L, d)
    cat(sprintf("  lettre %s : %d lignes\n", L, nrow(d))); rm(d); gc()
  }
  total <- sum(unlist(nb_par_part))
  if(anyDuplicated(ids_vus)) stop("etape_repartitionner_catalogue : id_profil dupliqué entre lettres (collision de hash tronqué) : " %+% paste(unique(ids_vus[duplicated(ids_vus)])[1:5], collapse = ", "), call. = FALSE)
  rm(ids_vus); gc()
  if(!is.null(meta) && !is.null(meta$nb_lignes) && total != meta$nb_lignes)
    stop(sprintf("etape_repartitionner_catalogue : %d lignes écrites != %s lignes au méta du catalogue.", total, meta$nb_lignes), call. = FALSE)
  cles_mag <- if(!is.null(meta)) meta[intersect(CLES_MAGASINS$catalogue, names(meta))] else valeurs_effectives_config()[CLES_MAGASINS$catalogue]
  side <- c(list(magasin = "catalogue", produit = "catalogue_longs_seuil (partitionné par lettre)", date = as.character(Sys.Date()), provenance = basename(mono)),
            cles_mag,
            list(nb_lignes_total = total, nb_lignes_par_part = nb_par_part, sum_poids_par_part = poids_par_part,
                 effectifs_dpec = stats::setNames(as.list(eff_dpec$n), eff_dpec$DPEC), version_typologie = typo$version, version_recette_id = RECETTE_ID,
                 duree_constante_longs = 3L, note = "longs : duree = 3 constante (périmètre 3-100), age = pivot age"))
  yaml::write_yaml(side, f_side)
  file.rename(mono, mono %+% ".ancien")
  cat("monofichier renommé en ", basename(mono), ".ancien (conservé) ; ", total, " lignes en ", length(lettres), " parts\n", sep = "")
  banniere_fin("etape_repartitionner_catalogue", t0, c(fichiers, f_side))
}

## ---- 3. Famille TIRAGE (aucun appel pRatihque) ----

# Contexte de tirage chargé une fois par session (refs parquet, REFS, libellés) ; `requis` =
# produits à exiger pour l'étape appelante (message listant les manquants).
charger_contexte_tirage <- function(requis, etape){
  if("catalogue" %in% requis){
    requis <- setdiff(requis, "catalogue")
    if(!(dir.exists(DIR_CATALOGUE()) && length(list.files(DIR_CATALOGUE(), pattern = "^part_")) > 0) && !file.exists(MONO_CATALOGUE()))
      stop(message_catalogue_absent(etape, DIR_CATALOGUE_M), call. = FALSE)
    garder_magasin("catalogue", if(file.exists(FICHIER_CATALOGUE_META())) FICHIER_CATALOGUE_META() else FICHIER_CATALOGUE_MONO_META(), etape)
  }
  requis <- sub("\\.parquet$", "", setdiff(requis, "catalogue_longs_seuil_meta.yaml"))
  if("ref_pivots_courts" %in% requis){ exiger_fichiers(FICHIER_PIVOTS_COURTS(), etape, "extraction (etape_refs) : le tirable courts, magasin 30_courts"); garder_magasin("courts", FICHIER_COURTS_META(), etape) }
  autres <- setdiff(requis, "ref_pivots_courts")
  if(length(autres)) { exiger_fichiers(vapply(autres, FICHIER_REF, character(1)), etape, "extraction (etape_refs)"); garder_magasin("references", FICHIER_REFERENCES_META(), etape) }
  if(!exists("ctx", envir = ETAPES_ENV, inherits = FALSE)){
    f_meta <- if(file.exists(FICHIER_CATALOGUE_META())) FICHIER_CATALOGUE_META() else FICHIER_CATALOGUE_MONO_META()
    meta_catalogue <- if(file.exists(f_meta)) yaml::read_yaml(f_meta) else NULL
    conv <- if(is.null(meta_catalogue)) CONVERSION_E669 else isTRUE(meta_catalogue$CONVERSION_E669)
    codes_imprecis <- codes_imprecis_de_cim(cim, MOTIF_IMPRECIS)
    if(conv) codes_imprecis <- codes_imprecis[!grepl("^E669", codes_imprecis)]
    paires <- list()
    if(file.exists(PATH_PAIRES_EXCLUES)) paires <- lapply(yaml::read_yaml(PATH_PAIRES_EXCLUES), as.character) else warning("Fichier absent : " %+% PATH_PAIRES_EXCLUES %+% " ; aucune paire exclue.")
    assign("ctx", list(meta_catalogue = meta_catalogue, conversion_e669 = conv, codes_imprecis = codes_imprecis, lib_cim = libelles_cim(cim), paires_exclues = paires), envir = ETAPES_ENV)
  }
  ctx <- get("ctx", envir = ETAPES_ENV, inherits = FALSE)
  lire <- function(nom) arrow::read_parquet(FICHIER_REF(nom))
  if(is.null(ctx$REFS) && "ref_comp_diabete" %in% requis){
    ctx$REFS <- construire_refs(comp_diabete = penaliser_comp_diabete(lire("ref_comp_diabete"), CAGE_PED, CAGE_AGES, PENALITE_9_AGES, PENALITE_9_AUTRES),
                                codes_diab = codes_diab, codes_comp_sat_diab = codes_comp_sat_diab, hta_autres = hta_autres,
                                code_did = code_did, code_dnid_ins = code_dnid_ins, code_dnid = code_dnid,
                                neo_codes = neo_codes_diabete, paires_exclues = ctx$paires_exclues)
    assign("ctx", ctx, envir = ETAPES_ENV)
  }
  ctx
}
# inherits = FALSE : l'état de session ne doit JAMAIS hériter d'un objet global homonyme (rapport, revue, selection… laissés
# par un script inline ou une session précédente) — fuite constatée au lot « notebook campagnes » (journal 20.2).
etat_tirage <- function(nom, defaut = NULL) if(exists(nom, envir = ETAPES_ENV, inherits = FALSE)) get(nom, envir = ETAPES_ENV, inherits = FALSE) else defaut
poser_tirage <- function(nom, valeur) assign(nom, valeur, envir = ETAPES_ENV)
stats_branche <- function(df, pivots, codes_imprecis, cols_e669){
  list(n = nrow(df), pivots = nrow(dplyr::distinct(df[, pivots])),
       distribution = distribution_nb_das(df), top_das = top_das_par_cmd(df, 30),
       taux_imprecis = taux_imprecis(df, codes_imprecis),
       controles = controler_scenarios(df, hta_autres, SEUIL_PIVOT),
       e669_residuels = compter_e669(df, cols_e669),
       e660 = effectifs_e660(df, c("diag2", "diagnostic_associes")))
}

# Étape T1 — séjours courts : ÉTAPE DE CAMPAGNE (chantier « courts en campagnes » : même statut que les longs dans le corpus).
# Tirable = 30_courts/ref_pivots_courts.parquet (magasin partagé, cumul ANS_COURTS, gardé) ; budget de la campagne =
# NB_CRH_CIBLE_COURTS (absolu) ou RATIO_COURTS × volume longs attendu (sélection de la campagne, donc APRÈS
# etape_selection_longs) ; budget réparti sur les pivots AU POIDS (n), variantes numérotées PAR PIVOT après la variante_max
# du registre (recyclage = variantes nouvelles directement, hash_das enregistrés exclus, aucun re-tirage), dédoublonnage
# souple ; chunks sous <profil>/40_campagnes/<C>/chunks_courts/ (transitoires) ; typologie DPEC/TPEC (vraie durée, âge tiré),
# population, habillage admin (v_admin_courts, stop nominatif si aucun candidat), scénarios habillés dans
# <C>/habille/courts/scenarios_courts.parquet + _meta.yaml. chunk_range = c(i, j) : plage seulement (parallélisme par
# sessions sur plages disjointes, sidecar partagé), assemblage à l'appel final sans plage ; débit par chunk.
etape_tirage_courts <- function(chunk_range = NULL, budget = NB_CRH_CIBLE_COURTS, ratio = RATIO_COURTS){
  mt <- lire_meta_si_present(FICHIER_SELECTION_META())
  bc <- budget_courts(budget, ratio, if(is.null(mt)) NULL else mt$volume_attendu)
  cat("\n==== CAMPAGNE = ", CAMPAGNE, " ; budget courts = ", bc$budget, " (source : ", bc$source, " — ", bc$detail, ") ; surcharge active : ", surcharge_active(), "\n", sep = "")
  t0 <- banniere_debut("etape_tirage_courts", "campagne = " %+% CAMPAGNE %+% " ; budget courts = " %+% bc$budget %+% " (" %+% bc$source %+% ") ; tirable = " %+% FICHIER_PIVOTS_COURTS() %+% " (ANS_COURTS = " %+% paste(ANS_COURTS, collapse = ",") %+%
                        ") ; chunking dynamique (NB_CHUNKS_MAX = " %+% NB_CHUNKS_MAX %+% ", CHUNK_SIZE_MIN = " %+% CHUNK_SIZE_MIN %+% ", CHUNK_SIZE_FIXE = " %+% CHUNK_SIZE_FIXE %+% ")" %+%
                        if(!is.null(chunk_range)) " ; plage " %+% chunk_range[1] %+% ".." %+% chunk_range[2] %+% " (session parallèle)" else "")
  ctx <- charger_contexte_tirage(c("ref_pivots_courts", "ref_das_chronique", "ref_nb_chroniques", "ref_comp_diabete", "ref_v_admin_courts"), "etape_tirage_courts")
  registre <- if(isTRUE(REGISTRE_ACTIF)) lire_registre(DIR_REGISTRE()) else NULL
  f_side <- file.path(DIR_CHUNKS_COURTS(), "courts_chunks_meta.yaml")
  if(!is.null(registre)){
    st <- statut_campagne_registre(CAMPAGNE, registre, "court")
    if(st$inscrite && !file.exists(f_side)) stop("etape_tirage_courts : campagne " %+% CAMPAGNE %+% " " %+% sub(" — changez d'identifiant$", "", st$texte) %+% " : campagne CLOSE côté courts (aucun chunk courts sur disque). Aucun re-tirage possible ; ouvrez une nouvelle campagne (RUN_aval.Rmd §3). Le registre ne se vide jamais.", call. = FALSE)
    if(st$inscrite) cat("campagne ", CAMPAGNE, " : courts déjà inscrits au registre (", st$nb_courts, " scénarios) ; chunks présents relus, aucun nouveau tirage (reprise sûre)\n", sep = "")
    else cat("campagne ", CAMPAGNE, " (courts) : ", st$texte, " ; registre : ", registre$nb_campagnes, " campagne(s), ", sum(registre$lignes$branche == "court"), " scénarios courts, ", sum(grepl("^k", registre$par_profil$id_profil)), " pivots consommés\n", sep = "")
  }
  lire <- function(nom) arrow::read_parquet(FICHIER_REF(nom))
  df_pivots <- lire("ref_pivots_courts"); df_nb_chroniques <- lire("ref_nb_chroniques")
  ref_chro <- prep_ref_chronique(lire("ref_das_chronique")); df_v_admin_courts <- lire("ref_v_admin_courts")
  p <- pivots_sous_registre(df_pivots[, c(PIVOTS_COURTS, "nb")], if(is.null(registre)) NULL else registre$par_profil)
  p$nb_tirages <- repartir_budget_pivots(bc$budget, p$nb)
  p <- p[p$nb_tirages > 0, , drop = FALSE]
  cat(sprintf("== Séjours courts %s : tirable %d pivots, %d tirés (budget %d réparti au poids ; vierges = %d, recyclés = %d) ; seed_base = %d ==\n",
              CAMPAGNE, nrow(df_pivots), nrow(p), bc$budget, sum(p$origine_profil == "vierge"), sum(p$origine_profil == "recycle"), seed_courts()))
  df_tirage <- pmap_chunks(p[, c(PIVOTS_COURTS, "nb", "nb_tirages", "id_profil", "variante_debut", "hash_exclus")], sample_das_court,
                           chunk_size = NULL, dossier = DIR_CHUNKS_COURTS(), prefixe = "courts", seed_base = seed_courts(),
                           garder_chunks = TRUE, chunk_range = chunk_range,
                           ref_chro = ref_chro, ref_nb_chro = df_nb_chroniques, refs = ctx$REFS, seuil_ref = SEUIL_REF_DAS,
                           cibles_defaut = CIBLES_NB_CHRONIQUES, age_max = AGE_MAX_OUVERT, dedoublonner = TRUE)
  if(!is.null(chunk_range)){
    cat("plage ", chunk_range[1], "..", chunk_range[2], " tirée ; assemblage, habillage et écriture à la fin : relancer etape_tirage_courts() sans plage (chunks présents sautés)\n", sep = "")
    return(banniere_fin("etape_tirage_courts", t0, list.files(DIR_CHUNKS_COURTS(), pattern = "^courts_chunk_.*\\.parquet$", full.names = TRUE)))
  }
  n_gardes <- nrow(df_tirage)
  if(n_gardes == 0) stop("etape_tirage_courts : aucun scénario court produit (strates de référence vides ou tout exclu par le registre).", call. = FALSE)
  # Typologie (vraie durée, âge tiré), population, lettre, campagne — parité avec les longs
  typo <- charger_typo()
  df_tirage <- typologie_sejour(df_tirage, typo, col_age = "age", col_duree = "duree")
  df_tirage$population <- population_de(as.character(df_tirage$cage), POPULATIONS); df_tirage$lettre <- lettre_de(df_tirage$diag2); df_tirage$campagne <- CAMPAGNE
  # Habillage admin : NB_VARIANTES_ADMIN_COURTS variantes par scénario, strate = les 6 pivots (même périmètre ANS_COURTS que v_admin_courts) ; jamais de NA silencieux
  set.seed(seed_courts() + 1e6)
  df_scenarios <- habiller_admin(df_tirage, df_v_admin_courts, niveaux = list(PIVOTS_COURTS), cols_apport = COLS_ADMIN, nb_variantes = NB_VARIANTES_ADMIN_COURTS, etiquette = "courts")
  rm(df_tirage)
  creer_dossiers(DIR_HABILLE_COURTS()); f_out <- FICHIER_COURTS_CAMPAGNE()
  arrow::write_parquet(df_scenarios, f_out)
  meta <- list(campagne = CAMPAGNE, PROFIL = PROFIL, date = as.character(Sys.Date()), budget = bc$budget, source_budget = bc$source, detail_budget = bc$detail,
               RATIO_COURTS = ratio, NB_CRH_CIBLE_COURTS = budget, volume_longs_attendu = if(is.null(mt)) NA else mt$volume_attendu,
               n_pivots_tirable = nrow(df_pivots), n_pivots_tires = nrow(p), n_pivots_vierges = sum(p$origine_profil == "vierge"), n_pivots_recycles = sum(p$origine_profil == "recycle"),
               scenarios_demandes = bc$budget, scenarios_gardes = n_gardes, scenarios_elimines = bc$budget - n_gardes, n_lignes = nrow(df_scenarios),
               ANS_COURTS = as.integer(ANS_COURTS), recette_id_courts = RECETTE_ID_COURTS, REGISTRE_ACTIF = isTRUE(REGISTRE_ACTIF), seed_base = seed_courts(),
               NB_VARIANTES_ADMIN_COURTS = NB_VARIANTES_ADMIN_COURTS, fichier = basename(f_out))
  yaml::write_yaml(meta, FICHIER_COURTS_CAMPAGNE_META())
  cat(sprintf("- Séjours courts %s : %d scénarios gardés / %d demandés (éliminés : doublons, hash du registre, strates vides = %d) -> %d lignes habillées -> %s (+ _meta.yaml)\n",
              CAMPAGNE, n_gardes, bc$budget, bc$budget - n_gardes, nrow(df_scenarios), f_out))
  rapport <- etat_tirage("rapport", list()); rapport$courts_meta <- meta; poser_tirage("rapport", rapport)
  rm(df_scenarios); gc()
  banniere_fin("etape_tirage_courts", t0, c(f_out, FICHIER_COURTS_CAMPAGNE_META()))
}

# Étape T2 — sélection des séjours longs, figée sous seed AVANT le premier chunk.
#  - quota_dp (diagnostic, avec remise) : selection/selection_longs.parquet + selection/_meta.yaml (logique historique) ;
#  - quota_dp_fixe (production par campagnes) : k = NB_LIGNES_PAR_DP lignes distinctes par DP au poids
#    SANS remise, variantes déduites (n_var = ceiling(X_dp / k)), PAR POPULATION (budget au prorata du
#    nb de DP), lettre par lettre (pic RAM = une lettre), plafonds PLAFONDS_DPEC par (DP × DPEC plafonné),
#    planchers d'unités seulement si k >= nb de types (sinon désactivés, mention) ;
#    sortie 40_campagnes/<C>/selection/<population>/part_<L>.parquet + effectifs csv + _meta.yaml par population et global ;
#  - catalogue_complet : retiré pour ce corpus -> stop() si budget < nb lignes du catalogue.
etape_selection_longs <- function(budget = NB_CRH_CIBLE, mode = MODE_SELECTION, k = NB_LIGNES_PAR_DP){
  t0 <- banniere_debut("etape_selection_longs", "mode = " %+% mode %+% " ; budget (NB_CRH_CIBLE) = " %+% budget %+% if(mode == "quota_dp_fixe") " ; k = " %+% k else "")
  if(!mode %in% c("catalogue_complet", "quota_dp", "quota_dp_fixe")) stop("etape_selection_longs : mode inconnu : " %+% mode)
  # Garde-fou précoce (§5) : sous registre, une campagne déjà inscrite ne se resélectionne pas — AVANT tout calcul
  if(mode == "quota_dp_fixe" && isTRUE(REGISTRE_ACTIF)){
    st <- statut_campagne_registre(CAMPAGNE, lire_registre(DIR_REGISTRE()))
    if(st$inscrite){
      # Q49 ACTÉE : campagne inscrite = close. Sélection PRÉSENTE de la MÊME campagne -> relecture (reprise sûre, no-op) ;
      # sinon (aucune sélection, ou sélection d'une autre campagne) -> stop. Aucun re-tirage possible dans les deux cas.
      f_mt <- FICHIER_SELECTION_META(); mt0 <- if(file.exists(f_mt)) yaml::read_yaml(f_mt) else NULL
      sel_meme <- !is.null(mt0) && identical(as.character(mt0$CAMPAGNE), as.character(CAMPAGNE)) &&
        all(vapply(names(POPULATIONS), function(pp) file.exists(FICHIER_SELECTION_POP_META(pp)), logical(1)))
      if(!sel_meme) stop("etape_selection_longs : campagne " %+% CAMPAGNE %+% " " %+% sub(" — changez d'identifiant$", "", st$texte) %+% " : campagne CLOSE (" %+%
                         (if(is.null(mt0)) "aucune sélection sur disque" else "la sélection présente porte la campagne " %+% mt0$CAMPAGNE) %+%
                         "). Aucun re-tirage possible ; ouvrez une nouvelle campagne — section 3 du notebook RUN_aval.Rmd (CAMPAGNE <- \"Cn\", Restart R, vidage gardé). Le registre ne se vide jamais.", call. = FALSE)
      cat("campagne ", CAMPAGNE, " déjà inscrite au registre (", st$nb, " scénarios le ", st$date, ") ; sélection relue, aucune nouvelle sélection (reprise sûre)\n", sep = "")
    } else cat("campagne ", CAMPAGNE, " : ", st$texte, "\n", sep = "")
  }
  ctx <- charger_contexte_tirage(c("catalogue", "catalogue_longs_seuil_meta.yaml"), "etape_selection_longs")
  creer_dossiers(DIR_SELECTION()); FICHIER_META_TIRAGE <- FICHIER_SELECTION_META()
  rapport <- etat_tirage("rapport", list())
  fichiers <- character(0)
  if(mode == "quota_dp_fixe"){
    typo <- charger_typo(); dir_ds <- DIR_CATALOGUE(); mono <- MONO_CATALOGUE()
    lettres <- lettres_catalogue(dir_ds, mono)
    if(length(lettres) == 0) stop("etape_selection_longs : catalogue vide.", call. = FALSE)
    # nb de DP par population (colonnes légères, par lettre) ; partition des cages vérifiée
    dp_pop <- purrr::map(lettres, function(L){
      d <- lire_catalogue(dir_ds, mono, L, c("diag2", "cage")); verifier_populations(POPULATIONS, unique(as.character(d$cage)))
      d$population <- population_de(d$cage, POPULATIONS); dplyr::distinct(d, population, diag2)
    }) |> purrr::list_rbind() |> dplyr::distinct()
    nb_dp <- vapply(names(POPULATIONS), function(pp) sum(dp_pop$population == pp), integer(1))
    budgets <- repartir_budget_populations(as.integer(budget), nb_dp)
    X <- as.integer(ifelse(nb_dp > 0, ceiling(budgets / pmax(nb_dp, 1)), 0L))
    names(X) <- names(POPULATIONS)
    side_cat <- lire_meta_si_present(FICHIER_CATALOGUE_META())
    if(isTRUE(REGISTRE_ACTIF) && (is.null(side_cat) || !identical(as.character(side_cat$version_recette_id), RECETTE_ID)))
      stop("etape_selection_longs : campagne sous registre impossible, le catalogue partitionné ne porte pas id_profil (recette " %+% RECETTE_ID %+% "). Relancez etape_repartitionner_catalogue() (garde-fou).", call. = FALSE)
    registre <- if(isTRUE(REGISTRE_ACTIF)) lire_registre(DIR_REGISTRE()) else NULL
    if(!is.null(registre)) cat(sprintf("registre : %d campagne(s), %d scénarios, %d profils consommés\n", registre$nb_campagnes, registre$nb_scenarios, nrow(registre$par_profil)))
    meta_global <- list(MODE_SELECTION = mode, NB_CRH_CIBLE = as.integer(budget), NB_LIGNES_PAR_DP = as.integer(k),
                        CAMPAGNE = CAMPAGNE, REGISTRE_ACTIF = isTRUE(REGISTRE_ACTIF), RECETTE_ID = RECETTE_ID,
                        NB_CHUNKS_MAX = as.integer(NB_CHUNKS_MAX), CHUNK_SIZE_MIN = as.integer(CHUNK_SIZE_MIN), CHUNK_SIZE_FIXE = as.integer(CHUNK_SIZE_FIXE),
                        SEED = as.integer(SEED), version_typologie = typo$version, populations = names(POPULATIONS),
                        PLAFONDS_DPEC = PLAFONDS_DPEC, nrow_catalogue = sum(unlist(if(!is.null(side_cat)) side_cat$nb_lignes_par_part else list(NA))))
    CLES <- c("MODE_SELECTION", "NB_CRH_CIBLE", "NB_LIGNES_PAR_DP", "CAMPAGNE", "REGISTRE_ACTIF", "RECETTE_ID", "NB_CHUNKS_MAX", "CHUNK_SIZE_MIN", "CHUNK_SIZE_FIXE", "SEED", "version_typologie")
    meta_existant <- if(file.exists(FICHIER_META_TIRAGE)) yaml::read_yaml(FICHIER_META_TIRAGE) else NULL
    msg <- verifier_meta_tirage(meta_existant, meta_global, CLES)
    if(!is.null(msg)) stop(msg, call. = FALSE)
    metas_pop <- list(); stats_tot <- NULL
    for(pop in names(POPULATIONS)){
      dir_pop <- DIR_SELECTION(pop); if(!dir.exists(dir_pop)) dir.create(dir_pop, recursive = TRUE)
      f_meta_pop <- FICHIER_SELECTION_POP_META(pop)
      if(file.exists(f_meta_pop) && length(list.files(dir_pop, pattern = "^part_")) > 0){
        cat("== Sélection ", pop, " : présente (", f_meta_pop, "), relue\n", sep = "")
        metas_pop[[pop]] <- yaml::read_yaml(f_meta_pop); next
      }
      cat(sprintf("== Sélection %s : %d DP, budget %d, X = %d par DP, k = %d ; campagne %s (registre %s) ==\n", pop, nb_dp[[pop]], budgets[[pop]], X[[pop]], k, CAMPAGNE, if(isTRUE(REGISTRE_ACTIF)) "actif" else "inactif"))
      # Plafonds de CLASSE DPEC (amendement Q33) : pré-passe sur les lignes de la population appartenant aux classes plafonnées
      quotas_classe <- NULL; resume_classes <- NULL
      if(length(PLAFONDS_DPEC) > 0){
        lc <- purrr::map(lettres, function(L){ d <- lire_catalogue(dir_ds, mono, L, c("diag2", "cage", "DPEC", "poids")); d[population_de(d$cage, POPULATIONS) == pop & d$DPEC %in% names(PLAFONDS_DPEC), , drop = FALSE] }) |> purrr::list_rbind()
        for(cl in names(PLAFONDS_DPEC)){
          a <- allocation_classe_plafonnee(lc[lc$DPEC == cl, , drop = FALSE], PLAFONDS_DPEC[[cl]])
          if(a$nb_dp > 0) quotas_classe <- dplyr::bind_rows(quotas_classe, dplyr::mutate(a$quotas, DPEC = cl))
          resume_classes <- dplyr::bind_rows(resume_classes, data.frame(population = pop, classe = cl, nb_dp = a$nb_dp, plafond = a$plafond, total_retenu = a$total, depassement = a$depassement))
          cat(sprintf("   classe plafonnée « %s » : %d DP, plafond %d -> total retenu %d%s\n", cl, a$nb_dp, a$plafond, a$total, if(a$depassement > 0) sprintf(" (DÉPASSEMENT : 1 représentant par DP prime, +%d)", a$depassement) else ""))
        }
        rm(lc)
      }
      if(X[[pop]] * nb_dp[[pop]] > budgets[[pop]]) cat(sprintf("   minimum %d par DP (X = ceiling(budget / nb_DP)) : volume final = X × nb_DP = %d > budget %d (plafonds en moins)\n", X[[pop]], X[[pop]] * nb_dp[[pop]], budgets[[pop]]))
      if(k < 2) cat("   planchers par type d'unité INACTIFS au quota courant (k = ", k, " < nb de types) : la couverture des unités relèvera du registre inter-campagnes\n", sep = "")
      stats_pop <- NULL; eff <- NULL; vol <- 0L
      for(L in lettres){
        d <- lire_catalogue(dir_ds, mono, L)
        d$population <- population_de(d$cage, POPULATIONS); d <- d[d$population == pop, , drop = FALSE]
        if(nrow(d) == 0) next
        if(!"DPEC" %in% names(d)) d <- typologie_sejour(d, typo, col_age = "age", col_duree = "duree", duree_defaut = 3)
        if(!"lettre" %in% names(d)) d$lettre <- L
        set.seed(seed_selection(seed_base_campagne(), pop, L, POPULATIONS))   # seed stable par (campagne, population, lettre)
        r <- selection_campagne_lettre(d, X[[pop]], k, quotas_classe, if(is.null(registre)) NULL else registre$par_profil)
        if(nrow(r$selection) > 0){
          r$selection$id_selection <- paste0(L, "_", seq_len(nrow(r$selection)))
          r$selection$campagne <- CAMPAGNE
          f <- file.path(dir_pop, nom_part_lettre(L)); arrow::write_parquet(r$selection, f); fichiers <- c(fichiers, f)
          vol <- vol + sum(r$selection$n_var)
          eff <- dplyr::bind_rows(eff, r$selection |> dplyr::summarise(lignes = dplyr::n(), variantes = sum(n_var), .by = c(diag2, DPEC, type_unite, origine_profil)))
        }
        stats_pop <- dplyr::bind_rows(stats_pop, r$stats)
        rm(d, r); gc()
      }
      f_eff <- file.path(dir_pop, "selection_longs_effectifs.csv"); utils::write.csv(eff, f_eff, row.names = FALSE); fichiers <- c(fichiers, f_eff)
      f_st <- file.path(dir_pop, "selection_longs_stats_dp.csv"); utils::write.csv(stats_pop, f_st, row.names = FALSE); fichiers <- c(fichiers, f_st)
      classes_pop <- if(is.null(resume_classes)) list() else lapply(split(resume_classes, seq_len(nrow(resume_classes))), function(r) as.list(r[, c("classe", "nb_dp", "plafond", "total_retenu", "depassement")]))
      meta_pop <- c(meta_global, list(population = pop, cages = POPULATIONS[[pop]], budget_population = budgets[[pop]], nb_dp = nb_dp[[pop]], X = X[[pop]], k = as.integer(k),
                                      classes_plafonnees = classes_pop,
                                      dp_vierges = if(is.null(stats_pop)) 0L else sum(stats_pop$nb_recycles == 0 & stats_pop$groupe == ".reste"),
                                      dp_recycles = if(is.null(stats_pop)) 0L else sum(stats_pop$nb_recycles > 0),
                                      dp_partiellement_consommes = if(is.null(stats_pop)) 0L else sum(stats_pop$nb_recycles == 0 & stats_pop$nb_vierges < stats_pop$lignes_disponibles),
                                      volume_attendu = as.integer(vol), plafonds_appliques = if(is.null(stats_pop)) 0L else sum(stats_pop$plafonne),
                                      lignes_plafonnees_par_dpec = if(is.null(stats_pop)) list() else as.list(stats::setNames(tapply(stats_pop$variantes[stats_pop$plafonne], stats_pop$groupe[stats_pop$plafonne], sum), unique(stats_pop$groupe[stats_pop$plafonne]))),
                                      planchers_unites_desactives = if(is.null(stats_pop)) 0L else sum(!stats_pop$planchers_actifs & stats_pop$lignes_disponibles > 1),
                                      manque_a_gagner = if(is.null(stats_pop)) 0L else sum(stats_pop$manque_a_gagner),
                                      seed_par_lettre = "SEED + 7e6 + 1e4 * index(population) + utf8ToInt(lettre)", date = as.character(Sys.Date())))
      yaml::write_yaml(meta_pop, f_meta_pop); fichiers <- c(fichiers, f_meta_pop); metas_pop[[pop]] <- meta_pop
      cat(sprintf("  %s : %d lignes sélectionnées, %d variantes attendues ; plafonds appliqués = %d ; manque à gagner = %d\n", pop,
                  if(is.null(eff)) 0L else sum(eff$lignes), vol, meta_pop$plafonds_appliques, meta_pop$manque_a_gagner))
      stats_tot <- dplyr::bind_rows(stats_tot, if(!is.null(stats_pop)) cbind(population = pop, stats_pop))
    }
    meta_global$volume_attendu <- sum(vapply(metas_pop, function(m) as.integer(m$volume_attendu), integer(1)))
    meta_global$par_population <- lapply(metas_pop, function(m) m[c("budget_population", "nb_dp", "X", "volume_attendu", "manque_a_gagner", "plafonds_appliques")])
    meta_global$PROFIL <- PROFIL; meta_global$date <- as.character(Sys.Date())
    yaml::write_yaml(meta_global, FICHIER_META_TIRAGE); fichiers <- c(fichiers, FICHIER_META_TIRAGE)
    rapport$selection_fixe <- list(metas = metas_pop, stats = stats_tot, registre_avant = if(is.null(registre)) NULL else registre$par_dpec)
    poser_tirage("meta_tirage", meta_global); poser_tirage("rapport", rapport); poser_tirage("mode_tirage", mode)
    cat(sprintf("== Volume attendu total = %d (cible %d) ==\n", meta_global$volume_attendu, budget))
    return(banniere_fin("etape_selection_longs", t0, fichiers))
  }
  df_prep_scenarios_seuil <- lire_catalogue(DIR_CATALOGUE(), MONO_CATALOGUE())
  if(mode == "catalogue_complet" && budget < nrow(df_prep_scenarios_seuil))
    stop(sprintf("etape_selection_longs : mode catalogue_complet retiré pour ce corpus (budget %d < %d lignes du catalogue) ; utilisez MODE_SELECTION = \"quota_dp_fixe\" (k lignes par DP, variantes déduites).", budget, nrow(df_prep_scenarios_seuil)), call. = FALSE)
  FICHIER_SELECTION   <- file.path(DIR_SELECTION(), "selection_longs.parquet")
  meta_tirage <- list(MODE_SELECTION = mode, BUDGET_TOTAL_LONGS = as.integer(budget),
                      QUOTA_MIN_PAR_UNITE = as.integer(QUOTA_MIN_PAR_UNITE), NB_CHUNKS_MAX = as.integer(NB_CHUNKS_MAX),
                      CHUNK_SIZE_MIN = as.integer(CHUNK_SIZE_MIN), CHUNK_SIZE_FIXE = as.integer(CHUNK_SIZE_FIXE),
                      SEED = as.integer(SEED), nrow_catalogue = nrow(df_prep_scenarios_seuil))
  CLES_META_TIRAGE <- c("MODE_SELECTION", "BUDGET_TOTAL_LONGS", "QUOTA_MIN_PAR_UNITE", "NB_CHUNKS_MAX", "CHUNK_SIZE_MIN", "CHUNK_SIZE_FIXE", "SEED", "nrow_catalogue")
  meta_existant <- if(file.exists(FICHIER_META_TIRAGE)) yaml::read_yaml(FICHIER_META_TIRAGE) else NULL
  msg <- verifier_meta_tirage(meta_existant, meta_tirage, CLES_META_TIRAGE)
  if(!is.null(msg)) stop(msg)
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
    f_eff <- file.path(DIR_SELECTION(), "selection_longs_effectifs.csv")
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
  poser_tirage("selection", df_selection); poser_tirage("nb_tirage_longs", nb_tirage_longs); poser_tirage("meta_tirage", meta_tirage); poser_tirage("rapport", rapport); poser_tirage("mode_tirage", mode)
  banniere_fin("etape_selection_longs", t0, fichiers)
}

# Relecture de la sélection figée (reprise de session) : selection/_meta.yaml + selection_longs.parquet
# (quota_dp) ou catalogue + NB_VARIANTES (catalogue_complet).
relire_selection_longs <- function(etape){
  f_meta <- FICHIER_SELECTION_META()
  exiger_fichiers(f_meta, etape, "etape_selection_longs()")
  meta_tirage <- yaml::read_yaml(f_meta)
  poser_tirage("mode_tirage", meta_tirage$MODE_SELECTION); poser_tirage("meta_tirage", meta_tirage)
  if(identical(meta_tirage$MODE_SELECTION, "quota_dp_fixe")){
    for(pop in names(POPULATIONS)) exiger_fichiers(FICHIER_SELECTION_POP_META(pop), etape, "etape_selection_longs()")
    cat("Sélection quota_dp_fixe relue (", f_meta, ") : populations ", paste(names(POPULATIONS), collapse = ", "), "\n", sep = "")
    return(invisible(NULL))
  }
  if(identical(meta_tirage$MODE_SELECTION, "quota_dp")){
    f_sel <- file.path(DIR_SELECTION(), "selection_longs.parquet")
    exiger_fichiers(f_sel, etape, "etape_selection_longs()")
    df_selection <- arrow::read_parquet(f_sel)
    rapport <- etat_tirage("rapport", list()); rapport$selection <- effectifs_selection(df_selection); poser_tirage("rapport", rapport)
  } else {
    df_selection <- lire_catalogue(DIR_CATALOGUE(), MONO_CATALOGUE())
  }
  cat("Sélection relue depuis ", f_meta, " (", nrow(df_selection), " lignes, NB_VARIANTES = ", meta_tirage$NB_VARIANTES, ")\n", sep = "")
  poser_tirage("selection", df_selection); poser_tirage("nb_tirage_longs", as.integer(meta_tirage$NB_VARIANTES)); poser_tirage("meta_tirage", meta_tirage)
  invisible(NULL)
}

# Étape T3 — tirage des DAS des séjours longs par chunks (sauvegardés / repris), SANS habillage.
# ref_das_aigu est INDEXÉ une fois (indexer_ref_das : accès direct par strate, identité prouvée en
# test) ; débit imprimé par chunk. Mode quota_dp_fixe : par population (dossier de chunks et seed
# dérivés par population), chaque ligne de sélection emportant ses n_var variantes dans le MÊME
# chunk, dédoublonnage souple des variantes (aucun re-tirage) ; chunk_range = c(i, j) pour une
# session parallèle sur une plage ; rien n'est assemblé en RAM (relecture par lots en aval).
# Modes historiques : assemblé en mémoire de session.
etape_tirage_das_longs <- function(chunk_range = NULL, populations = names(POPULATIONS)){
  cat("\n==== CAMPAGNE = ", CAMPAGNE, " ; NB_CRH_CIBLE effectif = ", NB_CRH_CIBLE, " ; surcharge active : ", surcharge_active(), "\n", sep = "")
  t0 <- banniere_debut("etape_tirage_das_longs", "chunks = " %+% DIR_CHUNKS_LONGS() %+% " ; chunking dynamique (NB_CHUNKS_MAX = " %+% NB_CHUNKS_MAX %+% ", CHUNK_SIZE_MIN = " %+% CHUNK_SIZE_MIN %+% ", CHUNK_SIZE_FIXE = " %+% CHUNK_SIZE_FIXE %+% ")" %+% if(!is.null(chunk_range)) " ; plage " %+% paste(chunk_range, collapse = "..") else "")
  ctx <- charger_contexte_tirage(c("ref_das_aigu", "ref_comp_diabete"), "etape_tirage_das_longs")
  if(is.null(etat_tirage("mode_tirage"))) relire_selection_longs("etape_tirage_das_longs")
  mode <- etat_tirage("mode_tirage")
  t_idx <- Sys.time()
  index_das <- indexer_ref_das(arrow::read_parquet(file.path(ns(DIR_REFERENCES), nom_ref("ref_das_aigu"))))
  cat(sprintf("index ref_das_aigu : %d strates en %.1f s\n", length(index_das), as.numeric(difftime(Sys.time(), t_idx, units = "secs"))))
  fichiers <- character(0)
  if(identical(mode, "quota_dp_fixe")){
    for(pop in populations){
      sel <- lire_catalogue(DIR_SELECTION(pop))
      if(is.null(sel) || nrow(sel) == 0){ cat("population ", pop, " : sélection vide, sautée\n", sep = ""); next }
      cat(sprintf("== Séjours longs %s : %d lignes × variantes (%d attendues) ; seed_base = %d ==\n", pop, nrow(sel), sum(sel$n_var), seed_population(pop)))
      df_pmap <- sel |> dplyr::select(dplyr::all_of(c(PIVOTS_LONGS, "diagnostic_associes", "poids")), nb_tirage = n_var,
                                     dplyr::any_of(c("id_profil", "variante_debut", "hash_exclus")))
      pmap_chunks(df_pmap, sample_das_long, chunk_size = NULL, dossier = DIR_CHUNKS_POP(pop), prefixe = "longs",
                  seed_base = seed_population(pop), garder_chunks = TRUE, chunk_range = chunk_range, assembler = FALSE,
                  ref_das_aigu = index_das, refs = ctx$REFS, dedoublonner = TRUE)
      fichiers <- c(fichiers, list.files(DIR_CHUNKS_POP(pop), pattern = "^longs_chunk_.*\\.parquet$", full.names = TRUE))
      rm(sel, df_pmap); gc()
    }
    return(banniere_fin("etape_tirage_das_longs", t0, fichiers))
  }
  if(is.null(etat_tirage("selection"))) relire_selection_longs("etape_tirage_das_longs")
  df_selection <- etat_tirage("selection"); nb_tirage_longs <- etat_tirage("nb_tirage_longs")
  cat("== Séjours longs : tirage par chunks (", nrow(df_selection), " lignes × ", nb_tirage_longs, " variante(s)) ==\n", sep = "")
  df_tirage <- pmap_chunks(df_selection |> dplyr::select(dplyr::all_of(c(PIVOTS_LONGS, "diagnostic_associes", "poids"))),
                           sample_das_long, chunk_size = NULL, dossier = DIR_CHUNKS_LONGS(), prefixe = "longs",
                           seed_base = SEED + 1e5, garder_chunks = GARDER_CHUNKS, chunk_range = chunk_range,
                           ref_das_aigu = index_das, refs = ctx$REFS, nb_tirage = nb_tirage_longs)
  if(is.null(chunk_range)){
    rapport <- etat_tirage("rapport", list()); rapport$longs_tirage_n <- nrow(df_tirage); poser_tirage("rapport", rapport)
    poser_tirage("df_tirage_longs", df_tirage)
  }
  rm(index_das); gc()
  banniere_fin("etape_tirage_das_longs", t0, list.files(DIR_CHUNKS_LONGS(), pattern = "^longs_chunk_", full.names = TRUE))
}

# Étape T4 — habillage admin des séjours longs depuis v_admin_longs.parquet (relu, jamais
# prep_data en direct : frontière tirage / base). HABILLAGE ROBUSTE (défaut trouvé en revue clinique : NA durée + modes) :
# jointure sur les 6 clés CLES_ADMIN_LONGS (nbda retiré), repli hiérarchique helpers K2 (age -> cage -> mode_hospit × cage
# × racine), colonne repli_admin tracée, stop nominatif si aucun candidat — jamais de NA silencieux. Résultat en
# mémoire de session (modes historiques) ; relit les chunks si l'étape T3 n'a pas tourné dans cette session.
etape_habillage_longs <- function(populations = names(POPULATIONS)){
  t0 <- banniere_debut("etape_habillage_longs", "NB_VARIANTES_ADMIN_LONGS = " %+% NB_VARIANTES_ADMIN_LONGS %+% " tenue(s) admin par scénario (tirage pondéré par les effectifs n, sans remise ; NA = toutes, v7.2 ; N > 1 : id_scenario suffixé -aN) ; clés = " %+% paste(CLES_ADMIN_LONGS, collapse = ",") %+% " ; même nombre aux replis (cage, puis mode_hospit × cage × racine) ; durées du périmètre " %+% min(DUREE_LONGS) %+% "-" %+% max(DUREE_LONGS))
  charger_contexte_tirage("ref_v_admin_longs", "etape_habillage_longs")
  if(is.null(etat_tirage("mode_tirage")) && file.exists(FICHIER_SELECTION_META())) relire_selection_longs("etape_habillage_longs")
  if(identical(etat_tirage("mode_tirage"), "quota_dp_fixe")){
    # Flux par lots de chunks : jointure v_admin_longs.parquet relu, slice_sample par lot (seed dérivé),
    # typologie DPEC/TPEC recalculée (duree = 3 constante, comme le catalogue), écriture habille/<pop>/lot_%04d.parquet
    df_v_admin_longs <- arrow::read_parquet(file.path(ns(DIR_REFERENCES), nom_ref("ref_v_admin_longs"))); typo <- charger_typo()
    fichiers <- character(0)
    for(pop in populations){
      dir_ch <- DIR_CHUNKS_POP(pop); f_side <- file.path(dir_ch, "longs_chunks_meta.yaml")
      if(!file.exists(f_side)){ cat("population ", pop, " : aucun chunk, sautée\n", sep = ""); next }
      side <- yaml::read_yaml(f_side); presents <- list.files(dir_ch, pattern = "^longs_chunk_[0-9]{4}\\.parquet$")
      if(length(presents) < side$nb_chunks) stop(sprintf("etape_habillage_longs : population %s : %d / %d chunks présents. Lancez etape_tirage_das_longs() (plages manquantes) d'abord.", pop, length(presents), side$nb_chunks), call. = FALSE)
      dir_h <- DIR_HABILLE(pop); unlink(dir_h, recursive = TRUE); dir.create(dir_h, recursive = TRUE)
      ipop <- match(pop, names(POPULATIONS))
      lire_chunks_par_lots(dir_ch, "longs", LOT_CHUNKS_FINALISATION, function(d, i_lot){
        d <- typologie_sejour(d, typo, col_age = "age", col_duree = "duree", duree_defaut = 3); d$population <- pop
        set.seed(SEED + 4e6 + 1e4 * ipop + i_lot)
        h <- habiller_admin(d, df_v_admin_longs, NIVEAUX_REPLI_ADMIN, c(COLS_ADMIN, "duree"), NB_VARIANTES_ADMIN_LONGS, NULL, "longs " %+% pop, suffixer_id = TRUE)
        f <- file.path(dir_h, sprintf("lot_%04d.parquet", i_lot)); arrow::write_parquet(h, f)
        rp <- table(h$repli_admin)
        cat(sprintf("  %s lot %04d : %d scénarios -> %d lignes habillées (repli_admin : %s)\n", pop, i_lot, nrow(d), nrow(h), paste(names(rp), rp, sep = "=", collapse = ", ")))
      })
      fichiers <- c(fichiers, list.files(dir_h, full.names = TRUE))
    }
    rm(df_v_admin_longs); gc()
    return(banniere_fin("etape_habillage_longs", t0, fichiers))
  }
  if(is.null(etat_tirage("df_tirage_longs"))){ cat("df_tirage_longs absent de la session : relecture des chunks via etape_tirage_das_longs()\n"); etape_tirage_das_longs() }
  df_tirage <- etat_tirage("df_tirage_longs")
  df_v_admin_longs <- arrow::read_parquet(file.path(ns(DIR_REFERENCES), nom_ref("ref_v_admin_longs")))
  # Habillage admin (v7.2 l.555) — robuste : 6 clés, repli hiérarchique, jamais de NA silencieux
  set.seed(SEED + 4e6)
  df_scenarios <- habiller_admin(df_tirage, df_v_admin_longs, NIVEAUX_REPLI_ADMIN, c(COLS_ADMIN, "duree"), NB_VARIANTES_ADMIN_LONGS, NULL, "longs", suffixer_id = TRUE)
  rm(df_tirage, df_v_admin_longs)
  poser_tirage("df_scenarios_longs", df_scenarios)
  if(exists("df_tirage_longs", envir = ETAPES_ENV, inherits = FALSE)) rm("df_tirage_longs", envir = ETAPES_ENV)
  cat("- Séjours longs habillés : ", nrow(df_scenarios), " lignes (en mémoire de session)\n", sep = "")
  rm(df_scenarios); gc()
  banniere_fin("etape_habillage_longs", t0, character(0))
}

# Étape T5 — finalisation : UN SEUL LIVRABLE par campagne (<profil>/60_export_final/scenarios_<CAMPAGNE>.parquet +
# scenarios_<CAMPAGNE>_meta.yaml) : TOUS les scénarios de la campagne — longs (toutes populations, colonne population)
# ET courts DE LA CAMPAGNE (tirés par etape_tirage_courts sous 40_campagnes/<C>/, même statut que les longs) — en union
# de schémas (colonne branche = "long" / "court" en tête, NA typés, familles de colonnes au méta) ; méta : volumes par
# branche, ratio réalisé ; contrôles §8.2 + « zéro NA d'habillage » et distribution de repli_admin.
# Au-delà de SEUIL_MONOFICHIER lignes : parts scenarios_<CAMPAGNE>/part_XXXX.parquet (lire_corpus_final lit les deux
# formes). Contrôles §8.2, rapport_<C>.txt, echantillon_revue_<C>.csv (tiré du livrable unifié, PART_REVUE_COURTS),
# top30_das_par_cmd_<C>.csv, registre (quota_dp_fixe sous REGISTRE_ACTIF). Garde-fous : méta d'une autre campagne ->
# stop ; même campagne -> réécriture idempotente. Relancée : reconstruit depuis les chunks présents (aucun re-tirage).
# Courts DE LA CAMPAGNE : scénarios habillés (<C>/habille/courts/) ; absents mais chunks présents -> reconstruction via
# etape_tirage_courts() (chunks sautés, aucun re-tirage) ; rien -> message actionnable.
charger_courts_campagne <- function(etape){
  f <- FICHIER_COURTS_CAMPAGNE()
  if(!file.exists(f)){
    if(file.exists(file.path(DIR_CHUNKS_COURTS(), "courts_chunks_meta.yaml"))){ cat("scénarios courts habillés absents, chunks courts présents : reconstruction via etape_tirage_courts() (aucun re-tirage)\n"); etape_tirage_courts() }
    else stop(message_courts_absent(DIR_HABILLE_COURTS()), call. = FALSE)
  }
  list(df = tibble::as_tibble(arrow::read_parquet(f)), meta = lire_meta_si_present(FICHIER_COURTS_CAMPAGNE_META()))
}
# Écrit le livrable (monofichier ou parts) depuis les courts et la liste des lots longs ; schéma = union ordonnée par familles.
ecrire_livrable <- function(df_courts, lots, fusionner = NULL, campagne = CAMPAGNE){
  modele <- do.call(modele_schema, c(list(df_courts), lots))
  ordre <- unlist(familles_colonnes(names(modele)), use.names = FALSE); modele <- modele[, ordre, drop = FALSE]
  total <- nrow(df_courts) + sum(vapply(lots, nrow, integer(1)))
  if(is.null(fusionner)) fusionner <- total <= SEUIL_MONOFICHIER
  creer_dossiers(DIR_EXPORT_FINAL)
  unlink(FICHIER_LIVRABLE(campagne)); unlink(DIR_LIVRABLE_PARTS(campagne), recursive = TRUE)   # réécriture idempotente (même campagne, garde-fou amont)
  if(isTRUE(fusionner)){
    d <- dplyr::bind_rows(c(list(harmoniser(df_courts, modele)), lapply(lots, harmoniser, modele = modele)))
    arrow::write_parquet(d, FICHIER_LIVRABLE(campagne)); fichiers <- FICHIER_LIVRABLE(campagne); rm(d); gc()
  } else {
    dir.create(DIR_LIVRABLE_PARTS(campagne), recursive = TRUE); fichiers <- character(0); k <- 0L
    for(d in c(list(df_courts), lots)){ k <- k + 1L; f <- file.path(DIR_LIVRABLE_PARTS(campagne), sprintf("part_%04d.parquet", k)); arrow::write_parquet(harmoniser(d, modele), f); fichiers <- c(fichiers, f) }
  }
  list(fichiers = fichiers, total = total, n_court = nrow(df_courts), n_long = total - nrow(df_courts), forme = if(isTRUE(fusionner)) "monofichier" else "parts",
       colonnes = names(modele), types = vapply(modele, function(x) class(x)[1], character(1)), familles = familles_colonnes(names(modele)))
}
etape_finalisation <- function(fusionner = NULL, populations = names(POPULATIONS)){
  t0 <- banniere_debut("etape_finalisation", "campagne = " %+% CAMPAGNE %+% " ; livrable = " %+% FICHIER_LIVRABLE())
  ctx <- charger_contexte_tirage(character(0), "etape_finalisation")
  if(is.null(etat_tirage("mode_tirage")) && file.exists(FICHIER_SELECTION_META())) relire_selection_longs("etape_finalisation")
  mode <- etat_tirage("mode_tirage")
  if(is.null(mode)) stop("etape_finalisation : aucune sélection pour la campagne " %+% CAMPAGNE %+% " (" %+% FICHIER_SELECTION_META() %+% " absent). Lancez etape_selection_longs() d'abord.", call. = FALSE)
  meta_tirage <- etat_tirage("meta_tirage"); conv <- ctx$conversion_e669; fixe <- identical(mode, "quota_dp_fixe")
  # Garde-fou du livrable nommé par campagne : méta d'une AUTRE campagne -> stop ; même campagne -> réécriture idempotente
  v <- verifier_dossier_final(if(file.exists(FICHIER_LIVRABLE_META())) yaml::read_yaml(FICHIER_LIVRABLE_META()) else if(file.exists(FICHIER_LIVRABLE()) || dir.exists(DIR_LIVRABLE_PARTS())) list() else NULL, CAMPAGNE, FICHIER_LIVRABLE())
  if(v$action == "stop") stop(v$message, call. = FALSE)
  cat(v$message, "\n", sep = "")
  # Courts DE LA CAMPAGNE (tirés sous 40_campagnes/<C>/ : même statut que les longs)
  cs <- charger_courts_campagne("etape_finalisation"); df_c <- cs$df; df_c$branche <- "court"
  rapport <- etat_tirage("rapport", list())
  rapport$courts_tirage_n <- if(!is.null(cs$meta$scenarios_gardes)) cs$meta$scenarios_gardes else NA
  rapport$courts_meta <- cs$meta
  rapport$courts <- stats_branche(df_c, PIVOTS_COURTS, ctx$codes_imprecis, c("diag2", "diagnostic_associes"))
  rapport$courts$habillage <- controle_habillage(df_c, COLS_ADMIN, DUREE_COURTS); rapport$courts$scenarios <- dplyr::n_distinct(df_c$id_scenario)
  cat("- Séjours courts de la campagne ", CAMPAGNE, " relus depuis ", FICHIER_COURTS_CAMPAGNE(), " : ", nrow(df_c), " lignes, ", rapport$courts$scenarios, " scénarios\n", sep = "")
  # Longs : lots habillés par population (quota_dp_fixe) ou résultat de session (modes historiques)
  lots <- list(); resultats <- list()
  if(fixe){
    for(pop in populations){
      dir_h <- DIR_HABILLE(pop)
      if(!dir.exists(dir_h) || length(list.files(dir_h, pattern = "^lot_")) == 0){
        if(dir.exists(DIR_CHUNKS_POP(pop)) && length(list.files(DIR_CHUNKS_POP(pop), pattern = "^longs_chunk_")) > 0){
          cat("population ", pop, " : lots habillés absents, chunks présents : reconstruction via etape_habillage_longs() (aucun re-tirage)\n", sep = ""); etape_habillage_longs(pop)
        } else { cat("population ", pop, " : aucun chunk ni lot habillé, sautée (lancez etape_tirage_das_longs())\n", sep = ""); next }
      }
      acc <- acc_stats_init(); doublons <- NULL; n_lot <- 0L; hab_na <- 0L; hab_dur <- 0L; hab_repli <- NULL; n_scen <- 0L
      for(f in sort(list.files(dir_h, pattern = "^lot_[0-9]{4}\\.parquet$", full.names = TRUE))){
        n_lot <- n_lot + 1L
        d <- tibble::as_tibble(arrow::read_parquet(f))
        acc <- acc_stats_ajouter(acc, d, PIVOTS_LONGS, ctx$codes_imprecis, hta_autres, SEUIL_PIVOT, c("diag2", "graine", "diagnostic_associes"))
        ch <- controle_habillage(d, c(COLS_ADMIN, "duree"), DUREE_LONGS); hab_na <- hab_na + ch$na_habillage; hab_dur <- hab_dur + ch$duree_hors_perimetre; hab_repli <- dplyr::bind_rows(hab_repli, ch$repli); if("id_scenario" %in% names(d)) n_scen <- n_scen + dplyr::n_distinct(id_scenario_base(d$id_scenario))
        if("nb_variantes_demandees" %in% names(d)){
          dd <- d |> dplyr::distinct(dplyr::across(dplyr::all_of(c(PIVOTS_LONGS, "graine", "variante", "nb_variantes_demandees")))) |>
            dplyr::summarise(demandees = dplyr::first(nb_variantes_demandees), gardees = dplyr::n(), .by = dplyr::all_of(c(PIVOTS_LONGS, "graine"))) |>
            dplyr::summarise(demandees = sum(demandees), gardees = sum(gardees), .by = diag2)
          doublons <- dplyr::bind_rows(doublons, dd) |> dplyr::summarise(demandees = sum(demandees), gardees = sum(gardees), .by = diag2)
        }
        d$branche <- "long"; if(!"population" %in% names(d)) d$population <- pop; if(!"campagne" %in% names(d)) d$campagne <- CAMPAGNE
        lots[[length(lots) + 1L]] <- d
        cat(sprintf("  %s lot %04d : %d lignes\n", pop, n_lot, nrow(d))); rm(d)
      }
      st <- acc_stats_final(acc)
      scen_gardes <- if(is.null(doublons)) NA_integer_ else sum(doublons$gardees); scen_demandes <- if(is.null(doublons)) NA_integer_ else sum(doublons$demandees)
      resultats[[pop]] <- list(stats = st, doublons = doublons, scenarios_gardes = scen_gardes, scenarios_demandes = scen_demandes, lignes_finales = st$n, scenarios = n_scen,
                               habillage = list(na_habillage = hab_na, duree_hors_perimetre = hab_dur, repli = if(is.null(hab_repli)) data.frame(niveau = character(0), n = integer(0)) else hab_repli |> dplyr::summarise(n = sum(n), .by = niveau),
                                                N = NB_VARIANTES_ADMIN_LONGS, lignes_attendues = if(is.na(NB_VARIANTES_ADMIN_LONGS)) NA_integer_ else as.integer(n_scen * NB_VARIANTES_ADMIN_LONGS),
                                                lignes_hors_multiplication = if(is.na(NB_VARIANTES_ADMIN_LONGS)) 0L else as.integer(max(0L, st$n - n_scen * NB_VARIANTES_ADMIN_LONGS)),   # lignes EN TROP = multiplication non demandée (anomalie)
                                                lignes_manquantes = if(is.na(NB_VARIANTES_ADMIN_LONGS)) 0L else as.integer(max(0L, n_scen * NB_VARIANTES_ADMIN_LONGS - st$n))))              # strates à moins de N combinaisons (information)
      cat(sprintf("== %s : %s scénarios gardés / %s demandés (cible population %s) ; %d lignes finales (habillage)\n", pop, format(scen_gardes), format(scen_demandes),
                  format(meta_tirage$par_population[[pop]]$budget_population), st$n))
    }
    rapport$longs_fixe <- resultats; rapport$longs_tirage_n <- sum(vapply(resultats, function(r) r$lignes_finales, integer(1)))
  } else {
    if(is.null(etat_tirage("df_scenarios_longs"))){ cat("df_scenarios_longs absent de la session : reconstruction via etape_habillage_longs()\n"); etape_habillage_longs() }
    if(is.null(rapport$longs_tirage_n)) rapport$longs_tirage_n <- NA
    d <- tibble::as_tibble(etat_tirage("df_scenarios_longs")); d$branche <- "long"; if(!"campagne" %in% names(d)) d$campagne <- CAMPAGNE
    rapport$longs <- stats_branche(d, PIVOTS_LONGS, ctx$codes_imprecis, c("diag2", "graine", "diagnostic_associes"))
    rapport$longs$habillage <- controle_habillage(d, c(COLS_ADMIN, "duree"), DUREE_LONGS); rapport$longs$scenarios <- if("id_scenario" %in% names(d)) dplyr::n_distinct(id_scenario_base(d$id_scenario)) else NA_integer_
    rapport$longs$habillage$N <- NB_VARIANTES_ADMIN_LONGS; rapport$longs$habillage$lignes_attendues <- if(is.na(NB_VARIANTES_ADMIN_LONGS) || is.na(rapport$longs$scenarios)) NA_integer_ else as.integer(rapport$longs$scenarios * NB_VARIANTES_ADMIN_LONGS)
    rapport$longs$habillage$lignes_hors_multiplication <- if(is.na(rapport$longs$habillage$lignes_attendues)) 0L else as.integer(max(0L, nrow(d) - rapport$longs$habillage$lignes_attendues))
    rapport$longs$habillage$lignes_manquantes <- if(is.na(rapport$longs$habillage$lignes_attendues)) 0L else as.integer(max(0L, rapport$longs$habillage$lignes_attendues - nrow(d)))
    lots[[1L]] <- d; rm(d); rm("df_scenarios_longs", envir = ETAPES_ENV); gc()
  }
  ## Livrable unique
  liv <- ecrire_livrable(df_c, lots, fusionner); rm(df_c, lots); gc()
  cat("livrable ", liv$forme, " : ", liv$total, " lignes (longs = ", liv$n_long, ", courts = ", liv$n_court, ") -> ", if(liv$forme == "monofichier") FICHIER_LIVRABLE() else DIR_LIVRABLE_PARTS(), "\n", sep = "")
  poser_tirage("rapport", rapport)
  f_reg <- character(0)
  if(fixe && isTRUE(REGISTRE_ACTIF) && isTRUE(meta_tirage$REGISTRE_ACTIF)) f_reg <- etape_registre_campagne(meta_tirage$CAMPAGNE, populations)

  ## Revue (tirée du livrable unifié), top30, méta, rapport
  corpus <- lire_corpus_final(CAMPAGNE)
  ech <- echantillonner_livrable(corpus, NB_REVUE, PART_REVUE_COURTS, SEED + 2e6)
  utile <- function(x) x[, vapply(x, function(col) !all(is.na(col)), logical(1)), drop = FALSE]
  df_revue <- dplyr::bind_rows(formater_revue(utile(ech[ech$branche == "court", , drop = FALSE]), "court", ctx$lib_cim),
                               formater_revue(utile(ech[ech$branche == "long",  , drop = FALSE]), "long",  ctx$lib_cim))
  rm(corpus, ech); gc()
  f_revue <- FICHIER_REVUE(); readr::write_excel_csv2(df_revue, f_revue)
  f_top <- FICHIER_TOP30()
  utils::write.csv(dplyr::bind_rows(c(list(court = rapport$courts$top_das), if(fixe) lapply(resultats, function(r) r$stats$top_das) else list(long = rapport$longs$top_das)), .id = "branche"), f_top, row.names = FALSE)
  scen_long <- if(fixe) sum(vapply(resultats, function(r) r$scenarios, integer(1))) else rapport$longs$scenarios
  na_hab_longs <- if(fixe) sum(vapply(resultats, function(r) r$habillage$na_habillage, integer(1))) else rapport$longs$habillage$na_habillage
  meta_liv <- list(livrable = basename(if(liv$forme == "monofichier") FICHIER_LIVRABLE() else DIR_LIVRABLE_PARTS()), forme = liv$forme, campagne = CAMPAGNE, date = as.character(Sys.Date()), PROFIL = PROFIL,
                   MODE_SELECTION = mode, total = liv$total, n_long = liv$n_long, n_court = liv$n_court,
                   volumes = list(long = list(lignes = liv$n_long, scenarios = scen_long), court = list(lignes = liv$n_court, scenarios = rapport$courts$scenarios)),
                   ratio_courts_realise = if(!is.na(scen_long) && scen_long > 0) round(rapport$courts$scenarios / scen_long, 4) else NA, ratio_courts_cible = if(!is.null(cs$meta$RATIO_COURTS)) cs$meta$RATIO_COURTS else RATIO_COURTS,
                   habillage_longs = list(NB_VARIANTES_ADMIN_LONGS = NB_VARIANTES_ADMIN_LONGS, lignes_attendues = if(fixe) sum(vapply(resultats, function(r) r$habillage$lignes_attendues, integer(1))) else rapport$longs$habillage$lignes_attendues,
                                          na_habillage = na_hab_longs, repli_admin = if(fixe) lapply(resultats, function(r) stats::setNames(as.list(r$habillage$repli$n), r$habillage$repli$niveau)) else stats::setNames(as.list(rapport$longs$habillage$repli$n), rapport$longs$habillage$repli$niveau)),
                   notes_familles = list(audit = NOTE_POIDS, tracabilite = "id_scenario = id_profil-variante (jeu de DAS, inscrit au registre) ; suffixe -aN = tenue admin N >= 2 quand NB_VARIANTES_ADMIN_LONGS > 1 (raffinement sous le registre)"),
                   populations = if(fixe) names(resultats) else NA, lignes_finales_longs = if(fixe) lapply(resultats, function(r) r$lignes_finales) else list(long = liv$n_long),
                   REGISTRE_ACTIF = isTRUE(REGISTRE_ACTIF) && fixe, version_recette_id = RECETTE_ID, version_recette_id_courts = RECETTE_ID_COURTS,
                   courts = list(source = FICHIER_COURTS_CAMPAGNE(), n = liv$n_court, meta = cs$meta),
                   selection = list(NB_CRH_CIBLE = meta_tirage$NB_CRH_CIBLE, BUDGET_TOTAL_LONGS = meta_tirage$BUDGET_TOTAL_LONGS, volume_attendu = meta_tirage$volume_attendu, date_selection = meta_tirage$date),
                   colonnes = as.list(liv$colonnes), types = as.list(liv$types), familles_colonnes = liv$familles,
                   note = "branche = long / court en tête ; NA typés où une colonne ne s'applique pas ; types unifiés par colonne (integer/numeric -> numeric, sinon texte : `age` est un entier chez les courts et une classe lt_18/ge_18 chez les longs) ; courts DE LA campagne (même statut que les longs) ; repli_admin = niveau de repli de l'habillage (0 = strate fine)")
  yaml::write_yaml(meta_liv, FICHIER_LIVRABLE_META())
  lignes <- rapport_finalisation(fixe, rapport, resultats, meta_tirage, ctx, liv, df_revue, f_reg, conv)
  f_rapport <- FICHIER_RAPPORT()
  writeLines(lignes, f_rapport); cat(lignes, sep = "\n")
  cat("Tirage terminé. Rapport : ", f_rapport, "\n", sep = "")
  banniere_fin("etape_finalisation", t0, c(liv$fichiers, FICHIER_LIVRABLE_META(), f_revue, f_top, f_rapport, f_reg))
}
# Rapport de contrôle (les deux modes) : livrable, volumétrie, distributions, top 30, imprécis, §8.2, campagne.
rapport_finalisation <- function(fixe, rapport, resultats, meta_tirage, ctx, liv, df_revue, f_reg, conv){
  cm <- rapport$courts_meta
  scen_long <- if(fixe) sum(vapply(resultats, function(r) r$scenarios, integer(1))) else rapport$longs$scenarios
  lignes <- c("RAPPORT DE CONTROLE — etape_finalisation — campagne " %+% CAMPAGNE %+% " — " %+% as.character(Sys.Date()) %+% " — PROFIL = " %+% PROFIL,
              "", "== L. Livrable unique (" %+% liv$forme %+% ") ==",
              sprintf("fichier : %s ; total = %d lignes ; branche long = %d ; branche court = %d (courts DE LA campagne, relus depuis %s)", basename(if(liv$forme == "monofichier") FICHIER_LIVRABLE() else DIR_LIVRABLE_PARTS()), liv$total, liv$n_long, liv$n_court, FICHIER_COURTS_CAMPAGNE()),
              sprintf("scénarios distincts : long = %s ; court = %s ; ratio courts réalisé = %s (cible RATIO_COURTS = %s, provisoire)", format(scen_long), format(rapport$courts$scenarios), if(!is.na(scen_long) && scen_long > 0) format(round(rapport$courts$scenarios / scen_long, 4)) else "NA", format(if(!is.null(cm$RATIO_COURTS)) cm$RATIO_COURTS else RATIO_COURTS)),
              "", "== C. Séjours courts de la campagne (étape de campagne, registre commun) ==",
              if(is.null(cm)) "   (méta du tirage courts absent)" else c(sprintf("budget = %s (source : %s — %s) ; tirable %s pivots (ANS_COURTS = %s), %s tirés (vierges %s, recyclés %s)", format(cm$budget), cm$source_budget, cm$detail_budget, format(cm$n_pivots_tirable), paste(unlist(cm$ANS_COURTS), collapse = ","), format(cm$n_pivots_tires), format(cm$n_pivots_vierges), format(cm$n_pivots_recycles)),
                                                                     sprintf("scénarios gardés = %s / demandés = %s (éliminés = %s : doublons souples, hash du registre, strates vides) ; lignes habillées = %s", format(cm$scenarios_gardes), format(cm$scenarios_demandes), format(cm$scenarios_elimines), format(cm$n_lignes))),
              "familles de colonnes : " %+% paste(vapply(names(liv$familles), function(f) f %+% " (" %+% paste(liv$familles[[f]], collapse = ", ") %+% ")", character(1)), collapse = " ; "),
              "", "== 0. Meta du catalogue ==", "   " %+% strsplit(yaml::as.yaml(ctx$meta_catalogue), "\n")[[1]],
              "", "== 0b. Meta de la sélection (selection/_meta.yaml) ==", "   " %+% strsplit(yaml::as.yaml(meta_tirage), "\n")[[1]], "")
  anomalies <- 0
  if(fixe){
    lignes <- c(lignes, "== 1. Volumétrie : réalisé vs cible (NB_CRH_CIBLE = " %+% meta_tirage$NB_CRH_CIBLE %+% " ; k = " %+% meta_tirage$NB_LIGNES_PAR_DP %+% ") ==",
                sprintf("sejours_courts : final = %d ; pivots distincts = %d", rapport$courts$n, rapport$courts$pivots))
    for(pop in names(resultats)){
      r <- resultats[[pop]]; mp <- meta_tirage$par_population[[pop]]
      lignes <- c(lignes, sprintf("%-10s cible = %s ; attendu (sélection) = %s ; scénarios gardés = %s ; doublons éliminés = %s ; lignes finales (habillage) = %d ; pivots distincts = %d",
                                  pop, format(mp$budget_population), format(mp$volume_attendu), format(r$scenarios_gardes), format(r$scenarios_demandes - r$scenarios_gardes), r$lignes_finales, r$stats$pivots),
                  sprintf("           nb_dp = %s ; X = %s ; plafonds appliqués = %s ; manque à gagner (sélection) = %s ; planchers d'unités désactivés (k < nb types) = %s",
                          format(mp$nb_dp), format(mp$X), format(mp$plafonds_appliques), format(mp$manque_a_gagner), format(rapport$selection_fixe$metas[[pop]]$planchers_unites_desactives)))
    }
    sf <- rapport$selection_fixe
    if(!is.null(sf) && !is.null(sf$stats)){
      lignes <- c(lignes, "", "== 1b. Lignes plafonnées par DPEC (variantes) ==", fmt_df(sf$stats |> dplyr::filter(plafonne) |> dplyr::summarise(variantes = sum(variantes), dp = dplyr::n(), .by = c(population, groupe))),
                  "== 1c. Les 30 DP au plus fort manque à gagner (X_dp - lignes disponibles) ==", fmt_df(sf$stats |> dplyr::arrange(dplyr::desc(manque_a_gagner)) |> utils::head(30) |> dplyr::select(population, dp, groupe, lignes_disponibles, X_dp, manque_a_gagner)))
    }
    if(!is.null(sf) && !is.null(sf$metas)){
      lignes <- c(lignes, "", "== 1e. Classes DPEC plafonnées (plafond = total de classe, 1 représentant par DP prime) ==")
      for(pop in names(sf$metas)) for(cl in sf$metas[[pop]]$classes_plafonnees)
        lignes <- c(lignes, sprintf("   %-10s %-30s nb_dp = %s ; plafond = %s ; total retenu = %s ; dépassement = %s", pop, cl$classe, cl$nb_dp, cl$plafond, cl$total_retenu, cl$depassement))
      lignes <- c(lignes, "", "== 1f. Campagne " %+% meta_tirage$CAMPAGNE %+% " (registre " %+% (if(isTRUE(meta_tirage$REGISTRE_ACTIF)) "actif" else "inactif") %+% ") ==")
      for(pop in names(sf$metas)){ m <- sf$metas[[pop]]
        lignes <- c(lignes, sprintf("   %-10s DP vierges = %s ; partiellement consommés = %s ; recyclés (variantes nouvelles) = %s", pop, m$dp_vierges, m$dp_partiellement_consommes, m$dp_recycles)) }
      if(isTRUE(meta_tirage$REGISTRE_ACTIF) && length(f_reg)){
        reg <- lire_registre(DIR_REGISTRE()); side_c <- yaml::read_yaml(FICHIER_CATALOGUE_META())
        conso <- reg$par_dpec |> dplyr::mutate(lignes_catalogue = unname(unlist(side_c$effectifs_dpec)[DPEC]), taux = round(nb_scenarios / lignes_catalogue, 4)) |> dplyr::arrange(DPEC)
        lignes <- c(lignes, "-- consommation cumulée du catalogue par DPEC (scénarios tirés toutes campagnes / lignes disponibles) :", fmt_df(conso),
                    "-- 30 DP les plus proches de l'épuisement total (lignes vierges restantes / lignes disponibles) :",
                    if(is.null(sf$stats)) "   (stats de sélection non disponibles dans cette session : sélection relue, non recalculée ; voir selection/<population>/selection_longs_stats_dp.csv)" else
                    fmt_df(sf$stats |> dplyr::filter(groupe == ".reste") |> dplyr::mutate(part_vierge = round(vierges_restantes / lignes_disponibles, 3)) |> dplyr::arrange(part_vierge, dplyr::desc(lignes_disponibles)) |> utils::head(30) |> dplyr::select(population, dp, lignes_disponibles, vierges_restantes, part_vierge, nb_recycles)))
      }
    }
    lignes <- c(lignes, "", "== 1d. Doublons éliminés par DP (30 premiers) ==")
    for(pop in names(resultats)) if(!is.null(resultats[[pop]]$doublons)) lignes <- c(lignes, "-- " %+% pop, fmt_df(resultats[[pop]]$doublons |> dplyr::mutate(elimines = demandees - gardees) |> dplyr::arrange(dplyr::desc(elimines)) |> utils::head(30)))
    lignes <- c(lignes, "", "== 2. Distribution du nombre de DAS par classe d'âge ==", "-- sejours_courts", fmt_df(rapport$courts$distribution))
    for(pop in names(resultats)) lignes <- c(lignes, "-- longs " %+% pop, fmt_df(resultats[[pop]]$stats$distribution))
    lignes <- c(lignes, "", "== 3. Top 30 DAS par CMD (" %+% basename(FICHIER_TOP30()) %+% ") ==", "-- sejours_courts", fmt_df(rapport$courts$top_das))
    for(pop in names(resultats)) lignes <- c(lignes, "-- longs " %+% pop, fmt_df(resultats[[pop]]$stats$top_das))
    lignes <- c(lignes, "", "== 4. Taux de codes « sans précision » ==", sprintf("sejours_courts : %s", format(rapport$courts$taux_imprecis)))
    for(pop in names(resultats)) lignes <- c(lignes, sprintf("longs %-8s : %s", pop, format(resultats[[pop]]$stats$taux_imprecis)))
    lignes <- c(lignes, "", "== 5. Vérifications programmatiques §8.2 (0 attendu) ==")
    fmt_cc <- function(nom, cc, e669){ c(sprintf("%-16s doublons_categorie = %s ; diabete_hors_flag = %s ; i10_avec_hta_autres = %s ; poids_sous_seuil = %s ; e669_residuels = %s%s",
                                                  nom, format(cc$doublons_categorie), format(cc$diabete_hors_flag), format(cc$i10_avec_hta_autres), format(cc$poids_sous_seuil), format(e669), if(conv && e669 > 0) "  <- ANOMALIE" else "")) }
    lignes <- c(lignes, fmt_cc("sejours_courts", rapport$courts$controles, rapport$courts$e669_residuels))
    anomalies <- anomalies + sum(unlist(rapport$courts$controles[c("doublons_categorie", "diabete_hors_flag", "i10_avec_hta_autres", "poids_sous_seuil")]), na.rm = TRUE) + if(conv) rapport$courts$e669_residuels else 0
    for(pop in names(resultats)){ st <- resultats[[pop]]$stats; lignes <- c(lignes, fmt_cc("longs " %+% pop, st$controles, st$e669_residuels))
      anomalies <- anomalies + sum(unlist(st$controles), na.rm = TRUE) + if(conv) st$e669_residuels else 0 }
    lignes <- c(lignes, "-- effectifs E660x par classe :", "   courts :", fmt_df(rapport$courts$e660))
    for(pop in names(resultats)) lignes <- c(lignes, "   longs " %+% pop %+% " :", fmt_df(resultats[[pop]]$stats$e660))
    lignes <- c(lignes, "", "== 5b. Habillage admin : zéro NA attendu (durée, modes) ; distribution du repli (0 = strate fine 6 clés, 1 = cage, 2 = mode_hospit × cage × racine) ==")
    for(pop in names(resultats)){ hb <- resultats[[pop]]$habillage
      lignes <- c(lignes, sprintf("longs %-8s na_habillage = %d%s ; duree_hors_perimetre (hors %d-%d) = %d%s ; repli_admin : %s", pop, hb$na_habillage, if(hb$na_habillage > 0) "  <- ANOMALIE" else "", min(DUREE_LONGS), max(DUREE_LONGS), hb$duree_hors_perimetre, if(hb$duree_hors_perimetre > 0) "  <- ANOMALIE" else "", if(nrow(hb$repli)) paste(hb$repli$niveau, hb$repli$n, sep = " = ", collapse = " ; ") else "(aucune ligne)"),
                  sprintf("           tenues admin N = %s : lignes = %d, scénarios = %d, attendu = scénarios × N = %s ; strates à moins de N combinaisons (lignes manquantes) = %d ; lignes_hors_multiplication = %d%s", format(hb$N), resultats[[pop]]$lignes_finales, resultats[[pop]]$scenarios, format(hb$lignes_attendues), hb$lignes_manquantes, hb$lignes_hors_multiplication, if(hb$lignes_hors_multiplication > 0) "  <- ANOMALIE" else ""))
      anomalies <- anomalies + hb$na_habillage + hb$duree_hors_perimetre + hb$lignes_hors_multiplication }
    lignes <- c(lignes, sprintf("sejours_courts   na_habillage = %d%s ; duree_hors_perimetre (hors %d-%d) = %d%s", rapport$courts$habillage$na_habillage, if(rapport$courts$habillage$na_habillage > 0) "  <- ANOMALIE" else "", min(DUREE_COURTS), max(DUREE_COURTS), rapport$courts$habillage$duree_hors_perimetre, if(rapport$courts$habillage$duree_hors_perimetre > 0) "  <- ANOMALIE" else ""))
    anomalies <- anomalies + rapport$courts$habillage$na_habillage + rapport$courts$habillage$duree_hors_perimetre
  } else {
    lignes <- c(lignes, "== 1. Volumétrie ==",
                sprintf("sejours_courts : tirage = %s ; final = %d ; pivots distincts = %d", format(rapport$courts_tirage_n), rapport$courts$n, rapport$courts$pivots),
                sprintf("sejours_longs  : tirage = %s ; final = %d ; pivots distincts = %d", format(rapport$longs_tirage_n), rapport$longs$n, rapport$longs$pivots))
    if(meta_tirage$MODE_SELECTION == "catalogue_complet"){
      lignes <- c(lignes, sprintf("mode catalogue_complet : nrow catalogue = %d ; NB_VARIANTES = %d ; volume attendu = %d ; volume tiré = %s",
                                  meta_tirage$nrow_catalogue, meta_tirage$NB_VARIANTES, meta_tirage$volume_attendu, format(rapport$longs_tirage_n)))
    } else {
      lignes <- c(lignes, sprintf("mode quota_dp : %d diag2 ; quota par DP = %d ; lignes sélectionnées = %d", meta_tirage$nb_dp, meta_tirage$quota_par_dp, meta_tirage$volume_attendu),
                  "-- effectifs sélectionnés diag2 × type_unite (50 premières lignes ; table complète : selection/selection_longs_effectifs.csv)",
                  fmt_df(utils::head(rapport$selection, 50)),
                  "-- totaux par type_unite :",
                  fmt_df(rapport$selection |> dplyr::select(-diag2, -total) |> dplyr::summarise(dplyr::across(dplyr::everything(), sum))))
    }
    lignes <- c(lignes, "", "== 2. Distribution du nombre de DAS par classe d'âge (à comparer aux cibles de saturation) ==",
                "-- sejours_courts", fmt_df(rapport$courts$distribution), "-- sejours_longs", fmt_df(rapport$longs$distribution),
                "-- cibles dégradées CIBLES_NB_CHRONIQUES :",
                "   " %+% names(CIBLES_NB_CHRONIQUES) %+% " : " %+% vapply(CIBLES_NB_CHRONIQUES, function(x) paste(x, collapse = "-"), character(1)), "")
    lignes <- c(lignes, "== 3. Top 30 DAS par CMD (fichier complet : " %+% basename(FICHIER_TOP30()) %+% ") ==",
                "-- sejours_courts", fmt_df(rapport$courts$top_das), "-- sejours_longs", fmt_df(rapport$longs$top_das), "")
    lignes <- c(lignes, "== 4. Taux de codes « sans précision » parmi les DAS de sortie (mesuré, non corrigé) ==",
                sprintf("sejours_courts : %s", format(rapport$courts$taux_imprecis)),
                sprintf("sejours_longs  : %s", format(rapport$longs$taux_imprecis)), "")
    lignes <- c(lignes, "== 5. Vérifications programmatiques §8.2 (0 attendu ; NA = non applicable) ==")
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
    hb <- rapport$longs$habillage
    lignes <- c(lignes, "== 5b. Habillage admin : zéro NA attendu (durée, modes) ; distribution du repli (0 = strate fine 6 clés, 1 = cage, 2 = mode_hospit × cage × racine) ==",
                sprintf("sejours_longs   na_habillage = %d%s ; duree_hors_perimetre (hors %d-%d) = %d%s ; repli_admin : %s", hb$na_habillage, if(hb$na_habillage > 0) "  <- ANOMALIE" else "", min(DUREE_LONGS), max(DUREE_LONGS), hb$duree_hors_perimetre, if(hb$duree_hors_perimetre > 0) "  <- ANOMALIE" else "", if(nrow(hb$repli)) paste(hb$repli$niveau, hb$repli$n, sep = " = ", collapse = " ; ") else "(aucune ligne)"),
                sprintf("sejours_longs   tenues admin N = %s : lignes = %d, scénarios = %s, attendu = %s ; strates à moins de N combinaisons (lignes manquantes) = %d ; lignes_hors_multiplication = %d%s", format(hb$N), rapport$longs$n, format(rapport$longs$scenarios), format(hb$lignes_attendues), hb$lignes_manquantes, hb$lignes_hors_multiplication, if(hb$lignes_hors_multiplication > 0) "  <- ANOMALIE" else ""),
                sprintf("sejours_courts  na_habillage = %d%s ; duree_hors_perimetre (hors %d-%d) = %d%s", rapport$courts$habillage$na_habillage, if(rapport$courts$habillage$na_habillage > 0) "  <- ANOMALIE" else "", min(DUREE_COURTS), max(DUREE_COURTS), rapport$courts$habillage$duree_hors_perimetre, if(rapport$courts$habillage$duree_hors_perimetre > 0) "  <- ANOMALIE" else ""))
    anomalies <- anomalies + hb$na_habillage + hb$duree_hors_perimetre + hb$lignes_hors_multiplication + rapport$courts$habillage$na_habillage + rapport$courts$habillage$duree_hors_perimetre
  }
  c(lignes, "TOTAL anomalies = " %+% anomalies, "",
    "Livrables : " %+% basename(if(liv$forme == "monofichier") FICHIER_LIVRABLE() else DIR_LIVRABLE_PARTS()) %+% " (+ " %+% basename(FICHIER_LIVRABLE_META()) %+% ") ; " %+% basename(FICHIER_REVUE()) %+% " (" %+% nrow(df_revue) %+% " scénarios) ; " %+%
      basename(FICHIER_TOP30()) %+% (if(length(f_reg)) " ; " %+% basename(f_reg) else "") %+% " — dans " %+% DIR_EXPORT_FINAL)
}

# Étape R — registre de campagne : écrit registre_<campagne>.parquet (append-only) depuis les chunks longs
# tirés (id_scenario réellement PRODUITS, après dédoublonnage) et la sélection (population, DPEC), ET depuis les
# scénarios courts de la campagne (branche = "court", même fichier). Une branche absente est signalée, pas bloquante.
etape_registre_campagne <- function(campagne = CAMPAGNE, populations = names(POPULATIONS)){
  # Q53 ACTÉE : une mesure de palier n'écrit JAMAIS dans la mémoire permanente (défense en profondeur ; palier.R impose
  # aussi REGISTRE_ACTIF <- FALSE). La protection vit dans le mécanisme, pas dans la discipline.
  if(palier_actif()) stop("etape_registre_campagne : surcharge de PALIER active (" %+% surcharge_active() %+% ") : une mesure n'écrit jamais au registre. Videz le palier (RUN_aval.Rmd §5), Restart R, puis relancez la finalisation de la campagne réelle.", call. = FALSE)
  t0 <- banniere_debut("etape_registre_campagne", "campagne = " %+% campagne %+% " ; " %+% DIR_REGISTRE())
  lignes <- NULL
  for(pop in populations){
    dir_ch <- DIR_CHUNKS_POP(pop); sel <- lire_catalogue(DIR_SELECTION(pop))
    if(is.null(sel) || !dir.exists(dir_ch)) next
    dpec_par_profil <- stats::setNames(sel$DPEC, sel$id_profil)
    lire_chunks_par_lots(dir_ch, "longs", LOT_CHUNKS_FINALISATION, function(d, i){
      if(!"id_profil" %in% names(d)) stop("etape_registre_campagne : les chunks de " %+% pop %+% " ne portent pas id_profil (tirés sans identifiants) : utilisez etape_retro_inscrire().", call. = FALSE)
      d$campagne <- campagne; d$population <- pop; d$DPEC <- unname(dpec_par_profil[d$id_profil]); d$date <- as.character(Sys.Date()); d$branche <- "long"
      lignes <<- dplyr::bind_rows(lignes, tibble::as_tibble(d[, COLONNES_REGISTRE]))
    })
  }
  n_longs <- if(is.null(lignes)) 0L else nrow(lignes)
  f_c <- FICHIER_COURTS_CAMPAGNE(campagne)
  if(file.exists(f_c)){
    lc <- registre_depuis_courts(arrow::read_parquet(f_c), campagne, charger_typo())
    lignes <- dplyr::bind_rows(lignes, lc)
  } else cat("courts de la campagne absents (", f_c, ") : branche court non inscrite (etape_tirage_courts() puis relancer)\n", sep = "")
  if(is.null(lignes) || nrow(lignes) == 0) stop("etape_registre_campagne : aucun scénario tiré à inscrire (lancez etape_tirage_das_longs() et etape_tirage_courts()).", call. = FALSE)
  if(n_longs == 0) cat("chunks longs absents : branche long non inscrite (etape_tirage_das_longs() puis relancer)\n")
  if(anyDuplicated(lignes$id_scenario)) stop("etape_registre_campagne : id_scenario dupliqué dans la campagne.", call. = FALSE)
  f <- ecrire_registre_campagne(lignes, campagne, DIR_REGISTRE())
  cat(sprintf("registre %s : %d scénarios (%d longs, %d courts), %d profils, %d DP -> %s\n", campagne, nrow(lignes), sum(lignes$branche == "long"), sum(lignes$branche == "court"), dplyr::n_distinct(lignes$id_profil), dplyr::n_distinct(lignes$diag2), f))
  banniere_fin("etape_registre_campagne", t0, f)
}

# Rétro-inscription d'une campagne tirée AVANT ce chantier (chunks sans id_profil) : relit la
# sélection (population, DPEC) et les chunks, recalcule id_profil (recette RECETTE_ID sur pivots +
# graine) et hash_das (jeux réellement tirés), garde la numérotation des variantes telle que tirée,
# écrit registre_<campagne>.parquet (idempotente : mêmes entrées -> même fichier).
etape_retro_inscrire <- function(dossier_selection, dossier_chunks, campagne, populations = names(POPULATIONS)){
  t0 <- banniere_debut("etape_retro_inscrire", "campagne = " %+% campagne %+% " ; sélection = " %+% dossier_selection %+% " ; chunks = " %+% dossier_chunks)
  typo <- charger_typo(); lignes <- NULL; nb_chunks_lignes <- 0L
  for(pop in populations){
    dir_ch <- file.path(dossier_chunks, pop); dir_sel <- file.path(dossier_selection, pop)
    if(!dir.exists(dir_ch)) next
    sel <- lire_catalogue(dir_sel)
    dpec_par_profil <- if(!is.null(sel) && all(c("DPEC", "diagnostic_associes") %in% names(sel))){ sel$id_profil <- id_profil_de(sel); stats::setNames(sel$DPEC, sel$id_profil) } else NULL
    lire_chunks_par_lots(dir_ch, "longs", LOT_CHUNKS_FINALISATION, function(d, i){
      nb_chunks_lignes <<- nb_chunks_lignes + nrow(d)
      lignes <<- dplyr::bind_rows(lignes, registre_depuis_chunks(d, campagne, pop, dpec_par_profil, typo))
    })
  }
  if(is.null(lignes) || nrow(lignes) == 0) stop("etape_retro_inscrire : aucun chunk trouvé sous " %+% dossier_chunks %+% "/<population>/.", call. = FALSE)
  if(anyDuplicated(lignes$id_scenario)) stop("etape_retro_inscrire : id_scenario dupliqué (mêmes profil et variante tirés deux fois ?).", call. = FALSE)
  f <- ecrire_registre_campagne(lignes, campagne, DIR_REGISTRE())
  cat(sprintf("vérifications : scénarios inscrits = %d ; lignes des chunks = %d (%s) ; DP couverts = %d ; profils = %d ; DPEC renseignés = %d / %d\n",
              nrow(lignes), nb_chunks_lignes, if(nrow(lignes) == nb_chunks_lignes) "égaux" else "DIFFÉRENTS", dplyr::n_distinct(lignes$diag2), dplyr::n_distinct(lignes$id_profil), sum(!is.na(lignes$DPEC)), nrow(lignes)))
  banniere_fin("etape_retro_inscrire", t0, f)
}
# Rétro-inscription des SÉJOURS COURTS historiques (corpus courts FIXE, post-réorganisation 30_courts/scenarios_courts.parquet ;
# argument de secours sinon) : id_profil (k + 15 hex sur les pivots) et hash_das recalculés, variantes telles que tirées,
# inscrits sous `campagne`, branche "court". AUCUN re-tirage. Idempotente (registre append-only, extension par branche).
# Vérification : scénarios inscrits == scénarios distincts du corpus (une ligne du corpus par variante d'habillage admin).
etape_retro_inscrire_courts <- function(campagne = "C1", fichier_courts = FICHIER_COURTS_HISTORIQUE()){
  t0 <- banniere_debut("etape_retro_inscrire_courts", "campagne = " %+% campagne %+% " ; corpus courts = " %+% fichier_courts)
  if(!file.exists(fichier_courts)) stop("etape_retro_inscrire_courts : corpus courts introuvable : " %+% fichier_courts %+% " (réorganisation : scenarios_courts_v8_<date>.parquet -> 30_courts/scenarios_courts.parquet ; sinon passez fichier_courts).", call. = FALSE)
  df <- tibble::as_tibble(arrow::read_parquet(fichier_courts))
  lignes <- registre_depuis_courts(df, campagne, charger_typo())
  n_scen <- dplyr::n_distinct(id_scenario_de(id_profil_courts_de(df), df$variante))
  f <- ecrire_registre_campagne(lignes, campagne, DIR_REGISTRE())
  cat(sprintf("vérifications : scénarios courts inscrits = %d ; scénarios distincts du corpus = %d (%s) ; lignes du corpus = %d (variantes d'habillage admin) ; pivots = %d ; DPEC renseignés = %d / %d\n",
              nrow(lignes), n_scen, if(nrow(lignes) == n_scen) "égaux" else "DIFFÉRENTS", nrow(df), dplyr::n_distinct(lignes$id_profil), sum(!is.na(lignes$DPEC)), nrow(lignes)))
  banniere_fin("etape_retro_inscrire_courts", t0, f)
}

# Lecture d'un corpus longs historique : dossier scenarios_longs_tirage_v8_<AAAAMMJJ>/ (sous-dossiers <population>/ lus
# récursivement, population posée ; sinon tous les parquets, population reconstituée par cage) ou fichier .parquet.
lire_corpus_longs_historique <- function(source, populations = names(POPULATIONS)){
  lire_tous <- function(fs){ d <- purrr::map(fs, function(f) tibble::as_tibble(arrow::read_parquet(f))) |> purrr::list_rbind(); if(".chunk_vide" %in% names(d)) d$.chunk_vide <- NULL; d }
  if(dir.exists(source)){
    sous <- populations[dir.exists(file.path(source, populations))]
    if(length(sous)) return(purrr::map(sous, function(pp){ fs <- list.files(file.path(source, pp), pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE); d <- lire_tous(fs); if(nrow(d)) d$population <- pp; d }) |> purrr::list_rbind())
    return(lire_tous(list.files(source, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)))
  }
  tibble::as_tibble(arrow::read_parquet(source))
}
# ADOPTION DE C1 (chantier « courts en campagnes » §4) : campagne = longs + courts historiques, SANS re-tirage.
# Reconstruit le livrable unifié depuis l'ancien corpus longs (scenarios_longs_tirage_v8_<AAAAMMJJ>/, sous-dossiers
# adulte/ et pediatrie/, population reconstituée si absente) et le corpus courts historique (30_courts/scenarios_courts.parquet),
# recalcule les identifiants (id_v1 / id_courts_v1) et hash_das, inscrit les deux branches au registre (append-only ;
# extension acceptée si les longs y sont déjà par rétro-inscription), écrit scenarios_<C>.parquet + méta (origine =
# "adoption", chemins sources, dates) dans <profil>/60_export_final/ ; annexes datées copiées et renommées par campagne
# si présentes. Garde-fous : livrable de la même campagne déjà présent et différent -> stop ; plusieurs corpus longs
# datés -> source_longs explicite obligatoire ; cohérence livrable / registre vérifiée (écart signalé). Idempotente.
etape_adopter_campagne <- function(campagne = "C1", source_longs = NULL, fichier_courts = FICHIER_COURTS_HISTORIQUE(),
                                   dossier_recherche = file.path(PATH_RESULTS, "_a_reorganiser", "exports"), annexes = TRUE, populations = names(POPULATIONS)){
  if(palier_actif()) stop("etape_adopter_campagne : surcharge de PALIER active : une mesure n'écrit jamais au registre.", call. = FALSE)
  t0 <- banniere_debut("etape_adopter_campagne", "campagne = " %+% campagne %+% " ; courts = " %+% fichier_courts)
  ch <- choisir_source_longs(source_longs, candidats_corpus_longs(dossier_recherche)); cat(ch$message, "\n", sep = "")
  if(!file.exists(fichier_courts)) stop("etape_adopter_campagne : corpus courts historique introuvable : " %+% fichier_courts %+% " (réorganisation : scenarios_courts_v8_<date>.parquet -> 30_courts/scenarios_courts.parquet ; sinon passez fichier_courts).", call. = FALSE)
  ml <- lire_meta_si_present(FICHIER_LIVRABLE_META(campagne)); present <- file.exists(FICHIER_LIVRABLE(campagne)) || dir.exists(DIR_LIVRABLE_PARTS(campagne))
  if(!is.null(ml) || present){
    if(is.null(ml) || !identical(ml$origine, "adoption")) stop("etape_adopter_campagne : un livrable de la campagne " %+% campagne %+% " existe déjà (" %+% FICHIER_LIVRABLE(campagne) %+% ") et n'est pas une adoption : différent, rien n'est écrasé.", call. = FALSE)
    if(!identical(as.character(ml$sources$longs), as.character(ch$source)) || !identical(as.character(ml$sources$courts), as.character(fichier_courts)))
      stop("etape_adopter_campagne : la campagne " %+% campagne %+% " a déjà été adoptée depuis d'autres sources (longs : " %+% ml$sources$longs %+% " ; courts : " %+% ml$sources$courts %+% ") : différent, rien n'est écrasé.", call. = FALSE)
    cat("livrable ", campagne, " déjà adopté depuis les mêmes sources : réécriture idempotente\n", sep = "")
  }
  typo <- charger_typo()
  pl <- preparer_longs_adoptes(lire_corpus_longs_historique(ch$source, populations), campagne, typo)
  cat(sprintf("longs adoptés : %d lignes, %d scénarios distincts, populations %s\n", nrow(pl$df), nrow(pl$registre), paste(sort(unique(pl$df$population)), collapse = ",")))
  pc <- preparer_courts_adoptes(arrow::read_parquet(fichier_courts), campagne, typo)
  cat(sprintf("courts adoptés : %d lignes, %d scénarios distincts (variantes d'habillage admin repliées)\n", nrow(pc$df), nrow(pc$registre)))
  reg <- dplyr::bind_rows(pl$registre, pc$registre)
  if(anyDuplicated(reg$id_scenario)) stop("etape_adopter_campagne : id_scenario dupliqué dans la campagne adoptée.", call. = FALSE)
  f_reg <- ecrire_registre_campagne(reg, campagne, DIR_REGISTRE())
  liv <- ecrire_livrable(pc$df, list(pl$df), NULL, campagne)
  reg_l <- lire_registre(DIR_REGISTRE())$lignes; reg_l <- reg_l[reg_l$campagne == campagne, , drop = FALSE]
  v <- verifier_adoption(lire_corpus_final(campagne), reg_l)
  cat("vérifications livrable / registre : ", v$texte, if(v$ok) "" else "  <- ÉCART (registre préexistant différent ? voir registre_" %+% campagne %+% ".parquet)", "\n", sep = "")
  mh <- lire_meta_si_present(FICHIER_COURTS_HISTORIQUE_META())
  meta_liv <- list(livrable = basename(if(liv$forme == "monofichier") FICHIER_LIVRABLE(campagne) else DIR_LIVRABLE_PARTS(campagne)), forme = liv$forme, campagne = campagne, date = as.character(Sys.Date()), PROFIL = PROFIL,
                   origine = "adoption", sources = list(longs = ch$source, longs_date = as.character(as.Date(file.info(ch$source)$mtime)), courts = fichier_courts, courts_date = as.character(as.Date(file.info(fichier_courts)$mtime)),
                                                        courts_date_origine = if(!is.null(mh$date_origine)) as.character(mh$date_origine) else NA),
                   total = liv$total, n_long = liv$n_long, n_court = liv$n_court,
                   volumes = list(long = list(lignes = liv$n_long, scenarios = nrow(pl$registre)), court = list(lignes = liv$n_court, scenarios = nrow(pc$registre))),
                   ratio_courts_realise = if(nrow(pl$registre) > 0) round(nrow(pc$registre) / nrow(pl$registre), 4) else NA,
                   verification_registre = list(ok = v$ok, livrable = as.list(v$livrable), registre = as.list(v$registre)),
                   populations = sort(unique(pl$df$population)), REGISTRE_ACTIF = TRUE, version_recette_id = RECETTE_ID, version_recette_id_courts = RECETTE_ID_COURTS,
                   colonnes = as.list(liv$colonnes), types = as.list(liv$types), familles_colonnes = liv$familles, notes_familles = list(audit = NOTE_POIDS),
                   note = "campagne ADOPTÉE (longs + courts historiques, aucun re-tirage) ; branche en tête, union de schémas ; les campagnes suivantes tirent leurs propres courts sous 40_campagnes/")
  yaml::write_yaml(meta_liv, FICHIER_LIVRABLE_META(campagne))
  copies <- character(0)
  if(isTRUE(annexes)){
    d_src <- dirname(ch$source); tag <- sub("^scenarios_longs_tirage_v8_([0-9]{8}).*$", "\\1", basename(ch$source))
    for(a in list(c("^rapport_v8_" %+% tag %+% "\\.txt$", FICHIER_RAPPORT(campagne)), c("^echantillon_revue(_v8)?_" %+% tag %+% "\\.csv$", FICHIER_REVUE(campagne)), c("^top30_das_par_cmd(_v8)?_" %+% tag %+% "\\.csv$", FICHIER_TOP30(campagne)))){
      f <- list.files(d_src, pattern = a[1], full.names = TRUE)
      if(length(f) == 1 && file.copy(f[1], a[2], overwrite = TRUE)){ copies <- c(copies, a[2]); cat("annexe copiée et renommée : ", basename(f[1]), " -> ", basename(a[2]), "\n", sep = "") }
    }
  }
  cat("livrable adopté ", liv$forme, " : ", liv$total, " lignes (longs = ", liv$n_long, ", courts = ", liv$n_court, ") -> ", FICHIER_LIVRABLE(campagne), "\n", sep = "")
  banniere_fin("etape_adopter_campagne", t0, c(liv$fichiers, FICHIER_LIVRABLE_META(campagne), f_reg, copies))
}

## ---- 4. Tableau de bord ----
# Statut FAIT / PARTIEL / À FAIRE par étape et pour le profil courant, avec preuves (fichiers seulement : aucune
# connexion requise). Arborescence par étapes : chaque ligne porte [partagé] (magasin commun aux profils) ou [profil].
etat_pipeline <- function(){
  plan <- resoudre_besoins(TYPES_ETBS_LONGS, ANS_HISTORIQUE, AN_REF,
                           fichiers_partiels = if(dir.exists(DIR_PARTIELS)) list.files(DIR_PARTIELS, pattern = "^catalogue_partiel_.*\\.parquet$") else character(0),
                           fichiers_exports = fichiers_refs_presents(),
                           forcer_refs = FALSE, noms_refs = NOMS_REFS, refs_chroniques = REFS_CHRONIQUES, ans_courts = ANS_COURTS, refs_courts = REFS_COURTS)
  statut3 <- function(n, total) if(total == 0) "À FAIRE" else if(n >= total) "FAIT" else if(n > 0) "PARTIEL" else "À FAIRE"
  lire_yaml <- function(f) if(file.exists(f)) yaml::read_yaml(f) else NULL
  rel <- function(p) sub("^" %+% gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", PATH_RESULTS), "", sub("/*$", "", p))
  garde <- function(magasin, f){ v <- verifier_magasin(magasin, lire_yaml(f), valeurs_effectives_config()); if(v$ok) "" else " ; GARDE EN ÉCART : " %+% paste(v$differences, collapse = ", ") }
  rows <- list()
  ajouter <- function(etape, statut, preuve, partage) rows[[length(rows) + 1]] <<- data.frame(etape = etape, statut = statut, partage = partage, preuve = preuve, stringsAsFactors = FALSE)
  # 1. prep_data
  tt <- if(exists("conn", envir = globalenv()) && !is.null(get("conn", envir = globalenv()))){
    ok <- vapply(c("prep_data_" %+% AN_REF, "prep_das_chro_" %+% AN_REF), table_temporaire_existe, logical(1))
    paste0(names(ok), " : ", ifelse(ok, "présente", "absente"), collapse = " ; ")
  } else "tables temporaires : inconnu hors connexion"
  ajouter("etape_prep_data", "(session)", tt %+% " ; " %+% rel(DIR_PARTIELS) %+% "/_meta.yaml : " %+% if(file.exists(FICHIER_PARTIELS_META())) "présent" else "absent", "[session]")
  # 2. refs
  n_refs <- sum(!plan$refs$a_faire)
  ajouter("etape_refs", statut3(n_refs, nrow(plan$refs)), sprintf("refs présentes %d / %d dans %s%s%s", n_refs, nrow(plan$refs), rel(DIR_REFERENCES),
          if(n_refs < nrow(plan$refs)) " ; manquantes : " %+% paste(plan$refs$nom[plan$refs$a_faire], collapse = ", ") else "", garde("references", FICHIER_REFERENCES_META())), "[partagé]")
  # 3. partiels
  n_p <- sum(!plan$iterations$a_faire)
  ajouter("etape_partiels_longs", statut3(n_p, nrow(plan$iterations)), sprintf("partiels présents %d / %d attendus du plan (%s × %s) dans %s%s%s", n_p, nrow(plan$iterations),
          paste(TYPES_ETBS_LONGS, collapse = ","), paste(range(ANS_HISTORIQUE), collapse = "-"), rel(DIR_PARTIELS),
          if(n_p < nrow(plan$iterations)) " ; manquants : " %+% paste(plan$iterations$fichier[plan$iterations$a_faire], collapse = ", ") else "",
          " ; " %+% basename(FICHIER_DIAG_MEMOIRE()) %+% " : " %+% if(file.exists(FICHIER_DIAG_MEMOIRE())) "présent" else "absent"), "[partagé]")
  # 4. catalogue (magasin 20_catalogue : monofichier transitoire, parts + _meta.yaml)
  side <- lire_yaml(FICHIER_CATALOGUE_META()); m <- if(!is.null(side)) side else lire_yaml(FICHIER_CATALOGUE_MONO_META())
  ds_ok <- dir.exists(DIR_CATALOGUE()) && length(list.files(DIR_CATALOGUE(), pattern = "^part_")) > 0
  ajouter("etape_catalogue", if(!is.null(m) && (ds_ok || file.exists(MONO_CATALOGUE()))) "FAIT" else "À FAIRE",
          if(is.null(m)) "catalogue absent de " %+% rel(DIR_CATALOGUE_M) else sprintf("catalogue du %s : %s lignes ; périmètre %s × %s ; CONVERSION_E669 = %s%s", m$date, if(!is.null(m$nb_lignes_total)) m$nb_lignes_total else m$nb_lignes,
                                                                     paste(unlist(m$TYPES_ETBS_LONGS), collapse = ","), paste(range(unlist(m$ANS_HISTORIQUE)), collapse = "-"), m$CONVERSION_E669,
                                                                     garde("catalogue", if(!is.null(side)) FICHIER_CATALOGUE_META() else FICHIER_CATALOGUE_MONO_META())), "[partagé]")
  ajouter("etape_repartitionner_catalogue", if(ds_ok && !is.null(side)) "FAIT" else "À FAIRE",
          if(ds_ok && !is.null(side)) sprintf("%d parts par lettre ; %s lignes ; typologie %s ; recette %s ; %s", length(side$nb_lignes_par_part), side$nb_lignes_total, side$version_typologie, side$version_recette_id, side$date)
          else if(file.exists(MONO_CATALOGUE())) "monofichier présent, non partitionné" else "absent", "[partagé]")
  # 5. tirable courts (magasin 30_courts : pivots = catalogue des courts)
  mc <- lire_yaml(FICHIER_COURTS_META())
  ajouter("tirable_courts", if(file.exists(FICHIER_PIVOTS_COURTS())) "FAIT" else "À FAIRE",
          if(file.exists(FICHIER_PIVOTS_COURTS())) sprintf("%s : %s pivots (ANS_COURTS = %s, %s)%s", rel(FICHIER_PIVOTS_COURTS()), format(if(!is.null(mc$n_pivots)) mc$n_pivots else nrow(arrow::read_parquet(FICHIER_PIVOTS_COURTS(), as_data_frame = FALSE))), paste(unlist(if(!is.null(mc$ANS_COURTS)) mc$ANS_COURTS else ANS_COURTS), collapse = ","), if(!is.null(mc$date)) mc$date else "méta absent", garde("courts", FICHIER_COURTS_META()))
          else "ref_pivots_courts.parquet absent de " %+% rel(DIR_COURTS) %+% " (etape_refs())" %+% if(file.exists(FICHIER_COURTS_HISTORIQUE())) " ; corpus courts historique présent (adoption)" else "", "[partagé]")
  # 6. sélection (campagne courante)
  mt <- lire_yaml(FICHIER_SELECTION_META())
  fixe <- !is.null(mt) && identical(mt$MODE_SELECTION, "quota_dp_fixe")
  ajouter("etape_selection_longs", if(is.null(mt)) "À FAIRE" else "FAIT",
          if(is.null(mt)) rel(FICHIER_SELECTION_META()) %+% " absent" else if(fixe) sprintf("campagne %s ; mode quota_dp_fixe ; NB_CRH_CIBLE %s ; k %s ; volume attendu %s ; populations %s ; date %s", CAMPAGNE, mt$NB_CRH_CIBLE, mt$NB_LIGNES_PAR_DP, mt$volume_attendu, paste(unlist(mt$populations), collapse = ","), mt$date)
          else sprintf("campagne %s ; mode %s ; budget %s ; NB_VARIANTES %s ; volume attendu %s ; date %s", CAMPAGNE, mt$MODE_SELECTION, mt$BUDGET_TOTAL_LONGS, mt$NB_VARIANTES, mt$volume_attendu, mt$date), "[profil]")
  # 7. chunks longs et habillage (campagne courante)
  if(fixe){
    pr <- character(0); n_tot <- 0L; att_tot <- 0L
    for(pop in unlist(mt$populations)){
      sd <- lire_yaml(file.path(DIR_CHUNKS_POP(pop), "longs_chunks_meta.yaml")); np <- if(dir.exists(DIR_CHUNKS_POP(pop))) length(list.files(DIR_CHUNKS_POP(pop), pattern = "^longs_chunk_[0-9]{4}\\.parquet$")) else 0L
      att <- if(is.null(sd)) NA else sd$nb_chunks; n_tot <- n_tot + np; att_tot <- att_tot + (if(is.na(att)) 0L else att)
      pr <- c(pr, sprintf("%s %d / %s", pop, np, format(att)))
    }
    ajouter("etape_tirage_das_longs", if(att_tot > 0) statut3(n_tot, att_tot) else if(n_tot > 0) "PARTIEL" else "À FAIRE", "chunks longs (" %+% rel(DIR_CHUNKS_LONGS()) %+% ") : " %+% paste(pr, collapse = " ; "), "[profil]")
    nh <- sum(vapply(unlist(mt$populations), function(pop) if(dir.exists(DIR_HABILLE(pop))) length(list.files(DIR_HABILLE(pop), pattern = "^lot_")) else 0L, integer(1)))
    ajouter("etape_habillage_longs", if(nh > 0) "FAIT" else "À FAIRE", sprintf("lots habillés : %d (%s/habille/<population>/)", nh, rel(DIR_CAMPAGNE())), "[profil]")
  } else {
    n_longs_att <- if(is.null(mt)) NA else { nl <- if(identical(mt$MODE_SELECTION, "quota_dp")) mt$volume_attendu else mt$nrow_catalogue; as.integer(ceiling(nl / taille_chunk(nl))) }
    n_longs <- if(dir.exists(DIR_CHUNKS_LONGS())) length(list.files(DIR_CHUNKS_LONGS(), pattern = "^longs_chunk_")) else 0L
    ajouter("etape_tirage_das_longs", if(!is.na(n_longs_att)) statut3(n_longs, n_longs_att) else if(n_longs > 0) "PARTIEL" else "À FAIRE", sprintf("chunks longs %d / %s (%s)", n_longs, format(n_longs_att), rel(DIR_CHUNKS_LONGS())), "[profil]")
    ajouter("etape_habillage_longs", if(!is.null(etat_tirage("df_scenarios_longs"))) "FAIT (session)" else "(session)", "résultat en mémoire de session uniquement", "[session]")
  }
  # 7b. courts de la campagne courante (étape de campagne)
  sdc <- lire_yaml(file.path(DIR_CHUNKS_COURTS(), "courts_chunks_meta.yaml")); ncc <- if(dir.exists(DIR_CHUNKS_COURTS())) length(list.files(DIR_CHUNKS_COURTS(), pattern = "^courts_chunk_[0-9]{4}\\.parquet$")) else 0L
  mcc <- lire_yaml(FICHIER_COURTS_CAMPAGNE_META())
  ajouter("etape_tirage_courts", if(file.exists(FICHIER_COURTS_CAMPAGNE())) "FAIT" else if(ncc > 0) "PARTIEL" else "À FAIRE",
          sprintf("campagne %s : chunks courts %d / %s (%s) ; scénarios courts : %s", CAMPAGNE, ncc, format(if(is.null(sdc)) NA else sdc$nb_chunks), rel(DIR_CHUNKS_COURTS()),
                  if(file.exists(FICHIER_COURTS_CAMPAGNE())) sprintf("%s lignes, %s scénarios, budget %s (%s), %s", format(mcc$n_lignes), format(mcc$scenarios_gardes), format(mcc$budget), mcc$source_budget, mcc$date) else "absents (après la sélection : etape_tirage_courts())"), "[profil]")
  # 8. registre et livrable (profil)
  reg <- tryCatch(lire_registre(DIR_REGISTRE()), error = function(e) NULL)
  ajouter("registre_tirages", if(!is.null(reg) && reg$nb_campagnes > 0) "FAIT" else "À FAIRE",
          if(is.null(reg) || reg$nb_campagnes == 0) "aucune campagne inscrite (" %+% rel(DIR_REGISTRE()) %+% ")" else sprintf("%d campagne(s) : %s ; %d scénarios cumulés (%d longs, %d courts) ; %d profils", reg$nb_campagnes, paste(sub("^registre_(.*)\\.parquet$", "\\1", basename(reg$fichiers)), collapse = ", "), reg$nb_scenarios, sum(reg$lignes$branche == "long"), sum(reg$lignes$branche == "court"), nrow(reg$par_profil)), "[profil]")
  livrables <- if(dir.exists(DIR_EXPORT_FINAL)) sub("^scenarios_(.*)_meta\\.yaml$", "\\1", list.files(DIR_EXPORT_FINAL, pattern = "^scenarios_.*_meta\\.yaml$")) else character(0)
  ml <- lire_yaml(FICHIER_LIVRABLE_META())
  ajouter("etape_finalisation", if(!is.null(ml) && (file.exists(FICHIER_LIVRABLE()) || dir.exists(DIR_LIVRABLE_PARTS()))) "FAIT" else "À FAIRE",
          sprintf("livrables dans %s : %s ; campagne courante %s : %s", rel(DIR_EXPORT_FINAL), if(length(livrables)) paste(livrables, collapse = ", ") else "aucun", CAMPAGNE,
                  if(is.null(ml)) "absent" else sprintf("%s (%s, %s lignes = %s longs + %s courts, %s) ; rapport : %s", ml$livrable, ml$forme, ml$total, ml$n_long, ml$n_court, ml$date, if(file.exists(FICHIER_RAPPORT())) "présent" else "absent")), "[profil]")
  etat <- do.call(rbind, rows)
  cat("== ÉTAT DU PIPELINE — PROFIL = ", PROFIL, " — ", format(Sys.time(), "%Y-%m-%d %H:%M"), " — PATH_RESULTS = ", PATH_RESULTS, " ==\n", sep = "")
  for(i in seq_len(nrow(etat))) cat(sprintf("  %-30s %-14s %-9s %s\n", etat$etape[i], etat$statut[i], etat$partage[i], etat$preuve[i]))
  invisible(etat)
}

## ---- 5. Réorganisation SUR PLACE de l'ancien results/ (chantier « arborescence par étapes ») ----
# Geste préalable (RUN.md) : copier À LA MAIN dans PATH_RESULTS/_a_reorganiser/ les trois dossiers de l'ancien
# results/ (partiels/, exports/, exports_diagnostic/). mode "plan" (défaut) : NE DÉPLACE RIEN, imprime les trois tables
# (reconnus -> destination, ignorés, non reconnus) ; mode "executer" : applique exactement le plan par COPIE depuis
# _a_reorganiser/ (intact, supprimable à la main ensuite), convertit les métas, vérifie, idempotent ; garde-fou : magasin
# différent déjà présent -> stop (verifier_magasin). migrer_registre : TRUE = les campagnes passées comptent.
etape_reorganiser <- function(dossier = file.path(PATH_RESULTS, "_a_reorganiser"), mode = c("plan", "executer"), migrer_registre = FALSE){
  mode <- match.arg(mode)
  t0 <- banniere_debut("etape_reorganiser", "mode = " %+% mode %+% " ; source = " %+% dossier %+% " ; migrer_registre = " %+% migrer_registre)
  if(!dir.exists(dossier)) stop("etape_reorganiser : dossier " %+% dossier %+% " absent. Copiez à la main (cp -r) partiels/, exports/ et exports_diagnostic/ de l'ancien results/ dans ce dossier (RUN.md, procédure de changement de répertoire).", call. = FALSE)
  fichiers <- list.files(dossier, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  if(length(fichiers) == 0) stop("etape_reorganiser : " %+% dossier %+% " est vide.", call. = FALSE)
  info <- file.info(file.path(dossier, fichiers))
  inventaire <- data.frame(chemin = fichiers, mtime = info$mtime, taille = info$size, stringsAsFactors = FALSE)
  plan <- planifier_reorganisation(inventaire, migrer_registre = migrer_registre)
  imprimer_plan_reorganisation(plan)
  nb <- table(factor(plan$categorie, levels = c("reconnu", "ignore", "inconnu")))
  cat(sprintf("\n%d fichiers inventoriés : %d reconnus, %d ignorés, %d non reconnus (aucun fichier ne manque à l'appel : %s)\n", nrow(plan), nb[["reconnu"]], nb[["ignore"]], nb[["inconnu"]], if(sum(nb) == nrow(plan)) "oui" else "NON"))
  if(mode == "plan"){ cat("mode plan : RIEN n'a été déplacé. Relancer avec mode = \"executer\" pour appliquer ce plan.\n"); return(invisible(plan)) }
  rec <- plan[plan$categorie == "reconnu", , drop = FALSE]
  copies <- 0L; deja <- 0L
  for(i in seq_len(nrow(rec))){
    src <- file.path(dossier, rec$source[i]); dst <- file.path(PATH_RESULTS, rec$destination[i]); creer_dossiers(dirname(dst))
    if(rec$action[i] == "convertir méta"){
      if(rec$destination[i] == "00_partiels/_meta.yaml"){
        m <- yaml::read_yaml(src); m$magasin <- "partiels"; m$provenance <- rec$source[i]
      } else {   # sidecar du catalogue + clés du magasin depuis catalogue_longs_seuil_meta.yaml (même dossier d'exports), sinon config
        side <- yaml::read_yaml(src); f_cm <- file.path(dossier, dirname(dirname(rec$source[i])), "catalogue_longs_seuil_meta.yaml")
        cm <- if(file.exists(f_cm)) yaml::read_yaml(f_cm) else valeurs_effectives_config()
        m <- c(list(magasin = "catalogue"), cm[intersect(CLES_MAGASINS$catalogue, names(cm))], side[setdiff(names(side), CLES_MAGASINS$catalogue)]); m$provenance <- rec$source[i]
      }
      if(file.exists(dst)){ v <- verifier_magasin(if(rec$destination[i] == "00_partiels/_meta.yaml") "partiels" else "catalogue", yaml::read_yaml(dst), m)
        if(!v$ok) stop("etape_reorganiser : magasin différent déjà présent — " %+% v$message, call. = FALSE); deja <- deja + 1L; next }
      yaml::write_yaml(m, dst); copies <- copies + 1L; next
    }
    if(file.exists(dst)){ if(file.size(dst) == file.size(src)) deja <- deja + 1L else stop("etape_reorganiser : " %+% dst %+% " existe déjà avec une taille différente (magasin différent déjà présent) : arbitrage humain.", call. = FALSE); next }
    if(!file.copy(src, dst, overwrite = FALSE)) stop("etape_reorganiser : copie échouée " %+% src %+% " -> " %+% dst, call. = FALSE)
    copies <- copies + 1L
  }
  # métas reconstruits pour les magasins qui n'en avaient pas (références, courts)
  cfg <- valeurs_effectives_config()
  f_cm_src <- rec$source[rec$destination == "20_catalogue/catalogue_longs_seuil_meta.yaml"]
  cfg_refs <- if(length(f_cm_src)){ cm <- yaml::read_yaml(file.path(dossier, f_cm_src[1])); modifyList(cfg, cm[intersect(CLES_MAGASINS$references, names(cm))]) } else cfg
  if(any(startsWith(rec$destination, "10_references/")) && !file.exists(FICHIER_REFERENCES_META())){
    yaml::write_yaml(meta_magasin("references", cfg_refs, provenance = "reorganisation (clés reprises du méta du catalogue d'origine, sinon config)", refs = as.list(NOMS_REFS)), FICHIER_REFERENCES_META()); copies <- copies + 1L }
  if(any(rec$destination == "30_courts/ref_pivots_courts.parquet") && !file.exists(FICHIER_COURTS_META())){
    yaml::write_yaml(meta_magasin("courts", cfg_refs, provenance = "reorganisation (clés = config courante)", n_pivots = nrow(arrow::read_parquet(FICHIER_PIVOTS_COURTS(), as_data_frame = FALSE)), fichier = basename(FICHIER_PIVOTS_COURTS()),
                                  note = "le TIRABLE courts (pivots = catalogue des courts) ; méta reconstruit à la réorganisation"), FICHIER_COURTS_META()); copies <- copies + 1L }
  src_courts <- rec$source[rec$destination == "30_courts/scenarios_courts.parquet"]
  if(length(src_courts) && !file.exists(FICHIER_COURTS_HISTORIQUE_META())){
    d_orig <- sub("^.*scenarios_courts_v8_([0-9]{8})\\.parquet$", "\\1", src_courts[1])
    yaml::write_yaml(list(objet = "corpus courts historique (fixe)", provenance = src_courts[1], date_origine = d_orig, date = as.character(Sys.Date()), n_lignes = nrow(arrow::read_parquet(FICHIER_COURTS_HISTORIQUE(), as_data_frame = FALSE)),
                          note = "corpus courts de l'ancienne génération, dé-daté ; à adopter en C1 avec les longs (etape_adopter_campagne), jamais re-tiré"), FICHIER_COURTS_HISTORIQUE_META()); copies <- copies + 1L }
  # vérifications
  cat(sprintf("\nexécuté : %d fichiers copiés / convertis, %d déjà présents (identiques)\n", copies, deja))
  n_part <- length(list.files(DIR_PARTIELS, pattern = "^catalogue_partiel_.*\\.parquet$")); cat("  partiels dans ", DIR_PARTIELS, " : ", n_part, "\n", sep = "")
  refs_pres <- file.exists(vapply(NOMS_REFS, FICHIER_REF, character(1))); cat("  références : ", sum(refs_pres), " / ", length(NOMS_REFS), " (tirable courts dans 30_courts)", if(!all(refs_pres)) " ; manquantes : " %+% paste(NOMS_REFS[!refs_pres], collapse = ", ") else "", "\n", sep = "")
  if(file.exists(FICHIER_CATALOGUE_META())){ side <- yaml::read_yaml(FICHIER_CATALOGUE_META()); parts <- list.files(DIR_CATALOGUE(), pattern = "^part_.*\\.parquet$", full.names = TRUE)
    n_lig <- sum(vapply(parts, function(f) nrow(arrow::read_parquet(f, as_data_frame = FALSE)), integer(1)))
    cat("  catalogue : ", length(parts), " parts, ", n_lig, " lignes (", if(n_lig == side$nb_lignes_total) "== méta" else "DIFFÉRENT du méta " %+% side$nb_lignes_total, ")\n", sep = "")
    if(n_lig != side$nb_lignes_total) stop("etape_reorganiser : lignes du catalogue != méta.", call. = FALSE) }
  if(file.exists(FICHIER_COURTS_HISTORIQUE())) cat("  corpus courts historique : ", FICHIER_COURTS_HISTORIQUE(), " (", nrow(arrow::read_parquet(FICHIER_COURTS_HISTORIQUE(), as_data_frame = FALSE)), " lignes ; à adopter : etape_adopter_campagne)\n", sep = "")
  if(migrer_registre && dir.exists(file.path(PATH_RESULTS, "production", "50_registre", "registre_tirages"))){
    reg <- lire_registre(file.path(PATH_RESULTS, "production", "50_registre", "registre_tirages"))
    cat("  registre migré (production) : ", reg$nb_campagnes, " campagne(s) ; ", reg$nb_scenarios, " scénarios\n", sep = "") }
  cat("_a_reorganiser/ laissé intact : à supprimer à la main après contrôle (etat_pipeline()).\n")
  banniere_fin("etape_reorganiser", t0, character(0))
  invisible(plan)
}
