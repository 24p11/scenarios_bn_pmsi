source("~/commun/projets_communs/DIM_siege/divers_projets/Scenario_crh_fictifs/utils.R")
path_projet ="~/commun/projets_communs/DIM_siege/divers_projets/Scenario_crh_fictifs/"
outfile= path_projet %+% "results/"
# On utilise le type d'autorisation
source("~/commun/projets_communs/DIM_siege/divers_projets/Scenario_crh_fictifs/exclusions.R")

conn <- pRatihque::connection_database()

# Pour tous les diag : on ne garde que les diag avec niveau de CMA, les autres sont transformé en catégorie CIM
# Ils seront tirés au sort en fonction de leur représentation dans la base au sein de la catégorie avec une pénalisation pour les .9


#pRatihque::atihble(conn, 'prd_vue_nompmsi.mco_diag_niveau') |> dplyr::filter(v2025>1) |> dplyr::collect() |> 
#  dplyr::distinct(code) |> readr::write_csv2(outfile %+% "cma.csv" )

TYPEAUT_SC = paste(c('01A', '01B', '02A', '02B', '02C', '02D', '02E', '02F', '02H', '02I',
                     '03A', '03B', '13A', '13B', '13G', '14A', '14B','16', 
                     '18', '15C', '15D', '15E', '15G', '15H', '15I'),collapse = "|")
TYPEAUT_SC_NEONAT = paste(c('05', '06'),collapse = "|")
TYPEAUT_UHCD <- c("07A","07B")
TYPEAUT_SC = c("01A","01B","13A","13B","03A","03B")
TYPEAUT_USI = c("02E","02A")

prep_data<-function(an,type_etbs){
  
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
        dplyr::distinct(ident,type_unite,.keep_all = TRUE) |> 
        dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                           dplyr::distinct(finessgeo,categ_pmsi)) |> 

        dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.fixe') |>
                            dplyr::select(anonyme,ident,dp,dr,age,sexe,provenance,modesortie,
                                          destination,duree,rumdudp,nbda,ghm2,passage_urg,nbrum)  |> 
                            dplyr::distinct(anonyme,ghm2,.keep_all = TRUE) |> 
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
                         dplyr::distinct(ident,.keep_all = TRUE))  |> 
      dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                         dplyr::filter(typ_diag==5,diag%in%c("I10")) |> 
                         dplyr::mutate(hta = "I10") |>
                         dplyr::select(ident,hta) |> 
                         dplyr::distinct(ident,.keep_all = TRUE) )-> query
    
 
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
      dplyr::distinct(ident,type_unite,.keep_all = TRUE) |> 
      dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                         dplyr::distinct(finessgeo,categ_pmsi)) |> 
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.fixe') |>
                          dplyr::filter(substr(ghm2,1,2)!="90") |> 
                          dplyr::distinct(anonyme,ghm2,.keep_all = TRUE) |> 
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
      dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                         dplyr::filter(typ_diag==5,diag%in%c(code_dnid_ins,code_dnid,code_did)) |> 
                         dplyr::mutate(diabete = dplyr::case_when(diag %in% code_dnid_ins ~ "E11i",
                                                                  diag %in% code_dnid ~ "E11ni",
                                                                  diag %in%code_did  ~ "E10",
                                                                  TRUE~NA)) |> 
                         dplyr::filter(!is.na(diabete)) |> 
                         dplyr::select(ident,diabete) |> 
                         dplyr::distinct(ident,.keep_all = TRUE))  |> 
      dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                         dplyr::filter(typ_diag==5,diag%in%c("I10")) |> 
                         dplyr::mutate(hta = "I10") |>
                         dplyr::select(ident,hta) |> 
                         dplyr::distinct(ident,.keep_all = TRUE) )-> query
    
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
      dplyr::distinct(ident,type_unite,.keep_all = TRUE) |>
      dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                         dplyr::distinct(finessgeo,categ_pmsi)) |> 
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.fixe') |>
                          dplyr::filter(substr(ghm2,1,2)!="90") |> 
                          dplyr::distinct(anonyme,ghm2,.keep_all = TRUE) |> 
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
      dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                         dplyr::filter(typ_diag==5,diag%in%c(code_dnid_ins,code_dnid,code_did)) |> 
                         dplyr::mutate(diabete = dplyr::case_when(diag %in% code_dnid_ins ~ "E11i",  
                                                                  diag %in% code_dnid ~ "E11ni",
                                                                  diag %in%code_did  ~ "E10",
                                                                  TRUE~NA)) |> 
                         dplyr::filter(!is.na(diabete)) |> 
                         dplyr::select(ident,diabete) |> 
                         dplyr::distinct(ident,.keep_all = TRUE))  |> 
      dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                         dplyr::filter(typ_diag==5,diag%in%c("I10")) |> 
                         dplyr::mutate(hta = "I10") |>
                         dplyr::select(ident,hta) |> 
                         dplyr::distinct(ident,.keep_all = TRUE) )-> query
    
    
    
  }

  query |> 
    dplyr::filter( (nbrum == 1 & type_unite == "UHCD" ) | type_unite != "UHCD" )  |> 
    dplyr::select(anonyme,ident,mode_hospit,mode_entree,mode_sortie,sexe,categ_pmsi,cage3,cage,racine,ghm2,
                  diabete,hta,diag2,mdp,rumdudp,nbda,duree,type_unite,prep_sc)  |>
    dplyr::filter(substr(ghm2,1,2)!="90") |> 
    dplyr::rename(age = cage3) |> 
    dplyr::mutate(diabete = ifelse(is.na(diabete),"N",diabete),
                  hta = ifelse(is.na(hta),"N",hta)) |> 
    dplyr::compute("prep_data_" %+% an,temporary=TRUE,overwrite=TRUE)
  
  
 
  
}

#----------------------------- Prépa DAS  -------------------------------#
prep_scenarios2<-function(an,type_etbs,nb_journees_aut,nbda_aut,nb_assoc_das,pivots){
  
  anseqta = dplyr::case_when(an<=17 ~ "21",
                             an>17 & an<=22 ~ "23",
                             TRUE ~ "25")
  
  
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
    
                    
    dplyr::left_join( pRatihque::atihble(conn, 'prd_vue_nompmsi.mco_diag_niveau') |> dplyr::filter(v2025>1) |> 
                        dplyr::select(all_of(c("code","v20"%+% anseqta))) |> 
                        dplyr::rename(das = code,niveau = !!dplyr::sym("v20"%+% anseqta))
    ) |> 
    
    dplyr::collect() -> df_das
  
  
  df_das |> 
    dplyr::mutate(niveau = ifelse(is.na(niveau),"0",niveau)) |> 
    dplyr::mutate(nb_das = dplyr::n(),.by= all_of(c(pivots,"das"))) -> df_das
  
  df_das |> 
    dplyr::arrange(ident,desc(niveau),desc(nb_das)) |> 
    dplyr::group_by(ident) |> 
    dplyr::slice(1:nb_assoc_das) -> df_das
  
  df_das |> 
    dplyr::group_by_at(c("ident",pivots)) |> 
    dplyr::arrange(das) |> 
    dplyr::summarise(diagnostic_associes = paste0(das,collapse = " "),.groups="drop") |> 
    dplyr::ungroup() |> 
    dplyr::summarise(n = dplyr::n(),.by=all_of(c(pivots,"diagnostic_associes"))) -> df_cases
  
  return(df_cases)
  
  
}

#----------------------------- Tirage au sort des DAS ----------------------#
sample_das<-function(mode_hospit,sexe,cage,racine,ghm2,diabete,hta,diag2,diagnostic_associes,nbda,df_das_ref,nb_tirage){
  
  mode_hospit_ = as.character(mode_hospit)
  sexe_ = as.character(sexe)
  cage_ = as.character(cage)
  ghm2_ = as.character(ghm2)
 
  da = unlist(stringr::str_split(diagnostic_associes," "))
  diagnostic_associes_ = paste(as.character(diagnostic_associes),collapse = "|")
  diag = as.character(diag2)
  diabete_ = as.character(diabete)
  diabete_ = dplyr::case_when(diag %in% code_dnid_ins ~ "E11i",  
                              diag %in% code_dnid ~ "E11ni",
                              diag %in%code_did  ~ "E10",
                              TRUE~diabete_)
  hta_ = as.character(hta)
  
  nbda = as.integer(nbda)
  
  
  
  df_das_ref |> dplyr::filter(diag2 == diag,mode_hospit == mode_hospit_, sexe_ ==sexe_, cage== cage_,ghm2==ghm2_,!das%in%da )  -> tmp

  if(nrow(tmp)<1) return()
  
  df_tmp<-NULL
  
  nb_max = min(nbda,nrow(tmp))

  for(i in 1:nb_tirage){
    
    sample(x= tmp$das,prob = tmp$n,size = nb_max)->das_samples
    
    das_samples<-filter_chap(c(da,das_samples))

    if(hta!="N" & length(intersect(hta_autres,das_samples))==0)das_samples<-c("I10",das_samples)
    
    #Pour le diabète :
    # - Vérification des DAS finalement choisis :
    #   * Si complication en lien avec diabète = complication multiples
    #   * Sinon : répartition en fonction de l'âge et du diabète
    if(diabete_!="N" ){
      
      if(length(intersect(c(diag,das_samples),codes_comp_sat_diab))!=0){
        
        code_diabete_sample<-retro_code_diabete(diabete_,7)
        comp=sample(c("2","3","4","5","6"),sample(3:4,1))
        
      }else{  df_res_epi_comp_diabete |> dplyr::filter(diabete == diabete_,cage==cage_) -> prep_diab
        
              sample(x= prep_diab$comp,prob = prep_diab$nb,size = 1)-> comp
              code_diabete_sample<-retro_code_diabete(diabete_,comp)
              if(comp=="8"){comp="9"
              code_diabete_sample<-retro_code_diabete(diabete_,9)
              }
              if(comp=="7"){comp=sample(c("2","3","4","5","6"),sample(3:4,1))}

      }

      das_samples = c(code_diabete_sample,das_samples)
      
      if(!"9"%in%comp){
        
        comp_diag<-NULL
        
            for(c in comp){   
             comp_diag_diag_tmp <- dplyr::case_when( c=="2"~sample(complications_diab$code[grepl("renal/asterisques_obligatoires",complications_diab$chemin)],1),
                                                     c=="3"~sample(complications_diab$code[grepl("oculaire/asterisques_obligatoires",complications_diab$chemin)],1), 
                                                     c=="4"~sample(complications_diab$code[grepl("neurologique/asterisques_obligatoires",complications_diab$chemin)],1), 
                                                     c=="5"~sample(complications_diab$code[grepl("vasculaire_peripherique/asterisques_obligatoires",complications_diab$chemin)],1), 
                                                     c=="6"~sample(complications_diab$code[grepl("autres_precisees/asterisques_obligatoires",complications_diab$chemin)],1)
                                                     )
             
             comp_diag<-c(comp_diag,comp_diag_diag_tmp)
            }   

        das_samples = c(comp_diag,das_samples)
      }
      
    }
    
    
    tibble::as_tibble(list("mode_hospit"=mode_hospit_,"sexe"=sexe_,"age"=age_,"ghm2"=ghm2_, "diag2" = diag,"nbda"=nbda )) |> 
      merge(tibble::as_tibble(list("diagnostic_associes" = paste(das_samples,collapse = " ")) ) ) |> 
      tibble::as_tibble() |> dplyr::bind_rows(df_tmp) -> df_tmp
    
  }
  
  #df_tmp_sav<<-df_tmp_sav |> dplyr::bind_rows(df_tmp)
  
  return(df_tmp)
  
}

filter_chap<-function(liste_diag){
  new_liste = NULL
  chap = NULL
  for(x in liste_diag){
    chap_ec = substr(x,1,1)
    if(chap_ec%in%chap){next}
    chap = c(chap,chap_ec)
    new_liste = c(new_liste,x)
  }
  return(new_liste)
}

#----------------------------- Preparation des tables ----------------------#

#Scenarios principaux
for(an in 17:26)prep_data(an)

#Tables de référence pour les diagnosticss associés
#Dia
an = 25
pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
  dplyr::filter(duree>3) |> 
  dplyr::rename(rum =  rumdudp) |> 
  dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                      dplyr::filter(typ_diag==5,!diag%in%c(comp_sat_diab,code_dnid_ins,code_dnid,code_did,
                                                           codes_astrisques_diabete,"I10")) |> 
                      dplyr::rename(das = diag) ) |> 
  
  dplyr::summarise(n =dplyr::n(),.by=all_of(c("mode_hospit","sexe","cage","racine","ghm2","diag2","das"))) |> 
  dplyr::collect()-> df_das_ref


pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
  dplyr::filter(categ_pmsi=="CHR/U") |> 
  dplyr::mutate(sc = max(prep_sc),.by = ident)
  dplyr::rename(rum =  rumdudp) |>   dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                                                         dplyr::filter(typ_diag==5,diag%in%c(code_dnid_ins,code_dnid,code_did))) |> 
  dplyr::mutate(diabete = dplyr::case_when(diag %in% code_dnid_ins ~ "E11i",  
                                           diag %in% code_dnid ~ "E11ni",
                                           diag %in%code_did  ~ "E10",
                                           TRUE~NA),
                comp = substr(diag,4,4)) |> 
  dplyr::summarise(nb = dplyr::n(),.by=c(cage,diabete,comp)) |> 
  dplyr::collect() ->df_res_epi_diabete_chu

#Correction des effectifs .9 (cf cartographie_cim10.R)
cage_ped<-c('[1-5[','[10-15[','[5-10[','[0-1[')
cage_ages<-c("[50-60[","[60-70[","[70-80[","[80-[")
df_res_epi_diabete_chu |>   dplyr::mutate(tot = sum(nb,na.rm=TRUE),.by=c(cage,diabete)) |> 
  dplyr::mutate(nb = dplyr::case_when(comp=="9"&cage%in%cage_ages~tot*0.2,
                                      comp=="9"&! cage%in%cage_ped~tot*0.5,
                                      TRUE~nb)) |> 
  dplyr::select(-tot) -> df_res_epi_comp_diabete





#----------------------------- Séjours longs -------------------------------#
pivots = c("mode_hospit","sexe","age","cage","racine","ghm2","diabete","hta","diag2","nbda")
type_etbs = c("CHR/U")
nb_journees_aut<-3:100
nbda_sup = 25
nb_assoc_das = 2
an_ref = 26
df_cases<-prep_scenarios2(an_ref,type_etbs,nb_journees_aut,nbda_sup,nb_assoc_das,pivots)
gc()
for(an in 17:(an_ref-1)){
  
  df_cases_tmp<-prep_scenarios2(an,type_etbs,nb_journees_aut,nbda_sup,nb_assoc_das,pivots)
  gc()
  df_cases <- dplyr::bind_rows(df_cases,df_cases_tmp) |> 
    dplyr::summarise(n =sum(n),.by=all_of(c(pivots,"diagnostic_associes")))
  rm(df_cases_tmp)
  
}

type_etbs = "CH"
for(an in 17:an_ref){
  
  df_cases_tmp<-prep_scenarios2(an,type_etbs,nb_journees_aut,nbda_sup,nb_assoc_das,pivots)
  gc()
  df_cases <- dplyr::bind_rows(df_cases,df_cases_tmp) |> 
    dplyr::summarise(n =sum(n),.by=all_of(c(pivots,"diagnostic_associes")))
  rm(df_cases_tmp)
  
}

# Sans 2025 : nb lignes = 8,679,586, nb extract (nb>9) 69,863
# New : nb lignes = 8,601,730 , nb extract (nb>9) 51,478

print("- Noombre de lignes = " %+% nrow(df_cases))
print("- Noombre de lignes = " %+% nrow(df_cases |> dplyr::filter(n>9)))

GHM_ACC_NORMAL = c("14C03A","14C07A", "14C08A", "14Z11A", "14Z12A",
                  "14Z13A", "14Z13T", "14Z14A", "14Z14T")
RACINES_ACC_PATHO = c("14C07", "14C08", "14Z10", "14Z11", "14Z12", "14Z13", "14Z14")
GHM_BB_NORMAL = c("15M05A", "15M06A", "15M07A", "15M08A", "15M09A",
                 "15M10A", "15M11A", "15M13A", "15M14A")
RACINES_BB_MED = c("15M05", "15M06", "15M07", "15M08", "15M09",
                  "15M10", "15M11", "15M13", "15M14")

df_cases<- df_cases |>  dplyr::inner_join(df_cases |> 
                                            dplyr::summarise(nb=sum(n),
                                                             .by =c("mode_hospit","sexe","age","cage",
                                                                    "racine","ghm2","diabete","hta","diag2") )  ) |> 
  dplyr::filter(nb>9) |> dplyr::select(-n)

print("- Noombre de lignes = " %+% nrow(df_cases))

arrow::write_parquet(df_cases , outfile %+% "scenarios_bn_long_sejours_prepa_v7.2_" %+% format(Sys.Date(),"%Y%m%d"))


df_cases |> dplyr::filter(ghm2 %in%c(GHM_ACC_NORMAL,GHM_BB_NORMAL,RACINES_ACC_PATHO,RACINES_BB_MED)) |> dplyr::sample_n(3000) ->df_cases_go

df_cases |> dplyr::filter(!ghm2 %in%c(GHM_ACC_NORMAL,GHM_BB_NORMAL,RACINES_ACC_PATHO,RACINES_BB_MED),nb>5000) |> 
  dplyr::bind_rows(df_cases_go) -> df_cases_

df_cases_f = purrr::pmap_df(df_cases_ |> 
                              dplyr::select_at(c("mode_hospit","sexe","cage","racine","ghm2","diabete","hta","diag2","diagnostic_associes","nbda")),
                            sample_das,df_das_ref,1)

#df_cases_f |> dplyr::mutate(diag = stringr::str_extract(diagnostic_associes,"\\bE1[0-4][0-9]{0,2}\\b"),comp = stringr::str_sub(diag,4,4)) |> dplyr::summarise(nb =dplyr::n(),.by=comp) |> 
#  dplyr::arrange(comp)

#Ajout des modes entrée/sortie
an = 25
pRatihque::atihble(conn, 'prep_data_' %+% an ) |> 
  dplyr::distinct(mode_hospit,mode_entree,mode_sortie,sexe,age,cage,ghm2,diag2,mdp,nbda,duree) |> 
  dplyr::collect()-> df_v_admin

df_cases_f |> dplyr::left_join(df_v_admin) -> df_cases_f

print("- Noombre de lignes = " %+% nrow(df_cases_f))
arrow::write_parquet(df_cases_f , outfile %+% "scenarios_bn_long_sejours_final_v7.2_" %+% format(Sys.Date(),"%Y%m%d"))


#----------------------------- Séjours courts -------------------------------#
pivots = c("mode_hospit","mode_entree","mode_sortie","sexe","age","cage","racine","ghm2","diag2","mdp","nbda")
type_etbs = "CHR/U"
nb_journees<-0:2
nbda_sup = 5
an_ref = 24
df_cases<-prep_scenarios(an_ref,type_etbs,nb_journees,nbda_sup,pivots)
gc()
for(an in 17:(an_ref-1)){
  
  df_cases_tmp<-prep_scenarios2(an,type_etbs,nb_journees,nbda_sup,pivots)
  gc()
  df_cases <- dplyr::bind_rows(df_cases,df_cases_tmp) |> 
    dplyr::summarise(n =sum(n),.by=all_of(c(pivots,"diagnostic_associes")))
  rm(df_cases_tmp)
  
}

# Sans 2025 : nb lignes = 5,818,147 , nb extract (nb>9) 45,152
arrow::write_parquet(df_cases |> dplyr::filter(n>9), outfile %+% "scenarios_bn_courts_sejours_" %+% format(Sys.Date(),"%Y%m%d"))




#----------------------------- Gériatrie, réan  -------------------------------#
prep_data<-function(an,type_etbs){
  
  if(an>22){
    pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.um') |>
      dplyr::mutate(mode_hospit = dplyr::case_when(type_hospum_1 == "P" ~"HP",
                                                   TRUE ~ "HC")) |> 
      #Ajouter soins critiques adu - ped - neonat / gériatrie
      dplyr::select(ident,finessgeo,mode_hospit) |> 
      dplyr::distinct(ident,.keep_all = TRUE) |> 
      dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                         dplyr::distinct(finessgeo,categ_pmsi)) |> 
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.fixe') |>
                          dplyr::select(anonyme,ident,dp,dr,age,sexe,provenance,modesortie,destination,duree,rumdudp,nbda,ghm2,passage_urg)  |> 
                          dplyr::distinct(anonyme,ghm2,.keep_all = TRUE) |> 
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
      ) -> query
    
    
  }
  
  if(an>17 & an<=22){
    pRatihque::atihble(conn,"PRD_VUE_MCOBL_20" %+% an %+%  '.um') |>
      dplyr::mutate(mode_hospit = dplyr::case_when(type_hospum_1 == "P" ~"HP",
                                                   TRUE ~ "HC")) |> 
      dplyr::select(ident,finessgeo,mode_hospit) |> 
      dplyr::distinct(ident,.keep_all = TRUE) |> 
      dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                         dplyr::distinct(finessgeo,categ_pmsi)) |> 
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.fixe') |>
                          dplyr::filter(substr(ghm2,1,2)!="90") |> 
                          dplyr::distinct(anonyme,ghm2,.keep_all = TRUE) |> 
                          dplyr::select(anonyme,ident,dp,dr,age,sexe,provenance,modesortie,destination,duree,rumdudp,nbda)  |> 
                          
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
                          dplyr::mutate(racine =substr(ghm2,1,5))) -> query
    
  }
  
  if(an<=17){
    
    pRatihque::atihble(conn,"PRD_VUE_MCOBL_20" %+% an %+%  '.um') |>
      dplyr::mutate(mode_hospit = dplyr::case_when(type_hospum_1 == "P" ~"HP",
                                                   TRUE ~ "HC")) |> 
      dplyr::select(ident,finessgeo,mode_hospit) |> 
      dplyr::distinct(ident,.keep_all = TRUE) |> 
      dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                         dplyr::distinct(finessgeo,categ_pmsi)) |> 
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.fixe') |>
                          dplyr::filter(substr(ghm2,1,2)!="90") |> 
                          dplyr::distinct(anonyme,ghm2,.keep_all = TRUE) |> 
                          dplyr::select(anonyme,ident,dp,dr,age,sexe,provenance,modesortie,destination,duree,rumdudp,nbda)  |> 
                          
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
                          dplyr::mutate(racine =substr(ghm2,1,5))) -> query
    
    
    
  }
  
  query |> dplyr::select(anonyme,ident,mode_hospit,mode_entree,mode_sortie,sexe,categ_pmsi,cage3,cage,racine,ghm2,diag2,mdp,rumdudp,nbda,duree)  |>
    dplyr::filter(substr(ghm2,1,2)!="90") |> 
    dplyr::rename(age = cage3) |> 
    dplyr::compute("prep_data_" %+% an,temporary=TRUE,overwrite=TRUE)
  
  
  
}





pivots = c("mode_hospit","sexe","age","racine","ghm2","diag2","mdp","nbda")
an = 24
pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
  dplyr::filter(categ_pmsi %in% type_etbs,duree%in%nb_journees,nbda<nbda_sup) |> 
  dplyr::rename(rum =  rumdudp) |> 
  dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCO_20" %+% an %+% '.diag') |> dplyr::filter(typ_diag==5) |> dplyr::rename(das = diag) ) |> 
  dplyr::distinct_at(c("ident",pivots,"das")) |> 
  dplyr::left_join( pRatihque::atihble(conn, 'prd_vue_nompmsi.mco_diag_niveau') |> dplyr::filter(v2025>1) |> 
                      dplyr::select(all_of(c("code","v20"%+% anseqta))) |> 
                      dplyr::rename(das = code,niveau = !!dplyr::sym("v20"%+% anseqta))
  ) |> 
  
  dplyr::collect() -> df_das

pivots2<-pivots[pivots!="nbda"]

df_das |> 
  dplyr::mutate(niveau = ifelse(is.na(niveau),0,niveau)) |> 
  dplyr::mutate(nb_das = dplyr::n(),.by= all_of(c(pivots2,"das"))) -> df_das

df_das |> dplyr::filter(nb_das>10) |> dplyr::distinct_at(c(pivots2,"das","nb_das")) |> dplyr::sample_frac(1) |> 
  dplyr::ungroup() |> 
  dplyr::group_by_at(pivots2) |> 
  dplyr::arrange_at(c(pivots2)) |> 
  dplyr::mutate(nb_das_sej = 1:dplyr::n()) |> 
  dplyr::arrange_at(c(pivots2,"nb_das_sej"))-> df_das_ref

pivots = c("mode_hospit","sexe","age","racine","ghm2","diag2","mdp","nbda")
type_etbs = "CHR/U"
nb_journees_aut<-3:100
nbda_aut = 4:15
an_ref = 24
df_cases<-prep_scenarios2(an_ref,type_etbs,nb_journees_aut,nbda_aut,pivots)
gc()
for(an in 17:(an_ref-1)){
  
  df_cases_tmp<-prep_scenarios2(an,type_etbs,nb_journees_aut,nbda_aut,pivots)
  gc()
  df_cases <- dplyr::bind_rows(df_cases,df_cases_tmp) |> 
    dplyr::summarise(n =sum(n),.by=all_of(c(pivots,"diagnostic_associes")))
  rm(df_cases_tmp)
  
}
gc()

type_etbs = "CH"
for(an in 17:an_ref){
  
  df_cases_tmp<-prep_scenarios2(an,type_etbs,nb_journees,nbda_sup,pivots)
  gc()
  df_cases <- dplyr::bind_rows(df_cases,df_cases_tmp) |> 
    dplyr::summarise(n =sum(n),.by=all_of(c(pivots,"diagnostic_associes")))
  rm(df_cases_tmp)
  
}

# Sans 2025 : nb lignes = 8,679,586, nb extract (nb>9) 23 271
arrow::write_parquet(df_cases |> dplyr::filter(n>9), outfile %+% "scenarios_bn_long_sejours_v2" %+% format(Sys.Date(),"%Y%m%d"))


distinct_das<-function(string){
  
  paste(unique(stringr::str_split(string," ")[[1]]),collapse = " ")
  
}


df_cases |> 
  dplyr::filter(n>9) |> 
  dplyr::left_join(df_das_ref) |> 
  dplyr::filter(nbda>nb_das_sej) |> 
  dplyr::group_by_at(names(df_cases)) |> 
  dplyr::arrange(das) |> 
  dplyr::summarise(diagnostic_associes2 = paste0(das,collapse = " "),.groups="drop") |> 
  dplyr::ungroup() |> 
  dplyr::mutate(diagnostic_associes = diagnostic_associes %+% " "%+% diagnostic_associes2) |> 
  dplyr::select(-diagnostic_associes2) |> 
  dplyr::mutate(diagnostic_associes=purrr::map_chr(diagnostic_associes, distinct_das)) -> df_cases_2

# Sans 2025 : nb lignes = 5,132,668  , nb extract (nb>9) 29,533
arrow::write_parquet(df_cases_2 |> dplyr::filter(n>9), outfile %+% "scenarios_bn_long_sejours_v2_" %+% format(Sys.Date(),"%Y%m%d"))



#----------------------------------------- Tirage et randomness ----------------------------------------#

pRatihque::atihble(conn, 'prep_data_' %+% an ) |> dplyr::summarise(nb=dplyr::n(),.by=c(mode_hospit,sexe,age,ghm2,diag2,mode_entree)) |> 
  dplyr::collect() |> 
  dplyr::group_by(mode_hospit,sexe,age,ghm2,diag2) |> 
  dplyr::summarise(mode_entree = paste0(mode_entree,collapse = ","),prob_mode_entree = paste0(nb,collapse = ","),.groups="drop")-> df_stat

pRatihque::atihble(conn, 'prep_data_' %+% an ) |> dplyr::summarise(nb=dplyr::n(),.by=c(mode_hospit,sexe,age,ghm2,diag2,mode_sortie)) |> 
  dplyr::collect() |> 
  dplyr::group_by(mode_hospit,sexe,age,ghm2,diag2) |> 
  dplyr::summarise(mode_sortie = paste0(mode_sortie,collapse = ","),prob_mode_sortie = paste0(nb,collapse = ","),.groups="drop") |> 
  dplyr::full_join(df_stat)-> df_stat

pRatihque::atihble(conn, 'prep_data_' %+% an ) |> dplyr::summarise(nb=dplyr::n(),.by=c(mode_hospit,sexe,age,ghm2,diag2,cage)) |> 
  dplyr::collect() |> 
  dplyr::group_by(mode_hospit,sexe,age,ghm2,diag2) |> 
  dplyr::summarise(cage = paste0(cage,collapse = ","),prob_cage = paste0(nb,collapse = ","),.groups="drop") |> 
  dplyr::full_join(df_stat)-> df_stat

pRatihque::atihble(conn, 'prep_data_' %+% an ) |> dplyr::summarise(nb=dplyr::n(),dms=mean(duree),vards=var(duree),.by=c(mode_hospit,sexe,age,ghm2,diag2)) |> 
  dplyr::filter(nb>9) |> 
  dplyr::collect() |> 
  dplyr::inner_join(df_stat)-> df_stat


cols_ref  = c("mode_hospit","sexe","age","ghm2","diag2")

df_cases |> dplyr::filter(n>9) |>  dplyr::summarise(nb = dplyr::n(),.by=c(mode_hospit,sexe,age,ghm2,diag2))  -> prep_df_cases


res<-purrr::pmap_df(prep_df_cases,sample_vars)

prep_df_cases |> dplyr::inner_join(res)


dplyr::bind_cols(res,df_cases |> dplyr::filter(n>9))

df_cases |> dplyr::filter(n>10) |> dplyr::full_join(res) |> 
  dplyr::filter(is.na(mode_entree))





### ------------------------------------------ Autres variables  -------------------------------------###
an = 25
pRatihque::atihble(conn, 'mco' %+% an %+% '.fixe') |>
  dplyr::filter(is.na(poids),is.na(agejour),is.na(age_gest)) |> 
  dplyr::summarise(nb = dplyr::n(),.by=c(ghm2, modeentree,provenance,modesortie,destination,top_gradation,
                                         contexte_pat,rescrit_tarif, cat_nb_inter, admin_rh,conv_hchp,
                                         raac, lit_palliatif, adnp, passage_urg)) |> 
  dplyr::collect()-> df_var_comp1

df_var_comp1 |> dplyr::filter(nb>10) |> View()
  
pRatihque::atihble(conn, 'mco' %+% an %+% '.fixe') |>
  dplyr::filter( ! is.na(poids) | ! is.na(agejour) | ! is.na(age_gest)) |> 
  dplyr::filter(! (is.na(age_gest) & poids < 4000 ) )  |> 
  dplyr::mutate(poids = ifelse(is.na(poids) | poids==9999,0,poids),
                #agejour = ifelse(is.na(agejour) ,0,agejour),
                age = ifelse(is.na(age) ,0,age),
                cat_poids = dplyr::case_when(
                  poids < 2000 ~ "[1500-2000[",
                  poids < 2500 ~ "[2000-2500[",
                  poids < 3000 ~ "[2500-3000[",
                  poids < 3500 ~ "[3000-3500[",
                  poids < 4000 ~ "[3500-4000[",
                  poids < 4500 ~ "[4000-4500[",
                  poids < 5000 ~ "[4500-5000[",
                  poids < 5500 ~ "[5000-5500[",
                  poids < 6000 ~ "[5500-6000[",
                  TRUE ~  "[6000-["
                )) |> 
  dplyr::summarise(nb = dplyr::n(),.by=c(ghm2, modeentree,provenance,modesortie,destination, passage_urg,age_gest,cat_poids,regles,age,agejour)) |> 
  dplyr::collect()-> df_var_comp2
  
df_var_comp2 |> dplyr::filter(nb>10) |> View()

hist(df_var_comp2$regles)




#----------------------------- READ PQ ------------------------#
#df<-arrow::read_parquet(outfile %+% "scenarios_bn_long_sejours_prepa_v7.2_20260822")
#
#df |> dplyr::distinct( mode_hospit,sexe,age,cage,racine,ghm2,  
#                       diabete,hta,diag2 ,diagnostic_associes)

