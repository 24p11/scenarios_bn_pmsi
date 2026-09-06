path_projet ="~/commun/projets_communs/DIM_siege/divers_projets/Scenario_crh_fictifs/"
outfile= path_projet %+% "results/"
conn <- pRatihque::connection_database()

source("~/commun/projets_communs/DIM_siege/divers_projets/Scenario_crh_fictifs/utils.R")
source("~/commun/projets_communs/DIM_siege/divers_projets/Scenario_crh_fictifs/referentiels.R")
# On utilise le type d'autorisation


file_version = "2"

df_ref_specialite <- readxl::read_excel(path_projet %+% "referentiels/referentiel_spe_racine_30_"%+%file_version%+%".xlsx", sheet = "Feuille1") |> 
  dplyr::rename(specialite_medicale = lib_spe_uma,cage2 = age)

#Extraction chirugie ambulatoire
#GHM en C (excluant les CMD 14 et 15) OU 
#GHM parmi la liste suivante :"03K02", "05K14", "11K07", "12K06", "09Z02", "14Z08", "23Z03"

prep_data<-function(an){

      pRatihque::atihble(conn, 'PRD_VUE_MCOBL_20'%+% an %+%'.um') |>
      dplyr::mutate(mode_hospit = dplyr::case_when(type_hospum_1 == "P" ~"HP",
                                                   TRUE ~ "HC")) |> 
      dplyr::select(ident,finessgeo,mode_hospit) |> 
      dplyr::distinct(ident,.keep_all = TRUE) |> 
      dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                         dplyr::distinct(finessgeo,categ_pmsi)) |> 
      dplyr::inner_join(pRatihque::atihble(conn, 'PRD_VUE_MCOBL_20' %+% an %+%'.fixe') |>
                          dplyr::select(anonyme,ident,dp,dr,age,sexe,provenance,modesortie,destination,duree,
                                        rumdudp,nbda,ghm2,passage_urg,raac)  |> 
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
                            diag2 = dplyr::case_when(diag2 %in% code_dnid_ins ~ "E11i",
                                                     diag2 %in% code_dnid ~ "E11ni",
                                                     diag2 %in%code_did  ~ "E10",
                                                     TRUE~diag2),
                            
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
                            cage2 = ifelse(age>=18,"ge_18","lt_18"),
                            cage2 = ifelse(cage2=="lt_18" & substr(ghm2,3,3)=="C" & age>14,"ge_18",cage2),
                            racine =substr(ghm2,1,5)
                          ) 
      ) |>
    dplyr::select(anonyme,ident,mode_hospit,mode_entree,mode_sortie,sexe,categ_pmsi,cage2,
                  cage,racine,ghm2,diag2,mdp,rumdudp,nbda,duree,raac)  |>
    dplyr::filter(substr(ghm2,1,2)!="90") |> 
    dplyr::compute("prep_data_" %+% an,temporary=TRUE,overwrite=TRUE)
  
  
  
}


prep_data(25)
# Chirurgie ambulatoire : 
# Base de tirage séjours en C de 0 j  + quelques racines
# Liste des diangotics associés les plus fréquents
# Info RAAC
# GHM en C (excluant les CMD 14 et 15) OU GHM parmi la liste suivante :"03K02", "05K14", "11K07", "12K06", "09Z02", "14Z08", "23Z03" 
prep_das<-function(an){
  
   pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
    dplyr::rename(rum =  rumdudp) |> 
    dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> dplyr::filter(typ_diag==5) |> 
                       dplyr::rename(das = diag) ) |> 
    dplyr::distinct_at(c("ident","diag2","das","cage","sexe")) |> 
    dplyr::left_join( pRatihque::atihble(conn, 'prd_vue_nompmsi.mco_diag_niveau') |> dplyr::filter(v2025>1) |> 
                        dplyr::select(all_of(c("code","v20"%+% an))) |> 
                        dplyr::rename(das = code,niveau = !!dplyr::sym("v20"%+% an))
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
    dplyr::summarise(nb_das = dplyr::n(),.by= c(diag2,das,sexe,cage,niveau,type_liste,caract)) |> 
    dplyr::compute("prep_das" %+% an,temporary=TRUE,overwrite=TRUE)

  
}
an =25
prep_das(an)


sample_das<-function(mode_hospit,sexe,cage,ghm2,diag2,duree,df_das){
  
  mode_hospit_ = as.character(mode_hospit)
  sexe_ = as.character(sexe)
  cage_ = as.character(cage)
  age_ =sample_age(cage_)
  ghm2_ = as.character(ghm2)
  diag = as.character(diag2)
  duree_ =as.integer(duree)
  
  neo_codes_diabete<-c("E10","E11i","E11ni")

  df_das |> dplyr::filter(diag2 == diag,sexe == sexe_,cage==cage_)  -> tmp
  #  df_das |> dplyr::filter(diag2 == diag,sexe == sexe_,cage==cage_, das%in%c( "E6694", "G473" ,"E11i"))  -> tmp
  if(substr(ghm2_,3,3)=="C"){
    tmp<-tmp |> dplyr::filter(das !="R2630", substr(das,1,2)!="F0", ( substr(das,1,2)!="F10"  | substr(das,1,3)=="F17") )
  }
  #  if(duree==0){
  #    tmp<-tmp |> dplyr::filter(niveau=="1")
  #  }
  
  if(nrow(tmp)<1) return()
  
  df_tmp<-NULL
  nb_max = min(4,nrow(tmp))
  nb_min = min(2,nrow(tmp))
  for(i in nb_min:nb_max){
    sample(x= tmp$das,prob = tmp$nb_das,size = i)->das_samples
    das_samples<-filter_cat(das_samples)
    
    diabete = intersect(neo_codes_diabete,c(das_samples,diag))
    
    if(length(diabete)>0){
      das_samples <- das_samples[!das_samples%in%diabete]
      codes_diabete = get_codes_diabete_from_neo(diabete,cage_)
      das_samples<-c(das_samples,codes_diabete)
    }
    
    tibble::as_tibble(list("mode_hospit"=mode_hospit_,"sexe"=sexe_,"age"=age_,"ghm2"=ghm2_, "diag2" = diag, "duree"=duree_)) |> 
      merge(tibble::as_tibble(list("diagnostic_associes" = paste(das_samples,collapse = " ")) ) ) |> 
      tibble::as_tibble() |> dplyr::bind_rows(df_tmp) -> df_tmp
    
  }
  
  
  return(df_tmp)
  
}

filter_cat<-function(liste_diag){
  new_liste = NULL
  chap = NULL
  for(x in liste_diag){
    chap_ec = substr(x,1,2)
    if(chap_ec%in%chap){next}
    chap = c(chap,chap_ec)
    new_liste = c(new_liste,x)
  }
  return(new_liste)
}


sample_age<-function(x){
  dplyr::case_when(x =="[0-1["~0,
                   x =="[1-5["~sample(1:5,1),
                   x =="[5-10["~sample(5:10,1),
                   x =="[10-15["~sample(10:15,1),
                   x =="[15-18["~sample(15:18,1),
                   x =="[18-30["~sample(18:30,1),
                   x =="[30-40["~sample(30:40,1),
                   x =="[40-50["~sample(40:50,1),
                   x =="[50-60["~sample(50:60,1),
                   x =="[60-70["~sample(60:70,1),
                   x =="[70-80["~sample(70:80,1),
                   x =="[80-["~sample(80:90,1),
                   TRUE~sample(20:60,1))
}

pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
  dplyr::filter(categ_pmsi=="CHR/U") |> 
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


pivots = c("mode_hospit","sexe","cage","ghm2","diag2","duree")

pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
  dplyr::filter(duree<3) |> dplyr::summarise(nb=dplyr::n(),.by=dplyr::all_of(pivots)) |> dplyr::filter(nb>10) |> 
  dplyr::collect()-> df_cases

df_das<- pRatihque::atihble(conn, "prep_das" %+% an) |> dplyr::collect()
df_das |> dplyr::mutate(niveau = case_when(das %in%neo_codes_diabete~"1",
                                           substr(das,1,1)=="C"~ "1",
                                           substr(das,2,2)=="E66"~"1")) -> df_cas

df_cases_f<-purrr::pmap_df(df_cases[,pivots],sample_das,df_das)
arrow::write_parquet(df_cases_f , outfile %+% "scenarios_bn_court_sejours_inter_v7.1_" %+% format(Sys.Date(),"%Y%m%d"))

an = 25
pRatihque::atihble(conn, 'prep_data_' %+% an ) |> 
  dplyr::distinct(mode_hospit,mode_entree,mode_sortie,sexe,cage,ghm2,diag2,mdp,duree) |> 
  dplyr::collect()-> df_v_admin



df_cases_f |> dplyr::mutate( cage = dplyr::case_when(
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
)) |> 
  dplyr::left_join(df_v_admin) |> 
  dplyr::group_by(mode_hospit,ghm2,diag2,diagnostic_associes,cage,duree) |> 
  dplyr::slice(1:2) |> 
  dplyr::ungroup()-> df_cases_f2
arrow::write_parquet(df_cases_f2 , outfile %+% "scenarios_bn_court_sejours_final_v7.1_" %+% format(Sys.Date(),"%Y%m%d"))

an = 25
pivots = c("mode_hospit","mode_entree","mode_sortie","sexe","cage","cage2","racine","ghm2","diag2","mdp","raac")

df_scenarios<- pRatihque::atihble(conn, "prep_data_" %+% an) |> 
  dplyr::filter(!substr(ghm2,1,2)%in%c("14","15"),duree==0) |> 
  dplyr::filter(substr(ghm2,3,3)=="C" | substr(ghm2,1,5) %in% c("03K02", "05K14", "11K07", "12K06", "09Z02", "14Z08", "23Z03")) |> 
  dplyr::collect()
df_scenarios_  <- df_scenarios |>   dplyr::summarise(nb = dplyr::n(),.by=all_of(pivots)) |> 
  dplyr::filter(nb>9) |> 
  dplyr::mutate(age = sample_age(cage)) |> 
  dplyr::left_join(df_dp_das,relationship = "many-to-many") |> 
  dplyr::left_join(df_ref_specialite |> dplyr::select(racine,cage2,specialite_medicale))
   

arrow::write_parquet(df_scenarios_ , outfile %+% "scenarios_chir_ambu_v3_" %+% format(Sys.Date(),"%Y%m%d"))





df_scenarios<- pRatihque::atihble(conn, "prep_data_" %+% an) |> 
  dplyr::filter(!substr(ghm2,1,2)%in%c("14","15"),duree<3) |> 
  dplyr::filter(substr(ghm2,3,3)=="C" | substr(ghm2,1,5) %in% c("03K02", "05K14", "11K07", "12K06", "09Z02", "14Z08", "23Z03")) |> 
  dplyr::collect()
df_scenarios |>   dplyr::summarise(nb = dplyr::n(),.by=all_of(pivots)) |> 
  dplyr::filter(nb>9) |> 
  dplyr::mutate(age2 = age,age = sample_age(cage)) |> 
  dplyr::left_join(df_dp_das,relationship = "many-to-many") -> df_scenarios_

arrow::write_parquet(df_scenarios_ , outfile %+% "scenarios_chir_ambu_" %+% format(Sys.Date(),"%Y%m%d"))

c("E112|E113|E114|E115|E116|E117|E118")

# ### Travaux sur les diagnostics
# E112	Diabète sucré de type 2, avec complications rénales
# E1120	Diabète sucré de type 2 insulinotraité, avec complications rénales
# E1128	Diabète sucré de type 2 non insulinotraité ou sans précision, avec complications rénales
# E113	Diabète sucré de type 2, avec complications oculaires
# E1130	Diabète sucré de type 2 insulinotraité, avec complications oculaires
# E1138	Diabète sucré de type 2 non insulinotraité ou sans précision, avec complications oculaires
# E114	Diabète sucré de type 2, avec complications neurologiques
# E1140	Diabète sucré de type 2 insulinotraité, avec complications neurologiques
# E1148	Diabète sucré de type 2 non insulinotraité ou sans précision, avec complications neurologiques
# E115	Diabète sucré de type 2, avec complications vasculaires périphériques
# E1150	Diabète sucré de type 2 insulinotraité, avec complications vasculaires périphériques
# E1158	Diabète sucré de type 2 non insulinotraité ou sans précision, avec complications vasculaires périphériques
# E116	Diabète sucré de type 2, avec autres complications précisées
# E1160	Diabète sucré de type 2 insulinotraité, avec autres complications précisées
# E1168	Diabète sucré de type 2 non insulinotraité ou sans précision, avec autres complications précisées
# E117	Diabète sucré de type 2, avec complications multiples
# E1170	Diabète sucré de type 2 insulinotraité, avec complications multiples
# E1178	Diabète sucré de type 2 non insulinotraité ou sans précision, avec complications multiples
# E118	Diabète sucré de type 2, avec complications non précisées
# E1180	Diabète sucré de type 2 insulinotraité, avec complications non précisées
# E1188	Diabète sucré de type 2 non insulinotraité ou sans précision, avec complications non précisées
# E119	Diabète sucré de type 2, sans complication
# E1190	Diabète sucré de type 2 insulinotraité, sans complication
# E1198	Diabète sucré de type 2 non insulinotraité ou sans précision, sans complication